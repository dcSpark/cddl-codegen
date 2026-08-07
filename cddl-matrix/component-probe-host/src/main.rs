//! A GENERIC wasmtime host for the matrix's component EXECUTION leg: it drives any generated
//! component's uniform CBOR doors from a vector manifest, without knowing the spec.
//!
//! # Why the dynamic API and no `bindgen!`
//!
//! `bindgen!` would type the host against ONE world at compile time, so every probed cell would
//! need its own host build (a wasmtime link per matrix row). The component face's execution surface
//! is uniform — `from-cbor-bytes: static func(list<u8>) -> result<t, string>` and
//! `to-cbor-bytes: func() -> list<u8>` on every class-backed resource — so the untyped `Val` API
//! drives every cell with ONE binary. The interface FQN is not hard-coded either: the resource is
//! searched across the component's exported instances, so a spec that lands its types in a
//! differently-named interface still runs.
//!
//! # Protocol (mechanism here, POLICY in the caller)
//!
//! ```text
//! component-probe-host <component.wasm> <resource-kebab-name> <vector-manifest>
//! ```
//!
//! The manifest is one vector per line, whitespace-separated:
//!
//! ```text
//! <name> <expect> <input-hex> <expected-hex|->
//! ```
//!
//! `expect` is echoed back, never acted on: which verdict is a PASS is the caller's judgement, and
//! a host that decided it could not report "a reject vector decoded fine" as an observation. Every
//! vector produces exactly one line on stdout:
//!
//! ```text
//! component-probe <name> <expect> <verdict> <detail>
//! ```
//!
//! with four DISTINCT verdict tokens, because the caller acts differently on each:
//!
//! | token      | meaning                                                                       |
//! |------------|-------------------------------------------------------------------------------|
//! | `ok`       | `from-cbor-bytes` returned `Ok`, and (if an expectation was given) re-encoding  |
//! |            | the handle through `to-cbor-bytes` produced exactly those bytes                |
//! | `err`      | the door returned `Err(string)` — a refusal that CROSSED the boundary          |
//! | `mismatch` | `Ok`, but the re-encoded bytes differ from the expectation                     |
//! | `trap`     | the call did not return a value at all                                         |
//!
//! `trap` deliberately covers the whole "no value crossed" class (a guest trap, or a call the host
//! could not make); the detail text names which. That is the distinction the leg exists to make —
//! an `Err` leaves the instance usable, a trap poisons the store and every later caller dies with
//! it — and the caller proves the survival half by putting a re-check vector after its rejects, so
//! usability is an OBSERVED line rather than something this host asserts.
//!
//! wasmtime 47 retired the caller-driven `post_return` step (the method is deprecated and does
//! nothing), so this host does not call it — a call sequence written against an older wasmtime
//! example will carry one and it is dead weight, not a correctness step.
//!
//! Vectors run IN ORDER against ONE instance, so a later vector's verdict is evidence about the
//! instance the earlier ones left behind. A trap stops the run (the store is poisoned, so every
//! later line would be an artefact of the trap): the trap line is printed, the remaining vectors
//! are not, and the caller sees missing verdicts.
//!
//! Exit codes: 0 = every vector produced a verdict; 1 = the host could not run the probe at all
//! (component/manifest unreadable, no door found); 2 = a trap cut the run short.

use std::fmt::Write as _;
use wasmtime::component::{Component, Func, Linker, ResourceTable, Val};
use wasmtime::{Config, Engine, Store};
use wasmtime_wasi::p2::add_to_linker_sync;
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

struct Host {
    table: ResourceTable,
    wasi: WasiCtx,
}
impl WasiView for Host {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.table,
        }
    }
}

/// One manifest line. `expect` is opaque here — see the module header.
struct Vector {
    name: String,
    expect: String,
    input: Vec<u8>,
    expected: Option<Vec<u8>>,
}

fn hex_decode(s: &str) -> Option<Vec<u8>> {
    if !s.len().is_multiple_of(2) {
        return None;
    }
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).ok())
        .collect()
}

fn hex_encode(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        let _ = write!(out, "{b:02x}");
    }
    out
}

/// `1` — the host could not run the probe at all. Distinct from every per-vector verdict: nothing
/// was observed about the boundary, so the caller must not read a missing verdict as a failure of
/// the component.
fn fail(msg: &str) -> ! {
    eprintln!("component-probe-host: {msg}");
    std::process::exit(1)
}

fn parse_manifest(text: &str) -> Vec<Vector> {
    let mut out = Vec::new();
    for (n, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let f: Vec<&str> = line.split_whitespace().collect();
        if f.len() != 4 {
            fail(&format!(
                "vector manifest line {} has {} field(s), expected 4 (<name> <expect> <hex> <expected-hex|->): {line:?}",
                n + 1,
                f.len()
            ));
        }
        let input = hex_decode(f[2])
            .unwrap_or_else(|| fail(&format!("vector {:?}: input {:?} is not hex", f[0], f[2])));
        let expected = if f[3] == "-" {
            None
        } else {
            Some(hex_decode(f[3]).unwrap_or_else(|| {
                fail(&format!(
                    "vector {:?}: expected {:?} is not hex",
                    f[0], f[3]
                ))
            }))
        };
        out.push(Vector {
            name: f[0].to_owned(),
            expect: f[1].to_owned(),
            input,
            expected,
        });
    }
    out
}

/// The two doors, looked up across every exported instance (plus the component's own top level).
/// Returning both together is deliberate: a resource whose decode door is exported without its
/// encode door cannot be round-tripped, and finding that out at lookup time names the missing door
/// instead of failing mid-vector.
fn find_doors(
    store: &mut Store<Host>,
    instance: &wasmtime::component::Instance,
    component: &Component,
    engine: &Engine,
    resource: &str,
) -> (Func, Func) {
    let from_name = format!("[static]{resource}.from-cbor-bytes");
    let to_name = format!("[method]{resource}.to-cbor-bytes");
    let ty = component.component_type();
    let mut scopes: Vec<Option<String>> = vec![None];
    scopes.extend(ty.exports(engine).map(|(name, _)| Some(name.to_owned())));
    drop(ty);

    let mut searched = Vec::new();
    for scope in scopes {
        let parent = match &scope {
            None => None,
            Some(name) => match instance.get_export_index(&mut *store, None, name) {
                Some(idx) => Some(idx),
                None => continue,
            },
        };
        searched.push(scope.clone().unwrap_or_else(|| "<world>".to_owned()));
        let Some(from_idx) = instance.get_export_index(&mut *store, parent.as_ref(), &from_name)
        else {
            continue;
        };
        let Some(to_idx) = instance.get_export_index(&mut *store, parent.as_ref(), &to_name) else {
            fail(&format!(
                "{scope:?} exports {from_name:?} but not {to_name:?} — the resource has a decode \
                 door with no encode door, so no round trip is expressible"
            ));
        };
        let from = instance
            .get_func(&mut *store, from_idx)
            .unwrap_or_else(|| fail(&format!("{from_name:?} is not a function export")));
        let to = instance
            .get_func(&mut *store, to_idx)
            .unwrap_or_else(|| fail(&format!("{to_name:?} is not a function export")));
        return (from, to);
    }
    fail(&format!(
        "no exported instance provides {from_name:?} (searched: {})",
        searched.join(", ")
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 4 {
        fail(
            "usage: component-probe-host <component.wasm> <resource-kebab-name> <vector-manifest>",
        );
    }
    let (wasm, resource, manifest_path) = (&args[1], &args[2], &args[3]);
    let vectors =
        parse_manifest(&std::fs::read_to_string(manifest_path).unwrap_or_else(|e| {
            fail(&format!("cannot read vector manifest {manifest_path}: {e}"))
        }));
    if vectors.is_empty() {
        fail("the vector manifest holds no vectors — nothing would be observed");
    }

    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config).unwrap_or_else(|e| fail(&format!("engine: {e}")));
    let component = Component::from_file(&engine, wasm)
        .unwrap_or_else(|e| fail(&format!("cannot load component {wasm}: {e}")));
    let mut linker: Linker<Host> = Linker::new(&engine);
    add_to_linker_sync(&mut linker).unwrap_or_else(|e| fail(&format!("wasi linker: {e}")));
    let mut store = Store::new(
        &engine,
        Host {
            table: ResourceTable::new(),
            wasi: WasiCtxBuilder::new().build(),
        },
    );
    let instance = linker
        .instantiate(&mut store, &component)
        .unwrap_or_else(|e| fail(&format!("cannot instantiate {wasm}: {e}")));
    let (from, to) = find_doors(&mut store, &instance, &component, &engine, resource);

    for v in &vectors {
        let emit = |verdict: &str, detail: &str| {
            println!("component-probe {} {} {verdict} {detail}", v.name, v.expect);
        };
        let args = [Val::List(v.input.iter().map(|b| Val::U8(*b)).collect())];
        let mut results = vec![Val::Bool(false)];
        if let Err(e) = from.call(&mut store, &args, &mut results) {
            emit("trap", &trap_detail("from-cbor-bytes", &e));
            std::process::exit(2);
        }
        let handle = match &results[0] {
            Val::Result(Ok(Some(h))) => (**h).clone(),
            Val::Result(Ok(None)) => {
                emit("ok", "(door returned Ok with no handle)");
                continue;
            }
            Val::Result(Err(msg)) => {
                let text = match msg.as_deref() {
                    Some(Val::String(s)) => s.clone(),
                    other => format!("{other:?}"),
                };
                emit("err", &format!("({text})"));
                continue;
            }
            other => {
                // Not a `result` at all: the door's WIT shape is not what this leg drives, which is
                // a fact about the component, not a trap.
                emit(
                    "mismatch",
                    &format!("(door returned {other:?}, not a result)"),
                );
                continue;
            }
        };
        let mut out = vec![Val::Bool(false)];
        if let Err(e) = to.call(&mut store, &[handle], &mut out) {
            emit("trap", &trap_detail("to-cbor-bytes", &e));
            std::process::exit(2);
        }
        let bytes: Vec<u8> = match &out[0] {
            Val::List(items) => items
                .iter()
                .map(|x| match x {
                    Val::U8(b) => *b,
                    other => fail(&format!(
                        "to-cbor-bytes returned a non-u8 element: {other:?}"
                    )),
                })
                .collect(),
            other => fail(&format!(
                "to-cbor-bytes returned {other:?}, expected list<u8>"
            )),
        };
        match &v.expected {
            Some(want) if want != &bytes => emit(
                "mismatch",
                &format!("(expected {} got {})", hex_encode(want), hex_encode(&bytes)),
            ),
            _ => emit("ok", &format!("({})", hex_encode(&bytes))),
        }
    }
}

/// A trap detail that names WHICH no-value-crossed shape happened. wasmtime reports a guest trap
/// and a call the host could not make through the same `Err`, and the two want different fixes (a
/// boundary defect vs a driver defect), so the text distinguishes them even though the verdict
/// token deliberately does not.
fn trap_detail(door: &str, e: &wasmtime::Error) -> String {
    let kind = if e.downcast_ref::<wasmtime::Trap>().is_some() {
        "wasm-trap"
    } else {
        "call-error"
    };
    format!("({door} {kind}: {})", format!("{e:#}").replace('\n', " / "))
}
