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
//! | `mismatch` | `Ok`, but the bytes could not be shown equal to the expectation — they differ, |
//! |            | or the door handed back no handle to re-encode, or it returned something that  |
//! |            | is not a `result` at all                                                       |
//! | `trap`     | the call did not return a value at all                                         |
//!
//! `mismatch` is the "crossed, but not byte-equal" verdict, and every shape that cannot be SHOWN
//! byte-equal belongs to it — including an `Ok` carrying no handle. Reporting that as `ok` would
//! make the leg's central assertion unfalsifiable for that shape: the caller would record a
//! round trip for a door that produced nothing to compare.
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

/// The observable outcome of a `from-cbor-bytes` return value.
///
/// Kept separate from the runtime loop so the dynamic, defensive `Val` protocol can be
/// unit-tested even for shapes no generated component can currently return.
enum FromCborBytesOutcome {
    Handle(Val),
    Verdict { token: &'static str, detail: String },
}

/// Classify every dynamic value a `from-cbor-bytes` call can hand the host.
///
/// The success handle is copied because `Func::call` owns the result vector while the following
/// `to-cbor-bytes` call needs an owned argument. Every non-handle shape remains a per-vector
/// observation rather than a trap: see the protocol table in the module header.
fn classify_from_cbor_bytes_result(value: &Val) -> FromCborBytesOutcome {
    match value {
        Val::Result(Ok(Some(handle))) => FromCborBytesOutcome::Handle((**handle).clone()),
        Val::Result(Ok(None)) => FromCborBytesOutcome::Verdict {
            token: "mismatch",
            detail: "(door returned Ok with no handle to re-encode)".to_owned(),
        },
        Val::Result(Err(message)) => {
            let text = match message.as_deref() {
                Some(Val::String(text)) => text.clone(),
                other => format!("{other:?}"),
            };
            FromCborBytesOutcome::Verdict {
                token: "err",
                detail: format!("({text})"),
            }
        }
        other => FromCborBytesOutcome::Verdict {
            token: "mismatch",
            detail: format!("(door returned {other:?}, not a result)"),
        },
    }
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
        let handle = match classify_from_cbor_bytes_result(&results[0]) {
            FromCborBytesOutcome::Handle(handle) => handle,
            FromCborBytesOutcome::Verdict { token, detail } => {
                emit(token, &detail);
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

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_verdict(value: Val, token: &str, detail: &str) {
        match classify_from_cbor_bytes_result(&value) {
            FromCborBytesOutcome::Verdict {
                token: actual_token,
                detail: actual_detail,
            } => {
                assert_eq!(actual_token, token);
                assert_eq!(actual_detail, detail);
            }
            FromCborBytesOutcome::Handle(handle) => {
                panic!("expected {token} {detail:?}, got handle {handle:?}")
            }
        }
    }

    #[test]
    fn from_cbor_bytes_ok_handle_carries_the_handle_to_the_reencode_step() {
        let value = Val::Result(Ok(Some(Box::new(Val::U8(42)))));
        match classify_from_cbor_bytes_result(&value) {
            FromCborBytesOutcome::Handle(Val::U8(42)) => {}
            FromCborBytesOutcome::Handle(handle) => {
                panic!("wrong handle carried forward: {handle:?}")
            }
            FromCborBytesOutcome::Verdict { token, detail } => {
                panic!("expected a handle, got {token} {detail}")
            }
        }
    }

    #[test]
    fn from_cbor_bytes_ok_without_handle_is_a_mismatch_not_a_false_pass() {
        assert_verdict(
            Val::Result(Ok(None)),
            "mismatch",
            "(door returned Ok with no handle to re-encode)",
        );
    }

    #[test]
    fn from_cbor_bytes_string_error_is_an_err_verdict() {
        assert_verdict(
            Val::Result(Err(Some(Box::new(Val::String(
                "decoder refused".to_owned(),
            ))))),
            "err",
            "(decoder refused)",
        );
    }

    #[test]
    fn from_cbor_bytes_non_string_error_preserves_the_defensive_detail_shape() {
        assert_verdict(
            Val::Result(Err(Some(Box::new(Val::U8(7))))),
            "err",
            "(Some(U8(7)))",
        );
    }

    #[test]
    fn from_cbor_bytes_error_without_payload_is_still_an_err_verdict() {
        assert_verdict(Val::Result(Err(None)), "err", "(None)");
    }

    #[test]
    fn from_cbor_bytes_non_result_is_a_mismatch_with_the_observed_shape() {
        assert_verdict(
            Val::Bool(false),
            "mismatch",
            "(door returned Bool(false), not a result)",
        );
    }
}
