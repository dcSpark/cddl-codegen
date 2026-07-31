//! Composition and loading for the acceptance gate. Deliberately thin: everything that makes a CLAIM
//! lives in `tests/acceptance.rs`, so a failure names the class it falsifies.
//!
//! Three facts this file establishes rather than assumes:
//!
//!  1. **The composition is SINGLE-INSTANTIATION, and that is a choice the composer makes.** One
//!     `instantiate` per package; the dependency's exported interface is aliased ONCE and used both
//!     as the consumer's instantiation argument and as a world export. [`Topology::TwoInstances`]
//!     builds the same graph with a second dependency instance so the gate can measure what the
//!     composer does about it — which is nothing.
//!  2. **`wac plug` is not usable here and is not used.** It consumes the dependency's exports
//!     without re-exporting them, leaving a single-export world a host cannot mint a dependency
//!     object in. The two `export` calls below are what make the world dual-export.
//!  3. **`wasmtime-wasi` must be in the linker.** A wasip2 reactor imports `wasi:*` interfaces even
//!     for a pure codec, so an empty linker fails instantiation before a single assertion runs.
//!
//! `wasmtime::Error` is NOT `anyhow::Error` in 47 — `wasmtime::Result` is the alias to reach for.

use wac_graph::types::Package;
use wac_graph::{CompositionGraph, EncodeOptions};
use wasmtime::component::{Component, Linker, ResourceTable};
use wasmtime::{Config, Engine, Result, Store};
use wasmtime_wasi::p2::add_to_linker_sync;
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

wasmtime::component::bindgen!({
    path: "wit",
    world: "composed",
});

pub use exports::cddl::chain::types as chain;
pub use exports::cddl::wallet::types as wallet;

/// `wac-graph` and `wit-component` report `anyhow::Error` and their own error enums, none of which
/// `wasmtime::Error` (a DISTINCT type in 47, not an `anyhow::Error` alias) has a `From` for.
/// Flattened to a message rather than boxed, and generic over `Display` rather than over any one of
/// those types: every caller here either asserts on the text or propagates it to a test failure, so
/// one error type across the file is worth more than a preserved chain.
fn flatten<E: std::fmt::Display>(e: E) -> wasmtime::Error {
    wasmtime::Error::msg(format!("{e:#}"))
}

/// The WIT identifiers the composition is wired through. They are the generated packages' own, so a
/// change to either crate's `--lib-name` moves these, `wit/world.wit` and the fixtures together.
pub const CHAIN_IFACE: &str = "cddl:chain/types@0.1.0";
pub const WALLET_IFACE: &str = "cddl:wallet/types@0.1.0";

/// How many times the dependency package is instantiated.
///
/// The distinction has no compose-time consequence — that is the finding the gate pins — and a
/// decisive runtime one: resource types are generative per instantiation, so under
/// [`Topology::TwoInstances`] the world's exported dependency interface and the one the consumer was
/// wired to are DIFFERENT types wearing the same name.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Topology {
    /// The prescribed shape: one dependency instance serves the consumer and the world's export.
    Once,
    /// The mistake: a second dependency instance supplies the world's export while the consumer
    /// stays wired to the first.
    TwoInstances,
}

/// Compose the two built components into one world exporting BOTH interfaces.
///
/// The `Package::from_file` calls are split off into locals rather than nested in
/// `register_package`: `g.types_mut()` and `g.register_package()` both borrow the graph mutably, so
/// the one-expression spelling does not borrow-check.
pub fn compose(chain_wasm: &str, wallet_wasm: &str, topology: Topology) -> Result<Vec<u8>> {
    let mut g = CompositionGraph::new();
    let p = Package::from_file("cddl:chain", None, chain_wasm, g.types_mut()).map_err(flatten)?;
    let chain_pkg = g.register_package(p).map_err(flatten)?;
    let p = Package::from_file("cddl:wallet", None, wallet_wasm, g.types_mut()).map_err(flatten)?;
    let wallet_pkg = g.register_package(p).map_err(flatten)?;

    let chain_inst = g.instantiate(chain_pkg);
    let wallet_inst = g.instantiate(wallet_pkg);

    // The consumer is ALWAYS wired to the first instance. Under `TwoInstances` the world's export
    // comes from a second one, which is exactly the topology whose handles cannot cross.
    let wired = g.alias_instance_export(chain_inst, CHAIN_IFACE).map_err(flatten)?;
    g.set_instantiation_argument(wallet_inst, CHAIN_IFACE, wired)
        .map_err(flatten)?;

    let exported = match topology {
        Topology::Once => wired,
        Topology::TwoInstances => {
            let second = g.instantiate(chain_pkg);
            g.alias_instance_export(second, CHAIN_IFACE).map_err(flatten)?
        }
    };
    // Re-exporting the dependency is what `wac plug` drops and what the acceptance flow needs: a
    // host cannot mint a dependency object through a world that does not export its interface.
    g.export(exported, CHAIN_IFACE).map_err(flatten)?;
    let wallet_export = g.alias_instance_export(wallet_inst, WALLET_IFACE).map_err(flatten)?;
    g.export(wallet_export, WALLET_IFACE).map_err(flatten)?;

    g.encode(EncodeOptions::default()).map_err(flatten)
}

/// The interface names a composed artifact's world EXPORTS, read back out of the encoded bytes.
///
/// Decoded rather than reported by the graph, because the claim under test is about the artifact a
/// host would be handed, not about the description the composer was working from.
pub fn exported_interfaces(bytes: &[u8]) -> Result<Vec<String>> {
    let mut names = Vec::new();
    match wit_component::decode(bytes).map_err(flatten)? {
        wit_component::DecodedWasm::Component(resolve, world) => {
            for key in resolve.worlds[world].exports.keys() {
                if let wit_parser::WorldKey::Interface(id) = key {
                    if let Some(name) = resolve.id_of(*id) {
                        names.push(name);
                    }
                }
            }
        }
        wit_component::DecodedWasm::WitPackage(..) => {
            return Err(wasmtime::Error::msg(
                "the composed artifact decoded as a WIT package rather than a component",
            ))
        }
    }
    names.sort();
    Ok(names)
}

pub struct Ctx {
    ctx: WasiCtx,
    table: ResourceTable,
}

impl WasiView for Ctx {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.ctx,
            table: &mut self.table,
        }
    }
}

/// A loaded composed world plus its store.
pub struct Harness {
    pub store: Store<Ctx>,
    pub world: Composed,
}

impl Harness {
    /// Split-borrow so an assertion can hold BOTH exported interfaces' function tables (`&`) while
    /// driving the store (`&mut`). Holding both at once is the point: the acceptance flow calls the
    /// consumer and then the dependency on the handle the consumer returned.
    pub fn split(&mut self) -> (&mut Store<Ctx>, &chain::Guest, &wallet::Guest) {
        (
            &mut self.store,
            self.world.cddl_chain_types(),
            self.world.cddl_wallet_types(),
        )
    }
}

/// The built components, named by the gate rather than by this source.
pub fn artifacts() -> (String, String) {
    let get = |k: &str| {
        std::env::var(k).unwrap_or_else(|_| {
            panic!(
                "{k} must name a built wasm32-wasip2 component — the gate sets it; running this \
                 crate by hand needs it set too"
            )
        })
    };
    (get("CDDL_CHAIN_WASM"), get("CDDL_WALLET_WASM"))
}

fn engine() -> Result<Engine> {
    let mut config = Config::new();
    config.wasm_component_model(true);
    Engine::new(&config)
}

fn store(engine: &Engine) -> Store<Ctx> {
    Store::new(
        engine,
        Ctx {
            // stdout/stderr are inherited so a guest-side panic's message reaches the gate's log
            // rather than vanishing behind an opaque trap.
            ctx: WasiCtxBuilder::new()
                .inherit_stdout()
                .inherit_stderr()
                .build(),
            table: ResourceTable::new(),
        },
    )
}

/// Compose with the given topology and instantiate the result.
pub fn load(topology: Topology) -> Result<Harness> {
    let (chain_wasm, wallet_wasm) = artifacts();
    let bytes = compose(&chain_wasm, &wallet_wasm, topology)?;
    load_bytes(&bytes)
}

pub fn load_bytes(bytes: &[u8]) -> Result<Harness> {
    let engine = engine()?;
    let component = Component::from_binary(&engine, bytes)?;
    let mut linker: Linker<Ctx> = Linker::new(&engine);
    // Load-bearing, not precautionary — see this module's header.
    add_to_linker_sync(&mut linker)?;
    let mut store = store(&engine);
    let world = Composed::instantiate(&mut store, &component, &linker)?;
    Ok(Harness { store, world })
}
