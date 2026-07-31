//! The wasmtime harness the behavioral assertions drive. Deliberately thin: everything that makes a
//! CLAIM lives in `tests/behavior.rs`, so a failure names the class it falsifies.
//!
//! Two facts this file establishes rather than assumes:
//!
//!  1. **`wasmtime-wasi` must be in the linker.** A wasip2 reactor imports `wasi:*` interfaces even
//!     for a pure codec, so an empty linker fails instantiation on `wasi:io/poll` before a single
//!     assertion runs. The negative control for it is `wasi_is_required_in_the_linker`.
//!  2. **The component under test is named by an ENV VAR, and the WIT by a RELATIVE path.** Both
//!     keep this crate's bytes run-independent: a literal scratch path baked into the source would
//!     make every gate-cache key unique to its run, which is the same failure the gate-cache
//!     input-closure rule exists to prevent from the other direction.
//!
//! `wasmtime::Error` is NOT `anyhow::Error` in 47 — `wasmtime::Result` is the alias to reach for.

use wasmtime::component::{Component, Linker, ResourceTable};
use wasmtime::{Config, Engine, Result, Store};
use wasmtime_wasi::p2::add_to_linker_sync;
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

wasmtime::component::bindgen!({
    path: "../component/wit",
    world: "cddl-lib-world",
});

pub use exports::cddl::cddl_lib::types as api;

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

/// A loaded instance plus its store. Every assertion drives the component through this.
pub struct Harness {
    pub store: Store<Ctx>,
    pub world: CddlLibWorld,
}

impl Harness {
    /// Split-borrow so an assertion can hold the exported interface's function table (`&`) while
    /// driving the store (`&mut`) — every guest call needs both at once.
    pub fn split(&mut self) -> (&mut Store<Ctx>, &api::Guest) {
        (&mut self.store, self.world.cddl_cddl_lib_types())
    }
}

/// The built component, named by the gate rather than by this source.
pub fn artifact() -> String {
    std::env::var("CDDL_COMPONENT_WASM").expect(
        "CDDL_COMPONENT_WASM must name the built wasm32-wasip2 component — the gate sets it; \
         running this crate by hand needs it set too",
    )
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

pub fn load() -> Result<Harness> {
    let engine = engine()?;
    let component = Component::from_file(&engine, artifact())?;
    let mut linker: Linker<Ctx> = Linker::new(&engine);
    // Load-bearing, not precautionary — see this module's header and the negative control.
    add_to_linker_sync(&mut linker)?;
    let mut store = store(&engine);
    let world = CddlLibWorld::instantiate(&mut store, &component, &linker)?;
    Ok(Harness { store, world })
}

/// The same load path with an EMPTY linker, so the requirement above is a measured fact rather than
/// a comment.
pub fn load_without_wasi() -> Result<Harness> {
    let engine = engine()?;
    let component = Component::from_file(&engine, artifact())?;
    let linker: Linker<Ctx> = Linker::new(&engine);
    let mut store = store(&engine);
    let world = CddlLibWorld::instantiate(&mut store, &component, &linker)?;
    Ok(Harness { store, world })
}
