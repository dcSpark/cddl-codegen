# Architectural decisions

Read before changing architecture, emission, logging, wrapper collision detection, or config, including extensions to an approach below.
Keep these decisions unless new evidence justifies reopening them; the runtime-carrier decision has a stronger permission requirement.

## Emission and orchestration

- Keep string-based emission.
  Rustfmt, snapshots, and comment preservation depend on emitted-token stability; an AST/quote emitter breaks the overlay.
- Keep `codegen` workarounds isolated, including newline-smuggled attributes and the `derivative)]` hack.
  Their underlying fix belongs upstream.
- Do not introduce a `Ctx { types, cli }` parameter-pair struct.
  Hiding `&mut GenerationScope` and `&IntermediateTypes` behind it makes borrow splitting harder.
- Keep `api::with_types` one linear narrative.

## Logging

Keep `print!` progress logging and its message text: humans and tests consume it as-is.
Do not add prefixes, level tags, or a `log`/`tracing` dependency.
Whether a message prints is already gated by [log.rs](../../src/log.rs)'s six macros, with default level `warn`, diagnostics on stderr, and run output on stdout.
The text constraint does not prohibit this existing level gating.

## WASM wrapper-name collisions

Keep parallel per-container-kind sibling detectors, rather than one generic detector: their meaningful diagnostic differences are pinned.
This includes reject-duplicates sets, preserve-duplicates pair maps, and future kinds.
They guard rule-ident-versus-wrapper-ident for each name family, including `preserve_pair_map_loose_wrapper_name_collisions` and `preserve_pair_map_non_empty_wrapper_name_collisions`.
Do not restore the retired preserve-versus-default map wrapper collision detector: flavor-specific names (`PairMapKToV` versus `MapKToV`) make that collision unrepresentable.
This ruling applies to the WASM wrapper-name family, not every collision detector.

## Config runtime carrier

The `[runtime]` table's carrier derivation must stay exactly as shipped.
The maintainer has closed this decision; do not re-investigate, reopen, or change it without explicit maintainer permission.

## Config cross-crate mediation

Keep committed-file sidecars plus the [convergence pass](generation-contract.md#config-convergence-and-committed-state-verdict); do not introduce global IR or in-memory request passing.
Config must add no semantics beyond its expanded flags, as required by `a_whole_config_generates_what_the_hand_written_flags_generate`; `IntermediateTypes` also borrows the AST.
The reopening signal for an in-memory fast path is `[converge] re-running` lines exceeding roughly half the config's crates on routine edits.
