# example

`test.cddl` is the getting-started example spec. It's the spec run verbatim by the commands in
[`docs/docs/getting_started.mdx`](../docs/docs/getting_started.mdx) (and pinned by the
`getting_started_example` integration test).

To get round-trip unit tests for the generated output, generate with the `--emit-tests` flag — it
emits per-type round-trip and reject tests into the generated crate. (This replaces an older manual
workflow of copying hand-written unit tests into the export directory.)
