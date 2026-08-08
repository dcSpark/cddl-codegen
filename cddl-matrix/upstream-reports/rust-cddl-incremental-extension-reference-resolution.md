# rust `cddl`: incremental `/=` chains lose the plain `=` statement's arm when resolved through a reference

Paste-ready draft for an upstream issue against <https://github.com/anweiss/cddl>. Observed on the
`local-fixes` fork at `ac1b98e` (based on 0.10.x); the resolution path involved is upstream code,
not a fork patch, so the defect is expected to reproduce on stock 0.10.x — re-confirm on a stock
build before filing.

## Summary

A rule assembled incrementally with `/=` (RFC 8610 §3.9 — each statement ADDS alternatives)
validates all of its arms when it is the **validation root** and its chain starts with the plain
`=` statement, but when the same rule is reached **through a typename reference** — or when the
chain's first statement is a `/=` — only the arms of the `/=` (alternate) statements are honored:
the plain `=` statement's arm is dropped. The same instance therefore validates or fails depending
on how the rule is reached, not on what the spec accumulates for it.

## Reproduction

`chain-root.cddl` (rule under test IS the root — first rule):

```cddl
extended = uint
extended /= text
```

`chain-ref.cddl` (same rule, referenced by the root):

```cddl
r = extended

extended = uint
extended /= text
```

Instance `zero.cbor`: the single byte `00` (unsigned integer 0 — matches the `uint` arm).

```console
$ cddl validate --cddl chain-root.cddl --cbor zero.cbor
[INFO] Validation of "zero.cbor" is successful          # correct

$ cddl validate --cddl chain-ref.cddl --cbor zero.cbor
[ERROR] Validation of "zero.cbor" failed: error validating type choice at cbor location :
expected type text, got Integer(Integer(0))             # WRONG — the uint arm vanished
```

Discriminating probes (each executed against the fork build at `ac1b98e`):

- Referenced form, instance `6178` (`"x"`, text) **passes** — the `/=` arm survives; only the `=`
  arm is lost.
- Referenced form, three-statement chain (`= uint` + `/= text` + `/= bytes`): loses the `uint`
  arm and reports both alternate arms (`expected type text` / `expected type bytes`).
- Referenced form, extension-first order (`/= text` then `= uint`): loses the `uint` arm
  identically (error names only `text`).
- ROOT position is only correct for BASE-FIRST chains: the two- and three-statement base-first
  chains accept `00` at root, but the extension-first chain (`ext_first /= text` +
  `ext_first = uint`) rejects `00` even as the validation root — so root-position resolution
  drops the `=` arm too when the chain's first statement is a `/=`.
- The ruby `cddl` gem accepts `00` against every spelling above (spec-valid per §3.9).

## Expected

RFC 8610 §3.9: "`/=` ... adds alternatives to an existing rule"; resolution of a typename
reference must see the accumulated choice — the same arm set the root-position path already uses.

## Impact on cddl-codegen

The generated-crate conformance oracle always points the validator at the tested rule through a
synthetic root alias (`__cddl_oracle_root = <rule>`), so every fixture that spells an incremental
chain hits the gap: spec-valid emitted bytes for the base arm are reported non-conforming. Tracked
as gap #15 in `cddl-matrix/README.md` § "Upstream oracle gaps"; the corpus fixture
`tests/corpus/assignt_extend.cddl` rides `RUST_ORACLE_SKIP` (ruby gem remains its enforcing
oracle) until a fix lands.
