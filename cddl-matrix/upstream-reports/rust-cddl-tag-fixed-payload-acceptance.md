# rust `cddl` accepts a wrong fixed payload under a declared CBOR tag

*Status: DRAFT, not yet filed. Prepared for the maintainer to paste into an issue on the rust
`cddl` crate tracker. The commands and exit codes below are the complete reproduction.*

We are the authors of [cddl-codegen](https://github.com/dcSpark/cddl-codegen).  Its
decode-conformance catalog uses the Ruby `cddl` gem and rust `cddl` CLI as independent oracles for
spec-invalid CBOR.  This report concerns one exact constraint vector for:

```cddl
t = [ v: #6.11(true) // label: tstr ]
```

The bytes `81 cb f4` are a one-element array containing tag 11 around `false`.  They preserve the
array shape and tag number of the `v` arm but change only its fixed payload from `true` to `false`.
The Ruby oracle rejects them; the pinned rust oracle accepts them.

## Environment and provenance

The Ruby binary reports `cddl tool version 0.12.14`.  The rust binary reports `cddl 0.10.6` and was
invoked at the repository default path
`~/Documents/git/cddl/target/debug/cddl`.  Version alone is not binary provenance: all relevant
local rust branches report 0.10.6.  cddl-codegen's startup fingerprint identifies the *behavior* of
this binary as the required `local-fixes @ ac1b98e` oracle behavior (the pin named by
`cddl-matrix/verify.ts`).

At probe time the sibling source checkout itself was on branch `local-fixes` at
`a7ed0784e89689784ff78ed0e85c7434a3528937` (with unrelated untracked `future-issues/`).  That
checkout state is deliberately not claimed as the binary's provenance: rebuilding a mutable source
tree would change the evidence.  The reported verdict is the observed `0.10.6` binary plus the
repository's behavioral fingerprint.

## Reproduction

The commands below use the validator's required `--ci` flag; without it rust `cddl` can print a
validation error while exiting zero.  The final `echo` after each validator is the observed verdict
(`0` = accept, `1` = reject).

```console
$ RUBY_CDDL="$(ruby -e 'puts Gem.user_dir')/bin/cddl"
$ RUST_CDDL="$HOME/Documents/git/cddl/target/debug/cddl"
$ printf '%s\n' 't = [ v: #6.11(true) // label: tstr ]' > probe.cddl

# For each HEX below:
$ printf '%s' "$HEX" | xxd -r -p > probe.cbor
$ "$RUBY_CDDL" probe.cddl validate probe.cbor >/dev/null 2>&1; echo "ruby exit=$?"
$ "$RUST_CDDL" --ci validate --cddl probe.cddl --cbor probe.cbor >/dev/null 2>&1; echo "rust exit=$?"
```

| case | CBOR | decoded item | Ruby exit | rust exit |
| --- | --- | --- | ---: | ---: |
| declared tag-11 arm | `81cbf5` | `[ #6.11(true) ]` | 0 | 0 |
| wrong fixed payload | `81cbf4` | `[ #6.11(false) ]` | 1 | 0 |
| wrong tag number | `81ccf5` | `[ #6.12(true) ]` | 1 | 0 |
| valid sibling arm | `81657370696379` | `[ "spicy" ]` | 0 | 0 |

The first and last rows show that both validators accept the two declared alternatives.  The second
row isolates the catalog vector: tag 11 and the outer shape remain correct; only `true` becomes
`false`. The third row is an independently invalid wrong-tag control that rust also accepts,
suggesting its typed-tag validation gap is broader than the committed fixed-payload vector. It does
not widen the catalog exemption: `81ccf5` is not a committed catalog vector, while
`DECODE_REJECT_ORACLE_GAP_EXEMPT` is intentionally keyed to the exact committed `81cbf4` bytes.

## Why `81cbf4` is spec-invalid

RFC 8610 §3.6 defines typed data tags: **“`#6.nnn(type)` has tag number nnn and type is the tagged
data item's type.”**  Therefore `#6.11(true)` requires both tag number 11 and a tagged data item
matching the fixed value `true`.  `81cbf4` retains `#6.11` but supplies `false`, so it fails solely
through the wrapped fixed-value constraint.  It is not a malformed CBOR, a wrong array shape, or a
wrong tag number.

The Ruby gem rejects the vector consistently with that reading.  The pinned rust binary accepts it,
so this catalog vector has the narrow `DECODE_REJECT_ORACLE_GAP_EXEMPT` entry naming only `rust`.
The exemption is stale-guarded: when rust begins rejecting this exact vector, minting fails until the
entry is narrowed or removed.  cddl-codegen's generated decoder independently rejects the vector
with a pinned error reason in the decode-conformance replay.

## The branch that would undo this

**If rust acceptance is correct, this vector/exemption and the corresponding Q4 classification are
wrong and must be removed.**  In that branch, `true` inside `#6.11(true)` would not be a constraint
on the tagged item, the row would no longer supply fixed-selector arm-selection evidence, and the
generated decoder's rejection would be over-strict.  We would remove the catalog vector and its
exemption, undo the Q4 classification that depends on it, and revisit the decoder behavior rather
than retain an exemption that merely protects our preferred result.
