# ruby `cddl` gem: extension-first `/=` chain crashes with "Duplicate rule definition"

Paste-ready draft for an upstream report against the `cddl` gem (observed on 0.12.14). Unfiled.

## Summary

A `/=` incremental type-choice chain whose plain `=` statement is written AFTER the `/=`
statement is a hard, uncaught RuntimeError at spec-parse time — though RFC 8610 §3.9 makes `/=`
accumulate alternatives and §2.2.2/§3.1 make rule order insignificant, so the spelling is legal.
The base-first spelling of the same chain parses and validates correctly.

## Reproduction

`ext-first.cddl`:

```cddl
ext_first /= text
ext_first = uint
```

```console
$ cddl ext-first.cddl validate zero.cbor   # zero.cbor = the single byte 00
.../cddl-0.12.14/lib/cddl.rb:231:in `block in rules': Duplicate rule definition ext_first \
  -- common: >[:type1, "< -- old: >text"]< -- new: >uint"]< (RuntimeError)
```

Exit 1 (uncaught RuntimeError), regardless of instance — the crash is in spec parsing, so it
poisons every rule in the file. Both `generate` and `validate` hit it (the generate side is what
keeps the corresponding decode-catalog row vectorless).

Control: the base-first spelling (`ext_first = uint` then `ext_first /= text`) parses, and
validates both `00` (uint arm) and `6178` (`"x"`, text arm) successfully.

## Expected

The chain should accumulate to `ext_first = text / uint` in any statement order; at minimum, a
legal-but-unsupported ordering deserves a caught, named error rather than a RuntimeError
backtrace.

## Impact on cddl-codegen

One parse crash poisons the whole spec file for the gem, so the corpus fixture holding this
ordering is isolated in its own file (`tests/corpus/assignt_extend_ext_first.cddl`) and ledgered
in `RUBY_EXPECTED_FAIL` (src/tests/integration_tests.rs), whose entry carries the
conformance-transfer argument (the ordering generates byte-identically to the folded spelling,
which the oracles do judge). Sibling of the gem's inline-composite control-arg parse gap recorded
in the same ledger's other entries.
