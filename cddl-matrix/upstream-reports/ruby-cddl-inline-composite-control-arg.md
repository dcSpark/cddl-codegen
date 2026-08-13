# ruby `cddl` gem: inline/composite type2 as a control-operator argument is a PARSE error

**Status:** upstream gem parser gap, ledgered locally; candidate upstream report.
**2026-08-04 update:** the `.cbor`/tagged rule-body force-wraps (`2108dac7`, `60ed8bb1`) gave the
long-committed construct-carrying rules STANDALONE codecs, so the `ir_conformance_corpus` minter now
dumps standalone cases the gem cannot judge — five new `RUBY_EXPECTED_FAIL` rows landed citing this
filing (`cbor_wrapped_group_array/wrapped`, `cbor_bignint_table/x`,
`cbor_nonempty_payload/wrapped_nonempty{,_map}`, `cbor_int_table/cbor_int_table`), and
`cbor_int_table` was split out of `int_alias.cddl` so the parse poison stops disqualifying the
gem-parseable siblings `bare_int`/`cbor_int` (collateral, not construct gaps).
**Gem:** `cddl` 0.12.14 (`gem install --user-install cddl`; binary at
`~/.local/share/gem/ruby/3.0.0/bin/cddl`).
**Found by:** `ir_conformance_corpus`'s decorrelated ruby-oracle sweep, first `check.ts full` run
after `tests/corpus/cbor_wrapped_group_array.cddl` landed (the `3450b4c` `.cbor`-wrapped
plain-group promotion fixture).

## Symptom

The gem's CDDL parser rejects a control operator whose controller (RHS argument) is an inline
composite `type2`, with exit 65 ("Look for syntax problems") — it never reaches validation, so
every rule in the containing spec becomes unjudgeable:

```
a = bytes .cbor [uint]          ; FAIL(65) — parse error at `.cbor [`
a = bytes .cbor {uint => uint}  ; FAIL(65)
a = bytes .cbor ~inner          ; FAIL(65)  (inner = [uint])
```

Working forms (same gem, same construct via a name):

```
inner = [uint]
a = bytes .cbor inner           ; OK
a = bytes .cbor uint            ; OK
a = bytes .size 4               ; OK
```

Per RFC 8610 the controller is a full `type2` (`type1 = type2 [S (rangeop / ctlop) S type2]`), so
the inline forms are spec-valid; both our parser (the `cddl` crate fork) and the rust CLI oracle
accept them. This is a sibling of the known ruby radix-position deviations
(`cddl-matrix/upstream-reports/ruby-cddl-radix-position-deviations.md`): the gem corroborates a construct class only in the
positions its parser happens to cover.

## Local disposition

- `RUBY_EXPECTED_FAIL` in `src/tests/integration_tests.rs` (the `ir_conformance_corpus` gate's
  per-(fixture, rule) ledger) carries `("cbor_wrapped_group_array", "holder", …)` citing this note.
  The divergence is a spec-parse failure, so the gem rejects the fixture's *innocent sibling* rule
  `holder = [c: [coords]]` too — its minted `[[0, 0]]` (`81 82 00 00`) is spec-valid (the plain
  group inlines to two uints); the reference-codec differential and the rust-oracle legs still
  cover it.
- Workaround for FUTURE fixtures that want ruby corroboration: name the payload type
  (`inner = [coords]`, `wrapped = bytes .cbor inner`) — but do NOT rewrite existing fixtures to
  dodge the gem; the inline form is exactly the shape `3450b4c` fixed, and the pinned ledger keeps
  the gem's gap visible instead of silently narrowing our corpus.

## Upstream steps

1. Reduce to the one-line repro above against the gem's grammar (`cddl-grammar` /
   `abnftt`-generated parser) and file against https://github.com/cabo/cddl.
2. When a fixed gem ships: remove the `RUBY_EXPECTED_FAIL` entry (the gate's stale-ledger guard
   flips red on its own once the divergence disappears), re-run `check.ts full`, and prune this
   note + the `cddl-matrix/roadmap.toml` upstream close-out bullet.
