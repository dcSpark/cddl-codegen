# `.size` on a signed `int`: semantics clarified by the RFC author (cbor-wg/cddl#32)

> **STATUS (2026-07-07): SEMANTICS CLARIFIED — cddl-codegen now REJECTS the construct gracefully;
> rust oracle over-rejection bug remains upstream.** The RFC author answered our tracking issue
> <https://github.com/cbor-wg/cddl/issues/32>: `int .size 8` is well-defined via choice
> distribution and matches exactly the `uint .size 8` window; negatives never match. That makes
> ruby's behavior correct (it is the author's own tool), the rust CLI's blanket error a real
> over-rejection bug (now PR-able, citing the issue), and cddl-codegen's `i{8N}` mapping
> nonconformant in both directions. Close-out steps at the bottom; the codegen decision is open.

## The clarified semantics (per the author, verbatim reasoning)

1. `int` is prelude sugar: `int = uint / nint` (RFC 8610 Appendix D). So
   `root = int .size 8` ≡ `root = (uint / nint) .size 8` ≡ `root = uint .size 8 / nint .size 8` —
   **a control distributes over a type choice**.
2. `.size` is defined on `uint`, not on `nint`. **A control applied to a value it is not defined
   for simply does not match** (a validator "would probably want" a diagnostic saying so — but the
   correct verdict is non-match, not a hard error and not spec rejection).
3. Therefore `int .size 8` matches exactly `0...(256**8)`: `4711` matches, `-4711` does not.
   In the author's words: "CBOR does not have signed integers, only unsigned ones (which are
   handled by .size as a target) and negative ones (which are not handled)."
4. Validators are NOT expected to type-analyze the target (it can be an arbitrary generic/complex
   type, e.g. `COMPLEXTYPE<somebase> .size 8` where the target is built from `.plus`-controlled
   range endpoints); controls apply at the **value level** to whatever value matched the target.

Point 4 is the generalizable principle for the whole matrix: control applicability is a per-VALUE
question after target matching, never a static domain check on the target type expression. Point 2
resolves what my earlier analysis called "undefined": out-of-domain application has a defined
outcome — non-match.

## Scoreboard against the clarified semantics

Spec: `root = int .size 8`. Correct matching set: `[0, 256**8)`.

| Instance | correct | ruby 0.12.14 | rust cddl (0.10.x + fork) | cddl-codegen (`i64`) |
|---|---|---|---|---|
| `0` | match | ok ✓ | hard error ✗ | ok ✓ |
| `-1` | no match | fail ✓ | hard error (right verdict, wrong mode) | **accepts ✗** |
| `-2^63` | no match | fail ✓ | hard error (right verdict, wrong mode) | **accepts ✗** |
| `2^64-1` | match | ok ✓ | hard error ✗ | **rejects ✗** |

- **ruby: fully correct.** Its `d >> (8*N) == 0` check IS the distribution semantics (arithmetic
  shift makes negatives fail = the nint arm not matching). Unsurprising in hindsight — the gem is
  the RFC author's own implementation. My earlier "silent uint-substitution, indefensible" judgment
  was exactly backwards.
- **rust: over-rejection bug**, now with citable semantics (the issue answer). Same class as the
  fixed non-uint-range gap: valid instances (`0`, `2^64-1`) are rejected because the validator
  errors on the construct instead of applying the control per-value. Candidate `local-fixes` fork
  fix + upstream PR: in `visit_range`/control handling, `.size` over an integer value applies the
  uint window to non-negative values and yields non-match (ideally with the diagnostic the author
  suggests) for negative values, instead of erroring. Cite cbor-wg/cddl#32 in the PR.
- **cddl-codegen: divergent in both directions.** `int .size N` → `i{8N}` accepts negatives that
  must not match AND rejects `[2^(8N-1), 2^8N)` values that must match. The author's answer also
  explicitly rejects the signed-window idea I proposed on the issue — there is no "signed integer"
  in CBOR's data model for it to describe.

## The cddl-codegen decision (DECIDED 2026-07-07: graceful rejection)

`int .size N` is now **rejected gracefully** at generation (parsing.rs, the `.size`→`int` arm;
pinned by `size_on_signed_int_rejects_gracefully` in `src/tests/robustness_tests.rs`, which also
pins the boundary — `uint .size N` keeps generating). Rationale: the old `i{8N}` mapping
mis-enforced the clarified window in both directions, and ALIGNING (generating the `u{8N}` window)
is blocked too — the rust `cddl` oracle (parser dep + conformance validator) hard-errors on the
construct, so an aligned implementation would be uncertifiable by the two-oracle gates. The
rejection message names both conformant spellings (`uint .size N`; explicit signed range).
The `int .size` uses in the integration fixtures (`tests/core`, `tests/json`,
`tests/preserve-encodings`, `tests/corpus/sized_int.cddl`) were re-spelled as explicit signed
ranges — generated output is byte-identical (exact-window ranges collapse onto the same rust
primitives), and `sized_int` left `RUST_ORACLE_SKIP` entirely (its other blocker fell to `885c61c`).

## Close-out steps (remaining)

- Fork fix + upstream PR for the rust over-rejection (see scoreboard bullet 2) — deferred, not
  queued. Once the oracle applies the per-value semantics, supporting `int .size N` as the uint
  window becomes certifiable (a `ctl.size.int` row becomes mintable) and the graceful rejection
  can be revisited.
- Watch cbor-wg/cddl#32 for whether the clarification lands as erratum/spec text; update
  `docs/docs/current_capacities.mdx` wording when it does.
