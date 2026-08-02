# ruby `cddl` gem: float prelude names are validated by value width, not by CBOR head

*Status: DRAFT, not yet filed. Prepared for the maintainer to paste into an issue on the ruby `cddl`
gem tracker. Everything below is reproducible with the commands shown.*

Environment: `cddl` gem **0.12.14** (`cddl --version` → `cddl tool version 0.12.14`), ruby 3.0,
Linux. Every command below is `cddl <spec> validate <cbor-file>`; the exit code is the verdict
(`0` = accept, non-zero = reject).

We are the authors of [cddl-codegen](https://github.com/dcSpark/cddl-codegen), which uses this gem
as one of two reference oracles when certifying CBOR test vectors. While implementing strict
validation of the float prelude names we found a behavior we cannot reconcile with RFC 8610, and one
sub-case we believe is a defect under *any* reading of the spec. We would rather be corrected than
ship a wrong reading, so this report is written to be answerable — the last section states exactly
what we will do if the answer is "ruby is right".

## Summary

For every spec below the shape is a two-element holder so the float sits in a member position:

```cddl
__probe_holder = [0, x]
x = <the float prelude name under test>
```

The gem appears to validate a float data item by computing **the narrowest IEEE width that
represents its VALUE exactly** and comparing that to the prelude name — ignoring the width actually
used on the wire. Two consequences:

1. **It rejects canonical in-set encodings.** `fa 3f c0 00 00` — 1.5 encoded as `#7.26`, a
   single-precision float — is REJECTED against `x = float32`. We believe this is a defect under
   every reading of RFC 8610, including the value-centric one (see § "The part we are confident
   about").
2. **It accepts out-of-set heads.** `fb 3f f8 00 00 00 00 00 00` — 1.5 encoded as `#7.27` — is
   ACCEPTED against `x = float16`. Whether this is a defect depends on the head-vs-value question
   we are asking about (see § "The part we are asking about").

## Reproduction

```console
$ cat > f32.cddl <<'EOF'
__probe_holder = [0, x]
x = float32
EOF

$ printf '\x82\x00\xfa\x3f\xc0\x00\x00' > fa_1_5.cbor      # [0, 1.5] with 1.5 as #7.26
$ cddl f32.cddl validate fa_1_5.cbor ; echo "exit=$?"
exit=1

$ printf '\x82\x00\xfa\x3f\x8c\xcc\xcd' > fa_1_1.cbor      # [0, 1.100000023841858] as #7.26
$ cddl f32.cddl validate fa_1_1.cbor ; echo "exit=$?"
exit=0
```

Both instances are single-precision floats in the `x` position. The first is rejected, the second
accepted; the difference is that 1.5 is also representable as a half-precision float and
1.100000023841858 is not.

The rejection message (truncated) names the expected type as `[:prim, 7, 26]` and reports the
decoded value, so the head appears not to participate in the decision:

```
[1.5, ["prim", 7, 26], "\noccur 0 < 1, not reached at 1 in array [0, 1.5]
 for [:array, [:member, 1, 1, nil, [:int, 0]], [:member, 1, 1, nil, [:prim, 7, 26]]] ...
```

### Full probe matrix

Six prelude names × five instances, all in the `[0, x]` holder. `ACCEPT` = exit 0.

| instance (hex, after the `8200` holder prefix) | value | `float16` | `float32` | `float64` | `float16-32` | `float32-64` | `float` |
|---|---|---|---|---|---|---|---|
| `f93e00` (`#7.25`) | 1.5 | ACCEPT | reject | reject | ACCEPT | reject | ACCEPT |
| `fa3fc00000` (`#7.26`) | 1.5 | ACCEPT | **reject** | reject | ACCEPT | reject | ACCEPT |
| `fb3ff8000000000000` (`#7.27`) | 1.5 | ACCEPT | reject | **reject** | ACCEPT | reject | ACCEPT |
| `fa3f8ccccd` (`#7.26`) | 1.100000023841858 | reject | ACCEPT | reject | ACCEPT | ACCEPT | ACCEPT |
| `fb3ff199999999999a` (`#7.27`) | 1.1 | reject | reject | ACCEPT | reject | ACCEPT | ACCEPT |

Reading the table by column: the verdict depends only on the value's minimal exact width (row 1–3
are the same value and give the same verdict; rows 4 and 5 differ only in value). The three bolded
cells are canonical encodings of a value at exactly the width the name declares, rejected.

Script that produced the table:
<https://github.com/dcSpark/cddl-codegen> — `draft/float-oracle-probe.ts` (it shells out to
`cddl <spec> validate <cbor>` exactly as above).

## The part we are confident about

**`fa 3f c0 00 00` must validate against `float32`, and `fb 3f f8 …` must validate against
`float64`.**

RFC 8610 § 3.8.1 (prelude descriptions) defines the names value-wise:

> "float32"  A number **representable as** a single-precision float [IEEE754] (major type 7,
> additional information 26).

1.5 *is* representable as a single-precision float — exactly, with the encoding `fa 3f c0 00 00`
that the gem rejects. So even on the value-centric reading the gem's own behavior appears to
implement, this instance should be accepted. The rule the gem actually applies is not "representable
as single-precision" but "**not** representable as anything narrower", which we cannot find a basis
for in RFC 8610. Under that rule `float32` and `float64` cannot match any value that happens to be
half-precision-exact — including `0.0`, `1.0`, `2.0` and every small integer-valued float — which
makes the two names unusable for most real protocol fields.

RFC 8610 § 2.2.3 (representation types) points the same way, more strongly:

> CDDL allows the specification of a data item type by referring to the CBOR representation
> (specifically, to major types and additional information […]).

And RFC 8949 § 3.3 is explicit that for major type 7 the width is not a property of the value at
all:

> Like the major types for integers, items of this major type do not carry content data; all the
> information is in the initial bytes (the head).

with Table 3 assigning additional information 25/26/27 to half/single/double precision.

## The part we are asking about

The remaining question is what `#7.25` denotes when a value is representable at more than one width.
Concretely: **should `fb 3f f8 00 00 00 00 00 00` (1.5 as `#7.27`) validate against
`x = float16`?**

- **Our reading — the head.** RFC 8610 App. D writes the prelude as `float16 = #7.25`,
  `float32 = #7.26`, `float64 = #7.27`, and § 2.2.3 says the `#` construction refers to *the CBOR
  representation*. On that reading `float16` matches exactly the data items whose head is `f9`, so
  the `fb`-headed instance does not match.
- **The value reading.** RFC 8610 § 3.8.1's prose ("A number representable as a half-precision
  float") reads value-wise, and on that reading the `fb`-headed 1.5 does match `float16`, because
  1.5 is representable as a half-precision float. This is presumably closer to what the gem intends.

One structural argument we find hard to answer on the value reading. The prelude defines

```cddl
float16-32 = float16 / float32
float32-64 = float32 / float64
float      = float16-32 / float64
```

As *value* sets these unions are redundant: every half-precision-representable number is also
single-precision-representable, so `float16 / float32` is just `float32`, and `float` is just
`float64`. Under the head reading each union adds a genuinely new head and all three definitions
carry information. A prelude that spells out three redundant unions seems less likely than one whose
members are disjoint representation classes.

We would like to know which reading the gem intends, because the answer decides whether our
validator or yours is the one that needs changing.

## What we did, and the branch that would undo it

cddl-codegen now decodes each float prelude name **head-strictly**: `float16` accepts `f9` only,
`float32` accepts `fa` only, `float64` accepts `fb` only, `float16-32` accepts `f9`/`fa`,
`float32-64` accepts `fa`/`fb`, and `float` accepts all three. Anything else is a decode error rather
than a silent narrowing or widening of the value.

Our test corpus certifies a "this CBOR is spec-invalid" vector by requiring **both** reference
oracles to reject it. Eight float vectors cannot meet that bar, so each carries a per-vector
exemption entry that names which oracle still accepts it and cites this document. Four of those
entries name the ruby gem:

| CDDL | instance | our verdict | gem verdict |
|---|---|---|---|
| `x = float16` | `8200fa3fc00000` | reject (head `#7.26` ∉ {`#7.25`}) | ACCEPT |
| `x = float16` | `8200fb3ff8000000000000` | reject (head `#7.27` ∉ {`#7.25`}) | ACCEPT |
| `x = float32` | `8200fb3ff19999a0000000` | reject (head `#7.27` ∉ {`#7.26`}) | ACCEPT |
| `x = float16-32` | `8200fb3ff8000000000000` | reject (head `#7.27` ∉ {`#7.25`,`#7.26`}) | ACCEPT |

(The other four exempt only the second oracle, a rust `cddl` build that performs no float head
validation at all. The gem rejects those four — by value width rather than by head, but it rejects
them.)

**The live branch.** If the value reading is the correct one — if `float16` really does mean "any
number representable as a half-precision float, at whatever width it was written" — then those four
exemptions are wrong, our decoder is over-strict, and the acceptance rule we just shipped has to be
reopened and reverted. We are asking directly rather than assuming: **is the gem's head-insensitive
matching intentional and spec-correct, or is the value-width comparison a bug?**

Note that the two questions are separable, and we think the first is a defect on either answer: even
if head-insensitivity is intended, `fa 3f c0 00 00` should validate against `float32`, because 1.5 is
representable as a single-precision float.

## Provenance

The behavior above was probed on 2026-08-02 against gem 0.12.14. The exemption entries citing this
document live in `cddl-matrix/lib.ts` (`DECODE_REJECT_ORACLE_GAP_EXEMPT`); a drift gate refuses an
entry whose cited writeup is missing, and the vector mint reports an entry as stale as soon as the
named oracle starts rejecting the bytes — so an answer to this report is picked up mechanically
rather than by anyone remembering to re-check.
