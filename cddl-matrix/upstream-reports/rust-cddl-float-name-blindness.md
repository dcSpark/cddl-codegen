# rust `cddl` crate: the six float prelude names all validate as "is it a float"

*Status: DRAFT, not yet filed. Prepared for the maintainer to paste into an issue on the rust `cddl`
crate tracker. Everything below is reproducible with the commands shown.*

Environment: rust `cddl` **0.10.6**, built from the `local-fixes` branch of our fork checkout at
`ac1b98e` (the build this repo pins as its validation oracle; the behavior below is identical on a
stock `cargo install cddl` 0.10.x — `local-fixes` carries no float-name changes). Every command
below is `cddl --ci validate --cddl <spec> --cbor <cbor-file>`; the exit code is the verdict
(`0` = accept, non-zero = reject). The `--ci` flag matters: without it the CLI prints the validation
error and still exits 0.

We are the authors of [cddl-codegen](https://github.com/dcSpark/cddl-codegen), which uses this crate
as one of two reference oracles when certifying CBOR test vectors. Certification normally requires
BOTH oracles to reject bytes we call spec-invalid. Seven of our float vectors cannot meet that bar,
because this validator accepts every float instance against every float prelude name. This report
states the behavior, states our reading of the spec, and — in the last section — states exactly what
we will change if the answer is "the validator is right".

## Summary

For every spec below the shape is a two-element holder so the float sits in a member position:

```cddl
__probe_holder = [0, x]
x = <the float prelude name under test>
```

RFC 8610 App. D defines six float names:

```cddl
float16 = #7.25
float32 = #7.26
float64 = #7.27
float16-32 = float16 / float32
float32-64 = float32 / float64
float = float16-32 / float64
```

**The validator does not distinguish them.** Six names × six float instances = 36 probes, and all 36
ACCEPT. The name is checked only for "is this data item a float": an integer or a text string in the
`x` position is correctly rejected, and that is the whole of the check.

## Reproduction

```console
$ cat > f16.cddl <<'EOF'
__probe_holder = [0, x]
x = float16
EOF

$ printf '\x82\x00\xfb\x3f\xf1\x99\x99\x99\x99\x99\x9a' > fb_1_1.cbor   # [0, 1.1] as #7.27
$ cddl --ci validate --cddl f16.cddl --cbor fb_1_1.cbor ; echo "exit=$?"
exit=0

$ printf '\x82\x00\x05' > int_5.cbor                                    # [0, 5]
$ cddl --ci validate --cddl f16.cddl --cbor int_5.cbor ; echo "exit=$?"
exit=1
```

1.1 is a double-precision value: it is not representable in binary32 or binary16 at all, at any
width, so it is outside `float16` under every reading of RFC 8610 we can construct. It validates.

### Full probe matrix

Six prelude names × six instances, all in the `[0, x]` holder (hex shown after the `8200` holder
prefix). `ACCEPT` = exit 0.

| instance | value | `float16` | `float32` | `float64` | `float16-32` | `float32-64` | `float` |
|---|---|---|---|---|---|---|---|
| `f93e00` | 1.5 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| `fa3fc00000` | 1.5 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| `fb3ff8000000000000` | 1.5 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| `fa3f8ccccd` | 1.100000023841858 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| `fb3ff199999999999a` | 1.1 | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| `fb7ff8000000000000` | NaN | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT | ACCEPT |

Non-float controls in the same position (`[0, 5]`, `[0, "xx"]`) are rejected by all six names, so the
member is being validated — the float NAME is what carries no information.

The whole table reproduces with one loop — no tooling of ours involved:

```bash
for name in float16 float32 float64 float16-32 float32-64 float; do
  printf '__probe_holder = [0, x]\nx = %s\n' "$name" > probe.cddl
  for hex in 8200f93e00 8200fa3fc00000 8200fb3ff8000000000000 \
             8200fa3f8ccccd 8200fb3ff199999999999a 8200fb7ff8000000000000; do
    printf "$(echo "$hex" | sed 's/../\\x&/g')" > probe.cbor
    cddl --ci validate --cddl probe.cddl --cbor probe.cbor >/dev/null 2>&1 \
      && echo "$name $hex ACCEPT" || echo "$name $hex reject"
  done
done
```

## What we believe the names mean

**The six names partition the float values by their SHORTEST lossless CBOR form.** `float16` is the
values whose shortest form is `#7.25`, `float32` `#7.26`, `float64` `#7.27`, with `float16-32` and
`float32-64` spanning two adjacent classes and `float` all three. Membership is a predicate on the
decoded value; the head the value arrived under is irrelevant in both directions.

Three things carry that reading.

1. **The names constrain values, not encodings.** RFC 8610 § 2.2.3 says of the `#7.x` notation that
   it "is about a set of values at the data model level […] it does not mandate that these values
   also do have to be serialized as half-precision floats: CDDL does not provide any language means
   to restrict the choice of serialization variants." § 3.3 spells the prelude descriptions
   value-wise: "float32 — A number **representable as** a single-precision float". A validator that
   judged the head instead would reject the output of every conforming preferred-serialization
   encoder (RFC 8949 § 4.1), which writes each value at its shortest lossless head.
2. **The unions must not be redundant.** Under the naive "representable as" (nested) reading, every
   binary16-representable number is binary32-representable, so `float16 / float32` collapses to
   `float32` and `float` collapses to `float64`; the prelude would be spelling out three unions that
   add nothing. Under the shortest-form partition the classes are disjoint and all six names carry
   information. That is the only reading we found under which the prelude is not redundant.
3. **The reference implementation agrees.** The ruby `cddl` gem 0.12.14, written by RFC 8610's
   author, implements exactly this partition — head-independently. Same six names × the same six
   instances, same holder, `cddl <spec> validate <cbor>`:

   | instance | value | `float16` | `float32` | `float64` | `float16-32` | `float32-64` | `float` |
   |---|---|---|---|---|---|---|---|
   | `f93e00` | 1.5 | ACCEPT | reject | reject | ACCEPT | reject | ACCEPT |
   | `fa3fc00000` | 1.5 | ACCEPT | reject | reject | ACCEPT | reject | ACCEPT |
   | `fb3ff8000000000000` | 1.5 | ACCEPT | reject | reject | ACCEPT | reject | ACCEPT |
   | `fa3f8ccccd` | 1.100000023841858 | reject | ACCEPT | reject | ACCEPT | ACCEPT | ACCEPT |
   | `fb3ff199999999999a` | 1.1 | reject | reject | ACCEPT | reject | ACCEPT | ACCEPT |
   | `fb7ff8000000000000` | NaN | ACCEPT | reject | reject | ACCEPT | reject | ACCEPT |

   Read the gem's table by row: the first three rows are one value at three heads and give one
   verdict, so the head does not participate. Read it by column: each value lands in exactly one of
   `float16`/`float32`/`float64`, so the classes are disjoint. (Canonical quiet NaN's shortest form
   is `f9 7e00`, which is why NaN is a `float16` value and a `float64` field has no NaN.)

cddl-codegen implements the partition, so our generated decoders and the gem agree on all 36 probes
above.

## What we rely on this for

Our decode-conformance catalog certifies "these bytes are spec-invalid" by requiring both reference
oracles to reject them. Seven float vectors cannot clear that bar against this validator, so each
carries a per-vector exemption naming `rust` as the accepting oracle and citing this document:

| CDDL | instance | value / shortest form | our verdict + the gem's | this validator |
|---|---|---|---|---|
| `x = float16` | `8200fa3f8ccccd` | 1.100000023841858 / `#7.26` | reject (a `float32` value) | ACCEPT |
| `x = float16` | `8200fb3ff199999999999a` | 1.1 / `#7.27` | reject (a `float64` value) | ACCEPT |
| `x = float16-32` | `8200fb3ff199999999999a` | 1.1 / `#7.27` | reject (a `float64` value) | ACCEPT |
| `x = float32` | `8200f93e00` | 1.5 / `#7.25` | reject (a `float16` value) | ACCEPT |
| `x = float32-64` | `8200f93e00` | 1.5 / `#7.25` | reject (a `float16` value) | ACCEPT |
| `x = float64` | `8200f93e00` | 1.5 / `#7.25` | reject (a `float16` value) | ACCEPT |
| `x = float64` | `8200fa3fc00000` | 1.5 / `#7.25` | reject (a `float16` value) | ACCEPT |

Every one of the seven is rejected by the ruby gem, so the exemption narrows the certification to
that oracle plus the argument above rather than removing it.

## The branch that would undo this

**If this validator is right — if all six float prelude names really do denote the same set (any
float value at any head) — then our decoders are over-strict, all seven exemptions are wrong, and the
shortest-form partition we shipped has to be reopened and replaced.** Concretely, taking that branch
means: the seven catalog vectors above stop being spec-invalid and become accepts; the five
constrained float names stop enforcing anything and lose their enforcement evidence; and
`float16-32` / `float32-64` become redundant spellings of `float`, which is the consequence we could
not reconcile with the prelude in the first place.

So the questions we are actually asking are:

1. Is the collapse of the six names into one is-float test intentional, or is it a missing
   additional-information/value check?
2. If intentional: what distinguishes `float16-32` from `float`, given both would then accept every
   float value at every head?

We would rather be corrected than keep an exemption we do not deserve. A "the validator is right"
answer is an actionable one for us, not a dispute — see the paragraph above for exactly what moves.

## Provenance

The behavior above was probed on 2026-08-03 against the `local-fixes` build at `ac1b98e` and gem
0.12.14. The exemption entries citing this document live in `cddl-matrix/lib.ts`
(`DECODE_REJECT_ORACLE_GAP_EXEMPT`); a drift gate refuses an entry whose cited writeup is missing,
and the vector mint reports an entry as stale as soon as the named oracle starts rejecting the bytes
— so an answer to this report is picked up mechanically rather than by anyone remembering to
re-check.
