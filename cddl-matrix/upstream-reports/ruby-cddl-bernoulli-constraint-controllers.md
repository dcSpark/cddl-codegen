# Upstream: ruby `cddl` gem — `generate` is a Bernoulli trial for constraint-carrying controllers

Status: root-caused locally, not yet reported upstream. Candidate for an upstream issue after
checking the gem's tracker for an existing report.

## Environment

- gem `cddl` **0.12.14** (also installed: `cbor-diag` 0.11.8, `abnc` 0.1.1)
- ruby 3.0.2p107, x86_64-linux (WSL2)
- binstub: `~/.local/share/gem/ruby/3.0.0/bin/cddl`

## The behavior

For a type with a constraint control operator, `cddl <file> generate` appears to generate a random
instance of the TARGET type and then self-validate it against the full controlled type. When the
controller constrains the value space, the draw usually lands outside it and the command exits
nonzero — nondeterministically.

Measured (2026-07-12, 8 consecutive runs of the same file):

```
$ printf 'x = uint .and (0..9)\n' > ctl_and.cddl
$ for i in 1..8: cddl ctl_and.cddl generate 1 ; echo $?
1 1 1 1 1 1 1 0        # 7 of 8 draws exit 1; the 1-in-8 success is a random uint landing in 0..9
```

So the exit code is a Bernoulli trial whose success probability is roughly the measure of the
controller's value set within the target type's generator distribution. For `uint .and (0..9)` the
observed rate matches a generator that draws small uints from a modest range; for tighter
constraints the success rate presumably approaches zero.

Distinct from (do not conflate): the exit-65 **parse** gap for inline-composite controller
arguments (`.cbor h'...'` etc.), which is deterministic and separately documented in
`cddl-matrix/upstream-reports/ruby-cddl-inline-composite-control-arg.md`. In the same probe sweep, `ctl.abnf`,
`ctl.abnfb`, `ctl.and` exited 1 (generate-then-self-validate), while `ctl.cborseq`, `ctl.join`,
`ctl.oid`, `ctl.printf` exited 65 (parse). Only the exit-1 family is the Bernoulli class; whether
`.abnf`/`.abnfb` are Bernoulli or deterministic-fail was not measured (they may simply lack
generator support for the controller).

## Why it matters to us

`cddl-matrix/verify.ts` uses the gem as one of two spec oracles; its per-feature ruby verdict for
control ops comes from `generate`. A Bernoulli verdict means:

- committed annotation evidence can flip `ruby=fail` ↔ `ruby=ok` on IDENTICAL input between runs
  (observed live on `ctl.and`, twice: once hand-reverted historically, once caught 2026-07-12 by
  the `verify_cache_transparency` gate's A/B diff — the gate's two verify legs rolled the dice
  differently);
- `verify_cache_transparency` carries a standing ~1-in-5 false-red rate from this class alone
  until the verdict source changes (both legs draw independently);
- any retry/majority absorber is mathematically unable to make the verdict deterministic — the
  spurious direction on a majority-fail op is an OK, so "retry a fail once" makes flips MORE
  likely, and majority-of-N only shrinks, never removes, the flip probability.

The local absorber SHIPPED (current state: `cddl-matrix/README.md` § "Gotchas", the `ruby=` clause
entry): `verify.ts` no longer derives a verdict from `generate` for a value-narrowing controller.
It classifies statically by controller op-name (`lib.ts` `rubyGenerateIsBernoulli`, self-tested at
startup) and reports a deterministic `ruby=` token — `ruby=ok(validate)`/`fail(validate)` from ruby
`validate` over the row's committed spec-valid decode vectors where the catalog has them, else the
stable `ruby=nondet(generate)`. That removes the `verify_cache_transparency` false-red this class
caused; the upstream fix below is still worth reporting so downstream users stop treating the
`generate` exit as a validity oracle.

## What an upstream fix could look like (for the future issue report)

1. Make `generate` constraint-aware (rejection-sample until valid, with a draw cap) — the obvious
   semantic fix; a draw cap keeps pathological controllers (`uint .and (99999..99999)`) from
   hanging.
2. Or expose a `--seed` flag so downstream harnesses can at least pin the dice.
3. Or document that `generate`'s exit code is not a validity oracle for controlled types, so
   downstream users stop treating it as one.

## Repro kit for a cold session

```sh
printf 'x = uint .and (0..9)\n' > /tmp/ctl_and.cddl
for i in $(seq 1 20); do ~/.local/share/gem/ruby/3.0.0/bin/cddl /tmp/ctl_and.cddl generate 1 >/dev/null 2>&1; printf '%s ' $?; done; echo
# expect a mix of 1s and 0s; the 0s are the random draws that landed in 0..9
```
