# Radix oracle deviations verdict

## Executive summary

- D1 — `thing = [0x2*0x4 tstr]`: RUBY_BUG. RFC 8610 and RFC 9682 both define `occur = [uint] "*" [uint]` and `uint` includes `0x` and `0b` forms. The spec-correct PEG parse is a single occurrence indicator `0x2*0x4` applying to `tstr`, i.e. 2..4 text strings.
- D2 — `thing = #6.0x20(tstr)`: RUBY_BUG. RFC 8610 permits `uint` after `#6.`, and RFC 9682 preserves that through `head-number = uint / ("<" type ">")`. Ruby parses the syntax but converts `"0x20"` with decimal `String#to_i`, producing tag 0 instead of 32.
- D3 — `thing = #7.0x20`: RUBY_BUG. RFC 8610 permits `uint` after `# DIGIT .`; RFC 9682 specifically adds `#7 ["." head-number]`, and says `#7` head numbers 32..255 stand for simple values. Ruby again converts `"0x20"` with decimal `String#to_i`, producing simple/additional-info 0 instead of 32.

## D1 — occurrence bounds `thing = [0x2*0x4 tstr]`

spec_says:

RFC 8610 Appendix B is normative and gives:

> "This appendix is normative." (RFC 8610 Appendix B, lines 2471-2473)

> `grpent = [occur S] [memberkey S] type`
> `       / [occur S] groupname [genericarg]  ; preempted by above`
> `       / [occur S] "(" S group S ")"`
> `occur = [uint] "*" [uint]`
> `      / "+"`
> `      / "?"`
> `uint = DIGIT1 *DIGIT`
> `     / "0x" 1*HEXDIG`
> `     / "0b" 1*BINDIG`
> `     / "0"` (RFC 8610 Appendix B, lines 2527-2546)

RFC 8610 also explains the semantics:

> "An occurrence indicator modifies the group given to its right by requiring the group to match the sequence to be matched exactly for a certain number of times..." (RFC 8610 Appendix B, lines 2842-2850)

RFC 9682 says its updated collected ABNF replaces RFC 8610 Appendix B:

> "the updated collected ABNF syntax in Figure 11 in Appendix A replaces the collected ABNF syntax in Appendix B of [RFC8610]." (RFC 9682 Section 1, lines 86-90)

The RFC 9682 Appendix A ABNF preserves the same occurrence and `uint` productions:

> `grpent = [occur S] [memberkey S] type`
> `       / [occur S] groupname [genericarg]  ; preempted by above`
> `       / [occur S] "(" S group S ")"`
> `occur = [uint] "*" [uint]`
> `      / "+"`
> `      / "?"`
> `uint = DIGIT1 *DIGIT`
> `     / "0x" 1*HEXDIG`
> `     / "0b" 1*BINDIG`
> `     / "0"` (RFC 9682 Appendix A, lines 522-541)

The ambiguity caution is real in a generative reading: because `grpchoice = *(grpent optcom)` and `optcom = S ["," S]` (RFC 8610 Appendix B, lines 2514-2539; RFC 9682 Appendix A, lines 518-534), `0x2*0x4 tstr` might be imagined as two comma-less group entries, value `0x2` followed by occurrence `*0x4 tstr`. However, RFC 8610 makes the Appendix B grammar intentionally PEG-compatible and normative:

> "PEGs ... resolves what would have been ambiguity in generative systems by introducing the concept of 'prioritized choice'." (RFC 8610 Appendix A, lines 2375-2378)

> "The two alternatives listed are to be tested in left-to-right order, locking in the first successful match..." (RFC 8610 Appendix A, lines 2380-2387)

> "Similarly, the occurrence indicators ('?', '*', '+') are 'greedy' in PEG, i.e., they consume as much input as they match..." (RFC 8610 Appendix A, lines 2402-2406)

> "the grammar of CDDL itself, as written in ABNF in Appendix B, can be interpreted both ... as a PEG. This was made possible by ordering the choices in the grammar such that a successful match made on the left-hand side of a '/' operator is always the intended match..." (RFC 8610 Appendix A, lines 2415-2423)

> "A, B" maps to PEG sequence, and "comma is decoration only" (RFC 8610 Appendix A, lines 2442-2444)

Under that PEG reading, the first `grpent` starts with optional `[occur S]`. At the beginning of `0x2*0x4 tstr`, `occur` can match the whole `0x2*0x4`, and the following `S` matches the space before `tstr`. Since PEG repetition/optionals are greedy and successful earlier structure is the intended match, the spec-correct parse is one group entry: occurrence lower bound `0x2` (= 2), upper bound `0x4` (= 4), applied to `tstr`. The alternative "value `0x2`, then occurrence `*0x4 tstr`" is a possible generative decomposition but is excluded as the intended parse by RFC 8610's PEG/greedy interpretation.

ruby_root_cause:

The bundled grammar in the gem allows radix `uint` in `occur`:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/data/cddl.abnf:52-59` contains `occur = [uint] "*" [uint]` and `uint = DIGIT1 *DIGIT / "0x" 1*HEXDIG / "0b" 1*BINDIG / "0"`.

The parse reaches the semantic occurrence converter, but the converter only accepts decimal digits:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/lib/cddl.rb:1906-1927`: `def occur(n)` dispatches on `n.to_s`; the bounded case is `when /\A(\d*)\*(\d*)\z/`, then `$1.to_i` and `$2.to_i`; otherwise it raises `fail "huh #{n.to_s}"`.

Concrete repro:

```sh
tmp=$(mktemp -d /tmp/cddl-radix.XXXXXX)
printf 'thing = [0x2*0x4 tstr]\n' > "$tmp/d1.cddl"
printf '8261616162' | xxd -r -p > "$tmp/array2.cbor"
RUBYOPT=-rset ruby /home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/bin/cddl "$tmp/d1.cddl" validate "$tmp/array2.cbor"
```

Observed output included:

```text
/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/lib/cddl.rb:1927:in `occur': huh 0x2*0x4 (RuntimeError)
exit=1
```

Control repro with decimal bounds: `thing = [2*4 tstr]` validated a two-element text-string array and a four-element text-string array with `exit=0`, while a one-element text-string array failed with `occur 1 < 2`.

upstream_status:

RubyGems public metadata checked via browser on 2026-07-06 showed the public `cddl` gem page at `https://rubygems.org/gems/cddl` listing version `0.12.13` as current, released March 27, 2026, with recent versions `0.12.13`, `0.12.12`, `0.12.11`, `0.12.10`, `0.12.9` (RubyGems page lines 6-24 and 69-71). The local installed gem is `0.12.14` (`gem list '^cddl$' -a` output: `cddl (0.12.14)`; `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/cddl.gemspec:3` says `s.version = '0.12.14'`). I found no newer public gem to install/test.

Shell network access was unavailable: `git ls-remote --tags https://github.com/cabo/cddl.git` failed with `Could not resolve host: github.com`, and `gem list -r '^cddl$' -a` returned no data. Browser checks found the RubyGems metadata above, but I found no specific upstream issue or commit for this radix-occurrence bug.

verdict:

RUBY_BUG. The RFC 8610 and RFC 9682 grammar clearly permits radix `uint` bounds in `occur`; RFC 8610's normative PEG discussion selects the `0x2*0x4` occurrence-indicator parse over the value-then-occurrence decomposition. Ruby's grammar accepts the syntax, but `lib/cddl.rb:1914` only recognizes decimal bounded occurrences.

recommended_test_expectation:

A spec-correct Rust implementation should parse `thing = [0x2*0x4 tstr]` as an array containing 2 to 4 text strings. The Rust test should assert that `[ "a", "b" ]`, `[ "a", "b", "c" ]`, and `[ "a", "b", "c", "d" ]` validate, while one text string and five text strings fail. It should not assert Ruby-oracle parity for this case; Ruby 0.12.14 is wrong here.

## D2 — tag head number `thing = #6.0x20(tstr)`

spec_says:

RFC 8610 Appendix B permits `uint` after `#6.`:

> `type2 = value`
> `      / typename [genericarg]`
> `      / "(" S type S ")"`
> `      / "{" S group S "}"`
> `      / "[" S group S "]"`
> `      / "~" S typename [genericarg]`
> `      / "&" S "(" S group S ")"`
> `      / "&" S groupname [genericarg]`
> `      / "#" "6" ["." uint] "(" S type S ")"`
> `      / "#" DIGIT ["." uint]                ; major/ai`
> `      / "#"                                 ; any` (RFC 8610 Appendix B, lines 2498-2508)

> `uint = DIGIT1 *DIGIT`
> `     / "0x" 1*HEXDIG`
> `     / "0b" 1*BINDIG`
> `     / "0"` (RFC 8610 Appendix B, lines 2543-2546)

RFC 8610 prose for tags agrees that the number is an unsigned integer:

> "A type can make use of a CBOR tag (major type 6) by using the representation type notation, giving #6.nnn(type) where nnn is an unsigned integer giving the tag number..." (RFC 8610 Section 3.6, lines 1372-1377)

RFC 9682 replaces the collected ABNF and changes this position from `uint` to `head-number`, but `head-number` still includes `uint`:

> `type2 = value`
> `      / typename [genericarg]`
> `      / "(" S type S ")"`
> `      / "{" S group S "}"`
> `      / "[" S group S "]"`
> `      / "~" S typename [genericarg]`
> `      / "&" S "(" S group S ")"`
> `      / "&" S groupname [genericarg]`
> `      / "#" "6" ["." head-number] "(" S type S ")"`
> `      / "#" "7" ["." head-number]`
> `      / "#" DIGIT ["." uint]                ; major/ai`
> `      / "#"                                 ; any`
> `head-number = uint / ("<" type ">")` (RFC 9682 Appendix A, lines 500-512)

> `uint = DIGIT1 *DIGIT`
> `     / "0x" 1*HEXDIG`
> `     / "0b" 1*BINDIG`
> `     / "0"` (RFC 9682 Appendix A, lines 538-541)

RFC 9682 Section 3.2 also describes the change:

> "The existing ABNF syntax for expressing tags in CDDL is as follows: ... `type2 =/ "#" "6" ["." uint] "(" S type S ")"`" (RFC 9682 Section 3.2, lines 303-310)

> "This means tag numbers can only be given as literal numbers (uints)." (RFC 9682 Section 3.2, lines 312-317)

> "This update extends the syntax to the following: ... `type2 =/ "#" "6" ["." head-number] "(" S type S ")" ... head-number = uint / ("<" type ">")`" (RFC 9682 Section 3.2, lines 319-327)

> "For #6, the head-number stands for the tag number." (RFC 9682 Section 3.2, lines 329-334)

Therefore `#6.0x20(tstr)` means CBOR tag number 32 containing a text string under both RFC 8610 and RFC 9682.

ruby_root_cause:

The gem bundles an updated grammar with `headnumber`:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/data/cddl.abnf:16-18` has `type1 = ... / "#" "6" ["." headnumber] "(" S type S ")" / "#" "7" ["." headnumber] / "#" DIGIT ["." uint]`; line 26 has `headnumber = uint / ("<" type ">")`; lines 56-59 include radix `uint`.

The semantic converter mishandles that radix `uint`:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/lib/cddl.rb:1809-1815`: for strings matching `\A#(\d+)`, it builds `s = [:prim, maj, *n.children(:uint).map(&:to_s).map(&:to_i)]`; then if `tn = n.headnumber` and `ui = tn.uint`, it appends `ui.to_s.to_i`.

Ruby `String#to_i` defaults to base 10, so `"0x20".to_i` is `0`. Concrete confirmation:

```sh
ruby -e 'p "0x20".to_i; p eval("0x20")'
```

Observed:

```text
0
32
```

The numeric literal evaluator would handle radix correctly, but it is only used for `value` nodes:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/lib/cddl.rb:1579-1583`: non-hexfloat values use `val = eval(s)`.

Validation then compares the decoded CBOR tag against the wrongly stored tag number:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/lib/cddl.rb:1394-1396`: `CBOR::Tagged === d && (Integer === where[2] ? d.tag == where[2] : validate1a(d.tag, where[2])) && validate1a(d.data, where[3])`.

Concrete repro:

```sh
tmp=$(mktemp -d /tmp/cddl-radix.XXXXXX)
printf 'thing = #6.0x20(tstr)\n' > "$tmp/d2.cddl"
printf 'd8206161' | xxd -r -p > "$tmp/tag32a.cbor"
RUBYOPT=-rset ruby /home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/bin/cddl "$tmp/d2.cddl" validate "$tmp/tag32a.cbor"
```

Observed:

```text
CDDL validation failure (nil for 32("a")):
[#<struct CBOR::Tagged tag=32, value="a">, [:prim, 6, 0, [:prim, 3]], ""]
[32("a"), ["prim", 6, 0, ["prim", 3]], ""]
exit=1
```

Introspection also produced `[:type1, [:prim, 6, 0, [:prim, 3]]]` for `thing = #6.0x20(tstr)`. The decimal control `thing = #6.32(tstr)` validated the same `d8206161` input with `exit=0`.

upstream_status:

RubyGems public metadata checked via browser on 2026-07-06 showed `cddl` version `0.12.13` as current and released March 27, 2026 (RubyGems page lines 6-24 and 69-71). The local installed `0.12.14` is newer than that public page (`gem list '^cddl$' -a`: `cddl (0.12.14)`; local `cddl.gemspec:3`: `s.version = '0.12.14'`). I found no newer public gem to install/test.

Shell network access was unavailable for upstream cloning/history search: `git ls-remote --tags https://github.com/cabo/cddl.git` failed with `Could not resolve host: github.com`. Browser checks did not turn up a specific upstream issue or commit for radix `headnumber` conversion.

verdict:

RUBY_BUG. The spec permits radix `uint` for tag numbers in RFC 8610 and still permits it in RFC 9682 through `head-number = uint / ("<" type ">")`. Ruby's parser accepts the syntax but semantically decodes the head number with base-10 `String#to_i`.

recommended_test_expectation:

A spec-correct Rust implementation should accept `thing = #6.0x20(tstr)` and validate CBOR `d8206161` (tag 32 around `"a"`). The Rust test should assert that tag 32 with a text string is valid, tag 32 with a non-text value is invalid, and a different tag such as tag 0 around `"a"` is invalid. Do not use Ruby 0.12.14 as an oracle for this case.

## D3 — simple value head `thing = #7.0x20`

spec_says:

RFC 8610 Appendix B permits `uint` after `# DIGIT .`, which includes `#7.0x20`:

> `type2 = value`
> `      / typename [genericarg]`
> `      / "(" S type S ")"`
> `      / "{" S group S "}"`
> `      / "[" S group S "]"`
> `      / "~" S typename [genericarg]`
> `      / "&" S "(" S group S ")"`
> `      / "&" S groupname [genericarg]`
> `      / "#" "6" ["." uint] "(" S type S ")"`
> `      / "#" DIGIT ["." uint]                ; major/ai`
> `      / "#"                                 ; any` (RFC 8610 Appendix B, lines 2498-2508)

> `uint = DIGIT1 *DIGIT`
> `     / "0x" 1*HEXDIG`
> `     / "0b" 1*BINDIG`
> `     / "0"` (RFC 8610 Appendix B, lines 2543-2546)

RFC 8610 prose clarifies that `#7.25` is a representation type notation at the data-model level:

> "although this notation is based on the CBOR serialization, it is about a set of values at the data model level, e.g., '#7.25' specifies the set of values that can be represented as half-precision floats..." (RFC 8610 Section 2, lines 735-740)

RFC 9682 changes `#7` from the general `"#" DIGIT ["." uint]` case into a specific `head-number` case:

> `type2 = value`
> `      / typename [genericarg]`
> `      / "(" S type S ")"`
> `      / "{" S group S "}"`
> `      / "[" S group S "]"`
> `      / "~" S typename [genericarg]`
> `      / "&" S "(" S group S ")"`
> `      / "&" S groupname [genericarg]`
> `      / "#" "6" ["." head-number] "(" S type S ")"`
> `      / "#" "7" ["." head-number]`
> `      / "#" DIGIT ["." uint]                ; major/ai`
> `      / "#"                                 ; any`
> `head-number = uint / ("<" type ">")` (RFC 9682 Appendix A, lines 500-512)

> `uint = DIGIT1 *DIGIT`
> `     / "0x" 1*HEXDIG`
> `     / "0b" 1*BINDIG`
> `     / "0"` (RFC 9682 Appendix A, lines 538-541)

RFC 9682 Section 3.2 explicitly discusses simple values:

> "Similar considerations apply to simple values (#7.xx)." (RFC 9682 Section 3.2, lines 312-317)

> `type2 =/ "#" "6" ["." head-number] "(" S type S ")"`
> `       / "#" "7" ["." head-number]`
> `head-number = uint / ("<" type ">")` (RFC 9682 Section 3.2, lines 319-327)

> "For #7, the head-number stands for the simple value if it is in the ranges 0..23 or 32..255 (as per Section 3.3 of RFC 8949 [STD94], the simple values 24..31 are not used). For 24..31, the head-number stands for the 'additional information', e.g., #7.25 or #7.<25> is a float16, etc. (All ranges mentioned here are inclusive.)" (RFC 9682 Section 3.2, lines 329-334)

Therefore, under RFC 9682, `#7.0x20` is allowed and the head number is 32, which is in the simple-value range 32..255. It denotes simple value 32, not additional information 0. RFC 9682 does restrict the interpretation of `#7` head numbers, but that restriction supports `0x20` because 32 is specifically in the allowed simple-value range.

ruby_root_cause:

The root cause is the same base-10 conversion path as D2:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/data/cddl.abnf:17` has `"#" "7" ["." headnumber]`; line 26 has `headnumber = uint / ("<" type ">")`; lines 56-59 include radix `uint`.

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/lib/cddl.rb:1809-1815`: the `#...` semantic converter appends `ui.to_s.to_i` for `headnumber` `uint`, so `"0x20"` becomes 0.

For simple values, validation then sees `where[2] == 0`:

> `/home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/lib/cddl.rb:1397-1423`: for major type 7, it handles `nil`, an `Array` head-number type, and decimal `25, 26, 27`; otherwise it raises/fails via `fail [:val7, d, where].inspect`.

In this observed CLI path the failure is reported as invalid rather than surfacing the internal `fail`, but the internal rule confirms the wrong head number:

```text
[:type1, [:prim, 7, 0]]
```

Concrete repro:

```sh
tmp=$(mktemp -d /tmp/cddl-radix.XXXXXX)
printf 'thing = #7.0x20\n' > "$tmp/d3.cddl"
printf 'f820' | xxd -r -p > "$tmp/simple32.cbor"
RUBYOPT=-rset ruby /home/sebdev20/.local/share/gem/ruby/3.0.0/gems/cddl-0.12.14/bin/cddl "$tmp/d3.cddl" validate "$tmp/simple32.cbor"
```

Observed:

```text
CDDL validation failure (nil for simple(32)):
[#<struct CBOR::Simple value=32>, [:prim, 7, 0], ""]
[simple(32), ["prim", 7, 0], ""]
exit=1
```

The decimal control `thing = #7.32` validated the same `f820` input with `exit=0`.

upstream_status:

RubyGems public metadata checked via browser on 2026-07-06 showed `cddl` version `0.12.13` as current and released March 27, 2026 (RubyGems page lines 6-24 and 69-71). The local installed `0.12.14` is newer than that public page (`gem list '^cddl$' -a`: `cddl (0.12.14)`; local `cddl.gemspec:3`: `s.version = '0.12.14'`). I found no newer public gem to install/test.

Shell network access was unavailable for upstream cloning/history search: `git ls-remote --tags https://github.com/cabo/cddl.git` failed with `Could not resolve host: github.com`. Browser checks did not turn up a specific upstream issue or commit for radix `#7` head-number conversion.

Optional cross-oracle note: `/home/sebdev20/.cargo/bin/cddl --version` reported `cddl 0.10.5`, but it rejected all three radix cases at parse time, e.g. `#7.0x20` failed with `expected one of: type assignment ('=' or '/='), group assignment ('=' or '//='), generic parameters '<...>'`. I did not use that older tool as authority for the verdict.

verdict:

RUBY_BUG. RFC 8610 permits `#7.0x20` through `"#" DIGIT ["." uint]`; RFC 9682 specifically permits `#7 ["." head-number]` with `head-number = uint / ("<" type ">")` and says 32..255 are simple values. Ruby accepts the syntax but changes the head number from 32 to 0 during semantic conversion.

recommended_test_expectation:

A spec-correct Rust implementation should accept `thing = #7.0x20` and validate CBOR `f820` (`simple(32)`). The Rust test should assert that simple value 32 is valid and a different simple value is invalid. For RFC 9682 semantics, it should also preserve the distinction that `#7.0x18` through `#7.0x1f` are the CBOR additional-information forms 24..31, while `#7.0x20` is the simple value 32.
## D4 — uppercase hexfloat exponent `thing = 0x1P3` (found post-verdict, same family)

spec_says: RFC 8610 Appendix B `hexfloat = ["-"] "0x" 1*HEXDIG ["." 1*HEXDIG] "p" exponent`; the
Appendix B preamble states quote-delimited ABNF strings are case-insensitive, so "p" matches "P".
`0x1P3` is a valid hexfloat (8.0). RFC 9682 Appendix A keeps the production unchanged.

ruby_root_cause: cddl-0.12.14 lib/cddl.rb:1579 routes hexfloats via the lowercase-only regex
`/p.*\d\z/` to scanf("%a"); `0x1P3` misses it and falls to `eval(s)` (line 1582), where Ruby's own
literal syntax raises SyntaxError ("unexpected constant"). Lowercase `0x1p3` validates fine (exit 0).

verdict: RUBY_BUG (same family as D1–D3: the gem substitutes Ruby-language literal machinery for the
CDDL ABNF). recommended_test_expectation: a spec-correct implementation accepts `0x1P3` as 8.0.
