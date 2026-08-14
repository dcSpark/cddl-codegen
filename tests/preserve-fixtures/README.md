# preserve-fixtures

File-fixture cases for `comment_preserve::preserve`, driven by
`src/tests/preserve_fixture_tests.rs`. Each directory holds `old.rs` + `new.rs` and exactly one
expectation: `expected.rs` (exact merge output, blessed with `BLESS_PRESERVE_FIXTURES=1`) or
`error.txt` (a substring the hard `PreserveError` message must contain). Intent lives here rather
than inside `old.rs` because any comment added to `old.rs` would itself be a merge input. See the
harness module docs for the byte-for-byte assertion and the three cross-cutting properties
(idempotent fixed point, never-silent, `changed==false ⇒ output==new`).

`.gitattributes` pins `* -text` so the CRLF cases keep their `\r\n` bytes across checkouts.

Ownership note: outside a `cddl-codegen:` block every comment is tool-owned, so a fixture whose
INTENT is "a user comment" marks it with a `// cddl-codegen:keep` marker (the bare form, so the
original comment text stays verbatim in `old.rs`/`expected.rs`); a fixture whose intent is "a tool
comment" leaves it unmarked and asserts it is dropped, self-cancelled, or trapped.

## `keep` cases — declaring a user comment

- `keep_inline_transfers` — `// cddl-codegen:keep <text>`: the whole line is the comment and travels verbatim, marker included (an unmarked copy would be unclassified next run).
- `keep_run_claims_contiguous_comments` — a bare marker claims the 3-line `//` run directly below it as one unit.
- `keep_run_claims_doc_comments` — the bare form over `///` lines: the only form that can carry doc comments.
- `keep_run_stops_at_blank_line` — a blank line ends the claimed run, so the comment below it is unclassified and trapped.
- `keep_rustfmt_folded_tail_marker` — an inline marker `rustfmt` folded onto a match tail arm's `}`; the entry unfold moves it own-line and it still places.
- `keep_positional_diversity_tails` — one file spanning keep markers at the last block statement,
  if/else tail, struct-literal last field, last enum variant, nested-module closing brace, and impl
  closing brace; each carries its own `POSITIONAL_DIVERSITY KEEP …` payload.
- `keep_inside_replaced_span_conflict_fails_loudly` — a `keep` block whose anchor lands strictly inside a replaced byte span (op-composition conflict); the replace still splices, the `keep` block fails loudly as "a user comment", not "a user code block".

## Unclassified-comment cases (the ownership rule)

- `reworded_tool_comment_fails_loudly_with_hint` — a tool comment whose text changed upstream (one word, same anchor — the real `ponytail:` → `careful:` regression's shape): the new wording appears once, the old only inside the `compile_error!`, and the hint names the new text.
- `deleted_tool_comment_fails_loudly` — a tool comment removed upstream is trapped, not kept forever as a phantom.
- `reflowed_tool_paragraph_fails_loudly` — a re-wrapped tool paragraph: the two lines whose wrap changed surface as loud residue, nothing is spliced mid-paragraph, and the hint lists exactly those two new lines.
- `unmarked_user_comment_teaches_keep` — an unmarked hand-written comment: the message names `// cddl-codegen:keep` so the notation is discoverable from the build output.

## Comment cases (migrated from `comment_preserve.rs`'s inline suite; name = old test name)

- `identity_transfers_own_line_comment` — file tokens unchanged, `keep`-marked comment transfers at same index.
- `header_self_cancels_no_duplicate` — comment-free regen is a byte-identical no-op.
- `trailing_comment_fails_loudly_with_hint` — a trailing (end-of-line) comment can't be re-placed.
- `per_item_transfer_with_unrelated_item_changed` — comment in an unchanged item survives an edit elsewhere.
- `unique_statement_reanchors_in_changed_body` — changed body, but the annotated statement is still unique.
- `changed_statement_fails_loudly` — the annotated statement itself changed → referent suspect.
- `non_unique_statement_fails_loudly` — the annotated statement now appears twice → ambiguous.
- `vanished_item_fails_loudly` — the enclosing item was deleted.
- `sentinel_block_carries_forward_across_two_regens` — an existing fail-loudly block carries forward verbatim.
- `idempotency_fixed_point` — two placed comments; the fixed-point property does the idempotency check.
- `fail_loudly_block_lands_after_leading_inner_attribute` — `compile_error!` must sit after `#![…]`.
- `deleted_duplicate_statement_fails_loudly_not_retargets` — a deleted duplicate's comment must not re-attach to the survivor.
- `generator_trailing_comment_cancels_by_text` — a generator trailing comment self-cancels by text.
- `shifted_generator_comment_not_duplicated` — a generator comment whose anchor shifted isn't duplicated.
- `stale_tool_doc_dropped_user_doc_on_undocumented_item_kept` — both halves in one case: an unmarked stale tool `///` drops, while a `keep`-marked user `///` on an undocumented item stays. Kept as one fixture because the value is the CONTRAST — the two docs sit in the same file and the marker is the only thing telling them apart.
- `same_key_count_change_fails_loudly` — same-keyed item count changed → occurrence match unsound.
- `preceding_comment_on_reordered_same_key_items_fails_loudly` — two changed same-key items → ordering untrustworthy.
- `comma_terminated_statement_reanchors` — a struct-literal field comment re-anchors at its `,`.
- `doc_on_vanished_item_drops_silently` — tool docs on a deleted item drop, not trapped in `compile_error!`.
- `crlf_old_file_self_cancels` — a CRLF prior output is a no-op against its LF twin.
- `crlf_user_comment_still_preserved` — a CRLF user comment is preserved without a stray `\r`.
- `eof_comment_preserved_at_file_end` — a comment past the last token stays at EOF.
- `comment_before_code_on_same_line_fails_loudly` — `/* note */ code` is out of own-line scope.

## Cross-version cases (a regen that also rewrote generated CODE)

Every other case here regenerates at the SAME tool version, so `old.rs` and `new.rs` agree on
generated code bytes except where the fixture deliberately drifts one item. A tool UPGRADE is a
different shape — it adds code tokens and rewrites others across the whole file at once, shifting
every anchor and drifting the items the rewrite touched — and that shape is what the two cases
below cover. The class exists because the anchored suppressions (positional self-cancel, insertion-
point dedup) cancel a generator comment only where the two sides agree on position: under an
upgrade they cannot fire, and only the text-presence check keeps a comment the fresh emission
carries verbatim out of a `compile_error!`.

- `cross_version_std_to_alloc_rewrite_self_cancels` — the no_std upgrade's shape (a consumer-filed
  false positive): `new` adds `extern crate alloc;` + `use alloc::…` and rewrites
  `::std::borrow::Cow` → `alloc::borrow::Cow` and `std::collections::BTreeSet` →
  `alloc::collections::BTreeSet`. A 3-line tool comment run sits directly above the rewritten
  `let` — its annotated statement is the one that changed, so no tier can re-anchor it — and `new`
  carries the run verbatim. Output is byte-identical to `new.rs`: zero sentinel blocks.
- `cross_version_rewrite_traps_only_the_reworded_line` — the same file with one of the three
  comment lines reworded upstream. Only that line traps; the two `new` still carries suppress. Pins
  both that the trap is not weakened and that suppression is per LINE, not per run.

## Insert-block cases

- `insert_identity` — block placed above its anchor when the file is otherwise unchanged.
- `insert_per_item` — block in an unchanged item survives a change to an unrelated item.
- `insert_unique_statement` — block re-anchors to a still-unique statement in a changed body.
- `insert_vanished_anchor` — block's following anchor vanished → entire block trapped in `compile_error!`.
- `insert_failed_block_carry_forward` — a trapped block carries forward verbatim on the next regen.
- `insert_with_interior_comment` — block's interior comment is excluded from the comment pass.
- `insert_adjacent_blocks` — two back-to-back blocks keep their top-to-bottom order.
- `insert_at_eof` — block with no following code lands at end of file.
- `insert_crlf` — a CRLF block is placed as clean LF.
- `insert_rustfmt_folded_tail_marker` — an insert block at a match's tail arm in `rustfmt`'s canonical folded form (`} // cddl-codegen:insert-start`, continuation lines re-indented); the entry unfold recognizes it and places it own-line above the match's closing brace.
- `insert_positional_diversity_tails` — the same six tail positions, with grammar-valid inserted
  statements, a struct field, enum variant, module item, and impl item, all marked by distinct
  `POSITIONAL_DIVERSITY INSERT …` payloads.

## Replace-block cases

A replace block records the generated code it overrides (`//`-commented under `replaces`); that
recorded original is both the placement anchor (lex it, find the token run in the regenerated item,
splice the user block over it) and the drift detector (needle gone ⇒ the generator changed ⇒ fail
loudly). When the whole enclosing item regenerates token-identically the block places by its own
position (the item-identity fast path — no uniqueness needed); otherwise the needle must be unique
on BOTH sides. Idempotent re-splice is not a separate fixture —
the harness's fixed-point property (`preserve(expected, new) == expected`) exercises it on every
`expected.rs` case below.

- `replace_one_line_swap` — one line of generated code swapped (case 1).
- `replace_multi_statement_swap` — a multi-statement region swapped (case 2).
- `replace_whole_fn_in_impl` — a whole member fn inside an `impl` swapped (case 3); the needle is the fn, the enclosing top-level item is the impl.
- `replace_whole_impl` — a whole top-level `impl` swapped; same code path, bigger needle.
- `replace_identity_tier_still_fires` — a file with a replace block whose generator output is unchanged; an unrelated plain comment still transfers via the identity tier (the reconstruction payoff).
- `replace_needle_midline_match_arm` — the needle matches mid-line in a one-liner match arm; the splice is byte-range, not line-based.
- `replace_interior_generator_comment_deleted_exterior_kept` — a generator comment interior to the replaced byte span is deleted with it; one exterior to the span survives.
- `replace_recorded_original_with_commented_generator_comment` — a recorded-original line that is itself a comment (`// //`) uncomments to a comment and lexes away (inert), so the needle ignores it.
- `replace_empty_user_section_pinned` — an empty user section (undocumented deletion) is pinned: the recorded original is deleted and the tag-only block sits in its place.
- `replace_insert_block_above_lands_above` — an insert block immediately above a replace block lands ABOVE the spliced code.
- `replace_insert_plain_comment_coexist` — a replace block, an insert block, and two plain comments coexist in one file.
- `replace_duplicate_fragment_positional` — a balanced fragment appears 3× in one item and a replace block over one occurrence records only that bare (non-unique) fragment; when the item regenerates token-identically the block places by its own position (item-identity fast path), round-tripping byte-identically.
- `replace_equal_delta_unbalanced_block_round_trips` — the CML CIP36 shape: one item with an insert block declaring a local flag plus three replace blocks, the third a Δ+1 `if flag {` / `if self.vp != 0 {` pair (equal-delta rule), the condition appearing 3× in the item; a pristine regeneration round-trips byte-identically via the item-identity path.
- `replace_two_occurrences_of_duplicated_fragment` — two replace blocks in one item replace two different occurrences of the same duplicated fragment (identical recorded originals); both place positionally under a token-identical regeneration (impossible under both-sides uniqueness alone).
- `replace_rustfmt_folded_tail_arm_markers` — a replace block whose user section is a match's tail arm, in `rustfmt`'s canonical folded form (`} // cddl-codegen:replaces`, the recorded-original + `:replace-end` lines re-indented as an aligned block); the entry unfold moves the trailing markers own-line so the arm's `}` stays in the user section, and the block splices verbatim. Pins that the tool's own `rustfmt` pass produces a stable fixed point.
- `replace_positional_diversity_tails` — the same six tail positions, replacing grammar-valid
  statements, a field, enum variant, nested module, and impl with distinct `POSITIONAL_DIVERSITY REPLACE …`
  payloads.

## Positional-diversity formatter posture

The three positional-diversity triples are ordinary fixtures: `old.rs` is the pinned-rustfmt output
of `expected.rs`, `new.rs` is pristine, and the glob's merge and formatter-cycle assertions cover
them without a special harness path. The current pinned rustfmt actively folds the established
match-tail family. A raw final unit-enum variant without a comma can also fold its marker, but the
normal comma-bearing enum geometry needed for a preserve fixed point stays own-line; so all six new
triple geometries are currently own-line after their full chain. They are deliberate
version-bump/re-ownership tripwires, not a claim that every trailing position folds today.

## Replace-block fail-loudly cases (`compile_error!`, blessed `expected.rs`)

- `replace_drift_fails_loudly` — the recorded original no longer appears in the regenerated item (drift); the whole block is trapped, recorded original included.
- `replace_ambiguous_in_new_fails_loudly` — the needle appears more than once in the matched new item.
- `replace_deleted_duplicate_fails_loudly` — the needle is non-unique in the virtual old item (a deleted duplicate); which occurrence it replaces is ambiguous, so it fails loudly rather than re-attaching to a survivor.
- `replace_vanished_item_fails_loudly` — the enclosing item was deleted.
- `replace_same_key_count_change_fails_loudly` — the same-keyed item count changed, so occurrence matching is unsound.
- `replace_positional_blocked_by_in_item_drift_fails_loudly` — a duplicated-fragment block whose item ALSO changed (unrelated in-item drift), so the item-identity fast path does not apply and the non-unique needle is trapped in a `compile_error!`.
- `replace_rustfmt_folded_tail_drift_fails_loudly` — the `rustfmt`-folded tail-arm shape, but the generator's tail arm changed so the recorded original no longer matches; the unfolded block is trapped in a `compile_error!` (folding must not weaken drift detection).

## Hard-error cases (`error.txt`)

- `insert_quoted_tag_line_errors` — a reserved tag inside a block (namespace reservation).
- `keep_bare_with_no_run_errors` — a bare `keep` marker with no comment on the line below it.
- `keep_unknown_suffix_tag_errors` — `// cddl-codegen:keep-this` is an unknown tag, not `keep` with a suffix.
- `keep_nested_in_insert_block_errors` — a `keep` marker inside an insert block hits the existing "unexpected reserved tag" branch.
- `insert_unbalanced_delimiters_errors` — the user section has unbalanced `{}`/`()`/`[]`.
- `insert_orphaned_end_errors` — `insert-end` with no matching `insert-start`.
- `insert_unclosed_errors` — `insert-start` with no matching `insert-end`.
- `namespace_bare_unpreserved_marker_errors` — a bare `unpreserved-comment` marker not backed by a `compile_error!`.
- `replace_missing_replaces_errors` — a replace block with no `replaces` marker (nothing separates user code from the recorded original).
- `replace_missing_end_errors` — a replace block with no `replace-end`.
- `replace_empty_recorded_original_errors` — a `replaces` section that lexes to zero code tokens.
- `replace_unbalanced_user_section_errors` — the user section closes a delimiter it does not open (an interior dip, e.g. `} else {`).
- `replace_unbalanced_recorded_original_errors` — the recorded original closes a delimiter it does not open.
- `replace_unequal_delta_errors` — the user section and recorded original change delimiter depth by unequal net amounts (Δ0 vs Δ+1).
- `replace_unlexable_recorded_original_errors` — the recorded original fails to lex (an unterminated string literal after uncommenting).
- `replace_orphaned_replaces_errors` — `replaces` outside any replace block.
- `replace_orphaned_end_errors` — `replace-end` with no matching `replace-start`.
- `replace_nested_blocks_errors` — a `replace-start` nested inside another replace block's user section.
- `replace_straddles_item_boundary_errors` — a recorded original spanning more than one top-level item.
- `trailing_unknown_cddl_tag_errors` — a trailing `// cddl-codegen:<unknown-tag>` comment; the entry unfold moves it own-line, so the reserved namespace catches it as a hard error rather than the softer "move it to its own line" trailing-comment trap (never-silent applied uniformly to the folded position).
