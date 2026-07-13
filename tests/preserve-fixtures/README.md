# preserve-fixtures

File-fixture cases for `comment_preserve::preserve`, driven by
`src/tests/preserve_fixture_tests.rs`. Each directory holds `old.rs` + `new.rs` and exactly one
expectation: `expected.rs` (exact merge output, blessed with `BLESS_PRESERVE_FIXTURES=1`) or
`error.txt` (a substring the hard `PreserveError` message must contain). Intent lives here rather
than inside `old.rs` because any comment added to `old.rs` would itself be a merge input. See the
harness module docs for the byte-for-byte assertion and the three cross-cutting properties
(idempotent fixed point, never-silent, `changed==false ⇒ output==new`).

`.gitattributes` pins `* -text` so the CRLF cases keep their `\r\n` bytes across checkouts.

## Comment cases (migrated from `comment_preserve.rs`'s inline suite; name = old test name)

- `identity_transfers_own_line_comment` — file tokens unchanged, comment transfers at same index.
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
- `stale_tool_doc_dropped_user_doc_on_undocumented_item_kept` — stale tool `///` drops; a user `///` on an undocumented item stays.
- `same_key_count_change_fails_loudly` — same-keyed item count changed → occurrence match unsound.
- `preceding_comment_on_reordered_same_key_items_fails_loudly` — two changed same-key items → ordering untrustworthy.
- `comma_terminated_statement_reanchors` — a struct-literal field comment re-anchors at its `,`.
- `doc_on_vanished_item_drops_silently` — tool docs on a deleted item drop, not trapped in `compile_error!`.
- `crlf_old_file_self_cancels` — a CRLF prior output is a no-op against its LF twin.
- `crlf_user_comment_still_preserved` — a CRLF user comment is preserved without a stray `\r`.
- `eof_comment_preserved_at_file_end` — a comment past the last token stays at EOF.
- `comment_before_code_on_same_line_fails_loudly` — `/* note */ code` is out of own-line scope.

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

## Replace-block cases

A replace block records the generated code it overrides (`//`-commented under `replaces`); that
recorded original is both the placement anchor (lex it, find the token run in the regenerated item,
splice the user block over it) and the drift detector (needle gone ⇒ the generator changed ⇒ fail
loudly). The needle must be unique on BOTH sides. Idempotent re-splice is not a separate fixture —
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

## Replace-block fail-loudly cases (`compile_error!`, blessed `expected.rs`)

- `replace_drift_fails_loudly` — the recorded original no longer appears in the regenerated item (drift); the whole block is trapped, recorded original included.
- `replace_ambiguous_in_new_fails_loudly` — the needle appears more than once in the matched new item.
- `replace_deleted_duplicate_fails_loudly` — the needle is non-unique in the virtual old item (a deleted duplicate); which occurrence it replaces is ambiguous, so it fails loudly rather than re-attaching to a survivor.
- `replace_vanished_item_fails_loudly` — the enclosing item was deleted.
- `replace_same_key_count_change_fails_loudly` — the same-keyed item count changed, so occurrence matching is unsound.
- `replace_comment_inside_replaced_span_conflict_fails_loudly` — a plain user comment whose anchor lands strictly inside a replaced byte span (op-composition conflict); the replace still splices, the comment fails loudly.

## Hard-error cases (`error.txt`)

- `insert_quoted_tag_line_errors` — a reserved tag inside a block (namespace reservation).
- `insert_unbalanced_delimiters_errors` — the user section has unbalanced `{}`/`()`/`[]`.
- `insert_orphaned_end_errors` — `insert-end` with no matching `insert-start`.
- `insert_unclosed_errors` — `insert-start` with no matching `insert-end`.
- `namespace_bare_unpreserved_marker_errors` — a bare `unpreserved-comment` marker not backed by a `compile_error!`.
- `replace_missing_replaces_errors` — a replace block with no `replaces` marker (nothing separates user code from the recorded original).
- `replace_missing_end_errors` — a replace block with no `replace-end`.
- `replace_empty_recorded_original_errors` — a `replaces` section that lexes to zero code tokens.
- `replace_unbalanced_user_section_errors` — the user section has unbalanced delimiters.
- `replace_unbalanced_recorded_original_errors` — the recorded original has unbalanced delimiters.
- `replace_unlexable_recorded_original_errors` — the recorded original fails to lex (an unterminated string literal after uncommenting).
- `replace_orphaned_replaces_errors` — `replaces` outside any replace block.
- `replace_orphaned_end_errors` — `replace-end` with no matching `replace-start`.
- `replace_nested_blocks_errors` — a `replace-start` nested inside another replace block's user section.
- `replace_straddles_item_boundary_errors` — a recorded original spanning more than one top-level item.
