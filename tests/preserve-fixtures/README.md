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

## Hard-error cases (`error.txt`)

- `insert_quoted_tag_line_errors` — a reserved tag inside a block (namespace reservation).
- `insert_unbalanced_delimiters_errors` — the user section has unbalanced `{}`/`()`/`[]`.
- `insert_orphaned_end_errors` — `insert-end` with no matching `insert-start`.
- `insert_unclosed_errors` — `insert-start` with no matching `insert-end`.
- `namespace_bare_unpreserved_marker_errors` — a bare `unpreserved-comment` marker not backed by a `compile_error!`.
- `namespace_replace_reserved_errors` — reserved-but-unsupported `replace-*` tags.
