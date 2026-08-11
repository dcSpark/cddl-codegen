# Matrix all-fields fixture
CLOSEOUT c — blocked.
RAW FRAGMENT PLACEMENT
RAW LEGACY MARKER PLACEMENT
RAW PART PLACEMENT
CLOSEOUT b — due.
CLOSEOUT a — waiting.
WORK j — pending_review; build_capability; regression_gap; cosmetic.
WORK i — delegated; add_regression; defect; resource_exhaustion.
WORK h — waiting_external; repair; missing_system; misleading_docs.
WORK g — deferred; change_documentation; documentation_integrity; false_pass_or_red.
WORK f — armed; optimize; optimization; abort_or_panic.
WORK e — blocked; establish_honest_refusal; feature; compile_failure.
WORK d — ready; build_system; infrastructure; wrong_public_api; low.
WORK c — ready; build_capability; coverage_cell; valid_rejection; normal.
WORK b — ready; add_regression; regression_gap; invalid_acceptance; high.
WORK a — ready; repair; defect; silent_wrong_bytes; critical.
FAMILY d — under_design; designing; reviewed_relation.
FAMILY c — under_design; closing; registry.
FAMILY b — under_design; enumerating; grammar.
FAMILY a — observed_only; designing.
SIGNAL k — cadence; unknown.
SIGNAL j — retirement_predicate; unmet.
SIGNAL i — watch_escalation; met.
SIGNAL h — unblock_predicate; stale.
SIGNAL g — promotion_trigger; manual; unknown.
SIGNAL f — reopening_signal; event; unmet.
SIGNAL e — promotion_trigger; quantitative; gt; met.
SIGNAL d — promotion_trigger; quantitative; ge; stale.
SIGNAL c — promotion_trigger; quantitative; eq; unmet.
SIGNAL b — promotion_trigger; quantitative; le; unmet.
SIGNAL a — promotion_trigger; quantitative; lt; met.
## Semantic authority boundary
Permanent matrix fixture boundary.
RAW OWNER — semantic shadow stays non-rendering.
POLICY c — boundary; reopenable.
POLICY b — boundary; permanent.
POLICY a — maintenance_protocol.
EVIDENCE m — decision; live; falsified.
EVIDENCE l — external_commit; stale; confirmed.
EVIDENCE k — external_issue; historical; proposed.
EVIDENCE j — incident; as_of; inapplicable.
EVIDENCE i — consumer_report; as_of; unknown.
EVIDENCE h — spec_read; stale; falsified.
EVIDENCE g — source_read; historical; confirmed.
EVIDENCE f — registry_enumeration; as_of; proposed.
EVIDENCE e — execution_probe; as_of; inapplicable.
EVIDENCE d — committed_vector; stale; unknown.
EVIDENCE c — harness_free_repro; historical; falsified.
EVIDENCE b — gate; as_of; confirmed.
EVIDENCE a — regression_pin; live; proposed.
CONTROL g — operator_procedure; live.
CONTROL f — upstream_issue; stale.
CONTROL e — consumer_ci; proposed.
CONTROL d — review_rule; live.
CONTROL c — fixture; stale.
CONTROL b — test; proposed.
CONTROL a — gate; live.
DECISION d — decided; reopenable.
DECISION c — decided.
DECISION b — held.
DECISION a — pending.
123 features (95 RFC8610 + 1 RFC9682 + 27 `CDDL_CODEGEN` vendor profile), 136 containment cells, and 293 cddl-codegen annotations
all 37 IANA ops probed
6 divergences, all `preserve`-side
96 `class="constraint"` enforcement reject vectors over 20 enforce-green rows
