# Documentation

Read this guide before editing documentation, roadmaps, or agent instructions, and before finishing a delivery.

## Current choices and rationale

State the current choice, then explain the failure it prevents: “Use a per-crate target directory to avoid reusing another scratch crate's artifact.”
Keep historical context only when it changes how a reader must implement, use, or maintain the current design.
Do not create incident archives to preserve explanations removed from active guidance.
Use semantic line feeds: one sentence per source line, without column wrapping.

README files describe current behavior; retain history there only when needed for compatibility.
Roadmaps describe future work; prune completed items unless a remaining item's context requires them.
New features start as failing tests that become green with implementation; the repository's roadmaps track testing work rather than duplicating that feature plan.

## Roadmaps and references

The authoritative roadmaps are [the matrix roadmap](../../cddl-matrix/roadmap.toml) and [the testing roadmap](../../tests/testing-roadmap.toml).
Read and edit their TOML sources.
Render human-review Markdown on demand from `cddl-matrix/` with `bun run project_roadmaps.ts --roadmap testing --write`; use `--roadmap matrix` for the matrix roadmap.
The renders under `draft/roadmaps/` are disposable, may be stale, and must never be committed.
See [Editing the testing roadmap](../../tests/README.md#editing-the-testing-roadmap) for authoring commands.

A deferred or declined item must name a reopening signal that:

- Is observable by someone who already has the problem.
- Measures the dimension on which the deferred cost grows; use magnitude within one consumer when consumer count would miss that growth.
- Has not already been satisfied by the entry's own evidence; otherwise implement the work or choose a meaningful unmet signal.

Cite stable rule, test, gate, or document identifiers, or exact item titles; never cite a roadmap item by number or position.
Before shipping, replace temporary code-to-roadmap planning references with the delivered system's documentation or test identifiers so future pruning cannot silently retarget the reference.
The `lint_doc_citations` gate checks positional citations and its registered hand-document references; passing it does not establish the truth of prose claims.

## Delivery sweep

At the end of every delivery, check each of these surfaces against the change:

- `docs/docs/*.mdx`
- `cddl-matrix/README.md` and `cddl-matrix/roadmap.toml`
- `tests/README.md` and `tests/testing-roadmap.toml`

For each document, either confirm accuracy with the reason or fix it; do not sample the list.
Put required corrections in the sweep's own commit.
Check sibling surfaces when behavior is described as shared, prose that mirrors registries or counts, and limitations or refusals removed by the change.
When changing developer guidance, also update the affected guides and incoming references.

## Maintaining agent guidance

Keep `AGENTS.md` as the short entry point and each supporting guide scoped to an explicit action.
Before adding a rule, identify the decision it changes, its trigger, its exceptions, and its authoritative home.
Prefer improving the existing rule or regression coverage over adding a new incident-shaped paragraph.
Do not make every task read the entire supporting documentation tree.

When restructuring, use a temporary requirement-to-destination inventory under `draft/` to check that each obligation and exception survives.
Review meaning as well as links: a shorter rule must not weaken a requirement or turn a bounded exception into a prohibition.
Update incoming source and structured roadmap citations when an authority moves.
Check the root's 1,200-word budget and local links, then walk representative tasks through the read triggers to detect missing guidance.
The inventory is a migration aid, not an additional permanent source of rules.
