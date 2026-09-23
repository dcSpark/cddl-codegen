# Delegation

Read this before assigning or reviewing another agent's work.
Provider-specific model preferences and Claude harness behavior are in [Claude operation](claude.md).

Keep orchestration, implementation-plan creation, plan/implementation review, very hard problems, and work cheaper to do inline in the main session.
Generally avoid tests in parallel agents unless explicitly intended; coordinate heavy tiers through the main session.

Write relevant operational rules into each delegation prompt rather than relying on the agent having read a guide.
For gate-running work, include foreground execution, extended timeout, full output to a file, and the requirement to return remaining verification to the main session; reassert these in mid-task corrections.
The main session runs `full`.
Require the budget-exhaustion protocol: commit only the green subset, report the precise remainder, stop cleanly, and never stall.
This protocol does not grant additional commit permissions.

For every delegation with a plan, require an item-by-item report and review every plan item against it rather than spot-checking apparent completeness.
For phased work, keep that specification under `draft/` with each code-behavior premise marked as a claim to probe.
Apply [probe discipline](probes.md) to both the implementer's premises and the reviewer's conclusions.

A delegation writing into a registry-governed tree must name its enforcing gate and tier, including required registration edits.
Apply [fixture registration and coverage](probes.md#fixture-registration-and-coverage) even when the worker's lane cannot run the enforcing tier.
When heavy tiers are serialized, constrain worker lanes to `fast` and explicitly assign their remaining checks to the main session.

Do not end a sub-agent turn merely to await work whose completion requires that agent to remain active.
Use bounded waits and the current harness's task controls; report completed, reviewed results.
For a stalled phase, read its own log and resume the same agent with the findings to preserve context, rather than immediately replacing it.
