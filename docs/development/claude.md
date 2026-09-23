# Claude operation

Read this when working in Claude Code; its model names and tool-lifetime statements do not apply to other harnesses.
Also read [Delegation](delegation.md) before assigning work.

## Model preferences

Opus is the session orchestrator.
Delegate implementation that has a clear plan to Opus agents.
Never use Haiku.
Do not manually choose Sonnet 5; selection by Claude Code itself or a tool is allowed.
Use Fable for a review when uncertainty remains after Opus review and another perspective warrants the higher cost.
Never fan out multiple Fable agents without explicit user permission.
Always pass an explicit `model:` in normal `agent()` calls to avoid unintended inheritance.

## Task lifetime and recovery

The main session's harness-tracked asynchronous Agent work delivers a completion notification and can reinvoke the main session after its turn ends; it may report interim status while that work runs.
The main session's tracked background gates can likewise survive its turn.
This exception does not cover an untracked watcher or a sub-agent ending its own turn to await anything.
Sub-agent background gates can die with the turn; `full` exceeds the supported sub-agent lifetime and must run in the main session.

Otherwise, poll with bounded foreground waits and end only with completed, reviewed results.
Follow task `.output` symlinks (`stat -L`) when inspecting transcripts.
An idle transcript does not establish a stalled or completed agent: a foreground tool call can produce no transcript updates until it returns.
Before recovery, inspect the final entry (`tool_use` means mid-call) and live build processes.
Read the invocation's own log, then use `SendMessage` to resume the same agent with findings; preserve its context for corrections and budget continuations.
