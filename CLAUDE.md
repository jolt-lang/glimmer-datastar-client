# Project Instructions for AI Agents

This file provides instructions and context for AI coding agents working on this project.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:6cd5cc61 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->


## Build & Test

```bash
jolt test                      # headless; no toolkit or display needed
# against a local glimmer checkout:
jolt -Sdeps '{:deps {jolt-lang/glimmer {:local/root "../../jolt-lang/glimmer"}}}' -M:test
```

## Architecture Overview

A Datastar-style native thin client on glimmer (see README.md). Namespaces
under `jolt.datastar.client`:

- `signals` — the signal store: one glimmer ratom per leaf path, RFC 7386
  merge-patch, snapshots filtered the way Datastar sends signals.
- `expr` — the EDN expression language: compiled once per form to a fn of an
  env; actions only where the env says `:action?`.
- `attrs` — Datastar's data-* attributes: `render-props` per render (reactive),
  `mount!` per element lifetime (returns a disposer).
- `codec` — the wire format (EDN default); the only place that parses.
- `protocol` — SSE events -> patch maps.
- `store` — the element store: one cell + glimmer component per id, patch
  modes, lifetimes synced at the end of each patch, parents first.
- `jolt.datastar.client` — wiring: requests, retries, per-element
  cancellation, applying patches on the UI thread. Default transport is
  jolt.http.sse (http-client).

Tests: `e2e-test` runs a real ring-chez-adapter SSE server; the rest are
headless with an in-memory glimmer backend (`test-backend`).

## Conventions & Patterns

- Toolkit independent: depend only on `glimmer.core`, `glimmer.ratom` and
  `glimmer.backend`; never require a backend outside tests or examples.
- Services the client needs from its host (HTTP, timers, clock) come in through
  the env (`:fetch`, `:timer`, `:now`) so everything stays testable headless.
