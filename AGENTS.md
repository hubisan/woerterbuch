# AI Agent Instructions

## Purpose

This repository uses AI agents to assist with development.

Important files:

- `TODO.org`: tasks and statuses
- `AI-NOTES.org`: blockers, open questions, decisions, test results
- `CHANGELOG.org`: approved and completed changes

Before making changes, always read `AGENTS.md` and `TODO.org`.

## Status

| Status | Responsible | Meaning                                                             |
|--------+-------------+---------------------------------------------------------------------|
| TODO   | User        | Task exists but is not ready for implementation. Do not work on it. |
| NEXT   | User        | Start signal for AI. Only work on tasks with this status.           |
| WAIT   | User/AI     | Blocked. Document the reason in `AI-NOTES.org`.                     |
| REVIEW | AI          | Implementation finished, waiting for user review.                   |
| DONE   | User/AI     | Approved by the user, completed, and added to the changelog.        |
| CANCEL | User        | Abandoned. Do not work on it.                                       |

## Workflow

1. The user sets a task in `TODO.org` to `NEXT`.
2. AI only works on tasks with status `NEXT`.
3. If currently on the main branch, AI creates a new branch:
   - `feature/task-description`
   - `fix/task-name`
   If not on the main branch you can assume that we are already working on a
   feature or fix. But please notify me, if you thinkg something is wrong.
4. AI sets completed tasks to `REVIEW`. Make a commit with a good message.
5. AI documents relevant information in `AI-NOTES.org`:
   - changes made
   - tests/checks performed
   - blockers
   - open questions
   - follow-up work
6. The user reviews tasks in `REVIEW`.
7. If changes are required, the user comments on the task and sets it back to `NEXT`.
8. After user approval, AI sets the task to `DONE`.
9. AI updates `CHANGELOG.org`.
10. AI creates a squash commit with a clear description.
11. The user merges into the main branch.

## WAIT Rule

If a task cannot continue:

- set the status to `WAIT`
- document the reason in `AI-NOTES.org`
- do not continue until the user sets the task back to `NEXT`

## Implementation Rules

AI should:

- make small, focused changes
- respect the existing style and architecture
- run tests, linters, type checks, or build checks when possible
- document failed checks in `AI-NOTES.org`
- avoid unnecessary refactoring
- avoid changing unrelated files

## Do Not Change Without Explicit Instruction

- secrets
- tokens
- `.env` files
- production configurations
- deployment credentials
- large refactorings
- dependency updates
- formatting entire files

## When Unsure

- do not invent assumptions
- document uncertainty in `AI-NOTES.org`
- ask a question if needed
