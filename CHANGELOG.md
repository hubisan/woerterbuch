# Changelog

This file records completed changes.

## Unreleased

- `CHANGED`: Renamed moved to old repo, woerterbuch instead of rust-woerterbuch
- `CHANGED`: Markdown now renders examples as nested `Examples` lists, while Org renders examples as foldable `:EXAMPLES:` drawers instead of separate `Examples` headings.
- `CHANGED`: Markdown and Org now render example blocks as nested `Examples` headings instead of plain inline labels.
- `FIXED`: Source URL handling is now more robust across Duden, Wiktionary, DWDS, and OpenThesaurus, including Swiss `ss` fallbacks where verification is needed.
- `CHANGED`: Human, Markdown, and Org output now include lookup headers; Org also sets `#+STARTUP: showall`, and JSON now includes a top-level timestamp.
- `CHANGED`: Human output now renders sense labels and references with single quotes instead of backticks.
- `CHANGED`: Text output now omits redundant `Entry 1` headings for sources that only have a single entry.
- `FIXED`: Duden lookups now resolve words with umlauts and `ß` correctly by using the `ae/oe/ue/ss` URL spellings Duden expects.
- `CHANGED`: Org output now uses `~label~`, JSON rejects `--layout`, and `--max-examples` limits rendered examples in text output.
- `CHANGED`: Renamed output layouts to `by-source` and `by-section`, made `by-source` the default, and aligned text output spacing with the documented examples.
- `CHANGED` Updated the README.
- `CHANGED` Improved Duden lookup latency by returning direct Duden entries immediately instead of waiting for the search page; search is now used only as fallback for missing or unparseable direct entries, this is usually the case, when there are multiple homographs like for the word Bank.
- `FIX` Cargo.toml had unexepected string `-`, oups.
- `ADDED` Github CI for automated testing.
- `CHANGED` Renamed binary to `woerterbuch`.
- `CHANGED`: Clarified human-readable source status messages so skipped sections, missing entries, and real errors are shown differently.
- `FIXED`: Fixed `--sections` filtering so requested nested examples, idioms, and synonyms are preserved and unsupported source/section combinations are skipped cleanly.
- `ADDED`: Added structured parsers and offline snapshot tests for Duden, Wiktionary, OpenThesaurus, and DWDS.
- `CHANGED`: Fetch Duden entry and search pages in parallel so homographs can be detected earlier while treating search failures as non-fatal when the direct entry lookup succeeds.

## Instructions

### Changelog Entry Types

- `ADDED`: Use for new features, new functionality, or newly introduced behavior.
- `CHANGED`: Use for modifications to existing behavior.
- `FIXED`: Use for bug fixes or incorrect behavior that now works correctly.
- `REMOVED`: Use for removed functionality, deleted features, or deprecated behavior that no longer exists.

### Rules

- Use one bullet per change.
- Keep entries short and clear.
- Write for users, not developers.
- Do not include unfinished work.
- Do not include internal refactorings unless they affect users.
- Add newest entries at the top of the `Unreleased` section.
- Use uppercase entry types:
  - `ADDED`
  - `CHANGED`
  - `FIXED`
  - `REMOVED`
- Keep wording simple and direct.
- Prefer describing the visible result instead of implementation details.

### Releases

When creating a release:

- move entries from `Unreleased` into a version section
- keep the same bullet format
- use the following heading format:

```markdown
## 0.1.0 - 2026-05-22

- `ADDED`: Added CSV export for Anki notes via AnkiConnect.
- `CHANGED`: Clarified AI workflow and commit rules.
- `FIXED`: Fixed incorrect WAIT status handling.
- `REMOVED`: Removed outdated setup instructions.
```
