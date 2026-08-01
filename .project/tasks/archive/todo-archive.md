# TODOs Archive

Those are alle finished tasks.

## DONE Fix: Examples as headings breaks structure

Ich arbeite an einem Rust-Projekt mit einem Renderer für `Human`, `Markdown`, `Org` und `Json`.

Bitte passe den Renderer so an, dass Beispiele (`examples`) nicht mehr als Heading gerendert werden.

Gewünschtes Verhalten:

Für Markdown sollen Examples als verschachtelte Liste unter der jeweiligen Definition erscheinen:

```markdown
- `1.` erste Bedeutung

  - Examples
    - eins
    - zwei
```

Für Org sollen Examples als foldbarer Org Drawer erscheinen, damit sie keine neue Heading erzeugen und die Dokumentstruktur korrekt bleibt:

```org
- ~1.~ erste Bedeutung

  :EXAMPLES:
  - eins
  - zwei
  :END:
```

Wichtig:

* `### Examples` bzw. `*** Examples` darf nicht mehr erzeugt werden.
* `content_heading_level` soll entfernt werden, falls es danach nur noch rekursiv weitergereicht wird.
* `push_examples_heading` soll entfernt oder passend umbenannt werden.
* Clippy darf nicht wegen `only_used_in_recursion` fehlschlagen.
* Tests sollen angepasst werden:

  * Markdown: Examples als nested list prüfen.
  * Org: Examples als `:EXAMPLES:` Drawer prüfen.
  * Sicherstellen, dass alte Headings wie `### Examples` oder `*** Examples` nicht mehr vorkommen.
  * `max_examples` soll weiterhin funktionieren.

Bitte ändere den Code minimal-invasiv und achte darauf, dass `cargo fmt --all --check`, `cargo clippy --all-targets --all-features -- -D warnings` und die Tests durchlaufen.

## DONE 2026-06-14 [Format: Examples as Heading Instead of Plain](./2026-06-14--format-examples-as-heading.md)

## DONE 2026-06-14 [Fix source URLs, Swiss `ss`, sharp S candidates, and provider quirks](./2026-06-14--fix-source-urls-swiss-ss-provider-quirks.md)

## DONE 2026-06-13 [Add timestamps/headers to dictionary output formats](./2026-06-13--add-timestamps-headers-output-formats.md)

## DONE 2026-06-13 [Bei Human Format 'nr' statt `nr` verwenden](./2026-06-13--human-format-single-quotes.md)

## DONE 2026-06-13 [Omit redundant single-entry headings](./2026-06-13--omit-redundant-single-entry-headings.md)

Improve text rendering by omitting `Entry 1` headings when a source has only one entry.

Decision:
- In human, markdown, and org output, render `Entry N` headings only when the current source has more than one entry.
- If a source has exactly one entry, render its sections directly under the source heading.
- This applies to both layouts:
  - `--layout by-source`
  - `--layout by-section`
- JSON output remains unchanged.
- The internal data model remains unchanged.

Examples:
- For `Buch`, sources like DWDS, Duden, and Wiktionary each have only one entry, so do not render `Entry 1`.
- For `Bank`, DWDS, Duden, and Wiktionary have multiple entries, so keep `Entry 1`, `Entry 2`, etc.

Acceptance criteria:
- `woerterbuch Buch --format markdown --layout by-source` does not contain redundant `### Entry 1` headings for single-entry sources.
- `woerterbuch Buch --format markdown --layout by-section` does not contain redundant `### Entry 1` headings for single-entry sources.
- `woerterbuch Bank --format markdown --layout by-source` still renders `Entry 1` and `Entry 2` where sources have multiple entries.
- `woerterbuch Bank --format markdown --layout by-section` still renders `Entry 1` and `Entry 2` where sources have multiple entries.
- `woerterbuch Buch --json` remains unchanged.
- `cargo fmt --all --check` passes.
- `cargo test` passes.
- `cargo clippy --all-targets --all-features -- -D warnings` passes.

## DONE 2026-06-13 [Fix Duden Umlaute](./2026-06-12--fix-duden-umlaute.md)

Dies gibt aktuell kein Ergebnis:
Gerüst: https://www.duden.de/rechtschreibung/Geruest
verrückt: https://www.duden.de/rechtschreibung/verrueckt

Ich gehe davon aus, dass dies aufgrund des Umlautes nicht geht.

DWDS hat Umlaute: https://www.dwds.de/wb/Ger%C3%BCst
Wiktionary auch: https://de.wiktionary.org/wiki/Ger%C3%BCst
Openthesaurus auch: https://www.openthesaurus.de/synonyme/Ger%C3%BCcht

## DONE 2026-06-12 Additional output formats

  - `--format human`
  - `--format json`
  - `--format markdown`
  - `--format org`

- Make two variants of the ouput: 
  - by-source
    Example in markdown: [example-output-markdown--by-source.md](example-output-markdown--by-source.md)
  - by-section
    Example for markdown [example-output-markdown--by-section.md](example-output-markdown--by-section.md)

## DONE 2026-06-12 [Fix Compiler Warnings](./2026-06-12--output-layout-naming-and-spacing.md)

## DONE 2026-06-12 [Rename sources-sections and sections-sources](./2026-06-12--output-layout-naming-and-spacing.md)

sources-sections to by-source
sections-sources to by-section

## DONE 2026-06-12 [Change Default](./2026-06-12--output-layout-naming-and-spacing.md)

Default is by-source

## DONE 2026-06-12 [Fix newlines](./2026-06-12--output-layout-naming-and-spacing.md)

Make the output use newlines as shown in the examples:

- by-source
  Example in markdown: [example-output-markdown--by-source.md](example-output-markdown--by-source.md)
- by-section
  Example for markdown [example-output-markdown--by-section.md](example-output-markdown--by-section.md)

## DONE 2026-06-12 [Org-mode use ~nr~ instead of `nr`](./2026-06-12--json-layout-org-labels-and-max-examples.md)

## DONE 2026-06-12 [JSON should not accept layout](./2026-06-12--json-layout-org-labels-and-max-examples.md)

Clarify layout behavior for JSON output.

### Decision

* JSON output must remain source-native and stable.
* `--format json` and `--json` always return the existing JSON shape.
* `--layout` only affects text-like output formats:

  * `human`
  * `markdown`
  * `org`
* Reject these combinations with a clear CLI error:

  * `--format json --layout by-source`
  * `--format json --layout by-section`
  * `--json --layout by-source`
  * `--json --layout by-section`
* Keep `--json` as a shortcut for `--format json`.
* Remove the grouped/presentation JSON variant if it exists.
* Keep the existing source-native JSON serializer as the single JSON output path.

### Implementation notes

* Make `layout` optional in the CLI instead of defaulting it immediately.
* In `main`, resolve the effective layout only for text-like formats.
* Default text layout should be `by-source`.
* If JSON format is selected and the user explicitly provided `--layout`, return an error.
* Remove or stop using any `format_json_by_sections` / `format_json_by_section` function if present.
* Do not add a second JSON schema for section-grouped output.

### Acceptance criteria

* `woerterbuch Bank --json` returns the existing JSON shape.
* `woerterbuch Bank --format json` returns the existing JSON shape.
* `woerterbuch Bank --format json --layout by-source` fails with a clear error.
* `woerterbuch Bank --format json --layout by-section` fails with a clear error.
* `woerterbuch Bank --json --layout by-source` fails with a clear error.
* `woerterbuch Bank --json --layout by-section` fails with a clear error.
* `woerterbuch Bank --format markdown` defaults to `--layout by-source`.
* `woerterbuch Bank --format markdown --layout by-section` works.
* `cargo fmt --all --check` passes.
* `cargo test` passes.
* `cargo clippy --all-targets --all-features -- -D warnings` passes.


## DONE 2026-06-12 [Limit rendered examples per definition](./2026-06-12--json-layout-org-labels-and-max-examples.md)

Add a CLI option to limit the number of rendered examples per definition.

### Decision

* Add `--max-examples <N>`.
* The option limits examples only in rendered text outputs:

  * `human`
  * `markdown`
  * `org`
* The limit applies per definition/sense/subsense, not globally.
* If `--max-examples` is omitted, render all examples as today.
* If `--max-examples 0` is used, render no examples.
* The limit must apply consistently in both layouts:

  * `--layout by-source`
  * `--layout by-section`
* JSON output must remain unchanged and always contain all examples.

### Implementation notes

* Add `max_examples: Option<usize>` to the CLI args.
* Pass `max_examples` into the text renderer.
* Apply the limit only when rendering examples.
* Do not apply the limit in scraping/parsing.
* Do not modify the data model for this.
* For JSON output, ignore `--max-examples` and return the full existing JSON unchanged.
* Prefer accepting `woerterbuch Bank --json --max-examples 1` and leaving JSON unchanged instead of failing.

### Acceptance criteria

* `woerterbuch Bank --format human --max-examples 2` shows at most 2 examples per definition.
* `woerterbuch Bank --format markdown --max-examples 2` shows at most 2 examples per definition.
* `woerterbuch Bank --format org --max-examples 2` shows at most 2 examples per definition.
* `woerterbuch Bank --format markdown --layout by-source --max-examples 1` shows at most 1 example per definition.
* `woerterbuch Bank --format markdown --layout by-section --max-examples 1` shows at most 1 example per definition.
* `woerterbuch Bank --format org --max-examples 0` renders definitions without example blocks.
* `woerterbuch Bank --json --max-examples 1` still returns the full JSON with all examples unchanged.
* `woerterbuch Bank --format json --max-examples 1` still returns the full JSON with all examples unchanged.
* `cargo fmt --all --check` passes.
* `cargo test` passes.
* `cargo clippy --all-targets --all-features -- -D warnings` passes.

## DONE 2026-06-12 [Refresh source fixtures and expected JSON output](./2026-06-12--refresh-source-fixtures-and-json-expected-output.md)

  - Download and store repo-local source fixtures for the existing test words.
  - Generate canonical expected JSON outputs from the current Rust implementation.
  - Update parser tests to consume those local fixtures and expected JSON files.

## DONE [Fix remaining Clippy warnings](./2026-06-12--fix-remaining-clippy-warnings.md)

  - Replace DWDS manual `iter().any(...)` check with `contains(...)`.
  - Move `dedupe(...)` before the test module in `models.rs`.

## DONE [Konsistente Statusdarstellung im Human-Output](./2026-06-11--human-output-statuses.md)

Ja, das ist aktuell tatsächlich missverständlich.
Im Human-Output gibt es heute nur diese grobe Unterscheidung in src/format.rs (line 16):
No results. wenn ok = true, aber entries leer sind
Error: ... wenn ok = false
Dadurch fallen im Moment zwei semantisch verschiedene Fälle zusammen:
bewusst übersprungen, z. B. DWDS bei --sections synonyms
erfolgreiche Quelle, aber nach Filterung bleibt nichts übrig
Beides erscheint aktuell als No results..
Ein 404 erscheint nicht einheitlich für alle Quellen:
Wiktionary: Error: No matches found in src/sources/wiktionary.rs (line 26)
Duden: bei Entry-404 meist ebenfalls Error: No matches found, sonst bei anderem HTTP-Fehler Error: HTTP error: ... in src/sources/duden.rs (line 33)
DWDS: echtes HTTP-404 würde derzeit eher als Reqwest-Fehlertext hochkommen, weil fetch_html(...).error_for_status()? verwendet wird in src/http.rs (line 16); wenn die Seite aber 200 liefert und nur kein Artikel da ist, dann Error: No matches found
Inhaltlich wäre sauberer, drei Fälle getrennt auszugeben:
Skipped: source does not support requested sections.
No entry found on source.
Error: HTTP 404 ... oder allgemeiner Error: ...
Der aktuelle Text ist also eher ein UI-Problem als ein Datenmodell-Problem. Wenn du willst, mache ich als nächsten kleinen Follow-up genau diese Statusdarstellung im Human-Output konsistent.

## DONE [Clarify and fix section filtering semantics](./2026-06-11--section-filtering-semantics.md)

### Context

The Rust CLI currently accepts `--sections definitions,examples,synonyms,origin,idioms`.

At the moment, every selected source performs its normal lookup and parser work first. Afterwards, `SourceResult::retain_sections` removes fields that were not requested.

This means `--sections` is currently an output filter, not true section-level scraping.

The old Emacs Lisp implementation passed `sections` into source-specific fetch/parse functions, but the current Rust architecture deliberately centralized filtering in `src/models.rs`.

### Goal

Define and implement stable section semantics:

* Keep `--sections` as a public CLI/API option.
* Treat it primarily as an output projection/filter.
* Avoid over-optimizing parser internals where the source must load a complete HTML/API response anyway.
* Prevent requested sections from disappearing accidentally when another related section is disabled.
* Add cheap source-level request skipping where it is clearly correct.

### Decision

Do not remove `--sections`.

Do not rewrite all source parsers to scrape only individual sections from already-downloaded pages.

Reasoning:

* Duden, DWDS and Wiktionary each require fetching a whole page/API response for a lookup.
* The expensive part is the HTTP request, not extracting a few extra DOM nodes.
* Section-aware parser branching would add complexity and risk missing data.
* `--sections` is still valuable for smaller JSON output and Emacs/UI consumers.

However, implement cheap request-level optimization where a source only supports sections that were not requested.

Example:

* `OpenThesaurus` only provides synonyms.
* If `synonyms` is not requested, skip the OpenThesaurus request entirely.

### Required Changes

#### 1. Fix `retain_sections` so requested nested data is preserved

Current behavior clears all `senses` when `definitions` is not requested:

```rust
if !wanted.contains(&Section::Definitions) {
    self.senses.clear();
}
```

This is wrong for requests such as:

```bash
cargo run -- Bank --sections examples
cargo run -- Bank --sections idioms
cargo run -- Bank --sections synonyms
```

because examples, idioms and sense-level synonyms can live inside `Sense`.

Change the logic so that disabling `definitions` only removes the definition text, not the whole sense.

Expected behavior:

* If `definitions` is not requested:

  * set `sense.definition = None`
  * keep `sense.label`, `source_id`, and nested content needed to locate examples/idioms/synonyms
* If `examples` is requested:

  * keep senses that contain examples
* If `idioms` is requested:

  * keep entry-level idioms and sense-level idioms
* If `synonyms` is requested:

  * keep entry-level synonym groups and sense-level synonyms
* After pruning, remove empty senses/subsenses only if they contain no requested content and no useful child content.

#### 2. Add source capability checks

Add a helper in `src/sources.rs` or on `Source`:

```rust
fn source_supports_any_section(source: Source, sections: &[Section]) -> bool
```

Suggested mapping:

* `OpenThesaurus`: `Synonyms`
* `Dwds`: `Definitions`, `Examples`, `Origin`, `Idioms`
* `Duden`: `Definitions`, `Examples`, `Synonyms`, `Origin`, `Idioms`
* `Wiktionary`: `Definitions`, `Examples`, `Synonyms`, `Origin`, `Idioms`

Use it before starting source jobs or inside `lookup_source`.

If a source supports none of the requested sections, do not perform HTTP.

Preferred behavior:

* Return a successful empty `SourceResult`, or omit the source result.
* Choose one behavior and document it.
* For stable output shape, prefer returning an empty `ok: true` result with no entries.

Example:

```rust
if !source_supports_any_section(source, sections) {
    return SourceResult::ok(source, None, Vec::new());
}
```

#### 3. Keep source parsers simple

Do not pass `sections` into all parser functions unless there is a proven performance issue.

Keep parsers source-complete:

```rust
duden::lookup(client, query).await
dwds::lookup(client, query).await
wiktionary::lookup(client, query).await
openthesaurus::lookup(client, query).await
```

Then apply central filtering.

Exception:

* OpenThesaurus can be skipped before lookup when `synonyms` is not requested.

#### 4. Add tests for section combinations

Add tests for `retain_sections` using synthetic `DictionaryEntry` / `Sense` data.

Required cases:

* `definitions` only keeps definitions, removes examples, idioms, synonyms, origin.
* `examples` only keeps example-bearing senses, does not delete all senses.
* `idioms` only keeps entry-level and sense-level idioms.
* `synonyms` only keeps entry-level synonym groups and sense-level synonyms.
* `origin` only keeps etymology.
* `examples,synonyms` keeps both without definitions.
* empty sections returns no content but does not panic.

Add at least one integration/snapshot-style test for CLI JSON behavior if the existing test structure makes that practical.

### Acceptance Criteria

* `cargo test` passes.
* `cargo run -- Bank --json --sections examples` returns examples when the source contains examples.
* `cargo run -- Bank --json --sections synonyms` does not request OpenThesaurus unless synonyms are requested.
* `cargo run -- Bank --json --sections definitions` does not include examples, idioms, synonyms or origin.
* `cargo run -- Bank --json --sections examples` does not include definition text unless definitions were also requested.
* No requested section is lost merely because another section is disabled.
* Source parsers remain independently testable with local fixtures.
* No live requests are introduced into tests.

### Out of scope

* Rewriting Duden/DWDS/Wiktionary parsers to parse only individual sections.
* Changing the JSON model shape.
* Removing the `--sections` CLI option.
* Benchmarking parser-level micro-optimizations unless later evidence shows this is needed.

### Open Points

* Should skipped sources appear as empty successful results, or be omitted from `results`?
  * Recommendation: keep them as empty successful results for stable source order.
  -> OK
* Should `origin` include only `etymology`, or also grammar/history-like metadata from sources?

  * Recommendation: keep `origin` mapped only to `etymology`.
  -> OK

## DONE DWDS: HTML-Parser + Snapshot-Tests

Task file: [./2026-06-09--dwds-parser.md](./2026-06-09--dwds-parser.md)

Portiere die alte DWDS-Scraping-Logik aus Emacs Lisp nach Rust und ersetze den aktuellen groben DWDS-Stub durch einen robusten Parser mit deterministischen Offline-Snapshot-Tests.

DWDS ist die letzte noch fehlende Quelle. Der aktuelle Rust-Code in `src/sources/dwds.rs` ist noch deutlich zu grob: Er liest nur ein paar globale Selektoren, sammelt Beispiele pauschal auf die erste Bedeutung und bildet Homographen, rekursive Bedeutungen, DWDS-IDs, Qualifier, Mehrwortausdrücke, Etymologie und Idiome noch nicht korrekt ab.

Maßgeblich ist die fachliche Logik aus `woerterbuch-dwds.el`, idiomatisch in Rust umgesetzt und auf die vorhandenen Rust-Modelle gemappt.

#### Wichtigste geprüfte Grundlage

Unbedingt zuerst lesen:

- `../../emacs-lisp/lisp/woerterbuch-dwds.el`
- `../../emacs-lisp/tests/test-woerterbuch-dwds.el`
- `../../emacs-lisp/tests/test-helper.el`
- `../../emacs-lisp/tests/files/dwds/`
- `../../src/sources/dwds.rs`
- `../../src/models.rs`
- `../../src/sources/openthesaurus.rs`
- `../../src/sources/wiktionary.rs`
- `../../src/sources/duden.rs`

Aus `woerterbuch-dwds.el` sind insbesondere diese Funktionen relevant und sollen fachlich portiert werden:

- `woerterbuch-dwds--build-url`
- `woerterbuch-dwds--clean-text`
- `woerterbuch-dwds--text`
- `woerterbuch-dwds--text-skipping-classes`
- `woerterbuch-dwds--canonical-url`
- `woerterbuch-dwds--field-text`
- `woerterbuch-dwds--wortart-from-grammar`
- `woerterbuch-dwds--extract-qualifiers`
- `woerterbuch-dwds--reference-definition`
- `woerterbuch-dwds--extract-reference-text`
- `woerterbuch-dwds--mwa-marker-p`
- `woerterbuch-dwds--local-mwa-marker-p`
- `woerterbuch-dwds--find-local-mwa-scope`
- `woerterbuch-dwds--extract-mwa-text`
- `woerterbuch-dwds--extract-definition-text`
- `woerterbuch-dwds--extract-examples`
- `woerterbuch-dwds--parse-idioms`
- `woerterbuch-dwds--parse-etymology`
- `woerterbuch-dwds--article-scope-p`
- `woerterbuch-dwds--article-scopes`
- `woerterbuch-dwds--entry-page-p`
- `woerterbuch-dwds--make-definition-parser`
- `woerterbuch-dwds--parse-homograph`
- `woerterbuch-dwds--parse-dom`
- `woerterbuch-dwds--fetch-callback`

#### Ziel

Der DWDS-Parser soll DWDS-HTML in die vorhandenen Rust-Modelle überführen:

- `SourceResult`
- `DictionaryEntry`
- `Sense`
- `UrlValue`

DWDS liefert in der alten Implementierung keine Synonyme. `synonym_groups` soll für DWDS daher leer bleiben, außer im aktuellen DWDS-HTML gibt es später eine eindeutig zuverlässige Synonym-Struktur. Für diese Aufgabe sind Definitionen, Beispiele, Herkunft und Idiome maßgeblich.

#### Architektur

Halte den Scope möglichst auf `src/sources/dwds.rs` begrenzt.

Erwartete Struktur:

- `lookup(client, query) -> Result<SourceResult>` bleibt für HTTP, URL-Bildung und Fehlerbehandlung zuständig.
- `parse(query, page_url, html) -> SourceResult` oder eine äquivalente reine Parser-Funktion bleibt unabhängig vom HTTP-Code testbar.
- Tests dürfen keine Live-Requests ausführen.
- Snapshot-Tests verwenden lokale HTML-Fixtures.
- Keine Panics bei fehlenden oder unerwarteten HTML-Sektionen.
- Keine große Umstellung der CLI.
- Keine Änderung an gemeinsamen Modellen, außer es ist wirklich notwendig.
- Ausgabe und Tests müssen deterministisch sein.

Die zentrale Section-Filterung existiert bereits über `SourceResult::retain_sections` in `src/models.rs`. Daher in DWDS nicht wieder das alte Elisp-`sections`-Argument nachbauen. Der Rust-DWDS-Parser soll grundsätzlich alle gefundenen DWDS-Daten extrahieren; die gewünschte Auswahl übernimmt danach die bestehende zentrale Logik.

#### Live-Lookup und URL

Portiere die URL-Logik aus `woerterbuch-dwds--build-url`.

DWDS nutzt direkte Wörterbuchseiten:

```text
https://www.dwds.de/wb/<lemma>
````

Regeln:

* Suchbegriff URL-encoden.
* Keine zusätzliche DWDS-Suchseite verwenden.
* Homographen wie `Bank` stehen auf derselben DWDS-Seite und werden als mehrere Artikel-Scope-Blöcke geparst.
* Die kanonische URL aus `<link rel="canonical" href="...">` soll bevorzugt verwendet werden.
* Falls kein Canonical-Link existiert, verwende die gebaute URL als Fallback.

Beispiele:

```text
Bank -> https://www.dwds.de/wb/Bank
Haus -> https://www.dwds.de/wb/Haus
```

#### HTTP-Verhalten

DWDS kann bei nicht vorhandenen Wörtern eine normale HTML-Seite ohne `dwdswb-artikel` liefern. Das ist fachlich ein No-Match, nicht zwingend ein HTTP-Fehler.

Erwartung:

* HTTP 2xx mit echter Artikelstruktur -> `SourceResult::ok(...)`
* HTTP 2xx ohne `dwdswb-artikel` -> `SourceResult::error(Source::Dwds, "No matches found")`
* HTTP-Fehler -> saubere Fehlerbehandlung wie bei den anderen Quellen
* Netzwerkfehler -> saubere Fehlerbehandlung über `anyhow`/zentralen `lookup_source`

Die alte Emacs-Lisp-Implementierung nutzte eigene DWDS-Header. Der globale Rust-Client setzt bereits einen Browser-User-Agent. Prüfe trotzdem, ob DWDS live stabil genug ist. Falls nötig, ergänze zentrale HTTP-Header nur allgemein und nicht DWDS-spezifisch, sofern das zur bestehenden Architektur passt.

#### Aktueller Rust-Stub: bekannte Probleme

Der aktuelle `src/sources/dwds.rs` macht ungefähr:

* globale Selektoren wie `.dwdswb-lesart-def`
* globale Beispiel-Sammlung
* Beispiele pauschal auf die erste Bedeutung
* keine echten Homographen
* keine rekursive Lesarten-Struktur
* keine DWDS-IDs
* keine Qualifier
* keine MWA-Definitionen
* keine saubere Etymologie pro Homograph
* keine Idiom-Relation-Blöcke
* kein echter No-Match bei HTML ohne Artikel

Diese Implementierung soll ersetzt oder stark überarbeitet werden.

#### DWDS-Seitenstruktur

Wichtige DWDS-Strukturen aus den Fixtures:

* `div.dwdswb-artikel`
* `h1.dwdswb-ft-lemmaansatz`
* `a.dwds-bookmark-button[data-hidx]`
* `div.dwdswb-ft-block`
* `span.dwdswb-ft-blocklabel`
* `span.dwdswb-ft-blocktext`
* `div.dwdswb-lesarten`
* `div.dwdswb-lesart`
* `span.dwdswb-lesart-n`
* `div.dwdswb-lesart-content`
* `div.dwdswb-lesart-def`
* `div.dwdswb-verwendungsbeispiele`
* `span.dwdswb-belegtext`
* `div.dwdswb-diasystematik`
* `div.etymwb-entry`
* `div[id^="relation-block-"][id$="-mwa"]`

Homographen können als mehrere Artikelbereiche beziehungsweise Tab-Panes auf derselben Seite vorkommen. `Bank` ist der zentrale Testfall.

#### Homographen

Portiere die Logik aus:

* `woerterbuch-dwds--article-scope-p`
* `woerterbuch-dwds--article-scopes`
* `woerterbuch-dwds--parse-homograph`

Regeln:

* Wenn `tab-pane`-Scopes mit echter `dwdswb-artikel`-Struktur existieren, parse diese als getrennte Einträge.
* Scope mit `id="0"` ignorieren, analog zum Elisp-Code.
* Falls keine solchen Tab-Panes existieren, aber die Seite selbst einen Artikel enthält, parse die ganze Seite als einen Scope.
* Jeder Homograph wird ein eigener `DictionaryEntry`.
* Reihenfolge bleibt DOM-Reihenfolge.
* Entry-IDs sind stabil: `1`, `2`, `3`, ...
* `DictionaryEntry.homograph` wird aus `a.dwds-bookmark-button[data-hidx]` gelesen.
* Falls `data-hidx` fehlt, fallback auf Scope-`id`.
* Bei einem leeren Homograph-Wert kann `homograph = None` bleiben.

Für `Bank` müssen zwei Einträge entstehen:

* Entry 1: `hidx = "1"`, Plural `Bänke`
* Entry 2: `hidx = "2"`, Plural `Banken`

Beide teilen sich dieselbe kanonische URL:

```text
https://www.dwds.de/wb/Bank
```

#### Mapping auf Rust-Modelle

Mapping:

* DWDS-Lemma aus `h1.dwdswb-ft-lemmaansatz b` -> `DictionaryEntry.headword`
* vollständiger Titel aus `h1.dwdswb-ft-lemmaansatz` -> `DictionaryEntry.title`
* Homograph-ID aus `data-hidx` oder Scope-ID -> `DictionaryEntry.homograph`
* Grammatikblock mit Label `Grammatik` -> `DictionaryEntry.grammar`
* Wortart aus Grammatik -> `DictionaryEntry.part_of_speech`
* Etymologie aus `div.etymwb-entry` -> `DictionaryEntry.etymology`
* Mehrwortausdrücke aus Relation-Block `relation-block-<n>-mwa` -> `DictionaryEntry.idioms`
* Lesarten aus `div.dwdswb-lesarten` -> `DictionaryEntry.senses`
* DWDS-Lesart-ID, zum Beispiel `d-1-1` -> `Sense.source_id`
* Lesart-Label, zum Beispiel `1.`, `a)`, `●` -> `Sense.label`
* Definitionstext -> `Sense.definition`
* Diasystematische Angaben -> `Sense.qualifiers`
* Verwendungsbeispiele -> `Sense.examples`
* Unterlesarten -> `Sense.subsenses`

DWDS-Synonyme bleiben leer.

#### Grammatik und Wortart

Portiere:

* `woerterbuch-dwds--field-text`
* `woerterbuch-dwds--wortart-from-grammar`

Regeln:

* Grammatik steht in einem `dwdswb-ft-block`, dessen Label `Grammatik` enthält.
* Blocktext wird bereinigt als `grammar` gespeichert.
* `part_of_speech` wird aus dem Anfang von `grammar` extrahiert:

 * vor dem ersten `·` abschneiden
 * Klammerzusätze entfernen
 * trimmen

Beispiele:

```text
Substantiv (Femininum) · Genitiv Singular: Bank · Nominativ Plural: Bänke
-> part_of_speech = Substantiv

Substantiv (Neutrum) · Genitiv Singular: Hauses · Nominativ Plural: Häuser
-> part_of_speech = Substantiv
```

#### Bedeutungen / Lesarten

Portiere die rekursive Logik aus:

* `woerterbuch-dwds--make-definition-parser`
* `woerterbuch-dwds--extract-definition-text`

DWDS-Lesarten sind rekursiv:

```text
div.dwdswb-lesarten
 div.dwdswb-lesart
   span.dwdswb-lesart-n
   div.dwdswb-lesart-content
     div.dwdswb-lesart-def
     div.dwdswb-verwendungsbeispiele
     div.dwdswb-lesart
```

Regeln:

* Nur direkte Child-Lesarten der aktuellen Ebene als `subsenses` übernehmen.
* Nicht alle Descendants global sammeln.
* Parent-Sense darf nicht versehentlich Beispiele/Definitionen aus Unterlesarten bekommen.
* `Sense.id` ist die laufende Nummer innerhalb der jeweiligen Ebene.
* `Sense.source_id` ist das DWDS-HTML-`id`, zum Beispiel `d-2-3-1`.
* `Sense.label` ist der Text aus `dwdswb-lesart-n`.
* `Sense.definition` kann leer sein, wenn die Bedeutung nur Qualifier und Unterbedeutungen hat.
* Unterbedeutungen werden rekursiv in `Sense.subsenses` gespeichert.

Für `Bank` muss unter anderem diese Struktur möglich sein:

```text
entry 1
sense 1 label=1. source_id=d-1-1 definition=Sitz für mehrere Personen nebeneinander, meist aus Holz
 sense 1 label=● source_id=d-1-1-1 definition=ohne Ausnahme
sense 2 label=2. source_id=d-1-2 definition=Handwerkstisch
sense 3 label=3. source_id=d-1-3 definition=Zusammenballung, Anhäufung
 sense 1 label=a) source_id=d-1-3-1 definition=von Sand, Fels, Schlamm, Tieren in Gewässern
```

#### Definitionstext

Portiere die Logik aus:

* `woerterbuch-dwds--extract-definition-text`
* `woerterbuch-dwds--text-skipping-classes`
* `woerterbuch-dwds--extract-reference-text`
* `woerterbuch-dwds--reference-definition`

Definitionstext soll nur aus fachlich relevanten Blöcken zusammengesetzt werden:

* `dwdswb-verweise`
* `dwdswb-syntagmatik`
* `dwdswb-definitionen`
* `dwdswb-definition`

Zu überspringen:

* `dwdswb-binnenquelle`
* `dwdswb-paraphrase`, außer bei MWA-Extraktion
* Quellenangaben
* Beleg-Metadaten
* UI-/Layout-Texte

DWDS-Verweise können Definitionen in `data-content` enthalten. Diese müssen als Text extrahiert und HTML-Tags entfernt werden.

Beispiel aus `Bank`:

```text
Synonym zu Bankhalter = Person, die das Spiel leitet, die Einsätze verwaltet und gegen die die übrigen Spieler spielen
```

#### Mehrwortausdrücke / MWA

Portiere besonders sorgfältig:

* `woerterbuch-dwds--mwa-marker-p`
* `woerterbuch-dwds--local-mwa-marker-p`
* `woerterbuch-dwds--find-local-mwa-scope`
* `woerterbuch-dwds--extract-mwa-text`
* `woerterbuch-dwds--explicit-phraseme-block-p`
* `woerterbuch-dwds--normalize-paraphrase-text`

DWDS markiert Mehrwortausdrücke unter anderem mit:

* `letter-mwa.svg`
* Tooltip/Text `Mehrwortausdruck`
* Klassen wie `dwdswb-phraseme`
* `dwdswb-phrasem`
* `dwdswb-konstruktionsmuster`
* `dwdswb-syntagmatik`

Erwartetes Format für MWA-Definitionen:

```text
eine Bank sein (MWA) = etw. sein, das die an es gestellten (hohen) Erwartungen verlässlich erfüllt; jmd. sein, der die von ihm erwartete (erfolgreiche) Leistung mit Sicherheit erbringt
```

Diese MWA-Definition gehört in `Sense.definition`, nicht in `DictionaryEntry.idioms`.

#### Qualifier

Portiere:

* `woerterbuch-dwds--qualifier-node-p`
* `woerterbuch-dwds--collect-qualifiers`
* `woerterbuch-dwds--extract-qualifiers`

Regeln:

* Qualifier stehen innerhalb von `dwdswb-diasystematik`.
* Der Container selbst zählt nicht als Qualifier.
* Unterknoten mit DWDS-Klassen werden in DOM-Reihenfolge gelesen.
* Leere Texte ignorieren.
* Beispiele:

 * `metonymisch`
 * `Glücksspiel`
 * `umgangssprachlich`
 * `übertragen`

Diese werden in `Sense.qualifiers` gespeichert.

#### Beispiele

Portiere:

* `woerterbuch-dwds--extract-examples`

Regeln:

* Beispiele nur aus dem zur jeweiligen Lesart gehörenden `dwdswb-verwendungsbeispiele`-Block extrahieren.
* Nur `dwdswb-belegtext` verwenden.
* Quellen, Zeitungsnamen, Jahreszahlen und DWDS-Metadaten nicht übernehmen.
* Beispiele bleiben an der passenden Bedeutung.
* Keine globale Beispiel-Sammlung.
* Keine pauschale Zuordnung aller Beispiele zur ersten Bedeutung.

#### Idiome / Mehrwortausdrücke als Relation-Block

Portiere:

* `woerterbuch-dwds--parse-idioms`
* `woerterbuch-dwds--extract-idioms-from-block`
* `woerterbuch-dwds--find-all-links`

DWDS hat zusätzlich Relation-Blöcke für Mehrwortausdrücke:

```text
id="relation-block-1-mwa"
id="relation-block-2-mwa"
```

Regeln:

* Nur Relation-Block des jeweiligen Artikels/Homographen parsen.
* Links mit `href` beginnend mit `/wb/` übernehmen.
* Linktext bereinigen.
* Deduplizieren mit stabiler Reihenfolge.
* Ergebnis in `DictionaryEntry.idioms`.

Beispiel `Bank`:

Entry 1:

```text
durch die Bank
etw. auf die lange Bank schieben
```

Entry 2:

```text
eine Bank sein
sichere Bank
todsichere Bank
```

#### Etymologie

Portiere:

* `woerterbuch-dwds--parse-etymology`

Regeln:

* Innerhalb des jeweiligen Homograph-Scopes nach `etymwb-entry` suchen.
* Text bereinigen.
* Wenn kein Etymologie-Text vorhanden ist, `None`.
* Bei Homographen darf die Etymologie nicht zwischen den Einträgen vermischt werden.

Für `Bank` müssen zwei verschiedene Herkunftstexte entstehen:

* `1 Bank f. ‘Sitzmöbel für mehrere’ ...`
* `2 Bank f. ‘Geschäft für Geldverkehr’ ...`

#### Textbereinigung

Portiere `woerterbuch-dwds--clean-text`.

Mindestregeln:

* Whitespace inklusive Non-Breaking-Spaces normalisieren.
* Mehrfachspaces zu einem Space.
* Zeilenumbrüche/Tabs zu Spaces.
* Leerzeichen vor `,` und `.` entfernen.
* Leerzeichen direkt nach `(` und direkt vor `)` entfernen.
* Leerzeichen nach `⟨` und vor `⟩` entfernen.
* Text trimmen.
* Fachlich relevante Typografie erhalten:

 * `⟨...⟩`
 * `²Bank`
 * `100-Dollar-Chips`
 * Gedankenstriche
 * Anführungszeichen
 * Klammerzusätze wie `(MWA)`

Nicht übernehmen:

* Navigation
* Buttons
* Lesezeichen/Zitieren-UI
* Quellen-Metadaten bei Beispielen
* Layouttexte
* versteckte Tooltip-Artefakte, außer gezielt ausgewertete `data-content`-Definitionen

#### Fixtures

Nutze die vorhandenen alten DWDS-Fixtures als Ausgangspunkt. Sie liegen bereits unter:

```text
../../emacs-lisp/tests/files/dwds/
```

Vorhandene relevante Fixtures:

* `Bank/dwds-Bank.html`
* `Haus/dwds-Haus.html`
* `springen/dwds-springen.html`
* `Wolke/dwds-Wolke.html`
* `Zaun/dwds-Zaun.html`
* `verlieben/dwds-verlieben.html`
* `Nixdaexistiert/dwds-Nixdaexistiert.html`

Die alten Expected-Dateien sind wichtige fachliche Oracle-Referenzen:

* `dwds-*-definitions-expected.el`
* `dwds-*-examples-expected.el`
* `dwds-*-origin-expected.el`
* `dwds-*-idioms-expected.el`
* `dwds-*-synonyms-expected.el`

Empfohlene neue Rust-Fixture-Struktur, falls nicht direkt die alten Fixtures per `include_str!` verwendet werden:

```text
tests/fixtures/dwds/Bank.html
tests/fixtures/dwds/Haus.html
tests/fixtures/dwds/springen.html
tests/fixtures/dwds/Wolke.html
tests/fixtures/dwds/Zaun.html
tests/fixtures/dwds/verlieben.html
tests/fixtures/dwds/Nixdaexistiert.html
```

Tests müssen offline laufen. Keine Live-Requests in Tests.

#### Snapshot-Tests

Ergänze DWDS-Snapshots analog zu den bestehenden Quellen:

```text
tests/snapshots/dwds/Bank.snap
tests/snapshots/dwds/Haus.snap
tests/snapshots/dwds/springen.snap
tests/snapshots/dwds/Wolke.snap
tests/snapshots/dwds/Zaun.snap
tests/snapshots/dwds/verlieben.snap
tests/snapshots/dwds/Nixdaexistiert.snap
```

Die Snapshot-Ausgabe soll textuell, deterministisch und gut diffbar sein.

Der Snapshot-Renderer muss rekursiv mit `Sense.subsenses` umgehen.

Beispielhafte Form:

```text
source=Dwds
ok=true
url=https://www.dwds.de/wb/Bank
entry 1 homograph=1 headword=Bank title=Bank, die part_of_speech=Substantiv grammar=Substantiv (Femininum) · Genitiv Singular: Bank · Nominativ Plural: Bänke url=https://www.dwds.de/wb/Bank
idioms=[durch die Bank | etw. auf die lange Bank schieben]
etymology=1 Bank f. ‘Sitzmöbel für mehrere’ ...
sense 1 source_id=d-1-1 label=1. definition=Sitz für mehrere Personen nebeneinander, meist aus Holz
examples=[eine Bank im Park, vor dem Haus | Anlagen mit Bänken | ...]
 sense 1 source_id=d-1-1-1 label=● definition=ohne Ausnahme
 examples=[auf den Schwindel sind alle durch die Bank hereingefallen | ...]
sense 2 source_id=d-1-2 label=2. definition=Handwerkstisch
sense 3 source_id=d-1-3 label=3. definition=Zusammenballung, Anhäufung
 sense 1 source_id=d-1-3-1 label=a) definition=von Sand, Fels, Schlamm, Tieren in Gewässern

entry 2 homograph=2 headword=Bank title=Bank, die part_of_speech=Substantiv grammar=Substantiv (Femininum) · Genitiv Singular: Bank · Nominativ Plural: Banken url=https://www.dwds.de/wb/Bank
idioms=[eine Bank sein | sichere Bank | todsichere Bank]
etymology=2 Bank f. ‘Geschäft für Geldverkehr’ ...
sense 1 source_id=d-2-1 label=1. definition=Unternehmen, das gewerbsmäßig Geldgeschäfte und Börsengeschäfte betreibt
 sense 1 source_id=d-2-1-1 label=● definition=einzelne Filiale einer ²Bank
 qualifiers=[metonymisch]
sense 3 source_id=d-2-3 label=3. definition=-
qualifiers=[Glücksspiel]
 sense 3 source_id=d-2-3-3 label=c) definition=eine Bank sein (MWA) = etw. sein, das die an es gestellten (hohen) Erwartungen verlässlich erfüllt; jmd. sein, der die von ihm erwartete (erfolgreiche) Leistung mit Sicherheit erbringt
 qualifiers=[umgangssprachlich]
```

Für `Nixdaexistiert`:

```text
source=Dwds
ok=false
url=-
error=No matches found
```

Die genaue Formatierung darf an vorhandene Snapshot-Helfer angepasst werden, muss aber stabil bleiben.

#### Zusätzliche Unit-Tests

Neben Snapshot-Tests bitte gezielte Unit-Tests für Parser-Hilfsfunktionen ergänzen.

Mindestens testen:

* URL-Bildung:

 * `Bank` -> `https://www.dwds.de/wb/Bank`
 * Leerzeichen und Sonderzeichen werden URL-encodiert.
* Canonical-URL:

 * `<link rel="canonical" href="...">` wird bevorzugt.
 * Fallback ist die gebaute URL.
* Entry-Erkennung:

 * HTML mit `dwdswb-artikel` ist Entry-Page.
 * HTML ohne `dwdswb-artikel` ergibt No-Match.
* Homographen:

 * `Bank` ergibt zwei Entries.
 * `data-hidx` wird in `DictionaryEntry.homograph` übernommen.
 * Scope `id="0"` wird ignoriert.
* Titel/Lemma:

 * `h1.dwdswb-ft-lemmaansatz` -> Titel.
 * `b` darin -> Lemma.
* Grammatik/Wortart:

 * `Grammatik`-Block wird gefunden.
 * Wortart wird aus Grammatik extrahiert.
* Rekursive Lesarten:

 * Parent-Sense mit Child-Senses.
 * Direkte Kinder statt globale Descendants.
 * DWDS-IDs wie `d-1-3-1` bleiben in `Sense.source_id`.
* Definitionstext:

 * `dwdswb-definition`
 * `dwdswb-definitionen`
 * `dwdswb-syntagmatik`
 * `dwdswb-verweise`
 * `data-content` mit HTML wird in Text umgewandelt.
* Skip-Klassen:

 * `dwdswb-binnenquelle`
 * `dwdswb-paraphrase` bei normaler Definition
* Qualifier:

 * `metonymisch`
 * `Glücksspiel`
 * `umgangssprachlich`
 * `übertragen`
* MWA:

 * Marker über `letter-mwa.svg`.
 * Marker über Tooltip `Mehrwortausdruck`.
 * Format `Phrase (MWA) = Paraphrase`.
* Beispiele:

 * Beispiele bleiben bei der passenden Lesart.
 * Keine globale Sammlung auf erster Bedeutung.
 * Nur `dwdswb-belegtext`.
* Idiome:

 * Relation-Block `relation-block-<n>-mwa`.
 * Nur `/wb/`-Links.
 * Deduplizierung mit stabiler Reihenfolge.
* Etymologie:

 * `etymwb-entry` je Homograph.
 * Herkunftstexte werden nicht zwischen Homographen vermischt.
* Textbereinigung:

 * Non-Breaking-Spaces.
 * Spaces vor Satzzeichen.
 * Spaces in Klammern.
 * Spaces in `⟨...⟩`.
* Fehlerfall:

 * `Nixdaexistiert` -> `ok=false`, `error=No matches found`.

#### Hinweise zu den alten Expected-Dateien

Die alten `.el`-Expected-Dateien sind keine 1:1-Rust-Snapshot-Vorlage, aber sie sind die fachliche Oracle-Referenz.

Besonders hilfreiche Dateien:

* `../../emacs-lisp/tests/files/dwds/Bank/dwds-Bank-definitions-expected.el`
* `../../emacs-lisp/tests/files/dwds/Bank/dwds-Bank-examples-expected.el`
* `../../emacs-lisp/tests/files/dwds/Bank/dwds-Bank-origin-expected.el`
* `../../emacs-lisp/tests/files/dwds/Bank/dwds-Bank-idioms-expected.el`
* `../../emacs-lisp/tests/files/dwds/Haus/dwds-Haus-definitions-expected.el`
* `../../emacs-lisp/tests/files/dwds/Haus/dwds-Haus-examples-expected.el`
* `../../emacs-lisp/tests/files/dwds/Haus/dwds-Haus-origin-expected.el`
* `../../emacs-lisp/tests/files/dwds/springen/dwds-springen-definitions-expected.el`
* `../../emacs-lisp/tests/files/dwds/springen/dwds-springen-examples-expected.el`
* `../../emacs-lisp/tests/files/dwds/Wolke/dwds-Wolke-definitions-expected.el`
* `../../emacs-lisp/tests/files/dwds/Zaun/dwds-Zaun-definitions-expected.el`
* `../../emacs-lisp/tests/files/dwds/verlieben/dwds-verlieben-definitions-expected.el`
* `../../emacs-lisp/tests/files/dwds/Nixdaexistiert/dwds-Nixdaexistiert-definitions-expected.el`

Die Rust-Snapshots sollen die gleichen fachlichen Daten enthalten, aber im vorhandenen Rust-Snapshot-Stil.

#### Workflow für Codex

Beim Umsetzen:

1. `.project/agents/AGENTS.md` und `.project/agents/repository.md` lesen.
2. Task-Datei nach Template anlegen, zum Beispiel `.project/tasks/2026-06-09--dwds-parser.md`.
3. Diese TODO-Heading beim Start mit der Task-Datei verlinken.
4. DWDS-Parser implementieren.
5. Fixtures/Snapshots/Unit-Tests ergänzen.
6. Relevante Checks ausführen.
7. Task-Datei mit Result, Changes, Checks und Open Points aktualisieren.
8. TODO-Status am Ende auf `REVIEW` setzen.

#### Akzeptanzkriterien

* DWDS-Live-Lookups verwenden `https://www.dwds.de/wb/<lemma>`.
* Die kanonische DWDS-URL wird aus dem HTML gelesen, falls vorhanden.
* `lookup(client, query)` bleibt für HTTP zuständig.
* Parser-Logik ist unabhängig vom HTTP-Code testbar.
* Tests laufen offline.
* Lokale DWDS-HTML-Fixtures werden verwendet.
* DWDS-Snapshots existieren unter `tests/snapshots/dwds/`.
* HTML ohne `dwdswb-artikel` ergibt `ok=false` und `error=No matches found`.
* `Bank` ergibt zwei `DictionaryEntry`-Einträge.
* Homograph-IDs werden in `DictionaryEntry.homograph` gespeichert.
* `DictionaryEntry.headword`, `title`, `part_of_speech`, `grammar`, `etymology`, `idioms`, `url` und `senses` werden sinnvoll befüllt.
* DWDS-Synonyme bleiben leer.
* Lesarten werden rekursiv als `Sense`/`subsenses` abgebildet.
* DWDS-Lesart-IDs wie `d-1-1` werden in `Sense.source_id` erhalten.
* Labels wie `1.`, `a)`, `●` werden stabil gesetzt.
* Definitionen werden aus den richtigen DWDS-Blöcken extrahiert.
* Verweise mit `data-content` werden korrekt als Definitionstext aufgelöst.
* Qualifier werden als `Sense.qualifiers` extrahiert.
* Beispiele werden der passenden Lesart zugeordnet.
* MWA-Definitionen werden als Definitionstext erkannt.
* Idiome aus Relation-Blöcken werden als `DictionaryEntry.idioms` extrahiert.
* Herkunft wird pro Homograph extrahiert.
* Fehlende Sektionen führen nicht zu Fehlern oder Panics.
* Navigation, Buttons, Quellen-Metadaten und Layout-Artefakte erscheinen nicht in Snapshots.
* Snapshot-Ausgaben sind stabil, deterministisch und gut diffbar.
* `cargo test dwds` läuft erfolgreich.
* Wenn möglich, läuft auch `cargo test` erfolgreich.
* Ein kurzer manueller Check mit `cargo run -- Bank --sources dwds --json` liefert zwei DWDS-Entries.

#### Out of scope

* Kein Lemmatizer.
* Kein Caching.
* Kein Rate Limiting.
* Keine DWDS-Suchseitenlogik.
* Keine DWDS-Synonym-Implementierung.
* Keine große CLI-Überarbeitung.
* Keine Änderungen an OpenThesaurus, Wiktionary oder Duden, außer wenn ein winziger gemeinsamer Test-/Snapshot-Helfer wirklich sinnvoll ist.
* Keine vollständige 1:1-Kompatibilität mit dem alten Emacs-Lisp-Plist-Output.
* Keine Live-Requests in Tests.
* Keine Umstellung auf `insta` oder ein neues Snapshot-Framework.

## DONE Duden: HTML-Parser + Snapshot-Tests

Task file: [./2026-06-09--duden-parser.md](./2026-06-09--duden-parser.md)

Portiere die alte Duden-Scraping-Logik aus Emacs Lisp nach Rust und ersetze den aktuellen groben Duden-Stub durch einen robusten AMP-HTML-Parser mit deterministischen Offline-Snapshot-Tests.

Diese Aufgabe ist der nächste Parser-Schritt nach OpenThesaurus und Wiktionary. Maßgeblich ist nicht eine freie Neuinterpretation des Duden-HTMLs, sondern die fachliche Logik aus `woerterbuch-duden.el`, idiomatisch in Rust umgesetzt und auf die vorhandenen Rust-Modelle gemappt.

### Wichtigste geprüfte Grundlage

Unbedingt zuerst lesen:

- `../../emacs-lisp/lisp/woerterbuch-duden.el`
- `../../emacs-lisp/tests/test-helper.el`
- `../../emacs-lisp/tests/test-woerterbuch-duden.el`
- `../../emacs-lisp/tests/files/duden/`
- `../../src/sources/duden.rs`
- `../../src/sources/wiktionary.rs`
- `../../src/sources/openthesaurus.rs`
- `../../src/models.rs`

Aus `woerterbuch-duden.el` sind insbesondere diese Funktionen relevant und sollen fachlich portiert werden:

- `woerterbuch-duden--build-url`
- `woerterbuch-duden--build-search-url`
- `woerterbuch-duden--clean-text`
- `woerterbuch-duden--text`
- `woerterbuch-duden--tuple-pairs`
- `woerterbuch-duden--notes`
- `woerterbuch-duden--definition-label`
- `woerterbuch-duden--extract-image-url`
- `woerterbuch-duden--extract-qualifiers`
- `woerterbuch-duden--extract-shortform-definition`
- `woerterbuch-duden--extract-definition-node`
- `woerterbuch-duden--parse-single-definition-section`
- `woerterbuch-duden--parse-definitions`
- `woerterbuch-duden--extract-title-node`
- `woerterbuch-duden--extract-lemma`
- `woerterbuch-duden--extract-title`
- `woerterbuch-duden--field-value`
- `woerterbuch-duden--wortart-from-grammar`
- `woerterbuch-duden--extract-origin`
- `woerterbuch-duden--split-synonym-text`
- `woerterbuch-duden--extract-synonyms`
- `woerterbuch-duden--parse-search-results`
- `woerterbuch-duden--result-from-homographs`
- `woerterbuch-duden--no-match-result`

Der aktuelle Rust-Code in `src/sources/duden.rs` ist nur ein grober Stub. Er verwendet Selektoren wie `.tuple__wortart` und `.tuple__grammatik`, die in den vorhandenen Duden-AMP-Fixtures nicht die eigentliche Struktur abbilden. Die echte Struktur besteht aus `h1.lemma__title`, `span.lemma__main`, `dl.tuple`, `div.division#bedeutungen`, `ol.enumeration`, `li.enumeration__item`, `li.enumeration__sub-item`, `dl.note`, `div#synonyme` und `div#herkunft`.

### Ziel

Der Duden-Parser soll Duden-AMP-HTML in die vorhandenen Rust-Modelle überführen:

- `SourceResult`
- `DictionaryEntry`
- `Sense`
- `SynonymGroup`
- `UrlValue`

Die Ausgabe muss die vorhandene Architektur respektieren und nach dem Muster von OpenThesaurus/Wiktionary über lokale Fixtures und `.snap`-Dateien testbar sein.

### Architektur

Halte den Scope möglichst auf `src/sources/duden.rs` begrenzt.

Erwartete Struktur:

- `lookup(client, query) -> Result<SourceResult>` bleibt für HTTP, Statuscodes, Fallback-Suche und Fehlerbehandlung zuständig.
- Der eigentliche Parser bleibt unabhängig vom HTTP-Code testbar.
- Der Parser soll ein vollständiges `SourceResult` oder testbare Hilfsfunktionen liefern, nicht nur lose Textlisten.
- Tests dürfen keine Live-Requests machen.
- Keine Panics bei fehlenden oder unerwarteten HTML-Sektionen.
- Keine große Umstellung der CLI.
- Keine Änderung an gemeinsamen Modellen, außer es ist wirklich notwendig.
- Keine Umstellung auf ein neues Snapshot-Framework.

Die zentrale Section-Filterung existiert bereits über `SourceResult::retain_sections` in `src/models.rs`. Daher in Duden nicht wieder das alte Elisp-`sections`-Argument nachbauen. Der Rust-Duden-Parser soll grundsätzlich alle gefundenen Daten extrahieren; die gewünschte Auswahl übernimmt danach die bestehende zentrale Logik.

### Live-Lookup und URLs

Die alte Duden-Implementierung und der Projektkontext nutzen die schlanke AMP-Seite.

Portiere die URL-Logik aus `woerterbuch-duden.el`:

- Direkte Duden-Eintrags-URL:
  - Basis: `https://www.duden.de/rechtschreibung/`
  - Leerraum im Lemma zuerst durch `_` ersetzen.
  - Danach URL-encoden.
  - Am Ende `?amp` anhängen.
  - Beispiel: `Bank` -> `https://www.duden.de/rechtschreibung/Bank?amp`

- Duden-Such-URL:
  - Basis: `https://www.duden.de/suchen/dudenonline/`
  - Suchbegriff URL-encoden.
  - Beispiel: `Bank` -> `https://www.duden.de/suchen/dudenonline/Bank`

Für diese Aufgabe ist das alte Duden-Verhalten mit `?amp` maßgeblich. Die alten Expected-Dateien erwarten ebenfalls URLs wie:

- `https://www.duden.de/rechtschreibung/Haus?amp`
- `https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp`
- `https://www.duden.de/rechtschreibung/Bank_Geldinstitut?amp`

Nicht in dieser Aufgabe auf normale Nicht-AMP-Ergebnis-URLs umstellen.

### HTTP-Verhalten

Der aktuelle `fetch_html`-Helper nutzt `error_for_status()` und ist für Duden allein nicht ausreichend, weil Duden bei Homographen wie `Bank` zuerst auf der direkten URL `404` liefern kann und danach die Suchseite abgefragt werden muss.

Portiere das Verhalten aus dem Elisp-Code:

1. Zuerst direkte AMP-URL abrufen.
2. Wenn die direkte URL erfolgreich ist, diese eine Eintragsseite parsen.
3. Wenn die direkte URL `404` ergibt, Duden-Suche abrufen.
4. Suchergebnisseite parsen und exakt passende Eintragslinks sammeln.
5. Alle exakt passenden Eintragsseiten abrufen und als mehrere `DictionaryEntry`-Werte zurückgeben.
6. Wenn keine passenden Einträge gefunden werden, `SourceResult::error(Source::Duden, "No matches found")` zurückgeben.
7. Bei Nicht-404-HTTP-Fehlern oder Netzwerkfehlern saubere Fehler liefern.

Für `Bank` ist der alte Testablauf in `test-helper.el` wichtig:

- direkte URL `https://www.duden.de/rechtschreibung/Bank?amp` -> simuliert `404`
- Suchseite `https://www.duden.de/suchen/dudenonline/Bank`
- daraus exakt:
  - `https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp`
  - `https://www.duden.de/rechtschreibung/Bank_Geldinstitut?amp`
- nicht aufnehmen:
  - `Merchant_Bank`
  - `Bad_Bank`
  - `Near_Bank`
  - `Bankster`
  - `Banker`
  - `_bank`
  - sonstige Teilsuche-Treffer

### Suchseite und Homographen

Portiere die Suchlogik aus `woerterbuch-duden--parse-search-results`.

Duden-Suchseiten enthalten einen Segmentblock:

- `div.segment`
- darin `h2.segment__title` mit Text `Wörterbuch`
- darin mehrere `section.vignette`
- relevante Links über `a.vignette__label[href]`

Exakt passende Treffer werden so erkannt:

- Link muss mit `/rechtschreibung/` beginnen.
- Sichtbares Lemma stammt bevorzugt aus `strong` innerhalb von `a.vignette__label`.
- Falls kein `strong` existiert, verwende den Text des Labels.
- Nur wenn der bereinigte sichtbare Text exakt dem bereinigten Suchwort entspricht, wird der Link übernommen.
- Reihenfolge der Treffer bleibt DOM-Reihenfolge.
- URLs werden zu absoluten Duden-URLs gemacht und mit `?amp` versehen.

Für mehrere Treffer:

- `SourceResult.url` soll `UrlValue::Many` enthalten.
- Jeder Treffer wird als eigener `DictionaryEntry` in `entries` abgelegt.
- Entry-IDs sind stabil: `1`, `2`, `3`, ...
- Die Reihenfolge entspricht der Suchergebnis-Reihenfolge.

### Mapping auf Rust-Modelle

Nutze die vorhandenen Felder aus `src/models.rs`.

Mapping:

- Duden-Lemma aus `span.lemma__main` -> `DictionaryEntry.headword`
- vollständiger Titel aus `h1.lemma__title` -> `DictionaryEntry.title`
- Duden-Wortart/Grammatik aus `dl.tuple` mit Key `Wortart` -> `DictionaryEntry.grammar`
- grobe Wortart aus `grammar` vor dem ersten Komma -> `DictionaryEntry.part_of_speech`
  - Beispiel `Substantiv, Neutrum` -> `Substantiv`
  - Beispiel `starkes Verb` -> `starkes Verb`
  - Beispiel `schwaches Verb` -> `schwaches Verb`
- Abschnitt `#herkunft` -> `DictionaryEntry.etymology`
- Abschnitt `#synonyme` -> `DictionaryEntry.synonym_groups`
- Abschnitt `#bedeutungen` oder `#bedeutung` -> `DictionaryEntry.senses`
- `li`-ID wie `Bedeutung-1a` -> `Sense.source_id`
- sichtbares Bedeutungslabel wie `1`, `1a`, `2b` -> `Sense.label`
- `div.enumeration__text` -> `Sense.definition`
- `dl.tuple` innerhalb einer Bedeutung -> `Sense.qualifiers`
- `dl.note` mit Titel `Beispiele` oder `Beispiel` -> `Sense.examples`
- `dl.note` mit Titel `Wendungen, Redensarten, Sprichwörter` -> `Sense.idioms`
- `figure.depiction a[href]` -> `Sense.image_url`
- Unterbedeutungen -> `Sense.subsenses`

Wichtig: Duden-Redewendungen stehen in den alten Fixtures meistens als `dl.note` direkt an einer Bedeutung oder Unterbedeutung. Sie sollen deshalb primär an `Sense.idioms` hängen, nicht pauschal an `DictionaryEntry.idioms`.

### Bedeutungen und Unterbedeutungen

Portiere die rekursive Logik aus `woerterbuch-duden--extract-definition-node`.

Die Duden-AMP-Struktur sieht typischerweise so aus:

- `div#bedeutungen`
- `ol.enumeration`
- direkte Kinder `li.enumeration__item`
- optional darin `ol.enumeration__sub`
- direkte Kinder davon `li.enumeration__sub-item`

Regeln:

- Top-Level-Bedeutungen erhalten Labels `1`, `2`, `3`, ...
- Unterbedeutungen erhalten Labels aus der Duden-ID, zum Beispiel `Bedeutung-1a` -> `1a`.
- Falls keine Duden-ID vorhanden ist, deterministisch fallbacken, zum Beispiel `1a`, `1b`.
- Wenn eine Top-Level-Bedeutung nur Unterbedeutungen hat, bleibt ihre eigene `definition` leer und die Unterbedeutungen stehen in `subsenses`.
- Wenn eine Top-Level-Bedeutung selbst `div.enumeration__text` hat, wird diese als eigene Definition übernommen.
- Für einfache Einträge ohne `ol.enumeration` soll `woerterbuch-duden--parse-single-definition-section` fachlich portiert werden.
- Kurze Formen mit führendem Tuple `Kurzform für` müssen als Definition erzeugt werden:
  - `Kurzform für: Sandbank`
  - `Kurzform für: verschiedene Handwerkstische wie Drehbank, Hobelbank, Werkbank u. a.`

Sehr wichtig für Rust mit `scraper`:

- Bei Bedeutungen möglichst direkte Kinder traversieren, nicht wahllos alle Descendants sammeln.
- Sonst landen Beispiele/Idiome aus Unterbedeutungen fälschlich auf dem Parent-Sense.
- Die Elisp-Funktionen `woerterbuch-duden--children-with-class`, `woerterbuch-duden--direct-child-by-tag-and-class` und `woerterbuch-duden--direct-children-by-tag-and-class` sind dafür die Vorlage.

### Beispiele, Redewendungen, Qualifier, Bilder

Portiere das Verhalten aus:

- `woerterbuch-duden--notes`
- `woerterbuch-duden--note-values`
- `woerterbuch-duden--extract-qualifiers`
- `woerterbuch-duden--extract-image-url`

Regeln:

- `dl.note` wird nur als direkter Note-Block der jeweiligen Bedeutung ausgewertet.
- Note-Titel werden normalisiert.
- `Beispiele` und `Beispiel` werden beide unterstützt.
- Alle `li` innerhalb der jeweiligen Note-Liste werden als Beispiele übernommen.
- `Wendungen, Redensarten, Sprichwörter` wird als Idiom-Liste der jeweiligen Bedeutung übernommen.
- `dl.tuple` innerhalb einer Bedeutung wird als Qualifier-Liste übernommen.
- Qualifier-Format: `Key: Value`, zum Beispiel `Gebrauch: Sport`.
- Tuple `Kurzform für` wird nicht als Qualifier übernommen, sondern als Definition-Fallback verwendet.
- Bild-URL kommt aus `figure.depiction a[href]`.

### Synonyme

Portiere die Logik aus:

- `woerterbuch-duden--split-synonym-text`
- `woerterbuch-duden--extract-synonyms`

Duden-Synonyme stehen in den Fixtures im Bereich:

- `div#synonyme`
- direkter `ul`
- darin `li`
- darin oft mehrere Links, getrennt durch Komma oder Semikolon

Regeln:

- Nur den eigentlichen Synonym-Block parsen, nicht den `nav.more`-Link `Zur Übersicht der Synonyme ...`.
- Synonyme auf Komma und Semikolon splitten, aber nicht innerhalb von Klammern.
- Beispiele:
  - `(österreichisch) sich verschauen` bleibt ein Synonym.
  - `(gehoben) in Liebe entbrennen/erglühen` bleibt ein Synonym.
- Reihenfolge bleibt stabil.
- Deduplizieren ohne Sortierung.
- Leere Einträge ignorieren.
- In Rust als `SynonymGroup` speichern.
- Wenn kein Sense-Bezug erkennbar ist, eine Gruppe mit `sense = None` verwenden.

### Herkunft

Portiere `woerterbuch-duden--extract-origin`.

Regeln:

- Bereich mit `id="herkunft"` suchen.
- Header, `small`, Navigation und Info-Icons ignorieren.
- Relevante direkte Kindtexte zusammenführen.
- Whitespace und HTML-Entities normalisieren.
- Wenn kein Herkunftstext vorhanden ist, `None`.

Beispiele aus den alten Expected-Dateien:

- `Haus`:
  - `mittelhochdeutsch, althochdeutsch hūs, eigentlich = das Bedeckende, Umhüllende`
- `Bank`, Sitzgelegenheit:
  - `mittelhochdeutsch, althochdeutsch banc = Bank, Tisch, ursprünglich = Erhöhung`
- `Bank`, Geldinstitut:
  - `italienisch banco, banca, eigentlich = Tisch des Geldwechslers, aus dem Germanischen`
- `verlieben`:
  - kein Herkunftstext, also `None`

### Textbereinigung

Portiere `woerterbuch-duden--clean-text` und ergänze nur vorsichtig, wo Rust/HTML es nötig macht.

Mindestregeln:

- Whitespace inklusive Non-Breaking-Spaces normalisieren.
- Mehrfachspaces zu einem Space.
- `〈` zu `⟨`.
- `〉` zu `⟩`.
- Leerzeichen vor `,` und `.` entfernen.
- Leerzeichen direkt nach `(` und direkt vor `)` entfernen.
- HTML-Entities sauber dekodieren, soweit `scraper`/Textnodes dies liefern.
- Duden-Layout-Texte wie `Anzeige`, `Weitere Beispiele anzeigen`, `Zur Übersicht der Synonyme ...`, Share-Buttons und Navigation nicht in Ergebnistext übernehmen.
- Sichtbare Duden-Schreibweise mit Soft Hyphen in Headword/Title nicht unnötig zerstören. Beispiele: `sprin­gen`, `Wol­ke`, `ver­lie­ben`.

Nicht die fachlich relevanten Duden-Typografie-Marker entfernen:

- `[ver]mieten`
- `[jemandem]`
- `⟨in übertragener Bedeutung:⟩`
- `(umgangssprachlich)`
- `(gehoben)`

### Fixtures

Lege lokale Rust-Fixtures an, vorzugsweise unter:

- `tests/fixtures/duden/`

Nutze die vorhandenen alten Duden-AMP-Fixtures als Ausgangspunkt. Diese liegen bereits im Repository unter:

- `../../emacs-lisp/tests/files/duden/Haus/duden-Haus.html`
- `../../emacs-lisp/tests/files/duden/Bank/duden-Bank-search.html`
- `../../emacs-lisp/tests/files/duden/Bank/duden-Bank-1.html`
- `../../emacs-lisp/tests/files/duden/Bank/duden-Bank-2.html`
- `../../emacs-lisp/tests/files/duden/springen/duden-springen.html`
- `../../emacs-lisp/tests/files/duden/verlieben/duden-verlieben.html`
- `../../emacs-lisp/tests/files/duden/Wolke/duden-Wolke.html`
- `../../emacs-lisp/tests/files/duden/Zaun/duden-Zaun.html`

Empfohlene Rust-Fixture-Namen:

- `tests/fixtures/duden/Haus.html`
- `tests/fixtures/duden/Bank-search.html`
- `tests/fixtures/duden/Bank-Sitzgelegenheit.html`
- `tests/fixtures/duden/Bank-Geldinstitut.html`
- `tests/fixtures/duden/springen.html`
- `tests/fixtures/duden/verlieben.html`
- `tests/fixtures/duden/Wolke.html`
- `tests/fixtures/duden/Zaun.html`

Für `Nixdaexistiert` gibt es in den alten Duden-Dateien nur Expected-Output, aber keine echte HTML-Fixture. Der Rust-Test für diesen Fall kann deshalb über einen `not_found_result`-Helper oder eine kleine synthetische leere Suchseite laufen.

Keine Live-Downloads in Tests.

### Snapshot-Tests

Ergänze Duden-Snapshots analog zu den bestehenden Quellen:

- `tests/snapshots/duden/Bank.snap`
- `tests/snapshots/duden/Haus.snap`
- `tests/snapshots/duden/springen.snap`
- `tests/snapshots/duden/verlieben.snap`
- `tests/snapshots/duden/Wolke.snap`
- `tests/snapshots/duden/Zaun.snap`
- `tests/snapshots/duden/Nixdaexistiert.snap`

Die Snapshot-Ausgabe soll textuell, deterministisch und gut diffbar sein, ähnlich wie bei Wiktionary/OpenThesaurus.

Der Duden-Snapshot-Renderer muss rekursiv mit `Sense.subsenses` umgehen. Sonst fehlen bei `Haus` und `Bank` die wichtigsten Bedeutungen wie `1a`, `1b`, `2a`.

Beispielhafte Form:

    source=Duden
    ok=true
    url=https://www.duden.de/rechtschreibung/Haus?amp
    entry 1 headword=Haus title=Haus, das part_of_speech=Substantiv grammar=Substantiv, Neutrum
    etymology=mittelhochdeutsch, althochdeutsch hūs, eigentlich = das Bedeckende, Umhüllende
    synonyms sense=- items=[Anwesen, Bau, Bauwerk, Gebäude]
    sense 1 label=1 source_id=- definition=-
    subsense 1.1 label=1a source_id=Bedeutung-1a definition=Gebäude, das Menschen zum Wohnen dient
    image 1.1=https://cdn.duden.de/_media_/full/H/Haus-201020510799.jpg
    examples 1.1=[ein großes, kleines, altes, mehrstöckiges, verwinkeltes Haus | armselige, einfache, verkommene, baufällige, moderne Häuser | ...]
    idioms 1.1=[Haus und Hof ... | [jemandem] ins Haus stehen ...]
    subsense 1.2 label=1b source_id=Bedeutung-1b definition=Gebäude, das zu einem bestimmten Zweck errichtet wurde

Für `Bank` muss der Snapshot zwei Entries enthalten:

    source=Duden
    ok=true
    url=[https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp | https://www.duden.de/rechtschreibung/Bank_Geldinstitut?amp]
    entry 1 headword=Bank title=Bank, die part_of_speech=Substantiv grammar=Substantiv, feminin
    ...
    entry 2 headword=Bank title=Bank, die part_of_speech=Substantiv grammar=Substantiv, feminin
    synonyms sense=- items=[Bankhaus, Geldinstitut, Kreditanstalt, Kreditinstitut]
    ...

Für `Nixdaexistiert`:

    source=Duden
    ok=false
    url=-
    error=No matches found

Die genaue Formatierung darf an die vorhandenen Render-Helfer angepasst werden, muss aber stabil bleiben.

### Zusätzliche Unit-Tests

Neben Snapshot-Tests bitte gezielte Unit-Tests für Parser-Hilfsfunktionen ergänzen.

Mindestens testen:

- URL-Bildung:
  - `Bank` -> `https://www.duden.de/rechtschreibung/Bank?amp`
  - Leerzeichen im Lemma werden für Eintrags-URLs zu `_`.
  - Such-URL nutzt `/suchen/dudenonline/`.
- Textbereinigung:
  - Non-Breaking-Spaces.
  - `〈...〉` -> `⟨...⟩`.
  - Spaces vor Satzzeichen.
  - Kein Verlust relevanter Klammern oder eckiger Klammern.
- Titel/Lemma:
  - `h1.lemma__title` und `span.lemma__main`.
  - `Haus, das` vs. `Haus`.
  - Soft-Hyphen-Schreibungen wie `ver­lie­ben`.
- Tuple-Parsing:
  - `Wortart: ⓘ` -> Key `Wortart`.
  - `Substantiv, Neutrum` -> `grammar`.
  - `part_of_speech` aus erstem Teil vor Komma.
- Suchergebnisse:
  - `Bank-search.html` liefert exakt zwei URLs.
  - `Merchant_Bank`, `Bad_Bank`, `Near_Bank`, `Bankster`, `Banker` werden nicht übernommen.
- Bedeutungslabels:
  - `Bedeutung-1a` -> `1a`.
  - `Bedeutung-2` -> `2`.
  - Fallback für fehlende IDs.
- Rekursive Bedeutungen:
  - Parent-Sense mit `subsenses`.
  - Flache Bedeutung ohne Unterbedeutungen.
  - Single-definition-Fallback.
- Beispiele:
  - `Beispiele` und `Beispiel`.
  - Beispiele bleiben an der passenden Bedeutung.
  - Parent-Sense sammelt nicht versehentlich Beispiele aus allen Sub-Senses.
- Redewendungen:
  - `Wendungen, Redensarten, Sprichwörter`.
  - Speicherung in `Sense.idioms`.
- Qualifier:
  - `Gebrauch: Sport`.
  - `Kurzform für` wird nicht als Qualifier übernommen.
- Kurzform:
  - führendes Tuple `Kurzform für` wird Definition-Fallback.
- Bilder:
  - `figure.depiction a[href]`.
- Synonyme:
  - Split auf Komma/Semikolon außerhalb von Klammern.
  - Deduplizierung mit stabiler Reihenfolge.
  - Keine Aufnahme des `Zur Übersicht der Synonyme ...`-Links.
- Herkunft:
  - Header/Info-Icons werden ignoriert.
  - Herkunftstext wird korrekt zusammengesetzt.
- Fehlerfall:
  - kein Eintrag -> `ok=false`, `error=No matches found`.

### Hinweise zu den alten Expected-Dateien

Die alten `.el`-Expected-Dateien sind keine 1:1-Rust-Snapshot-Vorlage, aber sie sind die fachliche Oracle-Referenz.

Besonders hilfreiche Dateien:

- `../../emacs-lisp/tests/files/duden/Haus/duden-Haus-definitions-expected.el`
- `../../emacs-lisp/tests/files/duden/Haus/duden-Haus-examples-expected.el`
- `../../emacs-lisp/tests/files/duden/Haus/duden-Haus-idioms-expected.el`
- `../../emacs-lisp/tests/files/duden/Haus/duden-Haus-origin-expected.el`
- `../../emacs-lisp/tests/files/duden/Haus/duden-Haus-synonyms-expected.el`
- `../../emacs-lisp/tests/files/duden/Bank/duden-Bank-definitions-expected.el`
- `../../emacs-lisp/tests/files/duden/Bank/duden-Bank-examples-expected.el`
- `../../emacs-lisp/tests/files/duden/Bank/duden-Bank-origin-expected.el`
- `../../emacs-lisp/tests/files/duden/Bank/duden-Bank-synonyms-expected.el`
- `../../emacs-lisp/tests/files/duden/Nixdaexistiert/duden-Nixdaexistiert-definitions-expected.el`

Die Rust-Snapshots sollen die gleichen fachlichen Daten enthalten, aber im vorhandenen Rust-Snapshot-Stil.

### Workflow für Codex

Beim Umsetzen:

1. `.project/agents/AGENTS.md` und `.project/agents/repository.md` lesen.
2. Task-Datei nach Template anlegen, zum Beispiel `.project/tasks/2026-06-09--duden-parser.md`.
3. Diese TODO-Heading beim Start mit der Task-Datei verlinken.
4. Duden-Parser implementieren.
5. Fixtures/Snapshots/Unit-Tests ergänzen.
6. Relevante Checks ausführen.
7. Task-Datei mit Result, Changes, Checks und Open Points aktualisieren.
8. TODO-Status am Ende auf `REVIEW` setzen.

### Akzeptanzkriterien

- Duden nutzt live die AMP-Eintragsseiten mit `?amp`.
- Direkte Duden-URL wird zuerst versucht.
- Bei `404` wird die Duden-Suchseite verwendet.
- Exakte Suchtreffer werden erkannt.
- Homographen wie `Bank` ergeben mehrere `DictionaryEntry`-Einträge.
- `Bank` liefert genau die beiden alten Duden-Einträge `Bank_Sitzgelegenheit` und `Bank_Geldinstitut`.
- Nicht exakt passende Suchtreffer werden ignoriert.
- Parser-Logik ist unabhängig vom HTTP-Code testbar.
- Tests laufen offline.
- Lokale Duden-Fixtures existieren unter `tests/fixtures/duden/` oder einem konsistenten bestehenden Fixture-Ort.
- Duden-Snapshots existieren unter `tests/snapshots/duden/`.
- `DictionaryEntry.headword`, `title`, `part_of_speech`, `grammar`, `etymology`, `url`, `synonym_groups` und `senses` werden sinnvoll befüllt.
- Bedeutungen und Unterbedeutungen werden rekursiv als `Sense`/`subsenses` abgebildet.
- Duden-Bedeutungs-IDs wie `Bedeutung-1a` werden in `Sense.source_id` erhalten.
- Labels wie `1`, `1a`, `2b` werden stabil gesetzt.
- Beispiele werden der passenden Bedeutung zugeordnet.
- Redewendungen werden der passenden Bedeutung als `Sense.idioms` zugeordnet.
- Qualifier wie `Gebrauch: Sport` werden extrahiert.
- Kurzform-Tuples werden als Definition-Fallback verwendet.
- Bild-URLs werden extrahiert, soweit vorhanden.
- Synonyme werden dedupliziert und stabil sortierungsfrei gespeichert.
- Herkunft wird extrahiert, soweit vorhanden.
- Fehlende Sektionen führen nicht zu Fehlern oder Panics.
- Duden-Artefakte wie Anzeige, Navigation, Share-Buttons und `Zur Übersicht der Synonyme ...` erscheinen nicht in Snapshots.
- `Nixdaexistiert` ergibt `ok=false` und `error=No matches found`.
- `cargo test duden` läuft erfolgreich.
- Wenn möglich, läuft auch `cargo test` erfolgreich.

### Out of scope

- Kein Lemmatizer.
- Kein Caching.
- Kein Rate Limiting.
- Keine große CLI-Überarbeitung.
- Keine DWDS-Implementierung in dieser Aufgabe.
- Keine Änderungen an OpenThesaurus oder Wiktionary, außer wenn ein winziger gemeinsamer Test-/Snapshot-Helfer wirklich sinnvoll ist.
- Keine vollständige 1:1-Kompatibilität mit dem alten Emacs-Lisp-Plist-Output.
- Keine Live-Requests in Tests.
- Keine Umstellung auf `insta` oder ein neues Snapshot-Framework.
- Keine Umstellung der Duden-Ergebnis-URLs von `?amp` auf Nicht-AMP in dieser Aufgabe.

## DONE Wiktionary: REST-HTML-Parser + Snapshot-Tests [archive/2026-06-09--wiktionary-parser.md](archive/2026-06-09--wiktionary-parser.md)

Implementiere den Wiktionary-Parser in Rust fertig und ergänze Snapshot-Tests analog zur bestehenden OpenThesaurus-Implementierung.

Der aktuelle Rust-Code enthält bereits ein Wiktionary-Modul:

* [../../src/sources/wiktionary.rs](../../src/sources/wiktionary.rs)

Dieses Modul nutzt bereits den richtigen REST-Endpunkt:

```text
https://de.wiktionary.org/api/rest_v1/page/html/{wort}
```

Der vorhandene Parser ist aber noch zu grob und soll anhand der alten Emacs-Lisp-Logik sauber fertiggestellt werden.

### Relevante Referenzen

Alte Emacs-Lisp-Implementierung:

* [../../emacs-lisp/lisp/woerterbuch-wiktionary.el](../../emacs-lisp/lisp/woerterbuch-wiktionary.el)

Alte Wiktionary-Testfixtures und erwartete Outputs:

* [../../emacs-lisp/tests/files/wiktionary/](../../emacs-lisp/tests/files/wiktionary/)
* [../../emacs-lisp/tests/test-woerterbuch-wiktionary.el](../../emacs-lisp/tests/test-woerterbuch-wiktionary.el)

Bestehendes Rust-Muster für Snapshot-Tests:

* [../../src/sources/openthesaurus.rs](../../src/sources/openthesaurus.rs)
* [../../tests/snapshots/openthesaurus/](../../tests/snapshots/openthesaurus/)

Gemeinsame Datenstruktur:

* [../../src/models.rs](../../src/models.rs)

### Ziel

Der Wiktionary-Parser soll das REST-HTML der deutschen Wiktionary-API parsen und in die vorhandenen Rust-Modelle überführen:

* `SourceResult`
* `DictionaryEntry`
* `Sense`
* `SynonymGroup`

Die normale Wiktionary-Webseite soll nicht mehr live gescraped werden. Live-Lookups müssen über den REST-HTML-Endpunkt laufen. Die menschenlesbare URL im Ergebnis darf weiterhin die normale Wiktionary-Seite sein, also zum Beispiel:

```text
https://de.wiktionary.org/wiki/Bank
```

### Wichtig: REST-HTML statt altes Webseiten-HTML

Die alten Fixtures unter `emacs-lisp/tests/files/wiktionary/` stammen noch aus der alten Emacs-Lisp-Welt und enthalten die normale Wiktionary-Webseite mit Seitenlayout.

Für die neue Rust-Implementierung sollen Snapshot-Tests dagegen mit gespeicherten REST-HTML-Fixtures arbeiten, also mit HTML, das von diesem Endpunkt stammt:

```text
https://de.wiktionary.org/api/rest_v1/page/html/{wort}
```

Bitte lege dafür neue lokale Fixtures passend zur Rust-Teststruktur an, zum Beispiel unter:

```text
tests/fixtures/wiktionary/
```

oder, falls im Projekt ein anderer Fixture-Ort naheliegender ist, konsistent dort.

Die Tests dürfen nicht gegen die Live-API laufen.

### Inhaltliche Parser-Logik

Orientiere dich fachlich an `woerterbuch-wiktionary.el`, aber schreibe den Rust-Code idiomatisch und passend zur vorhandenen Architektur.

Aus dem Elisp-Code sind insbesondere diese Punkte relevant:

* Wiktionary-Artikel enthalten einen deutschen Sprachbereich.
* Innerhalb davon gibt es mehrere Einträge beziehungsweise Homographen.
* Einträge werden über Überschriften wie `Substantiv, f, Bänke`, `Substantiv, f, Banken`, `Verb`, `Adjektiv` usw. erkannt.
* Labeled Blocks wie `Bedeutungen:`, `Beispiele:`, `Synonyme:`, `Sinnverwandte Wörter:`, `Redewendungen:` und `Herkunft:` müssen erkannt werden.
* Bedeutungen und Beispiele sind über Sense-Labels wie `[1]`, `[2]`, `[1, 2]` oder `[1–3]` miteinander verbunden.
* Synonyme sollen pro Bedeutung gruppiert werden.
* Redewendungen sollen als eigene Liste am Eintrag landen.
* Herkunft soll als `etymology` gespeichert werden.
* Fußnoten, Bearbeiten-Links, Referenzmarker und sonstige Wiktionary-Artefakte sollen aus dem Text entfernt werden.

### Homonyme / mehrere Einträge

Der Parser darf nicht nur den ersten gefundenen Wiktionary-Block auslesen.

Beispiel `Bank`:

* `Bank, Substantiv, f, Bänke`
* `Bank, Substantiv, f, Banken`

Diese müssen als zwei getrennte `DictionaryEntry`-Werte im `entries`-Vektor erscheinen.

Die IDs müssen stabil und deterministisch sein:

```text
entry 1
entry 2
entry 3
...
```

### Mapping auf die Rust-Modelle

Bitte nutze die vorhandenen Felder aus `src/models.rs`:

```rust
DictionaryEntry {
    id,
    headword,
    title,
    part_of_speech,
    grammar,
    etymology,
    idioms,
    synonym_groups,
    url,
    senses,
    ..
}
```

Vorgeschlagenes Mapping:

* Wiktionary-Lemma → `headword`
* komplette Wiktionary-Überschrift → `title`
* Wortart aus der Überschrift, zum Beispiel `Substantiv` → `part_of_speech`
* sinnvoller Grammatiktext aus der Überschrift → `grammar`
* `Herkunft` → `etymology`
* `Redewendungen` → `idioms`
* `Synonyme` und `Sinnverwandte Wörter` → `synonym_groups`
* `Bedeutungen` → `senses[].definition`
* Sense-Label `[1]` → `senses[].label`
* `Beispiele` → `senses[].examples`

Wenn eine Sektion fehlt, soll der Parser kein Fehlerergebnis erzeugen, sondern einfach leere Felder verwenden.

Wenn gar kein sinnvoller Eintrag gefunden wird, soll Wiktionary analog zu OpenThesaurus ein sauberes Fehlerergebnis liefern, zum Beispiel:

```text
ok=false
error=No matches found
```

### Snapshot-Tests

Ergänze Snapshot-Tests nach dem Muster von OpenThesaurus in `src/sources/openthesaurus.rs`.

Bitte erstelle für Wiktionary ebenfalls:

```text
tests/snapshots/wiktionary/
```

und teste lokale Fixtures deterministisch gegen gerenderte Snapshots.

Mindestens diese Wörter sollen abgedeckt werden:

* `Bank` — Homonym-Test mit mehreren Einträgen
* `Haus` — umfangreicher Substantiv-Test
* `springen` — Verb-Test
* `Wolke` oder `Zaun` — einfacher Substantiv-Test
* `Nixdaexistiert` — kein Treffer / Fehlerfall, sofern sinnvoll als Fixture darstellbar

Die Snapshot-Ausgabe soll bewusst textuell und gut diffbar sein, ähnlich wie bei OpenThesaurus:

```text
source=Wiktionary
ok=true
url=https://de.wiktionary.org/wiki/Bank
entry 1 headword=Bank title=Bank, Substantiv, f, Bänke part_of_speech=Substantiv grammar=Substantiv, f, Bänke
sense 1 label=1 definition=Sitz- oder Ablagegelegenheit ...
sense 2 label=2 definition=geologische Formation
synonyms 2 items=[Lage]
idioms=[...]
etymology=...
entry 2 headword=Bank title=Bank, Substantiv, f, Banken part_of_speech=Substantiv grammar=Substantiv, f, Banken
sense 1 label=1 definition=Finanzwesen: Geldinstitut ...
synonyms 1 items=[Geldhaus, Geldinstitut, Finanzinstitut, ...]
```

Die genaue Snapshot-Formatierung darf an die Implementierung angepasst werden, soll aber stabil, lesbar und review-freundlich bleiben.

### Zusätzliche gezielte Unit-Tests

Neben den Snapshot-Tests bitte ein paar kleine Unit-Tests für Parser-Hilfsfunktionen ergänzen, insbesondere für:

* Sense-Label parsing: `[1]`, `[1, 2]`, `[1–3]`
* Textbereinigung von Fußnoten und Wiktionary-Artefakten
* Zuordnung von Beispielen zu Bedeutungen
* Synonymgruppen pro Bedeutung
* mehrere Homographen in einem Artikel

Diese Tests können klein und künstlich sein, ähnlich dem bestehenden Elisp-Test `parses list-based blocks without dl/dd wrappers`.

### Umsetzungshinweise

Bitte halte dich an die vorhandene Architektur:

* `lookup(client, query)` bleibt für HTTP zuständig.
* `parse(query, page_url, html)` bleibt unabhängig vom HTTP-Code testbar.
* Keine Live-Requests in Tests.
* Keine Panics bei fehlenden oder unerwarteten HTML-Sektionen.
* Möglichst keine großen Refactorings außerhalb von `src/sources/wiktionary.rs`.
* Nur dann gemeinsame Modelle ändern, wenn es wirklich nötig ist.
* Ausgabe und Tests sollen deterministisch sein: stabile Reihenfolge, deduplizierte Listen, keine zufälligen Daten.

Falls die aktuelle Parser-Strategie in `src/sources/wiktionary.rs` mit `h2, h3, h4` nicht zuverlässig genug für REST-HTML ist, ersetze sie durch eine robustere Struktur, aber halte den Scope auf Wiktionary begrenzt.

### Akzeptanzkriterien

* Wiktionary-Live-Lookups verwenden `https://de.wiktionary.org/api/rest_v1/page/html/{wort}`.
* Die normale Wiktionary-Webseite wird nicht mehr als Live-HTML geparst.
* Der Parser ist unabhängig vom HTTP-Code testbar.
* Mehrere Wiktionary-Einträge pro Artikel werden unterstützt.
* `Bank` ergibt mehrere `DictionaryEntry`-Einträge.
* Bedeutungen werden als `Sense`-Werte extrahiert.
* Beispiele werden den passenden Bedeutungen zugeordnet.
* Synonyme werden als `SynonymGroup`-Werte extrahiert, möglichst mit Sense-Bezug.
* Herkunft wird als `etymology` extrahiert.
* Redewendungen werden als `idioms` extrahiert.
* Fehlende Sektionen führen nicht zu Fehlern oder Panics.
* Es gibt lokale REST-HTML-Fixtures für Wiktionary.
* Es gibt Snapshot-Tests unter `tests/snapshots/wiktionary/`.
* Die Snapshot-Tests laufen offline.
* Die Tests orientieren sich am bestehenden OpenThesaurus-Muster.
* `cargo test wiktionary` läuft erfolgreich.
* Wenn möglich, läuft auch `cargo test` erfolgreich.

### Out of scope

* Kein Lemmatizer.
* Kein Caching.
* Kein Rate Limiting.
* Keine große Überarbeitung der CLI.
* Keine Umstellung auf ein Snapshot-Framework wie `insta`, solange das Projekt bereits einfache `.snap`-Dateien mit `pretty_assertions` verwendet.
* Keine vollständige 1:1-Kompatibilität mit dem alten Emacs-Lisp-Plist-Output; maßgeblich sind die Rust-Modelle in `src/models.rs`.

### Schritt 3: Asynchrones Zusammenspiel & CLI-Interface

Nun führen wir die Konfigurationen und die parallele Ausführung in der Hauptfunktion zusammen.

#### Prompt für die KI

    Nun führen wir alles zusammen. In meinem Elisp-Code gibt es `woerterbuch-sources` (Reihenfolge der Quellen) und `woerterbuch-default-sections` (welche Inhalte standardmässig gewünscht sind).

    Bitte baue die `main`-Funktion so um, dass:
    1. Über `clap` das Suchwort direkt entgegengenommen wird, zusammen mit optionalen Flags (wie `--json` oder `--sources`).
    2. Die aktivierten Quellen (Duden via AMP, Wiktionary via REST-HTML, etc.) vollkommen parallel mit `tokio::join!` abgefragt und geparst werden.
    3. Die Ergebnisse gemäss der gewünschten Sektionen gefiltert, zusammengeführt und entweder schön formatiert im Terminal oder als JSON auf stdout ausgegeben werden.

## DONE Openthesaurus [archive/2026-06-09--openthesaurus-parser.md](archive/2026-06-09--openthesaurus-parser.md)

In dem Ordner sind die Emasc Lisp Dateien für das Scrapen/Parsen [emacs-lisp](../../emacs-lisp). Für OpenThesaurus ist relevant:

- [../../emacs-lisp/lisp/woerterbuch-openthesaurus.el](../../emacs-lisp/lisp/woerterbuch-openthesaurus.el) Logik für Scrapen/Parsen
- Testdateien, inbesondere den erwarteten Output je Section:
  [tests/openthesaurus](../../emacs-lisp/tests/files/openthesaurus/)

Bitte übernehme einfach die Logik, aber mache es so wie in Rust normal. Wenn du in der Logik Schwachstellen siehst, dann verbessere dies.

## DONE Release checkpoint

- Fresh clone test:
  - clone repo into an empty directory
  - follow README installation steps only
  - run `woerterbuch Bank`
  - run `woerterbuch Bank --json`

- Tag first private usable version:
  - `v0.1.0`

- Confirm README, CHANGELOG, CI, and basic Emacs usage are all in sync.
