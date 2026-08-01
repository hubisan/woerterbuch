# woerterbuch

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Tests](https://github.com/hubisan/rust-woerterbuch/actions/workflows/ci.yml/badge.svg)](https://github.com/hubisan/woerterbuch/actions/workflows/ci.yml)

`woerterbuch` is a small async Rust CLI for German dictionary lookups. It queries multiple German-language sources and returns either human-readable terminal output or structured JSON for Emacs, scripts, and other tools.

The project is intended to be installed from a Git clone, not published as a public Cargo package.

## Features

- Looks up German words and expressions across several sources.
- Queries selected sources concurrently.
- Provides structured JSON output as the stable integration format.
- Provides human-readable, Markdown, and Org output for quick use and notes.
- Supports filtering by source and by content section.
- Uses defensive HTML parsers, so source-specific website changes remain isolated in the corresponding modules.

## Installation from Git clone

Clone the repository first:

```bash
git clone https://github.com/hubisan/rust-woerterbuch
cd rust-woerterbuch
```

### Build a release binary

To build an optimized binary without installing it, run:

```bash
make build
```

This creates the binary at:

```bash
./target/release/woerterbuch
```

You can run it directly from the repository:

```bash
./target/release/woerterbuch Bank --json
```

### Install the command locally

To install `woerterbuch` into Cargo's local binary directory, run:

```bash
make install
```

This uses `cargo install --path . --locked --force` and makes the command available outside the repository, assuming Cargo's binary directory is in your `PATH`:

```bash
woerterbuch Bank --json
```

To test the installation without overwriting an existing binary, run:

```bash
make install-check
```

## Usage

### Options

`woerterbuch` takes the lookup query as its main argument:

```bash
woerterbuch <QUERY>
````

Example:

```bash
woerterbuch Bank
```

#### Sources

Use `--sources` to select which dictionary sources should be queried.

Default source order:

```text
openthesaurus,dwds,duden,wiktionary
```

Supported sources:

- `openthesaurus`
- `dwds`
- `duden`
- `wiktionary`

Example:

```bash
woerterbuch Bank --sources dwds,duden
```

Network lookups can be slow depending on the selected source. Duden in particular may sometimes respond slowly.

#### Sections

Use `--sections` to select which content sections should be included in the lookup result.

Common sections:

* `definitions`
* `synonyms`
* `examples`
* `origin`
* `idioms`

Examples:

```bash
woerterbuch Bank --sections definitions,synonyms
woerterbuch Bank --sections definitions,examples,origin
```

#### Output

By default, `woerterbuch` prints human-readable terminal output.

Use `--format` to choose an output format:

```bash
woerterbuch Bank --format human
woerterbuch Bank --format json
woerterbuch Bank --format markdown
woerterbuch Bank --format org
```

`--json` is kept as a backwards-compatible shortcut for `--format json`:

```bash
woerterbuch Bank --json
```

Use `--layout` to choose how formatted content is grouped:

```bash
woerterbuch Bank --format markdown --layout by-source
woerterbuch Bank --format markdown --layout by-section
```

`--layout by-source` groups by source first, then by entry and content section. `--layout by-section` groups by content section first, then by source. `--layout` is only supported for `human`, `markdown`, and `org` output; JSON always uses the source-native structure. In text-like output, idioms are rendered as their own final section; sense-level idioms keep a reference such as `1a`. Human-readable, Markdown, and Org output are intended for reading and may change more freely.

Use `--max-examples` to limit how many examples are rendered per definition in text-like output:

```bash
woerterbuch Bank --format markdown --max-examples 2
woerterbuch Bank --format org --layout by-section --max-examples 1
```

JSON ignores `--max-examples` and always returns the full source-native data.

### Examples

Basic lookup:

```bash
woerterbuch Bank
```

JSON output:

```bash
woerterbuch Bank --json
```

Use selected sources only:

```bash
woerterbuch Bank --sources dwds,duden
woerterbuch Bank --sources openthesaurus,wiktionary
```

Use selected sections only:

```bash
woerterbuch Bank --sections definitions,synonyms
woerterbuch Bank --sections definitions,examples,origin
```

Show command-line help:

```bash
woerterbuch --help
```

Run directly from the repository without installing:

```bash
cargo run -- Bank
cargo run -- Bank --json
cargo run -- Bank --format markdown --layout by-section
cargo run -- Bank --format markdown --max-examples 2
cargo run -- Bank --sources dwds,duden
```

## Development

Run the usual checks before committing:

```bash
cargo fmt
cargo test
cargo clippy --all-targets --all-features -- -D warnings
```

Build the release binary:

```bash
cargo build --release
```

Generate local Rust documentation:

```bash
cargo doc --no-deps --open
```

Live HTTP smoke tests are intentionally not part of the default recommendation, because external dictionary websites can be slow, temporarily unavailable, or change their HTML. Parser tests with local fixtures are more reliable for CI.

## Project structure

```text
src/
  main.rs                 CLI, parallel source execution, output handling
  models.rs               JSON-native lookup data structures
  http.rs                 reqwest client, User-Agent setup, HTML helper
  format.rs               human-readable, JSON, Markdown, and Org output
  sources.rs              source routing, timeouts, and section filtering
  sources/
    duden.rs              Duden fetcher and parser
    dwds.rs               DWDS fetcher and parser
    wiktionary.rs         Wiktionary REST HTML fetcher and parser
    openthesaurus.rs      OpenThesaurus fetcher and parser
```

## Emacs-Lisp Wrapper

This was originally an Emacs-Lisp package. I converted it to Rust and added the possibility to use Org-mode as output format.

These Emacs-Lisp wrappers come in handy:

``` emacs-lisp
(cl-defun woerterbuch
    (&optional query
     &key
     (command "woerterbuch")
     (sources '("openthesaurus" "dwds" "duden" "wiktionary"))
     (sections '("definitions" "examples" "synonyms" "origin" "idioms"))
     (layout "by-source")
     max-examples
     (display-function #'pop-to-buffer))
  "Lookup QUERY with woerterbuch asynchronously and display Org output.

When called interactively without prefix argument, ask for QUERY.
When called interactively with prefix argument, use word at point.

When called from Lisp with QUERY non-nil, use QUERY directly.
When called from Lisp with QUERY nil, read QUERY according to
`current-prefix-arg'."
  (interactive)
  (let* ((query
          (or query
              (if current-prefix-arg
                  (let ((word (thing-at-point 'word t)))
                    (unless (and word (not (string-empty-p word)))
                      (user-error "Kein Wort an Punkt gefunden"))
                    word)
                (read-string "Wort/Redewendung: "))))
         (buffer (generate-new-buffer (format "*woerterbuch: %s*" query)))
         (process
          (apply #'start-process
                 "woerterbuch"
                 buffer
                 command
                 (append
                  (list "--format" "org")
                  (when layout
                    (list "--layout" layout))
                  (when sources
                    (list "--sources" (mapconcat #'identity sources ",")))
                  (when sections
                    (list "--sections" (mapconcat #'identity sections ",")))
                  (when max-examples
                    (list "--max-examples" (number-to-string max-examples)))
                  (list query)))))
    (message "woerterbuch: async lookup started for %S" query)
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (when-let* ((buf (process-buffer proc)))
           (with-current-buffer buf
             (goto-char (point-min))
             (org-mode))
           (funcall display-function buf)))))))

(defun woerterbuch-all-by-source ()
  "Lookup all sections by source.
Without prefix argument, ask for a word or phrase.
With prefix argument, use word at point."
  (interactive)
  (woerterbuch
   nil
   :sections '("definitions" "examples" "synonyms" "origin" "idioms")
   :layout "by-source"))

(defun woerterbuch-all-by-section ()
  "Lookup all sections by section.
Without prefix argument, ask for a word or phrase.
With prefix argument, use word at point."
  (interactive)
  (woerterbuch
   nil
   :sections '("definitions" "examples" "synonyms" "origin" "idioms")
   :layout "by-section"))

(defun woerterbuch-synonyms-by-source ()
  "Lookup synonyms by source.
Without prefix argument, ask for a word or phrase.
With prefix argument, use word at point."
  (interactive)
  (woerterbuch
   nil
   :sections '("synonyms")
   :layout "by-source"))

(defun woerterbuch-definitions-by-source ()
  "Lookup definitions including examples by source.
Without prefix argument, ask for a word or phrase.
With prefix argument, use word at point."
  (interactive)
  (woerterbuch
   nil
   :sections '("definitions" "examples")
   :layout "by-source"))

(defun woerterbuch-origin-by-source ()
  "Lookup origin by source.
Without prefix argument, ask for a word or phrase.
With prefix argument, use word at point."
  (interactive)
  (woerterbuch
   nil
   :sections '("origin")
   :layout "by-source"))

(defun woerterbuch-idioms-by-source ()
  "Lookup idioms by source.
Without prefix argument, ask for a word or phrase.
With prefix argument, use word at point."
  (interactive)
  (woerterbuch
   nil
   :sections '("idioms")
   :layout "by-source"))
```

## License

This project is licensed under the GNU General Public License v3.0. See [`LICENSE`](LICENSE) for details.
