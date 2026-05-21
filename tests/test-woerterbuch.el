;;; test-woerterbuch.el --- Tests  -*- lexical-binding:t; no-byte-compile: t -*-

;; Testing Synonyms.

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'with-simulated-input)

(require 'test-helper)

(require 'woerterbuch)

;;; Configuration

(defconst test-helper-woerterbuch-output-words
  '("Bank" "Haus" "springen" "verlieben" "Wolke" "Zaun" "Nixdaexistiert")
  "Words used for generated test output.")

(defconst test-helper-woerterbuch-output-sources
  '(openthesaurus dwds duden wiktionary)
  "Sources used for generated test output.")

(defconst test-helper-woerterbuch-output-sections
  '(:definitions :examples :origin :synonyms :idioms)
  "Sections used for generated expected output.")

(defconst test-helper-woerterbuch-duden-extra-urls
  '(("Bank"
     ("https://www.duden.de/suchen/dudenonline/Bank"
      "duden-Bank-search.html")
     ("https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp"
      "duden-Bank-1.html")
     ("https://www.duden.de/rechtschreibung/Bank_Geldinstitut?amp"
      "duden-Bank-2.html")))
  "Fixed Duden URLs for homograph test words.")

;;; Get exptected Output and fetch HTML/JSON

;; Both will be stored. And tests will compare the output to the expected,
;; stored one. And the Html and JSON will be used to mockup the data.

(defun test-helper-woerterbuch--source-output-files (source word)
  "Return `(URL FILE-NAME)' pairs for SOURCE and WORD.
If there are multiple homographs for a word Duden has one page for each
homograph."
  (pcase source
    ('openthesaurus
     (list (list (woerterbuch-openthesaurus--build-url word)
                 (format "openthesaurus-%s.json" word))))
    ('dwds
     (list (list (woerterbuch-dwds--build-url word)
                 (format "dwds-%s.html" word))))
    ('wiktionary
     (list (list (woerterbuch-wiktionary--build-web-url word)
                 (format "wiktionary-%s.html" word))))
    ('duden
     (or (cdr (assoc word test-helper-woerterbuch-duden-extra-urls))
         (list (list (woerterbuch-duden--build-url word)
                     (format "duden-%s.html" word)))))
    (_
     (error "Unknown source: %S" source))))

(defun test-helper-woerterbuch--source-headers (source)
  "Return request headers for SOURCE."
  (pcase source
    ('dwds woerterbuch-dwds-request-headers)
    ('duden woerterbuch-duden-request-headers)
    ('wiktionary woerterbuch-wiktionary-request-headers)
    ('openthesaurus '(("User-Agent" . "woerterbuch/0.1")))
    (_ nil)))

(defun test-helper-woerterbuch-fetch-source-output ()
  "Fetch raw HTML/JSON source output for the fixed test words."
  (interactive)
  (let ((files-dir (test-helper-woerterbuch--files-dir)))
    (dolist (source test-helper-woerterbuch-output-sources)
      (dolist (word test-helper-woerterbuch-output-words)
        (dolist (entry (test-helper-woerterbuch--source-output-files source word))
          (pcase-let ((`(,url ,file-name) entry))
            (test-helper-woerterbuch--download-url
             url
             (expand-file-name
              (format "%s/%s/%s"
                      (symbol-name source)
                      word
                      file-name)
              files-dir)
             (test-helper-woerterbuch--source-headers source))))))))

(defun test-helper-woerterbuch-fetch-expected-output ()
  "Fetch expected Elisp output for the fixed test words."
  (interactive)
  (let ((files-dir (test-helper-woerterbuch--files-dir)))
    (dolist (source test-helper-woerterbuch-output-sources)
      (let ((source-name (symbol-name source)))
        (dolist (word test-helper-woerterbuch-output-words)
          (dolist (section test-helper-woerterbuch-output-sections)
            (let* ((section-name
                    (string-remove-prefix ":" (symbol-name section)))
                   (word-dir
                    (expand-file-name
                     (format "%s/%s/" source-name word)
                     files-dir))
                   (file
                    (expand-file-name
                     (format "%s-%s-%s-expected.el"
                             source-name word section-name)
                     word-dir))
                   (woerterbuch-sources (list source))
                   (result
                    (woerterbuch-fetch-all-sync
                     word
                     :sections (list section)
                     :timeout 10)))
              (make-directory word-dir t)
              (with-temp-file file
                (pp result (current-buffer))
                (terpri)))))))))

(defun test-helper-woerterbuch-update-output ()
  "Fetch raw source output and expected Elisp output for tests."
  (interactive)
  (test-helper-woerterbuch-fetch-source-output)
  (test-helper-woerterbuch-fetch-expected-output))

(provide 'test-woerterbuch)

;;; test-woerterbuch.el ends here
