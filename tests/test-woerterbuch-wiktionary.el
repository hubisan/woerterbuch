;;; test-woerterbuch-wiktionary.el --- Wiktionary backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'test-helper)

(describe "Wiktionary backend"
  (dolist (word test-helper-woerterbuch-output-words)
    (let ((word word))
      (describe word
        (dolist (section test-helper-woerterbuch-output-sections)
          (let ((section section))
            (it (format "matches expected %s output"
                        (symbol-name section))
              (expect
               (test-helper-woerterbuch-fetch-expected-output
                'wiktionary word section)
               :to-equal
               (test-helper-woerterbuch-read-expected
                'wiktionary word section)))))))))

(provide 'test-woerterbuch-wiktionary)

;;; test-woerterbuch-wiktionary.el ends here
