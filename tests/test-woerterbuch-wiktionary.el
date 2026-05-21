;;; test-woerterbuch-wiktionary.el --- Wiktionary backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'test-helper)

(describe "Wiktionary backend"
  (dolist (word test-helper-woerterbuch-output-words)
    (dolist (section test-helper-woerterbuch-output-sections)
      (let ((word word)
            (section section))
        (it (format "matches expected %s output for %s"
                    (symbol-name section)
                    word)
          (expect
           (test-helper-woerterbuch-fetch-expected-output
            'wiktionary word section)
           :to-equal
           (test-helper-woerterbuch-read-expected
            'wiktionary word section)))))))

(provide 'test-woerterbuch-wiktionary)

;;; test-woerterbuch-wiktionary.el ends here
