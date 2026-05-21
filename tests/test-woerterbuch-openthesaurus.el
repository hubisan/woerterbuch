;;; test-woerterbuch-openthesaurus.el --- OpenThesaurus backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'test-helper)

(describe "OpenThesaurus backend"
  (dolist (word test-helper-woerterbuch-output-words)
    (dolist (section test-helper-woerterbuch-output-sections)
      (let ((word word)
            (section section))
        (it (format "matches expected %s output for %s"
                    (symbol-name section)
                    word)
          (expect
           (test-helper-woerterbuch-fetch-expected-output
            'openthesaurus word section)
           :to-equal
           (test-helper-woerterbuch-read-expected
            'openthesaurus word section)))))))

(provide 'test-woerterbuch-openthesaurus)

;;; test-woerterbuch-openthesaurus.el ends here
