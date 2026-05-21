;;; test-woerterbuch-duden.el --- Duden backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'test-helper)

(describe "Duden backend"
  (dolist (word test-helper-woerterbuch-output-words)
    (let ((word word))
      (describe word
        (dolist (section test-helper-woerterbuch-output-sections)
          (let ((section section))
            (it (format "matches expected %s output"
                        (symbol-name section))
              (expect
               (test-helper-woerterbuch-fetch-expected-output
                'duden word section)
               :to-equal
               (test-helper-woerterbuch-read-expected
                'duden word section)))))))))

(provide 'test-woerterbuch-duden)

;;; test-woerterbuch-duden.el ends here
