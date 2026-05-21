;;; test-woerterbuch-dwds.el --- DWDS backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'test-helper)

(describe "DWDS backend"
  (dolist (word test-helper-woerterbuch-output-words)
    (let ((word word))
      (describe word
        (dolist (section test-helper-woerterbuch-output-sections)
          (let ((section section))
            (it (format "matches expected %s output"
                        (symbol-name section))
              (expect
               (test-helper-woerterbuch-fetch-expected-output
                'dwds word section)
               :to-equal
               (test-helper-woerterbuch-read-expected
                'dwds word section)))))))))

(provide 'test-woerterbuch-dwds)

;;; test-woerterbuch-dwds.el ends here
