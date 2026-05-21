;;; test-woerterbuch.el --- Tests  -*- lexical-binding:t; no-byte-compile: t -*-

;; Testing Synonyms.

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'with-simulated-input)

(require 'test-helper)

(require 'woerterbuch)

;;; Configuration

;;; Get exptected Output and fetch HTML/JSON

;; Both will be stored. And tests will compare the output to the expected,
;; stored one. And the Html and JSON will be used to mockup the data.

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
