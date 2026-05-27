;;; test-woerterbuch.el --- Tests  -*- lexical-binding:t; no-byte-compile: t -*-

;; Testing Synonyms.

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'with-simulated-input)

(require 'test-helper)

(require 'woerterbuch)

;;; Configuration

(describe "Lemma normalization"
  (it "keeps a base form when the DWDS snippet endpoint returns one"
    (let (result)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback cbargs &rest _args)
                   (let ((buffer (generate-new-buffer " *woerterbuch-lemma*")))
                     (with-current-buffer buffer
                       (insert "HTTP/1.1 200 OK\r\n\r\n")
                       (setq-local url-http-response-status 200)
                       (setq-local url-http-end-of-headers (point))
                       (insert
                        "[{\"wortart\":\"partizipiales Adjektiv\",\"url\":\"https://www.dwds.de/wb/verliebt\",\"lemma\":\"verliebt\",\"input\":\"verliebt\"}]")
                       (goto-char (point-min))
                       (apply callback (append (list nil) cbargs)))
                     buffer))))
        (woerterbuch-core-normalize-lemma
         "verliebt"
         (lambda (value)
           (setq result value))))
      (expect result
              :to-equal
              '(:ok t :input "verliebt" :lemma "verliebt" :source dwds))))

  (it "falls back to the DWDS frequency endpoint when snippet has no direct match"
    (let (result requests)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback cbargs &rest _args)
                   (push url requests)
                   (let ((buffer (generate-new-buffer " *woerterbuch-lemma*")))
                     (with-current-buffer buffer
                       (insert "HTTP/1.1 200 OK\r\n\r\n")
                       (setq-local url-http-response-status 200)
                       (setq-local url-http-end-of-headers (point))
                       (insert
                        (if (string-match-p "/api/wb/snippet/" url)
                            "[]"
                          "{\"lemma\":\"springen\"}"))
                       (goto-char (point-min))
                       (apply callback (append (list nil) cbargs)))
                     buffer))))
        (woerterbuch-core-normalize-lemma
         "springt"
         (lambda (value)
           (setq result value))))
      (expect (length requests) :to-equal 2)
      (expect result
              :to-equal
              '(:ok t :input "springt" :lemma "springen" :source dwds))))

  (it "skips lemma normalization for multi-word expressions"
    (let (result called)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _args)
                   (setq called t)
                   (error "Should not request lemma lookup for multi-word input"))))
        (woerterbuch-core-normalize-lemma
         "die Katze aus dem Sack lassen"
         (lambda (value)
           (setq result value))))
      (expect called :to-be nil)
      (expect result
              :to-equal
              '(:ok t :input "die Katze aus dem Sack lassen"
                      :lemma "die Katze aus dem Sack lassen"
                      :source dwds))))

  (it "builds source URLs for multi-word expressions correctly"
    (expect (woerterbuch-duden--build-url "sich spiegeln")
            :to-equal
            "https://www.duden.de/rechtschreibung/sich_spiegeln?amp")
    (expect (woerterbuch-wiktionary--build-web-url
             "die Katze aus dem Sack lassen")
            :to-equal
            "https://de.wiktionary.org/wiki/die_Katze_aus_dem_Sack_lassen")))

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

(defun test-helper-woerterbuch-generate-expected-output ()
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
  (test-helper-woerterbuch-generate-expected-output))

(provide 'test-woerterbuch)

;;; test-woerterbuch.el ends here
