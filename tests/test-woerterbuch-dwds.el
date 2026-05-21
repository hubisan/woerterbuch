;;; test-woerterbuch-dwds.el --- DWDS backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'woerterbuch-dwds)

(defconst test-woerterbuch-dwds--sections
  '(:definitions :examples :origin :idioms))

(defun test-woerterbuch-dwds--fixture (name)
  "Return absolute DWDS fixture path for NAME."
  (expand-file-name name
                    (expand-file-name "tests/files/dwds"
                                      default-directory)))

(defun test-woerterbuch-dwds--read-expected (name)
  "Read expected Lisp object from fixture NAME."
  (with-temp-buffer
    (insert-file-contents (test-woerterbuch-dwds--fixture name))
    (read (current-buffer))))

(defun test-woerterbuch-dwds--parse-result (file input)
  "Parse local DWDS FILE for INPUT into a full source result."
  (let* ((dom (with-temp-buffer
                (insert-file-contents (test-woerterbuch-dwds--fixture file))
                (libxml-parse-html-region (point-min) (point-max))))
         (entry (woerterbuch-dwds--parse-dom
                 dom input test-woerterbuch-dwds--sections))
         (result (woerterbuch-core-make-result 'dwds input)))
    (setq result (plist-put result :lemma (or (plist-get entry :lemma) input)))
    (setq result (plist-put result :url (plist-get entry :url)))
    (setq result (plist-put result :homographs (plist-get entry :homographs)))
    result))

(describe "DWDS Backend:"
  (it "parses Bank fixture into the expected source result object"
    (expect
     (test-woerterbuch-dwds--parse-result "dwds-wb-bank.html" "Bank")
     :to-equal
     (test-woerterbuch-dwds--read-expected "dwds-expected-bank.el")))

  (it "parses Haus fixture into the expected source result object"
    (expect
     (test-woerterbuch-dwds--parse-result "dwds-wb-haus.html" "Haus")
     :to-equal
     (test-woerterbuch-dwds--read-expected "dwds-expected-haus.el")))

  (it "parses Zaun fixture into the expected source result object"
    (expect
     (test-woerterbuch-dwds--parse-result "dwds-wb-zaun.html" "Zaun")
     :to-equal
     (test-woerterbuch-dwds--read-expected "dwds-expected-zaun.el")))

  (it "treats (error http 404) as HTTP error instead of network error"
    (let ((captured nil))
      (with-temp-buffer
        (insert "HTTP/1.1 404 Not Found\n\n")
        (let ((url-http-response-status nil))
          (woerterbuch-dwds--fetch-callback
           '(:error (error http 404))
           "Bank"
           test-woerterbuch-dwds--sections
           (lambda (result)
             (setq captured result)))))
      (expect captured
              :to-equal
              '(:source dwds :lemma "Bank" :ok nil
                        :homographs nil
                        :error "HTTP error: 404"))))

  (it "treats a DWDS search page without article as no match"
    (let ((captured nil))
      (with-temp-buffer
        (insert "HTTP/1.1 200 OK\n\n")
        (insert-file-contents
         (test-woerterbuch-dwds--fixture "dwds-wb-404.html"))
        (let ((url-http-response-status 200))
          (woerterbuch-dwds--fetch-callback
           nil
           "Nixtdaexistiert"
           test-woerterbuch-dwds--sections
           (lambda (result)
             (setq captured result)))))
      (expect captured
              :to-equal
              '(:source dwds :lemma "Nixtdaexistiert" :ok nil
                        :homographs nil
                        :error "No matches found")))))

(provide 'test-woerterbuch-dwds)

;;; test-woerterbuch-dwds.el ends here
