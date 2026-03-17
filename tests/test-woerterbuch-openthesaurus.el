;;; test-woerterbuch-openthesaurus.el --- OpenThesaurus backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'woerterbuch-openthesaurus)

(defun test-woerterbuch-openthesaurus--fixture (name)
  "Return absolute OpenThesaurus fixture path for NAME."
  (expand-file-name name
                    (expand-file-name "tests/files/openthesaurus"
                                      default-directory)))

(defun test-woerterbuch-openthesaurus--read-expected (name)
  "Read expected Lisp object from fixture NAME."
  (with-temp-buffer
    (insert-file-contents (test-woerterbuch-openthesaurus--fixture name))
    (read (current-buffer))))

(defun test-woerterbuch-openthesaurus--parse-response-file (name input)
  "Parse local OpenThesaurus response fixture NAME for INPUT."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n\n")
    (insert-file-contents (test-woerterbuch-openthesaurus--fixture name))
    (let ((url-http-response-status 200))
      (woerterbuch-openthesaurus--parse-response input '(:synonyms)))))

(describe "OpenThesaurus Backend:"
  (it "parses Bank fixture into the expected source result object"
    (expect
     (test-woerterbuch-openthesaurus--parse-response-file
      "openthesaurus-api-bank.json"
      "Bank")
     :to-equal
     (test-woerterbuch-openthesaurus--read-expected
      "openthesaurus-expected-bank.el")))

  (it "parses Haus fixture into the expected source result object"
    (expect
     (test-woerterbuch-openthesaurus--parse-response-file
      "openthesaurus-api-haus.json"
      "Haus")
     :to-equal
     (test-woerterbuch-openthesaurus--read-expected
      "openthesaurus-expected-haus.el")))

  (it "parses Zaun fixture into the expected source result object"
    (expect
     (test-woerterbuch-openthesaurus--parse-response-file
      "openthesaurus-api-zaun.json"
      "Zaun")
     :to-equal
     (test-woerterbuch-openthesaurus--read-expected
      "openthesaurus-expected-zaun.el")))

  (it "returns the expected no-match result object"
    (expect
     (test-woerterbuch-openthesaurus--parse-response-file
      "openthesaurus-api-nixdaexistiert.json"
      "Nixdaexistiert")
     :to-equal
     (test-woerterbuch-openthesaurus--read-expected
      "openthesaurus-expected-nixdaexistiert.el"))))

(provide 'test-woerterbuch-openthesaurus)

;;; test-woerterbuch-openthesaurus.el ends here
