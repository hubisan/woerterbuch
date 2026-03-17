;;; test-woerterbuch-duden.el --- Duden backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'woerterbuch-duden)

(defconst test-woerterbuch-duden--sections
  '(:definitions :examples :synonyms :origin :idioms))

(defun test-woerterbuch-duden--fixture (name)
  "Return absolute Duden fixture path for NAME."
  (expand-file-name name
                    (expand-file-name "tests/files/duden"
                                      default-directory)))

(defun test-woerterbuch-duden--read-expected (name)
  "Read expected Lisp object from fixture NAME."
  (with-temp-buffer
    (insert-file-contents (test-woerterbuch-duden--fixture name))
    (read (current-buffer))))

(defun test-woerterbuch-duden--parse-entry (file input &optional url homograph-id)
  "Parse local Duden FILE for INPUT.

Optional URL and HOMOGRAPH-ID are forwarded to the parser."
  (woerterbuch-duden--parse-html-file
   (test-woerterbuch-duden--fixture file)
   input
   test-woerterbuch-duden--sections
   url
   homograph-id))

(defun test-woerterbuch-duden--single-result (file input url)
  "Return full Duden source result for FILE, INPUT and URL."
  (woerterbuch-duden--result-from-homographs
   input
   (list (test-woerterbuch-duden--parse-entry file input url 1))))

(describe "Duden Backend:"
  (it "parses Haus fixture into the expected source result object"
    (expect
     (test-woerterbuch-duden--single-result
      "duden-rechtschreibung-haus.html"
      "Haus"
      "https://www.duden.de/rechtschreibung/Haus?amp")
     :to-equal
     (test-woerterbuch-duden--read-expected "duden-expected-haus.el")))

  (it "parses Zaun fixture into the expected source result object"
    (expect
     (test-woerterbuch-duden--single-result
      "duden-rechtschreibung-zaun.html"
      "Zaun"
      "https://www.duden.de/rechtschreibung/Zaun?amp")
     :to-equal
     (test-woerterbuch-duden--read-expected "duden-expected-zaun.el")))

  (it "parses Bank fixture into the expected homograph object"
    (expect
     (test-woerterbuch-duden--parse-entry
      "duden-rechtschreibung-bank-sitzgelegenheit.html"
      "Bank"
      "https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp"
      1)
     :to-equal
     (test-woerterbuch-duden--read-expected
      "duden-expected-bank-sitzgelegenheit.el"))))

(describe "Duden Backend No Match:"
  (it "returns the expected no-match result object"
    (expect
     (woerterbuch-duden--no-match-result "Nixdaexistiert")
     :to-equal
     (test-woerterbuch-duden--read-expected
      "duden-expected-nixdaexistiert.el"))))

(provide 'test-woerterbuch-duden)

;;; test-woerterbuch-duden.el ends here
