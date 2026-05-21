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
  (it "parses Haus fixture and normalizes wide angle brackets"
    (let* ((result (test-woerterbuch-duden--single-result
                    "duden-rechtschreibung-haus.html"
                    "Haus"
                    "https://www.duden.de/rechtschreibung/Haus?amp"))
           (entry (car (plist-get result :homographs)))
           (top-definitions (plist-get entry :definitions))
           (def-1 (car top-definitions))
           (def-1b (cadr (plist-get def-1 :definitions)))
           (def-1c (cl-caddr (plist-get def-1 :definitions))))
      (expect (plist-get result :source) :to-equal 'duden)
      (expect (plist-get result :lemma) :to-equal "Haus")
      (expect (length (plist-get result :homographs)) :to-equal 1)
      (expect (plist-get entry :title) :to-equal "Haus, das")
      (expect (plist-get def-1b :definition)
              :to-equal "Gebäude, das zu einem bestimmten Zweck errichtet wurde")
      (expect (seq-some (lambda (example)
                          (string-match-p "⟨in übertragener Bedeutung:⟩" example))
                        (plist-get def-1b :examples))
              :to-be-truthy)
      (expect (seq-some (lambda (example)
                          (string-match-p "⟨in übertragener Bedeutung:⟩" example))
                        (plist-get def-1c :examples))
              :to-be-truthy)))

  (it "parses Zaun fixture into the expected source result object"
    (expect
     (test-woerterbuch-duden--single-result
      "duden-rechtschreibung-zaun.html"
      "Zaun"
      "https://www.duden.de/rechtschreibung/Zaun?amp")
     :to-equal
     (test-woerterbuch-duden--read-expected "duden-expected-zaun.el")))

  (it "parses Bank fixture including shortform definitions"
    (let* ((entry (test-woerterbuch-duden--parse-entry
                   "duden-rechtschreibung-bank-sitzgelegenheit.html"
                   "Bank"
                   "https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp"
                   1))
           (definitions (plist-get entry :definitions))
           (def-2 (cadr definitions))
           (def-3 (cl-caddr definitions))
           (def-2a (car (plist-get def-2 :definitions)))
           (def-3a (car (plist-get def-3 :definitions)))
           (def-4 (nth 3 definitions)))
      (expect (plist-get entry :title) :to-equal "Bank, die")
      (expect (plist-get def-2a :definition)
              :to-equal
              "Kurzform für: verschiedene Handwerkstische wie Drehbank, Hobelbank, Werkbank u. a.")
      (expect (plist-get def-2a :qualifiers) :to-be nil)
      (expect (plist-get def-3a :definition)
              :to-equal "Kurzform für: Sandbank")
      (expect (plist-get def-3a :qualifiers) :to-be nil)
      (expect (seq-some (lambda (example)
                          (string-match-p "⟨in übertragener Bedeutung:⟩" example))
                        (plist-get def-4 :examples))
              :to-be-truthy))))

(describe "Duden Backend No Match:"
  (it "returns the expected no-match result object"
    (expect
     (woerterbuch-duden--no-match-result "Nixdaexistiert")
     :to-equal
     (test-woerterbuch-duden--read-expected
      "duden-expected-nixdaexistiert.el"))))

(provide 'test-woerterbuch-duden)

;;; test-woerterbuch-duden.el ends here
