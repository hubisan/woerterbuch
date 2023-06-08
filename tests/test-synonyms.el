;;; test-synoyms.el --- Tests  -*- lexical-binding:t -*-

;; Testing Synonyms.

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'with-simulated-input)

(require 'test-helper)

(require 'woerterbuch)

;;; Helpers

;;; Tests

(describe "Synonyms:"
  :var* (;; Create a buffer with the content from a API call to openthesaurus
         ;; stored as a text-file. It is not a good idea to call the API
         ;; directly as the synonyms might have changed.
         (openthesaurus-path
          (test-helper-get-file-in-test-dir
           "files/synonyms-url-retrieved-openthesaurus-api.txt"))
         (openthesaurus-buffer (find-file-noselect openthesaurus-path))
         ;; Get the expected plist from parsing the API data.
         (synonyms-raw-path (test-helper-get-file-in-test-dir
                             "files/synonyms-raw-plist.txt"))
         (synonyms-raw (read
                        (test-helper-file-read-contents synonyms-raw-path)))
         ;; Get raw synonyms when there is a baseform.
         (synonyms-raw-baseform-path (test-helper-get-file-in-test-dir
                                      "files/synonyms-raw-plist-baseform.txt"))
         (synonyms-raw-baseform (read
                                 (test-helper-file-read-contents
                                  synonyms-raw-baseform-path)))
         ;; Get the expected list of synonyms after converting the plist.
         (synonyms-list-path (test-helper-get-file-in-test-dir
                              "files/synonyms-list.txt"))
         (synonyms-list (read
                         (test-helper-file-read-contents synonyms-list-path)))
         ;; Synonym list as string.
         (synonyms-string-path (test-helper-get-file-in-test-dir
                              "./files/synonyms-string.txt"))
         (synonyms-string (read
                         (test-helper-file-read-contents synonyms-string-path))))

  (before-each
    (spy-on 'url-retrieve-synchronously :and-return-value openthesaurus-buffer)
    (spy-on 'kill-buffer))

  (it "- The JSON retrieved from the URL can be parsed into a plist (woerterbuch--synonyms-retrieve-raw)."
    (expect (woerterbuch--synonyms-retrieve-raw "Test") :to-equal synonyms-raw))

  (it "- It extracts the baseform from the plist if there is one (woerterbuch--synonyms-baseform)."
    (expect (woerterbuch--synonyms-baseform synonyms-raw-baseform) :to-equal "Test"))

  (it "- The raw synonyms plist can be converted into a list (woerterbuch--synonyms-to-list)."
    (expect (woerterbuch--synonyms-to-list synonyms-raw)
            :to-equal synonyms-list))

  (it "- Retrieves a cons with car word and cdr the synonyms (woerterbuch--synonyms-retrieve-as-list)."
    (expect (woerterbuch--synonyms-retrieve-as-list "Test")
            :to-equal (cons "Test" synonyms-list)))

  (it "- If a baseform is returned call the function again to get the synonyms (woerterbuch--synonyms-retrieve-as-list)."
    (spy-on 'woerterbuch--synonyms-retrieve-raw :and-call-fake
            (lambda (word)
              (if (string-equal word "Tests")
                  synonyms-raw-baseform
                synonyms-raw)))
    (expect (woerterbuch--synonyms-retrieve-as-list "Tests")
            :to-equal (cons "Test" synonyms-list)))

  (it "- Converts the synonyms to a string (woerterbuch--synonyms-convert-to-string)"
    (expect (woerterbuch--synonyms-convert-to-string synonyms-list)
            :to-equal synonyms-string))

  (it "- Retrieves synonyms as a string without a heading (woerterbuch--synonyms-retrieve-as-string)"
    (expect (woerterbuch--synonyms-retrieve-as-string "Test" nil)
            :to-equal (format "%s\n" synonyms-string)))

  (it "- Retrieves synonyms as a string with a heading (woerterbuch--synonyms-retrieve-as-string)"
    (expect (woerterbuch--synonyms-retrieve-as-string "Test" t)
            :to-equal (format "* Test\n\n%s\n" synonyms-string)))

  (it "- Reads a synonym from minibuffer."
    (expect (with-simulated-input
                "Klausur RET"
              (woerterbuch--read-synonym "Test"))
            :to-equal "Klausur"))

  ;; TODO woerterbuch-synonyms-show-in-org-buffer

  ;; TODO woerterbuch-synonyms-insert-into-org-buffer

  ;; TODO woerterbuch-synonyms-kill-as-org-mode-syntax

  (it "- Reads and insert a synonym into current buffer."
    (expect
     (with-temp-buffer
       (with-simulated-input
           "Klausur RET"
         (woerterbuch-synonyms-insert "Test"))
       (buffer-string))
     :to-equal "Klausur"))

  (it "- Looks up synonyms for word at point."
    (expect
     (with-temp-buffer
       (insert "Test")
       (with-simulated-input
           "Klausur RET"
         (woerterbuch-synonyms-lookup-word-at-point)))
     :to-equal "Klausur"))

  (it "- Replaces word at point with a synonym."
    (expect
     (with-temp-buffer
       (insert "Test")
       (with-simulated-input
           "Klausur RET"
         (woerterbuch-synonyms-replace-word-at-point))
       (buffer-string))
     :to-equal "Klausur"))
  )

(provide 'test-synoyms)

;;; test-synoyms.el ends here
