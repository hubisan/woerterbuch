;;; test-woerterbuch.el --- Tests  -*- lexical-binding:t -*-

;; Testing Synonyms.

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'with-simulated-input)

(require 'test-helper)

(require 'woerterbuch)

;;; Helpers

;;; Tests

(describe "Auxiliary Functions:"
  (it "- Adds an Org heading before the content."
    (expect (woerterbuch--org-add-heading "Test" 1 "content")
            :to-equal "* Test\n\ncontent")
    (expect (woerterbuch--org-add-heading "Test" 2 "content")
            :to-equal "** Test\n\ncontent"))

  (it "- Inserts text into Org-mode buffer."
    (expect (with-temp-buffer
              (woerterbuch--org-insert "Some text to insert" nil)
              (buffer-string))
            :to-equal "Some text to insert"))

  (it "- Inserts subtree into Org-mode buffer."
    (expect (with-temp-buffer
              (woerterbuch-mode)
              (insert "* Heading\n\nWith some content")
              (woerterbuch--org-insert "* Test\n\nSome text to insert" t)
              (buffer-string))
            :to-equal (concat "* Heading\n\nWith some content"
                              "\n* Test\n\nSome text to insert\n")))

  (it "- Gets word at point or selection."
    (expect (with-temp-buffer
              (insert "this is a test")
              (woerterbuch--get-word-at-point-or-selection))
            :to-equal (cons "test" (cons 11 15)))
    ;; FIXME This test works but not when running with Eldev. The region is not
    ;; active it seems.
    ;; (expect (with-temp-buffer
    ;;           (insert "this is anothertest")
    ;;           (goto-char 16)
    ;;           (set-mark 9)
    ;;           (woerterbuch--get-word-at-point-or-selection))
    ;;         :to-equal (cons "another" (cons 9 16)))
    ))

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
    ;; Use the prefetched version from a call to the API.
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

  (it "- Shows the synonyms in an a buffer in `woerterbuch-mode.'"
    (let* ((buffer (woerterbuch-synonyms-show-in-org-buffer "Test")))
      (expect (buffer-live-p (get-buffer woerterbuch--org-buffer-name))
              :to-be-truthy)
      (with-current-buffer buffer
        (expect major-mode :to-equal 'woerterbuch-mode)
        (expect (buffer-string) :to-equal
                (format "* Test\n\n%s\n" synonyms-string)))
      (kill-buffer buffer)))

  (it "- Inserts the synonyms into an Org buffer without a heading."
    (expect
     (with-temp-buffer
       (org-mode)
       (woerterbuch-synonyms-insert-into-org-buffer "Test")
       (buffer-string))
     :to-equal (format "%s\n" synonyms-string)))

  (it "- Inserts the synonyms into an Org buffer with a heading."
    (expect
     (with-temp-buffer
       (org-mode)
       (woerterbuch-synonyms-insert-into-org-buffer "Test" t)
       (buffer-string))
     :to-equal (format "* Test\n\n%s\n" synonyms-string)))

  ;; woerterbuch-synonyms-kill-as-org-mode-syntax > No test needed.

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
     :to-equal "Klausur")))

(provide 'test-woerterbuch)

;;; test-woerterbuch.el ends here
