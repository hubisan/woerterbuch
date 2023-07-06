;;; test-woerterbuch.el --- Tests  -*- lexical-binding:t -*-

;; Testing Synonyms.

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'with-simulated-input)

(require 'test-helper)

(require 'woerterbuch)

;;; Helpers

;;; Tests for Auxiliary Functions


(describe "Auxiliary Functions:"
  (it "- Adds an Org heading before the content (woerterbuch--org-add-heading)"
    (expect (woerterbuch--org-add-heading "Test" 1 "content")
            :to-equal "* Test\n\ncontent")
    (expect (woerterbuch--org-add-heading "Test" 2 "content")
            :to-equal "** Test\n\ncontent"))

  (it "- Inserts text into Org-mode buffer (woerterbuch--org-insert)"
    (expect (with-temp-buffer
              (woerterbuch--org-insert "Some text to insert" nil)
              (buffer-string))
            :to-equal "Some text to insert"))

  (it "- Inserts subtree into Org-mode buffer (woerterbuch--org-insert)"
    (expect (with-temp-buffer
              (woerterbuch-mode)
              (insert "* Heading\n\nWith some content")
              (woerterbuch--org-insert "* Test\n\nSome text to insert" t)
              (buffer-string))
            :to-equal (concat "* Heading\n\nWith some content"
                              "\n* Test\n\nSome text to insert\n")))

  (it "- Gets word at point or selection (woerterbuch--get-word-at-point-or-selection)"
    (expect (with-temp-buffer
              (insert "this is a test")
              (woerterbuch--get-word-at-point-or-selection))
            :to-equal (cons "test" (cons 11 15)))
    (expect (with-temp-buffer
              (insert "this is anothertest")
              (goto-char 16)
              (push-mark 9)
              (activate-mark)
              (woerterbuch--get-word-at-point-or-selection))
            :to-equal (cons "another" (cons 9 16)))))

;;; Tests for Definitions

;; TODO Testing with multiple words:
;; - einfach because it has two tabs
;; - Mensch because it has a lot of subdefinitions
;; - Katze because it has those strange links with (1), (b) etc.
;; - test a word without defintions (word that doesn't exists on dwds)



(describe "Definitions:"
  :var* (;; Create a buffer with the content of a dwsds definitions page
         ;; stored as a text-file. It is not a good idea to get the content
         ;; directly as the definitions might have changed or the site is down.
         (dwds-path
          (test-helper-get-file-in-test-dir
           "files/definitions-url-retrieved-for-wurst.txt"))
         (dwds-buffer (find-file-noselect dwds-path))
         ;; No definitions found.
         (dwds-no-definitions-path
          (test-helper-get-file-in-test-dir
           "files/definitions-url-retrieved-no-definitions-found.txt"))
         (dwds-no-definitions-buffer (find-file-noselect
                                      dwds-no-definitions-path))
         ;; Get the expected list from parsing the page.
         (defintions-raw-path (test-helper-get-file-in-test-dir
                             "files/definitions-raw-list.txt"))
         (definitions-raw (read (test-helper-file-read-contents
                                 defintions-raw-path)))
         ;; Get the expected list of synonyms after converting the plist.
         (definitions-list-path (test-helper-get-file-in-test-dir
                              "files/definitions-list.txt"))
         (definitions-list (read
                         (test-helper-file-read-contents definitions-list-path)))
         ;; Synonym list as string.
         (definitions-string-path (test-helper-get-file-in-test-dir
                              "./files/definitions-string.txt"))
         (definitions-string (read
                         (test-helper-file-read-contents definitions-string-path))))
  (before-each
    (spy-on 'woerterbuch--definitions-get-baseform :and-return-value "Wurst"))

  (it "- The page retrieved from the URL can be parsed into a list (woerterbuch--definitions-retrieve-raw)"
    (expect (woerterbuch--definitions-retrieve-raw "Wurst") :to-equal definitions-raw))

  (it "- The raw definitions list can be converted into a list (woerterbuch--definitions-to-list)."
    (expect (woerterbuch--definitions-to-list definitions-raw)
            :to-equal definitions-list))

  ;; woerterbuch--definitions-clean-text > no test needed as it is tested indirectly.

  (it "- Retrieves a cons with car word and cdr the definitions (woerterbuch--definitions-retrieve-as-list)."
    (expect (woerterbuch--definitions-retrieve-as-list "Wurst")
            :to-equal (cons "Wurst" definitions-list)))

  (it "- Converts the definitions to a string (woerterbuch--definitions-convert-to-string)"
    (expect (woerterbuch--definitions-to-string definitions-list)
            :to-equal definitions-string))

  (it "- Retrieves definitions as a string without a heading (woerterbuch--definitions-retrieve-as-string)"
    (expect (cdr-safe (woerterbuch--definitions-retrieve-as-string "Wurst" nil))
            :to-equal (format "%s\n" definitions-string)))

  (it "- If no definitions are found returns a string holding an error message - in this case without a heading (woerterbuch--definitions-retrieve-as-string)"
    (spy-on 'url-retrieve-synchronously :and-return-value dwds-no-definitions-buffer)
    (spy-on 'woerterbuch--definitions-get-baseform :and-return-value "Existiertnicht")
    (expect (cdr-safe (woerterbuch--definitions-retrieve-as-string "Existiertnicht" nil))
            :to-equal (format woerterbuch-definitions-no-matches-text-format
                              "Existiertnicht")))

  (it "- Retrieves definitions as a string with a heading (woerterbuch--definitions-retrieve-as-string)"
    (expect (cdr-safe (woerterbuch--definitions-retrieve-as-string "Wurst" t))
            :to-equal (format woerterbuch-insert-org-heading-format "*"
                              (format woerterbuch-definitions-heading-text-format "Wurst")
                (concat definitions-string "\n"))))

  (it "- Reads a definition from minibuffer (woerterbuch--definitions-read-definition)"
    (expect (with-simulated-input
                "völlig RET"
              (woerterbuch--definitions-read-definition "Wurst"))
            :to-equal "völlig unwichtig, gleichgültig"))

  (it "- Shows the definitions in a buffer in `woerterbuch-mode' (woerterbuch-definitions-show-in-org-buffer)"
    (let* ((buffer (woerterbuch-definitions-show-in-org-buffer "Wurst")))
      (expect (buffer-live-p (get-buffer woerterbuch--org-buffer-name))
              :to-be-truthy)
      (with-current-buffer buffer
        (expect major-mode :to-equal 'woerterbuch-mode)
        (expect (buffer-string) :to-equal
                (format woerterbuch-insert-org-heading-format "*"
                        (format woerterbuch-definitions-heading-text-format"Wurst")
                (concat definitions-string "\n"))))
      (kill-buffer buffer)))

  (it "- Shows the definitions in a buffer for word at point (woerterbuch-definitions-show-in-org-buffer-for-word-at-point)"
      (with-temp-buffer
        (insert "Wurst")
        (let* ((buffer (woerterbuch-definitions-show-in-org-buffer-for-word-at-point)))
          (with-current-buffer buffer
            (expect (buffer-string) :to-equal
                    (format woerterbuch-insert-org-heading-format "*"
                            (format woerterbuch-definitions-heading-text-format "Wurst")
                            (concat definitions-string "\n"))))
          (kill-buffer buffer))))

  (it "- Inserts the definitions into an Org buffer without a heading (woerterbuch-definitions-insert-into-org-buffer)"
    (expect
     (with-temp-buffer
       (org-mode)
       (woerterbuch-definitions-insert-into-org-buffer "Wurst")
       (buffer-string))
     :to-equal (format "%s\n" definitions-string)))

  (it "- Inserts the definitions into an Org buffer with a heading (woerterbuch-definitions-insert-into-org-buffer)"
    (expect
     (with-temp-buffer
       (org-mode)
       (woerterbuch-definitions-insert-into-org-buffer "Wurst" t)
       (buffer-string))
     :to-equal (format woerterbuch-insert-org-heading-format "*"
                       (format woerterbuch-definitions-heading-text-format "Wurst")
                (concat definitions-string "\n"))))

  ;; woerterbuch-definitions-kill-as-org-mode-syntax > No test needed.

  (it "- Reads and insert a definition into current buffer (woerterbuch-definitions-insert)"
    (expect
     (with-temp-buffer
       (with-simulated-input
           "völlig RET"
         (woerterbuch-definitions-insert "Wurst"))
       (buffer-string))
     :to-equal  "völlig unwichtig, gleichgültig")))

(describe "Definitinons (baseform):"
  ;; Moved this to another describe as I was not sure how to reset the spy just
  ;; for this part. This didn't work inside the it statement:
  ;; (spy-on 'woerterbuch--definitions-get-baseform :and-call-through)
  (it "- It is able to get a baseform (lemma) of a word (woerterbuch--definitions-get-baseform)"
    (spy-on 'woerterbuch--definitions-get-baseform :and-call-through)
    (expect
     (woerterbuch--definitions-get-baseform "Häuser") :to-equal "Haus")
    (expect (woerterbuch--definitions-get-baseform "Patienten") :to-equal "Patient")
    ;; Return word if already the base form.
    (expect (woerterbuch--definitions-get-baseform "Haus") :to-equal "Haus")
    ;; This is actually a baseform, good to test this.
    (expect (woerterbuch--definitions-get-baseform "Katzen") :to-equal "Katzen")))

;;; Tests for Synonyms

(describe "Synonyms:"
  :var* (;; Create a buffer with the content from a API call to openthesaurus
         ;; stored as a text-file. It is not a good idea to call the API
         ;; directly as the synonyms might have changed.
         (openthesaurus-path
          (test-helper-get-file-in-test-dir
           "files/synonyms-url-retrieved-openthesaurus-api.txt"))
         (openthesaurus-buffer (find-file-noselect openthesaurus-path))
         ;; No synonyms found.
         (openthesaurus-no-synonyms-path
          (test-helper-get-file-in-test-dir
           "files/synonyms-url-retrieved-openthesaurus-api-no-synonyms-found.txt"))
         (openthesaurus-no-synonyms-buffer (find-file-noselect
                                            openthesaurus-no-synonyms-path))
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
    ;; Disable `kill-buffer' to not make it kill the openthesaurus buffer .
    (spy-on 'kill-buffer))

  (it "- The JSON retrieved from the URL can be parsed into a plist (woerterbuch--synonyms-retrieve-raw)"
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
    (expect (woerterbuch--synonyms-to-string synonyms-list)
            :to-equal synonyms-string))

  (it "- Retrieves synonyms as a string without a heading (woerterbuch--synonyms-retrieve-as-string)"
    (expect (cdr-safe (woerterbuch--synonyms-retrieve-as-string "Test" nil))
            :to-equal (format "%s\n" synonyms-string)))

  (it "- If no synonyms are found returns a string holding an error message - in this case without a heading (woerterbuch--synonyms-retrieve-as-string)"
    (spy-on 'url-retrieve-synchronously :and-return-value openthesaurus-no-synonyms-buffer)
    (expect (cdr-safe (woerterbuch--synonyms-retrieve-as-string "Existiertnicht" nil))
            :to-equal (format woerterbuch-synonyms-no-matches-text-format
                              "Existiertnicht")))

  (it "- Retrieves synonyms as a string with a heading (woerterbuch--synonyms-retrieve-as-string)"
    (expect (cdr-safe (woerterbuch--synonyms-retrieve-as-string "Test" t))
            :to-equal (format woerterbuch-insert-org-heading-format "*"
                              (format woerterbuch-synonyms-heading-text-format "Test")
                (concat synonyms-string "\n"))))

  (it "- Reads a synonym from minibuffer (woerterbuch--synonyms-read-synonym)"
    (expect (with-simulated-input
                "Klausur RET"
              (woerterbuch--synonyms-read-synonym "Test"))
            :to-equal "Klausur"))

  (it "- Shows the synonyms in a buffer in `woerterbuch-mode' (woerterbuch-synonyms-show-in-org-buffer)"
    (let* ((buffer (woerterbuch-synonyms-show-in-org-buffer "Test")))
      (expect (buffer-live-p (get-buffer woerterbuch--org-buffer-name))
              :to-be-truthy)
      (with-current-buffer buffer
        (expect major-mode :to-equal 'woerterbuch-mode)
        (expect (buffer-string) :to-equal
                (format woerterbuch-insert-org-heading-format "*"
                        (format woerterbuch-synonyms-heading-text-format"Test")
                (concat synonyms-string "\n"))))
      (kill-buffer buffer)))

  (it "- Shows the synonyms in a buffer for word at point (woerterbuch-synonyms-show-in-org-buffer-for-word-at-point)"
      (with-temp-buffer
        (insert "Test")
        (let* ((buffer (woerterbuch-synonyms-show-in-org-buffer-for-word-at-point)))
          (with-current-buffer buffer
            (expect (buffer-string) :to-equal
                    (format woerterbuch-insert-org-heading-format "*"
                            (format woerterbuch-synonyms-heading-text-format "Test")
                            (concat synonyms-string "\n"))))
          (kill-buffer buffer))))

  (it "- Inserts the synonyms into an Org buffer without a heading (woerterbuch-synonyms-insert-into-org-buffer)"
    (expect
     (with-temp-buffer
       (org-mode)
       (woerterbuch-synonyms-insert-into-org-buffer "Test")
       (buffer-string))
     :to-equal (format "%s\n" synonyms-string)))

  (it "- Inserts the synonyms into an Org buffer with a heading (woerterbuch-synonyms-insert-into-org-buffer)"
    (expect
     (with-temp-buffer
       (org-mode)
       (woerterbuch-synonyms-insert-into-org-buffer "Test" t)
       (buffer-string))
     :to-equal (format woerterbuch-insert-org-heading-format "*"
                       (format woerterbuch-synonyms-heading-text-format "Test")
                (concat synonyms-string "\n"))))

  ;; woerterbuch-synonyms-kill-as-org-mode-syntax > No test needed.

  (it "- Reads and insert a synonym into current buffer (woerterbuch-synonyms-insert)"
    (expect
     (with-temp-buffer
       (with-simulated-input
           "Klausur RET"
         (woerterbuch-synonyms-insert "Test"))
       (buffer-string))
     :to-equal "Klausur"))

  (it "- Looks up synonyms for word at point (woerterbuch-synonyms-lookup-word-at-point)"
    (expect
     (with-temp-buffer
       (insert "Test")
       (with-simulated-input
           "Klausur RET"
         (woerterbuch-synonyms-lookup-word-at-point)))
     :to-equal "Klausur"))

  (it "- Replaces word at point with a synonym (woerterbuch-synonyms-replace-word-at-point)"
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
