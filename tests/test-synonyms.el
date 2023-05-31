;;; test-synoyms.el --- Tests  -*- lexical-binding:t -*-

;; Testing W

;;; Requirements

(require 'buttercup)
(require 'ert)
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
                         (test-helper-file-read-contents synonyms-list-path))))

  (it "The JSON retrieved from the URL can be parsed into a plist (woerterbuch--synonyms-retrieve-raw)."
    (spy-on 'url-retrieve-synchronously :and-return-value openthesaurus-buffer)
    (spy-on 'kill-buffer)
    (expect (woerterbuch--synonyms-retrieve-raw "Test") :to-equal synonyms-raw))

  (it "It extracts the baseform from the plist if there is one (woerterbuch--synonyms-baseform)."
    (expect (woerterbuch--synonyms-baseform synonyms-raw-baseform) :to-equal "Test"))

  (it "The raw synonyms plist can be converted into a list (woerterbuch--synonyms-to-list)."
    (expect (woerterbuch--synonyms-to-list synonyms-raw)
            :to-equal synonyms-list))

  (it "Retrieves a cons with car word and cdr the synonyms (woerterbuch--synonyms-retrieve-as-list)."
    (spy-on 'url-retrieve-synchronously :and-return-value openthesaurus-buffer)
    (spy-on 'kill-buffer)
    (expect (woerterbuch--synonyms-retrieve-as-list "Test")
            :to-equal (cons "Test" synonyms-list)))

  (xit "If there is a baseform correctly "
    (spy-on 'url-retrieve-synchronously :and-return-value openthesaurus-buffer)
    (spy-on 'kill-buffer)
    (expect (woerterbuch--synonyms-retrieve-as-list "Test")
            :to-equal (cons "Test" synonyms-list)))
  )

(provide 'test-synoyms)

;;; test-synoyms.el ends here
