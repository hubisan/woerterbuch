;;; test-helper.el --- Helper functions  -*- lexical-binding: t; no-byte-compile: t -*-

;; Helper functions for all tests.

;;; Requirements

(require 'cl-lib)

(require 'woerterbuch)
(require 'woerterbuch-duden)
(require 'woerterbuch-dwds)
(require 'woerterbuch-openthesaurus)
(require 'woerterbuch-wiktionary)

;;; Variables

(defconst test-helper-woerterbuch-output-words
  '("Bank" "Haus" "springen" "verlieben" "Wolke" "Zaun" "Nixdaexistiert")
  "Words used for generated test output.")

(defconst test-helper-woerterbuch-output-sources
  '(openthesaurus dwds duden wiktionary)
  "Sources used for generated output.")

(defconst test-helper-woerterbuch-output-sections
  '(:definitions :examples :origin :synonyms :idioms)
  "Sections used for generated expected output.")

(defconst test-helper-woerterbuch-duden-extra-urls
  '(("Bank"
     ("https://www.duden.de/suchen/dudenonline/Bank"
      "duden-Bank-search.html")
     ("https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp"
      "duden-Bank-1.html")
     ("https://www.duden.de/rechtschreibung/Bank_Geldinstitut?amp"
      "duden-Bank-2.html")))
  "Fixed Duden URLs for homograph test words.")

;;; Functions

(defun test-helper-woerterbuch--tests-dir ()
  "Return the tests directory this helper file lives in."
  (let ((file (or (symbol-file 'test-helper-woerterbuch--tests-dir)
                  load-file-name
                  buffer-file-name)))
    (unless file
      (error "Cannot determine tests directory"))
    (file-name-directory (expand-file-name file))))

(defun test-helper-woerterbuch--test-file (file)
  "Return absolute path to FILE below the tests directory."
  (expand-file-name file (test-helper-woerterbuch--tests-dir)))

(defun test-helper-woerterbuch--files-dir ()
  "Return the generated test files directory."
  (test-helper-woerterbuch--test-file "files/"))

(defun test-helper-woerterbuch--word-dir (source word)
  "Return fixture directory for SOURCE and WORD."
  (expand-file-name
   (format "%s/%s/" (symbol-name source) word)
   (test-helper-woerterbuch--files-dir)))

(defun test-helper-woerterbuch--fixture-file (source word name)
  "Return fixture file path for SOURCE, WORD and file NAME."
  (expand-file-name name
                    (test-helper-woerterbuch--word-dir source word)))

(defun test-helper-woerterbuch--expected-file (source word section)
  "Return expected-output file path for SOURCE, WORD and SECTION."
  (expand-file-name
   (format "%s-%s-%s-expected.el"
           (symbol-name source)
           word
           (string-remove-prefix ":" (symbol-name section)))
   (test-helper-woerterbuch--word-dir source word)))

(defun test-helper-woerterbuch-read-expected (source word section)
  "Read expected Lisp object for SOURCE, WORD and SECTION."
  (with-temp-buffer
    (insert-file-contents
     (test-helper-woerterbuch--expected-file source word section))
    (read (current-buffer))))

(defun test-helper-woerterbuch--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun test-helper-woerterbuch--source-output-files (source word)
  "Return `(URL FILE-NAME)' pairs for SOURCE and WORD.
If there are multiple homographs for WORD Duden has one page for each
homograph."
  (pcase source
    ('openthesaurus
     (list (list (woerterbuch-openthesaurus--build-url word)
                 (format "openthesaurus-%s.json" word))))
    ('dwds
     (list (list (woerterbuch-dwds--build-url word)
                 (format "dwds-%s.html" word))))
    ('wiktionary
     (list (list (woerterbuch-wiktionary--build-web-url word)
                 (format "wiktionary-%s.html" word))))
    ('duden
     (or (cdr (assoc word test-helper-woerterbuch-duden-extra-urls))
         (list (list (woerterbuch-duden--build-url word)
                     (format "duden-%s.html" word)))))
    (_
     (error "Unknown source: %S" source))))

(defun test-helper-woerterbuch--source-headers (source)
  "Return request headers for SOURCE."
  (pcase source
    ('dwds woerterbuch-dwds-request-headers)
    ('duden woerterbuch-duden-request-headers)
    ('wiktionary woerterbuch-wiktionary-request-headers)
    ('openthesaurus '(("User-Agent" . "woerterbuch/0.1")))
    (_ nil)))

(defun test-helper-woerterbuch--http-status ()
  "Return HTTP status code for the current url buffer, or nil."
  (or (and (boundp 'url-http-response-status)
           (numberp url-http-response-status)
           url-http-response-status)
      (save-excursion
        (goto-char (point-min))
        (when (looking-at "HTTP/[0-9.]+[[:space:]]+\\([0-9]+\\)")
          (string-to-number (match-string 1))))))

(defun test-helper-woerterbuch--goto-body ()
  "Move point to the start of the HTTP response body."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil 'move)))

(defun test-helper-woerterbuch--download-url (url file headers)
  "Download URL with HEADERS and save the response body to FILE.
Return the HTTP status code."
  (let ((url-request-extra-headers headers)
        (buffer (url-retrieve-synchronously url t t)))
    (unless buffer
      (error "Could not retrieve %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (test-helper-woerterbuch--http-status)))
            (when (or (null status) (< status 400))
              (make-directory (file-name-directory file) t)
              (test-helper-woerterbuch--goto-body)
              (write-region (point) (point-max) file nil 'silent))
            status))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun test-helper-woerterbuch--http-reason (status)
  "Return HTTP reason phrase for STATUS."
  (pcase status
    (200 "OK")
    (404 "Not Found")
    (_ "Test Response")))

(defun test-helper-woerterbuch--fixture-response (source word url)
  "Return mock `(STATUS . BODY)' for SOURCE, WORD and URL."
  (cond
   ((and (eq source 'duden)
         (string= word "Bank")
         (string= url (woerterbuch-duden--build-url word)))
    '(404 . nil))
   ((and (eq source 'duden)
         (string= word "Nixdaexistiert")
         (string= url (woerterbuch-duden--build-url word)))
    '(404 . nil))
   ((and (eq source 'duden)
         (string= word "Nixdaexistiert")
         (string= url (woerterbuch-duden--build-search-url word)))
    '(404 . "<html></html>"))
   ((and (eq source 'wiktionary)
         (string= word "Nixdaexistiert")
         (string= url (woerterbuch-wiktionary--build-web-url word)))
    '(404 . nil))
   (t
    (let* ((entry (assoc url (test-helper-woerterbuch--source-output-files
                              source word)))
           (file (and entry
                      (test-helper-woerterbuch--fixture-file
                       source word (cadr entry)))))
      (unless (and file (file-exists-p file))
        (error "Missing mock fixture for %S %s at %s" source word url))
      (cons 200 (test-helper-woerterbuch--read-file file))))))

(defun test-helper-woerterbuch-fetch-expected-output (source word section)
  "Fetch SOURCE for WORD and SECTION using stored fixtures only.

Return the same wrapper object that was persisted in the expected
output files."
  (let ((woerterbuch-sources (list source)))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback cbargs &rest _args)
                 (pcase-let* ((`(,status . ,body)
                               (test-helper-woerterbuch--fixture-response
                                source word url))
                              (buffer
                               (generate-new-buffer
                                (format " *woerterbuch-%s-%s*"
                                        (symbol-name source)
                                        word)))
                              (status-plist
                               (and (>= status 400)
                                    `(:error (error http ,status)))))
                   (with-current-buffer buffer
                     (insert (format "HTTP/1.1 %d %s\r\n\r\n"
                                     status
                                     (test-helper-woerterbuch--http-reason
                                      status)))
                     (setq-local url-http-response-status status)
                     (setq-local url-http-end-of-headers (point))
                     (when body
                       (insert body))
                     (goto-char (point-min))
                     (apply callback (append (list status-plist) cbargs)))
                   buffer))))
      (woerterbuch-fetch-all-sync
       word
       :sections (list section)
       :normalize-lemma nil
       :timeout 1))))

(provide 'test-helper)

;;; test-helper.el ends here
