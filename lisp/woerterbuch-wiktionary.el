;;; wiktionary-data.el --- Fast German Wiktionary data extraction -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: dictionary, wiktionary, linguistics
;; URL: https://example.com/wiktionary-data

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides fast structured extraction of linguistic
;; data from German Wiktionary using the MediaWiki API.
;;
;; Extracted fields include:
;;
;;  - meanings
;;  - origin (etymology)
;;  - examples
;;  - idioms (Redewendungen)
;;  - synonyms
;;  - related words (Sinnverwandte Wörter)
;;
;; The implementation fetches raw Wiktionary wikitext and parses
;; semantic blocks directly. This avoids slow HTML parsing and
;; keeps requests minimal (one API call per word).
;;
;; A simple in-memory cache is provided for repeated lookups.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)

(defgroup wiktionary-data nil
  "Structured extraction of data from German Wiktionary."
  :group 'applications)

(defcustom wiktionary-data-api-url
  "https://de.wiktionary.org/w/api.php"
  "Base URL of the German Wiktionary API."
  :type 'string
  :group 'wiktionary-data)

(defcustom wiktionary-data-request-timeout
  10
  "Timeout in seconds for Wiktionary HTTP requests."
  :type 'integer
  :group 'wiktionary-data)

(defvar wiktionary-data--cache
  (make-hash-table :test #'equal)
  "In-memory cache for Wiktionary entries.")

;;;; HTTP

(defun wiktionary-data--fetch-json (url)
  "Fetch URL synchronously and return parsed JSON."
  (let ((buffer (url-retrieve-synchronously url t t wiktionary-data-request-timeout)))
    (unless buffer
      (error "Failed to retrieve URL: %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "\n\n")
          (json-parse-string
           (buffer-substring-no-properties (point) (point-max))
           :object-type 'hash-table
           :array-type 'list
           :null-object nil
           :false-object :false))
      (kill-buffer buffer))))

(defun wiktionary-data--build-query-url (word)
  "Build API URL to fetch raw Wiktionary wikitext for WORD."
  (format
   "%s?action=query&prop=revisions&titles=%s&rvslots=*&rvprop=content&format=json&formatversion=2"
   wiktionary-data-api-url
   (url-hexify-string word)))

(defun wiktionary-data--extract-content (json)
  "Extract page content from MediaWiki JSON response."
  (let* ((query (gethash "query" json))
         (pages (gethash "pages" query))
         (page (car pages))
         (revisions (gethash "revisions" page))
         (revision (car revisions))
         (slots (gethash "slots" revision))
         (main (gethash "main" slots)))
    (gethash "content" main)))

;;;; Section extraction

(defun wiktionary-data--extract-section (text regexp)
  "Extract section from TEXT whose heading matches REGEXP."
  (when (string-match regexp text)
    (let* ((start (match-beginning 0))
           (end (or (and (string-match "^==[^=]" text (match-end 0))
                         (match-beginning 0))
                    (length text))))
      (substring text start end))))

(defun wiktionary-data--extract-language-section (text)
  "Extract the German language section from TEXT."
  (wiktionary-data--extract-section
   text
   "^== .*({{Sprache|Deutsch}}).*=="))

(defun wiktionary-data--extract-wordclass-section (text word-class)
  "Extract WORD-CLASS subsection from TEXT."
  (wiktionary-data--extract-section
   text
   (format "^=== .*{{Wortart|%s|Deutsch}}.*===" (regexp-quote word-class))))

(defun wiktionary-data--extract-block (text block)
  "Extract Wiktionary block named BLOCK from TEXT."
  (when (string-match (format "^{{%s}}" block) text)
    (let ((start (match-end 0)))
      (substring
       text
       start
       (or (and (string-match "^{{[A-Za-z]" text start)
                (match-beginning 0))
           (length text))))))

;;;; Text cleanup

(defun wiktionary-data--remove-refs (text)
  "Remove <ref> tags from TEXT."
  (replace-regexp-in-string "<ref[^>]*>.*?</ref>" "" text t t))

(defun wiktionary-data--replace-wikilinks (text)
  "Replace wikilinks in TEXT with visible labels."
  (setq text
        (replace-regexp-in-string
         "\\[\\[[^]|]+|\\([^]]+\\)\\]\\]" "\\1" text))
  (replace-regexp-in-string
   "\\[\\[\\([^]]+\\)\\]\\]" "\\1" text))

(defun wiktionary-data--cleanup-text (text)
  "Perform lightweight cleanup of Wiktionary markup in TEXT."
  (let ((result text))
    (setq result (wiktionary-data--remove-refs result))
    (setq result (wiktionary-data--replace-wikilinks result))
    (setq result (replace-regexp-in-string "{{[^{}\n]+}}" "" result))
    (setq result (replace-regexp-in-string "''+" "" result))
    (string-trim
     (replace-regexp-in-string "[ \t\n\r]+" " " result))))

;;;; Block parsers

(defun wiktionary-data--parse-numbered-lines (text)
  "Parse numbered lines like :[1] ... from TEXT."
  (let ((lines (split-string text "\n" t))
        results)
    (dolist (line lines (nreverse results))
      (when (string-match "^:\\[\\([0-9]+\\)\\] \\(.*\\)$" line)
        (push
         (list
          :index (string-to-number (match-string 1 line))
          :text (wiktionary-data--cleanup-text
                 (match-string 2 line)))
         results)))))

(defun wiktionary-data--parse-sense-lines (text)
  "Parse lines like :[1] text from TEXT."
  (let ((lines (split-string text "\n" t))
        results)
    (dolist (line lines (nreverse results))
      (when (string-match "^:\\[\\([^]]+\\)\\] \\(.*\\)$" line)
        (push
         (list
          :sense (match-string 1 line)
          :text (wiktionary-data--cleanup-text
                 (match-string 2 line)))
         results)))))

(defun wiktionary-data--parse-origin (text)
  "Parse origin block TEXT."
  (wiktionary-data--cleanup-text text))

(defun wiktionary-data--parse-related (text)
  "Parse related words block TEXT."
  (wiktionary-data--parse-sense-lines text))

;;;; Public API

(defun wiktionary-data-fetch (word &optional word-class)
  "Fetch structured data for WORD from German Wiktionary.

Optional WORD-CLASS restricts extraction to a specific class
such as \"Substantiv\", \"Verb\", or \"Adjektiv\".

Return a plist containing extracted data."
  (let* ((url (wiktionary-data--build-query-url word))
         (json (wiktionary-data--fetch-json url))
         (content (wiktionary-data--extract-content json))
         (lang (wiktionary-data--extract-language-section content))
         (entry (if word-class
                    (wiktionary-data--extract-wordclass-section
                     lang word-class)
                  lang))
         (meanings (wiktionary-data--extract-block entry "Bedeutungen"))
         (origin (wiktionary-data--extract-block entry "Herkunft"))
         (examples (wiktionary-data--extract-block entry "Beispiele"))
         (idioms (wiktionary-data--extract-block entry "Redewendungen"))
         (synonyms (wiktionary-data--extract-block entry "Synonyme"))
         (related (wiktionary-data--extract-block entry "Sinnverwandte Wörter")))
    (list
     :word word
     :meanings (and meanings
                    (wiktionary-data--parse-numbered-lines meanings))
     :origin (and origin
                  (wiktionary-data--parse-origin origin))
     :examples (and examples
                    (wiktionary-data--parse-sense-lines examples))
     :idioms (and idioms
                  (wiktionary-data--parse-sense-lines idioms))
     :synonyms (and synonyms
                    (wiktionary-data--parse-sense-lines synonyms))
     :related-words (and related
                         (wiktionary-data--parse-related related)))))

(defun wiktionary-data-fetch-cached (word &optional word-class)
  "Fetch Wiktionary data for WORD using cache."
  (let ((key (list word word-class)))
    (or (gethash key wiktionary-data--cache)
        (puthash key
                 (wiktionary-data-fetch word word-class)
                 wiktionary-data--cache))))

;;;###autoload
(defun wiktionary-data-fetch-noun (word)
  "Fetch noun data for WORD from German Wiktionary."
  (interactive "sWord: ")
  (wiktionary-data-fetch-cached word "Substantiv"))

(provide 'wiktionary-data)

;;; wiktionary-data.el ends here
