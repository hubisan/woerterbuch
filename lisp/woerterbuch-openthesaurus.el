;;; woerterbuch-openthesaurus.el --- Query synonyms from OpenThesaurus  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Your Name <you@example.com>
;; Keywords: language, dictionary
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; URL: https://www.openthesaurus.de

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Simple interface to the OpenThesaurus API.
;;
;; Example:
;;
;;   (woerterbuch-openthesaurus-synsets "Baum")
;;
;; returns
;;
;; ((:categories ("Botanik")
;;   :synonyms ("Makrophanerophyt"))
;;  (:categories ("Mathematik")
;;   :synonyms ("azyklischer, zusammenhängender Graph"))
;;  (:categories ("Computer" "Biologie")
;;   :synonyms ("Baumstruktur" "Kladogramm" ...)))

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defgroup woerterbuch-openthesaurus nil
  "Access the OpenThesaurus API."
  :group 'applications)

(defcustom woerterbuch-openthesaurus-user-agent
  "emacs-woerterbuch-openthesaurus/0.1 (contact: you@example.com)"
  "User-Agent sent to the OpenThesaurus API."
  :type 'string
  :group 'woerterbuch-openthesaurus)

(defvar woerterbuch-openthesaurus-api-url
  "https://www.openthesaurus.de/synonyme/search")

(defun woerterbuch-openthesaurus--request (word)
  "Return parsed JSON result for WORD from OpenThesaurus."
  (let* ((url-request-extra-headers
          `(("User-Agent" . ,woerterbuch-openthesaurus-user-agent)))
         (url (format "%s?q=%s&format=application/json"
                      woerterbuch-openthesaurus-api-url
                      (url-hexify-string word)))
         (buffer (url-retrieve-synchronously url t t 10)))
    (unless buffer
      (error "OpenThesaurus request failed"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (forward-char)
          (json-parse-buffer :object-type 'alist :array-type 'list))
      (kill-buffer buffer))))

(defun woerterbuch-openthesaurus-synsets (word)
  "Return synonym groups for WORD as plist structures.

Each entry has the form:

  (:categories (STRING...)
   :synonyms (STRING...))"
  (let* ((data (woerterbuch-openthesaurus--request word))
         (synsets (alist-get 'synsets data)))
    (cl-loop
     for synset in synsets
     for categories = (alist-get 'categories synset)
     for synonyms =
     (cl-loop for term in (alist-get 'terms synset)
              for value = (alist-get 'term term)
              unless (string= value word)
              collect value)
     when synonyms
     collect (list
              :categories categories
              :synonyms synonyms))))

(defun woerterbuch-openthesaurus-synonyms (word)
  "Return a flat list of synonyms for WORD."
  (delete-dups
   (cl-loop for entry in (woerterbuch-openthesaurus-synsets word)
            append (plist-get entry :synonyms))))

;;;###autoload
(defun woerterbuch-openthesaurus-lookup (word)
  "Display synonyms for WORD in the echo area."
  (interactive "sWord: ")
  (let ((result (woerterbuch-openthesaurus-synsets word)))
    (if result
        (dolist (entry result)
          (message "%s → %s"
                   (string-join (plist-get entry :categories) ", ")
                   (string-join (plist-get entry :synonyms) ", ")))
      (message "No synonyms found for %s" word))))

(provide 'woerterbuch-openthesaurus)

;;; woerterbuch-openthesaurus.el ends here
