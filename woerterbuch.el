;;; woerterbuch.el --- German Dictionary and Thesaurus -*- lexical-binding: t -*-

;; Copyright (C) 2023 Daniel Hubmann

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/woerterbuch
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ...

;; TODO

;;; Code:

;;;; Requirements

(require 'seq)
(require 'map)

;;;; Customization

(defgroup woerterbuch nil
  "German dictionary and thesaurus."
  :group 'convenience
  :link '(url-link "https://github.com/hubisan/woerterbuch"))

(defcustom woerterbuch-synoyms-display-function nil
  "The function used to the display the org buffer with the synonyms."
  :type 'function)

;;;; German Dictionary

;;;; German Thesaurus

(defvar woerterbuch--synonyms-openthesaurus-url
  (concat
   "https://www.openthesaurus.de/synonyme/search?q=%s"
   "&format=application/json"
   "&baseform=true")
  "Url to retrieve the synonyms for a word as JSON from openthesaurus.")

(defun woerterbuch--synonyms-retrieve (word)
  "Retrieve the synonyms for WORD as a list of lists.
Each list consist of the synonyms for one meaning of the word.
Returns nil if no synonyms are retrieved."
  (let* ((raw-synonyms (woerterbuch--synonyms-retrieve-raw word))
         (baseform (woerterbuch--synonyms-baseform raw-synonyms)))
    ;; If a baseform was found use that to retrieve the synonyms.
    (when baseform
      (setq raw-synonyms (woerterbuch--synonyms-retrieve-raw baseform)))
    (woerterbuch--synonyms-to-list raw-synonyms)))

(defun woerterbuch--synonyms-retrieve-raw (word)
  "Return the synonyms for a WORD as plist as retrieved with the API."
  (let* ((url (format woerterbuch--synonyms-openthesaurus-url
                      (url-hexify-string word)))
         (buffer (url-retrieve-synchronously url)))
    (if (not buffer)
        (error "Could not retrieve synonyms")
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line)
        (json-parse-buffer :object-type 'plist)))))

(defun woerterbuch--synonyms-to-list (raw-synonyms)
  "Convert the RAW-SYNONYMS retrieved with the API to a list of lists.
Each list consist of the synonyms for one meaning of the word."
  (let* ((synsets (seq-into (plist-get raw-synonyms :synsets) 'list)))
    (mapcar (lambda (synonyms-group)
              (mapcar #'cadr (plist-get synonyms-group :terms)))
            synsets)))

(defun woerterbuch--synonyms-baseform (raw-synonyms)
  "Try to get the baseform of the word from RAW-SYNONYMS.
Returns nil if not found."
  (map-elt raw-synonyms :baseforms)
  (car-safe (seq-into (plist-get raw-synonyms :baseforms) 'list)))

;;;###autoload
(defun woerterbuch-synonyms-show-in-org-buffer (&optional word)
  "Show the synonyms for WORD in an `org-mode' buffer."
  (interactive "P")

  )

;;;###autoload
(defun woerterbuch-synonyms-insert-into-org-buffer (&optional word include-heading)
  "Insert the synonyms for WORD into an `org-mode' buffer.
Will insert a list with each item being the synonyms for a meaning.
If INCLUDE-HEADING is non-nil it inserts a subheading with the WORD as text and
the list of synonyms below."
  (interactive "P")

  )

;;;###autoload
(defun woerterbuch-synonyms-kill-as-org-mode-syntax (&optional word include-heading)
  "Add the synonyms for WORD to the kill ring as `org-mode' syntax.
Will add a list with each item being the synonyms for a meaning.
If INCLUDE-HEADING is non-nil it will add a subheading with the WORD as text and
the list of synonyms below."
  (interactive "P")

  )

;;;###autoload
(defun woerterbuch-synonyms-lookup (word &optional to-kill-ring)
  "Lookup synonyms for WORD and insert selected word at point.
If TO-KILL-RING is non-nil it is added to the kill ring instead."
  (interactive "P")

  )

(provide 'woerterbuch)

;;; woerterbuch.el ends here
