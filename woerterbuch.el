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
(require 'thingatpt)

;; Require `org' once it is used.
(declare-function org-paste-subtree "org")
(declare-function org-current-level "org")

;;;; Customization

(defgroup woerterbuch nil
  "German dictionary and thesaurus."
  :group 'convenience
  :link '(url-link "https://github.com/hubisan/woerterbuch"))

(defcustom woerterbuch-org-buffer-display-function #'pop-to-buffer
  "Function used to the display the org buffer with the definitions or synonyms.
The function takes buffer as argument."
  :type 'function)

(defcustom woerterbuch-synonyms-list-bullet-point "-"
  "String to use as list bullet point when converting the synonyms to a string."
  :type 'string)

(defcustom woerterbuch-insert-org-heading-format "%s %s\n\n%s"
  "Format used when inserting an Org heading before content."
  :type 'string)

;;;; Major-Mode & Key Bindings

(define-derived-mode woerterbuch-mode org-mode "Woerterbuch"
  "Major mode for displaying woerterbuch buffer.")

(define-key woerterbuch-mode-map (kbd "q") 'quit-window)

;;;; Global Variables

(defvar woerterbuch--org-buffer-name "*woerterbuch*"
  "Name used for the Org buffer showing the defintions or synonyms.")

;;;; Auxiliary Functions

;; TODO Write Test
(defun woerterbuch--org-add-heading (heading level content)
  "Add text of HEADING with LEVEL as heading before CONTENT."
  (format woerterbuch-insert-org-heading-format
          (make-string level ?*) heading content))

;; TODO Write Test
(defun woerterbuch--org-insert (text with-heading &optional buffer)
  "Insert TEXT on current line if empty else on a new line.
If WITH-HEADING is non-nil the text includes an Org heading and is inserted
using `org-paste-subtree'. Will paste with the same level as the current
heading. If BUFFER is given, insert the text into that buffer instead of the
current."
  (with-current-buffer (or buffer (current-buffer))
    (if with-heading
        (if (eq major-mode 'woerterbuch-mode)
            (progn (require 'org)
                   (org-paste-subtree (or (org-current-level) 1) text))
          (user-error "Text can only be inserted in an Org buffer"))
      (if (string-match-p "\\`\\s-*$" (thing-at-point 'line))
          (kill-whole-line)
        (end-of-line)
        (newline))
      (insert text))))

;; TODO Write Test
(defun woerterbuch--get-word-at-point-or-selection ()
  "Get the word at point or the selection if region is active.
Returns a cons cell with the car being the word and cdr the bounds."
  (if-let ((bounds (if (use-region-p)
                       (cons (region-beginning) (region-end))
                     (bounds-of-thing-at-point 'word))))
      (cons
       (buffer-substring-no-properties (car bounds) (cdr bounds))
       bounds)
    (user-error "No word at point and no region active")))

;;;; German Definitions

;;;; German Synonyms

(defvar woerterbuch--synonyms-openthesaurus-url
  (concat
   "https://www.openthesaurus.de/synonyme/search?q=%s"
   "&format=application/json"
   "&baseform=true")
  "Url to retrieve the synonyms for a word as JSON from openthesaurus.")

(defun woerterbuch--synonyms-retrieve-raw (word)
  "Return the synonyms for a WORD as plist as retrieved with the API."
  (let* ((url (format woerterbuch--synonyms-openthesaurus-url
                      (url-hexify-string word)))
         (buffer (url-retrieve-synchronously url t)))
    (if (not buffer)
        (error "Could not retrieve synonyms")
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line)
        (prog1
            (json-parse-buffer :object-type 'plist)
          (kill-buffer buffer))))))

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

(defun woerterbuch--synonyms-retrieve-as-list (word)
  "Retrieve the synonyms for WORD as a list of lists.
Each list consist of the synonyms for one meaning of the word.
Returns a cons with car being the word and cdr the synonyms. The
word is returned as it can differntiate from the WORD used as
parameter when a baseform is used to retrieve the synonyms.
Returns nil if no synonyms are retrieved."
  (let* ((raw-synonyms (woerterbuch--synonyms-retrieve-raw word))
         (baseform (woerterbuch--synonyms-baseform raw-synonyms)))
    ;; If a baseform was found use that to retrieve the synonyms.
    (when baseform
      (setq raw-synonyms (woerterbuch--synonyms-retrieve-raw baseform)))
    (let ((synonyms (woerterbuch--synonyms-to-list raw-synonyms)))
      (when synonyms
        (cons (or baseform word) synonyms)))))

(defun woerterbuch--synonyms-convert-to-string (synonyms)
  "Convert the list of SYNONYMS to a string.
The string is a list. The group of synonyms for each meaning are
shown as an item. The list bullet point can be configured with
`woerterbuch-synonyms-list-bullet-point'"
  (mapconcat
     (lambda (elt)
       (format "%s %s"
               woerterbuch-synonyms-list-bullet-point
               (mapconcat #'identity elt ", ")))
     synonyms "\n"))

(defun woerterbuch--synonyms-retrieve-as-string (word with-heading)
  "Retrieve the synonyms for WORD as a string.
Returns a cons with car being the word and cdr the synonyms as string.
The word does not match WORD if it needed to use a baseform to retrieve the
synonyms. Returns nil if no synonyms are retrieved.
The returned string groups the synonyms for each meaning on one line.
It looks as follows:
- Erprobung, Probe, Prüfung
- Leistungsnachweis, Prüfung, Test
- etc.
If WITH-HEADING is non-nil a heading with the WORD as text is listed above the
synonyms."
  (when-let ((word-and-synonyms (woerterbuch--synonyms-retrieve-as-list word))
             (word-used (car word-and-synonyms))
             (synonyms (cdr word-and-synonyms))
             (synonyms-string
              (format "%s\n"
                      (woerterbuch--synonyms-convert-to-string synonyms))))
    (if with-heading
        (woerterbuch--org-add-heading word-used 1 synonyms-string)
      synonyms-string)))

(defun woerterbuch--read-synonym (word)
  "Read a synonym for WORD in the minibuffer and return it.
Returns nil if no synonym was selected.
Signals an user-error if there are no synoyms for WORD."
  (if-let ((word-and-synonyms (woerterbuch--synonyms-retrieve-as-list word))
           (word-used (car-safe word-and-synonyms))
           (synonyms (cdr-safe word-and-synonyms)))
      (when-let ((synonyms-flattened (apply #'append synonyms))
                 (synonyms-no-duplicates (seq-uniq synonyms-flattened))
                 (synonyms-sorted (seq-sort #'string-lessp
                                            synonyms-no-duplicates)))
        (completing-read "Select synonym: " synonyms-sorted nil t))
    (user-error "No synonyms found for %s" word)))

;; TODO Write Test
;;;###autoload
(defun woerterbuch-synonyms-show-in-org-buffer (&optional word)
  "Show the synonyms for WORD in an `org-mode' buffer."
  (interactive "sWort: ")
  (let ((buffer (get-buffer-create woerterbuch--org-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer))
    ;; FIXME Debug and maybe report bug (visual-fill-column-mode).
    ;; Moved turning `org-mode' on after displaying the buffer. Else I had some
    ;; problems with `visual-fill-column-mode' changing the margins of the
    ;; current buffer window. This is most likely a bug in
    ;; `visual-fill-column-mode'.
    (funcall woerterbuch-org-buffer-display-function buffer)
    (with-current-buffer buffer
      (woerterbuch-mode)
      (woerterbuch-synonyms-insert-into-org-buffer word t))))

;; TODO Write Test
;;;###autoload
(defun woerterbuch-synonyms-insert-into-org-buffer (word &optional with-heading)
  "Insert the synonyms for WORD into an `org-mode' buffer.
Will insert a list with each item being the synonyms for a
meaning. If WITH-HEADING is non-nil it inserts a heading with
the WORD as text and the list of synonyms below. In that case it
will insert a heading at the same level as the current level."
  (interactive "sWort: \nP")
  (let ((synonyms (woerterbuch--synonyms-retrieve-as-string word with-heading)))
    (save-excursion
      (woerterbuch--org-insert synonyms with-heading))))

;;;###autoload
(defun woerterbuch-synonyms-kill-as-org-mode-syntax (word &optional with-heading)
  "Add the synonyms for WORD to the kill ring as `org-mode' syntax.
Will add a list with each item being the synonyms for a meaning to the kill
ring. If WITH-HEADING is non-nil it will add a heading with the WORD as text
and the list of synonyms below."
  (interactive "sWort: \nP")
  (kill-new (woerterbuch--synonyms-retrieve-as-string word with-heading)))

;;;###autoload
(defun woerterbuch-synonyms-insert (word &optional to-kill-ring)
  "Lookup synonyms for WORD and insert selected word at point.
If TO-KILL-RING is non-nil it is added to the kill ring instead."
  (interactive "sWort: \nP")
  (when-let ((synonym (woerterbuch--read-synonym word)))
    (if to-kill-ring
        (kill-new synonym)
      (insert synonym))))

;;;###autoload
(defun woerterbuch-synonyms-lookup-word-at-point ()
  "Lookup synonyms for word at point and add to kill ring."
  (interactive)
  (if-let ((word-and-bounds (woerterbuch--get-word-at-point-or-selection))
           (word (car word-and-bounds)))
      (when-let ((synonym (woerterbuch--read-synonym word)))
        (kill-new synonym)
        synonym)
    (user-error "No word at point")))

;;;###autoload
(defun woerterbuch-synonyms-replace-word-at-point ()
  "Lookup synonyms for wort at point or selection and replace it."
  (interactive)
  (if-let ((word-and-bounds (woerterbuch--get-word-at-point-or-selection))
           (word (car word-and-bounds))
           (bounds (cdr word-and-bounds)))
      (when-let ((synonym (woerterbuch--read-synonym word)))
        (delete-region (car bounds) (cdr bounds))
        (insert synonym))
    (user-error "No word at point")))

(provide 'woerterbuch)

;;; woerterbuch.el ends here
