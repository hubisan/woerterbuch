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

;; Lookup definitions and synonyms for German words with Emacs.

;; Main features:

;; - Lookup synonyms for a word (read from minibuffer or word at point) and
;;   insert the selected synonym into the current buffer, replace the word at
;;   point or add it to the kill ring.
;; - Show synonyms for a word (read from minibuffer or word at point) in an
;;   Org-mode buffer, insert them into the current Org-mode buffer or add them
;;   to the kill ring.
;; - Show definitions for a word (read from minibuffer or word at point) in an
;;   Org-mode buffer, insert them into the current Org-mode buffer or add them
;;   to the kill ring.
;; - Show both in an Org-mode buffer.

;; Check out the documentation in the README for more.

;;; Code:

;;;; Requirements

(require 'seq)
(require 'map)
(require 'thingatpt)
(require 'dom)

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

(defcustom woerterbuch-list-bullet-point "-"
  "String to use as list bullet point.
This should be one compatible with `org-mode' lists."
  :type '(choice (string :tag "-" :value "-")
                 (string :tag "+" :value "+")))

(defcustom woerterbuch-insert-org-heading-format "%s %s\n\n%s"
  "Format used when inserting an Org heading before content.
You most likely only want to change this, if you want to change the number of
newlines.
Format is called with three parameters:
- Stars to start a heading
- Text of the heading
- The content"
  :type 'string)

(defcustom woerterbuch-definitions-heading-text-format
  "[[https://www.dwds.de/wb/%1$s][%1$s]] - Bedeutungen"
  "Format used for the heading text when inserting an Org heading before content.
Format is called with one parameters:
- The word (or baseform) definitions were retrieved for."
  :type 'string)

(defcustom woerterbuch-definitions-no-matches-text-format
  "Keine Bedeutungen für [[https://www.dwds.de/wb/%1$s][%1$s]] gefunden.\n"
  "Format used for the text when no definitions are found.
Format is called with one parameters:
- The word (or baseform) used to try to get definitions."
  :type 'string)

(defcustom woerterbuch-synonyms-heading-text-format
  "[[https://www.openthesaurus.de/synonyme/%1$s][%1$s]] - Synonyme"
  "Format used for the heading text when inserting an Org heading before content.
Format is called with one parameters:
- The word (or baseform) synonyms were retrieved for."
  :type 'string)

(defcustom woerterbuch-synonyms-no-matches-text-format
  "Keine Synonyme für [[https://www.openthesaurus.de/synonyme/%1$s][%1$s]] gefunden.\n"
  "Format used for the text when no synonyms are found.
Format is called with one parameters:
- The word (or baseform) used to try to get synonyms."
  :type 'string)

;;;; Major-Mode & Key Bindings

(define-derived-mode woerterbuch-mode org-mode "Woerterbuch"
  "Major mode for displaying woerterbuch buffer.")

(define-key woerterbuch-mode-map (kbd "C-c C-q") 'quit-window)

;;;; Global Variables

(defvar woerterbuch--org-buffer-name "*woerterbuch*"
  "Name used for the Org buffer showing the defintions or synonyms.")

;;;; Auxiliary Functions

(defun woerterbuch--org-add-heading (heading level content)
  "Add text of HEADING with LEVEL as heading before CONTENT."
  (format woerterbuch-insert-org-heading-format
          (make-string level ?*) heading content))

(defun woerterbuch--org-insert (text with-heading &optional buffer)
  "Insert TEXT on current line if empty else on a new line.
If WITH-HEADING is non-nil the text includes an Org heading and is inserted
using `org-paste-subtree'. Will paste with the same level as the current
heading. If BUFFER is given, insert the text into that buffer instead of the
current."
  (with-current-buffer (or buffer (current-buffer))
    (if with-heading
        (if (or (eq major-mode 'org-mode)
                (eq major-mode 'woerterbuch-mode))
            (progn (require 'org)
                   (org-paste-subtree (or (org-current-level) 1) text))
          (user-error "Text can only be inserted in an Org buffer"))
      ;; NOTE Not using (thing-at-point 'line) as this returns nil, if a buffer
      ;; only contains whitespace.
      (if (string-match-p "\\`\\s-*$"
                          (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
          ;; If on an empty line make sure to delete any whitespace.
          ;; This gives an error if point as at end of the buffer.
          (ignore-errors (kill-whole-line))
        (end-of-line)
        (newline))
      (insert text))))

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

(defvar woerterbuch--definitions-dwds-url "https://www.dwds.de/wb/%s"
  "Url to retrieve the definitions for a word as html from DWDS.")

(defun woerterbuch--definitions-retrieve-raw (word)
  "Return a raw list of definitions for WORD.
Each element is a cons with the car being the id of the defintion and the cadr
the text. Gets the definition from URL `https://www.dwds.de.'"
  (let* ((url (format woerterbuch--definitions-dwds-url
                      (url-hexify-string (string-trim word))))
         (buffer (url-retrieve-synchronously url t)))
    (if (not buffer)
        (error "Could not retrieve definitons")
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line 2)
        (unwind-protect
            (when-let* ((html-parse-tree
                         (libxml-parse-html-region (point) (point-max)))
                        (lesearten
                         (dom-by-class html-parse-tree "^dwdswb-lesart$")))
              ;; Make the list and remove nils.
              (remove nil
                      ;; There can be more then one group of definitions for a
                      ;; word. This is the case if the word can be noun or a
                      ;; verb or a adverb. So loop through lesearten to get em
                      ;; all.
                      (mapcar
                       (lambda (leseart)
                         (when-let ((id (dom-attr leseart 'id))
                                    (text (dom-texts
                                           (dom-by-class
                                            leseart
                                            "^dwdswb-definition$"))))
                           (when (and (stringp id) (not (string-empty-p text)))
                             (cons id text))))
                       lesearten)))
          (kill-buffer buffer))))))

(defun woerterbuch--definitions-get-baseform (word &optional raw-synonyms)
  "Return the baseform (lemma) of the WORD.
If the WORD is already the baseform return WORD.
If RAW-SYNONYMS has already been retrieved, it can be passed as parameter.
The function uses the Openthesaurus API to find the base form of a word, as I
couldn't find any other tool that could do this in a straightforward manner."
  (let* ((raw-synonyms (or raw-synonyms
                           (woerterbuch--synonyms-retrieve-raw word)))
         (baseform (woerterbuch--synonyms-baseform raw-synonyms)))
    (or baseform word)))

(defun woerterbuch--definitions-to-list (raw-definitions)
  "Tranform RAW-DEFINITIONS to a nested list with definitions and subdefinitions.
Not entirely sure if a defintion can only have one level of subexpressions,
therefore this function is designed to also work with more than one level."
  (let* ((definitions '())
         definition previous-id)
    (while (setq definition (pop raw-definitions))
      (let* ((id (car definition))
             ;; Get the text and clean it.
             (text (woerterbuch--definitions-clean-text (cdr definition))))
        (cond
         ;; If it is the first definition or the same level as before.
         ((or (not previous-id) (length= previous-id (length id)))
          ;; Add the defintion to definitions and cleanf
          (push (list text) definitions)
          (setq previous-id id))
         ;; If the id is longer than the previous one handle child definitions.
         ((and previous-id (length> id (length previous-id)))
          (let ((children (list definition)))
            ;; Get all the children.
            (while
                (and raw-definitions
                     (equal
                      (substring (caar raw-definitions) 0 (length previous-id))
                      previous-id))
              (setq definition (pop raw-definitions))
              (setq children (append children (list definition))))
            ;; Recursively call the function to add the children to the first
            ;; item in the definitions.
            (setcar definitions (list (caar definitions)
                                      (woerterbuch--definitions-to-list
                                       children))))))))
    (when definitions
      (nreverse definitions))))

(defun woerterbuch--definitions-clean-text (text)
  "Clean the TEXT of the definitions."
  (when-let*
      ((text-trimmed (string-trim text))
       ;; Remove those targets placed after links to other defintions. Either a
       ;; number, a letter or dot symbol ● in parentheses like (1), (2), (●). Or
       ;; actually multiple of those if it is nested like (1 b). A good example
       ;; is if getting the definitions for Katze or Wurst.
       (text-link-targets-removed
        (replace-regexp-in-string "([[:alnum:]● ]+)" "" text-trimmed))
       ;; If a word has more than one tab a superscript is used in links.
       ;; For instance in the definitions for word Wurst.
       (text-superscripts-removed
        (replace-regexp-in-string "[⁰¹²³⁴⁵⁶⁷⁸⁹]" "" text-link-targets-removed))
       ;; Change all consecutive spaces into one space.
       (text-multiple-spaces-removed
        (replace-regexp-in-string "[[:blank:]]+" " "
                                  text-superscripts-removed))
       ;; Sometimes there are spaces before commas.
       (text-space-before-comma-removed
        (replace-regexp-in-string " ," ","
                                  text-multiple-spaces-removed)))
    text-space-before-comma-removed))

(defun woerterbuch--definitions-retrieve-as-list (word)
  "Retrieve the definitions for WORD as a list.
Each list consist of one or multiple definitions (meanings) of a word. Each
definition can a list of hold subdefinitions. Returns a cons with car being the
word and cdr the definitions. The word is returned as it can differntiate from
the WORD used as parameter when a baseform is used to retrieve the definitions.
Returns nil if no definition was found."
  (let* ((baseform (woerterbuch--definitions-get-baseform word))
         (raw-definitions (woerterbuch--definitions-retrieve-raw baseform)))
    (let ((definitions (woerterbuch--definitions-to-list raw-definitions)))
      (cons baseform definitions))))

(defun woerterbuch--definitions-to-string (definitions &optional lvl)
  "Convert the list of DEFINITIONS to a string.
LVL is used when the function is called recursively to process the children.
The list bullet point can be configured with `woerterbuch-list-bullet-point'."
  (let ((lvl (or lvl 0)))
    (mapconcat
     (lambda (definition)
       (let ((text (format "%s%s %s"
                           (make-string (* 2 lvl) ? )
                           woerterbuch-list-bullet-point
                           (car definition)))
             (children (car-safe (cdr-safe definition))))
         (if children
             ;; Call function again with children.
             (format
              "%s\n%s" text
              (woerterbuch--definitions-to-string children (1+ lvl)))
           text)))
     definitions "\n")))

(defun woerterbuch--definitions-retrieve-as-string (word &optional with-heading)
  "Retrieve the definitions for WORD as a string.
Returns a cons with car being the WORD and cdr the definitions as string.
The car will be the baseform if the WORD was not a baseform.
If no definitions are found it inserts a link to the dwds page as string.
If WITH-HEADING is non-nil a heading with the WORD as text is added above the
definitions."
  (let* ((word-and-definitions (woerterbuch--definitions-retrieve-as-list
                                word))
         (word-used (car-safe word-and-definitions))
         (definitions (cdr-safe word-and-definitions))
         (text (if definitions
                   (format "%s\n" (woerterbuch--definitions-to-string
                                   definitions))
                 (format woerterbuch-definitions-no-matches-text-format
                         word-used)))
         (definitions-string
          (if with-heading
              (woerterbuch--org-add-heading
               (format woerterbuch-definitions-heading-text-format word-used)
               1 text)
            text)))
    (cons word-used definitions-string)))

(defun woerterbuch--definitions-read-definition (word)
  "Read a definition for WORD in the minibuffer and return it.
Returns nil if no definition was selected."
  (if-let ((word-and-definitions (woerterbuch--definitions-retrieve-as-list word))
           (word-used (car-safe word-and-definitions))
           (definitions (cdr-safe word-and-definitions)))
      (when-let ((definitions-flattened (flatten-list definitions)))
        (completing-read "Select definition: " definitions-flattened nil t))
    (user-error "No synonyms found for %s" word)))

;;;###autoload
(defun woerterbuch-definitions-show-in-org-buffer (&optional word)
  "Show the defintions for WORD in an `org-mode' buffer.
Returns the buffer."
  (interactive "sWort: ")
  (let ((buffer (get-buffer-create woerterbuch--org-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer))
    ;; FIXME Debug and maybe report bug (visual-fill-column-mode).
    ;; Moved turning `org-mode' on after displaying the buffer. Else I had some
    ;; problems with `visual-fill-column-mode' changing the margins of the
    ;; current buffer window, the selected window. Before the buffer was shown.
    ;; This is most likely a bug in `visual-fill-column-mode'.
    (funcall woerterbuch-org-buffer-display-function buffer)
    (with-current-buffer buffer
      (woerterbuch-mode)
      (woerterbuch-definitions-insert-into-org-buffer word t)
      buffer)))

;;;###autoload
(defun woerterbuch-definitions-show-in-org-buffer-for-word-at-point ()
  "Show the definitions for the word at point in an `org-mode' buffer.
Returns the buffer."
  (interactive)
  (if-let ((word-and-bounds (woerterbuch--get-word-at-point-or-selection))
           (word (car word-and-bounds)))
      (woerterbuch-definitions-show-in-org-buffer word)
    (user-error "No word at point")))

;;;###autoload
(defun woerterbuch-definitions-insert-into-org-buffer (word &optional with-heading)
  "Insert the definitions for WORD into an `org-mode' buffer.
If WITH-HEADING is non-nil it inserts a heading with
the WORD as text before the definitions."
  (interactive "sWort: \nP")
  (let* ((word-and-defintions
          (woerterbuch--definitions-retrieve-as-string word with-heading))
         (definitions (cdr-safe word-and-defintions)))
    (save-excursion
      (woerterbuch--org-insert definitions with-heading))))

;;;###autoload
(defun woerterbuch-definitions-kill-as-org-mode-syntax (word &optional with-heading)
  "Add the definitions for WORD to the kill ring as `org-mode' syntax.
If WITH-HEADING is non-nil it will add a heading with the WORD as text
before the definitions."
  (interactive "sWort: \nP")
  (kill-new (cdr-safe
             (woerterbuch--definitions-retrieve-as-string word with-heading))))

;;;###autoload
(defun woerterbuch-definitions-insert (word &optional to-kill-ring)
  "Lookup definitions for WORD and insert selected definition at point.
If TO-KILL-RING is non-nil it is added to the kill ring instead."
  (interactive "sWort: \nP")
  (when-let ((definition (woerterbuch--definitions-read-definition word)))
    (if to-kill-ring
        (kill-new definition)
      (insert definition))))

;;;; German Synonyms

(defvar woerterbuch--synonyms-openthesaurus-url
  "https://www.openthesaurus.de/synonyme/%s"
  "Url to link to a synonyms page.")

(defvar woerterbuch--synonyms-openthesaurus-api-url
  (concat
   "https://www.openthesaurus.de/synonyme/search?q=%s"
   "&format=application/json"
   "&baseform=true")
  "Url to retrieve the synonyms for a word as JSON from openthesaurus.")

(defun woerterbuch--synonyms-retrieve-raw (word)
  "Return the synonyms for a WORD as plist as retrieved with the API."
  (let* ((url (format woerterbuch--synonyms-openthesaurus-api-url
                      (url-hexify-string (string-trim word))))
         (buffer (url-retrieve-synchronously url t)))
    (if (not buffer)
        (error "Could not retrieve synonyms")
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line)
        (unwind-protect
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
Returns nil if not found.
Sometimes openthesaurus returns synsets (synoyms) for words that are not in the
baseform like for the word Kinder or Frauen. But sometimes it returns a baseform
for words that are already a baseform. To avoid problems this functions returns
nil if synsets are not empty."
  (if (not (seq-empty-p (plist-get raw-synonyms :synsets)))
      ;; For some words a baseform is given even though it already is a baseform
      ;; and synonyms are returned. In that case return nil to signal that
      ;; there is no baseform.
      nil
    (map-elt raw-synonyms :baseforms)
    (car-safe (seq-into (plist-get raw-synonyms :baseforms) 'list))))

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
      (cons (or baseform word) synonyms))))

(defun woerterbuch--synonyms-to-string (synonyms)
  "Convert the list of SYNONYMS to a string.
The string is a list. The group of synonyms for each meaning are
shown as an item. The list bullet point can be configured with
`woerterbuch-list-bullet-point'"
  (mapconcat
     (lambda (elt)
       (format "%s %s"
               woerterbuch-list-bullet-point
               (mapconcat #'identity elt ", ")))
     synonyms "\n"))

(defun woerterbuch--synonyms-retrieve-as-string (word &optional with-heading)
  "Retrieve the synonyms for WORD as a string in Org-mode syntax.
Returns a cons with car being the word and cdr the synonyms as string.
The car will be the baseform if the WORD was not a baseform.
If no synonyms are found it inserts a link to the openthesaurus page as string.
The returned string groups the synonyms for each meaning on one line.
It looks as follows:
- Erprobung, Probe, Prüfung
- Leistungsnachweis, Prüfung, Test
- etc.
If WITH-HEADING is non-nil a heading with the WORD as text is added above the
synonyms."
  (let* ((word-and-synonyms (woerterbuch--synonyms-retrieve-as-list word))
         (word-used (car-safe word-and-synonyms))
         (synonyms (cdr-safe word-and-synonyms))
         (text (if synonyms
                   (format "%s\n" (woerterbuch--synonyms-to-string synonyms))
                 (format woerterbuch-synonyms-no-matches-text-format
                         word-used)))
         ;; Add a heading if needed.
         (synonyms-string
          (if with-heading
              (woerterbuch--org-add-heading
               (format woerterbuch-synonyms-heading-text-format word-used)
               1 text)
            text)))
    (cons word-used synonyms-string)))

(defun woerterbuch--synonyms-read-synonym (word)
  "Read a synonym for WORD in the minibuffer and return it.
Returns nil if no synonym was selected."
  (if-let ((word-and-synonyms (woerterbuch--synonyms-retrieve-as-list word))
           (word-used (car-safe word-and-synonyms))
           (synonyms (cdr-safe word-and-synonyms)))
      (when-let ((synonyms-flattened (apply #'append synonyms))
                 (synonyms-no-duplicates (seq-uniq synonyms-flattened))
                 (synonyms-sorted (seq-sort #'string-lessp
                                            synonyms-no-duplicates)))
        (completing-read "Select synonym: " synonyms-sorted nil t))
    (user-error "No synonyms found for %s" word)))

;;;###autoload
(defun woerterbuch-synonyms-show-in-org-buffer (&optional word)
  "Show the synonyms for WORD in an `org-mode' buffer.
Returns the buffer."
  (interactive "sWort: ")
  (let ((buffer (get-buffer-create woerterbuch--org-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer))
    ;; FIXME Debug and maybe report bug (visual-fill-column-mode).
    ;; Moved turning `org-mode' on after displaying the buffer. Else I had some
    ;; problems with `visual-fill-column-mode' changing the margins of the
    ;; current buffer window, the selected window. Before the buffer was shown.
    ;; This is most likely a bug in `visual-fill-column-mode'.
    (funcall woerterbuch-org-buffer-display-function buffer)
    (with-current-buffer buffer
      (woerterbuch-mode)
      (woerterbuch-synonyms-insert-into-org-buffer word t)
      buffer)))

;;;###autoload
(defun woerterbuch-synonyms-show-in-org-buffer-for-word-at-point ()
  "Show the synonyms for the word at point in an `org-mode' buffer.
Returns the buffer."
  (interactive)
  (if-let ((word-and-bounds (woerterbuch--get-word-at-point-or-selection))
           (word (car word-and-bounds)))
      (woerterbuch-synonyms-show-in-org-buffer word)
    (user-error "No word at point")))

;;;###autoload
(defun woerterbuch-synonyms-insert-into-org-buffer (word &optional with-heading)
  "Insert the synonyms for WORD into an `org-mode' buffer.
Will insert a list with each item being the synonyms for a meaning. If
WITH-HEADING is non-nil it inserts a heading with the WORD as text before the
synonyms. If there are no synonyms found it will insert a text with a link to
openthesaurus."
  (interactive "sWort: \nP")
  (let* ((word-and-synonyms (woerterbuch--synonyms-retrieve-as-string
                             word with-heading))
         (synonyms (cdr-safe word-and-synonyms)))
    (save-excursion
      (woerterbuch--org-insert synonyms with-heading))))

;;;###autoload
(defun woerterbuch-synonyms-kill-as-org-mode-syntax (word &optional with-heading)
  "Add the synonyms for WORD to the kill ring as `org-mode' syntax.
Will add a list with each item being the synonyms for a meaning to the kill
ring. If WITH-HEADING is non-nil it will add a heading with the WORD as text
and the list of synonyms below."
  (interactive "sWort: \nP")
  (kill-new (cdr-safe
             (woerterbuch--synonyms-retrieve-as-string word with-heading))))

;;;###autoload
(defun woerterbuch-synonyms-insert (word &optional to-kill-ring)
  "Lookup synonyms for WORD and insert selected word at point.
If TO-KILL-RING is non-nil it is added to the kill ring instead."
  (interactive "sWort: \nP")
  (when-let ((synonym (woerterbuch--synonyms-read-synonym word)))
    (if to-kill-ring
        (kill-new synonym)
      (insert synonym))))

;;;###autoload
(defun woerterbuch-synonyms-lookup-word-at-point ()
  "Lookup synonyms for word at point and add to kill ring."
  (interactive)
  (if-let ((word-and-bounds (woerterbuch--get-word-at-point-or-selection))
           (word (car word-and-bounds)))
      (when-let ((synonym (woerterbuch--synonyms-read-synonym word)))
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
      (when-let ((synonym (woerterbuch--synonyms-read-synonym word)))
        (delete-region (car bounds) (cdr bounds))
        (insert synonym))
    (user-error "No word at point")))

(provide 'woerterbuch)

;;; woerterbuch.el ends here
