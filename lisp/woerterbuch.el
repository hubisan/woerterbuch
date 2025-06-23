;;; woerterbuch.el --- Lookup definitions and synonyms for German words -*- lexical-binding: t -*-

;; Copyright (C) 2023 Daniel Hubmann

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/woerterbuch
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: dictionary, thesaurus, convenience

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

;; Lookup definitions and synonyms for German words.

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

;;; Requirements

(require 'seq)
(require 'map)
(require 'thingatpt)
(require 'dom)
(require 'find-func)
(require 'org)
(require 'derived)

;;; Customization

(defgroup woerterbuch nil
  "German dictionary and thesaurus."
  :group 'convenience
  :link '(url-link "https://github.com/hubisan/woerterbuch"))

(defcustom woerterbuch-org-buffer-display-function #'pop-to-buffer
  "Function used to the display the org buffer with the definitions or synonyms.
The function takes buffer as argument.
The function `woerterbuch-display-in-side-window' may be used to show the org
buffer in a side window. Use with `apply-partially' to set the side."
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
- Text of the heading (see also other customization variables)
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

(defcustom woerterbuch-definitions-examples-add nil
  "If non-nil examples for definitions are added.
Use `woerterbuch-definitions-examples-max' to limit the number of examples, it
defaults to 2."
  :type 'integer
  :group 'woerterbuch)

(defcustom woerterbuch-definitions-examples-max 2
  "The maximum number of examples to add for each definition."
  :type 'integer
  :group 'woerterbuch)

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

(defcustom woerterbuch-synonyms-add-synonyms-from-wiktionary nil
  "If non-nil synoyms taken from Wiktionary are added.
This is only the case when using a function that displays the synonyms in an
org-buffer. If reading from the minibuffer the synonyms are not added.
The synonyms are added below those of Openthesaurus. The synonyms are not added
if reading from minibuffer."
  :type 'string)

(defcustom woerterbuch-synonyms-wiktionary-format "\nWiktionary:\n\n%3$s"
  "Format used for the synonyms added from wiktionary.
It is called with the url, the word and the text cleaned."
  :type 'string)

(defcustom woerterbuch-quit-window-key-binding "C-c C-k"
  "Key binding to use for `quit-window' in the woerterbuch buffer.
If set to nil no key binding is set."
  :type 'string)

;;; Major-Mode & Key Bindings

(defvar woerterbuch-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Not needed and makes it possible to not require `org-mode'.
    ;; (set-keymap-parent map org-mode-map)
    (when woerterbuch-quit-window-key-binding
      (define-key map (kbd woerterbuch-quit-window-key-binding) 'quit-window))
    map))

(define-derived-mode woerterbuch-mode org-mode "Woerterbuch"
  "Major mode for displaying woerterbuch buffer.")

;;; Global Variables

(defconst woerterbuch--package-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory)
  "The directory woerterbuch.el is stored in.")

(defconst woerterbuch--org-buffer-name "*woerterbuch*"
  "Name used for the Org buffer showing the defintions or synonyms.")

;;; Auxiliary Functions

(defun woerterbuch--org-add-heading (heading level content)
  "Add text of HEADING with LEVEL as heading before CONTENT."
  (format woerterbuch-insert-org-heading-format
          (make-string level ?*) heading content))

;;   "Insert TEXT on current line if empty else on a new line.
;; If WITH-HEADING is non-nil the text includes an Org heading and is inserted
;; using `org-paste-subtree'. Will paste with the same level as the current
;; heading. If BUFFER is given, insert the text into that buffer instead of the
;; current."

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
            (org-paste-subtree (or (org-current-level) 1) text)
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

(defun woerterbuch--word-at-point-or-selection ()
  "Get the word at point or the selection if region is active.
Returns a cons cell with the car being the word and cdr the bounds."
  (if-let* ((bounds (if (use-region-p)
                        (cons (region-beginning) (region-end))
                      (bounds-of-thing-at-point 'word))))
      (cons
       (buffer-substring-no-properties (car bounds) (cdr bounds))
       bounds)
    (user-error "No word at point and no region active")))

(defun woerterbuch--read-file-contents (path)
  "Return the contents of file at PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun woerterbuch-display-in-side-window (side width buffer)
  "Display BUFFER in side window on SIDE specified and select it.
Specify WIDTH and HEIGHT or set em to nil to not change it manually."
  (let* ((alist (list (cons 'side side)))
         (alist (if width
                    (append alist (list (cons 'window-width width)))
                  alist)))
    (select-window
     (display-buffer-in-side-window buffer alist))))

;;; Definitions

(defconst woerterbuch--definitions-dwds-url "https://www.dwds.de/wb/%s"
  "Url to retrieve the definitions for a word as html from DWDS.")

(defun woerterbuch--definitions-retrieve-examples (leseart)
  "Retrieve the examples for LESEART."
  (when-let* ((dom (dom-children
                    (car (dom-by-class
                          leseart
                          "^dwdswb-lesart-content$"))))
              (examples-dom (catch 'result
                              (dolist (element dom)
                                (cond
                                 ((string= (dom-attr element 'class)
                                           "dwdswb-verwendungsbeispiele")
                                  (throw 'result element))
                                 ((string= (dom-attr element 'class)
                                           "dwdswb-lesart")
                                  (throw 'result nil))))))
              (examples-texts (mapcar #'dom-texts
                                      (dom-by-class examples-dom
                                                    "^dwdswb-belegtext$"))))
    (if woerterbuch-definitions-examples-max
        (take woerterbuch-definitions-examples-max examples-texts)
      examples-texts)))

(defun woerterbuch--definitions-retrieve-raw (word &optional add-examples)
  "Return a raw list of definitions for WORD.
Each element is a cons with the car being the id of the defintion and the cadr
the text. Gets the definition from URL `https://www.dwds.de.'
If ADD-EXAMPLES is non-nil add the examples as well. Then the cadr is a cons
cell with the car being the meaning and its cadr the examples.
TODO Refactor this, should be separated into multiple functions."
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
                         (let* ((id (dom-attr leseart 'id))
                                ;; So ging es nicht, z. B. mit Wort kirre
                                ;; "^dwdswb-definition$". Und dies ging nicht mit
                                ;; jmdn. auf dem Kieker haben
                                ;; "^dwdswb-definitionen$"
                                (text (dom-texts (car (dom-by-class
                                                       leseart
                                                       "^dwdswb-lesart-def$"))))
                                ;; Empty string if there is none.
                                (text (or text ""))
                                (examples
                                 (when add-examples
                                   (woerterbuch--definitions-retrieve-examples
                                    leseart))))
                           (when (and (stringp id) (stringp text))
                             (if (and examples (listp examples))
                                 (cons id (list text examples))
                               (cons id text)))))
                       lesearten)))
          (kill-buffer buffer))))))

(defun woerterbuch--definitions-word-baseform (word &optional raw-synonyms)
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
             (content (cdr definition))
             ;; Get the text and clean it.
             (text (woerterbuch--definitions-clean-text
                    (if (listp content) (car content) content)))
             (examples (when (listp content)
                         (mapcar 'woerterbuch--definitions-clean-text
                                 (cadr content)))))
        (cond
         ;; If it is the first definition or the same level as before.
         ((or (not previous-id) (length= previous-id (length id)))
          ;; Add the defintion to definitions.
          (if examples
              (push (list :definition text :examples examples) definitions)
            (push (list :definition text) definitions))
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
            (let ((parent-definition (pop definitions)))
              (push (plist-put parent-definition :children
                               (woerterbuch--definitions-to-list children))
                    definitions)))))))
    (when definitions
      (nreverse definitions))))

(defun woerterbuch--definitions-clean-text (text)
  "Clean the TEXT of the definitions."
  (when-let*
      ((text-new-lines-removed
        (replace-regexp-in-string "\n" " " text))
       (text-trimmed (string-trim text-new-lines-removed))
       ;; Remove those targets placed after links to other defintions. Either a
       ;; number, a letter or dot symbol ● in parentheses like (1), (2), (●). Or
       ;; actually multiple of those if it is nested like (1 b). A good example
       ;; is if getting the definitions for Katze or Wurst.
       (text-link-targets-removed
        (replace-regexp-in-string "([●[:alnum:]])\\|(\\(?:[●[:alnum:]] \\)+)"
                                  "" text-trimmed))
       ;; If a word has more than one tab a superscript is used in links.
       ;; For instance in the definitions for word Wurst.
       (text-superscripts-removed
        (replace-regexp-in-string "[⁰¹²³⁴⁵⁶⁷⁸⁹]" "" text-link-targets-removed))
       ;; Change all consecutive spaces into one space.
       (text-multiple-spaces-removed
        (replace-regexp-in-string "[[:blank:]]+" " "
                                  text-superscripts-removed))
       ;; Replace strange parentheses with real ones.
       (text-strange-parens-changed
        (replace-regexp-in-string
         "⟩" ")" (replace-regexp-in-string "⟨" "(" text-multiple-spaces-removed)))
       (text-paren-and-space-removed
        ;; Remove after starting paren or before closing paren.
        (replace-regexp-in-string "\\(?1:(\\) \\| \\(?1:)\\)" "\\1"
                                  text-strange-parens-changed))
       ;; Sometimes there are spaces before commas.
       (text-space-before-comma-removed
        (replace-regexp-in-string " ," ","
                                  text-paren-and-space-removed)))
    text-space-before-comma-removed))

(defun woerterbuch--definitions-retrieve-as-list (word)
  "Retrieve the definitions for WORD as a list.
If INCLUDE-EXAMPLES is non-nil the examples are also returned.
Each list consist of one or multiple definitions (meanings) of a word. Each
definition can a list of hold subdefinitions. Returns a cons with car being the
word and cdr the definitions. The word is returned as it can differntiate from
the WORD used as parameter when a baseform is used to retrieve the definitions.
When INCLUDE-EXAMPLES is non-nil then each definition is a cons cell with the
car being the definition and the cdr the examples.
Returns nil if no definition was found."
  (let* ((baseform (woerterbuch--definitions-word-baseform word))
         (raw-definitions (woerterbuch--definitions-retrieve-raw
                           baseform woerterbuch-definitions-examples-add)))
    (let ((definitions (woerterbuch--definitions-to-list raw-definitions)))
      (cons baseform definitions))))

(defun woerterbuch--definitions-to-string (definitions &optional lvl)
  "Convert the list of DEFINITIONS to a string.
LVL is used when the function is called recursively to process the children.
The list bullet point can be configured with `woerterbuch-list-bullet-point'."
  (let ((lvl (or lvl 0)))
    (mapconcat
     (lambda (definition)
       (let* ((examples (woerterbuch--definitions-examples-to-string
                         definition lvl))
              ;; If examples are added make the meaning be bold.
              (definition-text (plist-get definition :definition))
              (text-format (if (and examples
                                    (not (string-empty-p definition-text)))
                               "%s%s *%s*"
                             "%s%s %s"))
              (text (format text-format
                            (make-string (* 2 lvl) ? )
                            woerterbuch-list-bullet-point
                            (plist-get definition :definition)))
              (text (if examples
                        (concat text "\n" examples)
                      text))
              (children (plist-get definition :children)))
         (if children
             ;; Call function again with children.
             (format
              "%s\n%s" text
              (woerterbuch--definitions-to-string children (1+ lvl)))
           text)))
     definitions "\n")))

(defun woerterbuch--definitions-examples-to-string (definition lvl)
  "Convert the examples in DEFINITION to a string.
If no examples exist nil is returned.
LVL is used when the function is called recursively to process the children."
  (when-let*((examples (plist-get definition :examples))
             (heading (format "%s%s Beispiele"
                              (make-string (+ 2 (* 2 lvl)) ? )
                              woerterbuch-list-bullet-point)))
    (concat heading
            "\n"
            (mapconcat (lambda (example)
                         (format "%s%s %s"
                                 (make-string (+ 4 (* 2 lvl)) ? )
                                 woerterbuch-list-bullet-point
                                 example))
                       examples "\n"))))

(defun woerterbuch--definitions-retrieve-as-string (word &optional with-heading)
  "Retrieve the definitions for WORD as a string.
Returns a cons with car being the WORD and cdr the definitions as string.
The car will be the baseform if the WORD was not a baseform.
If no definitions are found it inserts a link to the dwds page as string.
If WITH-HEADING is non-nil a heading with the WORD as text is added above the
definitions."
  (let* ((word-and-definitions (woerterbuch--definitions-retrieve-as-list word))
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
  (if-let* ((word-and-bounds (woerterbuch--word-at-point-or-selection))
            (word (car word-and-bounds)))
      (woerterbuch-definitions-show-in-org-buffer word)
    (user-error "No word at point")))

;;;###autoload
(defun woerterbuch-definitions-insert-into-org-buffer (word &optional with-heading)
  "Insert the definitions for WORD into the current `org-mode' buffer.
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

;;; Synonyms

(defconst woerterbuch--synonyms-openthesaurus-url
  "https://www.openthesaurus.de/synonyme/%s"
  "Url to link to a synonyms page.")

(defconst woerterbuch--synonyms-openthesaurus-api-url
  (concat
   "https://www.openthesaurus.de/synonyme/search?q=%s"
   "&format=application/json"
   "&baseform=true")
  "Url to retrieve the synonyms for a word as JSON from openthesaurus.")

(defconst woerterbuch--synonyms-wiktionary-url
  "https://de.wiktionary.org/wiki/%s"
  "Url to retrieve the synonyms from Wiktionary.")

(defun woerterbuch--synonyms-retrieve-raw (word)
  "Return the synonyms for a WORD as plist as retrieved with the API."
  (let* ((url (format woerterbuch--synonyms-openthesaurus-api-url
                      (url-hexify-string (string-trim word))))
         (buffer (url-retrieve-synchronously url t)))
    (when buffer
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

(defun woerterbuch--synonyms-clean-text (synonyms)
  "Clean the text of each synonym in the list of SYNONYMS.
Synonyms sometimes contains additional information in parentheses. That
information should be stripped when reading from minibuffer."
  (mapcar
   (lambda (synonyms-group)
     (mapcar (lambda (synonym)
               ;; Sometimes it has additional information in brackets for the
               ;; synonym.
               (replace-regexp-in-string " ?(.*?) ?" "" synonym))
             synonyms-group))
   synonyms))

(defun woerterbuch--synonyms-retrieve-as-list (word &optional clean)
  "Retrieve the synonyms for WORD as a list of lists.
Each list consist of the synonyms for one meaning of the word.
Returns a cons with car being the word and cdr the synonyms. The
word is returned as it can differntiate from the WORD used as
parameter when a baseform is used to retrieve the synonyms.
Returns nil if no synonyms are retrieved
If CLEAN is non-nil `woerterbuch--synonyms-clean-text' is called to clean the
synonyms. Additional information in parantheses is removed."
  (let* ((raw-synonyms (woerterbuch--synonyms-retrieve-raw word))
         (baseform (when raw-synonyms
                     (woerterbuch--synonyms-baseform raw-synonyms))))
    ;; If a baseform was found use that to retrieve the synonyms.
    (when baseform
      (setq raw-synonyms (woerterbuch--synonyms-retrieve-raw baseform)))
    (when raw-synonyms
      (let* ((synonyms (woerterbuch--synonyms-to-list raw-synonyms))
             (synonyms (if clean
                           (woerterbuch--synonyms-clean-text synonyms)
                         synonyms)))
        (cons (or baseform word) synonyms)))))

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
                         (or word-used word))))
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
  (if-let* ((word-and-synonyms (woerterbuch--synonyms-retrieve-as-list word t))
            (word-used (car-safe word-and-synonyms))
            (synonyms (cdr-safe word-and-synonyms)))
      (when-let*((synonyms-flattened (apply #'append synonyms))
                 (synonyms-no-duplicates (seq-uniq synonyms-flattened))
                 (synonyms-sorted (seq-sort #'string-lessp
                                            synonyms-no-duplicates)))
        (completing-read "Select synonym: " synonyms-sorted nil t))
    (user-error "No synonyms found for %s" word)))

(defun woerterbuch--synonyms-wiktionary-retrieve-as-string (word)
  "Get a list of synonyms from wiktionary for WORD in `org-mode' syntax.
The WORD needs to be in baseform."
  (let* ((url (format woerterbuch--synonyms-wiktionary-url
                      (url-hexify-string (string-trim word))))
         (buffer (url-retrieve-synchronously url t)))
    (when buffer
      (with-current-buffer buffer
        (unwind-protect
            (progn
              (set-buffer-multibyte t)
              (goto-char (point-min))
              (when-let*
                  ((found (re-search-forward
                           "\\(>Synonyme:</p>\\|>Sinnverwandte Wörter:</p>\\)"
                           nil t))
                   (start (1+ found))
                   (end (search-forward "</dl>"))
                   (dom (libxml-parse-html-region start end))
                   (text (dom-texts dom))
                   ;; Change the leading [1] to - for org-mode.
                   (text-cleaned
                    (replace-regexp-in-string "\\[[^]]+]" "-" text))
                   ;; Replace spaces with one space.
                   (text-cleaned
                    (replace-regexp-in-string " +" " " text-cleaned))
                   ;; Remove space before punctuation.
                   (text-cleaned
                    (replace-regexp-in-string "\\( \\)[,:;.]" "" text-cleaned
                                              nil nil 1))
                   ;; Remove space at end of line.
                   (text-cleaned
                    (replace-regexp-in-string " $" "" text-cleaned))
                   ;; Remove remarks with Siehe auch
                   (text-cleaned
                    (replace-regexp-in-string
                     "\\(; siehe auch:.*;\\|; siehe auch:.*$\\)" ""
                     text-cleaned))
                   ;; Second line and following have a space at the beginning.
                   (text-cleaned (replace-regexp-in-string "^ -" "-"
                                                           text-cleaned))
                   ;; Add spaces at the beginning if not starting with -.
                   (text-cleaned (replace-regexp-in-string "^[^-]" "  "
                                                           text-cleaned)))
                (format woerterbuch-synonyms-wiktionary-format
                        url word text-cleaned)))
          (kill-buffer buffer))))))

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
  (if-let* ((word-and-bounds (woerterbuch--word-at-point-or-selection))
            (word (car word-and-bounds)))
      (woerterbuch-synonyms-show-in-org-buffer word)
    (user-error "No word at point")))

;;;###autoload
(defun woerterbuch-synonyms-insert-into-org-buffer (word &optional with-heading)
  "Insert the synonyms for WORD into the current `org-mode' buffer.
Will insert a list with each item being the synonyms for a meaning. If
WITH-HEADING is non-nil it inserts a heading with the WORD as text before the
synonyms. If there are no synonyms found it will insert a text with a link to
openthesaurus."
  (interactive "sWort: \nP")
  (let* ((word-and-synonyms (woerterbuch--synonyms-retrieve-as-string
                             word with-heading))
         (word-used (car-safe word-and-synonyms))
         (synonyms (cdr-safe word-and-synonyms)))
    (when woerterbuch-synonyms-add-synonyms-from-wiktionary
      (when-let* ((wiki-synonyms
                   (woerterbuch--synonyms-wiktionary-retrieve-as-string
                    word-used)))
        (setq synonyms (concat synonyms wiki-synonyms))))
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
  (when-let*((synonym (woerterbuch--synonyms-read-synonym word)))
    (if to-kill-ring
        (kill-new synonym)
      (insert synonym))))

;;;###autoload
(defun woerterbuch-synonyms-lookup-word-at-point ()
  "Lookup synonyms for word at point and add to kill ring."
  (interactive)
  (if-let* ((word-and-bounds (woerterbuch--word-at-point-or-selection))
            (word (car word-and-bounds)))
      (when-let*((synonym (woerterbuch--synonyms-read-synonym word)))
        (kill-new synonym)
        synonym)
    (user-error "No word at point")))

;;;###autoload
(defun woerterbuch-synonyms-replace-word-at-point ()
  "Lookup synonyms for wort at point or selection and replace it."
  (interactive)
  (if-let* ((word-and-bounds (woerterbuch--word-at-point-or-selection))
            (word (car word-and-bounds))
            (bounds (cdr word-and-bounds)))
      (when-let*((synonym (woerterbuch--synonyms-read-synonym word)))
        (delete-region (car bounds) (cdr bounds))
        (insert synonym))
    (user-error "No word at point")))

;;; Synonyms & Definitions

;;;###autoload
(defun woerterbuch-definitions-and-synonyms-show-in-org-buffer (&optional word)
  "Show both the definitions and synonyms for WORD in an `org-mode' buffer.
Returns the buffer."
  (interactive "sWort: ")
  (let ((buffer (get-buffer-create woerterbuch--org-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer))
    (funcall woerterbuch-org-buffer-display-function buffer)
    (with-current-buffer buffer
      (woerterbuch-mode)
      (woerterbuch-definitions-and-synonyms-insert-into-org-buffer word)
      buffer)))

;;;###autoload
(defun woerterbuch-definitions-and-synonyms-show-in-org-buffer-for-word-at-point ()
  "Show both the definitions and synonyms for the word at point in an org buffer.
Returns the buffer."
  (interactive)
  (if-let* ((word-and-bounds (woerterbuch--word-at-point-or-selection))
            (word (car word-and-bounds)))
      (woerterbuch-definitions-and-synonyms-show-in-org-buffer word)
    (user-error "No word at point")))

;;;###autoload
(defun woerterbuch-definitions-and-synonyms-insert-into-org-buffer (&optional word)
  "Insert both the definitions and synonyms for WORD in the current buffer.
Checks if the current buffer is in `org-mode' or `woerterbuch-mode'."
  (interactive "sWort: ")
  (if (or (eq major-mode 'org-mode)
          (eq major-mode 'woerterbuch-mode))
      (let (text)
        (with-temp-buffer
          (insert (format "* %s\n\n" word))
          (insert "** Bedeutungen\n\n")
          (woerterbuch-definitions-insert-into-org-buffer word)
          (goto-char (point-max))
          (insert "\n** Synonyme\n\n")
          (woerterbuch-synonyms-insert-into-org-buffer word)
          (goto-char (point-max))
          (setq text (buffer-string)))
        (save-excursion
          (org-paste-subtree (or (org-current-level) 1) text)))
    (user-error "Text can only be inserted in an Org buffer")))

(provide 'woerterbuch)

;;; woerterbuch.el ends here
