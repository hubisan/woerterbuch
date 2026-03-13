;;; woerterbuch-dwds.el --- Parse DWDS HTML into nested Elisp data -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: OpenAI
;; Keywords: data, html, dictionary, tools
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This library fetches and parses DWDS article pages.
;;
;; The parser focuses on three sections:
;;
;; - meanings
;; - examples attached to meanings
;; - etymology
;;
;; Returned values are plain property lists. Meaning nodes are nested via
;; `:children'. Examples are stored separately in `:examples'.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)

(defgroup woerterbuch-dwds nil
  "Parse DWDS HTML into nested Elisp data."
  :group 'applications
  :prefix "woerterbuch-dwds-")

(defcustom woerterbuch-dwds-base-url "https://www.dwds.de/wb/"
  "Base URL for DWDS article pages."
  :type 'string
  :group 'woerterbuch-dwds)

(defcustom woerterbuch-dwds-user-agent
  "Mozilla/5.0 (compatible; Emacs woerterbuch-dwds)"
  "User-Agent header used for HTTP requests to DWDS."
  :type 'string
  :group 'woerterbuch-dwds)

(defcustom woerterbuch-dwds-request-timeout 15
  "Maximum number of seconds to wait for a DWDS response."
  :type 'integer
  :group 'woerterbuch-dwds)

(defcustom woerterbuch-dwds-include-empty-sections nil
  "When non-nil, include empty `:meanings' and `:etymology' lists."
  :type 'boolean
  :group 'woerterbuch-dwds)

(defcustom woerterbuch-dwds-keep-raw-ids t
  "When non-nil, preserve DWDS raw ids such as"
  :type 'boolean
  :group 'woerterbuch-dwds)

(defun woerterbuch-dwds-parse-file (file)
  "Parse local DWDS HTML FILE and return structured data."
  (with-temp-buffer
    (insert-file-contents file)
    (woerterbuch-dwds-parse-string (buffer-string))))

(defun woerterbuch-dwds-parse-url (url)
  "Fetch URL and parse the returned DWDS article HTML."
  (woerterbuch-dwds-parse-string
   (woerterbuch-dwds-fetch-url url)))

(defun woerterbuch-dwds-parse-lemma (lemma)
  "Fetch and parse the DWDS article page for LEMMA."
  (woerterbuch-dwds-parse-url
   (woerterbuch-dwds-lemma-url lemma)))

(defun woerterbuch-dwds-parse-string (html)
  "Parse DWDS HTML from HTML and return a plist.

The return value contains at least `:lemma'. Depending on the article, it may
also contain `:meanings' and `:etymology'."
  (with-temp-buffer
    (insert html)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (lemma (woerterbuch-dwds--extract-lemma dom))
           (meanings (woerterbuch-dwds--extract-meanings dom))
           (etymology (woerterbuch-dwds--extract-etymology dom))
           (result (list :lemma lemma)))
      (when (or meanings woerterbuch-dwds-include-empty-sections)
        (setq result (plist-put result :meanings meanings)))
      (when (or etymology woerterbuch-dwds-include-empty-sections)
        (setq result (plist-put result :etymology etymology)))
      result)))

(defun woerterbuch-dwds-fetch-url (url)
  "Fetch URL and return the decoded HTML body as a string."
  (let ((url-request-extra-headers
         `(("User-Agent" . ,woerterbuch-dwds-user-agent)
           ("Accept" . "text/html,application/xhtml+xml"))))
    (with-current-buffer
        (or (url-retrieve-synchronously
             url t t woerterbuch-dwds-request-timeout)
            (error "Failed to retrieve URL: %s" url))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (when (re-search-forward "^HTTP/[0-9.]+ \([0-9]+\)" nil t)
              (let ((status (string-to-number (match-string 1))))
                (unless (<= 200 status 299)
                  (error "DWDS request failed with HTTP status %d for %s"
                         status url))))
            (unless (re-search-forward "?
?
" nil t)
              (error "Could not find HTTP headers end for %s" url))
            (let* ((coding (or (and (boundp 'url-http-content-type)
                                    (woerterbuch-dwds--charset-from-content-type
                                     url-http-content-type))
                               'utf-8))
                   (html (decode-coding-string
                          (buffer-substring-no-properties (point) (point-max))
                          coding t)))
              (when (string-empty-p (string-trim html))
                (error "DWDS returned an empty response for %s" url))
              html))
        (kill-buffer (current-buffer))))))

(defun woerterbuch-dwds-lemma-url (lemma)
  "Return the DWDS article URL for LEMMA."
  (concat woerterbuch-dwds-base-url
          (url-hexify-string lemma)))

(defun woerterbuch-dwds--charset-from-content-type (content-type)
  "Extract and intern the charset from CONTENT-TYPE."
  (when (and content-type
             (string-match "charset=\([^;[:space:]]+\)" content-type))
    (let* ((charset (downcase (match-string 1 content-type)))
           (coding (intern charset)))
      (cond
       ((coding-system-p coding) coding)
       ((member charset '("utf8" "utf-8")) 'utf-8)
       (t nil)))))

(defun woerterbuch-dwds--extract-lemma (dom)
  "Extract the article lemma from DOM."
  (or (woerterbuch-dwds--text-of-first-by-class dom "dwdswb-stichwort")
      (woerterbuch-dwds--text-of-first-by-class dom "dwdswb-ft-lemmaansatz")
      (let ((title (woerterbuch-dwds--text-of-first-tag dom 'title)))
        (when (and title (string-match "\`\([^–]+\) –" title))
          (string-trim (match-string 1 title))))))

(defun woerterbuch-dwds--extract-meanings (dom)
  "Extract top-level meaning nodes from DOM."
  (let ((container (car (dom-by-class dom "dwdswb-lesarten"))))
    (when container
      (mapcar #'woerterbuch-dwds--parse-lesart
              (woerterbuch-dwds--direct-child-elements-by-class
               container "dwdswb-lesart")))))

(defun woerterbuch-dwds--parse-lesart (node)
  "Parse one DWDS meaning NODE into a nested plist."
  (let* ((raw-id (dom-attr node 'id))
         (label-node (woerterbuch-dwds--first-direct-child-by-class node "dwdswb-lesart-n"))
         (content-node (woerterbuch-dwds--first-direct-child-by-class node "dwdswb-lesart-content"))
         (label (woerterbuch-dwds--node-text label-node))
         (text (woerterbuch-dwds--extract-lesart-definition content-node))
         (type (woerterbuch-dwds--detect-lesart-type content-node label text))
         (examples (woerterbuch-dwds--extract-examples content-node))
         (children (mapcar #'woerterbuch-dwds--parse-lesart
                           (woerterbuch-dwds--direct-child-elements-by-class
                            content-node "dwdswb-lesart")))
         (usage (woerterbuch-dwds--extract-diasystematics content-node))
         (grammar (woerterbuch-dwds--extract-grammar content-node))
         (result (list :id (woerterbuch-dwds--node-id raw-id)
                       :type type
                       :label (woerterbuch-dwds--nil-if-empty label)
                       :text (woerterbuch-dwds--nil-if-empty text)
                       :examples examples
                       :children children)))
    (when (and raw-id woerterbuch-dwds-keep-raw-ids)
      (setq result (plist-put result :raw-id raw-id)))
    (when usage
      (setq result (plist-put result :usage usage)))
    (when grammar
      (setq result (plist-put result :grammar grammar)))
    result))

(defun woerterbuch-dwds--detect-lesart-type (content-node label text)
  "Return the semantic type for CONTENT-NODE, LABEL, and TEXT."
  (cond
   ((woerterbuch-dwds--has-direct-child-by-class-p content-node "dwdswb-phraseme")
    'phrase)
   ((and label (string-match-p "●" label))
    'subsense)
   ((and text (not (string-empty-p text)))
    'meaning)
   (t 'node)))

(defun woerterbuch-dwds--extract-lesart-definition (content-node)
  "Extract the definition text from CONTENT-NODE."
  (let ((def-node (woerterbuch-dwds--first-direct-child-by-class
                   content-node "dwdswb-lesart-def")))
    (woerterbuch-dwds--node-text def-node)))

(defun woerterbuch-dwds--extract-examples (node)
  "Extract examples from NODE.

The return value is a list of plists with `:type' and `:text'."
  (let ((result nil))
    (when (listp node)
      (dolist (block (dom-by-class node "dwdswb-verwendungsbeispiele"))
        (setq result
              (nconc result
                     (woerterbuch-dwds--extract-examples-from-block block)))))
    result))

(defun woerterbuch-dwds--extract-examples-from-block (block)
  "Extract examples from one example BLOCK."
  (let ((result nil))
    (dolist (child (dom-children block))
      (when (listp child)
        (cond
         ((woerterbuch-dwds--node-has-class-p child "dwdswb-kompetenzbeispiel")
          (let ((example (woerterbuch-dwds--parse-kompetenzbeispiel child)))
            (when example
              (push example result))))
         ((woerterbuch-dwds--node-has-class-p child "dwdswb-beleg")
          (let ((example (woerterbuch-dwds--parse-beleg child)))
            (when example
              (push example result)))))))
    (nreverse result)))

(defun woerterbuch-dwds--parse-kompetenzbeispiel (node)
  "Parse one competence example NODE."
  (let ((text (woerterbuch-dwds--text-of-first-by-class node "dwdswb-belegtext"))
        (usage (woerterbuch-dwds--extract-diasystematics node)))
    (when (not (string-empty-p (or text "")))
      (let ((result (list :type 'example
                          :text text)))
        (when usage
          (setq result (plist-put result :usage usage)))
        result))))

(defun woerterbuch-dwds--parse-beleg (node)
  "Parse one citation example NODE."
  (let* ((text (or (woerterbuch-dwds--text-of-first-by-class node "dwdswb-belegtext")
                   (woerterbuch-dwds--node-text node)))
         (source (woerterbuch-dwds--text-of-first-by-class node "dwdswb-fundstelle"))
         (usage (woerterbuch-dwds--extract-diasystematics node)))
    (when (not (string-empty-p (or text "")))
      (let ((result (list :type 'citation-example
                          :text text)))
        (when (not (string-empty-p (or source "")))
          (setq result (plist-put result :source source)))
        (when usage
          (setq result (plist-put result :usage usage)))
        result))))

(defun woerterbuch-dwds--extract-etymology (dom)
  "Extract the etymology section from DOM."
  (let* ((wrapper (car (dom-by-class dom "etymwb-wrapper")))
         (details (and wrapper (car (dom-by-class wrapper "details"))))
         (summary (and wrapper (car (dom-by-class wrapper "summary"))))
         (entry (or (and details (car (dom-by-class details "etymwb-entry")))
                    (and summary (car (dom-by-class summary "etymwb-entry")))
                    (and wrapper (car (dom-by-class wrapper "etymwb-entry")))))
         (text (woerterbuch-dwds--node-text entry))
         (heading (car (dom-by-id dom "etymwb-1"))))
    (when (not (string-empty-p (or text "")))
      (list (list :id (or (and heading (dom-attr heading 'id)) "etymwb-1")
                  :type 'etymology
                  :text text)))))

(defun woerterbuch-dwds--extract-diasystematics (node)
  "Extract diasystematic labels from NODE."
  (let ((result nil))
    (when (listp node)
      (dolist (diasystematic (dom-by-class node "dwdswb-diasystematik"))
        (dolist (child (dom-children diasystematic))
          (when (listp child)
            (let ((text (woerterbuch-dwds--node-text child)))
              (unless (string-empty-p text)
                (push text result)))))))
    (nreverse (delete-dups result))))

(defun woerterbuch-dwds--extract-grammar (node)
  "Extract grammar text from NODE or return nil."
  (let ((text (woerterbuch-dwds--text-of-first-by-class node "dwdswb-ft-la")))
    (woerterbuch-dwds--nil-if-empty text)))

(defun woerterbuch-dwds--text-of-first-by-class (node class)
  "Return normalized text of the first descendant of NODE with CLASS."
  (let ((match (and (listp node) (car (dom-by-class node class)))))
    (woerterbuch-dwds--node-text match)))

(defun woerterbuch-dwds--text-of-first-tag (node tag)
  "Return normalized text of the first descendant of NODE with TAG."
  (let ((match (and (listp node) (car (dom-by-tag node tag)))))
    (woerterbuch-dwds--node-text match)))

(defun woerterbuch-dwds--node-id (raw-id)
  "Return the preferred id for RAW-ID."
  (or raw-id
      (format "woerterbuch-dwds-node-%s" (cl-gensym))))

(defun woerterbuch-dwds--node-text (node)
  "Return normalized text content for NODE."
  (if (null node)
      ""
    (let* ((texts (dom-texts node))
           (raw-text (cond
                      ((stringp texts) texts)
                      ((listp texts) (mapconcat #'identity texts " "))
                      (t (format "%s" texts)))))
      (replace-regexp-in-string
       "[[:space:]]+" " "
       (string-trim raw-text)))))

(defun woerterbuch-dwds--nil-if-empty (string)
  "Return STRING unless it is nil or empty."
  (unless (string-empty-p (or string ""))
    string))

(defun woerterbuch-dwds--node-has-class-p (node class)
  "Return non-nil when NODE has CSS CLASS."
  (and (listp node)
       (member class
               (split-string (or (dom-attr node 'class) "") "[[:space:]]+" t))))

(defun woerterbuch-dwds--first-direct-child-by-class (node class)
  "Return the first direct child of NODE with CSS CLASS."
  (cl-find-if
   (lambda (child)
     (and (listp child)
          (woerterbuch-dwds--node-has-class-p child class)))
   (and (listp node) (dom-children node))))

(defun woerterbuch-dwds--has-direct-child-by-class-p (node class)
  "Return non-nil when NODE has a direct child with CLASS."
  (and (woerterbuch-dwds--first-direct-child-by-class node class) t))

(defun woerterbuch-dwds--direct-child-elements-by-class (node class)
  "Return all direct child elements of NODE with CSS CLASS."
  (let ((result nil))
    (dolist (child (and (listp node) (dom-children node)))
      (when (and (listp child)
                 (woerterbuch-dwds--node-has-class-p child class))
        (push child result)))
    (nreverse result)))

(provide 'woerterbuch-dwds)

;;; woerterbuch-dwds.el ends here
