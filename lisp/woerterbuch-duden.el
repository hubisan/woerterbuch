;;; duden-scrape.el --- Scrape Duden entries with url-retrieve -*- lexical-binding: t; -*-

(require 'url)
(require 'dom)
(require 'subr-x)
(require 'cl-lib)
(require 'json)

(defgroup duden-scrape nil
  "Extract structured dictionary data from Duden article pages."
  :group 'applications)

(defcustom duden-scrape-base-url "https://www.duden.de/rechtschreibung/"
  "Base URL for Duden headword pages."
  :type 'string)

(defcustom duden-scrape-user-agent
  "Mozilla/5.0 (X11; Linux x86_64) Emacs duden-scrape/0.3"
  "User-Agent header used for HTTP requests."
  :type 'string)

(defcustom duden-scrape-prefer-amp t
  "When non-nil, try the AMP page first and use the regular page as fallback."
  :type 'boolean)

(defcustom duden-scrape-timeout 30
  "Timeout in seconds for synchronous HTTP requests."
  :type 'integer)

(defun duden--normalize-space (string)
  "Normalize whitespace in STRING and trim surrounding space."
  (when string
    (string-trim
     (replace-regexp-in-string
      "[ \t\n\r]+" " "
      (replace-regexp-in-string "[[:space:]]+" " " string)))))

(defun duden--node-text (node)
  "Return the recursively concatenated text content of NODE."
  (cond
   ((null node) "")
   ((stringp node) node)
   ((listp node)
    (mapconcat #'duden--node-text (dom-children node) " "))
   (t "")))

(defun duden--text (node)
  "Return normalized text content of NODE."
  (duden--normalize-space (duden--node-text node)))

(defun duden--element-p (node)
  "Return non-nil if NODE is a DOM element node."
  (and (listp node) (symbolp (car node))))

(defun duden--attr (node attr)
  "Return attribute ATTR from DOM NODE."
  (dom-attr node attr))

(defun duden--class-list (node)
  "Return the list of CSS classes assigned to NODE."
  (split-string (or (duden--attr node 'class) "") "[[:space:]]+" t))

(defun duden--has-class-p (node class)
  "Return non-nil if NODE has CSS CLASS."
  (member class (duden--class-list node)))

(defun duden--find-first (predicate node)
  "Return the first descendant of NODE for which PREDICATE returns non-nil."
  (cond
   ((null node) nil)
   ((and (duden--element-p node) (funcall predicate node)) node)
   ((duden--element-p node)
    (cl-loop for child in (dom-children node)
             for hit = (duden--find-first predicate child)
             when hit return hit))
   (t nil)))

(defun duden--find-all (predicate node)
  "Return all descendants of NODE for which PREDICATE returns non-nil."
  (let (result)
    (cl-labels ((walk (current)
                  (when (duden--element-p current)
                    (when (funcall predicate current)
                      (push current result))
                    (dolist (child (dom-children current))
                      (walk child)))))
      (walk node))
    (nreverse result)))

(defun duden--by-id (dom id)
  "Return the first element in DOM whose id attribute equals ID."
  (duden--find-first
   (lambda (node) (equal (duden--attr node 'id) id))
   dom))

(defun duden--direct-children (node tag class)
  "Return direct children of NODE matching TAG and optional CLASS.

If CLASS is nil, only TAG is checked."
  (cl-remove-if-not
   (lambda (child)
     (and (duden--element-p child)
          (eq (dom-tag child) tag)
          (or (null class) (duden--has-class-p child class))))
   (dom-children node)))

(defun duden--first-direct-child (node tag class)
  "Return the first direct child of NODE matching TAG and optional CLASS."
  (car (duden--direct-children node tag class)))

(defun duden--tuple-alist (node)
  "Extract direct tuple fields from NODE as an alist.

The result has the form:

  ((\"Usage\" . \"colloquial\") (\"Grammar\" . \"without plural\"))

using the original visible labels from the page."
  (let ((pairs nil))
    (dolist (dl (duden--direct-children node 'dl "tuple"))
      (let* ((dt (duden--first-direct-child dl 'dt nil))
             (dd (duden--first-direct-child dl 'dd nil))
             (key (duden--text dt))
             (value (duden--text dd)))
        (when (and (not (string-empty-p key))
                   (not (string-empty-p value)))
          (push (cons key value) pairs))))
    (nreverse pairs)))

(defun duden--notes-alist (node)
  "Extract direct note blocks from NODE as an alist.

The result has the form:

  ((\"Examples\" . (\"...\"))
   (\"Idioms, phrases, proverbs\" . (\"...\" \"...\")))

using the original visible labels from the page."
  (let ((pairs nil))
    (dolist (dl (duden--direct-children node 'dl "note"))
      (let* ((dt (duden--first-direct-child dl 'dt nil))
             (dd (duden--first-direct-child dl 'dd nil))
             (key (duden--text dt))
             (items (duden--find-all (lambda (n) (eq (dom-tag n) 'li)) dd))
             (values (delq nil
                           (mapcar
                            (lambda (li)
                              (let ((text (duden--text li)))
                                (unless (string-empty-p text) text)))
                            items))))
        (when (and (not (string-empty-p key)) values)
          (push (cons key values) pairs))))
    (nreverse pairs)))

(defun duden--parse-id (raw-id)
  "Parse RAW-ID such as \"Bedeutung-2a\" into a plist.

The returned plist has keys :id and :parent.

Examples:

  \"Bedeutung-3\"  => (:id \"3\" :parent nil)
  \"Bedeutung-2a\" => (:id \"a\" :parent \"2\")"
  (cond
   ((null raw-id)
    (list :id nil :parent nil))
   ((string-match "\\`Bedeutung-\\([0-9]+\\)\\([a-z]\\)?\\'" raw-id)
    (let ((number (match-string 1 raw-id))
          (sub-id (match-string 2 raw-id)))
      (if sub-id
          (list :id sub-id :parent number)
        (list :id number :parent nil))))
   (t
    (list :id raw-id :parent nil))))

(defun duden--extract-sense-from-node (node)
  "Extract a single sense object from NODE.

Only a direct child `div.enumeration__text' counts as the actual sense text.
This avoids accidentally pulling text from nested sub-senses.

The returned plist uses English keys:

  :id
  :parent
  :text
  :usage
  :grammar
  :examples
  :idioms
  :raw-tuples
  :raw-notes"
  (let* ((text-node (duden--first-direct-child node 'div "enumeration__text"))
         (sense-text (duden--text text-node))
         (tuples (duden--tuple-alist node))
         (notes (duden--notes-alist node))
         (id-info (duden--parse-id (duden--attr node 'id))))
    (when (and sense-text (not (string-empty-p sense-text)))
      (list
       :id (plist-get id-info :id)
       :parent (plist-get id-info :parent)
       :text sense-text
       :usage (cdr (assoc "Gebrauch" tuples))
       :grammar (cdr (assoc "Grammatik" tuples))
       :examples (or (cdr (assoc "Beispiele" notes))
                     (cdr (assoc "Beispiel" notes)))
       :idioms (cdr (assoc "Wendungen, Redensarten, Sprichwörter" notes))
       :raw-tuples tuples
       :raw-notes notes))))

(defun duden--extract-subs (item parent-id)
  "Extract direct sub-senses from ITEM and attach PARENT-ID to each one."
  (let ((sub-ol (duden--first-direct-child item 'ol "enumeration__sub"))
        result)
    (when sub-ol
      (dolist (sub (duden--direct-children sub-ol 'li "enumeration__sub-item"))
        (let ((sense (duden--extract-sense-from-node sub)))
          (when sense
            (setq sense (plist-put sense :parent parent-id))
            (push sense result)))))
    (nreverse result)))

(defun duden--extract-senses (dom)
  "Extract the complete sense hierarchy from DOM.

Top-level senses receive numeric string ids such as \"1\" or \"2\".
Pure containers with only sub-senses are represented as wrapper nodes:

  (:id \"2\" :parent nil :text nil :children (...))

Sub-senses receive ids such as \"a\" or \"b\" and their :parent points
to the numeric top-level sense id."
  (let ((section (duden--by-id dom "bedeutungen"))
        results
        (counter 0))
    (when section
      (let ((ol (duden--find-first
                 (lambda (node)
                   (and (eq (dom-tag node) 'ol)
                        (duden--has-class-p node "enumeration")))
                 section)))
        (when ol
          (dolist (item (duden--direct-children ol 'li "enumeration__item"))
            (setq counter (1+ counter))
            (let* ((top-id (number-to-string counter))
                   (own-sense (duden--extract-sense-from-node item))
                   (children (duden--extract-subs item top-id)))
              (cond
               ((and own-sense (null children))
                (setq own-sense (plist-put own-sense :id top-id))
                (setq own-sense (plist-put own-sense :parent nil))
                (push own-sense results))
               ((and own-sense children)
                (setq own-sense (plist-put own-sense :id top-id))
                (setq own-sense (plist-put own-sense :parent nil))
                (setq own-sense (plist-put own-sense :children children))
                (push own-sense results))
               (children
                (push (list :id top-id
                            :parent nil
                            :text nil
                            :children children)
                      results))))))))
    (nreverse results)))

(defun duden--extract-synonyms (dom)
  "Extract the visible synonym list from DOM.

Only the first direct `ul' inside `#synonyme' is used. This avoids
capturing the help icon and the \"overview\" navigation link."
  (let* ((section (duden--by-id dom "synonyme"))
         (ul (and section
                  (duden--first-direct-child section 'ul nil))))
    (when ul
      (delete-dups
       (delq nil
             (mapcar
              (lambda (anchor)
                (let ((text (duden--text anchor)))
                  (unless (string-empty-p text) text)))
              (duden--find-all
               (lambda (node) (eq (dom-tag node) 'a))
               ul)))))))

(defun duden--extract-origin (dom)
  "Extract the origin text from DOM."
  (let* ((section (duden--by-id dom "herkunft"))
         (paragraph (and section
                         (duden--find-first
                          (lambda (node) (eq (dom-tag node) 'p))
                          section))))
    (when paragraph
      (duden--text paragraph))))

(defun duden--extract-grammar-summary (dom)
  "Extract the short grammar summary from DOM."
  (let* ((section (duden--by-id dom "grammatik"))
         (paragraph (and section
                         (duden--first-direct-child section 'p nil))))
    (when paragraph
      (duden--text paragraph))))

(defun duden--extract-lemma (dom)
  "Extract the lemma heading from DOM."
  (or
   (let* ((heading (duden--find-first (lambda (node) (eq (dom-tag node) 'h1)) dom))
          (text (duden--text heading)))
     (unless (string-empty-p text) text))
   ""))

(defun duden--extract-json-ld (dom)
  "Extract the first JSON-LD block from DOM and parse it.

Return a plist or nil if parsing fails."
  (let* ((scripts (duden--find-all
                   (lambda (node)
                     (and (eq (dom-tag node) 'script)
                          (equal (duden--attr node 'type) "application/ld+json")))
                   dom))
         (raw-json (and scripts (duden--text (car scripts)))))
    (when (and raw-json (not (string-empty-p raw-json)))
      (condition-case nil
          (json-parse-string raw-json :object-type 'plist :array-type 'list)
        (error nil)))))

(defun duden--parse-html-buffer ()
  "Parse the current HTTP response buffer into an HTML DOM tree."
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil t)
  (libxml-parse-html-region (point) (point-max)))

(defun duden--build-url (word &optional amp)
  "Build a Duden URL for WORD.

When AMP is non-nil, append `?amp' to the URL."
  (concat duden-scrape-base-url
          (url-hexify-string word)
          (when amp "?amp")))

(defun duden--parse-current-response (word url &optional source-variant)
  "Parse the current HTTP response buffer into a structured entry plist.

WORD is the original query word.
URL is the effective page URL.
SOURCE-VARIANT is a symbol such as `amp' or `default'."
  (let* ((dom (duden--parse-html-buffer))
         (lemma (duden--extract-lemma dom))
         (senses (duden--extract-senses dom))
         (synonyms (duden--extract-synonyms dom))
         (origin (duden--extract-origin dom))
         (grammar-summary (duden--extract-grammar-summary dom))
         (json-ld (duden--extract-json-ld dom)))
    (list
     :query-word word
     :url url
     :source-variant source-variant
     :lemma lemma
     :senses senses
     :synonyms synonyms
     :origin origin
     :grammar-summary grammar-summary
     :json-ld json-ld)))

(defun duden--fetch-and-parse-url (word url source-variant)
  "Fetch URL synchronously and parse it as a Duden entry for WORD.

SOURCE-VARIANT is stored in the returned plist."
  (let ((buffer (url-retrieve-synchronously url t t duden-scrape-timeout)))
    (unless buffer
      (error "No response for %s" url))
    (with-current-buffer buffer
      (unwind-protect
          (duden--parse-current-response word url source-variant)
        (kill-buffer buffer)))))

(defun duden-fetch-entry-sync (word)
  "Fetch the Duden entry for WORD synchronously.

If `duden-scrape-prefer-amp' is non-nil, the AMP version is tried first.
If it does not contain senses, the regular page is used as fallback."
  (let* ((url-request-extra-headers
          `(("User-Agent" . ,duden-scrape-user-agent)
            ("Accept-Language" . "de-DE,de;q=0.9,en;q=0.5")))
         (amp-url (duden--build-url word t))
         (default-url (duden--build-url word nil))
         entry)
    (if duden-scrape-prefer-amp
        (progn
          (setq entry (duden--fetch-and-parse-url word amp-url 'amp))
          (if (plist-get entry :senses)
              entry
            (duden--fetch-and-parse-url word default-url 'default)))
      (duden--fetch-and-parse-url word default-url 'default))))

(defun duden-fetch-entry (word callback)
  "Fetch the Duden entry for WORD asynchronously.

CALLBACK is called with the final entry plist."
  (let* ((url-request-extra-headers
          `(("User-Agent" . ,duden-scrape-user-agent)
            ("Accept-Language" . "de-DE,de;q=0.9,en;q=0.5")))
         (amp-url (duden--build-url word t))
         (default-url (duden--build-url word nil))
         (first-url (if duden-scrape-prefer-amp amp-url default-url))
         (first-variant (if duden-scrape-prefer-amp 'amp 'default)))
    (url-retrieve
     first-url
     (lambda (status)
       (unwind-protect
           (if (plist-get status :error)
               (funcall callback
                        (list :query-word word
                              :url first-url
                              :source-variant first-variant
                              :error status))
             (let ((entry (duden--parse-current-response word first-url first-variant)))
               (if (or (plist-get entry :senses)
                       (not duden-scrape-prefer-amp))
                   (funcall callback entry)
                 (let ((url-request-extra-headers
                        `(("User-Agent" . ,duden-scrape-user-agent)
                          ("Accept-Language" . "de-DE,de;q=0.9,en;q=0.5"))))
                   (url-retrieve
                    default-url
                    (lambda (status2)
                      (unwind-protect
                          (if (plist-get status2 :error)
                              (funcall callback entry)
                            (funcall callback
                                     (duden--parse-current-response
                                      word default-url 'default)))
                        (kill-buffer (current-buffer)))))))))
         (kill-buffer (current-buffer)))))))

(defun duden-flatten-senses (senses)
  "Flatten hierarchical SENSES into a single list.

Wrapper nodes that only contain :children are kept in the result as well.
Children are emitted immediately after their parent wrapper."
  (let (result)
    (cl-labels ((walk (items)
                  (dolist (item items)
                    (push item result)
                    (let ((children (plist-get item :children)))
                      (when children
                        (walk children))))))
      (walk senses))
    (nreverse result)))

(defun duden-demo-print (word)
  "Fetch WORD synchronously and pretty-print the resulting plist."
  (interactive "sDuden word: ")
  (let ((entry (duden-fetch-entry-sync word)))
    (with-current-buffer (get-buffer-create "*Duden Demo*")
      (erase-buffer)
      (pp entry (current-buffer))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'duden-scrape)

;;; duden-scrape.el ends here
