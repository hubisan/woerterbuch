;;; woerterbuch-dwds.el --- DWDS backend -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'woerterbuch-core)

(defconst woerterbuch-dwds-base-url
  "https://www.dwds.de/wb/"
  "Base URL for DWDS dictionary pages.")

(defconst woerterbuch-dwds--definition-qualifier-classes
  '("dwdswb-bedeutungsebene"
    "dwdswb-stilebene"
    "dwdswb-gebrauchsebene"
    "dwdswb-gebrauchszeitraum"
    "dwdswb-stilfaerbung")
  "DWDS classes that carry definition qualifiers.")

(defconst woerterbuch-dwds--definition-skip-classes
  '("dwdswb-binnenquelle" "dwdswb-paraphrase")
  "DWDS classes skipped while extracting definition text.")

(defun woerterbuch-dwds--build-url (lemma)
  "Build canonical DWDS URL for LEMMA."
  (concat woerterbuch-dwds-base-url (url-hexify-string lemma)))

(defun woerterbuch-dwds--clean-text (string)
  "Normalize whitespace and simple punctuation spacing in STRING."
  (when string
    (let ((s (string-trim
              (replace-regexp-in-string "[[:space:] ]+" " " string))))
      (setq s (replace-regexp-in-string " +," "," s))
      (setq s (replace-regexp-in-string " +\\." "." s))
      (setq s (replace-regexp-in-string "( +" "(" s))
      (setq s (replace-regexp-in-string " +)" ")" s))
      s)))

(defun woerterbuch-dwds--text (node)
  "Return normalized text content for NODE."
  (woerterbuch-dwds--clean-text
   (cond
    ((null node) "")
    ((stringp node) node)
    ((listp node)
     (mapconcat #'woerterbuch-dwds--text (dom-children node) " "))
    (t ""))))

(defun woerterbuch-dwds--text-skipping-classes (node classes)
  "Return text content for NODE while skipping elements in CLASSES."
  (woerterbuch-dwds--clean-text
   (cond
    ((null node) "")
    ((stringp node) node)
    ((and (listp node)
          (seq-some (lambda (class)
                      (woerterbuch-dwds--has-class-p node class))
                    classes))
     "")
    ((listp node)
     (mapconcat
      (lambda (child)
        (woerterbuch-dwds--text-skipping-classes child classes))
      (dom-children node)
      " "))
    (t ""))))

(defun woerterbuch-dwds--class-list (node)
  "Return CSS classes for NODE."
  (split-string (or (dom-attr node 'class) "") "[[:space:]]+" t))

(defun woerterbuch-dwds--has-class-p (node class)
  "Return non-nil when NODE has CSS CLASS."
  (member class (woerterbuch-dwds--class-list node)))

(defun woerterbuch-dwds--has-any-class-p (node classes)
  "Return non-nil when NODE has any CSS class from CLASSES."
  (seq-some (lambda (class)
              (woerterbuch-dwds--has-class-p node class))
            classes))

(defun woerterbuch-dwds--element-children (node)
  "Return element children of NODE."
  (seq-filter #'listp (dom-children node)))

(defun woerterbuch-dwds--children-with-class (node class)
  "Return direct child elements of NODE having CLASS."
  (seq-filter (lambda (child)
                (woerterbuch-dwds--has-class-p child class))
              (woerterbuch-dwds--element-children node)))

(defun woerterbuch-dwds--first-child-with-class (node class)
  "Return first direct child element of NODE having CLASS."
  (car (woerterbuch-dwds--children-with-class node class)))

(defun woerterbuch-dwds--descendants-with-class (node class)
  "Return all descendants of NODE having CLASS."
  (when node (dom-by-class node class)))

(defun woerterbuch-dwds--find-first (node predicate)
  "Return first descendant of NODE matching PREDICATE."
  (catch 'found
    (dolist (child (woerterbuch-dwds--element-children node))
      (when (funcall predicate child)
        (throw 'found child))
      (let ((match (woerterbuch-dwds--find-first child predicate)))
        (when match
          (throw 'found match))))
    nil))

(defun woerterbuch-dwds--canonical-url (dom fallback)
  "Read canonical URL from DOM or return FALLBACK."
  (let ((link (woerterbuch-dwds--find-first
               dom
               (lambda (node)
                 (and (eq (dom-tag node) 'link)
                      (equal (dom-attr node 'rel) "canonical"))))))
    (or (and link (dom-attr link 'href)) fallback)))

(defun woerterbuch-dwds--field-text (article label)
  "Return ARTICLE form field text for LABEL."
  (cl-loop
   for block
   in (woerterbuch-dwds--descendants-with-class article "dwdswb-ft-block")
   for block-label =
   (woerterbuch-dwds--text
    (woerterbuch-dwds--find-first
     block
     (lambda (node)
       (woerterbuch-dwds--has-class-p node "dwdswb-ft-blocklabel"))))
   when (and block-label
             (string-match-p (regexp-quote label) block-label))
   return
   (woerterbuch-dwds--text
    (woerterbuch-dwds--find-first
     block
     (lambda (node)
       (woerterbuch-dwds--has-class-p node "dwdswb-ft-blocktext"))))))

(defun woerterbuch-dwds--wortart-from-grammar (grammar)
  "Extract word class from full GRAMMAR text."
  (when (and grammar (not (string-empty-p grammar)))
    (let ((head (string-trim (car (split-string grammar "·" t)))))
      (if (string-match "^\\([^()]+\\)" head)
          (string-trim (match-string 1 head))
        head))))

(defun woerterbuch-dwds--collect-qualifiers (node)
  "Collect qualifier texts below NODE in DOM order."
  (let (out)
    (when (woerterbuch-dwds--has-any-class-p
           node
           woerterbuch-dwds--definition-qualifier-classes)
      (let ((txt (woerterbuch-dwds--text node)))
        (unless (string-empty-p txt)
          (push txt out))))
    (dolist (child (woerterbuch-dwds--element-children node))
      (setq out (nconc out (woerterbuch-dwds--collect-qualifiers child))))
    out))

(defun woerterbuch-dwds--extract-qualifiers (def-node)
  "Extract semantic qualifiers from DEF-NODE in DOM order."
  (let (out)
    (dolist (child (woerterbuch-dwds--element-children def-node))
      (when (woerterbuch-dwds--has-class-p child "dwdswb-diasystematik")
        (setq out (nconc out (woerterbuch-dwds--collect-qualifiers child)))))
    out))

(defun woerterbuch-dwds--extract-definition-text (def-node)
  "Extract definition text from DEF-NODE in DOM order.

Only syntagmatic and actual definition content is included. Semantic
qualifiers from `.dwdswb-diasystematik' are handled separately via
`woerterbuch-dwds--extract-qualifiers'."
  (let (parts)
    (dolist (child (woerterbuch-dwds--element-children def-node))
      (when (or (woerterbuch-dwds--has-class-p child "dwdswb-syntagmatik")
                (woerterbuch-dwds--has-class-p child "dwdswb-definitionen")
                (woerterbuch-dwds--has-class-p child "dwdswb-definition"))
        (let ((txt (woerterbuch-dwds--text-skipping-classes
                    child
                    woerterbuch-dwds--definition-skip-classes)))
          (unless (string-empty-p txt)
            (push txt parts)))))
    (string-join (nreverse parts) " ")))

(defun woerterbuch-dwds--extract-examples (usage-node)
  "Extract example texts from USAGE-NODE.

Only `.dwdswb-belegtext' is used, so newspaper sources and DWDS date
stamps are ignored automatically."
  (when usage-node
    (delq
     nil
     (mapcar
      (lambda (node)
        (let ((txt (woerterbuch-dwds--clean-text
                    (woerterbuch-dwds--text node))))
          (unless (string-empty-p txt)
            txt)))
      (woerterbuch-dwds--descendants-with-class usage-node
                                                "dwdswb-belegtext")))))

(defun woerterbuch-dwds--extract-idioms-from-block (block)
  "Extract idiom objects from Mehrwortausdrücke BLOCK."
  (let ((seen (make-hash-table :test #'equal))
        idioms)
    (dolist (link (woerterbuch-dwds--find-all-links block))
      (let ((text (woerterbuch-dwds--text link))
            (href (dom-attr link 'href)))
        (when (and (stringp href)
                   (string-prefix-p "/wb/" href)
                   (not (string-empty-p text))
                   (not (gethash text seen)))
          (puthash text t seen)
          (push text idioms))))
    (nreverse idioms)))

(defun woerterbuch-dwds--find-all-links (node)
  "Return all descendant links below NODE."
  (let (acc)
    (dolist (child (woerterbuch-dwds--element-children node))
      (when (eq (dom-tag child) 'a)
        (push child acc))
      (setq acc (nconc (nreverse (woerterbuch-dwds--find-all-links child))
                       acc)))
    (nreverse acc)))

(defun woerterbuch-dwds--parse-idioms (article)
  "Extract idioms from ARTICLE's Mehrwortausdrücke field."
  (let ((field-block
         (cl-loop
          for block in
          (woerterbuch-dwds--descendants-with-class article "dwdswb-ft-block")
          for label =
          (woerterbuch-dwds--text
           (woerterbuch-dwds--find-first
            block
            (lambda (node)
              (woerterbuch-dwds--has-class-p node "dwdswb-ft-blocklabel"))))
          when (and label (string-match-p "Mehrwortausdrücke" label))
          return
          (woerterbuch-dwds--find-first
           block
           (lambda (node)
             (woerterbuch-dwds--has-class-p node "dwdswb-ft-blocktext"))))))
    (when field-block
      (woerterbuch-dwds--extract-idioms-from-block field-block))))

(defun woerterbuch-dwds--parse-etymology (scope)
  "Extract etymology text from SCOPE."
  (let ((entry (woerterbuch-dwds--find-first
                scope
                (lambda (node)
                  (woerterbuch-dwds--has-class-p node "etymwb-entry")))))
    (let ((text (woerterbuch-dwds--text entry)))
      (unless (string-empty-p text)
        text))))

(defun woerterbuch-dwds--article-scope-p (node)
  "Return non-nil when NODE is a usable article scope."
  (and (woerterbuch-dwds--find-first
        node
        (lambda (child)
          (woerterbuch-dwds--has-class-p child "dwdswb-artikel")))
       (not (equal (dom-attr node 'id) "0"))))

(defun woerterbuch-dwds--article-scopes (dom)
  "Return scopes that each contain one article."
  (let ((panes (seq-filter #'woerterbuch-dwds--article-scope-p
                           (woerterbuch-dwds--descendants-with-class
                            dom
                            "tab-pane"))))
    (if panes panes (list dom))))

(defun woerterbuch-dwds--make-definition-parser (sections)
  "Return recursive parser closure for definitions according to SECTIONS."
  (let ((include-examples
         (woerterbuch-core-section-requested-p :examples sections)))
    (cl-labels
        ((parse-one (node id)
           (let* ((label-node
                   (woerterbuch-dwds--first-child-with-class
                    node
                    "dwdswb-lesart-n"))
                  (content-node
                   (woerterbuch-dwds--first-child-with-class
                    node
                    "dwdswb-lesart-content"))
                  (def-node
                   (and content-node
                        (woerterbuch-dwds--first-child-with-class
                         content-node
                         "dwdswb-lesart-def")))
                  (usage-node
                   (and include-examples
                        content-node
                        (woerterbuch-dwds--first-child-with-class
                         content-node
                         "dwdswb-verwendungsbeispiele")))
                  (child-nodes
                   (and content-node
                        (woerterbuch-dwds--children-with-class
                         content-node
                         "dwdswb-lesart"))))
             (list :id id
                   :dwds-id (dom-attr node 'id)
                   :label (woerterbuch-dwds--text label-node)
                   :definition (and def-node
                                    (woerterbuch-dwds--extract-definition-text
                                     def-node))
                   :qualifiers (and def-node
                                    (woerterbuch-dwds--extract-qualifiers
                                     def-node))
                   :examples (and include-examples
                                  usage-node
                                  (woerterbuch-dwds--extract-examples
                                   usage-node))
                   :definitions (parse-list child-nodes))))
         (parse-list (nodes)
           (cl-loop for node in nodes
                    for idx from 1
                    collect (parse-one node idx))))
      #'parse-list)))

(defun woerterbuch-dwds--parse-homograph (scope homograph-id sections)
  "Parse one DWDS article SCOPE as HOMOGRAPH-ID according to SECTIONS."
  (let* ((article (woerterbuch-dwds--find-first
                   scope
                   (lambda (node)
                     (woerterbuch-dwds--has-class-p node "dwdswb-artikel"))))
         (bookmark (and article
                        (woerterbuch-dwds--find-first
                         article
                         (lambda (node)
                           (woerterbuch-dwds--has-class-p
                            node
                            "dwds-bookmark-button")))))
         (heading (and article
                       (woerterbuch-dwds--find-first
                        article
                        (lambda (node)
                          (woerterbuch-dwds--has-class-p
                           node
                           "dwdswb-ft-lemmaansatz")))))
         (lemma-node
          (and heading
               (woerterbuch-dwds--find-first
                heading
                (lambda (node)
                  (eq (dom-tag node) 'b)))))
         (title (woerterbuch-dwds--text heading))
         (grammar
          (and article
               (woerterbuch-dwds--field-text article "Grammatik")))
         (want-definitions
          ;; Examples without link to defintions make no sense.
          (or (woerterbuch-core-section-requested-p :definitions sections)
              (woerterbuch-core-section-requested-p :examples sections)))
         (want-origin (woerterbuch-core-section-requested-p :origin sections))
         (want-idioms (woerterbuch-core-section-requested-p :idioms sections))
         (lesarten-root
          (and want-definitions
               (woerterbuch-dwds--find-first
                scope
                (lambda (node)
                  (woerterbuch-dwds--has-class-p node "dwdswb-lesarten")))))
         (parse-definitions
          (and want-definitions
               (woerterbuch-dwds--make-definition-parser sections))))
    (list :id homograph-id
          :hidx (or (and bookmark (dom-attr bookmark 'data-hidx))
                    (dom-attr scope 'id)
                    "")
          :lemma (woerterbuch-dwds--text lemma-node)
          :title title
          :wortart (woerterbuch-dwds--wortart-from-grammar grammar)
          :grammar grammar
          :origin (and want-origin (woerterbuch-dwds--parse-etymology scope))
          :idioms
          (and want-idioms
               article
               (woerterbuch-dwds--parse-idioms article))
          :definitions
          (and lesarten-root
               (funcall parse-definitions
                        (woerterbuch-dwds--children-with-class
                         lesarten-root
                         "dwdswb-lesart"))))))

(defun woerterbuch-dwds--parse-dom (dom lemma sections)
  "Parse DWDS DOM for LEMMA according to SECTIONS."
  (let* ((canonical-url
          (woerterbuch-dwds--canonical-url
           dom
           (woerterbuch-dwds--build-url lemma)))
         (scopes (woerterbuch-dwds--article-scopes dom))
         (homographs
          (cl-loop for scope in scopes
                   for idx from 1
                   collect
                   (woerterbuch-dwds--parse-homograph scope idx sections)))
         (page-lemma (or (plist-get (car homographs) :lemma) lemma)))
    (list :lemma page-lemma
          :url canonical-url
          :homographs homographs)))

(defun woerterbuch-dwds--parse-current-buffer (lemma sections)
  "Parse current HTTP buffer as a DWDS page for LEMMA."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil t))
  (skip-chars-forward "\r\n")
  (let* ((dom (libxml-parse-html-region (point) (point-max)))
         (entry (woerterbuch-dwds--parse-dom dom lemma sections))
         (result (woerterbuch-core-make-result 'dwds lemma)))
    (setq result (plist-put result :lemma (or (plist-get entry :lemma) lemma)))
    (setq result (plist-put result :url (plist-get entry :url)))
    (setq result (plist-put result :homographs (plist-get entry :homographs)))
    result))

(defun woerterbuch-dwds--fetch-callback (status lemma sections callback)
  "Handle DWDS response STATUS for LEMMA and invoke CALLBACK."
  (let ((result nil))
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((plist-get status :error)
                    (woerterbuch-core-make-error
                     'dwds
                     lemma
                     (format "Network error: %S" (plist-get status :error))))
                   ((and (boundp 'url-http-response-status)
                         (numberp url-http-response-status)
                         (>= url-http-response-status 400))
                    (woerterbuch-core-make-error
                     'dwds
                     lemma
                     (format "HTTP error: %s" url-http-response-status)))
                   (t
                    (woerterbuch-dwds--parse-current-buffer lemma sections)))
                (error
                 (woerterbuch-core-make-error
                  'dwds
                  lemma
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-dwds--request-needed-p (sections)
  "Return non-nil when DWDS can contribute anything for SECTIONS."
  (or (woerterbuch-core-section-requested-p :definitions sections)
      (woerterbuch-core-section-requested-p :examples sections)
      (woerterbuch-core-section-requested-p :origin sections)
      (woerterbuch-core-section-requested-p :idioms sections)))

(defun woerterbuch-dwds-fetch (lemma sections callback)
  "Fetch LEMMA from DWDS and invoke CALLBACK once.

The request goes directly to the canonical dictionary page
https://www.dwds.de/wb/<lemma>. Homographs such as Bank#1 and Bank#2 are
kept as subentries in :homographs but always share the same canonical :url."
  (if (not (woerterbuch-dwds--request-needed-p sections))
      (funcall callback (woerterbuch-core-make-result 'dwds lemma))
    (let ((url-request-extra-headers
           '(("User-Agent" . "woerterbuch/0.1")
             ("Accept-Language" . "de,en;q=0.8"))))
      (url-retrieve
       (woerterbuch-dwds--build-url lemma)
       #'woerterbuch-dwds--fetch-callback
       (list lemma sections callback)
       t
       t))))

(provide 'woerterbuch-dwds)

;;; woerterbuch-dwds.el ends here
