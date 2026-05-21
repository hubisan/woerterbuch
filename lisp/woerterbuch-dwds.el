;;; woerterbuch-dwds.el --- DWDS backend -*- lexical-binding: t; -*-

;;; Commentary:

;; DWDS backend implementation and DOM parsers.

;;; Code:

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

(defconst woerterbuch-dwds-request-headers
  '(("User-Agent"
     . "Mozilla/5.0 (Windows NT 10.0; rv:109.0) Gecko/20100101 Firefox/115.0")
    ("Accept"
     . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    ("Accept-Language" . "en-US,en;q=0.5")
    ("Accept-Encoding" . "gzip, deflate, br")
    ("DNT" . "1")
    ("Connection" . "keep-alive"))
  "HTTP headers used for DWDS requests, mimicking Tor Browser.")

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
              (replace-regexp-in-string
               "[[:space:]\n\r\t ]+"
               " "
               (replace-regexp-in-string "[\n\r\t]+" " " string)))))
      (setq s (replace-regexp-in-string " +," "," s))
      (setq s (replace-regexp-in-string " +\\." "." s))
      (setq s (replace-regexp-in-string "( +" "(" s))
      (setq s (replace-regexp-in-string " +)" ")" s))
      (setq s (replace-regexp-in-string "⟨ +" "⟨" s))
      (setq s (replace-regexp-in-string " +⟩" "⟩" s))
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

(defun woerterbuch-dwds--find-all (node predicate)
  "Return all descendants of NODE matching PREDICATE."
  (let (matches)
    (dolist (child (woerterbuch-dwds--element-children node))
      (when (funcall predicate child)
        (push child matches))
      (setq matches
            (nconc (nreverse (woerterbuch-dwds--find-all child predicate))
                   matches)))
    (nreverse matches)))

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

(defun woerterbuch-dwds--qualifier-node-p (node)
  "Return non-nil when NODE carries a qualifier within diasystematics."
  (let ((classes (woerterbuch-dwds--class-list node)))
    (and classes
         (not (member "dwdswb-diasystematik" classes))
         (seq-some (lambda (class)
                     (string-prefix-p "dwdswb-" class))
                   classes))))

(defun woerterbuch-dwds--collect-qualifiers (node)
  "Collect qualifier texts below NODE in DOM order."
  (let (out)
    (when (woerterbuch-dwds--qualifier-node-p node)
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

(defun woerterbuch-dwds--strip-html-tags (string)
  "Return STRING without HTML tags."
  (when string
    (replace-regexp-in-string "<[^>]+>" "" string)))

(defun woerterbuch-dwds--reference-definition (node)
  "Return definition text encoded in DWDS reference NODE."
  (let* ((popover (dom-attr node 'data-content))
         (text (woerterbuch-dwds--clean-text
                (woerterbuch-dwds--strip-html-tags popover))))
    (unless (string-empty-p (or text ""))
      text)))

(defun woerterbuch-dwds--extract-reference-text (node)
  "Return definition text for a DWDS reference wrapper NODE."
  (let* ((ref-node (or (and
                        (woerterbuch-dwds--has-class-p node "dwdswb-verweis")
                        node)
                       (woerterbuch-dwds--find-first
                        node
                        (lambda (child)
                          (woerterbuch-dwds--has-class-p child
                                                         "dwdswb-verweis")))))
         (headline (woerterbuch-dwds--text node))
         (definition (and ref-node
                          (woerterbuch-dwds--reference-definition ref-node))))
    (cond
     ((and (not (string-empty-p (or headline "")))
           (not (string-empty-p (or definition ""))))
      (format "%s = %s" headline definition))
     ((not (string-empty-p (or headline ""))) headline)
     (t definition))))

(defun woerterbuch-dwds--mwa-marker-p (node)
  "Return non-nil when NODE contains an MWA marker."
  (or (woerterbuch-dwds--find-first
       node
       (lambda (child)
         (let ((src (dom-attr child 'src)))
           (and src (string-match-p "letter-mwa\\.svg" src)))))
      (woerterbuch-dwds--find-first
       node
       (lambda (child)
         (let ((title (or (dom-attr child 'title)
                          (dom-attr child 'data-original-title))))
           (and title (string-match-p "Mehrwortausdruck" title)))))))

(defun woerterbuch-dwds--local-mwa-marker-p (content-node)
  "Return non-nil when CONTENT-NODE has an MWA marker outside child senses."
  (catch 'marker
    (dolist (child (woerterbuch-dwds--element-children content-node))
      (unless (woerterbuch-dwds--has-class-p child "dwdswb-lesart")
        (when (woerterbuch-dwds--mwa-marker-p child)
          (throw 'marker t))))
    nil))

(defun woerterbuch-dwds--find-local-mwa-scope (content-node)
  "Return the direct MWA scope belonging to CONTENT-NODE."
  (catch 'scope
    (dolist (child (woerterbuch-dwds--element-children content-node))
      (unless (woerterbuch-dwds--has-class-p child "dwdswb-lesart")
        (when (or (woerterbuch-dwds--has-class-p child "dwdswb-phraseme")
                  (woerterbuch-dwds--has-class-p child "dwdswb-syntagmatik")
                  (woerterbuch-dwds--has-class-p child
                                                 "dwdswb-konstruktionsmuster"))
          (throw 'scope child))
        (let ((match
               (woerterbuch-dwds--find-first
                child
                (lambda (node)
                  (or (woerterbuch-dwds--has-class-p node "dwdswb-phrasem")
                      (woerterbuch-dwds--has-class-p node
                                                     "dwdswb-konstruktionsmuster"))))))
          (when match
            (throw 'scope child)))))
    nil))

(defun woerterbuch-dwds--normalize-paraphrase-text (text)
  "Normalize DWDS paraphrase TEXT."
  (when text
    (let ((s (woerterbuch-dwds--clean-text text)))
      (when s
        (setq s (replace-regexp-in-string "\\`(= *" "" s))
        (setq s (replace-regexp-in-string " *)\\'" "" s))
        s))))

(defun woerterbuch-dwds--extract-mwa-text (content-node)
  "Return MWA definition text from CONTENT-NODE."
  (let ((local-scope (and content-node
                          (woerterbuch-dwds--find-local-mwa-scope
                           content-node))))
    (when (and local-scope
               (or (woerterbuch-dwds--mwa-marker-p local-scope)
                   (woerterbuch-dwds--local-mwa-marker-p content-node)))
      (let* ((phrase-scope
              (or (and
                   (woerterbuch-dwds--has-class-p local-scope "dwdswb-phrasem")
                   local-scope)
                  (woerterbuch-dwds--find-first
                   local-scope
                   (lambda (child)
                     (woerterbuch-dwds--has-class-p child "dwdswb-phrasem")))
                  (and (woerterbuch-dwds--has-class-p local-scope
                                                      "dwdswb-konstruktionsmuster")
                       local-scope)
                  (woerterbuch-dwds--find-first
                   local-scope
                   (lambda (child)
                     (woerterbuch-dwds--has-class-p
                      child
                      "dwdswb-konstruktionsmuster")))))
             (phrase-node
              (and phrase-scope
                   (woerterbuch-dwds--find-first
                    phrase-scope
                    (lambda (child)
                      (woerterbuch-dwds--has-class-p child "dwdswb-belegtext")))))
             (phrase (and phrase-node
                          (woerterbuch-dwds--text-skipping-classes
                           phrase-node
                           '("dwdswb-paraphrase"))))
             (paraphrases
              (delq nil
                    (mapcar
                     (lambda (node)
                       (woerterbuch-dwds--normalize-paraphrase-text
                        (woerterbuch-dwds--text node)))
                     (and phrase-scope
                          (woerterbuch-dwds--descendants-with-class
                           phrase-scope
                           "dwdswb-paraphrase"))))))
        (when (and phrase (not (string-empty-p phrase)))
          (if paraphrases
              (format "%s (MWA) = %s"
                      phrase
                      (string-join paraphrases "; "))
            (format "%s (MWA)" phrase)))))))

(defun woerterbuch-dwds--explicit-phraseme-block-p (content-node)
  "Return non-nil when CONTENT-NODE carries an explicit DWDS phraseme block."
  (seq-some (lambda (child)
              (woerterbuch-dwds--has-class-p child "dwdswb-phraseme"))
            (woerterbuch-dwds--element-children content-node)))

(defun woerterbuch-dwds--extract-definition-text (def-node content-node)
  "Extract definition text from DEF-NODE and CONTENT-NODE in DOM order.

Only syntagmatic and actual definition content is included. Semantic
qualifiers from `.dwdswb-diasystematik' are handled separately via
`woerterbuch-dwds--extract-qualifiers'."
  (let (parts)
    (dolist (child (woerterbuch-dwds--element-children def-node))
      (cond
       ((woerterbuch-dwds--has-class-p child "dwdswb-verweise")
        (let ((txt (woerterbuch-dwds--extract-reference-text child)))
          (unless (string-empty-p (or txt ""))
            (push txt parts))))
       ((or (woerterbuch-dwds--has-class-p child "dwdswb-syntagmatik")
            (woerterbuch-dwds--has-class-p child "dwdswb-definitionen")
            (woerterbuch-dwds--has-class-p child "dwdswb-definition"))
        (let ((txt (woerterbuch-dwds--text-skipping-classes
                    child
                    woerterbuch-dwds--definition-skip-classes)))
          (unless (string-empty-p txt)
            (push txt parts))))))
    (let ((definition (string-join (nreverse parts) " "))
          (mwa-definition (woerterbuch-dwds--extract-mwa-text content-node))
          (qualifiers (woerterbuch-dwds--extract-qualifiers def-node)))
      (cond
       ((and mwa-definition
             (woerterbuch-dwds--explicit-phraseme-block-p content-node))
        mwa-definition)
       ((and mwa-definition
             (not (string-empty-p definition))
             (string-prefix-p "⟨" definition)
             (member "übertragen" qualifiers))
        mwa-definition)
       ((not (string-empty-p definition)) definition)
       (t (or mwa-definition definition))))))

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
  "Extract idioms from ARTICLE's Mehrwortausdrücke relation block.
The block id is expected to match `relation-block-[0-9]+-mwa',
for example `relation-block-1-mwa' or `relation-block-2-mwa'."
  (when-let* ((field-block
               (woerterbuch-dwds--find-first
                article
                (lambda (node)
                  (let ((id (dom-attr node 'id)))
                    (and (stringp id)
                         (string-match-p
                          "\\`relation-block-[0-9]+-mwa\\'" id)))))))
    (woerterbuch-dwds--extract-idioms-from-block field-block)))

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
  "Return article scopes collected from DOM."
  (let ((panes (seq-filter #'woerterbuch-dwds--article-scope-p
                           (woerterbuch-dwds--descendants-with-class
                            dom
                            "tab-pane"))))
    (if panes panes (list dom))))

(defun woerterbuch-dwds--entry-page-p (dom)
  "Return non-nil when DOM contains a real DWDS article page."
  (and (woerterbuch-dwds--find-first
        dom
        (lambda (node)
          (woerterbuch-dwds--has-class-p node "dwdswb-artikel")))
       t))

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
                                     def-node
                                     content-node))
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
  "Parse current HTTP buffer as a DWDS page for LEMMA and SECTIONS."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil t))
  (skip-chars-forward "\r\n")
  (let* ((dom (libxml-parse-html-region (point) (point-max)))
         (entry-page-p (woerterbuch-dwds--entry-page-p dom)))
    (when entry-page-p
      (let* ((entry (woerterbuch-dwds--parse-dom dom lemma sections))
             (result (woerterbuch-core-make-result 'dwds lemma)))
        (setq result (plist-put result :lemma (or (plist-get entry :lemma)
                                                  lemma)))
        (setq result (plist-put result :url (plist-get entry :url)))
        (setq result
              (plist-put result :homographs (plist-get entry :homographs)))
        result))))

(defun woerterbuch-dwds--request-needed-p (sections)
  "Return non-nil when DWDS can contribute anything for SECTIONS."
  (or (woerterbuch-core-section-requested-p :definitions sections)
      (woerterbuch-core-section-requested-p :examples sections)
      (woerterbuch-core-section-requested-p :origin sections)
      (woerterbuch-core-section-requested-p :idioms sections)))

(defun woerterbuch-dwds--with-headers (thunk)
  "Call THUNK with DWDS request headers configured."
  (let ((url-request-extra-headers woerterbuch-dwds-request-headers))
    (funcall thunk)))

(defun woerterbuch-dwds--status-http-code (status)
  "Return HTTP status code from callback STATUS when available."
  (or (and (boundp 'url-http-response-status)
           (numberp url-http-response-status)
           url-http-response-status)
      (let ((err (plist-get status :error)))
        (and (consp err)
             (eq (car err) 'error)
             (eq (cadr err) 'http)
             (numberp (caddr err))
             (caddr err)))))

(defun woerterbuch-dwds--status-network-error-p (status)
  "Return non-nil when STATUS represents a non-HTTP network error."
  (let ((err (plist-get status :error)))
    (and err
         (not (woerterbuch-dwds--status-http-code status)))))

(defun woerterbuch-dwds--fetch-callback (status lemma sections callback)
  "Handle DWDS response STATUS for LEMMA and SECTIONS.

Invoke CALLBACK with the parsed result."
  (let ((result nil)
        (http-code (woerterbuch-dwds--status-http-code status)))
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((woerterbuch-dwds--status-network-error-p status)
                    (woerterbuch-core-make-error
                     'dwds
                     lemma
                     (format "Network error: %S" (plist-get status :error))))
                   ((and http-code
                         (>= http-code 400))
                    (woerterbuch-core-make-error
                     'dwds
                     lemma
                     (format "HTTP error: %s" http-code)))
                   (t
                    (or (woerterbuch-dwds--parse-current-buffer lemma sections)
                        (woerterbuch-core-make-error
                         'dwds
                         lemma
                         "No matches found"))))
                (error
                 (woerterbuch-core-make-error
                  'dwds
                  lemma
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-dwds-fetch (lemma sections callback)
  "Fetch LEMMA from DWDS for SECTIONS and invoke CALLBACK once.

The request goes directly to the canonical dictionary page
https://www.dwds.de/wb/<lemma>. Homographs such as Bank#1 and Bank#2 are
kept as subentries in :homographs but always share the same canonical :url."
  (if (not (woerterbuch-dwds--request-needed-p sections))
      (funcall callback (woerterbuch-core-make-result 'dwds lemma))
    (woerterbuch-dwds--with-headers
     (lambda ()
       (url-retrieve
        (woerterbuch-dwds--build-url lemma)
        #'woerterbuch-dwds--fetch-callback
        (list lemma sections callback)
        t
        t)))))

(provide 'woerterbuch-dwds)

;;; woerterbuch-dwds.el ends here
