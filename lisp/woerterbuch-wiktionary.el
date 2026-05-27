;;; woerterbuch-wiktionary.el --- Wiktionary backend -*- lexical-binding: t; -*-

;;; Commentary:

;; German Wiktionary backend implementation and DOM parsers.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'woerterbuch-core)

(defconst woerterbuch-wiktionary-web-url
  "https://de.wiktionary.org/wiki/"
  "Base URL for German Wiktionary article pages.")

(defconst woerterbuch-wiktionary-request-headers
  '(("User-Agent"
     . "Mozilla/5.0 (Windows NT 10.0; rv:109.0) Gecko/20100101 Firefox/115.0")
    ("Accept"
     . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    ("Accept-Language" . "en-US,en;q=0.5")
    ("Accept-Encoding" . "gzip, deflate, br")
    ("DNT" . "1")
    ("Connection" . "keep-alive"))
  "HTTP headers used for Duden requests, mimicking Tor Browser for better privacy.")

(defconst woerterbuch-wiktionary--label-map
  '(("Bedeutungen" . :definitions)
    ("Beispiele" . :examples)
    ("Synonyme" . :synonyms)
    ("Sinnverwandte Wörter" . :related-synonyms)
    ("Redewendungen" . :idioms)
    ("Herkunft" . :origin))
  "Rendered Wiktionary block labels mapped to internal keys.")

(defun woerterbuch-wiktionary--build-web-url (lemma)
  "Build human-facing Wiktionary URL for LEMMA."
  (concat woerterbuch-wiktionary-web-url
          (url-hexify-string
           (replace-regexp-in-string "[[:space:]]+" "_" lemma))))

(defun woerterbuch-wiktionary--clean-text (string)
  "Normalize whitespace and punctuation spacing in STRING."
  (when string
    (let ((s (string-trim
              (replace-regexp-in-string "[[:space:] ]+" " " string))))
      (setq s (replace-regexp-in-string " +," "," s))
      (setq s (replace-regexp-in-string " +\\." "." s))
      (setq s (replace-regexp-in-string "( +" "(" s))
      (setq s (replace-regexp-in-string " +)" ")" s))
      (setq s (replace-regexp-in-string " ;" ";" s))
      (setq s (replace-regexp-in-string " :" ":" s))
      s)))

(defun woerterbuch-wiktionary--strip-footnote-refs (string)
  "Remove rendered footnote markers from STRING."
  (when string
    (let ((s string))
      (setq s
            (replace-regexp-in-string
             "\\(?:[[:space:]]*\\[[[:space:]]*[0-9]+[[:space:]]*\\]\\)+"
             ""
             s))
      (woerterbuch-wiktionary--clean-text s))))

(defun woerterbuch-wiktionary--clean-content-text (string)
  "Normalize content STRING and remove rendered footnote markers."
  (when string
    (woerterbuch-wiktionary--strip-footnote-refs
     (woerterbuch-wiktionary--clean-text string))))

(defun woerterbuch-wiktionary--clean-origin-text (string)
  "Normalize etymology STRING from rendered Wiktionary HTML."
  (when string
    (let ((s (woerterbuch-wiktionary--clean-content-text string)))
      ;; Drop language-code backlinks like `→ got` or `→ gmh`.
      (setq s
            (replace-regexp-in-string
             "[[:space:]]*→[[:space:]]*[[:alnum:]-]+\\b" "" s))
      ;; Tighten typographic single quotes that often end up spaced out.
      (setq s (replace-regexp-in-string "‚[[:space:]]+" "‚" s))
      (setq s (replace-regexp-in-string "[[:space:]]+‘" "‘" s))
      (setq s (replace-regexp-in-string "»[[:space:]]+" "»" s))
      (setq s (replace-regexp-in-string "[[:space:]]+«" "«" s))
      ;; Clean a few frequent residual artifacts from rendered etymology prose.
      (setq s (replace-regexp-in-string " ☆" "" s))
      (setq s
            (replace-regexp-in-string "\\b\\([[:alpha:]]\\)[[:space:]]+-" "\\1-"
                                      s))
      (setq s
            (replace-regexp-in-string
             ")[[:space:]]+[fmn][[:space:]]+\\(‚\\|»\\)" ") \\1" s))
      (setq s (replace-regexp-in-string
               "\\(\\*[[:alpha:]āēīōūȳə̯-]+\\)-[[:space:]]+\\([[:alpha:]]\\)\\b"
               "\\1-\\2"
               s))
      ;; Re-run generic whitespace cleanup after targeted replacements.
      (setq s (woerterbuch-wiktionary--clean-text s))
      s)))

(defun woerterbuch-wiktionary--text (node)
  "Return normalized text content for NODE."
  (woerterbuch-wiktionary--clean-text
   (cond
    ((null node) "")
    ((stringp node) node)
    ((listp node)
     (mapconcat #'woerterbuch-wiktionary--text (dom-children node) " "))
    (t ""))))

(defun woerterbuch-wiktionary--class-list (node)
  "Return CSS classes for NODE."
  (split-string (or (dom-attr node 'class) "") "[[:space:]]+" t))

(defun woerterbuch-wiktionary--has-class-p (node class)
  "Return non-nil when NODE has CSS CLASS."
  (member class (woerterbuch-wiktionary--class-list node)))

(defun woerterbuch-wiktionary--element-children (node)
  "Return element children of NODE."
  (seq-filter #'listp (dom-children node)))

(defun woerterbuch-wiktionary--find-first (node predicate)
  "Return first descendant of NODE matching PREDICATE."
  (catch 'found
    (dolist (child (woerterbuch-wiktionary--element-children node))
      (when (funcall predicate child)
        (throw 'found child))
      (let ((match (woerterbuch-wiktionary--find-first child predicate)))
        (when match
          (throw 'found match))))
    nil))

(defun woerterbuch-wiktionary--descendants-by-tag (node tag)
  "Return all descendants of NODE with TAG."
  (let (acc)
    (dolist (child (woerterbuch-wiktionary--element-children node))
      (when (eq (dom-tag child) tag)
        (push child acc))
      (setq acc
            (nconc (nreverse
                    (woerterbuch-wiktionary--descendants-by-tag child tag))
                   acc)))
    (nreverse acc)))

(defun woerterbuch-wiktionary--heading-node-level-and-text (section)
  "Return `(LEVEL . TEXT)' for SECTION, or nil."
  (catch 'found
    (dolist (child (woerterbuch-wiktionary--element-children section))
      (when (or (eq (dom-tag child) 'div)
                (memq (dom-tag child) '(h1 h2 h3 h4 h5 h6)))
        (let ((heading
               (or (and (memq (dom-tag child) '(h1 h2 h3 h4 h5 h6))
                        child)
                   (woerterbuch-wiktionary--find-first
                    child
                    (lambda (node)
                      (memq (dom-tag node) '(h1 h2 h3 h4 h5 h6)))))))
          (when heading
            (throw 'found
                   (cons (string-to-number
                          (substring (symbol-name (dom-tag heading)) 1))
                         (woerterbuch-wiktionary--text heading)))))))
    nil))

(defun woerterbuch-wiktionary--collect-sections (node predicate)
  "Return descendant section nodes below NODE matching PREDICATE."
  (let (acc)
    (dolist (child (woerterbuch-wiktionary--element-children node))
      (when (eq (dom-tag child) 'section)
        (when (funcall predicate child)
          (push child acc))
        (setq acc
              (nconc (nreverse
                      (woerterbuch-wiktionary--collect-sections child predicate))
                     acc)))
      (unless (eq (dom-tag child) 'section)
        (setq acc
              (nconc (nreverse
                      (woerterbuch-wiktionary--collect-sections child predicate))
                     acc))))
    (nreverse acc)))

(defun woerterbuch-wiktionary--word-class-from-heading (heading)
  "Extract lexical class from rendered HEADING."
  (car (split-string heading ", " t)))

(defun woerterbuch-wiktionary--supported-heading-p (heading)
  "Return non-nil when rendered HEADING is currently supported."
  (let ((word-class (woerterbuch-wiktionary--word-class-from-heading heading)))
    (and word-class
         (not (string-empty-p word-class)))))

(defun woerterbuch-wiktionary--heading-like-node-p (node)
  "Return non-nil when NODE looks like a rendered Wiktionary block heading."
  (and (memq (dom-tag node) '(p div))
       (let ((text (woerterbuch-wiktionary--text node))
             (style (or (dom-attr node 'style) ""))
             (title (dom-attr node 'title)))
         (and text
              (string-suffix-p ":" text)
              (or (string-match-p "font-weight:[[:space:]]*bold" style)
                  title)))))

(defun woerterbuch-wiktionary--normalize-block-label (text)
  "Return normalized Wiktionary block label for TEXT, or nil."
  (let ((clean (woerterbuch-wiktionary--clean-text text)))
    (when clean
      (setq clean (replace-regexp-in-string ":\\'" "" clean))
      (car (rassoc (assoc-default clean woerterbuch-wiktionary--label-map)
                   woerterbuch-wiktionary--label-map)))))

(defun woerterbuch-wiktionary--label-key (text)
  "Return internal block key for rendered label TEXT."
  (cdr (assoc (woerterbuch-wiktionary--normalize-block-label text)
              woerterbuch-wiktionary--label-map)))

(defun woerterbuch-wiktionary--collect-labeled-blocks (section)
  "Return alist of labeled direct content blocks inside SECTION."
  (let ((children (woerterbuch-wiktionary--element-children section))
        current-key
        current-nodes
        blocks)
    (dolist (child children)
      (cond
       ((eq (dom-tag child) 'section)
        (when current-key
          (push (cons current-key (nreverse current-nodes)) blocks)
          (setq current-key nil
                current-nodes nil)))
       ((woerterbuch-wiktionary--heading-like-node-p child)
        (let ((label-key (woerterbuch-wiktionary--label-key
                          (woerterbuch-wiktionary--text child))))
          (when current-key
            (push (cons current-key (nreverse current-nodes)) blocks))
          (setq current-key label-key
                current-nodes nil)))
       (current-key
        (push child current-nodes))))
    (when current-key
      (push (cons current-key (nreverse current-nodes)) blocks))
    (nreverse blocks)))

(defun woerterbuch-wiktionary--block-nodes (blocks key)
  "Return concatenated block nodes from BLOCKS for KEY."
  (let (out)
    (dolist (entry blocks)
      (when (eq (car entry) key)
        (setq out (nconc out (copy-sequence (cdr entry))))))
    out))

(defun woerterbuch-wiktionary--descendants-by-tags (node tags)
  "Return all descendants of NODE whose tag is contained in TAGS."
  (let (out)
    (dolist (tag tags)
      (setq out (nconc out (woerterbuch-wiktionary--descendants-by-tag node tag))))
    out))

(defun woerterbuch-wiktionary--item-nodes (nodes)
  "Return meaningful list-item nodes from NODES.
Prefer `dd' descendants, but fall back to `li' descendants for entries whose
rendered blocks are plain ordered or unordered lists."
  (let (dds lis)
    (dolist (node nodes)
      (setq dds (nconc dds (woerterbuch-wiktionary--descendants-by-tag node 'dd)))
      (setq lis (nconc lis (woerterbuch-wiktionary--descendants-by-tag node 'li))))
    (or dds lis)))

(defun woerterbuch-wiktionary--item-texts (nodes)
  "Return full readable texts from meaningful list items in NODES.
This preserves inline qualifier text such as register, domain, or style
markers because it is used for definitions and examples."
  (let (out)
    (dolist (item (woerterbuch-wiktionary--item-nodes nodes))
      (let ((text (woerterbuch-wiktionary--text item)))
        (unless (string-empty-p text)
          (push text out))))
    (nreverse out)))

(defun woerterbuch-wiktionary--plain-texts (nodes)
  "Return readable texts for NODES."
  (let (out)
    (dolist (node nodes)
      (let ((text (woerterbuch-wiktionary--text node)))
        (unless (string-empty-p text)
          (push text out))))
    (nreverse out)))

(defun woerterbuch-wiktionary--descendants-by-tag-in-nodes (nodes tag)
  "Return all descendants with TAG across NODES."
  (let (out)
    (dolist (node nodes)
      (setq out
            (nconc out (woerterbuch-wiktionary--descendants-by-tag node tag))))
    out))

(defun woerterbuch-wiktionary--parse-sense-text (text)
  "Parse Wiktionary sense TEXT into `(LABEL . REST)'."
  (when (and text
             (string-match "^\\[\\([^]]+\\)\\][[:space:]]*\\(.*\\)$" text))
    (cons (match-string 1 text)
          (woerterbuch-wiktionary--clean-content-text (match-string 2 text)))))

(defun woerterbuch-wiktionary--expand-sense-label (label)
  "Expand LABEL like `1, 2' or `1-3' into a list of individual sense labels."
  (let (labels)
    (dolist (part (split-string (or label "") "[[:space:]]*,[[:space:]]*" t))
      (let ((clean (string-trim part)))
        (cond
         ((string-match
           "\\`\\([0-9]+\\)[[:space:]]*[–-][[:space:]]*\\([0-9]+\\)\\'" clean)
          (let ((start (string-to-number (match-string 1 clean)))
                (end (string-to-number (match-string 2 clean))))
            (when (<= start end)
              (dotimes (offset (1+ (- end start)))
                (push (number-to-string (+ start offset)) labels)))))
         ((not (string-empty-p clean))
          (push clean labels)))))
    (nreverse labels)))

(defun woerterbuch-wiktionary--extract-link-texts (node &optional skip)
  "Return readable descendant link texts from NODE.
This is for synonym-like item lists, not full definitions.  Links below
formatting/meta tags such as `i', `em', and `sup' are ignored so qualifier
links like \"familiär:\" do not become items.
When SKIP is non-nil, links below NODE are ignored."
  (let ((tag (and (listp node) (dom-tag node))))
    (cond
     ((stringp node) nil)
     ((null node) nil)
     ((and (eq tag 'a) (not skip))
      (let ((href (or (dom-attr node 'href) ""))
            (text (woerterbuch-wiktionary--clean-content-text
                   (woerterbuch-wiktionary--text node))))
        (if (or (string-prefix-p "#" href)
                (string-suffix-p ":" text)
                (string-empty-p text))
            nil
          (list text))))
     ((listp node)
      (let ((child-skip (or skip (memq tag '(i em sup)))))
        (apply #'nconc
               (mapcar (lambda (child)
                         (woerterbuch-wiktionary--extract-link-texts child
                                                                     child-skip))
                       (dom-children node)))))
     (t nil))))

(defun woerterbuch-wiktionary--parse-sense-items-from-dd (dd)
  "Parse one synonym-like DD node into `(LABELS . ITEMS)'."
  (let* ((text (woerterbuch-wiktionary--text dd))
         (sense-pair (woerterbuch-wiktionary--parse-sense-text text))
         (labels (and sense-pair
                      (woerterbuch-wiktionary--expand-sense-label
                       (car sense-pair))))
         (items (delete-dups
                 (or (woerterbuch-wiktionary--extract-link-texts dd)
                     (woerterbuch-wiktionary--split-list-items
                      (and sense-pair (cdr sense-pair)))))))
    (when (and labels items)
      (cons labels items))))

(defun woerterbuch-wiktionary--split-list-items (text)
  "Split comma-separated TEXT into readable items."
  (delq nil
        (mapcar
         (lambda (item)
           (let ((clean (woerterbuch-wiktionary--clean-content-text item)))
             (unless (string-empty-p clean)
               clean)))
         (split-string (or text "") "[[:space:]]*,[[:space:]]*" t))))

(defun woerterbuch-wiktionary--group-sense-items
    (sense-pairs &optional splitter)
  "Group SENSE-PAIRS into `:sense' / `:items' plists.
When SPLITTER is non-nil it is called with the sense text and should
return a list of item strings."
  (let ((table (make-hash-table :test #'equal))
        order)
    (dolist (pair sense-pairs)
      (let* ((sense (car pair))
             (text (cdr pair))
             (bucket (gethash sense table))
             (items (if splitter
                        (funcall splitter text)
                      (and text (list text)))))
        (unless (member sense order)
          (push sense order))
        (when items
          (puthash sense (delete-dups (nconc bucket (copy-sequence items)))
                   table))))
    (cl-loop for sense in (nreverse order)
             for items = (gethash sense table)
             when items
             collect (list :sense sense :items items))))

(defun woerterbuch-wiktionary--definition-list (blocks sections)
  "Parse definition objects from BLOCKS according to SECTIONS.
Definitions are parsed from full `dd' text via
`woerterbuch-wiktionary--dd-texts', not from extracted link texts, so inline
qualifiers are preserved."
  (let* ((definition-pairs
          (delq nil
                (mapcar #'woerterbuch-wiktionary--parse-sense-text
                        (woerterbuch-wiktionary--item-texts
                         (woerterbuch-wiktionary--block-nodes
                          blocks :definitions)))))
         (example-pairs
          (and (woerterbuch-core-section-requested-p :examples sections)
               (delq nil
                     (mapcar #'woerterbuch-wiktionary--parse-sense-text
                             (woerterbuch-wiktionary--item-texts
                              (woerterbuch-wiktionary--block-nodes
                               blocks :examples))))))
         (example-table (make-hash-table :test #'equal)))
    (dolist (pair example-pairs)
      (dolist (label (woerterbuch-wiktionary--expand-sense-label (car pair)))
        (puthash label
                 (delete-dups
                  (nconc (gethash label example-table)
                         (list (cdr pair))))
                 example-table)))
    (cl-loop for pair in definition-pairs
             for idx from 1
             collect
             (list :id idx
                   :label (car pair)
                   :definition (cdr pair)
                   :qualifiers nil
                   :examples (gethash (car pair) example-table)
                   :definitions nil))))

(defun woerterbuch-wiktionary--origin-text (blocks)
  "Return origin text parsed from BLOCKS."
  (let* ((nodes (woerterbuch-wiktionary--block-nodes blocks :origin))
         (texts (or (woerterbuch-wiktionary--item-texts nodes)
                    (woerterbuch-wiktionary--plain-texts nodes))))
    (when texts
      (woerterbuch-wiktionary--clean-origin-text
       (string-join texts " ")))))

(defun woerterbuch-wiktionary--idioms (blocks)
  "Return idiom texts parsed from BLOCKS.
Rules:
- Parse idioms from `dd' nodes in the idioms block.
- Prefer explicit wiki link texts over fallback text.
- If a `dd' contains multiple wiki links, join all link texts with
  \"; \" and return them as one idiom entry.
- Use cleaned fallback text only when no wiki link text exists.
- In fallback text, discard explanations after a spaced dash separator.
- Drop empty idioms and remove duplicates while preserving order."
  (let (idioms)
    (dolist (item (woerterbuch-wiktionary--item-nodes
                   (woerterbuch-wiktionary--block-nodes blocks :idioms)))
      (let* ((raw-text (woerterbuch-wiktionary--text item))
             (sense-pair (woerterbuch-wiktionary--parse-sense-text raw-text))
             (link-texts (woerterbuch-wiktionary--extract-link-texts item))
             (fallback-text (woerterbuch-wiktionary--clean-content-text
                             (or (and sense-pair (cdr sense-pair))
                                 raw-text)))
             (fallback-text
              (and fallback-text
                   (car (split-string fallback-text
                                      "[[:space:]]+[–-][[:space:]]+"
                                      t))))
             (idiom (or (and link-texts
                             (string-join link-texts "; "))
                        fallback-text)))
        (when (and idiom (not (string-empty-p idiom)))
          (push idiom idioms))))
    (nreverse (delete-dups (nreverse idioms)))))

(defun woerterbuch-wiktionary--synonyms (blocks)
  "Return synonym groups parsed from BLOCKS."
  (let ((table (make-hash-table :test #'equal))
        order)
    (dolist (item (append
                   (woerterbuch-wiktionary--item-nodes
                    (woerterbuch-wiktionary--block-nodes blocks :synonyms))
                   (woerterbuch-wiktionary--item-nodes
                    (woerterbuch-wiktionary--block-nodes
                     blocks
                     :related-synonyms))))
      (let ((parsed (woerterbuch-wiktionary--parse-sense-items-from-dd item)))
        (when parsed
          (dolist (sense (car parsed))
            (unless (member sense order)
              (push sense order))
            (puthash sense
                     (delete-dups
                      (nconc (gethash sense table)
                             (copy-sequence (cdr parsed))))
                     table)))))
    (cl-loop for sense in (nreverse order)
             for items = (gethash sense table)
             when items
             collect (list :sense sense :items items))))

(defun woerterbuch-wiktionary--homograph-title (lemma heading)
  "Create a readable homograph title from LEMMA and rendered HEADING."
  (string-join (delq nil (list lemma heading)) ", "))

(defun woerterbuch-wiktionary--parse-entry-section
    (lemma section id sections url)
  "Parse one rendered Wiktionary SECTION for LEMMA into a homograph plist."
  (let*
      ((heading-info
        (woerterbuch-wiktionary--heading-node-level-and-text section))
       (heading (cdr heading-info))
       (word-class (woerterbuch-wiktionary--word-class-from-heading heading))
       (blocks (woerterbuch-wiktionary--collect-labeled-blocks section))
       (origin (and (woerterbuch-core-section-requested-p :origin sections)
                    (woerterbuch-wiktionary--origin-text blocks)))
       (idioms (and (woerterbuch-core-section-requested-p :idioms sections)
                    (woerterbuch-wiktionary--idioms blocks)))
       (synonyms (and
                  (woerterbuch-core-section-requested-p :synonyms sections)
                  (woerterbuch-wiktionary--synonyms blocks))))
    (list :id id
          :lemma lemma
          :title (woerterbuch-wiktionary--homograph-title lemma heading)
          :wortart word-class
          :grammar word-class
          :origin origin
          :idioms idioms
          :synonyms synonyms
          :url url
          :definitions (woerterbuch-wiktionary--definition-list blocks sections))))

(defun woerterbuch-wiktionary--substantive-entry-p (homograph)
  "Return non-nil when HOMOGRAPH carries useful extracted content."
  (or (plist-get homograph :definitions)
      (plist-get homograph :origin)
      (plist-get homograph :idioms)
      (plist-get homograph :synonyms)))

(defun woerterbuch-wiktionary--canonical-url (dom fallback)
  "Return canonical page URL from DOM or FALLBACK."
  (let ((link (woerterbuch-wiktionary--find-first
               dom
               (lambda (node)
                 (and (eq (dom-tag node) 'link)
                      (equal (dom-attr node 'rel) "canonical"))))))
    (or (and link (dom-attr link 'href)) fallback)))

(defun woerterbuch-wiktionary--page-title (dom fallback)
  "Return visible page title from DOM or FALLBACK."
  (let ((node
         (woerterbuch-wiktionary--find-first
          dom
          (lambda (candidate)
            (and (eq (dom-tag candidate) 'span)
                 (woerterbuch-wiktionary--has-class-p
                  candidate
                  "mw-page-title-main"))))))
    (or (and node (woerterbuch-wiktionary--text node)) fallback)))

(defun woerterbuch-wiktionary--parse-html-dom (dom input sections)
  "Parse rendered Wiktionary DOM DOM for INPUT according to SECTIONS."
  (let* ((root (woerterbuch-wiktionary--find-first
                dom
                (lambda (node)
                  (woerterbuch-wiktionary--has-class-p
                   node
                   "mw-parser-output"))))
         (lemma (woerterbuch-wiktionary--page-title dom input))
         (url (woerterbuch-wiktionary--canonical-url
               dom
               (woerterbuch-wiktionary--build-web-url lemma)))
         (language-sections
          (and root
               (woerterbuch-wiktionary--collect-sections
                root
                (lambda (section)
                  (let ((info
                         (woerterbuch-wiktionary--heading-node-level-and-text
                          section)))
                    (and info
                         (= (car info) 2)
                         (string-match-p "Deutsch" (cdr info))))))))
         (homograph-sections
          (apply #'nconc
                 (mapcar
                  (lambda (section)
                    (woerterbuch-wiktionary--collect-sections
                     section
                     (lambda (candidate)
                       (let ((info
                              (woerterbuch-wiktionary--heading-node-level-and-text
                               candidate)))
                         (and info
                              (= (car info) 3)
                              (woerterbuch-wiktionary--supported-heading-p
                               (cdr info)))))))
                  language-sections)))
         (homographs
          (cl-loop for section in homograph-sections
                   for idx from 1
                   for homograph =
                   (woerterbuch-wiktionary--parse-entry-section
                    lemma
                    section
                    idx
                    sections
                    url)
                   when (woerterbuch-wiktionary--substantive-entry-p homograph)
                   collect homograph)))
    (if (null homographs)
        (let ((result (woerterbuch-core-make-error
                       'wiktionary
                       (or lemma input)
                       "No matches found")))
          (plist-put result :url
                     (or url
                         (woerterbuch-wiktionary--build-web-url
                          (or lemma input)))))
      (let
          ((result (woerterbuch-core-make-result 'wiktionary (or lemma input))))
        (setq result (plist-put result :url url))
        (plist-put result :homographs homographs)))))

(defun woerterbuch-wiktionary--parse-current-buffer (input sections)
  "Parse current HTTP buffer as rendered Wiktionary page for INPUT and SECTIONS."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil t))
  (skip-chars-forward "\r\n")
  (condition-case nil
      (woerterbuch-wiktionary--parse-html-dom
       (libxml-parse-html-region (point) (point-max))
       input
       sections)
    (error
     (let ((result (woerterbuch-core-make-error
                    'wiktionary input "Failed to parse Wiktionary HTML")))
       (plist-put result :url (woerterbuch-wiktionary--build-web-url input))))))

(defun woerterbuch-wiktionary--status-http-code (status)
  "Return HTTP code inferred from STATUS or current buffer context."
  (or (and (boundp 'url-http-response-status)
           (numberp url-http-response-status)
           url-http-response-status)
      (let ((err (plist-get status :error)))
        (when (and (listp err)
                   (eq (car err) 'error)
                   (eq (cadr err) 'http)
                   (numberp (caddr err)))
          (caddr err)))))

(defun woerterbuch-wiktionary--status-network-error-p (status)
  "Return non-nil when STATUS represents a non-HTTP network error."
  (let ((err (plist-get status :error)))
    (and err
         (not (woerterbuch-wiktionary--status-http-code status)))))

(defun woerterbuch-wiktionary--request-needed-p (sections)
  "Return non-nil when Wiktionary can contribute anything for SECTIONS."
  (or (woerterbuch-core-section-requested-p :definitions sections)
      (woerterbuch-core-section-requested-p :examples sections)
      (woerterbuch-core-section-requested-p :origin sections)
      (woerterbuch-core-section-requested-p :idioms sections)
      (woerterbuch-core-section-requested-p :synonyms sections)))

(defun woerterbuch-wiktionary--request-callback (status input sections callback)
  "Handle Wiktionary page response STATUS for INPUT and SECTIONS.

Invoke CALLBACK with the parsed result."
  (let (result)
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((woerterbuch-wiktionary--status-network-error-p status)
                    (woerterbuch-core-make-error
                     'wiktionary
                     input
                     (format "Network error: %S" (plist-get status :error))))
                   ((let ((http-code
                           (woerterbuch-wiktionary--status-http-code status)))
                      (and http-code
                           (>= http-code 400)))
                    (woerterbuch-core-make-error
                     'wiktionary
                     input
                     (format "HTTP error: %s"
                             (woerterbuch-wiktionary--status-http-code status))))
                   (t
                    (woerterbuch-wiktionary--parse-current-buffer input sections)))
                (error
                 (woerterbuch-core-make-error
                  'wiktionary
                  input
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-wiktionary-fetch (input sections callback)
  "Fetch INPUT from German Wiktionary for SECTIONS and invoke CALLBACK once."
  (if (not (woerterbuch-wiktionary--request-needed-p sections))
      (let ((result (woerterbuch-core-make-result 'wiktionary input)))
        (setq result
              (plist-put result :url
                         (woerterbuch-wiktionary--build-web-url input)))
        (funcall callback result))
    (let ((url-request-extra-headers woerterbuch-wiktionary-request-headers))
      (url-retrieve
       (woerterbuch-wiktionary--build-web-url input)
       #'woerterbuch-wiktionary--request-callback
       (list input sections callback)
       t
       t))))

(provide 'woerterbuch-wiktionary)

;;; woerterbuch-wiktionary.el ends here
