;;; woerterbuch-duden.el --- Duden backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Duden backend implementation and offline parsing helpers.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'woerterbuch-core)

(defconst woerterbuch-duden-base-url
  "https://www.duden.de/rechtschreibung/"
  "Base URL for Duden dictionary pages.")

(defconst woerterbuch-duden-search-url
  "https://www.duden.de/suchen/dudenonline/"
  "Base URL for Duden search pages.")

(defconst woerterbuch-duden-request-headers
  '(("User-Agent"
     . "Mozilla/5.0 (Windows NT 10.0; rv:109.0) Gecko/20100101 Firefox/115.0")
    ("Accept"
     . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    ("Accept-Language" . "en-US,en;q=0.5")
    ("Accept-Encoding" . "gzip, deflate, br")
    ("DNT" . "1")
    ("Connection" . "keep-alive"))
  "HTTP headers used for Duden requests, mimicking Tor Browser for better privacy.")

(defun woerterbuch-duden--build-url (lemma)
  "Build Duden AMP page URL for LEMMA."
  (concat woerterbuch-duden-base-url
          (url-hexify-string
           (replace-regexp-in-string "[[:space:]]+" "_" lemma))
          "?amp"))

(defun woerterbuch-duden--build-search-url (lemma)
  "Build Duden search URL for LEMMA."
  (concat woerterbuch-duden-search-url
          (url-hexify-string lemma)))

(defun woerterbuch-duden--clean-text (string)
  "Normalize whitespace and punctuation spacing in STRING."
  (when string
    (let ((s (string-trim
              (replace-regexp-in-string "[[:space:] ]+" " " string))))
      (setq s (replace-regexp-in-string "〈" "⟨" s))
      (setq s (replace-regexp-in-string "〉" "⟩" s))
      (setq s (replace-regexp-in-string " +," "," s))
      (setq s (replace-regexp-in-string " +\\." "." s))
      (setq s (replace-regexp-in-string "( +" "(" s))
      (setq s (replace-regexp-in-string " +)" ")" s))
      s)))

(defun woerterbuch-duden--text (node)
  "Return normalized text content for NODE."
  (woerterbuch-duden--clean-text
   (cond
    ((null node) "")
    ((stringp node) node)
    ((listp node)
     (mapconcat #'woerterbuch-duden--text (dom-children node) " "))
    (t ""))))

(defun woerterbuch-duden--class-list (node)
  "Return CSS classes for NODE."
  (split-string (or (dom-attr node 'class) "") "[[:space:]]+" t))

(defun woerterbuch-duden--has-class-p (node class)
  "Return non-nil when NODE has CSS CLASS."
  (member class (woerterbuch-duden--class-list node)))

(defun woerterbuch-duden--element-children (node)
  "Return element children of NODE."
  (seq-filter #'listp (dom-children node)))

(defun woerterbuch-duden--children-with-class (node class)
  "Return direct child elements of NODE having CLASS."
  (seq-filter (lambda (child)
                (woerterbuch-duden--has-class-p child class))
              (woerterbuch-duden--element-children node)))

(defun woerterbuch-duden--first-child-with-class (node class)
  "Return first direct child element of NODE having CLASS."
  (car (woerterbuch-duden--children-with-class node class)))

(defun woerterbuch-duden--descendants-with-class (node class)
  "Return all descendants of NODE having CLASS."
  (when node (dom-by-class node class)))

(defun woerterbuch-duden--find-first (node predicate)
  "Return first descendant of NODE matching PREDICATE."
  (catch 'found
    (dolist (child (woerterbuch-duden--element-children node))
      (when (funcall predicate child)
        (throw 'found child))
      (let ((match (woerterbuch-duden--find-first child predicate)))
        (when match
          (throw 'found match))))
    nil))

(defun woerterbuch-duden--canonical-url (dom fallback)
  "Return canonical page URL from DOM or FALLBACK."
  (let ((link (woerterbuch-duden--find-first
               dom
               (lambda (node)
                 (and (eq (dom-tag node) 'link)
                      (equal (dom-attr node 'rel) "canonical"))))))
    (or (and link (dom-attr link 'href)) fallback)))

(defun woerterbuch-duden--normalize-key (string)
  "Normalize tuple or note title STRING."
  (let ((s (woerterbuch-duden--clean-text string)))
    (when s
      (setq s (replace-regexp-in-string " *ⓘ" "" s))
      (setq s (replace-regexp-in-string ":\\'" "" s))
      (string-trim s))))

(defun woerterbuch-duden--tuple-pairs (node)
  "Return direct tuple pairs below NODE."
  (let (pairs)
    (dolist (dl (woerterbuch-duden--children-with-class node "tuple"))
      (when (eq (dom-tag dl) 'dl)
        (let* ((dt (woerterbuch-duden--find-first
                    dl
                    (lambda (child) (eq (dom-tag child) 'dt))))
               (dd (woerterbuch-duden--find-first
                    dl
                    (lambda (child) (eq (dom-tag child) 'dd))))
               (key (woerterbuch-duden--normalize-key
                     (woerterbuch-duden--text dt)))
               (val (woerterbuch-duden--text dd)))
          (when (and key (not (string-empty-p key))
                     val (not (string-empty-p val)))
            (push (cons key val) pairs)))))
    (nreverse pairs)))

(defun woerterbuch-duden--notes (node)
  "Return direct note blocks below NODE."
  (let (notes)
    (dolist (dl (woerterbuch-duden--children-with-class node "note"))
      (when (eq (dom-tag dl) 'dl)
        (let* ((dt (woerterbuch-duden--find-first
                    dl
                    (lambda (child) (eq (dom-tag child) 'dt))))
               (key (woerterbuch-duden--normalize-key
                     (woerterbuch-duden--text dt)))
               (items
                (delq nil
                      (mapcar
                       (lambda (li)
                         (let ((text (woerterbuch-duden--text li)))
                           (unless (string-empty-p text) text)))
                       (woerterbuch-duden--find-all-li dl)))))
          (when (and key items)
            (push (cons key items) notes)))))
    (nreverse notes)))

(defun woerterbuch-duden--find-all-li (node)
  "Return all `li' descendants below NODE."
  (let (acc)
    (dolist (child (woerterbuch-duden--element-children node))
      (when (eq (dom-tag child) 'li)
        (push child acc))
      (setq acc (nconc (nreverse (woerterbuch-duden--find-all-li child)) acc)))
    (nreverse acc)))

(defun woerterbuch-duden--direct-child-by-tag-and-class (node tag class)
  "Return first direct child of NODE matching TAG and CLASS."
  (seq-find (lambda (child)
              (and (eq (dom-tag child) tag)
                   (or (null class)
                       (woerterbuch-duden--has-class-p child class))))
            (woerterbuch-duden--element-children node)))

(defun woerterbuch-duden--direct-children-by-tag-and-class (node tag class)
  "Return direct children of NODE matching TAG and CLASS."
  (seq-filter (lambda (child)
                (and (eq (dom-tag child) tag)
                     (or (null class)
                         (woerterbuch-duden--has-class-p child class))))
              (woerterbuch-duden--element-children node)))

(defun woerterbuch-duden--definition-label (node fallback)
  "Return human-visible label for meaning NODE, or FALLBACK."
  (let ((raw-id (dom-attr node 'id)))
    (cond
     ((and raw-id
           (string-match "\\`Bedeutung-\\([0-9]+[a-z]?\\)\\'" raw-id))
      (match-string 1 raw-id))
     (t fallback))))

(defun woerterbuch-duden--extract-image-url (node)
  "Return image URL from direct depiction below NODE."
  (let* ((figure (woerterbuch-duden--direct-child-by-tag-and-class
                  node 'figure "depiction"))
         (link (and figure
                    (woerterbuch-duden--find-first
                     figure
                     (lambda (child) (eq (dom-tag child) 'a)))))
         (href (and link (dom-attr link 'href))))
    (unless (or (null href) (string-empty-p href))
      href)))

(defun woerterbuch-duden--extract-qualifiers (node)
  "Return qualifiers for meaning NODE."
  (let ((pairs (woerterbuch-duden--tuple-pairs node)))
    (mapcar (lambda (pair)
              (format "%s: %s" (car pair) (cdr pair)))
            (seq-remove (lambda (pair)
                          (string-equal (car pair) "Kurzform für"))
                        pairs))))

(defun woerterbuch-duden--extract-shortform-definition (node)
  "Return a definition string for a leading `Kurzform für' tuple in NODE."
  (let ((first-child (car (woerterbuch-duden--element-children node))))
    (when (and first-child
               (eq (dom-tag first-child) 'dl)
               (woerterbuch-duden--has-class-p first-child "tuple"))
      (let* ((pairs (woerterbuch-duden--tuple-pairs node))
             (pair (car pairs)))
        (when (and pair (string-equal (car pair) "Kurzform für"))
          (format "%s: %s" (car pair) (cdr pair)))))))

(defun woerterbuch-duden--note-values (notes title)
  "Return note values from NOTES for TITLE."
  (cdr (assoc title notes)))

(defun woerterbuch-duden--extract-definition-node (node index sections label)
  "Parse one Duden meaning NODE as definition INDEX according to SECTIONS.

LABEL is the human-visible numbering label."
  (let* ((text-node (woerterbuch-duden--direct-child-by-tag-and-class
                     node 'div "enumeration__text"))
         (notes (woerterbuch-duden--notes node))
         (sub-ol (woerterbuch-duden--direct-child-by-tag-and-class
                  node 'ol "enumeration__sub"))
         (sub-items (and sub-ol
                         (woerterbuch-duden--direct-children-by-tag-and-class
                          sub-ol 'li "enumeration__sub-item")))
         (want-examples
          (woerterbuch-core-section-requested-p :examples sections))
         (want-idioms
          (woerterbuch-core-section-requested-p :idioms sections))
         (definition
          (or
           (let ((txt (woerterbuch-duden--text text-node)))
             (unless (string-empty-p txt) txt))
           (woerterbuch-duden--extract-shortform-definition node)))
         (qualifiers (woerterbuch-duden--extract-qualifiers node))
         (image (woerterbuch-duden--extract-image-url node))
         (children
          (cl-loop for child in sub-items
                   for idx from 1
                   collect
                   (woerterbuch-duden--extract-definition-node
                    child idx sections
                    (woerterbuch-duden--definition-label
                     child
                     (format "%s%c" label (+ ?a (1- idx))))))))
    (list :id index
          :duden-id (dom-attr node 'id)
          :label label
          :definition definition
          :qualifiers qualifiers
          :examples (and want-examples
                         (or (woerterbuch-duden--note-values notes "Beispiele")
                             (woerterbuch-duden--note-values notes "Beispiel")))
          :idioms (and want-idioms
                       (woerterbuch-duden--note-values
                        notes
                        "Wendungen, Redensarten, Sprichwörter"))
          :image image
          :definitions children)))

(defun woerterbuch-duden--parse-single-definition-section (section sections)
  "Parse flat singular meaning SECTION according to SECTIONS."
  (let* ((notes (woerterbuch-duden--notes section))
         (want-examples
          (woerterbuch-core-section-requested-p :examples sections))
         (want-idioms
          (woerterbuch-core-section-requested-p :idioms sections))
         (definition
          (or
           (let ((p (seq-find (lambda (child) (eq (dom-tag child) 'p))
                              (woerterbuch-duden--element-children section))))
             (let ((txt (woerterbuch-duden--text p)))
               (unless (string-empty-p txt) txt)))
           (let ((text-node (woerterbuch-duden--direct-child-by-tag-and-class
                             section 'div "enumeration__text")))
             (let ((txt (woerterbuch-duden--text text-node)))
               (unless (string-empty-p txt) txt)))
           (woerterbuch-duden--extract-shortform-definition section))))
    (when definition
      (list
       (list :id 1
             :duden-id nil
             :label "1"
             :definition definition
             :qualifiers (woerterbuch-duden--extract-qualifiers section)
             :examples (and want-examples
                            (or
                             (woerterbuch-duden--note-values notes "Beispiele")
                             (woerterbuch-duden--note-values notes "Beispiel")))
             :idioms (and want-idioms
                          (woerterbuch-duden--note-values
                           notes
                           "Wendungen, Redensarten, Sprichwörter"))
             :image (woerterbuch-duden--extract-image-url section)
             :definitions nil)))))

(defun woerterbuch-duden--parse-definitions (dom sections)
  "Parse Duden definitions from DOM according to SECTIONS."
  (let* ((section (woerterbuch-duden--find-first
                   dom
                   (lambda (node)
                     (member (dom-attr node 'id)
                             '("bedeutungen" "bedeutung")))))
         (ol (and section
                  (woerterbuch-duden--find-first
                   section
                   (lambda (node)
                     (and (eq (dom-tag node) 'ol)
                          (woerterbuch-duden--has-class-p node "enumeration"))))))
         (items (and ol
                     (woerterbuch-duden--direct-children-by-tag-and-class
                      ol 'li "enumeration__item"))))
    (if items
        (cl-loop for item in items
                 for idx from 1
                 collect
                 (woerterbuch-duden--extract-definition-node
                  item idx sections (number-to-string idx)))
      (woerterbuch-duden--parse-single-definition-section section sections))))

(defun woerterbuch-duden--extract-title-node (dom)
  "Return Duden title node from DOM."
  (woerterbuch-duden--find-first
   dom
   (lambda (node)
     (and (eq (dom-tag node) 'h1)
          (woerterbuch-duden--has-class-p node "lemma__title")))))

(defun woerterbuch-duden--extract-lemma (title-node fallback)
  "Extract lemma from TITLE-NODE or use FALLBACK."
  (let* ((main (and title-node
                    (woerterbuch-duden--find-first
                     title-node
                     (lambda (node)
                       (woerterbuch-duden--has-class-p node "lemma__main")))))
         (txt (woerterbuch-duden--text main)))
    (if (and txt (not (string-empty-p txt))) txt fallback)))

(defun woerterbuch-duden--extract-title (title-node fallback)
  "Extract visible title from TITLE-NODE or use FALLBACK."
  (let ((txt (woerterbuch-duden--text title-node)))
    (if (and txt (not (string-empty-p txt))) txt fallback)))

(defun woerterbuch-duden--field-value (dom label)
  "Extract page-level tuple value for LABEL from DOM."
  (cl-loop
   for dl in (woerterbuch-duden--descendants-with-class dom "tuple")
   when (eq (dom-tag dl) 'dl)
   for dt = (woerterbuch-duden--find-first
             dl
             (lambda (child) (eq (dom-tag child) 'dt)))
   for dd = (woerterbuch-duden--find-first
             dl
             (lambda (child) (eq (dom-tag child) 'dd)))
   for key = (woerterbuch-duden--normalize-key
              (woerterbuch-duden--text dt))
   when (string-equal key label)
   return (woerterbuch-duden--text dd)))

(defun woerterbuch-duden--wortart-from-grammar (grammar)
  "Extract coarse word class from GRAMMAR."
  (when (and grammar (not (string-empty-p grammar)))
    (string-trim (car (split-string grammar "," t)))))

(defun woerterbuch-duden--extract-origin (dom)
  "Extract origin text from DOM."
  (let ((section (woerterbuch-duden--find-first
                  dom
                  (lambda (node)
                    (equal (dom-attr node 'id) "herkunft")))))
    (when section
      (let ((parts
             (delq nil
                   (mapcar
                    (lambda (child)
                      (unless (memq (dom-tag child) '(header small nav))
                        (let ((txt (woerterbuch-duden--text child)))
                          (unless (string-empty-p txt) txt))))
                    (woerterbuch-duden--element-children section)))))
        (unless (null parts)
          (string-join parts " "))))))

(defun woerterbuch-duden--split-synonym-text (string)
  "Split synonym STRING on commas and semicolons outside parentheses."
  (let ((parts nil)
        (current "")
        (depth 0))
    (dolist (char (string-to-list (or string "")))
      (cond
       ((eq char ?\()
        (setq depth (1+ depth))
        (setq current (concat current (string char))))
       ((eq char ?\))
        (setq depth (max 0 (1- depth)))
        (setq current (concat current (string char))))
       ((and (= depth 0) (memq char '(?, ?\;)))
        (push current parts)
        (setq current ""))
       (t
        (setq current (concat current (string char))))))
    (push current parts)
    (delq nil
          (mapcar (lambda (part)
                    (let ((txt (woerterbuch-duden--clean-text part)))
                      (unless (string-empty-p txt) txt)))
                  (nreverse parts)))))

(defun woerterbuch-duden--extract-synonyms (dom)
  "Extract synonyms from DOM."
  (let* ((section (woerterbuch-duden--find-first
                   dom
                   (lambda (node)
                     (equal (dom-attr node 'id) "synonyme"))))
         (ul (and section
                  (seq-find (lambda (child) (eq (dom-tag child) 'ul))
                            (woerterbuch-duden--element-children section)))))
    (when ul
      (let ((seen (make-hash-table :test #'equal))
            out)
        (dolist
            (li (woerterbuch-duden--direct-children-by-tag-and-class ul 'li nil))
          (dolist (syn (woerterbuch-duden--split-synonym-text
                        (woerterbuch-duden--text li)))
            (unless (gethash syn seen)
              (puthash syn t seen)
              (push syn out))))
        (nreverse out)))))

(defun woerterbuch-duden--find-search-segment (dom)
  "Return the Wörterbuch segment from search DOM."
  (seq-find
   (lambda (segment)
     (let ((title (woerterbuch-duden--find-first
                   segment
                   (lambda (node)
                     (woerterbuch-duden--has-class-p node "segment__title")))))
       (string-equal (woerterbuch-duden--text title) "Wörterbuch")))
   (woerterbuch-duden--descendants-with-class dom "segment")))

(defun woerterbuch-duden--search-result-lemma (label-node)
  "Return visible lemma text from search LABEL-NODE."
  (let* ((strong (and label-node
                      (woerterbuch-duden--find-first
                       label-node
                       (lambda (node) (eq (dom-tag node) 'strong)))))
         (txt (woerterbuch-duden--text strong)))
    (if (and txt (not (string-empty-p txt)))
        txt
      (woerterbuch-duden--text label-node))))

(defun woerterbuch-duden--parse-search-results (dom lemma)
  "Return exact-match entry URLs from search DOM for LEMMA."
  (let ((segment (woerterbuch-duden--find-search-segment dom))
        urls)
    (when segment
      (dolist (section (woerterbuch-duden--direct-children-by-tag-and-class
                        segment 'section "vignette"))
        (let* ((label (woerterbuch-duden--find-first
                       section
                       (lambda (node)
                         (woerterbuch-duden--has-class-p node "vignette__label"))))
               (visible (woerterbuch-duden--search-result-lemma label))
               (href (and label (dom-attr label 'href))))
          (when (and href
                     (string-prefix-p "/rechtschreibung/" href)
                     (string-equal (woerterbuch-duden--clean-text visible)
                                   (woerterbuch-duden--clean-text lemma)))
            (push (concat "https://www.duden.de" href "?amp") urls)))))
    (nreverse urls)))

(defun woerterbuch-duden--parse-dom (dom input sections url homograph-id)
  "Parse Duden entry DOM for INPUT and return one homograph plist.

SECTIONS controls which data is extracted. URL is the entry URL.
HOMOGRAPH-ID is the 1-based index assigned by the caller."
  (let* ((title-node (woerterbuch-duden--extract-title-node dom))
         (lemma (woerterbuch-duden--extract-lemma title-node input))
         (title (woerterbuch-duden--extract-title title-node lemma))
         (grammar (woerterbuch-duden--field-value dom "Wortart"))
         (resolved-url (or url
                           (woerterbuch-duden--canonical-url dom nil)))
         (want-definitions
          (or (woerterbuch-core-section-requested-p :definitions sections)
              (woerterbuch-core-section-requested-p :examples sections)
              (woerterbuch-core-section-requested-p :idioms sections)))
         (want-origin (woerterbuch-core-section-requested-p :origin sections))
         (want-synonyms
          (woerterbuch-core-section-requested-p :synonyms sections)))
    (list :id homograph-id
          :lemma lemma
          :title title
          :wortart (woerterbuch-duden--wortart-from-grammar grammar)
          :grammar grammar
          :origin (and want-origin (woerterbuch-duden--extract-origin dom))
          :idioms nil
          :synonyms (and want-synonyms
                         (woerterbuch-duden--extract-synonyms dom))
          :url resolved-url
          :definitions (and want-definitions
                            (woerterbuch-duden--parse-definitions
                             dom sections)))))

(defun woerterbuch-duden--parse-buffer-dom ()
  "Parse the current HTTP response buffer into an HTML DOM."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil t))
  (skip-chars-forward "\r\n")
  (libxml-parse-html-region (point) (point-max)))

(defun woerterbuch-duden--result-from-homographs (input homographs)
  "Build normalized Duden result for INPUT from HOMOGRAPHS."
  (let ((result (woerterbuch-core-make-result 'duden input))
        (urls (delq nil (mapcar (lambda (entry) (plist-get entry :url))
                                homographs))))
    (setq result
          (plist-put result :lemma
                     (or (plist-get (car homographs) :lemma) input)))
    (setq result (plist-put result :url urls))
    (setq result (plist-put result :homographs homographs))
    result))

(defun woerterbuch-duden--no-match-result (input)
  "Build no-match error result for INPUT."
  (let ((result (woerterbuch-core-make-error
                 'duden input "No matches found")))
    (plist-put result :url nil)))

(defun woerterbuch-duden--request-needed-p (sections)
  "Return non-nil when Duden can contribute anything for SECTIONS."
  (or (woerterbuch-core-section-requested-p :definitions sections)
      (woerterbuch-core-section-requested-p :examples sections)
      (woerterbuch-core-section-requested-p :origin sections)
      (woerterbuch-core-section-requested-p :idioms sections)
      (woerterbuch-core-section-requested-p :synonyms sections)))

(defun woerterbuch-duden--with-headers (thunk)
  "Call THUNK with Duden request headers configured."
  (let ((url-request-extra-headers woerterbuch-duden-request-headers))
    (funcall thunk)))

(defun woerterbuch-duden--status-http-code (status)
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

(defun woerterbuch-duden--status-network-error-p (status)
  "Return non-nil when STATUS represents a non-HTTP network error."
  (let ((err (plist-get status :error)))
    (and err
         (not (woerterbuch-duden--status-http-code status)))))

(defun woerterbuch-duden--fetch-search (input sections callback)
  "Fetch Duden search results for INPUT covering SECTIONS.

Continue with CALLBACK."
  (woerterbuch-duden--with-headers
   (lambda ()
     (url-retrieve
      (woerterbuch-duden--build-search-url input)
      #'woerterbuch-duden--search-callback
      (list input sections callback)
      t
      t))))

(defun woerterbuch-duden--search-callback (status input sections callback)
  "Handle Duden search response STATUS for INPUT, SECTIONS and CALLBACK."
  (let ((http-code (woerterbuch-duden--status-http-code status))
        result)
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((woerterbuch-duden--status-network-error-p status)
                    (woerterbuch-core-make-error
                     'duden input
                     (format "Network error: %S" (plist-get status :error))))
                   ((and http-code
                         (>= http-code 400)
                         (/= http-code 404))
                    (woerterbuch-core-make-error
                     'duden input
                     (format "HTTP error: %s" http-code)))
                   (t
                    (let* ((dom (woerterbuch-duden--parse-buffer-dom))
                           (urls (woerterbuch-duden--parse-search-results
                                  dom input)))
                      (if (null urls)
                          (woerterbuch-duden--no-match-result input)
                        (woerterbuch-duden--fetch-entry-urls
                         input sections urls callback)
                        :async))))
                (error
                 (woerterbuch-core-make-error
                  'duden input
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (unless (eq result :async)
      (funcall callback result))))

(defun woerterbuch-duden--fetch-entry-urls (input sections urls callback)
  "Fetch each Duden entry in URLS for INPUT and SECTIONS.

Invoke CALLBACK once."
  (let ((remaining urls)
        (homographs nil)
        (failed nil))
    (cl-labels
        ((step ()
           (cond
            (failed
             (funcall callback failed))
            ((null remaining)
             (funcall callback
                      (woerterbuch-duden--result-from-homographs
                       input (nreverse homographs))))
            (t
             (let* ((url (car remaining))
                    (index (1+ (length homographs))))
               (setq remaining (cdr remaining))
               (woerterbuch-duden--with-headers
                (lambda ()
                  (url-retrieve
                   url
                   #'woerterbuch-duden--entry-callback
                   (list input sections url index #'step
                         (lambda (entry)
                           (push entry homographs))
                         (lambda (error-result)
                           (setq failed error-result)))
                   t
                   t))))))))
      (step))))

(defun woerterbuch-duden--entry-callback
    (status input sections url index continue push-entry fail)
  "Handle one fetched Duden entry.

STATUS is the URL callback status. INPUT, SECTIONS, URL and INDEX
describe the requested entry. CONTINUE continues the sequence.
PUSH-ENTRY stores the parsed homograph. FAIL stores an error result."
  (let ((http-code (woerterbuch-duden--status-http-code status))
        result)
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((woerterbuch-duden--status-network-error-p status)
                    (woerterbuch-core-make-error
                     'duden input
                     (format "Network error: %S" (plist-get status :error))))
                   ((and http-code
                         (>= http-code 400))
                    (woerterbuch-core-make-error
                     'duden input
                     (format "HTTP error: %s" http-code)))
                   (t
                    (funcall
                     push-entry
                     (woerterbuch-duden--parse-dom
                      (woerterbuch-duden--parse-buffer-dom)
                      input sections url index))
                    :continue))
                (error
                 (woerterbuch-core-make-error
                  'duden input
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (unless (eq result :continue)
      (funcall fail result))
    (funcall continue)))

(defun woerterbuch-duden--initial-callback (status input sections callback)
  "Handle initial Duden entry lookup STATUS for INPUT and SECTIONS.
CALLBACK is handled over to follow up function."
  (let ((http-code (woerterbuch-duden--status-http-code status))
        result)
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((woerterbuch-duden--status-network-error-p status)
                    (woerterbuch-core-make-error
                     'duden input
                     (format "Network error: %S" (plist-get status :error))))
                   ((eq http-code 404)
                    (woerterbuch-duden--fetch-search input sections callback)
                    :async)
                   ((and http-code
                         (>= http-code 400))
                    (woerterbuch-core-make-error
                     'duden input
                     (format "HTTP error: %s" http-code)))
                   (t
                    (woerterbuch-duden--result-from-homographs
                     input
                     (list
                      (woerterbuch-duden--parse-dom
                       (woerterbuch-duden--parse-buffer-dom)
                       input sections
                       (woerterbuch-duden--build-url input)
                       1)))))
                (error
                 (woerterbuch-core-make-error
                  'duden input
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (unless (eq result :async)
      (funcall callback result))))

(defun woerterbuch-duden-fetch (input sections callback)
  "Fetch INPUT from Duden asynchronously for SECTIONS.

Invoke CALLBACK once."
  (if (not (woerterbuch-duden--request-needed-p sections))
      (funcall callback (woerterbuch-core-make-result 'duden input))
    (woerterbuch-duden--with-headers
     (lambda ()
       (url-retrieve
        (woerterbuch-duden--build-url input)
        #'woerterbuch-duden--initial-callback
        (list input sections callback)
        t
        t)))))

(defun woerterbuch-duden--parse-html-string
    (html input sections &optional url homograph-id)
  "Parse HTML fixture string HTML as Duden entry for INPUT.

SECTIONS controls extraction. URL and HOMOGRAPH-ID override defaults."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (woerterbuch-duden--parse-dom
     (libxml-parse-html-region (point-min) (point-max))
     input sections url (or homograph-id 1))))

(defun woerterbuch-duden--parse-html-file
    (file input sections &optional url homograph-id)
  "Parse local Duden HTML FILE as entry for INPUT.

SECTIONS controls extraction.  URL and HOMOGRAPH-ID override defaults.
This helper is meant for offline tests."
  (with-temp-buffer
    (insert-file-contents file)
    (woerterbuch-duden--parse-dom
     (libxml-parse-html-region (point-min) (point-max))
     input sections url (or homograph-id 1))))

(provide 'woerterbuch-duden)

;;; woerterbuch-duden.el ends here
