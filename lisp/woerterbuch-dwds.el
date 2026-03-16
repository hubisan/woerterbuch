;;; woerterbuch-dwds.el --- DWDS backend for woerterbuch -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dom)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'woerterbuch-core)

(defconst woerterbuch-dwds-base-url
  "https://www.dwds.de/wb/"
  "Base URL for DWDS dictionary pages.")

(defun woerterbuch-dwds--build-url (lemma)
  "Build the canonical DWDS page URL for LEMMA."
  (concat woerterbuch-dwds-base-url
          (url-hexify-string lemma)))

(defun woerterbuch-dwds--clean-text (string)
  "Normalize whitespace in STRING."
  (when string
    (string-trim
     (replace-regexp-in-string "[[:space:] ]+" " " string))))

(defun woerterbuch-dwds--element-children (node)
  "Return only element children of NODE."
  (cl-remove-if-not #'listp (dom-children node)))

(defun woerterbuch-dwds--text (node)
  "Return normalized text content for NODE."
  (woerterbuch-dwds--clean-text
   (cond
    ((null node) "")
    ((stringp node) node)
    ((listp node)
     (mapconcat #'woerterbuch-dwds--text (dom-children node) " "))
    (t ""))))

(defun woerterbuch-dwds--class-list (node)
  "Return CSS class list for NODE."
  (split-string (or (dom-attr node 'class) "") "[[:space:]]+" t))

(defun woerterbuch-dwds--has-class-p (node class)
  "Return non-nil when NODE has CLASS."
  (member class (woerterbuch-dwds--class-list node)))

(defun woerterbuch-dwds--find-all (node predicate)
  "Collect all descendants of NODE matching PREDICATE in document order."
  (let (acc)
    (dolist (child (woerterbuch-dwds--element-children node))
      (when (funcall predicate child)
        (setq acc (nconc acc (list child))))
      (setq acc (nconc acc (woerterbuch-dwds--find-all child predicate))))
    acc))

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

(defun woerterbuch-dwds--children-by-class (node class)
  "Return direct children of NODE that have CLASS."
  (cl-remove-if-not
   (lambda (child)
     (woerterbuch-dwds--has-class-p child class))
   (woerterbuch-dwds--element-children node)))

(defun woerterbuch-dwds--descendants-by-class (node class)
  "Return all descendants of NODE that have CLASS."
  (woerterbuch-dwds--find-all
   node
   (lambda (child)
     (woerterbuch-dwds--has-class-p child class))))

(defun woerterbuch-dwds--canonical-url (dom fallback)
  "Read canonical URL from DOM or return FALLBACK."
  (let ((link (woerterbuch-dwds--find-first
               dom
               (lambda (node)
                 (and (eq (dom-tag node) 'link)
                      (equal (dom-attr node 'rel) "canonical"))))))
    (or (and link (dom-attr link 'href))
        fallback)))

(defun woerterbuch-dwds--field-text (article label)
  "Return ARTICLE form field text for LABEL."
  (cl-loop for block in (woerterbuch-dwds--descendants-by-class article "dwdswb-ft-block")
           for block-label = (woerterbuch-dwds--text
                              (woerterbuch-dwds--find-first
                               block
                               (lambda (node)
                                 (woerterbuch-dwds--has-class-p node "dwdswb-ft-blocklabel"))))
           when (and block-label
                     (string-match-p (regexp-quote label) block-label))
           return (woerterbuch-dwds--text
                   (woerterbuch-dwds--find-first
                    block
                    (lambda (node)
                      (woerterbuch-dwds--has-class-p node "dwdswb-ft-blocktext"))))))

(defun woerterbuch-dwds--wortart-from-grammar (grammar)
  "Extract word class from GRAMMAR."
  (when grammar
    (string-trim (car (split-string grammar "·" t)))))

(defun woerterbuch-dwds--definition-text (def-node)
  "Return flattened definition text from DEF-NODE."
  (when def-node
    (let* ((defs (woerterbuch-dwds--descendants-by-class def-node "dwdswb-definition"))
           (joined (woerterbuch-dwds--clean-text
                    (mapconcat #'woerterbuch-dwds--text defs " | ")))
           (fallback (woerterbuch-dwds--text def-node)))
      (if (not (string-empty-p joined))
          joined
        fallback))))

(defun woerterbuch-dwds--collect-examples (container)
  "Collect examples from CONTAINER."
  (when container
    (cl-delete-duplicates
     (cl-loop for node in (woerterbuch-dwds--find-all
                           container
                           (lambda (child)
                             (or (woerterbuch-dwds--has-class-p child "dwdswb-kompetenzbeispiel")
                                 (woerterbuch-dwds--has-class-p child "dwdswb-beleg"))))
              for txt = (woerterbuch-dwds--text node)
              unless (string-empty-p txt)
              collect txt)
     :test #'equal)))

(defun woerterbuch-dwds--parse-lesart (node)
  "Parse a DWDS meaning NODE recursively."
  (let* ((label-node (car (woerterbuch-dwds--children-by-class node "dwdswb-lesart-n")))
         (content-node (car (woerterbuch-dwds--children-by-class node "dwdswb-lesart-content")))
         (def-node (and content-node
                        (car (woerterbuch-dwds--children-by-class content-node "dwdswb-lesart-def"))))
         (usage-node (and content-node
                          (car (woerterbuch-dwds--children-by-class content-node "dwdswb-verwendungsbeispiele"))))
         (child-lesarten (and content-node
                              (woerterbuch-dwds--children-by-class content-node "dwdswb-lesart")))
         (qualifiers (and def-node
                          (mapcar #'woerterbuch-dwds--text
                                  (woerterbuch-dwds--descendants-by-class def-node "dwdswb-diasystematik")))))
    (list :id (dom-attr node 'id)
          :label (woerterbuch-dwds--text label-node)
          :definition (woerterbuch-dwds--definition-text def-node)
          :qualifiers (cl-remove-if #'string-empty-p qualifiers)
          :examples (or (woerterbuch-dwds--collect-examples usage-node) '())
          :submeanings (mapcar #'woerterbuch-dwds--parse-lesart child-lesarten))))

(defun woerterbuch-dwds--article-scope-p (node)
  "Return non-nil when NODE is a usable article scope.

For homograph pages DWDS contains an overview pane with id 0. That pane must
be ignored for parsing and only the real article panes are used."
  (and (woerterbuch-dwds--find-first
        node
        (lambda (child)
          (woerterbuch-dwds--has-class-p child "dwdswb-artikel")))
       (not (equal (dom-attr node 'id) "0"))))

(defun woerterbuch-dwds--article-scopes (dom)
  "Return all scopes that each contain exactly one article.

For pages with homographs this returns one scope per tab. For normal pages
without article tabs the whole DOM is returned as a single scope."
  (let ((panes (woerterbuch-dwds--find-all
                dom
                (lambda (node)
                  (and (woerterbuch-dwds--has-class-p node "tab-pane")
                       (woerterbuch-dwds--article-scope-p node))))))
    (if panes
        panes
      (list dom))))

(defun woerterbuch-dwds--parse-etymology (scope)
  "Extract etymology text from SCOPE."
  (let ((entry (woerterbuch-dwds--find-first
                scope
                (lambda (node)
                  (woerterbuch-dwds--has-class-p node "etymwb-entry")))))
    (let ((text (woerterbuch-dwds--text entry)))
      (unless (string-empty-p text)
        text))))

(defun woerterbuch-dwds--parse-synonyms (scope)
  "Extract a conservative synonym list from SCOPE.

This intentionally only reads the OpenThesaurus block from the rendered DWDS
page and returns unique linked entries."
  (let (synonyms)
    (dolist (block (woerterbuch-dwds--descendants-by-class scope "ot-synset-block"))
      (dolist (link (woerterbuch-dwds--find-all
                     block
                     (lambda (node)
                       (eq (dom-tag node) 'a))))
        (let ((text (woerterbuch-dwds--text link)))
          (when (and (not (string-empty-p text))
                     (string-prefix-p "/wb/" (or (dom-attr link 'href) "")))
            (push text synonyms)))))
    (nreverse (cl-delete-duplicates synonyms :test #'equal))))

(defun woerterbuch-dwds--parse-article (scope)
  "Parse one article from SCOPE.

Returns a homograph plist with meanings nested as a tree."
  (let* ((article (woerterbuch-dwds--find-first
                   scope
                   (lambda (node)
                     (woerterbuch-dwds--has-class-p node "dwdswb-artikel"))))
         (bookmark (and article
                        (woerterbuch-dwds--find-first
                         article
                         (lambda (node)
                           (woerterbuch-dwds--has-class-p node "dwds-bookmark-button")))))
         (heading (and article
                       (woerterbuch-dwds--find-first
                        article
                        (lambda (node)
                          (woerterbuch-dwds--has-class-p node "dwdswb-ft-lemmaansatz")))))
         (lemma-node (and heading
                          (woerterbuch-dwds--find-first
                           heading
                           (lambda (node)
                             (eq (dom-tag node) 'b)))))
         (grammar (and article (woerterbuch-dwds--field-text article "Grammatik")))
         (lesarten-root (woerterbuch-dwds--find-first
                         scope
                         (lambda (node)
                           (woerterbuch-dwds--has-class-p node "dwdswb-lesarten"))))
         (meanings (if lesarten-root
                       (mapcar #'woerterbuch-dwds--parse-lesart
                               (woerterbuch-dwds--children-by-class lesarten-root "dwdswb-lesart"))
                     '()))
         (origin (woerterbuch-dwds--parse-etymology scope))
         (synonyms (woerterbuch-dwds--parse-synonyms scope)))
    (list :hidx (or (and bookmark (dom-attr bookmark 'data-hidx))
                    (dom-attr scope 'id)
                    "1")
          :lemma (woerterbuch-dwds--text lemma-node)
          :heading (woerterbuch-dwds--text heading)
          :wortart (woerterbuch-dwds--wortart-from-grammar grammar)
          :grammar grammar
          :origin origin
          :meanings meanings
          :synonyms synonyms)))

(defun woerterbuch-dwds--parse-dom (dom lemma)
  "Parse DWDS DOM for LEMMA and return one canonical page object."
  (let* ((canonical-url (woerterbuch-dwds--canonical-url dom
                                                         (woerterbuch-dwds--build-url lemma)))
         (homographs (mapcar #'woerterbuch-dwds--parse-article
                             (woerterbuch-dwds--article-scopes dom)))
         (page-lemma (or (plist-get (car homographs) :lemma)
                         lemma))
         (all-synonyms (cl-delete-duplicates
                        (apply #'append
                               (or (mapcar (lambda (homograph)
                                             (or (plist-get homograph :synonyms) '()))
                                           homographs)
                                   '(nil)))
                        :test #'equal))
         (origins (cl-loop for homograph in homographs
                           for origin = (plist-get homograph :origin)
                           when origin
                           collect (list :hidx (plist-get homograph :hidx)
                                         :lemma (or (plist-get homograph :lemma) page-lemma)
                                         :text origin))))
    (list :lemma page-lemma
          :url canonical-url
          :homographs homographs
          :definitions homographs
          :origin origins
          :synonyms all-synonyms)))

(defun woerterbuch-dwds--parse-current-buffer (lemma sections)
  "Parse current HTTP buffer as a DWDS page for LEMMA.

SECTIONS controls which standard result slots are populated, but the parser also
returns extra DWDS-specific data such as :url and :homographs."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil t))
  (skip-chars-forward "\r\n")
  (let* ((dom (libxml-parse-html-region (point) (point-max)))
         (entry (woerterbuch-dwds--parse-dom dom lemma))
         (result (woerterbuch-core-make-result 'dwds lemma)))
    (setq result (plist-put result :lemma (or (plist-get entry :lemma) lemma)))
    (setq result (plist-put result :url (plist-get entry :url)))
    (setq result (plist-put result :homographs (plist-get entry :homographs)))
    (when (woerterbuch-core-section-requested-p :definitions sections)
      (setq result (plist-put result :definitions (plist-get entry :definitions))))
    (when (woerterbuch-core-section-requested-p :origin sections)
      (setq result (plist-put result :origin (plist-get entry :origin))))
    (when (woerterbuch-core-section-requested-p :synonyms sections)
      (setq result (plist-put result :synonyms (plist-get entry :synonyms))))
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
                     'dwds lemma
                     (format "Network error: %S" (plist-get status :error))))
                   ((and (boundp 'url-http-response-status)
                         (numberp url-http-response-status)
                         (>= url-http-response-status 400))
                    (woerterbuch-core-make-error
                     'dwds lemma
                     (format "HTTP error: %s" url-http-response-status)))
                   (t
                    (woerterbuch-dwds--parse-current-buffer lemma sections)))
                (error
                 (woerterbuch-core-make-error
                  'dwds lemma
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-dwds-fetch (lemma sections callback)
  "Fetch LEMMA from DWDS and invoke CALLBACK once.

The request goes directly to the canonical dictionary page
https://www.dwds.de/wb/<lemma>. Homographs such as Bank#1 and Bank#2 are kept
as subentries in :homographs but always share the same canonical :url."
  (let ((url-request-extra-headers
         '(("User-Agent" . "woerterbuch/0.1")
           ("Accept-Language" . "de,en;q=0.8"))))
    (url-retrieve
     (woerterbuch-dwds--build-url lemma)
     #'woerterbuch-dwds--fetch-callback
     (list lemma sections callback)
     t
     t)))

(provide 'woerterbuch-dwds)

;;; woerterbuch-dwds.el ends here
