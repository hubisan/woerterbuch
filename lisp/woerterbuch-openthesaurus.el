;;; woerterbuch-openthesaurus.el --- OpenThesaurus backend -*- lexical-binding: t; -*-

;;; Commentary:

;; OpenThesaurus backend implementation.

;;; Code:

(require 'url)
(require 'json)
(require 'subr-x)
(require 'woerterbuch-core)

(defconst woerterbuch-openthesaurus-base-url
  "https://www.openthesaurus.de/synonyme/search"
  "Base URL for OpenThesaurus requests.")

(defconst woerterbuch-openthesaurus-web-url
  "https://www.openthesaurus.de/synonyme/"
  "Base web URL for OpenThesaurus entries.")

(defun woerterbuch-openthesaurus--build-url (input)
  "Build OpenThesaurus API URL for INPUT."
  (concat woerterbuch-openthesaurus-base-url
          "?format=application/json"
          "&q=" (url-hexify-string input)))

(defun woerterbuch-openthesaurus--build-web-url (lemma)
  "Build OpenThesaurus web URL for LEMMA."
  (concat woerterbuch-openthesaurus-web-url
          (url-hexify-string lemma)))

(defun woerterbuch-openthesaurus-fetch (input sections callback)
  "Fetch INPUT asynchronously from OpenThesaurus for SECTIONS.
CALLBACK is tranmitted to the follow up function."
  (if (not (woerterbuch-core-section-requested-p :synonyms sections))
      (let ((result (woerterbuch-core-make-result 'openthesaurus input)))
        (setq result
              (plist-put result :url
                         (woerterbuch-openthesaurus--build-web-url input)))
        (funcall callback result))
    (let ((url-request-extra-headers
           '(("User-Agent" . "woerterbuch/0.1"))))
      (url-retrieve
       (woerterbuch-openthesaurus--build-url input)
       #'woerterbuch-openthesaurus--request-callback
       (list input sections callback)
       'silent
       'inhibit-cookies))))

(defun woerterbuch-openthesaurus--request-callback
    (status input sections callback)
  "Handle async response STATUS for INPUT, SECTIONS, and CALLBACK."
  (let (result)
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((plist-get status :error)
                    (woerterbuch-core-make-error
                     'openthesaurus
                     input
                     (format "Network error: %S" (plist-get status :error))))

                   ((and (boundp 'url-http-response-status)
                         (numberp url-http-response-status)
                         (>= url-http-response-status 400))
                    (woerterbuch-core-make-error
                     'openthesaurus
                     input
                     (format "HTTP error: %s" url-http-response-status)))

                   (t
                    (woerterbuch-openthesaurus--parse-response input sections)))
                (error
                 (woerterbuch-core-make-error
                  'openthesaurus
                  input
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-openthesaurus--parse-response (input _sections)
  "Parse current response buffer for INPUT."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil t))
  (skip-chars-forward "\r\n")
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (data (json-read))
         (groups (woerterbuch-openthesaurus--extract-definition-groups
                  data input)))
    (if (null groups)
        (let ((result (woerterbuch-core-make-error
                       'openthesaurus input "No matches found")))
          (plist-put result :url
                     (woerterbuch-openthesaurus--build-web-url input)))
      (let ((result (woerterbuch-core-make-result 'openthesaurus input)))
        (setq result
              (plist-put result :url
                         (woerterbuch-openthesaurus--build-web-url input)))
        (plist-put result :homographs
                   (list
                    (list :id 1
                          :lemma input
                          :definitions groups)))))))

(defun woerterbuch-openthesaurus--extract-definition-groups (data lemma)
  "Extract synonym groups as definitions from OpenThesaurus DATA for LEMMA."
  (let ((synsets (alist-get 'synsets data))
        (index 0)
        groups)
    (dolist (synset synsets)
      (setq index (1+ index))
      (push
       (list :id index
             :lemma lemma
             :categories
             (woerterbuch-openthesaurus--normalize-categories
              (alist-get 'categories synset))
             :synonyms
             (woerterbuch-openthesaurus--extract-synonyms-from-synset synset
                                                                      lemma))
       groups))
    (nreverse groups)))

(defun woerterbuch-openthesaurus--normalize-categories (categories)
  "Normalize OpenThesaurus CATEGORIES."
  (when categories
    (seq-filter #'stringp categories)))

(defun woerterbuch-openthesaurus--extract-synonyms-from-synset (synset lemma)
  "Extract synonyms from SYNSET, excluding LEMMA itself."
  (let ((terms (alist-get 'terms synset))
        (seen (make-hash-table :test #'equal))
        synonyms)
    (dolist (term terms)
      (let ((candidate (alist-get 'term term)))
        (when (and (stringp candidate)
                   (not (string-empty-p candidate))
                   (not (string-equal (downcase candidate)
                                      (downcase lemma)))
                   (not (gethash candidate seen)))
          (puthash candidate t seen)
          (push candidate synonyms))))
    (nreverse synonyms)))

(provide 'woerterbuch-openthesaurus)

;;; woerterbuch-openthesaurus.el ends here
