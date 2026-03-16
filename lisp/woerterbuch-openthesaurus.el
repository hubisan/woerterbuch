;;; woerterbuch-openthesaurus.el --- OpenThesaurus backend -*- lexical-binding: t; -*-

(require 'url)
(require 'json)
(require 'subr-x)
(require 'woerterbuch-core)

(defconst woerterbuch-openthesaurus-base-url
  "https://www.openthesaurus.de/synonyme/search"
  "Base URL for OpenThesaurus requests.")

(defun woerterbuch-openthesaurus--build-url (word)
  "Build OpenThesaurus API URL for WORD."
  (concat woerterbuch-openthesaurus-base-url
          "?format=application/json"
          "&q=" (url-hexify-string word)))

(defun woerterbuch-openthesaurus-fetch (word sections callback)
  "Fetch WORD asynchronously from OpenThesaurus.

SECTIONS is the requested section list.
CALLBACK receives exactly one normalized result plist."
  (let ((url-request-extra-headers
         '(("User-Agent" . "woerterbuch/0.1"))))
    (url-retrieve
     (woerterbuch-openthesaurus--build-url word)
     #'woerterbuch-openthesaurus--request-callback
     (list word sections callback)
     t
     t)))

(defun woerterbuch-openthesaurus--request-callback (status word sections callback)
  "Handle async response STATUS for WORD, SECTIONS, and CALLBACK."
  (let (result)
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((plist-get status :error)
                    (woerterbuch-core-make-error
                     'openthesaurus
                     word
                     (format "Network error: %S" (plist-get status :error))))

                   ((and (boundp 'url-http-response-status)
                         (numberp url-http-response-status)
                         (>= url-http-response-status 400))
                    (woerterbuch-core-make-error
                     'openthesaurus
                     word
                     (format "HTTP error: %s" url-http-response-status)))

                   (t
                    (woerterbuch-openthesaurus--parse-response word sections)))
                (error
                 (woerterbuch-core-make-error
                  'openthesaurus
                  word
                  (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-openthesaurus--parse-response (word sections)
  "Parse current response buffer for WORD and SECTIONS."
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
         (result (woerterbuch-core-make-result 'openthesaurus word)))
    (when (woerterbuch-core-section-requested-p :synonyms sections)
      (setq result
            (plist-put result :synonyms
                       (woerterbuch-openthesaurus--extract-synonyms data word))))
    result))

(defun woerterbuch-openthesaurus--extract-synonyms (data word)
  "Extract synonym list from OpenThesaurus DATA for WORD."
  (let ((sets (alist-get 'synsets data))
        (seen (make-hash-table :test #'equal))
        synonyms)
    (dolist (synset sets)
      (dolist (term (alist-get 'terms synset))
        (let ((candidate (alist-get 'term term)))
          (when (and (stringp candidate)
                     (not (string-empty-p candidate))
                     (not (string-equal (downcase candidate)
                                        (downcase word)))
                     (not (gethash candidate seen)))
            (puthash candidate t seen)
            (push candidate synonyms)))))
    (nreverse synonyms)))

(provide 'woerterbuch-openthesaurus)

;;; woerterbuch-openthesaurus.el ends here
