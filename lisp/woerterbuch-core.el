;;; woerterbuch-core.el --- Core helpers for woerterbuch -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'url)
(require 'json)
(require 'subr-x)

;;; Customization

(defgroup woerterbuch nil
  "German dictionary and thesaurus."
  :group 'convenience
  :prefix "woerterbuch-"
  :link '(url-link "https://github.com/hubisan/woerterbuch"))

(defcustom woerterbuch-sources
  '(openthesaurus dwds)
  "Ordered list of enabled woerterbuch sources.

Each symbol must correspond to a loaded backend.  The order determines
the order in which results are returned by `woerterbuch-fetch-all'."
  :type '(repeat (choice (const openthesaurus)
                         (const dwds)
                         (const duden)
                         (const wiktionary)
                         symbol))
  :group 'woerterbuch)

(defcustom woerterbuch-normalize-lemma t
  "Whether `woerterbuch-fetch-all' normalizes words to their lemma by default.
If `woerterbuch-fetch-all' is called with a non-nil or nil optional
NORMALIZE-LEMMA argument, that argument overrides this variable."
  :type 'boolean
  :group 'woerterbuch)

(defcustom woerterbuch-default-sections
  '(:definitions :examples :synonyms :origin)
  "Default sections to fetch from dictionary sources.

Used by `woerterbuch-fetch-all' when :sections is not provided."
  :type
  '(repeat
    (choice
     (const :tag "Definitions" :definitions)
     (const :tag "Examples" :examples)
     (const :tag "Synonyms" :synonyms)
     (const :tag "Origin / Etymology" :origin)))
  :group 'woerterbuch)

;;; Constants

(defconst woerterbuch-core-lemma-url
  "https://www.dwds.de/api/frequency/"
  "DWDS endpoint used for lemma normalization.")

;;; Result constructors

(defun woerterbuch-core-make-result (source word)
  "Create normalized success result for SOURCE and WORD."
  (list :source source
        :word word
        :lemma word
        :ok t
        :definitions nil
        :synonyms nil
        :origin nil
        :idioms nil))

(defun woerterbuch-core-make-error (source word message)
  "Create normalized error result for SOURCE, WORD, and MESSAGE."
  (list :source source
        :word word
        :lemma word
        :ok nil
        :error message))

;;; Helpers

(defun woerterbuch-core-section-requested-p (section sections)
  "Return non-nil when SECTION is present in SECTIONS."
  (memq section sections))

(defun woerterbuch-core--source-fetcher (source)
  "Return fetch function symbol for SOURCE."
  (pcase source
    ('openthesaurus #'woerterbuch-openthesaurus-fetch)
    ('dwds          #'woerterbuch-dwds-fetch)
    (_ (error "Unknown woerterbuch source: %S" source))))

;;; Lemma normalization

(defun woerterbuch-core--build-lemma-url (word)
  "Build DWDS lemma lookup URL for WORD."
  (concat woerterbuch-core-lemma-url
          "?q="
          (url-hexify-string word)))

(defun woerterbuch-core-normalize-lemma (word callback)
  "Normalize WORD to a lemma via DWDS and call CALLBACK once.

CALLBACK receives a plist:

Success:
  (:ok t :word WORD :lemma LEMMA :source dwds)

Failure:
  (:ok nil :word WORD :lemma WORD :source dwds :error MESSAGE)"
  (let ((url-request-extra-headers
         '(("User-Agent" . "woerterbuch/0.1"))))
    (url-retrieve
     (woerterbuch-core--build-lemma-url word)
     #'woerterbuch-core--normalize-lemma-callback
     (list word callback)
     t
     t)))

(defun woerterbuch-core--normalize-lemma-callback (status word callback)
  "Handle DWDS lemma response STATUS for WORD and CALLBACK."
  (let ((result nil))
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((plist-get status :error)
                    (list :ok nil
                          :word word
                          :lemma word
                          :source 'dwds
                          :error (format "Network error: %S"
                                         (plist-get status :error))))

                   ((and (boundp 'url-http-response-status)
                         (numberp url-http-response-status)
                         (>= url-http-response-status 400))
                    (list :ok nil
                          :word word
                          :lemma word
                          :source 'dwds
                          :error (format "HTTP error: %s"
                                         url-http-response-status)))

                   (t
                    (woerterbuch-core--parse-lemma-response word)))
                (error
                 (list :ok nil
                       :word word
                       :lemma word
                       :source 'dwds
                       :error (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-core--parse-lemma-response (word)
  "Parse current DWDS lemma response buffer for WORD."
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
         (lemma (alist-get 'lemma data)))
    (list :ok t
          :word word
          :lemma (if (and (stringp lemma)
                          (not (string-empty-p lemma)))
                     lemma
                   word)
          :source 'dwds)))

;;; Fetching

(defun woerterbuch-core--fetch-all-with-query (word lemma sections final-callback)
  "Fetch SECTIONS for WORD using LEMMA as backend query.

FINAL-CALLBACK is called exactly once with a list of normalized
results in stable source order."
  (let* ((sources woerterbuch-sources)
         (pending (length sources))
         (results (make-hash-table :test #'eq))
         (done nil))
    (if (zerop pending)
        (funcall final-callback nil)
      (dolist (source sources)
        (let ((fetcher (woerterbuch-core--source-fetcher source)))
          (funcall
           fetcher
           lemma
           sections
           (lambda (result)
             (unless done
               (puthash
                source
                (plist-put
                 (plist-put result :word word)
                 :lemma lemma)
                results)
               (setq pending (1- pending))
               (when (zerop pending)
                 (setq done t)
                 (funcall
                  final-callback
                  (mapcar (lambda (s) (gethash s results)) sources)))))))))))

(cl-defun woerterbuch-fetch-all
    (word final-callback
          &key
          (sections woerterbuch-default-sections)
          (normalize-lemma woerterbuch-normalize-lemma))
  "Fetch WORD from all configured sources.

FINAL-CALLBACK is called once with a list of result plists, one per
source in `woerterbuch-sources'.

SECTIONS is a list of keywords such as `:synonyms' or `:definitions'.
When omitted, `woerterbuch-default-sections' is used.

When NORMALIZE-LEMMA is non-nil, WORD is first normalized to its base
form via DWDS before querying backends."
  (if normalize-lemma
      (woerterbuch-core-normalize-lemma
       word
       (lambda (lemma-result)
         (woerterbuch-core--fetch-all-with-query
          word
          (or (plist-get lemma-result :lemma) word)
          sections
          final-callback)))
    (woerterbuch-core--fetch-all-with-query
     word
     word
     sections
     final-callback)))

(provide 'woerterbuch-core)

;;; woerterbuch-core.el ends here
