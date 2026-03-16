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

(defcustom woerterbuch-default-source-timeout 10
  "Fallback timeout if source is not in `woerterbuch-source-timeouts`."
  :type 'number
  :group 'woerterbuch)

(defcustom woerterbuch-source-timeouts
  '((dwds . 10)
    (openthesaurus . 5))
  "Per-source timeout in seconds."
  :type '(alist :key-type symbol :value-type number)
  :group 'woerterbuch)

(defcustom woerterbuch-sync-poll-interval 0.05
  "Polling interval in seconds for `woerterbuch-fetch-all-sync'."
  :type 'number
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

(defun woerterbuch-core--source-timeout (source)
  "Return timeout in seconds for SOURCE."
  (or (cdr (assq source woerterbuch-source-timeouts))
      woerterbuch-default-source-timeout))

(defun woerterbuch-core--normalize-result (source word lemma result)
  "Normalize RESULT for SOURCE, original WORD, and query LEMMA."
  (let ((result (or result (woerterbuch-core-make-result source word))))
    (setq result (plist-put result :source source))
    (setq result (plist-put result :word word))
    (setq result (plist-put result :lemma lemma))
    result))

(defun woerterbuch-core--make-timeout-error (source word timeout)
  "Create timeout error result for SOURCE, WORD, and TIMEOUT."
  (woerterbuch-core-make-error
   source word
   (format "Timeout after %ss" timeout)))

(defun woerterbuch-core--call-fetcher-with-timeout
    (source fetcher word lemma sections callback)
  "Call FETCHER for SOURCE with timeout handling.

WORD is the original user input, LEMMA the normalized backend query,
SECTIONS the requested sections.  CALLBACK is called exactly once."
  (let* ((timeout (woerterbuch-core--source-timeout source))
         (finished nil)
         timer)
    (setq timer
          (run-at-time
           timeout nil
           (lambda ()
             (unless finished
               (setq finished t)
               (funcall callback
                        (woerterbuch-core--make-timeout-error
                         source word timeout))))))
    (funcall
     fetcher
     lemma
     sections
     (lambda (result)
       (unless finished
         (setq finished t)
         (when (timerp timer)
           (cancel-timer timer))
         (funcall callback
                  (woerterbuch-core--normalize-result
                   source word lemma result)))))))

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
         (finished nil))
    (if (zerop pending)
        (funcall final-callback nil)
      (dolist (source sources)
        (let ((fetcher (woerterbuch-core--source-fetcher source)))
          (woerterbuch-core--call-fetcher-with-timeout
           source
           fetcher
           word
           lemma
           sections
           (lambda (result)
             (unless finished
               (puthash source result results)
               (setq pending (1- pending))
               (when (zerop pending)
                 (setq finished t)
                 (funcall
                  final-callback
                  (mapcar (lambda (s)
                            (or (gethash s results)
                                (woerterbuch-core-make-error
                                 s word "No result returned")))
                          sources)))))))))))

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

(cl-defun woerterbuch-fetch-all-sync
    (word &key
          (sections woerterbuch-default-sections)
          (normalize-lemma woerterbuch-normalize-lemma)
          timeout)
  "Synchronously fetch WORD from all configured sources.

Returns the final result list that `woerterbuch-fetch-all' would pass to
its callback.

TIMEOUT limits the total wait time in seconds for the whole operation.
When nil, use the maximum configured source timeout plus 1 second."
  (let* ((done nil)
         (result nil)
         (timeout (or timeout
                      (1+ (apply #'max
                                 woerterbuch-default-source-timeout
                                 (mapcar #'cdr woerterbuch-source-timeouts)))))
         (deadline (+ (float-time) timeout)))
    (woerterbuch-fetch-all
     word
     (lambda (res)
       (setq result res)
       (setq done t))
     :sections sections
     :normalize-lemma normalize-lemma)
    (while (and (not done)
                (< (float-time) deadline))
      (accept-process-output nil woerterbuch-sync-poll-interval))
    (unless done
      (error "woerterbuch-fetch-all-sync timed out after %ss" timeout))
    result))

(provide 'woerterbuch-core)

;;; woerterbuch-core.el ends here
