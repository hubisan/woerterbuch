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
  '(openthesaurus dwds duden wiktionary)
  "Ordered list of enabled woerterbuch sources.

Each symbol must correspond to a loaded backend. The order determines
the order in which results are returned by `woerterbuch-fetch-all'."
  :type '(repeat (choice (const openthesaurus)
                         (const dwds)
                         (const duden)
                         (const wiktionary)
                         symbol))
  :group 'woerterbuch)

(defcustom woerterbuch-normalize-lemma t
  "Whether `woerterbuch-fetch-all' normalizes input to its lemma by default.

If `woerterbuch-fetch-all' is called with a non-nil or nil optional
NORMALIZE-LEMMA argument, that argument overrides this variable."
  :type 'boolean
  :group 'woerterbuch)

(defcustom woerterbuch-default-sections
  '(:definitions :examples :synonyms :origin :idioms)
  "Default sections to fetch from dictionary sources.

Used by `woerterbuch-fetch-all' when :sections is not provided."
  :type
  '(repeat
    (choice
     (const :tag "Definitions" :definitions)
     (const :tag "Examples" :examples)
     (const :tag "Synonyms" :synonyms)
     (const :tag "Origin / Etymology" :origin)
     (const :tag "Idioms / Mehrwortausdrücke" :idioms)))
  :group 'woerterbuch)

(defcustom woerterbuch-default-source-timeout 10
  "Fallback timeout if source is not in `woerterbuch-source-timeouts'."
  :type 'number
  :group 'woerterbuch)

(defcustom woerterbuch-source-timeouts
  '((dwds . 10)
    (duden . 20)
    (openthesaurus . 5)
    (wiktionary . 10))
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

(defun woerterbuch-core-make-wrapper (input &optional lemma)
  "Create normalized wrapper result for INPUT.

INPUT is the original user input. Some sources may support not only
single words, but also multi-word expressions or idioms (for example
DWDS).

LEMMA is the normalized lemma for INPUT. If INPUT is not normalized,
LEMMA should be equal to INPUT."
  (list :input input
        :lemma (or lemma input)
        :sources nil))

(defun woerterbuch-core-make-result (source lemma)
  "Create normalized success result for SOURCE and LEMMA.

LEMMA is the source-specific lemma. It may differ from the wrapper
lemma, for example if the source redirects or normalizes differently."
  (list :source source
        :lemma lemma
        :ok t))

(defun woerterbuch-core-make-error (source lemma message)
  "Create normalized error result for SOURCE, LEMMA, and MESSAGE."
  (list :source source
        :lemma lemma
        :ok nil
        :homographs nil
        :error message))

;;; Helpers

(defun woerterbuch-core-section-requested-p (section sections)
  "Return non-nil when SECTION is present in SECTIONS."
  (memq section sections))

(defun woerterbuch-core--source-fetcher (source)
  "Return fetch function symbol for SOURCE."
  (let ((fn (intern-soft
             (format "woerterbuch-%s-fetch" source))))
    (unless (and fn (fboundp fn))
      (error "Unknown or unavailable woerterbuch source: %S" source))
    fn))

(defun woerterbuch-core--source-timeout (source)
  "Return timeout in seconds for SOURCE."
  (or (cdr (assq source woerterbuch-source-timeouts))
      woerterbuch-default-source-timeout))

(defun woerterbuch-core--normalize-result (source lemma result)
  "Normalize RESULT for SOURCE and wrapper LEMMA."
  (let ((result (or result (woerterbuch-core-make-result source lemma))))
    (setq result (plist-put result :source source))
    (unless (plist-member result :lemma)
      (setq result (plist-put result :lemma lemma)))
    result))

(defun woerterbuch-core--make-timeout-error (source lemma timeout)
  "Create timeout error result for SOURCE, LEMMA, and TIMEOUT."
  (woerterbuch-core-make-error
   source lemma
   (format "Timeout after %ss" timeout)))

(defun woerterbuch-core--with-timeout (source lemma thunk callback)
  "Run THUNK with timeout handling for SOURCE and LEMMA.

THUNK is called with one argument, a done callback. CALLBACK is then
called exactly once, either with the normal result or with a timeout
error result."
  (let* ((timeout (woerterbuch-core--source-timeout source))
         (finished nil)
         (resource nil)
         timer)
    (setq timer
          (run-at-time
           timeout nil
           (lambda ()
             (unless finished
               (setq finished t)
               (cond
                ((processp resource)
                 (delete-process resource))
                ((buffer-live-p resource)
                 (let ((proc (get-buffer-process resource)))
                   (when (processp proc)
                     (delete-process proc)))
                 (kill-buffer resource)))
               (funcall callback
                        (woerterbuch-core--make-timeout-error
                         source lemma timeout))))))
    (setq resource
          (funcall
           thunk
           (lambda (result)
             (unless finished
               (setq finished t)
               (when (timerp timer)
                 (cancel-timer timer))
               (funcall callback result)))))))

;;; Lemma normalization

(defun woerterbuch-core--build-lemma-url (input)
  "Build DWDS lemma lookup URL for INPUT."
  (concat woerterbuch-core-lemma-url
          "?q="
          (url-hexify-string input)))

(defun woerterbuch-core-normalize-lemma (input callback)
  "Normalize INPUT to a lemma via DWDS and call CALLBACK once.

INPUT is the original user input. Some sources may support not only
single words, but also multi-word expressions or idioms (for example
DWDS).

CALLBACK receives a plist:

Success:
  (:ok t :input INPUT :lemma LEMMA :source dwds)

Failure:
  (:ok nil :input INPUT :lemma INPUT :source dwds :error MESSAGE)"
  (woerterbuch-core--with-timeout
   'dwds input
   (lambda (done)
     (let ((url-request-extra-headers
            '(("User-Agent" . "woerterbuch/0.1"))))
       (url-retrieve
        (woerterbuch-core--build-lemma-url input)
        #'woerterbuch-core--normalize-lemma-callback
        (list input done)
        t
        t)))
   callback))

(defun woerterbuch-core--normalize-lemma-callback (status input callback)
  "Handle DWDS lemma response STATUS for INPUT and CALLBACK."
  (let ((result nil))
    (unwind-protect
        (setq result
              (condition-case err
                  (cond
                   ((plist-get status :error)
                    (list :ok nil
                          :input input
                          :lemma input
                          :source 'dwds
                          :error (format "Network error: %S"
                                         (plist-get status :error))))

                   ((and (boundp 'url-http-response-status)
                         (numberp url-http-response-status)
                         (>= url-http-response-status 400))
                    (list :ok nil
                          :input input
                          :lemma input
                          :source 'dwds
                          :error (format "HTTP error: %s"
                                         url-http-response-status)))

                   (t
                    (woerterbuch-core--parse-lemma-response input)))
                (error
                 (list :ok nil
                       :input input
                       :lemma input
                       :source 'dwds
                       :error (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-core--parse-lemma-response (input)
  "Parse current DWDS lemma response buffer for INPUT."
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
          :input input
          :lemma (if (and (stringp lemma)
                          (not (string-empty-p lemma)))
                     lemma
                   input)
          :source 'dwds)))

;;; Fetching

(defun woerterbuch-core--build-wrapper (input lemma source-results)
  "Create wrapper result for INPUT, LEMMA, and SOURCE-RESULTS."
  (let ((wrapper (woerterbuch-core-make-wrapper input lemma)))
    (plist-put wrapper :sources source-results)))

(defun woerterbuch-core--fetch-all-with-lemma
    (input lemma sections final-callback)
  "Fetch SECTIONS for INPUT using LEMMA as backend query.

FINAL-CALLBACK is called exactly once with a wrapper plist of the form:

  (:input INPUT :lemma LEMMA :sources SOURCES)

where SOURCES is a list of normalized source results in stable source
order."
  (let* ((sources woerterbuch-sources)
         (pending (length sources))
         (results (make-hash-table :test #'eq))
         (finished nil))
    (if (zerop pending)
        (funcall final-callback
                 (woerterbuch-core--build-wrapper input lemma nil))
      (dolist (source sources)
        (let ((fetcher (woerterbuch-core--source-fetcher source)))
          (woerterbuch-core--with-timeout
           source lemma
           (lambda (done)
             (funcall fetcher lemma sections done))
           (lambda (result)
             (unless finished
               (puthash source
                        (woerterbuch-core--normalize-result source lemma result)
                        results)
               (setq pending (1- pending))
               (when (zerop pending)
                 (setq finished t)
                 (funcall
                  final-callback
                  (woerterbuch-core--build-wrapper
                   input
                   lemma
                   (mapcar
                    (lambda (s)
                      (or (gethash s results)
                          (woerterbuch-core-make-error
                           s lemma "No result returned")))
                    sources))))))))))))

(cl-defun woerterbuch-fetch-all
    (input final-callback
           &key
           (sections woerterbuch-default-sections)
           (normalize-lemma woerterbuch-normalize-lemma))
  "Fetch INPUT from all configured sources.

INPUT is the original user input. Some sources may support not only
single words, but also multi-word expressions or idioms (for example
DWDS).

FINAL-CALLBACK is called once with a wrapper plist of the form:

  (:input INPUT :lemma LEMMA :sources SOURCES)

SECTIONS is a list of keywords such as `:synonyms' or `:definitions'.
When omitted, `woerterbuch-default-sections' is used.

When NORMALIZE-LEMMA is non-nil, INPUT is first normalized to its base
form via DWDS before querying backends. If INPUT is not normalized,
then the wrapper lemma is equal to INPUT."
  (if normalize-lemma
      (woerterbuch-core-normalize-lemma
       input
       (lambda (lemma-result)
         (woerterbuch-core--fetch-all-with-lemma
          input
          (or (plist-get lemma-result :lemma) input)
          sections
          final-callback)))
    (woerterbuch-core--fetch-all-with-lemma
     input
     input
     sections
     final-callback)))

(cl-defun woerterbuch-fetch-all-sync
    (input &key
           (sections woerterbuch-default-sections)
           (normalize-lemma woerterbuch-normalize-lemma)
           timeout)
  "Synchronously fetch INPUT from all configured sources.

Returns the wrapper result that `woerterbuch-fetch-all' would pass to
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
     input
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
