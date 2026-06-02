;;; woerterbuch-core.el --- Core helpers for woerterbuch -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared helpers for coordinating source backends and normalizing results.

;;; Code:

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

(defconst woerterbuch-core-lemma-snippet-url
  "https://www.dwds.de/api/wb/snippet/"
  "DWDS snippet endpoint used to detect direct lemma matches.")

(defconst woerterbuch-core-lemma-frequency-url
  "https://www.dwds.de/api/frequency/"
  "DWDS frequency endpoint used as fallback for lemma normalization.")

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

(defun woerterbuch-core--build-lemma-snippet-url (input)
  "Build DWDS snippet lookup URL for INPUT."
  (concat woerterbuch-core-lemma-snippet-url
          "?q="
          (url-hexify-string input)))

(defun woerterbuch-core--build-lemma-frequency-url (input)
  "Build DWDS frequency lookup URL for INPUT."
  (concat woerterbuch-core-lemma-frequency-url
          "?q="
          (url-hexify-string input)))

(defun woerterbuch-core--single-word-input-p (input)
  "Return non-nil when INPUT should be lemma-normalized as a single word."
  (not (string-match-p "[[:space:]]" (string-trim (or input "")))))

(defun woerterbuch-core--lemma-result (input lemma)
  "Return normalized DWDS lemma result for INPUT and LEMMA."
  (list :ok t
        :input input
        :lemma lemma
        :source 'dwds))

(defun woerterbuch-core--extract-lemma-from-snippet-data (data)
  "Return lemma string extracted from DWDS snippet DATA, or nil."
  (let ((entry
         (cond
          ((and (listp data) (listp (car data)))
           (car data))
          ((and (listp data)
                (assq 'lemma data))
           data)
          (t nil))))
    (let ((lemma (and entry (alist-get 'lemma entry))))
      (when (and (stringp lemma)
                 (not (string-empty-p lemma)))
        lemma))))

(defun woerterbuch-core-normalize-lemma (input callback)
  "Normalize INPUT to a lemma via DWDS and call CALLBACK once.

INPUT is the original user input. Some sources may support not only
single words, but also multi-word expressions or idioms (for example
DWDS).

CALLBACK is a function that receives one normalization-result plist:

Success:
  (:ok t :input INPUT :lemma LEMMA :source dwds)

Failure:
  (:ok nil :input INPUT :lemma INPUT :source dwds :error MESSAGE)"
  (if (not (woerterbuch-core--single-word-input-p input))
      (funcall callback (woerterbuch-core--lemma-result input input))
    (woerterbuch-core--with-timeout
     'dwds input
     (lambda (done)
       (let ((url-request-extra-headers
              '(("User-Agent" . "woerterbuch/0.1"))))
         (url-retrieve
          (woerterbuch-core--build-lemma-snippet-url input)
          #'woerterbuch-core--normalize-lemma-snippet-callback
          (list input done)
          t
          t)))
     callback)))

(defun woerterbuch-core--normalize-lemma-snippet-callback (status input callback)
  "Handle DWDS snippet response STATUS for INPUT and CALLBACK.

STATUS is the plist returned by `url-retrieve'. INPUT is the original
query string. CALLBACK is the continuation that receives the final
normalization result plist."
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
                    (let ((lemma (woerterbuch-core--parse-snippet-lemma-response)))
                      (if lemma
                          (woerterbuch-core--lemma-result input lemma)
                        (woerterbuch-core--normalize-lemma-via-frequency
                         input callback)
                        :async))))
                (error
                 (list :ok nil
                       :input input
                       :lemma input
                       :source 'dwds
                       :error (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (unless (eq result :async)
      (funcall callback result))))

(defun woerterbuch-core--parse-snippet-lemma-response ()
  "Parse current DWDS snippet response buffer and return a lemma or nil."
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
         (lemma (woerterbuch-core--extract-lemma-from-snippet-data data)))
    lemma))

(defun woerterbuch-core--normalize-lemma-via-frequency (input callback)
  "Normalize INPUT via the DWDS frequency endpoint and call CALLBACK.

INPUT is the original single-word query string. CALLBACK is the
continuation that receives the final normalization result plist."
  (let ((url-request-extra-headers
         '(("User-Agent" . "woerterbuch/0.1"))))
    (url-retrieve
     (woerterbuch-core--build-lemma-frequency-url input)
     #'woerterbuch-core--normalize-lemma-frequency-callback
     (list input callback)
     t
     t)))

(defun woerterbuch-core--normalize-lemma-frequency-callback (status input callback)
  "Handle DWDS frequency response STATUS for INPUT and CALLBACK.

STATUS is the plist returned by `url-retrieve'. INPUT is the original
query string. CALLBACK is the continuation that receives the final
normalization result plist."
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
                    (woerterbuch-core--parse-frequency-lemma-response input)))
                (error
                 (list :ok nil
                       :input input
                       :lemma input
                       :source 'dwds
                       :error (error-message-string err)))))
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))
    (funcall callback result)))

(defun woerterbuch-core--parse-frequency-lemma-response (input)
  "Parse current DWDS frequency response buffer for INPUT.

INPUT is the original single-word query string used as fallback if the
response does not contain a usable lemma."
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
    (woerterbuch-core--lemma-result
     input
     (if (and (stringp lemma)
              (not (string-empty-p lemma)))
         lemma
       input))))

;;; Fetching

(defun woerterbuch-core--build-wrapper (input lemma source-results)
  "Create wrapper result for INPUT, LEMMA, and SOURCE-RESULTS."
  (let ((wrapper (woerterbuch-core-make-wrapper input lemma)))
    (plist-put wrapper :sources source-results)))

(defun woerterbuch-core--fetch-all-with-lemma
    (input lemma sources sections final-callback)
  "Fetch SECTIONS from SOURCES for INPUT using LEMMA as backend query.

INPUT is the original query string. LEMMA is the normalized query sent
to all backends. SECTIONS is the requested section list.

SOURCES is the ordered list of backends to query.  Each element must be
a source symbol such as `dwds', `duden', `openthesaurus' or
`wiktionary'.  The order determines the order of source results in the
returned wrapper plist.

FINAL-CALLBACK is called exactly once with a wrapper plist of the form:
  (:input INPUT :lemma LEMMA :sources SOURCES)
where SOURCES in the returned plist is a list of normalized source
results in stable source order."
  (let* ((pending (length sources))
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
           (sources woerterbuch-sources)
           (sections woerterbuch-default-sections)
           (normalize-lemma woerterbuch-normalize-lemma))
  "Fetch dictionary data for INPUT from configured SOURCES.

INPUT is the original user query string. FINAL-CALLBACK is a function
that is called once with a wrapper plist:
  (:input INPUT :lemma LEMMA :sources SOURCES)
where SOURCES in the returned plist is a list of normalized source
results in stable source order.

INPUT may be a word, phrase, or idiom, depending on backend support.

SOURCES is the ordered list of backends to query.  Each element must be
a source symbol such as `dwds', `duden', `openthesaurus' or
`wiktionary'.  When nil or omitted, use `woerterbuch-sources'.

SECTIONS limits requested data to keys such as `:definitions',
`:examples', `:origin', `:synonyms' or `:idioms'.  When nil or
omitted, use `woerterbuch-default-sections'.

When NORMALIZE-LEMMA is non-nil, normalize INPUT through DWDS before
querying the backends.  The normalized form becomes LEMMA in the
wrapper plist.  When normalization fails or is disabled, LEMMA is
INPUT."
  (if normalize-lemma
      (woerterbuch-core-normalize-lemma
       input
       (lambda (lemma-result)
         (woerterbuch-core--fetch-all-with-lemma
          input
          (or (plist-get lemma-result :lemma) input)
          sources
          sections
          final-callback)))
    (woerterbuch-core--fetch-all-with-lemma
     input
     input
     sources
     sections
     final-callback)))

(cl-defun woerterbuch-fetch-all-sync
    (input &key
           (sources woerterbuch-sources)
           (sections woerterbuch-default-sections)
           (normalize-lemma woerterbuch-normalize-lemma)
           timeout)
  "Synchronously fetch dictionary data for INPUT from configured SOURCES.

This function blocks until all requested source results have been
collected, or until TIMEOUT is reached. Unlike `woerterbuch-fetch-all',
the caller does not receive results later through a callback.

Fetch the requested SECTIONS from each selected source and return a
wrapper plist with the original INPUT, the lemma used for backend
lookups, and the collected per-source results:
  (:input input :lemma lemma :sources source-results)
Source-results is a list of result plists, one for each requested
source, in the same order as SOURCES. Each result contains the data
returned by that source for the requested SECTIONS, or an error entry
if the source failed or timed out.

INPUT is the original user query string.

SOURCES is the ordered list of backends to query. For allowed sources
see `woerterbuch-sources'. When nil, use `woerterbuch-sources'.

SECTIONS is a list limiting requested data to sections keys such as
`:definitions', `:examples', `:origin', `:synonyms' or `:idioms'. When nil, use
`woerterbuch-default-sections'.

When NORMALIZE-LEMMA is non-nil, normalize INPUT through DWDS before
querying the backends. The normalized form becomes LEMMA in the
wrapper plist. When normalization fails or is disabled, LEMMA is
INPUT.

TIMEOUT is the maximum total wait time in seconds. When nil, use the
largest configured timeout for the requested SOURCES plus one second.
This is sufficient because the individual source requests are started
concurrently, even though this function waits synchronously for their
combined result."
  (let* ((done nil)
         (result nil)
         (timeout
          (or timeout
              (let
                  ((timeouts
                    (delq nil
                          (mapcar
                           (lambda (s)
                             (cdr (assq s woerterbuch-source-timeouts)))
                           sources))))
                (if timeouts
                    (apply #'max timeouts)
                  woerterbuch-default-source-timeout))))
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
     :sources sources
     :sections sections
     :normalize-lemma normalize-lemma)
    (while (and (not done)
                (< (float-time) deadline))
      (accept-process-output nil woerterbuch-sync-poll-interval))
    (unless done
      (error "Timed out waiting in woerterbuch-fetch-all-sync after %ss"
             timeout))
    result))

(provide 'woerterbuch-core)

;;; woerterbuch-core.el ends here
