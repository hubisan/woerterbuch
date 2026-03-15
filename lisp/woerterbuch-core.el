;;; woerterbuch-core.el --- Core helpers for woerterbuch -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst woerterbuch-core-sources
  '(openthesaurus)
  "Ordered list of enabled woerterbuch sources for Phase 1.")

(defun woerterbuch-core-make-result (source word)
  "Create normalized success result for SOURCE and WORD."
  (list :source source
        :word word
        :ok t
        :definitions nil
        :synonyms nil
        :origin nil
        :idioms nil))

(defun woerterbuch-core-make-error (source word message)
  "Create normalized error result for SOURCE, WORD, and MESSAGE."
  (list :source source
        :word word
        :ok nil
        :error message))

(defun woerterbuch-core-section-requested-p (section sections)
  "Return non-nil when SECTION is present in SECTIONS."
  (memq section sections))

(defun woerterbuch-core--source-fetcher (source)
  "Return fetch function symbol for SOURCE."
  (pcase source
    ('openthesaurus #'woerterbuch-openthesaurus-fetch)
    (_ (error "Unknown woerterbuch source: %S" source))))

(defun woerterbuch-fetch-all (word sections final-callback)
  "Fetch WORD for SECTIONS from all configured sources.

FINAL-CALLBACK is called exactly once with a list of normalized
results in stable source order."
  (let* ((sources woerterbuch-core-sources)
         (pending (length sources))
         (results (make-hash-table :test #'eq))
         (done nil))
    (if (zerop pending)
        (funcall final-callback nil)
      (dolist (source sources)
        (let ((fetcher (woerterbuch-core--source-fetcher source)))
          (funcall
           fetcher
           word
           sections
           (lambda (result)
             (unless done
               (puthash source result results)
               (setq pending (1- pending))
               (when (zerop pending)
                 (setq done t)
                 (funcall
                  final-callback
                  (mapcar (lambda (s) (gethash s results)) sources)))))))))))

(provide 'woerterbuch-core)

;;; woerterbuch-core.el ends here
