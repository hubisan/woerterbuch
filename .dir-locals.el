((emacs-lisp-mode
  . ((indent-tabs-mode . nil)
     (fill-column . 80)
     (sentence-end-double-space . nil)
     (checkdoc-verb-check-experimental-flag . nil)
     ;; This is the default outline-regexp in Emacs 29.
     (outline-regexp . ";;;;* [^ 	\n]\\|(\\|\\(^;;;###\\(\\([-[:alnum:]]+?\\)-\\)?\\(autoload\\)\\)")
     (outline-heading-end-regexp . "\n")))))))
