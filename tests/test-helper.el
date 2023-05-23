;;; test-helper.el --- Helper functions  -*- lexical-binding: t -*-

;; Helper functions for all tests.

;;; Variables

;;; Functions

(defun test-helper-file-read-contents (path)
  "Return the contents of file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(provide 'test-helper)

;;; test-helper.el ends here
