;;; test-helper.el --- Helper functions  -*- lexical-binding: t -*-

;; Helper functions for all tests.

;;; Variables

;;; Functions

(defun test-helper-get-file-in-test-dir (file)
  "Return the absolute path of the FILE located inside the testing directory.
Example:
  (test-helper-get-file-in-test-dir \"test.txt\")
  Returns the absolute path to \"tests/test.txt\""
  (let* ((pkg-dir (expand-file-name default-directory))
         (tests-dir (expand-file-name "tests" pkg-dir)))
    (expand-file-name file tests-dir)))

(defun test-helper-file-read-contents (path)
  "Return the contents of file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(provide 'test-helper)

;;; test-helper.el ends here
