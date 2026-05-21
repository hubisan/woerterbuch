;;; test-helper.el --- Helper functions  -*- lexical-binding: t; no-byte-compile: t -*-

;; Helper functions for all tests.

;;; Variables

;;; Functions

(defun test-helper-woerterbuch--tests-dir ()
  "Return the tests directory this helper file lives in."
  (let ((file (or (symbol-file 'test-helper-woerterbuch--tests-dir)
                  load-file-name
                  buffer-file-name)))
    (unless file
      (error "Cannot determine tests directory"))
    (file-name-directory (expand-file-name file))))

(defun test-helper-woerterbuch--test-file (file)
  "Return absolute path to FILE below the tests directory."
  (expand-file-name file (test-helper-woerterbuch--tests-dir)))

(defun test-helper-woerterbuch--files-dir ()
  "Return the generated test files directory."
  (test-helper-woerterbuch--test-file "files/"))

(defun test-helper-woerterbuch--http-status ()
  "Return HTTP status code for the current url buffer, or nil."
  (or (and (boundp 'url-http-response-status)
           (numberp url-http-response-status)
           url-http-response-status)
      (save-excursion
        (goto-char (point-min))
        (when (looking-at "HTTP/[0-9.]+[[:space:]]+\\([0-9]+\\)")
          (string-to-number (match-string 1))))))

(defun test-helper-woerterbuch--goto-body ()
  "Move point to the start of the HTTP response body."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers)
           (integerp url-http-end-of-headers))
      (goto-char url-http-end-of-headers)
    (re-search-forward "\r?\n\r?\n" nil 'move)))

(defun test-helper-woerterbuch--download-url (url file headers)
  "Download URL with HEADERS and save the response body to FILE.
Return the HTTP status code."
  (let ((url-request-extra-headers headers)
        (buffer (url-retrieve-synchronously url t t)))
    (unless buffer
      (error "Could not retrieve %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (test-helper-woerterbuch--http-status)))
            (when (or (null status) (< status 400))
              (make-directory (file-name-directory file) t)
              (test-helper-woerterbuch--goto-body)
              (write-region (point) (point-max) file nil 'silent))
            status))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'test-helper)

;;; test-helper.el ends here
