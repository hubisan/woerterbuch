;;; test-synoyms.el --- Tests  -*- lexical-binding:t -*-

;; Testing W

;;; Requirements

(require 'buttercup)
(require 'ert)

(require 'woerterbuch)

;;; Helpers

;;; Tests

(describe "Synonyms"
  (it "can be retrieved as plist"
    (expect t :to-be t)))

(provide 'test-synoyms)

;;; test-synoyms.el ends here
