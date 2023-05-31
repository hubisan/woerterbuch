;;; test-woerterbuch.el --- Tests  -*- lexical-binding:t -*-

;; Testing W

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'test-helper)

(require 'woerterbuch)

;;; Helpers

;;; Tests

(describe "Synonyms"
  (it "can be retrieved as plist"
    (expect t :to-be t)))

(provide 'test-woerterbuch)

;;; test-woerterbuch.el ends here
