;;; test-woerterbuch-wiktionary.el --- Wiktionary backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'test-helper)

(describe "Wiktionary parser internals"
  (it "parses list-based blocks without dl/dd wrappers"
    (with-temp-buffer
      (insert
       "<html><body><section><div><h3>Substantiv</h3></div>"
       "<p style=\"font-weight:bold\">Bedeutungen:</p>"
       "<ol><li>[1] erste Bedeutung</li><li>[2] zweite Bedeutung</li></ol>"
       "<p style=\"font-weight:bold\">Beispiele:</p>"
       "<ul><li>[1] erstes Beispiel</li><li>[2] zweites Beispiel</li></ul>"
       "<p style=\"font-weight:bold\">Synonyme:</p>"
       "<ul><li>[1] <a href=\"/wiki/Eins\">Eins</a>, <a href=\"/wiki/Erstens\">Erstens</a></li></ul>"
       "<p style=\"font-weight:bold\">Redewendungen:</p>"
       "<ul><li>[1] <a href=\"/wiki/etwas_tun\">etwas tun</a>/<a href=\"/wiki/anders_tun\">anders tun</a></li></ul>"
       "<p style=\"font-weight:bold\">Herkunft:</p>"
       "<ul><li>aus dem Testbestand</li></ul>"
       "</section></body></html>")
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
             (section
              (woerterbuch-wiktionary--find-first
               dom
               (lambda (node)
                 (and (eq (dom-tag node) 'section)
                      (equal
                       (woerterbuch-wiktionary--heading-node-level-and-text node)
                       '(3 . "Substantiv"))))))
             (blocks (woerterbuch-wiktionary--collect-labeled-blocks section)))
        (expect (woerterbuch-wiktionary--origin-text blocks)
                :to-equal "aus dem Testbestand")
        (expect (woerterbuch-wiktionary--idioms blocks)
                :to-equal '("etwas tun; anders tun"))
        (expect (woerterbuch-wiktionary--synonyms blocks)
                :to-equal '((:sense "1" :items ("Eins" "Erstens"))))
        (expect
         (woerterbuch-wiktionary--definition-list
          blocks
          '(:definitions :examples))
         :to-equal
         '((:id 1 :label "1" :definition "erste Bedeutung"
                 :qualifiers nil :examples ("erstes Beispiel")
                 :definitions nil)
           (:id 2 :label "2" :definition "zweite Bedeutung"
                 :qualifiers nil :examples ("zweites Beispiel")
                 :definitions nil)))
        (expect (woerterbuch-wiktionary--item-texts
                 (woerterbuch-wiktionary--block-nodes blocks :definitions))
                :to-equal
                '("[1] erste Bedeutung" "[2] zweite Bedeutung"))))))

(describe "Wiktionary backend"
  (dolist (word test-helper-woerterbuch-output-words)
    (let ((word word))
      (describe word
        (dolist (section test-helper-woerterbuch-output-sections)
          (let ((section section))
            (it (format "matches expected %s output"
                        (symbol-name section))
              (expect
               (test-helper-woerterbuch-fetch-expected-output
                'wiktionary word section)
               :to-equal
               (test-helper-woerterbuch-read-expected
                'wiktionary word section)))))))))

(provide 'test-woerterbuch-wiktionary)

;;; test-woerterbuch-wiktionary.el ends here
