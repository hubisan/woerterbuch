;;; test-woerterbuch-wiktionary.el --- Wiktionary backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'woerterbuch-wiktionary)

(defconst test-woerterbuch-wiktionary--sections
  '(:definitions :examples :synonyms :origin :idioms))

(defun test-woerterbuch-wiktionary--fixture (name)
  "Return absolute Wiktionary fixture path for NAME."
  (expand-file-name name
                    (expand-file-name "tests/files/wiktionary"
                                      default-directory)))

(defun test-woerterbuch-wiktionary--parse-response-file (name input)
  "Parse local Wiktionary HTML fixture NAME for INPUT."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n\n")
    (insert-file-contents (test-woerterbuch-wiktionary--fixture name))
    (let ((url-http-response-status 200))
      (woerterbuch-wiktionary--parse-current-buffer
       input
       test-woerterbuch-wiktionary--sections))))

(describe "Wiktionary Backend:"
  (it "parses Bank into two noun homographs"
    (let* ((result (test-woerterbuch-wiktionary--parse-response-file
                    "wiktionary-bank.html"
                    "Bank"))
           (homographs (plist-get result :homographs))
           (first (nth 0 homographs))
           (second (nth 1 homographs))
           (first-def (car (plist-get first :definitions)))
           (second-def (car (plist-get second :definitions))))
      (expect (plist-get result :source) :to-equal 'wiktionary)
      (expect (plist-get result :lemma) :to-equal "Bank")
      (expect (plist-get result :url)
              :to-equal "https://de.wiktionary.org/wiki/Bank")
      (expect (length homographs) :to-equal 2)
      (expect (plist-get first :title) :to-equal "Bank, Substantiv, f")
      (expect (plist-get second :title) :to-equal "Bank, Substantiv, f")
      (expect (plist-get first :wortart) :to-equal "Substantiv")
      (expect (plist-get second :grammar) :to-equal "Substantiv")
      (expect (plist-get first-def :label) :to-equal "1")
      (expect (plist-get first-def :definition)
              :to-match "Sitz- oder Ablagegelegenheit")
      (expect (plist-get second-def :definition)
              :to-match "Geldinstitut")
      (expect (car (plist-get first-def :examples))
              :to-match "Diese Bank war")
      (expect (plist-get first :origin) :to-match "banki-")
      (expect (plist-get first :idioms)
              :to-contain "durch die Bank")
      (expect (plist-get first :idioms)
              :to-contain "etwas auf die lange Bank schieben")
      (expect (plist-get first :idioms)
              :to-contain "vor leeren Bänken sprechen")
      (expect (plist-get second :idioms)
              :to-contain "Die Bank gewinnt immer.")
      (expect (plist-get first :synonyms)
              :to-equal
              '((:sense "2" :items ("Bankett"))
                (:sense "5" :items ("Tisch" "Theke" "Werkbank"))
                (:sense "6" :items ("Auswechselbank" "Ersatzbank"))))
      (expect (plist-get second :synonyms)
              :to-equal
              '((:sense "1" :items ("Geldhaus"
                                    "Geldinstitut"
                                    "Finanzinstitut"
                                    "Finanzunternehmen"
                                    "Kreditinstitut"
                                    "Bankhaus"))
                (:sense "2" :items ("Kasino" "Spielbank"))))))

  (it "parses unlabeled idiom entries without appending gloss text"
    (let* ((snippet
            "<div><dl><dd>[1] <a href=\"/wiki/vor_Freude_an_die_Decke_springen\">vor Freude an die Decke springen</a></dd><dd><a href=\"/wiki/dem_Tod_von_der_Schippe_springen\">dem Tod von der Schippe springen</a></dd><dd><a href=\"/wiki/der_springende_Punkt\">der springende Punkt</a></dd><dd><a href=\"/wiki/in_St%C3%BCcke_springen\">in Stücke springen</a> - <i><a href=\"/wiki/zerspringen\">zerspringen</a></i></dd></dl></div>")
           (dom (with-temp-buffer
                  (insert snippet)
                  (libxml-parse-html-region (point-min) (point-max))))
           (blocks (list (cons :idioms (list dom)))))
      (expect (woerterbuch-wiktionary--idioms blocks)
              :to-equal
              '("vor Freude an die Decke springen"
                "dem Tod von der Schippe springen"
                "der springende Punkt"
                "in Stücke springen"))))

  (it "parses Haus and ignores the later declension-only section"
    (let* ((result (test-woerterbuch-wiktionary--parse-response-file
                    "wiktionary-haus.html"
                    "Haus"))
           (homographs (plist-get result :homographs))
           (entry (car homographs)))
      (expect (length homographs) :to-equal 1)
      (expect (plist-get entry :title) :to-equal "Haus, Substantiv, n")
      (expect (plist-get entry :wortart) :to-equal "Substantiv")
      (expect (length (plist-get entry :definitions)) :to-equal 15)
      (expect (plist-get (car (plist-get entry :definitions)) :definition)
              :to-match "zu einem bestimmten Zweck erbautes Gebaeude")
      (expect (plist-get entry :origin) :to-match "seit dem 8. Jahrhundert")
      (expect (plist-get entry :synonyms)
              :to-contain
              '(:sense "8" :items ("Haushalt" "Hausstand")))))

  (it "parses Zaun including merged synonyms and related words"
    (let* ((result (test-woerterbuch-wiktionary--parse-response-file
                    "wiktionary-zaun.html"
                    "Zaun"))
           (entry (car (plist-get result :homographs))))
      (expect (length (plist-get result :homographs)) :to-equal 1)
      (expect (plist-get entry :title) :to-equal "Zaun, Substantiv, m")
      (expect (length (plist-get entry :definitions)) :to-equal 1)
      (expect (plist-get (car (plist-get entry :definitions)) :definition)
              :to-match "Vorrichtung")
      (expect (plist-get entry :origin) :to-match "seit dem 8. Jahrhundert")
      (expect (plist-get entry :idioms)
              :to-contain "ein lebender Zaun")
      (expect (plist-get entry :synonyms)
              :to-equal
              '((:sense "1" :items ("Abzäunung"
                                    "Einfriedigung"
                                    "Einfriedung"
                                    "Einzäunung"
                                    "Umzäunung"
                                    "Befriedung"
                                    "Umfriedigung"
                                    "Umfriedung"
                                    "Einhegung"
                                    "Hag"
                                    "Fence"
                                    "Fenz"
                                    "Eingrenzung"
                                    "Gatter"
                                    "Gitter"
                                    "Hecke"
                                    "Palisade"))))))

  (it "returns the expected no-match result object"
    (expect
     (test-woerterbuch-wiktionary--parse-response-file
      "wiktionary-existiertnicht.html"
      "Existiertnicht")
     :to-equal
     '(:source wiktionary
       :lemma "Existiertnicht"
       :ok nil
       :homographs nil
       :error "No matches found"
       :url "https://de.wiktionary.org/wiki/Existiertnicht"))))

(provide 'test-woerterbuch-wiktionary)

;;; test-woerterbuch-wiktionary.el ends here
