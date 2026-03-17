;;; test-woerterbuch-duden.el --- Duden backend tests -*- lexical-binding:t; no-byte-compile: t -*-

(require 'buttercup)

(require 'woerterbuch-duden)

(defconst test-woerterbuch-duden--sections
  '(:definitions :examples :synonyms :origin :idioms))

(defun test-woerterbuch-duden--fixture (name)
  "Return absolute Duden fixture path for NAME."
  (expand-file-name name
                    (expand-file-name "tests/files"
                                      default-directory)))

(defun test-woerterbuch-duden--parse-entry (file input &optional url homograph-id)
  "Parse local Duden FILE for INPUT.

Optional URL and HOMOGRAPH-ID are forwarded to the parser."
  (woerterbuch-duden--parse-html-file
   (test-woerterbuch-duden--fixture file)
   input
   test-woerterbuch-duden--sections
   url
   homograph-id))

(defun test-woerterbuch-duden--single-result (file input url)
  "Return full Duden source result for FILE, INPUT and URL."
  (woerterbuch-duden--result-from-homographs
   input
   (list (test-woerterbuch-duden--parse-entry file input url 1))))

(describe "Duden Backend:"
  (it "parses Haus fixture into the expected source result object"
    (expect
     (test-woerterbuch-duden--single-result
      "duden-rechtschreibung-haus.html"
      "Haus"
      "https://www.duden.de/rechtschreibung/Haus?amp")
     :to-equal
     '(:source duden :lemma "Haus" :ok t :url
               ("https://www.duden.de/rechtschreibung/Haus?amp") :homographs
               ((:id 1 :lemma "Haus" :title "Haus, das" :wortart "Substantiv"
                     :grammar "Substantiv, Neutrum" :origin
                     "mittelhochdeutsch, althochdeutsch hūs, eigentlich = das Bedeckende, Umhüllende"
                     :idioms nil :synonyms
                     ("Anwesen" "Bau" "Bauwerk" "Gebäude") :url
                     "https://www.duden.de/rechtschreibung/Haus?amp"
                     :definitions
                     ((:id 1 :duden-id nil :label "1" :definition nil
                           :qualifiers nil :examples nil :idioms nil :image
                           nil :definitions
                           ((:id 1 :duden-id "Bedeutung-1a" :label "1a"
                                 :definition
                                 "Gebäude, das Menschen zum Wohnen dient"
                                 :qualifiers nil :examples
                                 ("ein großes, kleines, altes, mehrstöckiges, verwinkeltes Haus"
                                  "armselige, einfache, verkommene, baufällige, moderne Häuser"
                                  "das Haus seiner Eltern"
                                  "das Haus ist auf uns, in andere Hände übergegangen"
                                  "ein Haus bauen, einrichten, beziehen, bewohnen"
                                  "ein Haus [ver]mieten, [ver]kaufen"
                                  "ein Haus renovieren, umbauen"
                                  "ein eigenes Haus haben, besitzen"
                                  "Haus an Haus (nebeneinander) wohnen"
                                  "sie führten ihre Gäste durch das ganze Haus"
                                  "im elterlichen Haus[e] wohnen"
                                  "der Hausmeister jagte die spielenden Kinder aus dem Haus"
                                  "das väterliche Haus")
                                 :idioms
                                 ("Haus und Hof (der gesamte Besitz: er hat Haus und Hof verspielt, vertrunken)"
                                  "[jemandem] ins Haus stehen (umgangssprachlich: [jemandem] bevorstehen: eine Neuerung steht [ihm] ins Haus)")
                                 :image
                                 "https://cdn.duden.de/_media_/full/H/Haus-201020510799.jpg"
                                 :definitions nil)
                            (:id 2 :duden-id "Bedeutung-1b" :label "1b"
                                 :definition
                                 "Gebäude, das zu einem bestimmten Zweck errichtet wurde"
                                 :qualifiers nil :examples
                                 ("das große (besonders für Aufführungen von Opern, großen Schauspielen o. Ä. bestimmte), kleine (besonders für kleinere Bühnenstücke bestimmte) Haus des Theaters war bis auf den letzten Platz ausverkauft"
                                  "das weltberühmte Orchester hat auf seinen Tourneen volle Häuser (seine Konzerte sind ausverkauft)"
                                  "Haus (Hotel, Pension) Meeresblick"
                                  "(gehoben) das Haus des Herrn (Gotteshaus)"
                                  "das Weiße Haus in Washington (der Amtssitz des Präsidenten der USA)"
                                  "(veraltet verhüllend) ein öffentliches Haus (Bordell)"
                                  "das erste Haus (Hotel) am Platz[e]"
                                  "die Chefin ist zurzeit nicht im Haus[e] (im Gebäude der Firma) ; Abkürzung: i. H."
                                  "〈in übertragener Bedeutung:〉 das gemeinsame europäische Haus")
                                 :idioms
                                 ("Haus der offenen Tür (Gelegenheit, bei der Betriebe, Verwaltungsstellen usw. von allen Bürgern besichtigt werden können)"
                                  "um die Häuser ziehen (umgangssprachlich: ausgehen; ein Lokal nach dem anderen aufsuchen: wir sollten mal wieder zusammen um die Häuser ziehen; früher sind wir nächtelang um die Häuser gezogen)")
                                 :image nil :definitions nil)
                            (:id 3 :duden-id "Bedeutung-1c" :label "1c"
                                 :definition
                                 "Wohnung, Heim, in dem jemand ständig lebt"
                                 :qualifiers nil :examples
                                 ("jemandem das/sein Haus verbieten, öffnen"
                                  "(Kaufmannssprache) die Lieferung erfolgt frei Haus (ohne zusätzliche Transportkosten bis zum Bestimmungsort)"
                                  "(umgangssprachlich) das ganze Haus auf den Kopf stellen (so sehr nach etwas suchen, dass alles in Unordnung gerät)"
                                  "bei dieser Kälte gehe ich nicht aus dem Haus[e]"
                                  "außer Haus (nicht im Haus, auswärts) sein, essen"
                                  "er kommt mir nicht ins Haus"
                                  "nach Haus[e] gehen, fahren, kommen"
                                  "jemanden nach Haus[e] begleiten, bringen"
                                  "der Bettler ging von Haus zu Haus"
                                  "einige Zeit von Hause (umgangssprachlich; von zu Hause) fortbleiben"
                                  "an diesem Abend blieb, war, saß er zu Haus[e]"
                                  "Haus an Haus (nebeneinander) wohnen"
                                  "sie fühlt sich schon ganz [wie] zu Haus[e] (fühlt sich in einer neuen Umgebung nicht mehr fremd)"
                                  "von zu Hause abhauen, fort sein"
                                  "〈in übertragener Bedeutung:〉 aus dem Haus sein (nicht mehr bei den Eltern wohnen)"
                                  "〈in übertragener Bedeutung:〉 ein Paket, einen Brief nach Haus[e] (an die Angehörigen) schicken"
                                  "〈in übertragener Bedeutung:〉 sie ist, wohnt noch zu Haus[e] (bei den Eltern)"
                                  "〈in übertragener Bedeutung:〉 er war in Berlin zu Hause (wohnte in Berlin)"
                                  "〈in übertragener Bedeutung:〉 der Brauch des Osterreitens ist in der Lausitz zu Haus[e] (wird dort gepflegt; ist dort üblich; kommt von dort)"
                                  "〈in übertragener Bedeutung:〉 sie war überall zu Hause (kannte sich überall bestens aus)"
                                  "〈in übertragener Bedeutung:〉 ich bin für niemanden/für dich bin ich immer zu Haus[e] (zu sprechen)"
                                  "〈in übertragener Bedeutung:〉 der Verein spielt, tritt am Samstag zu Haus[e] (Sportjargon; auf dem eigenen Platz; vor einheimischem Publikum) [zum Wettkampf] an")
                                 :idioms
                                 ("das Haus hüten (aus irgendeinem Grund nicht mit andern nach draußen gehen [können], zu Hause bleiben [müssen])"
                                  "jemandem das Haus einrennen/einlaufen (umgangssprachlich: 1. jemanden ständig wegen einer Sache zu Hause aufsuchen und bedrängen. 2. [von Käufern, Käuferinnen o. Ä.] auf jemandes Angebot mit großem Zulauf, Interesse reagieren.)"
                                  "jemandem ins Haus schneien/geschneit kommen (umgangssprachlich: überraschend, unerwartet jemanden besuchen, bei jemandem auftauchen)"
                                  "auf einem bestimmten Gebiet/in etwas zu Hause sein (umgangssprachlich: sich mit, in etwas genau auskennen; mit, in etwas gut Bescheid wissen)"
                                  "mit etwas zu Hause bleiben (umgangssprachlich: etwas für sich behalten; jemanden mit der Mitteilung einer Belanglosigkeit verschonen: bleib du mit deinen Weisheiten lieber zu Hause!)"
                                  "mein Haus, meine Welt (Ausdruck der Zufriedenheit mit der häuslichen Umgebung)"
                                  "komm du nur nach Haus[e]! (Drohung als Ankündigung von Strafe, Schelte o. Ä.)")
                                 :image nil :definitions nil)))
                      (:id 2 :duden-id nil :label "2" :definition nil
                           :qualifiers nil :examples nil :idioms nil :image
                           nil :definitions
                           ((:id 1 :duden-id "Bedeutung-2a" :label "2a"
                                 :definition "Gesamtheit der Hausbewohner"
                                 :qualifiers ("Gebrauch: umgangssprachlich")
                                 :examples
                                 ("das Haus war vollzählig versammelt"
                                  "das ganze Haus lief zusammen")
                                 :idioms nil :image nil :definitions nil)
                            (:id 2 :duden-id "Bedeutung-2b" :label "2b"
                                 :definition
                                 "Gesamtheit von Personen, die sich in einer bestimmten Funktion in einem bestimmten Haus (1b) aufhalten, dort tätig sind"
                                 :qualifiers nil :examples
                                 ("das Hohe Haus (das Parlament)"
                                  "die beiden Häuser (Kammern) des Parlaments"
                                  "er hatte alle Geschäftsfreunde seines Hauses (seiner Firma) geladen"
                                  "das ganze Haus (gesamte Theaterpublikum) klatschte begeistert Beifall")
                                 :idioms nil :image nil :definitions nil)
                            (:id 3 :duden-id "Bedeutung-2c" :label "2c"
                                 :definition "Familie" :qualifiers
                                 ("Gebrauch: gehoben") :examples
                                 ("ein gastliches, bürgerliches, angesehenes Haus"
                                  "sie kommt aus bestem Hause"
                                  "er ist nicht mehr Herr im eigenen Haus (hat in der Familie nichts mehr zu sagen)"
                                  "sie verkehrt in den ersten Häusern (angesehensten Familien) der Stadt"
                                  "(in Grußformeln am Briefschluss) herzliche Grüße, mit den besten Grüßen von Haus zu Haus")
                                 :idioms
                                 ("von Haus[e] aus (1. von der Familie her: von Haus[e] aus ist sie sehr begütert. 2. seit jeher, von Natur aus: von Haus[e] aus ist er schüchtern. 3. ursprünglich, eigentlich: von Haus[e] aus ist er Tischler.)")
                                 :image nil :definitions nil)
                            (:id 4 :duden-id "Bedeutung-2d" :label "2d"
                                 :definition
                                 "Haushalt, Wirtschaft, Hauswesen einer Familie"
                                 :qualifiers nil :examples
                                 ("jemandem das Haus besorgen"
                                  "ein großes Haus führen (häufig Gäste haben und sie aufwendig bewirten)"
                                  "jemanden ins Haus nehmen")
                                 :idioms
                                 ("Haus und Herd (eigener Hausstand)"
                                  "[mit etwas] Haus halten (1. [mit etwas] sparsam wirtschaften; mittelhochdeutsch hūs halten = das Haus bewahren: mit dem Wirtschaftsgeld, den Vorräten Haus halten müssen. 2. sich etwas einteilen, [mit etwas] sparsam, ökonomisch umgehen: er hielt mit seinen Kräften nicht Haus.)"
                                  "sein/das Haus bestellen (gehoben: vor einer längeren Abwesenheit, vor dem Tode seinen Besitz, seine Angelegenheiten ordnen; ein Testament machen; nach Jesaja 38, 1)")
                                 :image nil :definitions nil)))
                      (:id 3 :duden-id "Bedeutung-3" :label "3" :definition
                           "Dynastie, [Herrscher]geschlecht" :qualifiers nil
                           :examples
                           ("das Haus Davids"
                            "ein Angehöriger des Hauses Habsburg"
                            "das Haus Rothschild" "aus fürstlichem Hause"
                            "sie stammt vom kaiserlichen Hause ab")
                           :idioms nil :image nil :definitions nil)
                      (:id 4 :duden-id "Bedeutung-4" :label "4" :definition
                           "Person, Mensch" :qualifiers
                           ("Gebrauch: umgangssprachlich scherzhaft")
                           :examples
                           ("er ist ein fideles, gemütliches, gelehrtes Haus"
                            "wie gehts, altes Haus (alter Freund)")
                           :idioms nil :image nil :definitions nil)
                      (:id 5 :duden-id nil :label "5" :definition nil
                           :qualifiers nil :examples nil :idioms nil :image
                           nil :definitions
                           ((:id 1 :duden-id "Bedeutung-5a" :label "5a"
                                 :definition
                                 "Tierkreiszeichen in seiner Zuordnung zu einem Planeten"
                                 :qualifiers ("Gebrauch: Astrologie")
                                 :examples nil :idioms nil :image nil
                                 :definitions nil)
                            (:id 2 :duden-id "Bedeutung-5b" :label "5b"
                                 :definition
                                 "einer der zwölf Abschnitte, in die der Tierkreis eingeteilt ist"
                                 :qualifiers ("Gebrauch: Astrologie")
                                 :examples ("die Sonne steht im elften Haus")
                                 :idioms nil :image nil :definitions nil)))))))))

  (it "parses Zaun fixture into the expected source result object"
    (expect
     (test-woerterbuch-duden--single-result
      "duden-rechtschreibung-zaun.html"
      "Zaun"
      "https://www.duden.de/rechtschreibung/Zaun?amp")
     :to-equal
     '(:source duden :lemma "Zaun" :ok t :url
               ("https://www.duden.de/rechtschreibung/Zaun?amp") :homographs
               ((:id 1 :lemma "Zaun" :title "Zaun, der" :wortart "Substantiv"
                     :grammar "Substantiv, maskulin" :origin
                     "mittelhochdeutsch, althochdeutsch zūn = Umzäunung, Hecke, Gehege"
                     :idioms nil :synonyms
                     ("Abzäunung" "Einzäunung" "Gatter" "Gitter") :url
                     "https://www.duden.de/rechtschreibung/Zaun?amp"
                     :definitions
                     ((:id 1 :duden-id nil :label "1" :definition
                           "Abgrenzung, Einfriedigung aus (parallel angeordneten, gekreuzten o. ä.) Metall- oder Holzstäben oder aus Drahtgeflecht"
                           :qualifiers nil :examples
                           ("ein hoher, niedriger, elektrischer Zaun"
                            "ein Zaun aus Latten"
                            "einen Zaun ziehen, errichten, reparieren, erneuern, anstreichen"
                            "die Kinder schlüpften durch den Zaun, kletterten über den Zaun")
                           :idioms
                           ("ein lebender Zaun (eine Hecke b)"
                            "mit etwas [nicht] hinter dem/hinterm Zaun halten (etwas Wesentliches [nicht] verschweigen)"
                            "einen Streit/Zwist/Krieg o. Ä. vom Zaun brechen (heraufbeschwören, plötzlich damit beginnen; eigentlich = so unvermittelt mit einem Streit beginnen, wie man eine Latte [als Waffe] von der nächsten Umzäunung bricht)")
                           :image
                           "https://cdn.duden.de/_media_/full/Z/Zaun-201100281681.jpg"
                           :definitions nil)))))))

  ;; This fixture covers the Sitzgelegenheit homograph directly. The
  ;; canonical 404 and search fallback are stored as separate fixtures and
  ;; can be exercised in dedicated flow tests later.
  (it "parses Bank fixture into the expected homograph object"
    (expect
     (test-woerterbuch-duden--parse-entry
      "duden-rechtschreibung-bank-sitzgelegenheit.html"
      "Bank"
      "https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp"
      1)
     :to-equal
     '(:id 1 :lemma "Bank" :title "Bank, die" :wortart "Substantiv"
       :grammar "Substantiv, feminin" :origin
       "mittelhochdeutsch, althochdeutsch banc = Bank, Tisch, ursprünglich = Erhöhung"
       :idioms nil :synonyms nil :url
       "https://www.duden.de/rechtschreibung/Bank_Sitzgelegenheit?amp"
       :definitions
       ((:id 1 :duden-id nil :label "1" :definition nil
             :qualifiers nil :examples nil :idioms nil :image nil
             :definitions
             ((:id 1 :duden-id "Bedeutung-1a" :label "1a"
                   :definition
                   "Sitzgelegenheit aus Holz, Stein o. Ä., die mehreren Personen nebeneinander Platz bietet"
                   :qualifiers nil :examples
                   ("sich auf eine Bank setzen"
                    "in der Schule in einer Bank (Schulbank) sitzen"
                    "der Angeklagte saß unruhig in der Bank (Anklagebank)")
                   :idioms
                   ("etwas auf die lange Bank schieben (umgangssprachlich: etwas Unangenehmes aufschieben, hinauszögern: er schob den Arztbesuch auf die lange Bank; eigentlich = bis zur Bearbeitung in den langen Aktentruhen der Gerichte aufbewahren lassen)"
                    "durch die Bank (umgangssprachlich: durchweg, ohne Ausnahme, ohne Unterschied: das Obst war durch die Bank frisch; eigentlich = in der Reihenfolge, wie die Leute auf einer Bank sitzen)"
                    "vor leeren Bänken (vor wenigen Zuhörenden, Zuschauenden: sie spielten vor leeren Bänken)")
                   :image
                   "https://cdn.duden.de/_media_/full/B/Bank-201100043436.jpg"
                   :definitions nil)
              (:id 2 :duden-id "Bedeutung-1b" :label "1b"
                   :definition "Auswechselbank" :qualifiers
                   ("Gebrauch: Sport") :examples nil :idioms nil :image
                   "https://cdn.duden.de/_media_/full/B/Bank-201100287392.jpg"
                   :definitions nil)))
        (:id 2 :duden-id nil :label "2" :definition nil
             :qualifiers nil :examples nil :idioms nil :image nil
             :definitions
             ((:id 1 :duden-id "Bedeutung-2a" :label "2a"
                   :definition nil :qualifiers
                   ("Kurzform für: verschiedene Handwerkstische wie Drehbank, Hobelbank, Werkbank u. a.")
                   :examples ("an der Bank arbeiten") :idioms nil :image
                   nil :definitions nil)
              (:id 2 :duden-id "Bedeutung-2b" :label "2b"
                   :definition "bankförmiges Turngerät" :qualifiers nil
                   :examples nil :idioms nil :image nil :definitions nil)))
        (:id 3 :duden-id nil :label "3" :definition nil
             :qualifiers nil :examples nil :idioms nil :image nil
             :definitions
             ((:id 1 :duden-id "Bedeutung-3a" :label "3a"
                   :definition nil :qualifiers
                   ("Kurzform für: Sandbank") :examples nil :idioms nil
                   :image nil :definitions nil)
              (:id 2 :duden-id "Bedeutung-3b" :label "3b"
                   :definition
                   "Anhäufung von Meereslebewesen, die eine Erhöhung über dem Meeresgrund hervorruft"
                   :qualifiers nil :examples
                   ("hohe Bänke von Austern, Korallen") :idioms nil :image
                   nil :definitions nil)
              (:id 3 :duden-id "Bedeutung-3c" :label "3c"
                   :definition "lange Wolken- oder Dunstschicht"
                   :qualifiers nil :examples nil :idioms nil :image nil
                   :definitions nil)
              (:id 4 :duden-id "Bedeutung-3d" :label "3d"
                   :definition
                   "vom umliegenden Gestein gesonderte, fest zusammenhängende Gesteinsschicht"
                   :qualifiers ("Gebrauch: Geologie") :examples nil
                   :idioms nil :image nil :definitions nil)))
        (:id 4 :duden-id "Bedeutung-4" :label "4" :definition
             "unverändert beibehaltene Vorhersage auf Tippscheinen"
             :qualifiers nil :examples
             ("eine Bank tippen"
              "dieses Spiel ist eine Bank (kann man als Bank tippen)"
              "〈in übertragener Bedeutung:〉 diese Stürmerin ist eine Bank in unserem Team (umgangssprachlich; man kann sich hundertprozentig auf sie verlassen); dieses Geschenk ist eine Bank (ein sicherer Erfolg)")
             :idioms nil :image nil :definitions nil)
        (:id 5 :duden-id "Bedeutung-5" :label "5" :definition
             "Ausgangsstellung auf dem Boden mit auf Knie und Arme gestütztem Körper"
             :qualifiers ("Gebrauch: Sport") :examples nil :idioms nil
             :image nil :definitions nil))))))

(provide 'test-woerterbuch-duden)

;;; test-woerterbuch-duden.el ends here
