; -*- mode: emacs-lisp; lexical-binding: t -*-

(eval-when-compile
  (defvar sentence-end-double-space)
  (defvar checkdoc-verb-check-experimental-flag))

;; Domain Specific Language
;; https://emacs-eask.github.io/DSL/
(package "woerterbuch"
         "0.1.0"
         "Lookup definitions and synonyms for German words")

(website-url "https://github.com/hubisan/woerterbuch")
(keywords "dictionary" "thesaurus" "convenience")
(license "GPLv3")

(package-file "lisp/woerterbuch.el")
(files "lisp/*.el")

(load-paths "./lisp/" "./tests/")

;; https://emacs-eask.github.io/DSL/#-source-alias
(source "gnu")
(source "nongnu")
(source "melpa")

;; https://emacs-eask.github.io/DSL/#-depends-on-package-name-rest-recipe
(depends-on "emacs" "29.1")

;; https://emacs-eask.github.io/DSL/#-development-rest-body
(development
 (depends-on "buttercup")
 (depends-on "with-simulated-input"))

;; Tell checkdoc not to demand two spaces after a period.
(setq sentence-end-double-space nil)

;; Dont' check docstring for correct verb use.
(setq checkdoc-verb-check-experimental-flag nil)
