;;; woerterbuch.el --- Lookup definitions and synonyms for German words -*- lexical-binding: t -*-

;; Copyright (C) 2026 Daniel Hubmann

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/woerterbuch
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: dictionary, thesaurus, convenience

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Check out the documentation in the README for more.

;;; Code:

;;; Requirements

(require 'woerterbuch-core)
(require 'woerterbuch-duden)
(require 'woerterbuch-dwds)
(require 'woerterbuch-openthesaurus)
(require 'woerterbuch-wiktionary)

;;; Customization

(defgroup woerterbuch nil
  "German dictionary and thesaurus."
  :group 'convenience
  :link '(url-link "https://github.com/hubisan/woerterbuch"))

(provide 'woerterbuch)

;;; woerterbuch.el ends here
