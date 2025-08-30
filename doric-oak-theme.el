;;; doric-oak-theme.el --- Minimalist theme with light background and woody+earthly hues -*- lexical-binding:t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/ef-themes
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A collection of highly legible, minimalist themes.  If you want
;; something more colourful, use my `ef-themes'.  For a "good default"
;; theme, try my `modus-themes'.
;;
;; The backronym of the `doric-themes' is: Doric Only Really
;; Intensifies Conservatively ... themes.

;;; Code:

(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'doric-themes t))
    (require 'doric-themes))

  (defvar doric-oak-palette
    '((cursor "#497020")
      (bg-main "#e0d8c7")
      (fg-main "#3a2018")
      (border "#8f9373")

      (bg-shadow-subtle "#d5c9b5")
      (fg-shadow-subtle "#6b5225")

      (bg-neutral "#c2b19e")
      (fg-neutral "#53402f")

      (bg-shadow-intense "#b1bf88")
      (fg-shadow-intense "#3f5000")
      
      (bg-accent "#b7d2b7")
      (fg-accent "#0f5420")

      (fg-red "#982500")
      (fg-green "#226700")
      (fg-yellow "#595000")
      (fg-blue "#103077")
      (fg-magenta "#700054")
      (fg-cyan "#005460")

      (bg-red "#e3b8a0")
      (bg-green "#b8caa0")
      (bg-yellow "#e2e0a8")
      (bg-blue "#c4c8dd")
      (bg-magenta "#d8bade")
      (bg-cyan "#bee0db"))
  "Palette of `doric-oak' theme.")

  (doric-themes-define-theme doric-oak light "Minimalist theme with light background and woody+earthly hues"))

(provide 'doric-oak-theme)
;;; doric-oak-theme.el ends here
