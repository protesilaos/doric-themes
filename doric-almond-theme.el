;;; doric-almond-theme.el --- Minimalist theme with light background and green+magenta hues -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/doric-themes
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

  (defvar doric-almond-palette
    '((cursor "#902728")
      (bg-main "#fef8ed")
      (fg-main "#40282e")
      (border "#c9a3b4")

      (bg-shadow-subtle "#efe4e6")
      (fg-shadow-subtle "#6a6098")

      (bg-neutral "#dfd9de")
      (fg-neutral "#4e4053")

      (bg-shadow-intense "#cce7b4")
      (fg-shadow-intense "#206038")

      (bg-accent "#fee1ce")
      (fg-accent "#880058")

      (fg-red "#a00040")
      (fg-green "#006730")
      (fg-yellow "#704000")
      (fg-blue "#203080")
      (fg-magenta "#800060")
      (fg-cyan "#005560")

      (bg-red "#f4bbc7")
      (bg-green "#aeddd4")
      (bg-yellow "#e5e0b0")
      (bg-blue "#bbcce8")
      (bg-magenta "#e0c0e7")
      (bg-cyan "#c2e7f8"))
  "Palette of `doric-almond' theme.")

  (doric-themes-define-theme doric-almond light "Minimalist theme with light background and green+magenta hues"))

(provide 'doric-almond-theme)
;;; doric-almond-theme.el ends here
