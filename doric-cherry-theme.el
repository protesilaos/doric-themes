;;; doric-cherry-theme.el --- Minimalist theme with light background and pink+purple hues -*- lexical-binding:t -*-

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

  (defvar doric-cherry-palette
    '((cursor "#a02050")
      (bg-main "#f7edf1")
      (fg-main "#35292f")
      (border "#c4a7b0")

      (bg-shadow-subtle "#e5dde0")
      (fg-shadow-subtle "#675462")

      (bg-neutral "#d7c9d0")
      (fg-neutral "#4e4053")

      (bg-shadow-intense "#cc95b7")
      (fg-shadow-intense "#683455")

      (bg-accent "#ecc0e4")
      (fg-accent "#a03068")

      (fg-red "#a00040")
      (fg-green "#006730")
      (fg-yellow "#704000")
      (fg-blue "#203080")
      (fg-magenta "#800060")
      (fg-cyan "#005560")

      (bg-red "#f2c0c5")
      (bg-green "#b9e2d0")
      (bg-yellow "#e9e6b0")
      (bg-blue "#bbcce8")
      (bg-magenta "#e0c0e7")
      (bg-cyan "#c2ebe8"))
  "Palette of `doric-cherry' theme.")

  (doric-themes-define-theme doric-cherry light "Minimalist theme with light background and pink+purple hues"))

(provide 'doric-cherry-theme)
;;; doric-cherry-theme.el ends here
