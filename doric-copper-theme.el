;;; doric-copper-theme.el --- Minimalist theme with dark background and orange hues -*- lexical-binding:t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

  (defvar doric-copper-palette
    '((cursor "#74e0ab")
      (bg-main "#44312e")
      (fg-main "#e7d0bf")
      (border "#85787f")

      (bg-shadow-subtle "#584040")
      (fg-shadow-subtle "#9aa3af")

      (bg-neutral "#654f52")
      (fg-neutral "#d0cfc8")

      (bg-shadow-intense "#7e462c")
      (fg-shadow-intense "#e2a077")

      (bg-accent "#634047")
      (fg-accent "#d29cb7")

      (fg-red "#eca28f")
      (fg-green "#b9d0aa")
      (fg-yellow "#c0b060")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#6e2e27")
      (bg-green "#355029")
      (bg-yellow "#624e2f")
      (bg-blue "#304061")
      (bg-magenta "#643870")
      (bg-cyan "#35526f"))
  "Palette of `doric-copper' theme.")

  (doric-themes-define-theme doric-copper dark "Minimalist theme with dark background and orange hues"))

(provide 'doric-copper-theme)
;;; doric-copper-theme.el ends here
