;;; doric-valley-theme.el --- Minimalist theme with dark background and indigo+gold hues -*- lexical-binding:t -*-

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

  (defvar doric-valley-palette
    '((cursor "#a4bedb")
      (bg-main "#383035")
      (fg-main "#e0d5b7")
      (border "#7a787f")

      (bg-shadow-subtle "#484040")
      (fg-shadow-subtle "#afa497")

      (bg-neutral "#554f52")
      (fg-neutral "#d9cfbe")

      (bg-shadow-intense "#5a5470")
      (fg-shadow-intense "#d2cae7")

      (bg-accent "#714d4c")
      (fg-accent "#f6c097")

      (fg-red "#eca28f")
      (fg-green "#b9d0aa")
      (fg-yellow "#c0b080")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#621f33")
      (bg-green "#19402f")
      (bg-yellow "#50442f")
      (bg-blue "#3a386b")
      (bg-magenta "#5a2f40")
      (bg-cyan "#2f495f"))
  "Palette of `doric-valley' theme.")

  (doric-themes-define-theme doric-valley dark "Minimalist theme with dark background and indigo+gold hues"))

(provide 'doric-valley-theme)
;;; doric-valley-theme.el ends here
