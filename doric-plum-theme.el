;;; doric-plum-theme.el --- Minimalist theme with dark background and pink+purple hues -*- lexical-binding:t -*-

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

  (defvar doric-plum-palette
    '((cursor "#c070d0")
      (bg-main "#221832")
      (fg-main "#e2d7e7")
      (border "#6a647e")

      (bg-shadow-subtle "#302f3c")
      (fg-shadow-subtle "#a694b1")

      (bg-neutral "#423b53")
      (fg-neutral "#c6c1d3")

      (bg-shadow-intense "#5e4170")
      (fg-shadow-intense "#cea6d0")

      (bg-accent "#501e3e")
      (fg-accent "#c586ba")

      (fg-red "#eca28f")
      (fg-green "#b9d0aa")
      (fg-yellow "#c0b080")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#602038")
      (bg-green "#124438")
      (bg-yellow "#50442f")
      (bg-blue "#3a386b")
      (bg-magenta "#5a2f40")
      (bg-cyan "#2f495f"))
  "Palette of `doric-plum' theme.")

  (doric-themes-define-theme doric-plum dark "Minimalist theme with dark background and pink+purple hues"))

(provide 'doric-plum-theme)
;;; doric-plum-theme.el ends here
