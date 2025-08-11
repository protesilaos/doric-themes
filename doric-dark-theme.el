;;; doric-dark-theme.el --- Minimalist theme with dark background and cool hues -*- lexical-binding:t -*-

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

  (defvar doric-dark-palette
    '((cursor "#ccaaee")
      (bg-main "#000000")
      (fg-main "#ffffff")
      (border "#707070")

      (bg-shadow-subtle "#332d38")
      (fg-shadow-subtle "#a2a0b2")

      (bg-neutral "#43404c")
      (fg-neutral "#c5c7d4")

      (bg-shadow-intense "#50447f")
      (fg-shadow-intense "#cfcff8")

      (bg-accent "#521e40")
      (fg-accent "#cda4df")

      (fg-red "#eca28f")
      (fg-green "#b9d0aa")
      (fg-yellow "#c0b080")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#4d1f20")
      (bg-green "#1f402e")
      (bg-yellow "#504432")
      (bg-blue "#223567")
      (bg-magenta "#603254")
      (bg-cyan "#2f4f54"))
  "Palette of `doric-dark' theme.")

  (doric-themes-define-theme doric-dark dark "Minimalist theme with dark background and cool hues"))

(provide 'doric-dark-theme)
;;; doric-dark-theme.el ends here
