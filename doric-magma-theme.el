;;; doric-magma-theme.el --- Minimalist theme with dark background and red hues -*- lexical-binding:t -*-

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

  (defvar doric-magma-palette
    '((cursor "#ef3839")
      (bg-main "#351b10")
      (fg-main "#e0baa0")
      (border "#706061")

      (bg-shadow-subtle "#48332b")
      (fg-shadow-subtle "#a0a196")

      (bg-neutral "#5f4743")
      (fg-neutral "#cfbbb0")

      (bg-shadow-intense "#7c301c")
      (fg-shadow-intense "#ff9d7e")

      (bg-accent "#5a2914")
      (fg-accent "#f65f47")

      (fg-red "#fc826f")
      (fg-green "#b9c06a")
      (fg-yellow "#cfa030")
      (fg-blue "#7fafc7")
      (fg-magenta "#df70af")
      (fg-cyan "#70a0c0")

      (bg-red "#5f240f")
      (bg-green "#3f440f")
      (bg-yellow "#554600")
      (bg-blue "#3f2457")
      (bg-magenta "#5a2f40")
      (bg-cyan "#2f4954"))
  "Palette of `doric-magma' theme.")

  (doric-themes-define-theme doric-magma dark "Minimalist theme with dark background and red hues"))

(provide 'doric-magma-theme)
;;; doric-magma-theme.el ends here
