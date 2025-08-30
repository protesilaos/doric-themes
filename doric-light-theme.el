;;; doric-light-theme.el --- Minimalist theme with light background and cool hues -*- lexical-binding:t -*-

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

  (defvar doric-light-palette
    '((cursor "#2266bb")
      (bg-main "#ffffff")
      (fg-main "#000000")
      (border "#b0b0b0")

      (bg-shadow-subtle "#efeff2")
      (fg-shadow-subtle "#5a6268")

      (bg-neutral "#dbdce1")
      (fg-neutral "#424d53")

      (bg-shadow-intense "#a0bcd0")
      (fg-shadow-intense "#213067")

      (bg-accent "#d8f1f3")
      (fg-accent "#084092")

      (fg-red "#a01010")
      (fg-green "#106710")
      (fg-yellow "#60400f")
      (fg-blue "#103077")
      (fg-magenta "#700d50")
      (fg-cyan "#005355")

      (bg-red "#f0c4c4")
      (bg-green "#c0e8c2")
      (bg-yellow "#f0f0b0")
      (bg-blue "#c4cfe8")
      (bg-magenta "#eec2e6")
      (bg-cyan "#c1ebe4"))
  "Palette of `doric-light' theme.")

  (doric-themes-define-theme doric-light light "Minimalist theme with light background and cool hues"))

(provide 'doric-light-theme)
;;; doric-light-theme.el ends here
