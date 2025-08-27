;;; doric-beach-theme.el --- Minimalist theme with light background and gold+teal hues -*- lexical-binding:t -*-

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

  (defvar doric-beach-palette
    '((cursor "#3f8cab")
      (bg-main "#eae3d8")
      (fg-main "#213937")
      (border "#b4afb0")

      (bg-shadow-subtle "#ded4c0")
      (fg-shadow-subtle "#605d48")

      (bg-neutral "#d4c2ab")
      (fg-neutral "#504533")

      (bg-shadow-intense "#abd2c9")
      (fg-shadow-intense "#045e53")

      (bg-accent "#efd0a9")
      (fg-accent "#834328")

      (fg-red "#902000")
      (fg-green "#006000")
      (fg-yellow "#595000")
      (fg-blue "#103077")
      (fg-magenta "#700054")
      (fg-cyan "#005460")

      (bg-red "#e0baa5")
      (bg-green "#bfcba8")
      (bg-yellow "#d5cc98")
      (bg-blue "#c4c8dd")
      (bg-magenta "#d8bade")
      (bg-cyan "#bee0db"))
  "Palette of `doric-beach' theme.")

  (doric-themes-define-theme doric-beach light "Minimalist theme with light background and gold+teal hues"))

(provide 'doric-beach-theme)
;;; doric-beach-theme.el ends here
