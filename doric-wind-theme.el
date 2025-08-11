;;; doric-wind-theme.el --- Minimalist theme with light background and green+teal hues -*- lexical-binding:t -*-

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

  (defvar doric-wind-palette
    '((cursor "#10600b")
      (bg-main "#e8f0e9")
      (fg-main "#000f0f")
      (border "#98a09a")

      (bg-shadow-subtle "#d5e1d9")
      (fg-shadow-subtle "#565962")

      (bg-neutral "#c2ced0")
      (fg-neutral "#404248")

      (bg-shadow-intense "#94c2bf")
      (fg-shadow-intense "#104038")

      (bg-accent "#c7e2c1")
      (fg-accent "#2f6920")

      (fg-red "#990020")
      (fg-green "#006710")
      (fg-yellow "#706000")
      (fg-blue "#003370")
      (fg-magenta "#782050")
      (fg-cyan "#006070")

      (bg-red "#e4c2c7")
      (bg-green "#b0e0d0")
      (bg-yellow "#e9e6b0")
      (bg-blue "#bbd1e8")
      (bg-magenta "#e0c3e7")
      (bg-cyan "#c2ebe8"))
    "Palette of `doric-wind' theme.")

  (doric-themes-define-theme doric-wind light "Minimalist theme with light background and green+teal hues"))

(provide 'doric-wind-theme)
;;; doric-wind-theme.el ends here
