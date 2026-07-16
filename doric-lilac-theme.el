;;; doric-lilac-theme.el --- Minimalist theme with light background and green+purple hues -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026  Free Software Foundation, Inc.

;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
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

  (defvar doric-lilac-palette
    '((cursor "#a07f50")
      (bg-main "#f2f0e7")
      (fg-main "#1a3530")
      (border "#8f9373")

      (bg-shadow-subtle "#e7e2d7")
      (fg-shadow-subtle "#6e522a")

      (bg-neutral "#d5cbc7")
      (fg-neutral "#53402f")

      (bg-shadow-intense "#c7c0e4")
      (fg-shadow-intense "#5b4295")

      (bg-accent "#cfe4c7")
      (fg-accent "#435a00")

      (fg-red "#982500")
      (fg-green "#226700")
      (fg-yellow "#595000")
      (fg-blue "#103077")
      (fg-magenta "#700054")
      (fg-cyan "#005460")

      (bg-red "#e3b8a0")
      (bg-green "#b8caa0")
      (bg-yellow "#dfc085")
      (bg-blue "#c4c8dd")
      (bg-magenta "#d8bade")
      (bg-cyan "#bee0db"))
  "Palette of `doric-lilac' theme.")

  (doric-themes-define-theme doric-lilac light "Minimalist theme with light background and green+purple hues"))

(provide 'doric-lilac-theme)
;;; doric-lilac-theme.el ends here
