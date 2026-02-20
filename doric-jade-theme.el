;;; doric-jade-theme.el --- Minimalist theme with light background and green hues -*- lexical-binding:t -*-

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

  (defvar doric-jade-palette
    '((cursor "#00a093")
      (bg-main "#e2f0c9")
      (fg-main "#25311f")
      (border "#98a09a")

      (bg-shadow-subtle "#d5e1c0")
      (fg-shadow-subtle "#6b5a77")

      (bg-neutral "#c2ceb9")
      (fg-neutral "#605258")

      (bg-shadow-intense "#74d98f")
      (fg-shadow-intense "#006000")

      (bg-accent "#cfe2d4")
      (fg-accent "#106870")

      (fg-red "#990020")
      (fg-green "#006710")
      (fg-yellow "#706000")
      (fg-blue "#003370")
      (fg-magenta "#782050")
      (fg-cyan "#006070")

      (bg-red "#e4c297")
      (bg-green "#aed2a8")
      (bg-yellow "#e0dd90")
      (bg-blue "#b9cce0")
      (bg-magenta "#ddc5da")
      (bg-cyan "#a4dfdc"))
    "Palette of `doric-jade' theme.")

  (doric-themes-define-theme doric-jade light "Minimalist theme with light background and green hues"))

(provide 'doric-jade-theme)
;;; doric-jade-theme.el ends here
