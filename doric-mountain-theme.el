;;; doric-mountain-theme.el --- Minimalist theme with dark background and earthly tones -*- lexical-binding:t -*-

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

  (defvar doric-mountain-palette
    '((cursor "#b7b0cb")
      (bg-main "#383025")
      (fg-main "#e5daa0")
      (border "#7e7479")

      (bg-shadow-subtle "#514b44")
      (fg-shadow-subtle "#c8b9ad")

      (bg-neutral "#61574e")
      (fg-neutral "#d2d0ba")

      (bg-shadow-intense "#6c5a49")
      (fg-shadow-intense "#f0d080")

      (bg-accent "#4c553a")
      (fg-accent "#bdd487")

      (fg-red "#e0a47f")
      (fg-green "#a0c080")
      (fg-yellow "#b9b56f")
      (fg-blue "#a0b0eb")
      (fg-magenta "#d2a2c0")
      (fg-cyan "#9bc3c3")

      (bg-red "#79473f")
      (bg-green "#4b6441")
      (bg-yellow "#685730")
      (bg-blue "#3a4f6f")
      (bg-magenta "#673f55")
      (bg-cyan "#3f5b62"))
    "Palette of `doric-mountain' theme.")

  (doric-themes-define-theme doric-mountain dark "Minimalist theme with dark background and earthly tones"))

(provide 'doric-mountain-theme)
;;; doric-mountain-theme.el ends here
