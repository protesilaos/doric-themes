;;; doric-tiger-theme.el --- Minimalist theme with light background and warm colours -*- lexical-binding:t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

  (defvar doric-tiger-palette
    '((cursor "#000000")
      (bg-main "#f0deab")
      (fg-main "#402f3e")
      (border "#a29986")

      (bg-shadow-subtle "#dfce9f")
      (fg-shadow-subtle "#485027")

      (bg-neutral "#d2b78f")
      (fg-neutral "#5a483a")

      (bg-shadow-intense "#d69f60")
      (fg-shadow-intense "#90300f")

      (bg-accent "#dec780")
      (fg-accent "#7e4e0a")

      (fg-red "#a02600")
      (fg-green "#406900")
      (fg-yellow "#783400")
      (fg-blue "#183172")
      (fg-magenta "#820145")
      (fg-cyan "#025763")

      (bg-red "#ffbc87")
      (bg-green "#b2d078")
      (bg-yellow "#e6c264")
      (bg-blue "#aac0df")
      (bg-magenta "#d2c3d0")
      (bg-cyan "#b5e0c3"))
    "Palette of `doric-tiger' theme.")

  (doric-themes-define-theme doric-tiger light "Minimalist theme with light background and warm colours"))

(provide 'doric-tiger-theme)
;;; doric-tiger-theme.el ends here
