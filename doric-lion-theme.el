;;; doric-lion-theme.el --- Minimalist theme with dark background and warm colours -*- lexical-binding:t -*-

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

  (defvar doric-lion-palette
    '((cursor "#f3cf6f")
      (bg-main "#392716")
      (fg-main "#f8d48c")
      (border "#725841")

      (bg-shadow-subtle "#403023")
      (fg-shadow-subtle "#afaf92")

      (bg-neutral "#4b3c2a")
      (fg-neutral "#d0c09e")

      (bg-shadow-intense "#604410")
      (fg-shadow-intense "#d09b39")

      (bg-accent "#4e2c20")
      (fg-accent "#e08746")

      (fg-red "#ec825f")
      (fg-green "#89b75a")
      (fg-yellow "#d5ba50")
      (fg-blue "#7fbad0")
      (fg-magenta "#e98cbf")
      (fg-cyan "#68c2ce")

      (bg-red "#692b1f")
      (bg-green "#3f4e1f")
      (bg-yellow "#694720")
      (bg-blue "#364267")
      (bg-magenta "#623540")
      (bg-cyan "#2f4e5f"))
  "Palette of `doric-lion' theme.")

  (doric-themes-define-theme doric-lion dark "Minimalist theme with dark background and warm colours"))

(provide 'doric-lion-theme)
;;; doric-lion-theme.el ends here
