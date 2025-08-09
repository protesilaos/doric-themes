;;; doric-pine-theme.el --- Minimalist dark theme -*- lexical-binding:t -*-

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

  (defvar doric-pine-palette
    '((cursor "#c0a27a")
      (bg-main "#303f2d")
      (fg-main "#d0e2c8")
      (border "#8c997f")

      (bg-shadow-subtle "#40503d")
      (fg-shadow-subtle "#bcbeaf")

      (bg-neutral "#575f4b")
      (fg-neutral "#cdd9be")

      (bg-shadow-intense "#70523a")
      (fg-shadow-intense "#c8b399")

      (bg-accent "#305d42")
      (fg-accent "#b0d593")

      (fg-red "#d0a080")
      (fg-green "#90c090")
      (fg-yellow "#c0b080")
      (fg-blue "#90a0c0")
      (fg-magenta "#c0a0b0")
      (fg-cyan "#90b0c0")

      (bg-red "#4a2a2a")
      (bg-green "#2a4a2a")
      (bg-yellow "#4a4a2a")
      (bg-blue "#2a3a4a")
      (bg-magenta "#4a2a3a")
      (bg-cyan "#2a4a4a"))
  "Palette of `doric-pine' theme.")

  (doric-themes-define-theme doric-pine dark))

(provide 'doric-pine-theme)
;;; doric-pine-theme.el ends here
