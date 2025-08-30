;;; doric-water-theme.el --- Minimalist theme with dark background and blue+teal hues -*- lexical-binding:t -*-

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

  (defvar doric-water-palette
    '((cursor "#99ddee")
      (bg-main "#2a283d")
      (fg-main "#edf0f8")
      (border "#8d8c9e")

      (bg-shadow-subtle "#3a3c4c")
      (fg-shadow-subtle "#aea6b8")

      (bg-neutral "#4a4a5f")
      (fg-neutral "#d4d9dc")

      (bg-shadow-intense "#496278")
      (fg-shadow-intense "#c0ddf2")

      (bg-accent "#403f75")
      (fg-accent "#adade0")

      (fg-red "#eca28f")
      (fg-green "#a0d0ba")
      (fg-yellow "#c0b080")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#602638")
      (bg-green "#104c3a")
      (bg-yellow "#50442f")
      (bg-blue "#323a6b")
      (bg-magenta "#5a2855")
      (bg-cyan "#2f495f"))
  "Palette of `doric-water' theme.")

  (doric-themes-define-theme doric-water dark "Minimalist theme with dark background and blue+teal hues"))

(provide 'doric-water-theme)
;;; doric-water-theme.el ends here
