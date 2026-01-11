;;; doric-mermaid-theme.el --- Minimalist theme with dark background and blue-grey hues -*- lexical-binding:t -*-

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

  (defvar doric-mermaid-palette
    '((cursor "#e08ca0")
      (bg-main "#2a2f48")
      (fg-main "#dde0f8")
      (border "#757a90")

      (bg-shadow-subtle "#3c3e55")
      (fg-shadow-subtle "#aea7c3")

      (bg-neutral "#4c4f67")
      (fg-neutral "#d8d0e6")

      (bg-shadow-intense "#424381")
      (fg-shadow-intense "#c4abfe")

      (bg-accent "#573f5d")
      (fg-accent "#e8add2")

      (fg-red "#eca28f")
      (fg-green "#a0d0ba")
      (fg-yellow "#c0b080")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#59323f")
      (bg-green "#304848")
      (bg-yellow "#50442f")
      (bg-blue "#323a6b")
      (bg-magenta "#5a2855")
      (bg-cyan "#2f495f"))
  "Palette of `doric-mermaid' theme.")

  (doric-themes-define-theme doric-mermaid dark "Minimalist theme with dark background and blue-grey hues"))

(provide 'doric-mermaid-theme)
;;; doric-mermaid-theme.el ends here
