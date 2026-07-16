;;; doric-borage-theme.el --- Minimalist theme with dark background and orange+green hues -*- lexical-binding:t -*-

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

  (defvar doric-borage-palette
    '((cursor "#b079ff")
      (bg-main "#3a2e3f")
      (fg-main "#eed0c7")
      (border "#6a647e")

      (bg-shadow-subtle "#42394c")
      (fg-shadow-subtle "#a690c9")

      (bg-neutral "#4c4760")
      (fg-neutral "#c8c4d7")

      (bg-shadow-intense "#425639")
      (fg-shadow-intense "#afc487")

      (bg-accent "#583d49")
      (fg-accent "#dfa487")

      (fg-red "#eca28f")
      (fg-green "#b9d0aa")
      (fg-yellow "#c0b060")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#613229")
      (bg-green "#325331")
      (bg-yellow "#514e2f")
      (bg-blue "#2a3f5d")
      (bg-magenta "#5a2f40")
      (bg-cyan "#2f4e4f"))
  "Palette of `doric-borage' theme.")

  (doric-themes-define-theme doric-borage dark "Minimalist theme with dark background and orange+green hues"))

(provide 'doric-borage-theme)
;;; doric-borage-theme.el ends here
