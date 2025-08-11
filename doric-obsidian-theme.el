;;; doric-obsidian-theme.el --- Minimalist theme with dark grey background and rocky hues -*- lexical-binding:t -*-

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

  (defvar doric-obsidian-palette
    '((cursor "#eeddbb")
      (bg-main "#181818")
      (fg-main "#e7e7e7")
      (border "#727272")

      (bg-shadow-subtle "#2f2f2f")
      (fg-shadow-subtle "#969696")

      (bg-neutral "#3a3a3a")
      (fg-neutral "#bcbcbc")

      (bg-shadow-intense "#505050")
      (fg-shadow-intense "#b0b0b0")

      (bg-accent "#432f2a")
      (fg-accent "#b59487")

      (fg-red "#eca28f")
      (fg-green "#b9d0aa")
      (fg-yellow "#c0b080")
      (fg-blue "#9fbfe7")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#4d1f20")
      (bg-green "#1f402e")
      (bg-yellow "#504432")
      (bg-blue "#223567")
      (bg-magenta "#603254")
      (bg-cyan "#2f4f54"))
  "Palette of `doric-obsidian' theme.")

  (doric-themes-define-theme doric-obsidian dark "Minimalist theme with dark grey background and rocky hues"))

(provide 'doric-obsidian-theme)
;;; doric-obsidian-theme.el ends here
