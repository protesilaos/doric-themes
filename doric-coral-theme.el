;;; doric-coral-theme.el --- Minimalist theme with light background and red+cyan hues -*- lexical-binding:t -*-

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

  (defvar doric-coral-palette
    '((cursor "#205798")
      (bg-main "#fcf0e5")
      (fg-main "#40282e")
      (border "#c3a8bf")

      (bg-shadow-subtle "#efe4db")
      (fg-shadow-subtle "#8f5854")

      (bg-neutral "#e6d5d0")
      (fg-neutral "#514250")

      (bg-shadow-intense "#fcb894")
      (fg-shadow-intense "#a02016")

      (bg-accent "#c8f0e3")
      (fg-accent "#087078")

      (fg-red "#a02610")
      (fg-green "#006940")
      (fg-yellow "#753800")
      (fg-blue "#183182")
      (fg-magenta "#820145")
      (fg-cyan "#025763")

      (bg-red "#ffbca7")
      (bg-green "#b2efd8")
      (bg-yellow "#e6e294")
      (bg-blue "#baceef")
      (bg-magenta "#e2c1e0")
      (bg-cyan "#c0e6f9"))
  "Palette of `doric-coral' theme.")

  (doric-themes-define-theme doric-coral light "Minimalist theme with light background and red+cyan hues"))

(provide 'doric-coral-theme)
;;; doric-coral-theme.el ends here
