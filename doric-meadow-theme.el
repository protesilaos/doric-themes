;;; doric-meadow-theme.el --- Minimalist theme with light background and green+yellow hues -*- lexical-binding:t -*-

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

  (defvar doric-meadow-palette
    '((cursor "#705060")
      (bg-main "#eaf0c0")
      (fg-main "#023f2d")
      (border "#8f9373")

      (bg-shadow-subtle "#e0e2be")
      (fg-shadow-subtle "#60504a")

      (bg-neutral "#d0d2b0")
      (fg-neutral "#53402f")

      (bg-shadow-intense "#b8e0a7")
      (fg-shadow-intense "#206502")

      (bg-accent "#ecdb9f")
      (fg-accent "#753100")

      (fg-red "#982500")
      (fg-green "#226700")
      (fg-yellow "#595000")
      (fg-blue "#103077")
      (fg-magenta "#700054")
      (fg-cyan "#005460")

      (bg-red "#d0af90")
      (bg-green "#b3d39c")
      (bg-yellow "#d0c085")
      (bg-blue "#c0c2d0")
      (bg-magenta "#cdbad0")
      (bg-cyan "#bedad0"))
  "Palette of `doric-meadow' theme.")

  (doric-themes-define-theme doric-meadow light "Minimalist theme with light background and green+yellow hues"))

(provide 'doric-meadow-theme)
;;; doric-meadow-theme.el ends here
