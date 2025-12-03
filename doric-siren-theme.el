;;; doric-siren-theme.el --- Minimalist theme with light blue-grey background and marine hues -*- lexical-binding:t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

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

  (defvar doric-siren-palette
    '((cursor "#2040a0")
      (bg-main "#d9e0ed")
      (fg-main "#001020")
      (border "#9a9aba")

      (bg-shadow-subtle "#c2d1df")
      (fg-shadow-subtle "#3f5569")

      (bg-neutral "#b0c0cd")
      (fg-neutral "#3a434a")

      (bg-shadow-intense "#90b0e0")
      (fg-shadow-intense "#00498b")

      (bg-accent "#dfc1d2")
      (fg-accent "#68203b")

      (fg-red "#a01010")
      (fg-green "#106710")
      (fg-yellow "#60400f")
      (fg-blue "#103077")
      (fg-magenta "#700d50")
      (fg-cyan "#005355")

      (bg-red "#e0c0c4")
      (bg-green "#aadecb")
      (bg-yellow "#e2e0ac")
      (bg-blue "#bcd0ef")
      (bg-magenta "#dfc2e9")
      (bg-cyan "#b2d7e0"))
  "Palette of `doric-siren' theme.")

  (doric-themes-define-theme doric-siren light "Minimalist theme with light blue-grey background and marine hues"))

(provide 'doric-siren-theme)
;;; doric-siren-theme.el ends here
