;;; doric-marble-theme.el --- Minimalist theme with light grey background and rocky hues -*- lexical-binding:t -*-

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

  (defvar doric-marble-palette
    '((cursor "#403030")
      (bg-main "#ededed")
      (fg-main "#202020")
      (border "#9a9a9a")

      (bg-shadow-subtle "#dfdfdf")
      (fg-shadow-subtle "#595959")

      (bg-neutral "#cdcdcd")
      (fg-neutral "#4a4a4a")

      (bg-shadow-intense "#b0b0b0")
      (fg-shadow-intense "#404040")

      (bg-accent "#e5d7c5")
      (fg-accent "#603d3a")

      (fg-red "#a01010")
      (fg-green "#106710")
      (fg-yellow "#60400f")
      (fg-blue "#103077")
      (fg-magenta "#700d50")
      (fg-cyan "#005355")

      (bg-red "#eac0bf")
      (bg-green "#bde0c2")
      (bg-yellow "#f0f0b0")
      (bg-blue "#c4cfe8")
      (bg-magenta "#eec2e6")
      (bg-cyan "#c1ebe4"))
  "Palette of `doric-marble' theme.")

  (doric-themes-define-theme doric-marble light "Minimalist theme with light grey background and rocky hues"))

(provide 'doric-marble-theme)
;;; doric-marble-theme.el ends here
