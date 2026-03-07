;;; doric-walnut-theme.el --- Minimalist theme with dark background and green+brown hues -*- lexical-binding:t -*-

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

  (defvar doric-walnut-palette
    '((cursor "#d3b18e")
      (bg-main "#153227")
      (fg-main "#b9d3b0")
      (border "#596c5e")

      (bg-shadow-subtle "#303931")
      (fg-shadow-subtle "#a09f9b")

      (bg-neutral "#3d4543")
      (fg-neutral "#afc2be")

      (bg-shadow-intense "#0c512c")
      (fg-shadow-intense "#63c789")

      (bg-accent "#42342f")
      (fg-accent "#aea084")

      (fg-red "#e8a27f")
      (fg-green "#82cc7f")
      (fg-yellow "#c4b980")
      (fg-blue "#8fbae5")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#5b3d2c")
      (bg-green "#395c2d")
      (bg-yellow "#595432")
      (bg-blue "#284060")
      (bg-magenta "#52313f")
      (bg-cyan "#2f495f"))
    "Palette of `doric-walnut' theme.")

  (doric-themes-define-theme doric-walnut dark "Minimalist theme with dark background and green+brown hues"))

(provide 'doric-walnut-theme)
;;; doric-walnut-theme.el ends here
