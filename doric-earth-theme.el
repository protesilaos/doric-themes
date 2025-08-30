;;; doric-earth-theme.el --- Minimalist theme with light background and brown hues -*- lexical-binding:t -*-

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

  (defvar doric-earth-palette
    '((cursor "#770000")
      (bg-main "#f0eddf")
      (fg-main "#30232e")
      (border "#a29986")

      (bg-shadow-subtle "#dfdfce")
      (fg-shadow-subtle "#635650")

      (bg-neutral "#d1ceb6")
      (fg-neutral "#504033")

      (bg-shadow-intense "#c09fa0")
      (fg-shadow-intense "#58383f")

      (bg-accent "#e7d5b9")
      (fg-accent "#74321f")

      (fg-red "#a03000")
      (fg-green "#206700")
      (fg-yellow "#705200")
      (fg-blue "#103060")
      (fg-magenta "#690f44")
      (fg-cyan "#105f66")

      (bg-red "#f0c8b5")
      (bg-green "#c2e3b0")
      (bg-yellow "#efe4b0")
      (bg-blue "#bac9e5")
      (bg-magenta "#e7bfd7")
      (bg-cyan "#c9e6e0"))
    "Palette of `doric-earth' theme.")

  (doric-themes-define-theme doric-earth light "Minimalist theme with light background and brown hues"))

(provide 'doric-earth-theme)
;;; doric-earth-theme.el ends here
