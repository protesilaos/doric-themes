;;; doric-pine-theme.el --- Minimalist theme with dark background and woody+earthly hues -*- lexical-binding:t -*-

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

  (defvar doric-pine-palette
    '((cursor "#c0a27a")
      (bg-main "#303f2d")
      (fg-main "#d0e2c8")
      (border "#8c997f")

      (bg-shadow-subtle "#40503d")
      (fg-shadow-subtle "#bcbeaf")

      (bg-neutral "#575f4b")
      (fg-neutral "#cdd9be")

      (bg-shadow-intense "#70523a")
      (fg-shadow-intense "#c8b399")

      (bg-accent "#305d42")
      (fg-accent "#b0d593")

      (fg-red "#e8a28f")
      (fg-green "#a2d09a")
      (fg-yellow "#c4b980")
      (fg-blue "#98bae0")
      (fg-magenta "#e9acbf")
      (fg-cyan "#a0c0d0")

      (bg-red "#5b3c2b")
      (bg-green "#385b30")
      (bg-yellow "#595432")
      (bg-blue "#284060")
      (bg-magenta "#52313f")
      (bg-cyan "#2f495f"))
  "Palette of `doric-pine' theme.")

  (doric-themes-define-theme doric-pine dark "Minimalist theme with dark background and woody+earthly hues"))

(provide 'doric-pine-theme)
;;; doric-pine-theme.el ends here
