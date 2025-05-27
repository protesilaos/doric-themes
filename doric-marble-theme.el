;;; doric-marble-theme.el --- Minimalist light theme -*- lexical-binding:t -*-

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
      (fg-neutral "#505050")

      (bg-shadow-intense "#b0b0b0")
      (fg-shadow-intense "#404040")

      (bg-accent "#e5d7c5")
      (fg-accent "#603d3a")

      (fg-faint-red "#750000")
      (fg-faint-green "#056100")
      (fg-faint-yellow "#5f4602")
      (fg-faint-blue "#353362")
      (fg-faint-magenta "#553372")
      (fg-faint-cyan "#35485e"))
  "Palette of `doric-marble' theme.")

  (doric-themes-define-theme doric-marble light))

(provide 'doric-marble-theme)
;;; doric-marble-theme.el ends here
