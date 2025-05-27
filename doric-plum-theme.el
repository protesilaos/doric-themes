;;; doric-plum-theme.el --- Minimalist dark theme -*- lexical-binding:t -*-

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

  (defvar doric-plum-palette
    '((cursor "#c070d0")
      (bg-main "#221832")
      (fg-main "#e2d7e7")
      (border "#6a647e")

      (bg-shadow-subtle "#302f3c")
      (fg-shadow-subtle "#a694b1")

      (bg-neutral "#423b53")
      (fg-neutral "#b3b1be")

      (bg-shadow-intense "#5e4170")
      (fg-shadow-intense "#cea6d0")

      (bg-accent "#501e3e")
      (fg-accent "#c586ba")

      (fg-faint-red "#dba2a2")
      (fg-faint-green "#85c397")
      (fg-faint-yellow "#c4a992")
      (fg-faint-blue "#95afd2")
      (fg-faint-magenta "#c5a3b2")
      (fg-faint-cyan "#a5bfce"))
  "Palette of `doric-plum' theme.")

  (doric-themes-define-theme doric-plum dark))

(provide 'doric-plum-theme)
;;; doric-plum-theme.el ends here
