;;; doric-water-theme.el --- Minimalist dark theme -*- lexical-binding:t -*-

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

  (defvar doric-water-palette
    '((cursor "#99ddee")
      (bg-main "#2a283d")
      (fg-main "#edf0f8")
      (border "#8d8c9e")

      (bg-shadow-subtle "#3a3c4c")
      (fg-shadow-subtle "#aea6b8")

      (bg-neutral "#4a4a5f")
      (fg-neutral "##c3c4c8")

      (bg-shadow-intense "#496278")
      (fg-shadow-intense "#c0ddf2")

      (bg-accent "#403f75")
      (fg-accent "#adade0")

      (fg-faint-red "#dba2a2")
      (fg-faint-green "#85c397")
      (fg-faint-yellow "#c4a992")
      (fg-faint-blue "#95afd2")
      (fg-faint-magenta "#c5a3b2")
      (fg-faint-cyan "#a5bfce"))
  "Palette of `doric-water' theme.")

  (doric-themes-define-theme doric-water dark))

(provide 'doric-water-theme)
;;; doric-water-theme.el ends here
