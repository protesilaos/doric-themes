;;; doric-themes-test.el --- Unit tests for the Doric themes -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the Doric themes.  Note that we are using Shorthands in
;; this file, so the "dtt-" prefix really is "doric-themes-test-".
;; Evaluate the following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)
(require 'doric-themes)

(defvar dtt-face-groups
  (let ((variables nil))
    (mapatoms
     (lambda (symbol)
       (when-let* ((_ (not (symbol-function symbol)))
                   (string (symbol-name symbol))
                   (_ (and (string-match-p "\\`doric-themes-.+faces" string)
                           (not (string-match-p "\\`doric-themes-test" string)))))
         (push symbol variables))))
    variables)
  "List of symbols with Doric themes variables for face groups.")

(defun dtt-is-member-p (x y elements)
  "Return non-nil if X and Y are members of ELEMENTS."
  (and (memq x elements) (memq y elements)))

(ert-deftest dtt-distinct-faces ()
  (should-not
   (let ((matches nil))
     (dolist (x dtt-face-groups)
       (dolist (y dtt-face-groups)
         (unless (or (eq x y) (dtt-is-member-p x y (delete-dups (flatten-list (mapcar #'car matches)))))
           (when-let* ((match (seq-intersection (symbol-value x) (symbol-value y))))
             (push (cons (cons x y) match) matches)))))
     matches)))

(provide 'doric-themes-test)
;;; doric-themes-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dtt" . "doric-themes-test-"))
;; End:
