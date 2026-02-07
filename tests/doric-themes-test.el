;;; doric-themes-test.el --- Unit tests for the Doric themes -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026  Free Software Foundation, Inc.

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

(defvar dtt-face-bundles
  '(doric-themes-selection-faces
    doric-themes-intense-shadow-faces
    doric-themes-intense-shadow-foreground-only-faces
    doric-themes-subtle-shadow-faces
    doric-themes-subtle-shadow-foreground-only-faces
    doric-themes-accent-foreground-only-faces
    doric-themes-bold-accent-foreground-only-faces
    doric-themes-main-foreground-only-faces
    doric-themes-bold-faces
    doric-themes-bold-intense-faces
    doric-themes-bold-italic-faces
    doric-themes-italic-faces
    doric-themes-italic-only-faces
    doric-themes-underline-emphasis-faces
    doric-themes-underline-emphasis-subtle-faces
    doric-themes-underline-link-faces
    doric-themes-underline-subtle-shadow-faces
    doric-themes-diff-added-faces
    doric-themes-diff-added-highlight-faces
    doric-themes-diff-added-refine-faces
    doric-themes-diff-changed-faces
    doric-themes-diff-changed-highlight-faces
    doric-themes-diff-changed-refine-faces
    doric-themes-diff-removed-faces
    doric-themes-diff-removed-highlight-faces
    doric-themes-diff-removed-refine-faces
    doric-themes-error-foreground-only-faces
    doric-themes-error-background-faces
    doric-themes-warning-foreground-only-faces
    doric-themes-success-foreground-only-faces
    doric-themes-success-background-faces
    doric-themes-error-underline-faces
    doric-themes-warning-underline-faces
    doric-themes-success-underline-faces
    doric-themes-cite-odd-faces
    doric-themes-cite-even-faces
    doric-themes-mark-select-faces
    doric-themes-mark-delete-faces)
  "List of symbols with Doric themes variables for face bundles.")

(defun dtt-is-member-p (x y elements)
  "Return non-nil if X and Y are members of ELEMENTS."
  (and (memq x elements) (memq y elements)))

(ert-deftest dtt-distinct-faces ()
  (should-not
   (let ((matches nil))
     (dolist (x dtt-face-bundles)
       (dolist (y dtt-face-bundles)
         (unless (or (eq x y) (dtt-is-member-p x y (delete-dups (flatten-list (mapcar #'car matches)))))
           (when-let* ((match (seq-intersection (symbol-value x) (symbol-value y))))
             (push (cons (cons x y) match) matches)))))
     matches)))

(provide 'doric-themes-test)
;;; doric-themes-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dtt" . "doric-themes-test-"))
;; End:
