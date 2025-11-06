;;; doric-themes.el --- Highly legible minimalist themes with precise typography -*- lexical-binding:t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/doric-themes
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1"))
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

(require 'seq)
(require 'color)
(eval-when-compile (require 'subr-x))

(defconst doric-themes-light-themes
  '(doric-beach
    doric-cherry
    doric-earth
    doric-light
    doric-marble
    doric-oak
    doric-wind)
  "Light themes.")

(defconst doric-themes-dark-themes
  '(doric-dark
    doric-fire
    doric-obsidian
    doric-pine
    doric-plum
    doric-valley
    doric-water)
  "Dark themes.")

(defconst doric-themes-collection
  (append doric-themes-light-themes doric-themes-dark-themes)
  "Symbols of all the Doric themes.")

(defgroup doric-themes nil
  "Highly legible minimalist themes with precise typography."
  :group 'faces
  :link '(url-link :tag "Sample pictures" "https://protesilaos.com/emacs/doric-themes-pictures"))

;;;; User options

(defcustom doric-themes-to-toggle '(doric-light doric-dark)
  "Specify two themes for the `doric-themes-toggle' command.
The variable `doric-themes-collection' contains the symbols of all
themes that form part of this collection."
  :type (let ((themes (mapcar
                       (lambda (theme)
                         (list 'const theme))
                       doric-themes-collection)))
          `(choice
            (const :tag "No toggle" nil)
            (list :tag "Pick two themes to toggle between"
                  (choice :tag "Theme one of two" ,@themes)
                  (choice :tag "Theme two of two" ,@themes))))
  :package-version '(doric-themes . "0.1.0")
  :group 'doric-themes)

(defcustom doric-themes-to-rotate doric-themes-collection
  "List of themes to rotate among when using the command `doric-themes-rotate'."
  :type `(repeat (choice
                  :tag "A theme among the `doric-themes-collection'"
                  ,@(mapcar (lambda (theme) (list 'const theme)) doric-themes-collection)))
  :package-version '(doric-themes . "0.1.0")
  :group 'doric-themes)

(defvaralias 'doric-themes-post-load-hook 'doric-themes-after-load-theme-hook
  "Alias for `doric-themes-after-load-theme-hook'.")

(defcustom doric-themes-after-load-theme-hook nil
  "Hook that runs after loading a Doric theme.
This is used by the commands `doric-themes-toggle',
`doric-themes-rotate', `doric-themes-load-random',
`doric-themes-select', as well as the function
`doric-themes-load-theme'."
  :type 'hook
  :package-version '(doric-themes . "0.1.0")
  :group 'doric-themes)

;;;; Commands and their helper functions

(defun doric-themes--doric-p (theme)
  "Return non-nil if THEME name has a doric- prefix."
  (string-prefix-p "doric-" (symbol-name theme)))

(defun doric-themes--list-enabled-themes ()
  "Return list of `custom-enabled-themes' matching `doric-themes--doric-p'."
  (seq-filter #'doric-themes--doric-p custom-enabled-themes))

(defun doric-themes--enable-themes ()
  "Enable the Doric themes."
  (dolist (theme doric-themes-collection)
    (unless (memq theme custom-known-themes)
      (load-theme theme :no-confirm :no-enable))))

(defun doric-themes--list-known-themes ()
  "Return list of `custom-known-themes' matching `doric-themes--doric-p'."
  (doric-themes--enable-themes)
  (seq-filter #'doric-themes--doric-p custom-known-themes))

(defun doric-themes--current-theme ()
  "Return first enabled Doric theme."
  (car (or (doric-themes--list-enabled-themes)
           (doric-themes--list-known-themes))))

(defun doric-themes--annotate-theme (theme)
  "Return completion annotation for THEME."
  (when-let* ((symbol (intern-soft theme))
              (doc-string (get symbol 'theme-documentation)))
    (format " -- %s" (propertize (car (split-string doc-string "\\.")) 'face 'completions-annotations))))

(defun doric-themes--completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

(defun doric-themes--completion-table-candidates ()
  "Render `doric-themes--list-known-themes' as completion with theme category."
  (doric-themes--completion-table 'theme (doric-themes--list-known-themes)))

(defvar doric-themes-select-theme-history nil
  "Minibuffer history of `doric-themes-select-prompt'.")

(defun doric-themes-select-prompt (&optional prompt)
  "Minibuffer prompt to select a Doric theme.
With optional PROMPT string, use it.  Else use a generic prompt."
  (let ((completion-extra-properties `(:annotation-function ,#'doric-themes--annotate-theme)))
    (intern
     (completing-read
      (or prompt "Select Doric theme: ")
      (doric-themes--completion-table-candidates)
      nil t nil 'doric-themes-select-theme-history))))

(defun doric-themes-load-theme (theme)
  "Load THEME while disabling other themes and return THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm)
  (run-hooks 'doric-themes-after-load-theme-hook)
  theme)

;;;###autoload
(defun doric-themes-select (theme)
  "Load a Doric THEME using minibuffer completion.
Run `doric-themes-after-load-theme-hook' after loading the theme."
  (interactive (list (doric-themes-select-prompt)))
  (doric-themes-load-theme theme))

(defun doric-themes--toggle-theme-p ()
  "Return non-nil if `doric-themes-to-toggle' are valid."
  (condition-case nil
      (dolist (theme doric-themes-to-toggle)
        (or (memq theme doric-themes-collection)
            (memq theme (doric-themes--list-known-themes))
            (error "`%s' is not part of `doric-themes-collection'" theme)))
    (error nil)
    (:success doric-themes-to-toggle)))

;;;###autoload
(defun doric-themes-toggle ()
  "Toggle between the two `doric-themes-to-toggle'.
If `doric-themes-to-toggle' does not specify two Doric themes, inform
the user about it while prompting with completion for a theme among our
collection (this is practically the same as the `doric-themes-select'
command).

Run `doric-themes-after-load-theme-hook' after loading the theme."
  (interactive)
  (if (doric-themes--toggle-theme-p)
      (pcase-let ((`(,one ,two) doric-themes-to-toggle))
        (if (eq (car custom-enabled-themes) one)
            (doric-themes-load-theme two)
          (doric-themes-load-theme one)))
    (doric-themes-load-theme
     (doric-themes-select-prompt
      (concat "Set two `doric-themes-to-toggle'; "
              "switching to theme selection for now: ")))))

(defun doric-themes--rotate (themes)
  "Rotate THEMES rightward such that the car is moved to the end."
  (if (proper-list-p themes)
      (let* ((index (seq-position themes (doric-themes--current-theme)))
             (offset (1+ index)))
        (append (nthcdr offset themes) (take offset themes)))
    (error "The `%s' is not a list" themes)))

(defun doric-themes--rotate-p (themes)
  "Return a new theme among THEMES if it is possible to rotate to it."
  (if-let* ((new-theme (car (doric-themes--rotate themes))))
      (if (eq new-theme (doric-themes--current-theme))
          (car (doric-themes--rotate-p (doric-themes--rotate themes)))
        new-theme)
    (error "Cannot determine a theme among `%s'" themes)))

;;;###autoload
(defun doric-themes-rotate (themes)
  "Rotate to the next theme among THEMES.
When called interactively THEMES is the value of `doric-themes-to-rotate'.

If the current theme is already the next in line, then move to the one
after.  Perform the rotation rightwards, such that the first element in
the list becomes the last.  Do not modify THEMES in the process.

Run `doric-themes-after-load-theme-hook' after loading a theme."
  (interactive (list doric-themes-to-rotate))
  (unless (proper-list-p themes)
    "This is not a list of themes: `%s'" themes)
  (let ((candidate (doric-themes--rotate-p themes)))
    (if (doric-themes--doric-p candidate)
        (progn
          (message "Rotating to `%s'" (propertize (symbol-name candidate) 'face 'bold))
          (doric-themes-load-theme candidate))
      (user-error "`%s' is not part of the Doric collection" candidate))))

(defun doric-themes--minus-current (&optional variant)
  "Return list of Doric themes minus the current one.
Optional VARIANT limits the list of themes to either the dark or light
subset.  VARIANT is either `light' or `dark', which correspond to
`doric-themes-light-themes' and `doric-themes-dark-themes',
respectively."
  (let* ((sequence (or
                    (cond
                     ((eq variant 'dark)
                      doric-themes-dark-themes)
                     ((eq variant 'light)
                      doric-themes-light-themes))
                    (doric-themes--list-known-themes)))
         (themes (copy-sequence sequence)))
    (if-let* ((current-theme (doric-themes--current-theme)))
        (delete current-theme themes)
      themes)))

(make-obsolete-variable 'doric-themes-subset-history nil "0.2.0")

(defun doric-themes-subset-prompt ()
  "Select `dark' or `light' and return it as a symbol."
  (intern
   (cadr
    (read-multiple-choice
     "Variant"
     '((?d "dark" "Load a random dark theme")
       (?l "light" "Load a random light theme"))
     "Limit to the dark or light subset of the Doric themes collection."))))

;;;###autoload
(defun doric-themes-load-random (&optional variant)
  "Load a Doric theme at random, excluding the current one.

With optional VARIANT as a prefix argument, prompt to limit the set of
themes to either dark or light variants.  When called from Lisp, VARIANT
is either the `dark' or `light' symbol.

Run `doric-themes-after-load-theme-hook' after loading a theme."
  (interactive
   (list
    (when current-prefix-arg
      (doric-themes-subset-prompt))))
  (let* ((themes (doric-themes--minus-current variant))
         (match (or (nth (random (length themes)) themes) (car themes))))
    (doric-themes-load-theme match)
    (message "Match `%s'" (propertize (symbol-name match) 'face 'bold))))

;;;; Face customisations

(defconst doric-themes-selection-faces
  '(avy-goto-char-timer-face
    avy-lead-face
    avy-lead-face-0
    avy-lead-face-1
    avy-lead-face-2
    completions-highlight
    consult-highlight-mark
    consult-highlight-match
    consult-preview-insertion
    header-line-highlight
    highlight
    hl-line
    icomplete-selected-match
    ido-first-match
    magit-diff-file-heading-selection
    markdown-highlighting-face
    mode-line-highlight
    next-error
    org-dispatcher-highlight
    proced-marked
    pulse-highlight-start-face
    rectangle-preview
    speedbar-highlight-face
    tab-bar-tab-highlight
    tab-line-highlight
    transient-enabled-suffix
    vertico-current))

(defconst doric-themes-intense-shadow-faces
  '(blink-matching-paren-offscreen
    company-template-field
    company-tooltip-selection
    company-tooltip-scrollbar-thumb
    corfu-current
    eww-form-file
    eww-form-submit
    gnus-summary-cancelled
    magit-blame-highlight
    magit-diff-lines-boundary
    region
    show-paren-match
    speedbar-separator-face
    substitute-match))

(defconst doric-themes-intense-shadow-foreground-only-faces
  '(calendar-weekday-header
    change-log-date
    denote-faces-date
    denote-faces-day
    denote-faces-hour
    denote-faces-minute
    denote-faces-month
    denote-faces-second
    denote-faces-time
    denote-faces-year
    diredfl-date-time
    display-time-date-and-time
    ediff-current-diff-Ancestor
    elfeed-search-date-face
    epa-field-body
    epa-field-name
    eshell-ls-readonly
    font-lock-function-name-face
    haskell-constructor-face
    mm-uu-extract
    magit-log-author
    magit-log-date
    marginalia-date
    message-header-cc
    message-header-other
    notmuch-search-date
    org-agenda-calendar-daterange
    org-agenda-column-dateline
    org-date
    org-sexp-date
    proced-time-colon
    rcirc-timestamp
    vc-state-base
    vc-up-to-date-state
    ztreep-diff-header-small-face))

(defconst doric-themes-subtle-shadow-faces
  '(company-preview
    company-tooltip-scrollbar-track
    consult-preview-line
    corfu-popupinfo
    edebug-enabled-breakpoint
    ediff-even-diff-A
    ediff-even-diff-Ancestor
    ediff-even-diff-B
    ediff-even-diff-C
    ediff-odd-diff-A
    ediff-odd-diff-Ancestor
    ediff-odd-diff-B
    ediff-odd-diff-C
    eww-form-checkbox
    eww-form-select
    eww-form-textarea
    eww-form-text
    header-line
    magit-blame-heading
    magit-blame-margin
    match
    menu
    message-separator
    mu4e-region-code
    org-agenda-clocking
    org-agenda-diary
    org-agenda-restriction-lock
    org-clock-overlay
    secondary-selection
    show-paren-match-expression
    tab-bar
    tab-line
    transient-disabled-suffix
    trashed-restored
    tool-bar
    vc-dir-status-ignored
    widget-documentation
    widget-field
    widget-inactive
    widget-single-line-field
    widget-unselected
    xref-match))

(defconst doric-themes-subtle-shadow-foreground-only-faces
  '(all-the-icons-blue
    all-the-icons-blue-alt
    all-the-icons-completion-dir-face
    all-the-icons-cyan
    all-the-icons-cyan-alt
    all-the-icons-dblue
    all-the-icons-dcyan
    all-the-icons-dgreen
    all-the-icons-dired-dir-face
    all-the-icons-dmaroon
    all-the-icons-dorange
    all-the-icons-dpink
    all-the-icons-dpurple
    all-the-icons-dred
    all-the-icons-dsilver
    all-the-icons-dyellow
    all-the-icons-green
    all-the-icons-ibuffer-dir-face
    all-the-icons-ibuffer-file-face
    all-the-icons-ibuffer-mode-face
    all-the-icons-ibuffer-size-face
    all-the-icons-lblue
    all-the-icons-lcyan
    all-the-icons-lgreen
    all-the-icons-lmaroon
    all-the-icons-lorange
    all-the-icons-lpink
    all-the-icons-lpurple
    all-the-icons-lred
    all-the-icons-lsilver
    all-the-icons-lyellow
    all-the-icons-maroon
    all-the-icons-orange
    all-the-icons-pink
    all-the-icons-purple
    all-the-icons-purple-alt
    all-the-icons-red
    all-the-icons-red-alt
    all-the-icons-silver
    all-the-icons-yellow
    avy-background-face
    aw-background-face
    breadcrumb-face
    calendar-weekend-header
    change-log-email
    compilation-column-number
    compilation-line-number
    consult-grep-context
    consult-help
    consult-line-number
    consult-line-number-prefix
    consult-line-number-wrapped
    consult-narrow-indicator
    corfu-deprecated
    custom-documentation
    denote-faces-delimiter
    denote-faces-extension
    denote-faces-time-delimiter
    diff-context
    dired-ignored
    diredfl-compressed-file-suffix
    diredfl-dir-priv
    diredfl-file-suffix
    diredfl-ignored-file-name
    diredfl-link-priv
    diredfl-no-priv
    diredfl-other-priv
    diredfl-rare-priv
    diredfl-read-priv
    diredfl-tagged-autofile-name
    diredfl-write-priv
    edebug-disabled-breakpoint
    elfeed-search-title-face
    epa-validity-disabled
    eshell-ls-unreadable
    file-name-shadow
    font-latex-sedate-face
    font-latex-string-face
    font-latex-verbatim-face
    font-lock-string-face
    gnus-header-name
    gnus-splash
    gnus-summary-high-ancient
    gnus-summary-high-read
    gnus-summary-low-ancient
    gnus-summary-low-read
    gnus-summary-normal-ancient
    gnus-summary-normal-read
    hexl-ascii-region
    icomplete-vertical-unselected-prefix-indicator-face
    line-number
    magit-diff-context
    magit-log-graph
    marginalia-documentation
    marginalia-file-name
    marginalia-file-priv-no
    marginalia-file-priv-other
    marginalia-file-priv-rare
    marginalia-file-priv-read
    marginalia-file-priv-write
    marginalia-function
    marginalia-installed
    marginalia-lighter
    marginalia-list
    marginalia-mode
    marginalia-modified
    marginalia-null
    marginalia-number
    marginalia-off
    marginalia-on
    marginalia-size
    marginalia-string
    marginalia-symbol
    marginalia-true
    marginalia-type
    marginalia-value
    marginalia-version
    message-header-newsgroups
    message-header-xheader
    mu4e-header-face
    nerd-icons-blue
    nerd-icons-blue-alt
    nerd-icons-completion-dir-face
    nerd-icons-cyan
    nerd-icons-cyan-alt
    nerd-icons-dblue
    nerd-icons-dcyan
    nerd-icons-dgreen
    nerd-icons-dired-dir-face
    nerd-icons-dmaroon
    nerd-icons-dorange
    nerd-icons-dpink
    nerd-icons-dpurple
    nerd-icons-dred
    nerd-icons-dsilver
    nerd-icons-dyellow
    nerd-icons-green
    nerd-icons-ibuffer-dir-face
    nerd-icons-ibuffer-file-face
    nerd-icons-ibuffer-mode-face
    nerd-icons-ibuffer-size-face
    nerd-icons-lblue
    nerd-icons-lcyan
    nerd-icons-lgreen
    nerd-icons-lmaroon
    nerd-icons-lorange
    nerd-icons-lpink
    nerd-icons-lpurple
    nerd-icons-lred
    nerd-icons-lsilver
    nerd-icons-lyellow
    nerd-icons-maroon
    nerd-icons-orange
    nerd-icons-pink
    nerd-icons-purple
    nerd-icons-purple-alt
    nerd-icons-red
    nerd-icons-red-alt
    nerd-icons-silver
    nerd-icons-yellow
    notmuch-crypto-part-header
    notmuch-search-count
    notmuch-search-non-matching-authors
    notmuch-tag-face
    notmuch-tree-match-tag-face
    notmuch-tree-no-match-date-face
    notmuch-tree-no-match-face
    org-agenda-dimmed-todo-face
    org-agenda-done
    org-column
    org-done
    org-headline-done
    org-special-keyword
    org-tag
    org-time-grid
    org-upcoming-deadline
    org-upcoming-distant-deadline
    package-status-available
    package-status-built-in
    package-status-dependency
    package-status-external
    package-status-from-source
    package-status-new
    proced-executable
    proced-interruptible-sleep-status-code
    proced-mem
    shadow
    tab-bar-tab-group-inactive
    tab-bar-tab-ungrouped
    transient-inactive-argument
    transient-inactive-value
    transient-unreachable
    transient-unreachable-key
    vc-ignored-state
    vertico-multiline
    window-divider
    window-divider-first-pixel
    window-divider-last-pixel
    xref-line-number
    ztreep-expand-sign-face))

(defconst doric-themes-accent-foreground-only-faces
  '(change-log-acknowledgment
    dired-directory
    diredfl-dir-name
    diredfl-exec-priv
    diredfl-executable-tag
    elfeed-search-feed-face
    epa-validity-high
    escape-glyph
    eshell-ls-executable
    eshell-ls-special
    font-latex-math-face
    font-latex-script-char-face
    gnus-server-agent
    gnus-server-cloud-host
    hexl-address-region
    homoglyph
    ido-subdir
    log-view-message
    magit-hash
    marginalia-file-priv-exec
    notmuch-search-matching-authors
    notmuch-tree-match-author-face
    notmuch-search-flagged-face
    org-headline-todo
    org-scheduled-previously
    org-table-row
    org-todo
    org-warning
    package-status-installed
    proced-pgrp
    proced-pid
    proced-ppid
    proced-sess
    speedbar-directory-face
    tab-line-close-highlight
    transient-value
    which-key-command-description-face
    widget-button
    widget-button-pressed
    woman-addition
    ztreep-node-face))

(defconst doric-themes-bold-accent-foreground-only-faces
  '(diary
    magit-diff-file-heading
    org-imminent-deadline))

(defconst doric-themes-main-foreground-only-faces
  '(border
    breadcrumb-imenu-crumbs-face
    breadcrumb-project-base-face
    breadcrumb-project-crumbs-face
    c-annotation-face
    change-log-function
    child-frame-border
    consult-bookmark
    consult-buffer
    consult-file
    denote-faces-prompt-current-name
    denote-faces-title
    dictionary-word-definition-face
    dired-mark
    dired-perm-write
    dired-set-id
    dired-special
    diredfl-autofile-name
    diredfl-compressed-file-name
    diredfl-file-name
    diredfl-number
    epa-mark
    epa-validity-low
    epa-validity-medium
    eww-valid-certificate
    font-lock-bracket-face
    font-lock-constant-face
    font-lock-delimiter-face
    font-lock-misc-punctuation-face
    font-lock-negation-char-face
    font-lock-number-face
    font-lock-punctuation-face
    gnus-server-opened
    gnus-summary-high-undownloaded
    gnus-summary-high-unread
    gnus-summary-low-undownloaded
    gnus-summary-low-unread
    gnus-summary-normal-undownloaded
    gnus-summary-normal-unread
    gnus-summary-selected
    icomplete-vertical-selected-prefix-indicator-face
    ido-only-match
    icon
    kmacro-menu-flagged
    kmacro-menu-mark
    kmacro-menu-marked
    log-edit-unknown-header
    log-view-commit-body
    magit-cherry-equivalent
    magit-bisect-bad
    magit-bisect-good
    magit-bisect-skip
    magit-reflog-amend
    magit-reflog-checkout
    magit-reflog-cherry-pick
    magit-reflog-commit
    magit-reflog-merge
    magit-reflog-other
    magit-reflog-rebase
    magit-reflog-remote
    magit-reflog-reset
    magit-sequence-done
    magit-sequence-drop
    magit-sequence-exec
    magit-sequence-head
    magit-sequence-onto
    magit-sequence-part
    magit-sequence-pick
    magit-sequence-stop
    marginalia-archive
    marginalia-char
    marginalia-file-owner
    message-signature-separator
    minibuffer-depth-indicator
    mm-command-output
    mouse
    mouse-drag-and-drop-region
    next-error-message
    nobreak-hyphen
    nobreak-space
    notmuch-tag-unread
    notmuch-tag-flagged
    org-agenda-current-time
    org-agenda-filter-category
    org-agenda-filter-effort
    org-agenda-filter-regexp
    org-agenda-filter-tags
    org-archived
    org-default
    org-document-info
    org-mode-line-clock
    org-scheduled
    org-scheduled-today
    proced-cpu
    proced-mark
    proced-memory-low-usage
    proced-memory-medium-usage
    proced-user
    sgml-namespace
    shr-abbreviation
    shr-sliced-image
    shr-strike-through
    shr-sup
    shr-text
    shortdoc-section
    so-long-mode-line-inactive
    speedbar-file-face
    tabulated-list-fake-header
    vc-dir-directory
    vc-dir-file
    vc-dir-header-value
    vc-dir-mark-indicator
    vc-dir-status-up-to-date
    vtable
    which-key-highlighted-command-face
    which-key-note-face which-key-separator-face
    ztreep-leaf-face))

(defconst doric-themes-bold-faces
  '(abbrev-table-name
    bookmark-face
    bookmark-menu-bookmark
    breadcrumb-imenu-leaf-face
    breadcrumb-project-leaf-face
    buffer-menu-buffer
    calendar-month-header
    change-log-name
    change-log-file
    circe-prompt-face
    comint-highlight-prompt
    company-tooltip-quick-access
    company-tooltip-quick-access-selection
    compilation-info
    compilation-mode-line-exit
    compilation-mode-line-fail
    compilation-mode-line-run
    compilation-warning
    consult-async-failed
    consult-async-finished
    consult-async-running
    consult-async-split
    css-property
    custom-face-tag
    custom-group-subtitle
    custom-group-tag
    custom-group-tag-1
    custom-variable-button
    custom-variable-obsolete
    custom-variable-tag
    denote-faces-keywords
    denote-faces-signature
    denote-faces-subdirectory
    dictionary-button-face
    diff-nonexistent
    ediff-fine-diff-Ancestor
    edmacro-label
    elfeed-log-debug-level-face
    elfeed-log-error-level-face
    elfeed-log-info-level-face
    elfeed-log-warn-level-face
    erc-prompt-face
    eshell-ls-archive
    eshell-ls-backup
    eshell-ls-clutter
    eshell-ls-directory
    eshell-ls-missing
    eshell-ls-product
    eshell-prompt
    font-latex-bold-face
    font-latex-sectioning-0-face
    font-latex-sectioning-1-face
    font-latex-sectioning-2-face
    font-latex-sectioning-3-face
    font-latex-sectioning-4-face
    font-latex-sectioning-5-face
    font-latex-slide-title-face
    font-lock-keyword-face
    font-lock-operator-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    geiser-font-lock-repl-prompt
    gnus-emphasis-bold
    gnus-header-content
    gnus-header-from
    gnus-header-newsgroups
    gnus-header-subject
    gnus-group-mail-1
    gnus-group-mail-2
    gnus-group-mail-3
    gnus-group-mail-low
    gnus-group-news-1
    gnus-group-news-2
    gnus-group-news-3
    gnus-group-news-4
    gnus-group-news-5
    gnus-group-news-6
    gnus-group-news-low
    gnus-server-cloud
    gnus-summary-high-ticked
    gnus-summary-low-ticked
    gnus-summary-normal-ticked
    grep-heading
    help-for-help-header
    icomplete-first-match
    indium-repl-prompt-face
    info-header-node
    info-index-match
    info-menu-header
    info-menu-star
    info-title-1
    info-title-2
    info-title-3
    info-title-4
    keycast-command
    log-edit-summary
    magit-branch-local
    magit-branch-remote
    magit-branch-remote-head
    magit-branch-upstream
    magit-mode-line-process
    magit-process-ok
    magit-signature-good
    magit-tag
    Man-overstrike
    markdown-header-face-1
    markdown-header-face-2
    markdown-header-face-3
    markdown-header-face-4
    markdown-header-face-5
    markdown-header-face-6
    message-header-subject
    message-header-to
    minibuffer-prompt
    mode-line-buffer-id
    mode-line-emphasis
    org-agenda-date
    org-checkbox-statistics-done
    org-checkbox-statistics-todo
    org-document-title
    org-level-1
    org-level-2
    org-level-3
    org-level-4
    org-level-5
    org-level-6
    org-level-7
    org-level-8
    org-list-dt
    org-table-header
    org-tag-group
    org-target
    outline-1
    outline-2
    outline-3
    outline-4
    outline-5
    outline-6
    outline-7
    outline-8
    proced-emacs-pid
    proced-sort-header
    rcirc-prompt
    rcirc-other-nick
    reb-regexp-grouping-backslash
    reb-regexp-grouping-construct
    sh-escaped-newline
    sh-quoted-exec
    shortdoc-heading
    shr-h1
    shr-h2
    shr-h3
    shr-h4
    shr-h5
    shr-h6
    slime-repl-prompt-face
    sly-mrepl-prompt-face
    speedbar-button-face
    so-long-mode-line-active
    telega-chat-prompt
    texinfo-heading
    transient-heading
    transient-mismatched-key
    transient-nonstandard-key
    trashed-directory
    vc-conflict-state
    vc-dir-header
    vc-dir-status-warning
    vc-locked-state
    vc-missing-state
    vc-needs-update-state
    vc-removed-state
    which-func
    woman-bold
    world-clock-label
    xref-file-header
    ztreep-diff-header-face
    ztreep-header-face))

(defconst doric-themes-bold-intense-faces
  '(dired-header
    diredfl-dir-heading
    elfeed-search-unread-title-face
    git-commit-comment-heading
    git-commit-summary
    line-number-current-line
    log-edit-header
    magit-section-heading
    markdown-metadata-key-face
    message-header-name
    notmuch-crypto-decryption
    notmuch-crypto-signature-bad
    notmuch-crypto-signature-good
    notmuch-crypto-signature-good-key
    notmuch-crypto-signature-unknown
    org-agenda-structure
    package-help-section-name))

(defconst doric-themes-bold-italic-faces
  '(appt-notification
    aw-key-face
    change-log-conditionals
    change-log-list
    comint-highlight-input
    compilation-error
    completions-group-title
    confusingly-reordered
    consult-imenu-prefix
    consult-key
    css-proprietary-property
    css-selector
    custom-changed
    custom-invalid
    dictionary-word-entry-face
    diff-error
    diff-file-header
    dired-warning
    elfeed-search-filter-face
    eww-invalid-certificate
    font-lock-builtin-face
    font-lock-preprocessor-face
    font-lock-type-face
    git-commit-comment-action
    git-commit-comment-branch-remote
    git-commit-comment-branch-local
    gnus-emphasis-bold-italic
    gnus-server-denied
    ibuffer-locked-buffer
    ido-indicator
    info-header-xref
    line-number-major-tick
    magit-branch-warning
    magit-process-ng
    magit-signature-bad
    marginalia-file-priv-dir
    marginalia-key
    message-mml
    org-mode-line-clock-overrun
    package-status-avail-obso
    package-status-disabled
    package-status-held
    package-status-incompat
    package-status-unsigned
    proced-memory-high-usage
    proced-run-status-code
    proced-uninterruptible-sleep-status-code
    rcirc-bright-nick
    rcirc-track-keyword
    smerge-markers
    tab-line-tab-modified
    transient-key
    transient-key-exit
    transient-key-recurse
    transient-key-return
    transient-key-stack
    transient-key-stay
    vertico-group-title))

(defconst doric-themes-italic-faces
  '(Info-quoted
    aw-minibuffer-leading-char-face
    completions-annotations
    company-tooltip-annotation
    company-tooltip-annotation-selection
    corfu-annotations
    custom-comment
    custom-comment-tag
    custom-modified
    custom-rogue
    custom-saved
    custom-set
    custom-state
    custom-themed
    diary-anniversary
    diary-time
    diff-function
    diff-index
    elfeed-search-tag-face
    elisp-shorthand-font-lock-face
    epa-string
    font-latex-doctex-documentation-face
    font-latex-doctex-preprocessor-face
    font-latex-italic-face
    font-lock-doc-face
    font-lock-doc-markup-face
    git-commit-comment-file
    git-commit-nonempty-second-line
    git-commit-overlong-summary
    gnus-cite-attribution
    gnus-emphasis-italic
    gnus-group-mail-1-empty
    gnus-group-mail-2-empty
    gnus-group-mail-3-empty
    gnus-group-mail-low-empty
    gnus-group-news-1-empty
    gnus-group-news-2-empty
    gnus-group-news-3-empty
    gnus-group-news-4-empty
    gnus-group-news-5-empty
    gnus-group-news-6-empty
    gnus-group-news-low-empty
    gnus-server-offline
    help-argument-name
    holiday
    line-number-minor-tick
    magit-branch-current
    magit-cherry-unmatched
    magit-signature-error
    magit-signature-expired
    magit-signature-expired-key
    magit-signature-revoked
    magit-signature-untrusted
    magit-mode-line-process-error
    markdown-blockquote-face
    markdown-inline-code-face
    notmuch-wash-cited-text
    org-agenda-calendar-event
    org-agenda-calendar-sexp
    org-agenda-structure-secondary
    org-inline-src-block
    org-latex-and-related
    org-macro
    org-priority
    package-description
    rcirc-dim-nick
    sh-heredoc
    shr-code
    speedbar-tag-face
    tab-line-tab-special
    transient-key-noop
    vc-dir-status-edited
    vc-git-log-edit-summary-max-warning
    vc-git-log-edit-summary-target-warning
    which-key-docstring-face
    which-key-group-description-face
    which-key-local-map-description-face
    woman-italic
    ztreep-node-count-children-face))

(defconst doric-themes-underline-emphasis-faces
  '(company-echo-common
    company-preview-common
    company-preview-search
    company-tooltip-common
    company-tooltip-common-selection
    completions-common-part
    completions-first-difference
    consult-preview-match
    custom-visibility
    eldoc-highlight-function-argument
    font-latex-underline-face
    gnus-emphasis-highlight-words
    lazy-highlight
    Man-underline
    notmuch-tag-added
    orderless-match-face-0
    orderless-match-face-1
    orderless-match-face-2
    orderless-match-face-3
    proced-session-leader-pid
    rcirc-my-nick
    rcirc-nick-in-message
    rcirc-nick-in-message-full-line
    rcirc-track-nick
    show-paren-mismatch
    speedbar-selected-face
    transient-argument
    which-key-special-key-face))

(defconst doric-themes-underline-link-faces
  '(browse-url-button
    button
    custom-link
    denote-faces-link
    denote-faces-query-link
    dictionary-reference-face
    dired-symlink
    diredfl-symlink
    eshell-ls-symlink
    info-node
    info-xref
    info-xref-visited
    link
    link-visited
    marginalia-file-priv-link
    org-cite
    org-cite-key
    org-footnote
    org-link
    package-name
    rcirc-url
    shr-link))

(defconst doric-themes-diff-added-faces
  '(denote-faces-prompt-new-name
    diff-added
    diff-indicator-added
    magit-diff-added
    magit-diffstat-added
    smerge-lower
    ztreep-diff-model-add-face))

(defconst doric-themes-diff-added-highlight-faces
  '(diff-hl-insert
    diff-hl-dired-insert
    ediff-current-diff-B
    magit-diff-added-highlight))

(defconst doric-themes-diff-changed-highlight-faces
  '(diff-hl-change
    diff-hl-dired-change
    ediff-current-diff-C
    magit-diff-base-highlight))

(defconst doric-themes-diff-removed-highlight-faces
  '(diff-hl-delete
    diff-hl-dired-delete
    ediff-current-diff-A
    magit-diff-removed-highlight))

(defconst doric-themes-diff-added-refine-faces
  '(diff-refine-added
    ediff-fine-diff-B
    smerge-refined-added))

(defconst doric-themes-diff-changed-faces
  '(diff-changed
    diff-changed-unspecified
    diff-indicator-changed
    magit-diff-base
    smerge-base))

(defconst doric-themes-diff-changed-refine-faces
  '(diff-refine-changed
    ediff-fine-diff-C
    smerge-refined-changed))

(defconst doric-themes-diff-removed-faces
  '(denote-faces-prompt-old-name
    diff-removed
    diff-indicator-removed
    magit-diff-removed
    magit-diffstat-removed
    smerge-upper
    ztreep-diff-model-diff-face))

(defconst doric-themes-diff-removed-refine-faces
  '(diff-refine-removed
    ediff-fine-diff-A
    smerge-refined-removed))

(defconst doric-themes-error-foreground-only-faces
  '(TeX-error-description-error
    dired-broken-symlink
    error
    ert-test-result-unexpected))

(defconst doric-themes-warning-foreground-only-faces
  '(TeX-error-description-warning
    font-latex-warning-face
    font-lock-escape-facex
    font-lock-warning-face
    warning))

(defconst doric-themes-success-foreground-only-faces
  '(TeX-error-description-help
    TeX-error-description-tex-said
    ert-test-result-expected
    success))

(defconst doric-themes-error-underline-faces
  '(flycheck-error
    flyspell-incorrect
    flymake-error
    writegood-duplicates-face))

(defconst doric-themes-warning-underline-faces
  '(flycheck-warning
    flyspell-duplicate
    flymake-warning
    jinx-misspelled
    writegood-passive-voice-face
    writegood-weasels-face))

(defconst doric-themes-success-underline-faces
  '(flycheck-info
    flymake-note))

(defconst doric-themes-cite-odd
  '(gnus-cite-1
    gnus-cite-3
    gnus-cite-5
    gnus-cite-7
    gnus-cite-9
    gnus-cite-11
    message-cited-text-1
    message-cited-text-3
    mu4e-cited-1-face
    mu4e-cited-3-face
    mu4e-cited-5-face
    mu4e-cited-7-face))

(defconst doric-themes-cite-even
  '(gnus-cite-2
    gnus-cite-4
    gnus-cite-6
    gnus-cite-8
    gnus-cite-10
    message-cited-text-2
    message-cited-text-4
    mu4e-cited-2-face
    mu4e-cited-4-face
    mu4e-cited-6-face))

(defgroup doric-themes-faces ()
  "Faces defined by the Doric themes."
  :group 'doric-themes
  :link '(url-link :tag "Sample pictures" "https://protesilaos.com/emacs/doric-themes-pictures")
  :prefix "doric-themes-"
  :tag "Doric themes Faces")

(dolist (scope '(note warning error))
  (custom-declare-face
   (intern (format "doric-themes-prominent-%s" scope))
   nil (format "Prominent notification of type %s." scope)
   :package-version '(doric-themes . "0.4.0")
   :version "30.1"
   :group 'doric-themes-faces))

(defun doric-themes-prepare-faces (&rest faces-and-attributes)
  "Set faces to their respective attributes in FACES-AND-ATTRIBUTES."
  (pcase-let ((`(,faces . ,attributes) faces-and-attributes))
    (mapcar
     (lambda (face)
       (backquote (list ',face (list (list t ,@attributes)))))
     faces)))

(defun doric-themes-adjust-value (hex-rgb percentage)
  "Adjust value of HEX-RGB colour by PERCENTAGE."
  (pcase-let* ((`(,r ,g ,b) (color-name-to-rgb hex-rgb))
               (fn (if (color-dark-p (list r g b))
                       #'color-lighten-name
                     #'color-darken-name)))
    (funcall fn hex-rgb percentage)))

;;;###autoload
(defmacro doric-themes-define-theme (name background-mode &optional description)
  "Define theme with NAME and `light' or `dark' BACKGROUND-MODE.
With optional DESCRIPTION, use it to describe the theme, otherwise
default to a generic text that mentions the BACKGROUND-MODE."
  (declare (indent 0))
  (unless (memq background-mode '(light dark))
    (error "The BACKGROUND-MODE must be either `light' or `dark'"))
  (if-let* ((palette (symbol-value (intern (format "%s-palette" name)))))
      (let ((theme-exists-p (custom-theme-p name)))
        `(progn
           ,@(unless theme-exists-p
               (list `(custom-declare-theme
                       ',name 'doric-themes
                       ,(or description (format "Minimalist %s theme." background-mode))
                       (list :kind 'color-scheme :background-mode ',background-mode :family 'doric))))
           (let ,palette
             (custom-theme-set-faces
              ',name
              `(default ((t :background ,bg-main :foreground ,fg-main)))
              `(fringe ((t :background unspecified :foreground ,fg-accent)))
              `(cursor ((t :background ,cursor)))

              '(bold ((t :weight bold)))
              '(italic ((t :slant italic)))
              '(bold-italic ((t :weight bold :slant italic)))
              '(underline ((t :underline t)))
              `(internal-border ((t :foreground ,border)))
              `(vertical-border ((t :foreground ,border)))
              `(separator-line ((t :underline ,border)))
              `(scroll-bar ((t :background ,bg-main :foreground ,border)))
              `(fill-column-indicator ((t :foreground ,bg-shadow-intense)))
              `(minibuffer-nonselected
                ((((supports :strike-through t)) :strike-through t)
                 (t :inverse-video t)))
              `(tooltip ((t :background ,bg-accent :foreground ,fg-accent)))
              `(tty-menu-disabled-face ((t :background ,bg-accent :foreground ,fg-shadow-subtle)))
              `(tty-menu-enabled-face ((t :background ,bg-accent :foreground ,fg-main)))
              `(tty-menu-selected-face ((t :background ,fg-main :foreground ,bg-main)))
              `(read-multiple-choice-face ((t :inherit bold-italic :background ,fg-shadow-intense :foreground ,bg-main)))

              '(adoc-meta-face ((t :inherit fixed-pitch)))
              '(adoc-meta-hide-face ((t :inherit fixed-pitch)))
              '(adoc-secondary-text-face ((t :inherit fixed-pitch)))
              '(adoc-table-face ((t :inherit fixed-pitch)))

              `(ansi-color-bright-black ((t :background "gray30" :foreground "gray30")))
              `(ansi-color-black ((t :background "black" :foreground "black")))
              `(ansi-color-bright-white ((t :background "white" :foreground "white")))
              `(ansi-color-white ((t :background "gray70" :foreground "gray70")))
              `(ansi-color-bright-red ((t :background ,fg-red :foreground ,fg-red)))
              `(ansi-color-red ((t :background ,fg-red :foreground ,fg-red)))
              `(ansi-color-bright-green ((t :background ,fg-green :foreground ,fg-green)))
              `(ansi-color-green ((t :background ,fg-green :foreground ,fg-green)))
              `(ansi-color-bright-yellow ((t :background ,fg-yellow :foreground ,fg-yellow)))
              `(ansi-color-yellow ((t :background ,fg-yellow :foreground ,fg-yellow)))
              `(ansi-color-bright-blue ((t :background ,fg-blue :foreground ,fg-blue)))
              `(ansi-color-blue ((t :background ,fg-blue :foreground ,fg-blue)))
              `(ansi-color-bright-magenta ((t :background ,fg-magenta :foreground ,fg-magenta)))
              `(ansi-color-magenta ((t :background ,fg-magenta :foreground ,fg-magenta)))
              `(ansi-color-bright-cyan ((t :background ,fg-cyan :foreground ,fg-cyan)))
              `(ansi-color-cyan ((t :background ,fg-cyan :foreground ,fg-cyan)))

              `(aw-leading-char-face ((t :inherit bold-italic :height 1.5 :foreground ,fg-accent)))

              `(calendar-today
                ((default :foreground ,fg-accent :inverse-video t)
                 (((supports :box t))
                  :box (:line-width (-1 . -1) :color ,fg-main))))

              `(company-tooltip ((t :inherit fixed-pitch :background ,bg-shadow-subtle :foreground ,fg-shadow-subtle)))

              `(corfu-default ((t :inherit fixed-pitch :background ,bg-shadow-subtle :foreground ,fg-shadow-subtle)))

              `(custom-button
                ((default :inherit variable-pitch :background ,bg-shadow-intense :foreground ,fg-shadow-intense)
                 (((supports :box t))
                  :box (:line-width 1 :color ,border :style released-button))
                 (t :underline ,border)))
              `(custom-button-mouse
                ((default :inherit variable-pitch :background ,bg-accent :foreground ,fg-accent)
                 (((supports :box t))
                  :box (:line-width 1 :color ,border :style released-button))
                 (t :underline ,border)))
              `(custom-button-pressed
                ((default :inherit variable-pitch :background ,bg-main :foreground ,fg-accent)
                 (((supports :box t))
                  :box (:line-width 1 :color ,border :style pressed-button))
                 (t :underline ,border)))

              '(devdocs-code-block ((t :inherit fixed-pitch)))

              '(diff-header (( )))
              `(diff-hunk-header ((t :inherit bold :background ,bg-shadow-subtle)))
              `(diff-function ((t :background ,bg-shadow-subtle)))

              `(dired-marked ((t :inherit bold-italic :background ,bg-accent :foreground ,fg-main)))
              `(dired-flagged ((t :inherit bold-italic :background ,bg-shadow-intense :foreground ,fg-main)))

              `(diredfl-deletion ((t :inherit dired-mark)))
              `(diredfl-deletion-file-name ((t :inherit dired-flagged)))
              `(diredfl-flag-mark ((t :inherit dired-mark)))
              `(diredfl-flag-mark-line ((t :inherit dired-marked)))

              ,@(doric-themes-prepare-faces doric-themes-intense-shadow-faces :background 'bg-shadow-intense :foreground 'fg-shadow-intense)
              ,@(doric-themes-prepare-faces doric-themes-subtle-shadow-faces :background 'bg-shadow-subtle :foreground 'fg-shadow-subtle)
              ,@(doric-themes-prepare-faces doric-themes-intense-shadow-foreground-only-faces :foreground 'fg-shadow-intense)
              ,@(doric-themes-prepare-faces doric-themes-subtle-shadow-foreground-only-faces :foreground 'fg-shadow-subtle)
              ,@(doric-themes-prepare-faces doric-themes-accent-foreground-only-faces :foreground 'fg-accent)
              ,@(doric-themes-prepare-faces doric-themes-bold-accent-foreground-only-faces :inherit ''bold  :foreground 'fg-accent)
              ,@(doric-themes-prepare-faces doric-themes-main-foreground-only-faces :foreground 'fg-main)

              ,@(doric-themes-prepare-faces doric-themes-error-foreground-only-faces :inherit ''bold :foreground 'fg-red)
              ,@(doric-themes-prepare-faces doric-themes-warning-foreground-only-faces :inherit ''bold :foreground 'fg-yellow)
              ,@(doric-themes-prepare-faces doric-themes-success-foreground-only-faces :inherit ''bold :foreground 'fg-green)
              ,@(doric-themes-prepare-faces doric-themes-error-underline-faces :underline '(list :style 'wave :color fg-red))
              ,@(doric-themes-prepare-faces doric-themes-warning-underline-faces :underline '(list :style 'wave :color fg-yellow))
              ,@(doric-themes-prepare-faces doric-themes-success-underline-faces :underline '(list :style 'wave :color fg-cyan))

              ,@(doric-themes-prepare-faces doric-themes-bold-faces :inherit ''bold :foreground 'fg-shadow-intense)
              ,@(doric-themes-prepare-faces doric-themes-bold-intense-faces :inherit ''bold :foreground 'fg-main)
              ,@(doric-themes-prepare-faces doric-themes-bold-italic-faces :inherit ''bold-italic :foreground 'fg-shadow-subtle)
              ,@(doric-themes-prepare-faces doric-themes-italic-faces :inherit ''italic :foreground 'fg-shadow-subtle)
              ,@(doric-themes-prepare-faces doric-themes-underline-link-faces :inherit ''underline :foreground 'fg-accent)
              ,@(doric-themes-prepare-faces doric-themes-underline-emphasis-faces :inherit ''(underline italic) :foreground 'fg-shadow-subtle)

              ,@(doric-themes-prepare-faces doric-themes-selection-faces :background 'bg-accent)

              ,@(doric-themes-prepare-faces doric-themes-diff-added-faces :background '(doric-themes-adjust-value bg-green -10) :foreground 'fg-neutral)
              ,@(doric-themes-prepare-faces doric-themes-diff-added-highlight-faces :background 'bg-green :foreground 'fg-green)
              ,@(doric-themes-prepare-faces doric-themes-diff-added-refine-faces :inherit ''bold :background '(doric-themes-adjust-value bg-green 10))
              ,@(doric-themes-prepare-faces doric-themes-diff-changed-faces :background '(doric-themes-adjust-value bg-yellow -10) :foreground 'fg-neutral)
              ,@(doric-themes-prepare-faces doric-themes-diff-changed-highlight-faces :background 'bg-yellow :foreground 'fg-yellow)
              ,@(doric-themes-prepare-faces doric-themes-diff-changed-refine-faces :inherit ''bold :background '(doric-themes-adjust-value bg-yellow 10))
              ,@(doric-themes-prepare-faces doric-themes-diff-removed-faces :background '(doric-themes-adjust-value bg-red -10) :foreground 'fg-neutral)
              ,@(doric-themes-prepare-faces doric-themes-diff-removed-highlight-faces :background 'bg-red :foreground 'fg-red)
              ,@(doric-themes-prepare-faces doric-themes-diff-removed-refine-faces :inherit ''bold :background '(doric-themes-adjust-value bg-red 10))

              ,@(doric-themes-prepare-faces doric-themes-cite-odd :inherit ''italic :foreground 'fg-accent)
              ,@(doric-themes-prepare-faces doric-themes-cite-even :inherit ''italic :foreground 'fg-shadow-subtle)

              `(doric-themes-prominent-error ((t :background ,bg-red :foreground ,fg-red)))
              `(doric-themes-prominent-warning ((t :background ,bg-yellow :foreground ,fg-yellow)))
              `(doric-themes-prominent-note ((t :background ,bg-cyan :foreground ,fg-cyan)))

              '(embark-keybinding ((t :inherit (fixed-pitch bold-italic))))

              `(font-lock-comment-delimiter-face ((t :inherit italic :foreground ,fg-accent)))
              `(font-lock-comment-face ((t :inherit italic :foreground ,fg-accent)))
              `(font-lock-variable-name-face  ((t :inherit italic)))

              ;; The :inverse-video prevents hl-line-mode from
              ;; overriding the background.  Such an override really
              ;; defeats the purpose of setting those highlights.
              '(hi-aquamarine
                ((default :inverse-video t)
                 (((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#227f8f")
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#56abcc")))
              '(hi-black-b ((t :inverse-video t)))
              `(hi-black-hb ((t :background ,bg-shadow-subtle :foreground ,fg-shadow-subtle :inverse-video t)))
              '(hi-blue
                ((default :inverse-video t)
                 (((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#4360bd")
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#9abcef")))
              '(hi-blue-b ((t :inherit (bold hi-blue))))
              '(hi-green
                ((default :inverse-video t)
                 (((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#407820")
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#76bd46")))
              '(hi-green-b ((t :inherit (bold hi-green))))
              '(hi-pink
                ((default :inverse-video t)
                 (((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#ad507a")
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#ef92bf")))
              '(hi-red-b
                ((default :inverse-video t)
                 (((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#990000")
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#dd6060")))
              '(hi-salmon
                ((default :inverse-video t)
                 (((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#9f654a")
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#e0aa80")))
              '(hi-yellow
                ((default :inverse-video t)
                 (((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#806f00")
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#cab000")))

              `(isearch ((t :background ,bg-shadow-intense :foreground ,fg-main)))
              `(isearch-fail ((t :inherit (underline bold))))
              `(isearch-group-1 ((t :background ,bg-accent :foreground ,fg-accent)))
              `(isearch-group-2 ((t :background ,bg-shadow-intense :foreground ,fg-shadow-intense)))
              `(query-replace ((t :inherit isearch)))

              '(help-key-binding ((t :inherit (fixed-pitch bold-italic))))

              `(keycast-key ((t :inherit bold-italic :background ,fg-shadow-intense :foreground ,bg-main)))

              `(lin-blue ((t :background ,bg-blue)))
              `(lin-cyan ((t :background ,bg-cyan)))
              `(lin-green ((t :background ,bg-green)))
              `(lin-magenta ((t :background ,bg-magenta)))
              `(lin-red ((t :background ,bg-red)))
              `(lin-yellow ((t :background ,bg-yellow)))
              `(lin-blue-override-fg ((t :background ,bg-blue :foreground ,fg-main)))
              `(lin-cyan-override-fg ((t :background ,bg-cyan :foreground ,fg-main)))
              `(lin-green-override-fg ((t :background ,bg-green :foreground ,fg-main)))
              `(lin-magenta-override-fg ((t :background ,bg-magenta :foreground ,fg-main)))
              `(lin-red-override-fg ((t :background ,bg-red :foreground ,fg-main)))
              `(lin-yellow-override-fg ((t :background ,bg-yellow :foreground ,fg-main)))

              `(magit-diff-context-highlight ((t :background ,bg-shadow-subtle :foreground ,fg-shadow-subtle)))
              `(magit-diff-file-heading-highlight ((t :inherit magit-diff-file-heading :background ,bg-shadow-subtle)))
              `(magit-diff-file-heading-selection ((t :inherit bold :background ,bg-accent)))
              `(magit-diff-hunk-heading ((t :background ,bg-shadow-subtle)))
              `(magit-diff-hunk-heading-highlight ((t :inherit bold :background ,bg-neutral)))
              `(magit-diff-hunk-heading-selection ((t :inherit bold :background ,bg-accent)))
              `(magit-diff-lines-heading ((t :background ,fg-shadow-subtle :foreground ,bg-main)))
              `(magit-section-heading-selection ((t :inherit bold :background ,bg-accent)))
              `(magit-section-highlight ((t :background ,bg-shadow-subtle)))

              `(markdown-code-face ((t :inherit fixed-pitch :background ,bg-shadow-subtle :extend t)))
              `(markdown-language-keyword-face ((t :inherit fixed-pitch :background ,bg-neutral :foreground ,fg-neutral)))
              `(markdown-table-face ((t :inherit fixed-pitch :foreground ,fg-accent)))

              '(markup-meta-face ((t :inherit fixed-pitch)))
              '(markup-replacement-face ((t :inherit fixed-pitch)))

              `(mode-line
                ((default :background ,bg-shadow-intense :foreground ,fg-shadow-intense)
                 (((supports :box t))
                  :box ,border)
                 (t :underline ,border)))

              `(mode-line-active ((t :inherit mode-line)))
              `(mode-line-inactive
                ((default :background ,bg-shadow-subtle :foreground ,fg-shadow-subtle)
                 (((supports :box t))
                  :box ,border)
                 (t :underline ,border)))

              `(notmuch-message-summary-face
                ((default :background ,bg-shadow-subtle)
                 (((supports :overline t))
                  :overline ,fg-shadow-subtle)))

              `(org-agenda-date-weekend ((t :inherit (bold shadow))))
              `(org-agenda-date-today ((t :inherit (underline org-agenda-date))))
              `(org-agenda-date-weekend-today ((t :inherit (underline org-agenda-date-weekend))))

              `(org-block ((t :inherit fixed-pitch :background ,bg-shadow-subtle :extend t)))
              `(org-block-begin-line ((t :inherit fixed-pitch :background ,bg-neutral :foreground ,fg-neutral :extend t)))
              `(org-block-end-line ((t :inherit org-block-begin-line)))
              '(org-checkbox ((t :inherit (fixed-pitch bold))))
              `(org-code ((t :inherit (fixed-pitch italic) :foreground ,fg-shadow-subtle)))
              `(org-column-title ((t :inherit fixed-pitch :foreground ,fg-shadow-subtle)))
              '(org-date-selected ((t :inherit calendar-today)))
              `(org-document-info-keyword ((t :inherit fixed-pitch :foreground ,fg-shadow-subtle)))
              `(org-drawer ((t :inherit fixed-pitch :foreground ,fg-shadow-subtle)))
              `(org-ellipsis (( ))) ; inherits from the heading's color
              '(org-formula ((t :inherit fixed-pitch)))

              `(org-habit-alert-face ((t :background ,(doric-themes-adjust-value "#ffff00" 10) :foreground "black")))
              `(org-habit-alert-future-face ((t :background ,(doric-themes-adjust-value "#ffff00" 50) :foreground "white")))
              `(org-habit-clear-face ((t :background ,(doric-themes-adjust-value "#0000dd" 10) :foreground "white")))
              `(org-habit-clear-future-face ((t :background ,(doric-themes-adjust-value "#0000dd" 50) :foreground "white")))
              `(org-habit-overdue-face ((t :background ,(doric-themes-adjust-value "#dd0000" 10) :foreground "white")))
              `(org-habit-overdue-future-face ((t :background ,(doric-themes-adjust-value "#dd0000" 50) :foreground "black")))
              `(org-habit-ready-face ((t :background ,(doric-themes-adjust-value "#00bb00" 10) :foreground "white")))
              `(org-habit-ready-future-face ((t :background ,(doric-themes-adjust-value "#00bb00" 50) :foreground "white")))

              `(org-hide ((t :foreground ,bg-main)))
              `(org-indent ((t :inherit (fixed-pitch org-hide))))
              `(org-meta-line ((t :inherit fixed-pitch :foreground ,fg-shadow-subtle)))
              '(org-property-value ((t :inherit fixed-pitch)))
              '(org-quote ((t :inherit (italic org-block))))
              `(org-verbatim ((t :inherit (fixed-pitch italic) :foreground ,fg-shadow-subtle)))
              '(org-verse ((t :inherit org-block)))
              `(org-table ((t :inherit fixed-pitch :foreground ,fg-accent)))

              `(package-mark-delete-line ((t :inherit bold-italic :background ,bg-shadow-intense :foreground ,fg-main)))
              `(package-mark-install-line ((t :inherit bold-italic :background ,bg-accent :foreground ,fg-main)))

              `(pulsar-blue ((t :background ,bg-blue)))
              `(pulsar-cyan ((t :background ,bg-cyan)))
              `(pulsar-green ((t :background ,bg-green)))
              `(pulsar-magenta ((t :background ,bg-magenta)))
              `(pulsar-red ((t :background ,bg-red)))
              `(pulsar-yellow ((t :background ,bg-yellow)))

              '(rcirc-monospace-text ((t :inherit fixed-pitch)))
              '(rcirc-server ((t :inherit font-lock-comment-face)))

              `(reb-match-0 ((t :background ,bg-accent :foreground ,fg-main)))
              `(reb-match-1 ((t :background ,bg-shadow-subtle :foreground ,fg-shadow-subtle)))
              `(reb-match-2 ((t :background ,bg-accent :foreground ,fg-accent)))
              `(reb-match-3 ((t :background ,bg-shadow-intense :foreground ,fg-shadow-intense)))

              `(spacious-padding-line-active ((t :foreground ,fg-accent)))
              `(spacious-padding-line-inactive ((t :foreground ,bg-accent)))
              `(spacious-padding-subtle-mode-line-active ((t :foreground ,fg-accent)))
              `(spacious-padding-subtle-mode-line-inactive ((t :foreground ,bg-accent)))

              `(tab-bar-tab ((t :inherit bold :background ,bg-main :foreground ,fg-main)))
              `(tab-bar-tab-inactive ((t :background ,bg-neutral :foreground ,fg-neutral)))

              `(tab-line-tab ((t :inherit tab-line-tab-current)))
              `(tab-line-tab-current ((t :inherit bold :background ,bg-main :foreground ,fg-main)))
              `(tab-line-tab-inactive ((t :background ,bg-neutral :foreground ,fg-neutral)))
              `(tab-line-tab-inactive-alternate ((t :inherit tab-line-tab-inactive)))

              '(telega-webpage-fixed ((t :inherit fixed-pitch)))
              '(telega-webpage-preformatted ((t :inherit fixed-pitch)))

              '(textsec-suspicious (( )))

              `(vc-edited-state ((t :inherit italic)))
              `(vc-locally-added-state ((t :inherit italic)))

              '(vtable ((t :inherit fixed-pitch)))

              '(which-key-key-face ((t :inherit (fixed-pitch bold-italic))))

              `(whitespace-big-indent ((t :foreground ,bg-shadow-intense)))
              `(whitespace-empty ((t :foreground ,bg-shadow-intense)))
              `(whitespace-hspace ((t :foreground ,bg-shadow-intense)))
              `(whitespace-indentation ((t :foreground ,bg-shadow-intense)))
              `(whitespace-line ((t :foreground ,bg-shadow-intense)))
              `(whitespace-missing-newline-at-eof ((t :foreground ,bg-shadow-intense)))
              `(whitespace-newline ((t :foreground ,bg-shadow-intense)))
              `(whitespace-space ((t :foreground ,bg-shadow-intense)))

              `(whitespace-space-before-tab ((t :foreground ,bg-shadow-intense)))
              `(whitespace-tab ((t :foreground ,bg-shadow-intense)))))
           (custom-theme-set-variables
            ',name
            '(diff-font-lock-syntax nil)
            '(elisp-fontify-semantically nil)
            '(frame-background-mode ',background-mode)
            `(flymake-note-bitmap '(exclamation-mark doric-themes-prominent-note))
            `(flymake-warning-bitmap '(exclamation-mark doric-themes-prominent-warning))
            `(flymake-error-bitmap '(flymake-double-exclamation-mark doric-themes-prominent-error)))
           ,@(unless theme-exists-p
               (list `(provide-theme ',name)))))
    (error "No palette found for `%s'" name)))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'doric-themes)
;;; doric-themes.el ends here
