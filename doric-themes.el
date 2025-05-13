;;; doric-themes.el --- Minimalist themes -*- lexical-binding:t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/doric-themes
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1"))
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

(defvar doric-themes-selection-faces
  '(calendar-today
    completions-highlight
    consult-highlight-mark
    consult-highlight-match
    consult-preview-insertion
    custom-button-mouse
    custom-button-pressed
    custom-button-pressed-unraised
    custom-button-unraised
    header-line-highlight
    highlight
    hl-line
    keycast-key
    magit-diff-file-heading-selection
    markdown-highlighting-face
    mode-line-highlight
    next-error
    org-date-selected
    org-dispatcher-highlight
    read-multiple-choice-face
    rectangle-preview
    tab-bar-tab-highlight
    transient-enabled-suffix
    vertico-current))

(defvar doric-themes-intense-shadow-faces
  '(blink-matching-paren-offscreen
    corfu-current
    custom-button
    gnus-summary-cancelled
    magit-diff-lines-boundary
    mode-line
    mode-line-active
    region
    show-paren-match
    substitute-match
    tab-bar-tab))

(defvar doric-themes-intense-shadow-foreground-only-faces
  '(calendar-month-header
    change-log-date
    denote-faces-date
    denote-faces-day
    denote-faces-hour
    denote-faces-minute
    denote-faces-month
    denote-faces-second
    denote-faces-time
    denote-faces-year
    elfeed-search-date-face
    elfeed-search-feed-face
    epa-field-body
    epa-field-name
    file-name-shadow
    magit-log-author
    magit-log-date
    marginalia-date
    org-agenda-calendar-daterange
    org-agenda-column-dateline
    org-date
    org-sexp-date))

(defvar doric-themes-subtle-shadow-faces
  '(consult-preview-line
    corfu-default
    corfu-popupinfo
    diff-header
    header-line
    header-line-active
    header-line-inactive
    markdown-blockquote-face
    match
    mode-line-inactive
    org-agenda-clocking
    org-agenda-restriction-lock
    org-clock-overlay
    secondary-selection
    show-paren-match-expression
    tab-bar
    transient-disabled-suffix
    tool-bar
    vc-dir-status-ignored
    widget-button
    widget-button-pressed
    widget-documentation
    widget-field
    widget-inactive
    widget-single-line-field
    widget-unselected
    xref-match))

(defvar doric-themes-subtle-shadow-foreground-only-faces
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
    elfeed-search-title-face
    epa-validity-disabled
    font-lock-string-face
    glyphless-char
    gnus-splash
    gnus-summary-high-ancient
    gnus-summary-high-read
    gnus-summary-low-ancient
    gnus-summary-low-read
    gnus-summary-normal-ancient
    gnus-summary-normal-read
    hexl-ascii-region
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
    org-agenda-dimmed-todo-face
    org-agenda-done
    org-agenda-structure-secondary
    org-column
    org-column-title
    org-document-info-keyword
    org-done
    org-drawer
    org-headline-done
    org-meta-line
    org-scheduled
    org-scheduled-previously
    org-scheduled-today
    org-special-keyword
    org-table
    org-table-header
    org-table-row
    org-time-grid
    org-upcoming-deadline
    org-upcoming-distant-deadline
    package-help-section-name
    package-status-available
    package-status-built-in
    package-status-dependency
    package-status-external
    package-status-from-source
    package-status-new
    shadow
    transient-inactive-argument
    transient-inactive-value
    transient-unreachable
    transient-unreachable-key
    vertico-multiline
    vc-ignored-state
    window-divider
    window-divider-first-pixel
    window-divider-last-pixel
    xref-line-number))

(defvar doric-themes-accent-foreground-only-faces
  '(change-log-acknowledgment
    dired-directory
    epa-validity-high
    hexl-address-region
    log-view-message
    magit-hash
    marginalia-file-priv-exec
    org-imminent-deadline
    org-headline-todo
    org-todo
    org-warning
    package-status-installed
    transient-value
    which-key-command-description-face))

(defvar doric-themes-main-foreground-only-faces
  '(border
    breadcrumb-imenu-crumbs-face
    breadcrumb-project-base-face
    breadcrumb-project-crumbs-face
    c-annotation-face
    change-log-function
    change-log-list
    child-frame-border
    consult-bookmark
    consult-buffer
    consult-file
    denote-faces-prompt-current-name
    denote-faces-title
    diary
    dired-mark
    dired-perm-write
    dired-set-id
    dired-special
    display-time-date-and-time
    epa-mark
    epa-validity-low
    epa-validity-medium
    escape-glyph
    eww-valid-certificate
    flymake-end-of-line-diagnostics-face
    flymake-error
    flymake-error-echo
    flymake-error-echo-at-eol
    flymake-note
    flymake-note-echo
    flymake-note-echo-at-eol
    flymake-warning
    flymake-warning-echo
    flymake-warning-echo-at-eol
    font-lock-bracket-face
    font-lock-constant-face
    font-lock-delimiter-face
    font-lock-escape-face
    font-lock-function-call-face
    font-lock-function-name-face
    font-lock-misc-punctuation-face
    font-lock-negation-char-face
    font-lock-number-face
    font-lock-operator-face
    font-lock-property-name-face
    font-lock-property-use-face
    font-lock-punctuation-face
    font-lock-regexp-face
    font-lock-variable-name-face
    font-lock-variable-use-face
    fringe
    gnus-summary-high-undownloaded
    gnus-summary-high-unread
    gnus-summary-low-undownloaded
    gnus-summary-low-unread
    gnus-summary-normal-undownloaded
    gnus-summary-normal-unread
    gnus-summary-selected
    help-for-help-header
    homoglyph
    icon
    log-edit-header
    kmacro-menu-flagged
    kmacro-menu-mark
    kmacro-menu-marked
    log-edit-unknown-header
    log-view-commit-body
    magit-cherry-equivalent
    magit-diff-context-highlight
    marginalia-archive
    marginalia-char
    marginalia-file-owner
    menu
    message-signature-separator
    minibuffer-depth-indicator
    mm-command-output
    mouse
    mouse-drag-and-drop-region
    next-error-message
    nobreak-hyphen
    nobreak-space
    org-agenda-current-time
    org-agenda-date
    org-agenda-date-weekend
    org-agenda-diary
    org-agenda-filter-category
    org-agenda-filter-effort
    org-agenda-filter-regexp
    org-agenda-filter-tags
    org-agenda-structure-filter
    org-archived
    org-checkbox
    org-default
    org-document-info
    org-ellipsis
    org-formula
    org-inline-src-block
    org-latex-and-related
    org-mode-line-clock
    org-property-value
    org-quote
    org-verse
    scroll-bar
    sgml-namespace
    sh-escaped-newline
    sh-heredoc
    sh-quoted-exec
    shr-abbreviation
    shr-sliced-image
    shr-strike-through
    shr-sup
    shr-text
    so-long-mode-line-inactive
    substitute-match
    tab-bar-tab-group-inactive
    tab-bar-tab-inactive
    tab-bar-tab-ungrouped
    tab-line
    tabulated-list-fake-header
    vc-dir-directory
    vc-dir-file
    vc-dir-header-value
    vc-dir-mark-indicator
    vc-dir-status-up-to-date
    vc-state-base
    vc-up-to-date-state
    vtable
    which-key-highlighted-command-face
    which-key-note-face which-key-separator-face))

(defvar doric-themes-bold-faces
  '(abbrev-table-name
    bookmark-face
    bookmark-menu-bookmark
    breadcrumb-imenu-leaf-face
    breadcrumb-project-leaf-face
    buffer-menu-buffer
    calendar-weekday-header
    change-log-name
    change-log-file
    circe-prompt-face
    comint-highlight-prompt
    compilation-info
    compilation-mode-line-exit
    compilation-mode-line-fail
    compilation-mode-line-run
    compilation-warning
    consult-async-failed
    consult-async-finished
    consult-async-running
    consult-async-split
    consult-file
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
    diff-nonexistent
    dired-header
    edmacro-label
    elfeed-log-debug-level-face
    elfeed-log-error-level-face
    elfeed-log-info-level-face
    elfeed-log-warn-level-face
    elfeed-search-unread-title-face
    erc-prompt-face
    eshell-prompt
    font-lock-keyword-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    geiser-font-lock-repl-prompt
    git-commit-comment-action
    git-commit-comment-branch-local
    git-commit-comment-heading
    git-commit-summary
    gnus-emphasis-bold
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
    gnus-summary-high-ticked
    gnus-summary-low-ticked
    gnus-summary-normal-ticked
    grep-heading
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
    magit-diff-conflict-heading
    magit-diff-hunk-heading-highlight
    magit-mode-line-process
    magit-section-heading
    magit-tag
    markdown-header-face-1
    markdown-header-face-2
    markdown-header-face-3
    markdown-header-face-4
    markdown-header-face-5
    markdown-header-face-6
    message-header-cc
    message-header-other
    message-header-subject
    message-header-to
    minibuffer-prompt
    mode-line-buffer-id
    mode-line-emphasis
    org-agenda-date-today
    org-agenda-date-weekend-today
    org-agenda-structure
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
    shr-h1
    shr-h2
    shr-h3
    shr-h4
    shr-h5
    shr-h6
    slime-repl-prompt-face
    sly-mrepl-prompt-face
    so-long-mode-line-active
    telega-chat-prompt
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
    world-clock-label
    xref-file-header))

(defvar doric-themes-bold-italic-faces
  '(appt-notification
    change-log-conditionals
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
    diff-error
    diff-file-header
    diff-hunk-header
    dired-warning
    elfeed-search-filter-face
    eww-invalid-certificate
    font-lock-builtin-face
    font-lock-preprocessor-face
    font-lock-type-face
    gnus-emphasis-bold-italic
    help-key-binding
    info-header-xref
    magit-branch-warning
    magit-diff-file-heading-highlight
    magit-diff-lines-heading
    marginalia-file-priv-dir
    marginalia-key
    message-mml
    message-separator
    org-mode-line-clock-overrun
    package-status-avail-obso
    package-status-disabled
    package-status-held
    package-status-incompat
    package-status-unsigned
    smerge-markers
    transient-key
    transient-key-exit
    transient-key-recurse
    transient-key-return
    transient-key-stack
    transient-key-stay
    vertico-group-title
    which-key-key-face))

(defvar doric-themes-italic-faces
  '(Info-quoted
    completions-annotations
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
    diff-function
    diff-index
    elfeed-search-tag-face
    elisp-shorthand-font-lock-face
    epa-string
    font-lock-doc-face
    font-lock-doc-markup-face
    git-commit-comment-file
    git-commit-nonempty-second-line
    git-commit-overlong-summary
    gnus-cite-1
    gnus-cite-10
    gnus-cite-11
    gnus-cite-2
    gnus-cite-3
    gnus-cite-4
    gnus-cite-5
    gnus-cite-6
    gnus-cite-7
    gnus-cite-8
    gnus-cite-9
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
    help-argument-name
    holiday
    magit-branch-current
    magit-cherry-unmatched
    magit-diff-hunk-heading
    magit-diff-file-heading
    magit-mode-line-process-error
    markdown-inline-code-face
    message-cited-text-1
    message-cited-text-2
    message-cited-text-3
    message-cited-text-4
    message-header-mml
    message-header-name
    message-header-newsgroups
    message-header-xheader
    mu4e-cited-1-face
    mu4e-cited-2-face
    mu4e-cited-3-face
    mu4e-cited-4-face
    mu4e-cited-5-face
    mu4e-cited-6-face
    mu4e-cited-7-face
    notmuch-wash-cited-text
    org-agenda-calendar-event
    org-agenda-calendar-sexp
    org-code
    org-inline-src-block
    org-latex-and-related
    org-macro
    org-priority
    org-tag
    org-verbatim
    package-description
    shr-code
    transient-key-noop
    vc-dir-status-edited
    vc-edited-state
    vc-locally-added-state
    vc-git-log-edit-summary-max-warning
    vc-git-log-edit-summary-target-warning
    which-key-docstring-face
    which-key-group-description-face
    which-key-local-map-description-face))

(defvar doric-themes-underline-emphasis-faces
  '(completions-common-part
    completions-first-difference
    consult-preview-match
    custom-visibility
    eldoc-highlight-function-argument
    lazy-highlight
    orderless-match-face-0
    orderless-match-face-1
    orderless-match-face-2
    orderless-match-face-3
    show-paren-mismatch
    transient-argument
    which-key-special-key-face))

(defvar doric-themes-underline-link-faces
  '(browse-url-button
    button
    custom-link
    denote-faces-link
    denote-faces-query-link
    dired-symlink
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
    shr-link))

(defvar doric-themes-diff-added-faces
  '(denote-faces-prompt-new-name
    diff-added
    diff-indicator-added
    magit-diff-added
    magit-diffstat-added
    smerge-lower))

(defvar doric-themes-diff-added-refine-faces
  '(diff-refine-added
    magit-diff-added-highlight
    smerge-refined-added))

(defvar doric-themes-diff-changed-faces
  '(diff-changed
    diff-changed-unspecified
    diff-indicator-changed
    magit-diff-base
    smerge-base))

(defvar doric-themes-diff-changed-refine-faces
  '(diff-refine-changed
    magit-diff-base-highlight
    smerge-refined-changed))

(defvar doric-themes-diff-removed-faces
  '(denote-faces-prompt-old-name
    diff-removed
    diff-indicator-removed
    magit-diff-removed
    magit-diffstat-removed
    smerge-upper))

(defvar doric-themes-diff-removed-refine-faces
  '(diff-refine-removed
    magit-diff-removed-highlight
    smerge-refined-removed))

(defun doric-themes-prepare-faces (&rest faces-and-attributes)
  "Set faces to their respective attributes in FACES-AND-ATTRIBUTES."
  (pcase-let ((`(,faces . ,attributes) faces-and-attributes))
    (mapcar
     (lambda (face)
       (backquote (list ',face (list (list t ,@attributes)))))
     faces)))

;;;###autoload
(defmacro doric-themes-define-theme (name background-mode)
  "Define theme with NAME and `light' or `dark' BACKGROUND-MODE."
  (declare (indent 0))
  (unless (memq background-mode '(light dark))
    (error "The BACKGROUND-MODE must be either `light' or `dark'"))
  (if-let* ((palette (symbol-value (intern (format "%s-palette" name)))))
      `(progn
         (custom-declare-theme
          ',name 'doric-themes
          ,(format "Minimalist %s theme." background-mode)
          (list :kind 'color-scheme :background-mode ',background-mode :family 'doric))
         (let ,palette
           (custom-theme-set-faces
            ',name
            `(default ((t :background ,bg-main :foreground ,fg-main)))
            `(cursor ((t :background ,cursor)))

            '(bold ((t :weight bold)))
            '(italic ((t :slant italic)))
            '(bold-italic ((t :weight bold :slant italic)))
            '(underline ((t :underline t)))
            `(error ((t :foreground ,fg-faint-red)))
            `(warning ((t :foreground ,fg-faint-yellow)))
            `(success ((t :foreground ,fg-faint-green)))
            `(internal-border ((t :foreground ,bg-shadow-subtle)))
            `(vertical-border ((t :foreground ,bg-shadow-subtle)))
            `(separator-line ((t :underline ,fg-shadow-subtle)))
            `(fill-column-indicator ((t :foreground ,bg-shadow-intense)))
            `(tooltip ((t :background ,bg-accent :foreground ,fg-accent)))
            `(tty-menu-disabled-face ((t :background ,bg-accent :foreground ,fg-shadow-subtle)))
            `(tty-menu-enabled-face ((t :background ,bg-accent :foreground ,fg-main)))
            `(tty-menu-selected-face ((t :background ,fg-main :foreground ,bg-main)))

            `(ansi-color-bright-black ((t :background "gray30" :foreground "gray30")))
            `(ansi-color-black ((t :background "black" :foreground "black")))
            `(ansi-color-bright-white ((t :background "white" :foreground "white")))
            `(ansi-color-white ((t :background "gray70" :foreground "gray70")))
            `(ansi-color-bright-red ((t :background ,fg-faint-red :foreground ,fg-faint-red)))
            `(ansi-color-red ((t :background ,fg-faint-red :foreground ,fg-faint-red)))
            `(ansi-color-bright-green ((t :background ,fg-faint-green :foreground ,fg-faint-green)))
            `(ansi-color-green ((t :background ,fg-faint-green :foreground ,fg-faint-green)))
            `(ansi-color-bright-yellow ((t :background ,fg-faint-yellow :foreground ,fg-faint-yellow)))
            `(ansi-color-yellow ((t :background ,fg-faint-yellow :foreground ,fg-faint-yellow)))
            `(ansi-color-bright-blue ((t :background ,fg-faint-blue :foreground ,fg-faint-blue)))
            `(ansi-color-blue ((t :background ,fg-faint-blue :foreground ,fg-faint-blue)))
            `(ansi-color-bright-magenta ((t :background ,fg-faint-magenta :foreground ,fg-faint-magenta)))
            `(ansi-color-magenta ((t :background ,fg-faint-magenta :foreground ,fg-faint-magenta)))
            `(ansi-color-bright-cyan ((t :background ,fg-faint-cyan :foreground ,fg-faint-cyan)))
            `(ansi-color-cyan ((t :background ,fg-faint-cyan :foreground ,fg-faint-cyan)))

            ,@(doric-themes-prepare-faces doric-themes-intense-shadow-faces :background 'bg-shadow-intense :foreground 'fg-shadow-intense)
            ,@(doric-themes-prepare-faces doric-themes-subtle-shadow-faces :background 'bg-shadow-subtle :foreground 'fg-shadow-subtle)
            ,@(doric-themes-prepare-faces doric-themes-intense-shadow-foreground-only-faces :foreground 'fg-shadow-intense)
            ,@(doric-themes-prepare-faces doric-themes-subtle-shadow-foreground-only-faces :foreground 'fg-shadow-subtle)
            ,@(doric-themes-prepare-faces doric-themes-accent-foreground-only-faces :foreground 'fg-accent)
            ,@(doric-themes-prepare-faces doric-themes-main-foreground-only-faces :foreground 'fg-main)

            ,@(doric-themes-prepare-faces doric-themes-bold-faces :inherit ''bold :foreground 'fg-shadow-intense)
            ,@(doric-themes-prepare-faces doric-themes-bold-italic-faces :inherit ''bold-italic :foreground 'fg-shadow-intense)
            ,@(doric-themes-prepare-faces doric-themes-italic-faces :inherit ''italic :foreground 'fg-shadow-subtle)
            ,@(doric-themes-prepare-faces doric-themes-underline-link-faces :inherit ''underline :foreground 'fg-accent)
            ,@(doric-themes-prepare-faces doric-themes-underline-emphasis-faces :inherit ''(underline italic) :foreground 'fg-shadow-subtle)

            ,@(doric-themes-prepare-faces doric-themes-selection-faces :background 'bg-accent)

            ,@(doric-themes-prepare-faces doric-themes-diff-added-faces :foreground 'fg-faint-green)
            ,@(doric-themes-prepare-faces doric-themes-diff-added-refine-faces :inherit ''bold :foreground 'fg-faint-green)
            ,@(doric-themes-prepare-faces doric-themes-diff-changed-faces :foreground 'fg-faint-yellow)
            ,@(doric-themes-prepare-faces doric-themes-diff-changed-refine-faces :inherit ''bold :foreground 'fg-faint-yellow)
            ,@(doric-themes-prepare-faces doric-themes-diff-removed-faces :foreground 'fg-faint-red)
            ,@(doric-themes-prepare-faces doric-themes-diff-removed-refine-faces :inherit ''bold :foreground 'fg-faint-red)

            `(dired-broken-symlink ((t :inherit (underline error))))
            `(dired-marked ((t :inherit bold-italic :background ,bg-accent :foreground ,fg-main)))
            `(dired-flagged ((t :inherit bold-italic :background ,bg-shadow-intense :foreground ,fg-main)))

            `(font-lock-comment-delimiter-face ((t :inherit italic :foreground ,fg-accent)))
            `(font-lock-comment-face ((t :inherit italic :foreground ,fg-accent)))
            `(font-lock-warning-face ((t :inherit warning)))

            `(isearch ((t :inherit nil :slant normal :underline nil :background ,bg-shadow-intense :foreground ,fg-shadow-intense)))
            `(isearch-fail ((t :inherit (underline bold))))
            `(isearch-group-1 ((t :background ,bg-accent :foreground ,fg-accent)))
            `(isearch-group-2 ((t :background ,bg-shadow-intense :foreground ,fg-shadow-intense)))
            `(query-replace ((t :inherit isearch)))

            `(line-number-current-line ((t :inherit (highlight bold))))

            `(magit-diff-context-highlight (( )))
            `(magit-section-highlight (( )))

            `(org-block ((t :background ,bg-shadow-subtle :extend t)))
            `(org-block-begin-line ((t :background ,bg-shadow-subtle :foreground ,fg-shadow-intense :extend t)))
            `(org-block-end-line ((t :inherit org-block-begin-line)))
            `(org-hide ((t :background ,bg-main)))

            `(spacious-padding-subtle-mode-line-active ((t :foreground ,fg-accent)))
            `(spacious-padding-subtle-mode-line-inactive ((t :foreground ,bg-accent)))))
         (custom-theme-set-variables
          ',name
          '(frame-background-mode ',background-mode)
          '(diff-font-lock-syntax nil))
         (provide-theme ',name))
    (error "No palette found for `%s'" name)))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'doric-themes)
;;; doric-themes.el ends here
