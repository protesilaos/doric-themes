(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'doric-themes t))
    (require 'doric-themes))

  (defvar doric-fire-palette
    '((cursor "#fe7062")
      (bg-main "#2a281d")
      (fg-main "#ffffff")

      (bg-shadow-subtle "#45372f")
      (fg-shadow-subtle "#b0a8a0")

      (bg-shadow-intense "#66242f")
      (fg-shadow-intense "#d0bbb8")

      (bg-accent "#5a4218")
      (fg-accent "#dfc880")

      (fg-faint-red "#d09090")
      (fg-faint-green "#85b897")
      (fg-faint-yellow "#bfa392")
      (fg-faint-blue "#95afd2")
      (fg-faint-magenta "#c5a3b2")
      (fg-faint-cyan "#a5bfce"))
  "Palette of `doric-fire' theme.")

  (doric-themes-define-theme doric-fire dark))
