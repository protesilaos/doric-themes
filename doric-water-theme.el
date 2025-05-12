(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'doric-themes t))
    (require 'doric-themes))

  (defvar doric-water-palette
    '((cursor "#69eebb")
      (bg-main "#2a283d")
      (fg-main "#ffffff")

      (bg-shadow-subtle "#404457")
      (fg-shadow-subtle "#aea8b8")

      (bg-shadow-intense "#30409f")
      (fg-shadow-intense "#cfcff8")

      (bg-accent "#38594f")
      (fg-accent "#98dcbf")

      (fg-faint-red "#dba2a2")
      (fg-faint-green "#85b897")
      (fg-faint-yellow "#bfa392")
      (fg-faint-blue "#95afd2")
      (fg-faint-magenta "#c5a3b2")
      (fg-faint-cyan "#a5bfce"))
  "Palette of `doric-water' theme.")

  (doric-themes-define-theme doric-water dark))
