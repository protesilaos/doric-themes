(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'doric-themes t))
    (require 'doric-themes))

  (defvar doric-wind-palette
    '((cursor "#000077")
      (bg-main "#e8f0e9")
      (fg-main "#000000")
      
      (bg-shadow-subtle "#d3dbd9")
      (fg-shadow-subtle "#40484f")

      (bg-shadow-intense "#8fbbbe")
      (fg-shadow-intense "#0f1f30")

      (bg-accent "#bad0f4")
      (fg-accent "#0f2381")

      (fg-faint-red "#750000")
      (fg-faint-green "#25582f")
      (fg-faint-yellow "#5f4302")
      (fg-faint-blue "#353362")
      (fg-faint-magenta "#553372")
      (fg-faint-cyan "#35485e"))
    "Palette of `doric-wind' theme.")

  (doric-themes-define-theme doric-wind light))
