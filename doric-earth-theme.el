(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'doric-themes t))
    (require 'doric-themes))

  (defvar doric-earth-palette
    '((cursor "#770000")
      (bg-main "#f0eddf")
      (fg-main "#000000")

      (bg-shadow-subtle "#dfd8c8")
      (fg-shadow-subtle "#605040")

      (bg-shadow-intense "#c09fa0")
      (fg-shadow-intense "#3f0b20")

      (bg-accent "#dfc8ab")
      (fg-accent "#783002")

      (fg-faint-red "#750000")
      (fg-faint-green "#056100")
      (fg-faint-yellow "#5f4602")
      (fg-faint-blue "#353362")
      (fg-faint-magenta "#553372")
      (fg-faint-cyan "#35485e"))
  "Palette of `doric-earth' theme.")

  (doric-themes-define-theme doric-earth light))
