# Doric themes for GNU Emacs

These are my minimalist themes. They use few colours and will appear
mostly monochromatic in many contexts. Styles involve the careful use
of typography, such as italics and bold italics.

If you want maximalist themes in terms of colour, check my `ef-themes`
package. For something in-between, which I would consider the best
"default theme" for a text editor, opt for my `modus-themes`.

+ Package name (GNU ELPA): `doric-themes` (WORK-IN-PROGRESS)
+ Sample pictures: <https://protesilaos.com/emacs/doric-themes-pictures>
+ Git repository: <https://github.com/protesilaos/doric-themes>
+ Backronym: Doric Only Really Intensifies Conservatively ... themes.

## Sample configuration

```elisp
(use-package doric-themes
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
  :bind
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-rotate)))
```

