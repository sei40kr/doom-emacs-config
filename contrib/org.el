;;; $DOOMDIR/contrib/org.el -*- lexical-binding: t; -*-

(setq org-startup-folded nil
      org-startup-with-inline-images t)

(after! smartparens-org
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\\[" "\\]"))
