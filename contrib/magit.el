;;; $DOOMDIR/contrib/magit.el -*- lexical-binding: t; -*-

(setq magit-clone-set-remote\.pushDefault t
      magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                               ("Version" 25 magit-repolist-column-version nil)
                               ("Path" 99 magit-repolist-column-path nil))
      magit-revision-insert-related-refs nil)

(after! magit
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff))
