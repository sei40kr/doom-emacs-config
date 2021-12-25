;;; $DOOMDIR/contrib/magit.el -*- lexical-binding: t; -*-

(setq magit-clone-set-remote\.pushDefault t
      magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                               ("Version" 25 magit-repolist-column-version nil)
                               ("Path" 99 magit-repolist-column-path nil)))


;;
;;; Performance

(setq magit-refresh-status-buffer nil
      ;; Diff Performance
      magit-diff-highlight-indentation nil
      magit-diff-highlight-trailing nil
      magit-diff-paint-whitespace nil
      magit-diff-highlight-hunk-body nil
      magit-diff-refine-hunk nil
      magit-revision-insert-related-refs nil)

(after! magit
  ;; Refs Buffer Perfomance
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  ;; Committing Performance
  (remove-hook 'server-switch-hook 'magit-commit-diff))
