;;; $DOOMDIR/magit.el -*- lexical-binding: t; -*-

(defun +magit--clone-default-directory (url-or-repo)
  (and (string-match "\\([^/:]+\\)/\\(.+\\)$" url-or-repo)
       (format "~/projects/%s/" (match-string 1 url-or-repo))))

(setq magit-clone-default-directory #'+magit--clone-default-directory
      magit-clone-set-remote\.pushDefault t
      magit-refresh-status-buffer nil
      magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                               ("Version" 25 magit-repolist-column-version nil)
                               ("Path" 99 magit-repolist-column-path nil))
      magit-revision-insert-related-refs nil)

(after! magit
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff))
