;;; $DOOMDIR/modules/tools/ghq/config.el -*- lexical-binding: t; -*-

(use-package! ghq :defer t)


(after! magit
  (require 'ghq)
  (setq ;; magit-repos
        magit-repository-directories
        (delete-dups (append `((,ghq--root . 3)) magit-repository-directories))
        ;; magit-clone
        magit-clone-default-directory #'+ghq-magit-clone-default-directory-fn))

(after! projectile
  (require 'ghq)
  (setq projectile-project-search-path
        (delete-dups (append `((,ghq--root . 3)) projectile-project-search-path))))


(defun +ghq-magit-clone-default-directory-fn (repo)
  (require 'ghq)
  (when (or (string-match "\\`\\(?:[^@]+@\\)?\\([^:]+\\):/?\\([^/]+\\)" repo)
            (string-match "\\`[^:]+://\\([^/]+\\)/\\([^/]+\\)" repo))
    (let* ((host (match-string 1 repo))
           (user (match-string 2 repo)))
      (format "%s/%s/%s/" ghq--root host user))))
