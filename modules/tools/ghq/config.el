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


(defun +ghq-magit-clone-default-directory-fn (url)
  (let* ((parts (last (split-string url "/" nil "\\.git\\'") 3))
         (rel-path (concat (pcase (length parts)
                             (1 (format "github.com/%s/" (+ghq--get-user)))
                             (2 "github.com/")
                             (_ ""))
                           (string-join parts "/"))))
    (require 'ghq)
    (format "%s/%s" ghq--root rel-path)))

(defun +ghq--get-user ()
  (require 'magit)
  (or (magit-get "ghq.user") (magit-get "github.user") user-login-name))
