;;; $DOOMDIR/contrib/projectile-magit.el -*- lexical-binding: t; -*-

(setq magit-repository-directories '(("~/.dotfiles" . 0)
                                     ("~/.emacs.d" . 0)
                                     ("~/.doom.d" . 0)
                                     ("~/dev/ws" . 2)))

(defadvice! +projectile-magit--add-magit-repos-as-known-projects-a (&rest _)
  :override #'projectile-load-known-projects
  :after #'magit-clone
  (require 'magit-repos)
  (setq projectile-known-projects
        (mapcar #'file-name-as-directory
                (mapcar #'abbreviate-file-name
                        (cl-remove-if #'projectile-ignored-project-p (magit-list-repos)))))
  (projectile-save-known-projects))

(defadvice! +projectile-magit--inhibit-saving-known-projects-a (&rest _)
  :override
  #'projectile-clear-known-projects
  #'projectile-merge-known-projects)
