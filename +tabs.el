;;; $DOOMDIR/+tabs.el -*- lexical-binding: t; -*-

(after! centaur-tabs
  (defun +custom/tabs/centaur-tabs-buffer-groups ()
    (cond ((or (string-equal "*" (substring (buffer-name) 0 1))
               (memq major-mode '(magit-blame-mode
                                  magit-blob-mode
                                  magit-diff-mode
                                  magit-file-mode
                                  magit-log-mode
                                  magit-process-mode
                                  magit-revision-mode
                                  magit-status-mode))) '("Emacs"))
          ((memq major-mode '(help-mode helpful-mode)) '("Help"))
          ((memq major-mode '(org-mode
                              diary-mode
                              org-agenda-log-mode
                              org-bullets-mode
                              org-cdlatex-mode
                              org-agenda-clockreport-mode
                              org-agenda-mode
                              org-beamer-mode
                              org-indent-mode
                              org-src-mode)) '("Org"))
          ((eq major-mode 'xwidget-webkit-mode) '("Xwidgets"))
          (t (centaur-tabs-projectile-buffer-groups))))

  (setq centaur-tabs-buffer-list-function #'+workspace-buffer-list
        centaur-tabs-buffer-groups-function
        #'+custom/tabs/centaur-tabs-buffer-groups
        centaur-tabs-close-button "×")

  (map! :map centaur-tabs-mode-map
        "M-<left>"  #'centaur-tabs-backward-tab
        "M-<right>" #'centaur-tabs-forward-tab))
