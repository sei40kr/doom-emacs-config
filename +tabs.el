;;; $DOOMDIR/+tabs.el -*- lexical-binding: t; -*-

(after! centaur-tabs
  (defun +my-tabs/centaur-tabs-buffer-groups ()
    (cond ((memq major-mode '(help-mode helpful-mode)) '("Help"))
          ((memq major-mode '(magit-blame-mode
                              magit-blob-mode
                              magit-diff-mode
                              magit-file-mode
                              magit-log-mode
                              magit-process-mode
                              magit-revision-mode
                              magit-status-mode)) '("Magit"))
          ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
          ((or (get-buffer-process (current-buffer))
               (memq major-mode '(comint-mode compilation-mode))) '("Term"))
          ((memq major-mode '(calendar-mode diary-mode)) '("Org"))
          ((eq major-mode 'xwidget-webkit-mode) '("Xwidgets"))
          (t '("Other"))))

  (setq centaur-tabs-buffer-list-function #'+workspace-buffer-list
        centaur-tabs-buffer-groups-function #'+my-tabs/centaur-tabs-buffer-groups
        centaur-tabs-close-button "×"
        centaur-tabs-modified-marker "⚫")

  (map! :map centaur-tabs-mode-map
        "M-<left>"  #'centaur-tabs-backward-tab
        "M-<right>" #'centaur-tabs-forward-tab)

  (custom-set-faces!
    '((centaur-tabs-default
       centaur-tabs-unselected
       centaur-tabs-selected
       centaur-tabs-unselected-modified
       centaur-tabs-selected-modified
       centaur-tabs-close-unselected
       centaur-tabs-close-selected
       centaur-tabs-close-mouse-face
       centaur-tabs-modified-marker-selected
       centaur-tabs-modified-marker-unselected)
      :inherit variable-pitch)))
