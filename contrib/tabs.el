;;; $DOOMDIR/contrib/tabs.el -*- lexical-binding: t; -*-

(after! centaur-tabs
  (defun +tabs--buffer-groups-fn ()
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

  (setq centaur-tabs-height 31
        centaur-tabs-left-edge-margin ""
        centaur-tabs-right-edge-margin " "
        centaur-tabs-buffer-list-function #'+workspace-buffer-list
        centaur-tabs-buffer-groups-function #'+tabs--buffer-groups-fn)
  (after! centaur-tabs
    (setq centaur-tabs-set-bar 'under
          x-underline-at-descent-line t))

  (map! :map centaur-tabs-mode-map
        "M-<left>"  #'+tabs:previous-or-goto
        "M-<right>" #'+tabs:next-or-goto))
