;;; $DOOMDIR/contrib/evil-treemacs.el -*- lexical-binding: t; -*-

(after! treemacs
  (defun +evil-treemacs/treemacs-no-actions (&rest _)
    (treemacs-pulse-on-failure "There is nothing to do here."))

  (defun +evil-treemacs/treemacs-collapse-lsp-symbol-or-goto-parent (btn)
    (let* ((children (treemacs-collect-child-nodes btn)))
      (treemacs--do-collapse-lsp-symbol btn)
      (unless children
        (treemacs-goto-parent-node))))

  (defun +evil-treemacs/treemacs-expand-or-goto-lsp-symbol (btn)
    (treemacs--do-expand-lsp-symbol btn)
    ;; (unless (treemacs-collect-child-nodes btn)
    ;;   (lsp-treemacs-goto-symbol))
    )

  (cl-defmacro +evil-treemacs/treemacs-do-for-lsp-button-state!
      (&key on-lsp-error-open
            on-lsp-error-closed
            on-lsp-files-open
            on-lsp-files-closed
            on-lsp-projects-open
            on-lsp-projects-closed
            on-lsp-symbol-open
            on-lsp-symbol-closed
            on-lsp-treemacs-deps-open
            on-lsp-treemacs-deps-closed)
    `(if-let* ((btn (treemacs-current-button)))
         (pcase (treemacs-button-get btn :state)
           ,@(when on-lsp-error-open
               `(('treemacs-lsp-error-open-state ,on-lsp-error-open)))
           ,@(when on-lsp-error-closed
               `(('treemacs-lsp-error-closed-state ,on-lsp-error-closed)))
           ,@(when on-lsp-files-open
               `(('treemacs-lsp-files-open-state ,on-lsp-files-open)))
           ,@(when on-lsp-files-closed
               `(('treemacs-lsp-files-closed-state ,on-lsp-files-closed)))
           ,@(when on-lsp-projects-open
               `(('treemacs-lsp-projects-open-state ,on-lsp-projects-open)))
           ,@(when on-lsp-projects-closed
               `(('treemacs-lsp-projects-closed-state ,on-lsp-projects-closed)))
           ,@(when on-lsp-symbol-open
               `(('treemacs-lsp-symbol-open-state ,on-lsp-symbol-open)))
           ,@(when on-lsp-symbol-closed
               `(('treemacs-lsp-symbol-closed-state ,on-lsp-symbol-closed)))
           ,@(when on-lsp-treemacs-deps-open
               `(('treemacs-lsp-treemacs-deps-open-state ,on-lsp-treemacs-deps-open)))
           ,@(when on-lsp-treemacs-deps-closed
               `(('treemacs-lsp-treemacs-deps-closed-state ,on-lsp-treemacs-deps-closed))))))

  (defun +evil-treemacs/treemacs-collapse-or-up (&rest _)
    (interactive "P")
    (treemacs-do-for-button-state
     :on-root-node-open   (treemacs--collapse-root-node btn)
     :on-root-node-closed (+evil-treemacs/treemacs-no-actions)
     :on-dir-node-open    (treemacs--collapse-dir-node btn)
     :on-dir-node-closed  (treemacs-goto-parent-node)
     :on-file-node-open   (treemacs--collapse-file-node btn)
     :on-file-node-closed (treemacs-goto-parent-node)
     :on-tag-node-open    (treemacs--collapse-tag-node btn)
     :on-tag-node-closed  (treemacs-goto-parent-node)
     :on-tag-node-leaf    (treemacs-goto-parent-node)
     :no-error            t)
    (+evil-treemacs/treemacs-do-for-lsp-button-state!
     :on-lsp-error-open           (treemacs-goto-parent-node)
     :on-lsp-error-closed         (treemacs-goto-parent-node)
     :on-lsp-files-open           (treemacs--do-collapse-lsp-files btn)
     :on-lsp-files-closed         (treemacs-goto-parent-node)
     :on-lsp-projects-open        (treemacs--do-collapse-lsp-projects btn)
     :on-lsp-projects-closed      (+evil-treemacs/treemacs-no-actions)
     :on-lsp-symbol-open          (+evil-treemacs/treemacs-collapse-lsp-symbol-or-goto-parent btn)
     :on-lsp-symbol-closed        (treemacs-goto-parent-node)
     :on-lsp-treemacs-deps-open   (treemacs--do-collapse-lsp-treemacs-deps btn)
     :on-lsp-treemacs-deps-closed (treemacs-goto-parent-node)))

  (defun +evil-treemacs/treemacs-expand-or-down (&optional arg)
    (interactive "P")
    (treemacs-do-for-button-state
     :on-root-node-open   (treemacs-next-line 1)
     :on-root-node-closed (treemacs--expand-root-node btn)
     :on-dir-node-open    (treemacs-next-line 1)
     :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
     :on-file-node-open   (treemacs-visit-node-default)
     :on-file-node-closed (treemacs-visit-node-default)
     :on-tag-node-open    (treemacs-next-line 1)
     :on-tag-node-closed  (treemacs--expand-tag-node btn)
     :on-tag-node-leaf    (treemacs-visit-node-default)
     :no-error            t)
    (+evil-treemacs/treemacs-do-for-lsp-button-state!
     :on-lsp-error-open           (lsp-treemacs-open-error btn)
     :on-lsp-error-closed         (lsp-treemacs-open-error btn)
     :on-lsp-files-open           (treemacs-next-line 1)
     :on-lsp-files-closed         (treemacs--do-expand-lsp-files btn)
     :on-lsp-projects-open        (treemacs-next-line 1)
     :on-lsp-projects-closed      (treemacs--do-expand-lsp-projects btn)
     :on-lsp-symbol-open          (treemacs-next-line 1)
     :on-lsp-symbol-closed        (+evil-treemacs/treemacs-expand-or-goto-lsp-symbol btn)
     :on-lsp-treemacs-deps-open   (treemacs-next-line 1)
     :on-lsp-treemacs-deps-closed (treemacs--do-expand-lsp-treemacs-deps btn)))

  (defun +evil-treemacs/treemacs-root-up (&rest _)
    (interactive "P")
    (treemacs-root-up)
    (when-let* ((btn (treemacs-current-button))
                (_ (treemacs-is-node-collapsed? btn)))
      (treemacs--expand-root-node btn)))

  (defun +evil-treemacs/treemacs-select-down (&optional arg)
    (interactive "P")
    (treemacs-do-for-button-state
     :on-root-node-open   (+evil-treemacs/treemacs-no-actions)
     :on-root-node-closed (treemacs--expand-root-node btn)
     :on-dir-node-open    (+evil-treemacs/treemacs-no-actions)
     :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
     :on-file-node-open   (+evil-treemacs/treemacs-no-actions)
     :on-file-node-closed (treemacs--expand-file-node btn)
     :on-tag-node-open    (+evil-treemacs/treemacs-no-actions)
     :on-tag-node-closed  (treemacs--expand-tag-node btn)
     :on-tag-node-leaf    (+evil-treemacs/treemacs-no-actions))
    (when-let* ((btn (treemacs-current-button))
                (_ (treemacs-collect-child-nodes btn)))
      (treemacs-next-line 1)))

  (defun +evil-treemacs/treemacs-select-up (&rest _)
    (interactive "P")
    (treemacs-do-for-button-state
     :on-root-node-open   (+evil-treemacs/treemacs-root-up)
     :on-root-node-closed (+evil-treemacs/treemacs-root-up)
     :on-dir-node-open    (treemacs-goto-parent-node)
     :on-dir-node-closed  (treemacs-goto-parent-node)
     :on-file-node-open   (treemacs-goto-parent-node)
     :on-file-node-closed (treemacs-goto-parent-node)
     :on-tag-node-open    (treemacs-goto-parent-node)
     :on-tag-node-closed  (treemacs-goto-parent-node)
     :on-tag-node-leaf    (treemacs-goto-parent-node)))

  (setq treemacs-RET-actions-config
        '((root-node-open   . +evil-treemacs/treemacs-expand-or-down)
          (root-node-closed . +evil-treemacs/treemacs-expand-or-down)
          (dir-node-open    . +evil-treemacs/treemacs-expand-or-down)
          (dir-node-closed  . +evil-treemacs/treemacs-expand-or-down)
          (file-node-open   . +evil-treemacs/treemacs-expand-or-down)
          (file-node-closed . +evil-treemacs/treemacs-expand-or-down)
          (tag-node-open    . +evil-treemacs/treemacs-expand-or-down)
          (tag-node-closed  . +evil-treemacs/treemacs-expand-or-down)
          (tag-node-leaf    . +evil-treemacs/treemacs-expand-or-down))
        treemacs-TAB-actions-config
        '((root-node-open   . treemacs-toggle-node)
          (root-node-closed . treemacs-toggle-node)
          (dir-node-open    . treemacs-toggle-node)
          (dir-node-closed  . treemacs-toggle-node)
          (file-node-open   . treemacs-toggle-node)
          (file-node-closed . treemacs-toggle-node)
          (tag-node-open    . treemacs-toggle-node)
          (tag-node-closed  . treemacs-toggle-node)
          (tag-node-leaf    . +evil-treemacs/treemacs-no-actions))
        treemacs-recenter-after-file-follow 'always
        treemacs-recenter-after-tag-follow  'always
        treemacs-show-cursor t)

  (evil-define-key 'treemacs treemacs-mode-map
    (kbd "K")  '+evil-treemacs/treemacs-select-up
    (kbd "L")  'treemacs-next-neighbour
    (kbd "gr") 'treemacs-refresh
    (kbd "h")  '+evil-treemacs/treemacs-collapse-or-up
    (kbd "l")  '+evil-treemacs/treemacs-expand-or-down)
  (define-key evil-treemacs-state-map (kbd "H") 'treemacs-previous-neighbour)
  (define-key treemacs-mode-map (kbd "J") '+evil-treemacs/treemacs-select-down)
  (define-key treemacs-mode-map (kbd "R") 'treemacs-root-down)
  (define-key treemacs-mode-map (kbd "r") 'treemacs-rename))
