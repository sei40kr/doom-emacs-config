;;; $DOOMDIR/+jupyter.el -*- lexical-binding: t; -*-

(defadvice! +jupyter--org-dwim-at-point-a (&optional _)
  :before-until #'+org/dwim-at-point
  (when-let ((info (org-babel-get-src-block-info t))
             ((string-prefix-p "jupyter-" (car info))))
    (jupyter-org-execute-and-next-block)
    t))

(setq-hook! 'jupyter-org-interaction-mode-hook
  company-idle-delay 0.2)
