;;; $DOOMDIR/contrib/org-jupyter.el -*- lexical-binding: t; -*-

(setq-hook! 'jupyter-org-interaction-mode-hook
  company-minimum-prefix-length 1
  company-idle-delay 0.2)

(defadvice! +org-jupyter--org-dwim-at-point-a (&optional _)
  :before-until #'+org/dwim-at-point
  (when-let ((info (org-babel-get-src-block-info t))
             ((string-prefix-p "jupyter-" (car info))))
    (jupyter-org-execute-and-next-block)
    t))
