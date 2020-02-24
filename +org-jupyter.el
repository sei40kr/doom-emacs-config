;;; $DOOMDIR/+org-jupyter.el -*- lexical-binding: t; -*-

(defun +custom/org-jupyter/org-babel-get-src-block-lang ()
  (when-let* ((info (org-babel-get-src-block-info)))
    (car info)))

(defun +custom/org-jupyter/org-babel-jupyter-src-block-p ()
  (string-prefix-p "jupyter-" (+custom/org-jupyter/org-babel-get-src-block-lang)))

(after! ob-jupyter
  (delq :text/html jupyter-org-mime-types)

  (defun +custom/org-jupyter/org-dwim-at-point-a ()
    (when (+custom/org-jupyter/org-babel-jupyter-src-block-p)
      (jupyter-org-execute-and-next-block)
      t))
  (advice-add #'+org/dwim-at-point
              :before-until #'+custom/org-jupyter/org-dwim-at-point-a))

(after! org
  (defun +custom/org-jupyter/org-lookup-documentation ()
    (interactive)
    (when (and (+custom/org-jupyter/org-babel-jupyter-src-block-p)
               (symbolp (call-interactively #'jupyter-inspect-at-point)))
      (pop-to-buffer (help-buffer))
      t))

  (set-company-backend! 'org-mode
    'company-capf
    'company-dabbrev
    'company-files
    'company-yasnippet)
  (set-lookup-handlers! 'org-mode
    :documentation #'+custom/org-jupyter/org-lookup-documentation)

  (add-hook! 'org-mode-hook
    (require 'ob-jupyter)
    (jupyter-org-interaction-mode 1))
  (setq-hook! 'org-mode-hook
    company-idle-delay 0.2
    company-minimum-prefix-length 1))

(after! company-box
  (defconst +custom-org-jupyter--company-box-icons-alist
    '(("class"     . Class)
      ("function"  . Function)
      ("instance"  . Variable)
      ("keyword"   . Keyword)
      ("module"    . Module)
      ("statement" . Variable)
      ("param"     . Property)
      ("path"      . File)))

  (defun +custom/org-jupyter/company-box-icons-jupyter (candidate)
    (when (and (eq major-mode 'org-mode)
               (string-equal (+custom/org-jupyter/org-babel-get-src-block-lang)
                             "jupyter-python"))
      (alist-get (string-trim (get-text-property 0 'annot candidate))
                 +custom-org-jupyter--company-box-icons-alist
                 nil nil #'string-equal)))
  (push #'+custom/org-jupyter/company-box-icons-jupyter
        company-box-icons-functions))
