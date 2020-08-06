;;; $DOOMDIR/+org-jupyter.el -*- lexical-binding: t; -*-

(defun +custom/org-jupyter/org-babel-get-src-block-lang ()
  (when-let* ((info (org-babel-get-src-block-info)))
    (car info)))

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
      (when-let* ((type (get-text-property 0 'annot candidate))
                  (type (string-trim type)))
        (alist-get type +custom-org-jupyter--company-box-icons-alist
                   nil nil #'string-equal))))
  (push #'+custom/org-jupyter/company-box-icons-jupyter
        company-box-icons-functions))
