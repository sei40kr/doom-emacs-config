;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 1024 1024 1024)
      +lsp-company-backend 'company-capf
      lsp-ui-sideline-show-diagnostics nil)

(setq-hook! 'lsp-mode-hook
  company-idle-delay 0.0
  company-minimum-prefix-length 1)


(defun +my-lsp/lsp--suggest-project-root-a ()
  "Get project root."
  (when (featurep 'projectile)
    (condition-case nil
        (let* ((projectile-project-root-files-functions '(projectile-root-top-down
                                                          projectile-root-top-down-recurring
                                                          projectile-root-bottom-up)))
          (projectile-project-root))
      (error nil))))
(advice-add 'lsp--suggest-project-root
            :before-until #'+my-lsp/lsp--suggest-project-root-a)


;; LSP + Doom Themes
(defun +my-lsp/pick-doom-color (key)
  (nth (if (display-graphic-p) 0 1) (alist-get key doom-themes--colors)))

(after! (lsp-ui doom-themes)
  (setq lsp-ui-imenu-colors `(,(+my-lsp/pick-doom-color 'dark-blue)
                              ,(+my-lsp/pick-doom-color 'cyan)))
  (set-face-foreground 'lsp-ui-sideline-code-action
                       (+my-lsp/pick-doom-color 'yellow)))
