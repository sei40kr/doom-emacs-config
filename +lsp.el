;;; $DOOMDIR/+lsp.el -*- lexical-binding: t; -*-

(setq +lsp-company-backend '(company-yasnippet :separate company-lsp)
      gc-cons-threshold (* 1024 1024 1024)
      read-process-output-max (* 1024 1024))

(after! lsp-mode
  ;; prevent lsp from selecting the lsp checker explicitly
  (defun +custom/lsp/lsp-flycheck-enable-a (&rest _)
    (flycheck-mode 1)
    (when lsp-flycheck-live-reporting
      (setq-local flycheck-check-syntax-automatically nil))
    ;; (setq-local flycheck-checker 'lsp)
    (lsp-flycheck-add-mode major-mode)
    (add-to-list 'flycheck-checkers 'lsp)
    (add-hook 'lsp-after-diagnostics-hook #'lsp--flycheck-report nil t))
  (advice-add #'lsp-flycheck-enable
              :override #'+custom/lsp/lsp-flycheck-enable-a)

  (setq-hook! 'lsp-mode-hook
    company-idle-delay 0.0
    company-minimum-prefix-length 1))

(after! lsp-ui
  (setq lsp-ui-sideline-show-diagnostics nil))
