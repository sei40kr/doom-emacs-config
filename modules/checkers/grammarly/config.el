;;; $DOOMDIR/modules/checkers/grammarly/config.el

(use-package! lsp-grammarly
  :after lsp-mode
  :config
  (setq lsp-grammarly-active-modes '(org-mode
                                     markdown-mode
                                     TeX-mode
                                     rst-mode
                                     mu4e-compose-mode
                                     message-mode
                                     git-commit-mode)))

(use-package! keytar
  :defer t
  :config
  (setq keytar-install-dir (concat doom-etc-dir "keytar")))
