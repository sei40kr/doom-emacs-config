;;; $DOOMDIR/modules/checkers/grammarly/config.el

;;
;;; Variables

(defvar +grammarly-enabled-modes '(org-mode
                                   markdown-mode
                                   TeX-mode
                                   rst-mode
                                   mu4e-compose-mode
                                   message-mode
                                   git-commit-mode)
  "A list of major modes in which to work with Grammarly.")


;;
;;; Packages

(use-package! flycheck-grammarly
  :when (featurep! :checkers syntax)
  :init
  (setq flycheck-grammarly-active-modes +grammarly-enabled-modes))

(use-package! lsp-grammarly
  :when (featurep! :tools lsp)
  :when (not (featurep! :tools lsp +eglot))
  :after lsp-mode
  :config
  (setq lsp-grammarly-active-modes +grammarly-enabled-modes))

(use-package! keytar
  :when (featurep! :tools lsp)
  :when (not (featurep! :tools lsp +eglot))
  :defer t
  :config
  (setq keytar-install-dir (concat doom-etc-dir "keytar")))

(use-package! eglot-grammarly
  :when (featurep! :tools lsp +eglot)
  :after eglot
  :init
  (setq eglot-grammarly-active-modes +grammarly-enabled-modes))
