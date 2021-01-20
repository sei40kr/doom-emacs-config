;;; editor/spacemacs-editing/config.el -*- lexical-binding: t; -*-

(use-package! expand-region
  :defer t
  :init
  (setq expand-region-fast-keys-enabled nil)

  (defun +spacemacs-editing/expand-region-and-prepare-for-more-expansions ()
    (interactive)
    (call-interactively #'er/expand-region)
    (call-interactively #'+spacemacs-editing/expansions/body))

  (map! :leader
        :desc "Expand region" "v" #'+spacemacs-editing/expand-region-and-prepare-for-more-expansions))
