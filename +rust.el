;;; $DOOMDIR/+rust.el -*- lexical-binding: t; -*-

(when (featurep! :lang rust +lsp)
  (setq rustic-lsp-server 'rust-analyzer
        lsp-rust-server 'rust-analyzer
        lsp-rust-clippy-preference "on")

  (defun +my-rust/lsp-mode--lsp--suggest-project-root-a ()
    "Get project root."
    (when (and (eq major-mode 'rustic-mode) (featurep 'projectile))
      (condition-case nil
          (projectile-root-bottom-up (file-truename default-directory)
                                     `("Cargo.toml" @,projectile-project-root-files-bottom-up))
        (error nil))))
  (advice-add 'lsp--suggest-project-root
              :before-until #'+my-rust/lsp-mode--lsp--suggest-project-root-a))
