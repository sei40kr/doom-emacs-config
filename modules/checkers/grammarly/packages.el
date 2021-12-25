;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/modules/checkers/grammarly/packages.el

(when (featurep! :checkers syntax)
  (package! flycheck-grammarly :pin "cb086c996db0837e774a5dc9edca9592e2e8f9a8"))
(when (featurep! :tools lsp)
    (if (featurep! :tools lsp +eglot)
        (package! eglot-grammarly
          :recipe (:host github :repo "emacs-grammarly/eglot-grammarly")
          :pin "8c5d09643d73cd961c364ab2785e421bcab905f1")
      (package! lsp-grammarly :pin "0a8d9468aeb414bc698566534389031837ba354d")
      (package! keytar :pin "584395339f85a95ffe3ade3f4e30898bad495ecd")))
