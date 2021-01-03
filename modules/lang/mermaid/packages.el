;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/modules/lang/mermaid/packages.el

(package! mermaid-mode :pin "6ec97ab934023a8aa094705bb1c9803fd85d24c1")
(when (featurep! :lang org)
  (package! ob-mermaid :pin "ae388e032bff7ce01085f1e1062826d8cc8ef59f"))
