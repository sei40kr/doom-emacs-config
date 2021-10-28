;;; $DOOMDIR/modules/checkers/grammarly/doctor.el


(assert! (featurep! :tools lsp)
         "This module requires (:tools lsp)")

(assert! (not (featurep! :tools lsp +eglot))
         "This module is not supported with eglot, only with lsp-mode.")
