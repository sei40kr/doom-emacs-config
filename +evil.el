;;; $DOOMDIR/+evil.el -*- lexical-binding: t; -*-

(setq +evil-want-o/O-to-continue-comments nil)

;; retain visual-mode on selection shift
(after! evil
  (evil-set-command-property 'evil-shift-left  :keep-visual t)
  (evil-set-command-property 'evil-shift-right :keep-visual t))

(after! evil-snipe
  (setq evil-snipe-repeat-keys t
        evil-snipe-repeat-scope 'line))

(after! evil-surround
  (map! :map evil-surround-mode-map
        :v "s" 'evil-surround-region))


;;
;; expand-region

;; Expand region with lsp if server is capable
(defun +my-evil/expand-region ()
  "Increase selected region by semantic units."
  (interactive)
  (call-interactively
   (if (and (bound-and-true-p lsp-mode)
            (lsp-feature? "textDocument/selectionRange"))
       #'lsp-extend-selection
     #'er/expand-region)))

(map! :nv "C-=" #'er/contract-region
      :nv "C-+" #'+my-evil/expand-region)
