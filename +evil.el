;;; $DOOMDIR/+evil.el -*- lexical-binding: t; -*-

(setq +evil-want-o/O-to-continue-comments nil)

;; use C-h as backspace everywhere
(map! :g "C-h" nil
      :ie "C-h" #'backward-delete-char-untabify
      :map minibuffer-local-map
      :g "C-h" #'doom/silent-backward-delete-char)
(after! company
  (map! :map company-active-map
        :g "C-h" nil))
(after! evil
  (map! :map evil-ex-completion-map
        :g "C-h" #'evil-ex-delete-backward-char))
(after! evil-org
  (map! :map evil-org-mode-map
        :i "C-h" (general-predicate-dispatch 'evil-org-delete-backward-char
                   (org-at-table-p) 'org-table-previous-field)
        :e "C-h" (general-predicate-dispatch 'backward-delete-char-untabify
                   (org-at-table-p) 'org-table-previous-field)))
(after! ivy
  (map! :map ivy-minibuffer-map
        :g "C-h" #'ivy-backward-delete-char))
(after! vterm
  (map! :map vterm-mode-map
        :ie "C-h" 'vterm--self-insert))

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

;; expand-region
(map! :nv "C-=" #'er/contract-region
      :nv "C-+" #'er/expand-region)
