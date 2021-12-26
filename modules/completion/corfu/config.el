;;; $DOOMDIR/modules/completion/corfu/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! corfu
  :hook (doom-first-input . corfu-global-mode)
  :init
  (setq corfu-count 14
        corfu-auto t
        corfu-auto-prefix 2
        corfu-commit-predicate #'corfu-candidate-previewed-p
        corfu-preselect-first nil
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               message-mode
                               help-mode
                               gud-mode
                               vterm-mode))
  (if (display-graphic-p)
      (setq corfu-min-width 60
            corfu-max-width 260
            ;; Don't show documentation in echo area, because corfu displays its
            ;; own in a child frame.
            corfu-echo-documentation nil)
    (setq corfu-min-width 0
          corfu-max-width most-positive-fixnum))

  ;; Omni-completion
  (when (featurep! :editor evil)
    (map! (:prefix "C-x"
           :i "C-l" #'cape-line
           :i "C-k" #'+corfu/cape-dict-or-keywords
           :i "C-f" #'cape-file
           :i "C-]" #'complete-tag
           :i "s"   #'cape-ispell
           :i "C-o" #'completion-at-point)))
  :config
  (when (featurep! :editor evil)
    (evil-make-overriding-map corfu-map)
    (advice-add 'corfu--setup :after #'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after #'evil-normalize-keymaps)
    (map! (:map corfu-map
           "C-n"        #'corfu-next
           "C-p"        #'corfu-previous
           "C-j"        #'corfu-next
           "C-k"        #'corfu-previous
           "C-h"        #'corfu-show-documentation
           "C-u"        #'corfu-scroll-up
           "C-d"        #'corfu-scroll-down
           "C-h"        #'corfu-doc-show
           "C-S-h"      #'corfu-show-documentation
           [backtab]    #'corfu-previous))))

(use-package! cape
  :defer t
  :config
  (setq company-dict-dir (expand-file-name "dicts" doom-private-dir)))

(use-package! kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package! corfu-doc :defer t)
