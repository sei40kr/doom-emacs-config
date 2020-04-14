;;; $DOOMDIR/+lookup.el -*- lexical-binding: t; -*-

(when (and (fboundp 'xwidget-webkit-browse-url) (display-graphic-p))
  (after! dash-docs
    (setq dash-docs-browser-func #'xwidget-webkit-browse-url)))

(after! xwidget
  (defun +lookup--display-xwidget-webkit-session-buffer-a (&rest _)
    (display-buffer xwidget-webkit-last-session-buffer))
  (advice-add #'xwidget-webkit-goto-url
              :after #'+lookup--display-xwidget-webkit-session-buffer-a)
  (advice-add #'xwidget-webkit-new-session
              :after #'+lookup--display-xwidget-webkit-session-buffer-a)

  (set-popup-rule! "^\\*xwidget" :vslot -11 :size 0.35 :select nil)

  (when (featurep! :editor evil +everywhere)
    (add-transient-hook! 'xwidget-webkit-mode-hook
      (+evil-collection-init 'xwidget))))
