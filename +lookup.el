;;; $DOOMDIR/+lookup.el -*- lexical-binding: t; -*-

(when (and (display-graphic-p) (fboundp 'xwidget-webkit-browse-url))
  (after! dash-docs
    (setq dash-docs-browser-func #'xwidget-webkit-browse-url)))

(after! xwidget
  (defun +custom/xwidget-webkit-goto-url-a (url)
    "Goto URL."
    (if (xwidget-webkit-current-session)
        (progn
          (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url)
          (display-buffer xwidget-webkit-last-session-buffer))
      (xwidget-webkit-new-session url)))
  (advice-add #'xwidget-webkit-goto-url
              :override #'+custom/xwidget-webkit-goto-url-a)

  (defun +custom/xwidget-webkit-new-session-a (url)
    "Create a new webkit session buffer with URL."
    (let* ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
           xw)
      (setq xwidget-webkit-last-session-buffer (get-buffer-create bufname)
            xwidget-webkit-created-window      (display-buffer
                                                xwidget-webkit-last-session-buffer))
      ;; The xwidget id is stored in a text property, so we need to have
      ;; at least character in this buffer.
      ;; Insert invisible url, good default for next `g' to browse url.
      (with-selected-window xwidget-webkit-created-window
        (insert url)
        (put-text-property 1 (+ 1 (length url)) 'invisible t)
        (setq xw (xwidget-insert 1 'webkit bufname
                                 (xwidget-window-inside-pixel-width (selected-window))
                                 (xwidget-window-inside-pixel-height (selected-window))))
        (xwidget-put xw 'callback 'xwidget-webkit-callback)
        (xwidget-webkit-mode)
        (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url))))
  (advice-add #'xwidget-webkit-new-session
              :override #'+custom/xwidget-webkit-new-session-a)

  (set-popup-rule! "^\\*xwidget"
    :vslot -11 :size 0.35 :select nil)

  (when (featurep! :editor evil +everywhere)
    (add-transient-hook! 'xwidget-webkit-mode-hook
      (+evil-collection-init 'xwidget))))
