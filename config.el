;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Seong Yong-ju"
      user-mail-address "sei40kr@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(defun +custom--noop (&rest _))

(setq confirm-nonexistent-file-or-buffer nil
      vc-follow-symlinks t)

(global-auto-revert-mode +1)

(when (featurep 'display-fill-column-indicator)
  (add-hook! '(text-mode-hook prog-mode-hook)
    (display-fill-column-indicator-mode 1)))


;; core

;; list magit repos as projectile projects
(defun +projectile--load-known-projects-a (&rest _)
  (require 'magit)
  (setq projectile-known-projects
        (mapcar #'abbreviate-file-name (magit-list-repos))))
(after! projectile
  (setq projectile-git-submodule-command nil))
(advice-add 'projectile-load-known-projects
            :override #'+projectile--load-known-projects-a)
(dolist (func '(projectile-add-known-projects
                projectile-cleanup-known-projects
                projectile-clear-known-projects
                projectile-merge-known-projects
                projectile-remove-known-project
                projectile-save-known-projects))
  (advice-add func :override #'(lambda (&rest _))))
(advice-add 'counsel-projectile-switch-project
            :before #'+projectile--load-known-projects-a)


;; completion/company

(when (featurep! :completion company)
  (after! company
    (map! :map company-active-map
          "TAB" t
          [tab] t
          [backtab] t))

  (after! company-box
    (setq company-box-doc-enable nil)))


;; ui/doom

(when (featurep! :ui doom)
  (after! doom-themes
    (setq doom-modeline-buffer-file-name-style #'truncate-upto-root)))


;; ui/popup

(when (featurep! :ui popup)
  (setq +popup-default-alist '((window-height . 30)
                               (reusable-frames . 'visible))))


;; ui/tabs

(when (featurep! :ui tabs)
  (after! centaur-tabs
    (defun +custom--centaur-tabs-buffer-groups ()
      (cond ((or (string-equal "*" (substring (buffer-name) 0 1))
                 (memq major-mode '(magit-blame-mode
                                    magit-blob-mode
                                    magit-diff-mode
                                    magit-file-mode
                                    magit-log-mode
                                    magit-process-mode
                                    magit-status-mode))) '("Emacs"))
            ((memq major-mode '(help-mode helpful-mode)) '("Help"))
            ((memq major-mode '(org-mode
                                diary-mode
                                org-agenda-log-mode
                                org-bullets-mode
                                org-cdlatex-mode
                                org-agenda-clockreport-mode
                                org-agenda-mode
                                org-beamer-mode
                                org-indent-mode
                                org-src-mode)) '("Org"))
            (t (centaur-tabs-projectile-buffer-groups))))

    ;; list only workspace buffers
    (setq centaur-tabs-buffer-list-function #'+workspace-buffer-list
          centaur-tabs-buffer-groups-function #'+custom--centaur-tabs-buffer-groups
          centaur-tabs-close-button "×")

    (map! :map centaur-tabs-mode-map
          "M-<left>"  #'centaur-tabs-backward-tab
          "M-<right>" #'centaur-tabs-forward-tab)))


;; ui/treemacs

(when (featurep! :ui treemacs)
  (after! treemacs
    (defun +custom--treemacs-no-actions ()
      (treemacs-pulse-on-failure "There is nothing to do here."))

    (cl-defmacro +custom--lsp-treemacs-do-for-button-state!
        (&key on-lsp-error-open
              on-lsp-error-closed
              on-lsp-files-open
              on-lsp-files-closed
              on-lsp-projects-open
              on-lsp-projects-closed
              on-lsp-symbol-open
              on-lsp-symbol-closed
              on-lsp-treemacs-deps-open
              on-lsp-treemacs-deps-closed)
      `(if-let* ((btn (treemacs-current-button)))
           (pcase (treemacs-button-get btn :state)
             ,@(when on-lsp-error-open
                 `(('treemacs-lsp-error-open-state ,on-lsp-error-open)))
             ,@(when on-lsp-error-closed
                 `(('treemacs-lsp-error-closed-state ,on-lsp-error-closed)))
             ,@(when on-lsp-files-open
                 `(('treemacs-lsp-files-open-state ,on-lsp-files-open)))
             ,@(when on-lsp-files-closed
                 `(('treemacs-lsp-files-closed-state ,on-lsp-files-closed)))
             ,@(when on-lsp-projects-open
                 `(('treemacs-lsp-projects-open-state ,on-lsp-projects-open)))
             ,@(when on-lsp-projects-closed
                 `(('treemacs-lsp-projects-closed-state ,on-lsp-projects-closed)))
             ,@(when on-lsp-symbol-open
                 `(('treemacs-lsp-symbol-open-state ,on-lsp-symbol-open)))
             ,@(when on-lsp-symbol-closed
                 `(('treemacs-lsp-symbol-closed-state ,on-lsp-symbol-closed)))
             ,@(when on-lsp-treemacs-deps-open
                 `(('treemacs-lsp-treemacs-deps-open-state ,on-lsp-treemacs-deps-open)))
             ,@(when on-lsp-treemacs-deps-closed
                 `(('treemacs-lsp-treemacs-deps-closed-state ,on-lsp-treemacs-deps-closed))))))

    (defun +custom/treemacs/collapse-or-up (&rest _)
      (interactive "P")
      (treemacs-do-for-button-state
       :on-root-node-open   (treemacs--collapse-root-node btn)
       :on-root-node-closed (+custom--treemacs-no-actions)
       :on-dir-node-open    (treemacs--collapse-dir-node btn)
       :on-dir-node-closed  (treemacs-goto-parent-node)
       :on-file-node-open   (treemacs--collapse-file-node btn)
       :on-file-node-closed (treemacs-goto-parent-node)
       :on-tag-node-open    (treemacs--collapse-tag-node btn)
       :on-tag-node-closed  (treemacs-goto-parent-node)
       :on-tag-node-leaf    (treemacs-goto-parent-node)
       :no-error            t)
      (+custom--lsp-treemacs-do-for-button-state!
       :on-lsp-error-open           (treemacs-goto-parent-node)
       :on-lsp-error-closed         (treemacs-goto-parent-node)
       :on-lsp-files-open           (treemacs-collapse-lsp-files btn)
       :on-lsp-files-closed         (treemacs-goto-parent-node)
       :on-lsp-projects-open        (treemacs-collapse-lsp-projects btn)
       :on-lsp-projects-closed      (+custom--treemacs-no-actions)
       :on-lsp-symbol-open          (treemacs-collapse-lsp-symbol btn)
       :on-lsp-symbol-closed        (treemacs-goto-parent-node)))

    (defun +custom/treemacs/expand-or-down (&optional arg)
      (interactive "P")
      (treemacs-do-for-button-state
       :on-root-node-open   (treemacs-next-line 1)
       :on-root-node-closed (treemacs--expand-root-node btn)
       :on-dir-node-open    (treemacs-next-line 1)
       :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
       :on-file-node-open   (treemacs-visit-node-default)
       :on-file-node-closed (treemacs-visit-node-default)
       :on-tag-node-open    (treemacs-next-line 1)
       :on-tag-node-closed  (treemacs--expand-tag-node btn)
       :on-tag-node-leaf    (treemacs-visit-node-default)
       :no-error            t)
      (+custom--lsp-treemacs-do-for-button-state!
       :on-lsp-error-open           (lsp-treemacs-open-error btn)
       :on-lsp-error-closed         (lsp-treemacs-open-error btn)
       :on-lsp-files-open           (treemacs-next-line 1)
       :on-lsp-files-closed         (treemacs-expand-lsp-files)
       :on-lsp-projects-open        (treemacs-next-line 1)
       :on-lsp-projects-closed      (treemacs-expand-lsp-projects btn)
       :on-lsp-symbol-open          (lsp-treemacs-goto-symbol)
       :on-lsp-symbol-closed        (lsp-treemacs-goto-symbol)))

    (defun +custom/treemacs/root-up (&rest _)
      (interactive "P")
      (treemacs-root-up)
      (when-let* ((btn (treemacs-current-button))
                  (_ (treemacs-is-node-collapsed? btn)))
        (treemacs--expand-root-node btn)))

    (defun +custom/treemacs/select-down (&optional arg)
      (interactive "P")
      (treemacs-do-for-button-state
       :on-root-node-open   (+custom--noop)
       :on-root-node-closed (treemacs--expand-root-node btn)
       :on-dir-node-open    (+custom--noop)
       :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
       :on-file-node-open   (+custom--noop)
       :on-file-node-closed (treemacs--expand-file-node btn)
       :on-tag-node-open    (+custom--noop)
       :on-tag-node-closed  (treemacs--expand-tag-node btn)
       :on-tag-node-leaf    (+custom--noop))
      (when-let* ((btn (treemacs-current-button))
                  (_ (treemacs-collect-child-nodes btn)))
        (treemacs-next-line 1)))

    (defun +custom/treemacs/select-up (&rest _)
      (interactive "P")
      (treemacs-do-for-button-state
       :on-root-node-open   (+custom/treemacs/root-up)
       :on-root-node-closed (+custom/treemacs/root-up)
       :on-dir-node-open    (treemacs-goto-parent-node)
       :on-dir-node-closed  (treemacs-goto-parent-node)
       :on-file-node-open   (treemacs-goto-parent-node)
       :on-file-node-closed (treemacs-goto-parent-node)
       :on-tag-node-open    (treemacs-goto-parent-node)
       :on-tag-node-closed  (treemacs-goto-parent-node)
       :on-tag-node-leaf    (treemacs-goto-parent-node)))

    (setq treemacs-RET-actions-config
          '((root-node-open   . +custom/treemacs/expand-or-down)
            (root-node-closed . +custom/treemacs/expand-or-down)
            (dir-node-open    . +custom/treemacs/expand-or-down)
            (dir-node-closed  . +custom/treemacs/expand-or-down)
            (file-node-open   . +custom/treemacs/expand-or-down)
            (file-node-closed . +custom/treemacs/expand-or-down)
            (tag-node-open    . +custom/treemacs/expand-or-down)
            (tag-node-closed  . +custom/treemacs/expand-or-down)
            (tag-node-leaf    . +custom/treemacs/expand-or-down))
          treemacs-TAB-actions-config
          '((root-node-open   . treemacs-toggle-node)
            (root-node-closed . treemacs-toggle-node)
            (dir-node-open    . treemacs-toggle-node)
            (dir-node-closed  . treemacs-toggle-node)
            (file-node-open   . treemacs-toggle-node)
            (file-node-closed . treemacs-toggle-node)
            (tag-node-open    . treemacs-toggle-node)
            (tag-node-closed  . treemacs-toggle-node)
            (tag-node-leaf    . +custom--treemacs-no-actions))
          treemacs-recenter-after-file-follow 'always
          treemacs-recenter-after-tag-follow  'always
          treemacs-show-cursor t)

    (evil-define-key 'treemacs treemacs-mode-map
      (kbd "K")  '+custom/treemacs/select-up
      (kbd "L")  'treemacs-next-neighbour
      (kbd "gr") 'treemacs-refresh
      (kbd "h")  '+custom/treemacs/collapse-or-up
      (kbd "l")  '+custom/treemacs/expand-or-down)
    (define-key evil-treemacs-state-map (kbd "H") 'treemacs-previous-neighbour)
    (define-key treemacs-mode-map (kbd "J") '+custom/treemacs/select-down)
    (define-key treemacs-mode-map (kbd "R") 'treemacs-root-down)
    (define-key treemacs-mode-map (kbd "r") 'treemacs-rename))

  ;; Treemacs + Doom Themes
  (after! (treemacs doom-themes)
    (setq doom-themes-treemacs-enable-variable-pitch nil)

    (custom-set-faces
     '(treemacs-root-face ((t (:inherit font-lock-string-face :weight bold :height 1.0)))))))


;; ui/workspaces

(when (featurep! :ui workspaces)
  (setq +workspaces-switch-project-function '(lambda (_))
        +workspaces-on-switch-project-behavior t)

  (after! persp-mode
    (setq persp-kill-foreign-buffer-behaviour 'kill
          persp-remove-buffers-from-nil-persp-behaviour nil)))


;; editor/evil

(when (featurep! :editor evil)
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

  (after! evil-mc
    (map!
     :ni "C-M-j" 'evil-mc-make-cursor-move-next-line
     :ni "C-M-k" 'evil-mc-make-cursor-move-prev-line
     :map evil-mc-key-map))

  (after! evil-snipe
    (setq evil-snipe-repeat-keys t
          evil-snipe-repeat-scope 'line))

  (after! evil-surround
    (map! :map evil-surround-mode-map
          :v "s" 'evil-surround-region))

  ;; expand-region
  (map! :nv "C-=" #'er/contract-region
        :nv "C-+" #'er/expand-region))


;; editor/file-templates

(when (featurep! :editor file-templates)
  (setq +file-templates-dir (expand-file-name "templates/" doom-private-dir))

  (set-file-templates!
   '("\\.c$"                 :trigger "__c")
   '("\\.cpp$"               :trigger "__cpp")
   '(dockerfile-mode)
   '(editorconfig-conf-mode)
   '(go-mode)
   '(java-mode)
   '(js2-mode)
   '(kotlin-mode)
   '(perl-mode)
   '(cperl-mode)
   '(python-mode)
   '(ruby-mode)
   '(enh-ruby-mode)
   '(rust-mode)
   '(rustic-mode)
   '(scala-mode)
   '("\\.bash$"              :trigger "__bash")
   '("\\.zsh$"               :trigger "__zsh")
   '(sh-mode)))

;; editor/format

(when (featurep! :editor format)
  (setq +format-on-save-enabled-modes '(c-mode
                                        c++-mode
                                        go-mode
                                        rustic-mode)
        +format-preserve-indentation nil)

  (set-formatter! 'clang-format
    '("clang-format"
      ("-assume-filename=%s" (or buffer-file-name mode-result ""))
      ("-style=Google"))
    :modes
    '((c-mode ".c")
      (c++-mode ".cpp")
      (java-mode ".java")
      (objc-mode ".m")
      (protobuf-mode ".proto"))))


;; emacs/vc

(when (featurep! :emacs vc)
  (after! magit
    (setq magit-refresh-status-buffer nil
          magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                                   ("Version" 25 magit-repolist-column-version nil)
                                   ("Path" 99 magit-repolist-column-path nil))
          magit-repository-directories '(("~/.dotfiles" . 0)
                                         ("~/.emacs.d" . 0)
                                         ("~/.doom.d" . 0)
                                         ("~/develop/workspace" . 2))
          magit-revision-insert-related-refs nil)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)))


;; tools/lsp

(when (featurep! :tools lsp)
  (setq +lsp-company-backend '(company-yasnippet :separate company-lsp)
        gc-cons-threshold (* 1024 1024 1024)
        read-process-output-max (* 1024 1024))

  (after! lsp-mode
    (setq-hook! 'lsp-mode-hook
      company-idle-delay 0.0
      company-minimum-prefix-length 1))

  ;; LSP + Doom Themes
  (defun +custom--pick-doom-color (key)
    (nth (if (display-graphic-p) 0 1) (alist-get key doom-themes--colors)))
  (after! (lsp-ui doom-themes)
    (setq lsp-ui-imenu-colors `(,(+custom--pick-doom-color 'dark-blue)
                                ,(+custom--pick-doom-color 'cyan)))
    (set-face-foreground 'lsp-ui-sideline-code-action
                         (+custom--pick-doom-color 'yellow))))


;; tools/docker

(when (featurep! :tools docker)
  (after! docker-tramp
    (setq docker-tramp-use-names t)))


;; tools/magit

(when (featurep! :tools magit)
  (after! transient
    (map! :map (transient-map transient-edit-map)
          :g "<escape>" 'transient-quit-one)
    (map! :map transient-sticky-map
          :g "<escape>" 'transient-quit-seq)))


;; lang/go

(when (featurep! :lang go)
  (after! (go-mode lsp-ui flycheck)
    (set-next-checker! 'go-mode 'lsp-ui '(t . go-gofmt))))


;; lang/java

(when (featurep! :lang java)
  (after! lsp-java
    (setq lsp-java-maven-download-sources t
          lsp-java-format-enabled nil
          lsp-java-autobuild-enabled nil
          lsp-java-selection-enabled nil
          lsp-java-format-comments-enabled nil
          lsp-java-format-on-type-enabled nil
          lsp-java-code-generation-use-blocks t
          lsp-java-code-generation-generate-comments t
          lsp-java-code-generation-to-string-code-style "STRING_BUILDER")))


;; lang/javascript

(when (featurep! :lang javascript)
  (after! ((:or js2-mode rjsx-mode) flycheck)
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'javascript-standard)))


;; lang/markdown

(when (featurep! :lang markdown)
  (after! markdown-mode
    (setq markdown-header-scaling t)

    (set-company-backend! '(markdown-mode gfm-mode) '(company-emoji)))

  (after! grip-mode
    (require 'auth-source)
    (let* ((credential (auth-source-user-and-password "api.github.com")))
      (setq grip-github-user (car credential)
            grip-github-password (cadr credential)))))


;; lang/org

(after! org
  (set-company-backend! 'org-mode
    'company-capf
    'company-dabbrev
    'company-files
    'company-yasnippet)
  (add-hook 'org-mode-hook #'jupyter-org-interaction-mode)

  (defun +custom/org-lookup-documentation ()
    (interactive)
    (when-let* ((info (org-babel-get-src-block-info)))
      (when (and (string-prefix-p "jupyter-" (car info))
                 (symbolp (call-interactively #'jupyter-inspect-at-point)))
        (pop-to-buffer (help-buffer))
        t)))
  (set-lookup-handlers! 'org-mode
    :documentation #'+custom/org-lookup-documentation)

  (setq-hook! 'org-mode-hook
    company-idle-delay 0.2
    company-minimum-prefix-length 1))


;; lang/python

(when (featurep! :lang python)
  (after! (python lsp-ui flycheck)
    (set-next-checker! 'python-mode 'lsp-ui '(t . python-flake8))))


;; lang/ruby

(when (featurep! :lang ruby)
  (after! (ruby-mode lsp-ui flycheck)
    (set-next-checker! 'ruby-mode 'lsp-ui '(t . ruby-rubocop)))
  (after! (enh-ruby-mode lsp-ui flycheck)
    (set-next-checker! 'enh-ruby-mode 'lsp-ui '(t . ruby-rubocop))))


;; lang/rust

(when (featurep! :lang rust)
  (after! (rustic flycheck)
    (flycheck-add-mode 'rust 'rustic-mode)
    (flycheck-add-mode 'rust-cargo 'rustic-mode)
    (set-next-checker! 'rustic-mode 'rust-cargo 'rustic-clippy)))


;; lang/solidity

(when (featurep! :lang solidity)
  (setq solidity-flycheck-solc-checker-active t
        solidity-flycheck-solium-checker-active t)
  (after! solidity-mode
    (set-company-backend! 'solidity-mode
      '(company-solidity :with company-yasnippet))))


;; TypeScript

(after! (typescript-mode lsp-ui flycheck)
  (set-next-checker! 'typescript-mode 'lsp-ui '(t . javascript-eslint)))


;; lang/web

(when (featurep! :lang web)
  (after! emmet-mode
    (setq emmet-self-closing-tag-style " /")))
