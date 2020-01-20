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

(use-package! atcoder-tools
  :defer t
  :load-path "~/.doom.d/packages/atcoder-tools")
(use-package! competitive-programming-snippets
  :config
  (competitive-programming-snippets-initialize)
  :after yasnippet
  :load-path "~/.doom.d/packages/competitive-programming-snippets")


(defun +custom--noop (&rest _))

(setq +popup-default-alist                '((window-height . 30)
                                            (reusable-frames . 'visible))
      +workspaces-switch-project-function '(lambda (_))
      +evil-want-o/O-to-continue-comments nil
      +file-templates-dir                 (expand-file-name "templates/" doom-private-dir)
      +format-preserve-indentation        nil)


;; Tabs

(after! centaur-tabs
  ;; Show only workspace-local buffers
  (setq centaur-tabs-buffer-list-function '+workspace-buffer-list)

  (map! :map centaur-tabs-mode-map
        :g
        "M-<left>"  'centaur-tabs-backward-tab
        "M-<right>" 'centaur-tabs-forward-tab))


;; Treemacs

(after! treemacs
  (defun +custom--treemacs-no-actions ()
    (treemacs-pulse-on-failure "There is nothing to do here."))

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
     :on-nil              (+custom--treemacs-no-actions)))

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
     :on-nil              (+custom--treemacs-no-actions)))

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
     :on-tag-node-leaf    (+custom--noop)
     :on-nil              (+custom--noop))
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
     :on-tag-node-leaf    (treemacs-goto-parent-node)
     :on-nil              (+custom--treemacs-no-actions)))

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


;; Editor

;; C-h
(map! :g "C-h" 'delete-backward-char)
(after! company
  (map! :map company-active-map
        :i "C-h" nil
        :e "C-h" nil))
(after! org-mode
  (map! :map org-mode-map
        :i "C-h" 'org-delete-backward-char
        :e "C-h" 'org-delete-backward-char))
(after! vterm
  (map! :map vterm-mode-map
        :i "C-h" 'vterm--self-insert
        :e "C-h" 'vterm--self-insert))

;; Esc to escape from key sequence
(after! transient
  (map! :map (transient-map transient-edit-map)
        :g "<escape>" 'transient-quit-one)
  (map! :map transient-sticky-map
        :g "<escape>" 'transient-quit-seq))

;; Prevent avy from waiting for second character
(after! avy
  (setq avy-timeout-seconds 0.0))

;; Spacemacs-like multiple cursors key bindings
(after! evil-mc
  (map!
   :ni
   "C-M-j" 'evil-mc-make-cursor-move-next-line
   "C-M-k" 'evil-mc-make-cursor-move-prev-line
   :map evil-mc-key-map))


;; File Templates

(set-file-templates!
 '(c++-mode                 :trigger "__.cpp")
 '(c-mode                   :trigger "__.c")
 '(cperl-mode               :trigger "__.pl"                :mode perl-mode)
 '(dockerfile-mode          :trigger "__Dockerfile")
 '(editorconfig-conf-mode   :trigger "__editorconfig")
 '(enh-ruby-mode            :trigger "__.rb"                :mode ruby-mode)
 '(go-mode                  :trigger "__.go")
 '(java-mode                :trigger "__.java")
 '(js2-mode                 :trigger "__.js")
 '(kotlin-mode              :trigger "__.kt")
 '(perl-mode                :trigger "__.pl")
 '(python-mode              :trigger "__.py")
 '(ruby-mode                :trigger "__.rb")
 '(rust-mode                :trigger "__.rs")
 '(scala-mode               :trigger "__.scala")
 '("\\.bash$"               :trigger "__.bash"              :mode sh-mode)
 '("\\.zsh$"                :trigger "__.zsh"               :mode sh-mode)
 '(sh-mode                  :trigger "__.sh")
 '("/docker-compose\\.yml$" :trigger "__docker-compose.yml" :mode yaml-mode)
 '("-service\\.ya?ml$"      :trigger "__-service.yml"       :mode yaml-mode)
 '("-volumeclaim\\.ya?ml$"  :trigger "__-volumeclaim.yml"   :mode yaml-mode)
 '("/ecs-params\\.yml$"     :trigger "__ecs-params.yml"     :mode yaml-mode)
 '("/\\.travis\\.ya?ml$"    :trigger "__travis.yml"         :mode yaml-mode))


;; Doom Themes

(after! doom-themes
  (setq doom-modeline-buffer-file-name-style #'truncate-upto-root)

  (defun +custom--pick-doom-color (key)
    (nth (if (display-graphic-p) 0 1) (alist-get key doom-themes--colors))))

;; Doom Themes + LSP
(after! (lsp-ui doom-themes)
  (setq lsp-ui-imenu-colors '((+custom--pick-doom-color 'cyan)
                              (+custom--pick-doom-color 'green))))

;; Doom Themes + Treemacs
(after! (treemacs doom-themes)
  (setq doom-themes-treemacs-enable-variable-pitch nil)

  (custom-set-faces
   '(treemacs-root-face ((t (:inherit font-lock-string-face :weight bold :height 1.0))))))


;; Perspective

(after! persp-mode
  (setq persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil))


;; Version Control

(setq vc-follow-symlinks t)
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
  (remove-hook 'server-switch-hook 'magit-commit-diff))


;; Projectile

(defun +custom--register-magit-repos-as-projectile-known-projects ()
  (require 'magit-repos)
  (setq projectile-known-projects (mapcar 'abbreviate-file-name (magit-list-repos))))

(after! projectile
  (setq projectile-git-submodule-command nil)

  (advice-add 'projectile-load-known-projects
              :override '+custom--register-magit-repos-as-projectile-known-projects)
  (advice-add 'projectile-remove-known-project
              :override '(lambda (&optional project)))
  (advice-add 'projectile-add-known-projects
              :override '(lambda (project-root)))
  (dolist (func '(projectile-cleanup-known-projects
                  projectile-clear-known-projects
                  projectile-save-known-projects
                  projectile-merge-known-projects))
    (advice-add func
                :override '(lambda ()))))
;; (after! counsel-projectile
;;   (setq projectile-current-project-on-switch 'remove
;;         counsel-projectile-remove-current-project t)

;;   (advice-add 'counsel-projectile-switch-project
;;               :before '(lambda (&optional _)
;;                          (+custom--register-magit-repos-as-projectile-known-projects))))


;; Markdown
(after! grip-mode
  (setq markdown-header-scaling t)

  (require 'auth-source)
  (let* ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))


;; Python

(set-repl-handler! 'python-mode '+python/open-jupyter-repl)


;; Web

(after! emmet-mode
  (setq emmet-self-closing-tag-style " /"))
