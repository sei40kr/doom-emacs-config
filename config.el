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
(setq doom-font (font-spec :family "Operator Mono" :size 18))

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

(setq confirm-nonexistent-file-or-buffer nil
      vc-follow-symlinks t)
(load! "+secrets")

(global-auto-revert-mode +1)
(global-subword-mode +1)

(when EMACS27+
  (add-hook! '(text-mode-hook prog-mode-hook)
    (display-fill-column-indicator-mode 1)))


;;
;; core

;; List repositories under `magit-repository-directories' as projectile
;; projects.
(setq magit-repository-directories '(("~/.dotfiles" . 0)
                                     ("~/.emacs.d" . 0)
                                     ("~/.doom.d" . 0)
                                     ("~/projects" . 2)))
(defun +core--projectile-load-known-projects-a (&rest _)
  (require 'magit)
  (setq projectile-known-projects (mapcar #'abbreviate-file-name
                                          (magit-list-repos))))
(advice-add 'projectile-load-known-projects
            :override #'+core--projectile-load-known-projects-a)
(advice-add 'counsel-projectile-switch-project
            :before #'+core--projectile-load-known-projects-a)
(dolist (func '(projectile-add-known-projects
                projectile-cleanup-known-projects
                projectile-clear-known-projects
                projectile-merge-known-projects
                projectile-remove-known-project
                projectile-save-known-projects))
  (advice-add func :override #'(lambda (&rest _))))

(setq projectile-git-submodule-command nil)


;;
;; completion/company

(when (featurep! :completion company)
  (after! company
    (map! :map company-active-map
          "TAB" nil
          [tab] nil
          [backtab] nil))

  (after! company-box
    (setq company-box-doc-enable nil)))


;;
;; ui/doom

(when (featurep! :ui doom)
  (after! doom-themes
    (setq doom-modeline-buffer-file-name-style #'truncate-upto-root)
    (custom-theme-set-faces! 'doom-one
      `(font-lock-comment-face
        :foreground ,(doom-color 'comments)
        :background ,(when doom-one-comment-bg
                       (doom-lighten (doom-color 'bg) 0.05))
        :slant italic))))


;;
;; ui/popup

(when (featurep! :ui popup)
  (setq +popup-default-alist '((window-height . 30)
                               (reusable-frames . 'visible))))


;;
;; ui/tabs

(when (featurep! :ui tabs)
  (load! "+tabs"))


;;
;; ui/treemacs

(when (featurep! :ui treemacs)
  (load! "+evil-treemacs")

  ;; Treemacs + Doom Themes
  (after! (treemacs doom-themes)
    (setq doom-themes-treemacs-enable-variable-pitch nil)

    (custom-set-faces
     '(treemacs-root-face ((t (:inherit font-lock-string-face :weight bold :height 1.0)))))))


;;
;; ui/workspaces

(when (featurep! :ui workspaces)
  (setq +workspaces-switch-project-function #'(lambda (project-dir)
                                                (switch-to-buffer (doom-fallback-buffer))
                                                (setq default-directory project-dir))
        +workspaces-on-switch-project-behavior t)

  (after! persp-mode
    (setq persp-kill-foreign-buffer-behaviour 'kill
          persp-remove-buffers-from-nil-persp-behaviour nil)))


;;
;; editor/evil

(when (featurep! :editor evil)
  (load! "+evil"))


;;
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


;;
;; editor/format

(when (featurep! :editor format)
  (setq +format-on-save-enabled-modes '(c-mode
                                        c++-mode
                                        go-mode
                                        rustic-mode)
        +format-preserve-indentation nil))


;;
;; editor/multiple-cursors

(when (featurep! :editor multiple-cursors)
  (map! :nv "C-n" #'evil-mc-make-and-goto-next-match
        :nv "C-p" #'evil-mc-make-and-goto-prev-match))


;;
;; emacs/vc

(when (featurep! :emacs vc)
  (defun +vc--magit-clone-default-directory (url-or-repo)
    (and (string-match "\\([^/:]+\\)/\\(.+\\)$" url-or-repo)
         (format "~/projects/%s/" (match-string 1 url-or-repo))))
  (setq magit-clone-default-directory #'+vc--magit-clone-default-directory
        magit-clone-set-remote\.pushDefault t
        magit-refresh-status-buffer nil
        magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                                 ("Version" 25 magit-repolist-column-version nil)
                                 ("Path" 99 magit-repolist-column-path nil))
        magit-revision-insert-related-refs nil)

  (after! magit
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'server-switch-hook 'magit-commit-diff)))


;;
;; tools/docker

(when (featurep! :tools docker)
  (after! docker-tramp
    (setq docker-tramp-use-names t)))


;;
;; tools/eval

(when (featurep! :tools eval +overlay)
  (setq eros-eval-result-prefix "⇒ "))


;;
;; tools/lsp

(when (featurep! :tools lsp)
  (setq gc-cons-threshold (* 1024 1024 1024)
        +lsp-company-backend 'company-capf
        lsp-signature-render-documentation nil)

  (after! lsp-mode
    (setq-hook! 'lsp-mode-hook
      company-idle-delay 0.0
      company-minimum-prefix-length 1))

  (after! lsp-ui
    (setq lsp-ui-sideline-show-diagnostics nil))

  ;; LSP + Doom Themes
  (after! (lsp-ui doom-themes)
    (setq lsp-ui-imenu-colors `(,(doom-color 'dark-blue)
                                ,(doom-color 'cyan)))))


;;
;; tools/magit

(when (featurep! :tools magit)
  (after! transient
    (map! :map (transient-map transient-edit-map)
          :g "<escape>" 'transient-quit-one)
    (map! :map transient-sticky-map
          :g "<escape>" 'transient-quit-seq)))


;;
;; lang/cc

;; Enforce Google C++ Style Guide
;; See https://google.github.io/styleguide/cppguide.html
(when (featurep! :lang cc)
  (set-formatter! 'clang-format
    '("clang-format"
      ("-assume-filename=%s" (or buffer-file-name mode-result ""))
      ("-style=Google"))
    :modes '((c-mode ".c")
             (c++-mode ".cpp")))
  (setq-hook! '(c-mode-hook c++-mode-hook)
    tab-width 2
    fill-column 80))


;;
;; lang/go

(when (and (featurep! :lang go)
           (featurep! :checkers syntax))
  (set-next-checker! 'go-mode 'lsp 'go-gofmt))


;;
;; lang/java

;; Enforce Google Java Code Style
;; See https://google.github.io/styleguide/javaguide.html
(when (featurep! :lang java)
  (when (featurep! :lang java +lsp)
    (setq lsp-java-format-settings-url "http://google.github.io/styleguide/eclipse-java-google-style.xml"))
  (set-formatter! 'google-java-format
    '("google-java-format" "-")
    :modes 'java-mode)
  (setq-hook! 'java-mode-hook
    tab-width 2
    fill-column 100))

(when (featurep! :lang java +lsp)
  (setq lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled nil
        lsp-java-selection-enabled nil
        lsp-java-code-generation-use-blocks t
        lsp-java-code-generation-generate-comments t
        lsp-java-code-generation-to-string-code-style "STRING_BUILDER")

  ;; Lombok support
  ;; See https://github.com/redhat-developer/vscode-java/wiki/Lombok-support
  (after! lsp-java
    (push (concat "-javaagent:"
                  (expand-file-name (concat doom-private-dir
                                            "etc/lombok/lombok-1.18.12.jar")))
          lsp-java-vmargs))

  ;;
  ;; Groovy
  (setq lsp-groovy-server-file (concat doom-private-dir
                                       "etc/lsp/lsp-groovy/groovy-language-server-all.jar"))
  (add-hook 'groovy-mode-local-vars-hook #'lsp!))


;;
;; lang/javascript

(when (and (featurep! :lang javascript)
           (featurep! :checkers syntax))
  (after! flycheck
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'javascript-standard))

  (when (featurep! :lang javascript +lsp)
    (set-next-checker! 'js2-mode  'lsp 'javascript-eslint)
    (set-next-checker! 'rjsx-mode 'lsp 'javascript-eslint)
    (set-next-checker! 'typescript-mode 'lsp 'javascript-eslint)))


;;
;; lang/kotlin

(when (featurep! :lang kotlin +lsp)
  (setq lsp-clients-kotlin-server-executable (concat doom-private-dir
                                                     "etc/lsp/lsp-kotlin/bin/kotlin-language-server")))


;;
;; lang/markdown

(when (featurep! :lang markdown)
  (after! markdown-mode
    (setq markdown-header-scaling t)

    (set-company-backend! '(markdown-mode gfm-mode) '(company-emoji))))


;;
;; lang/org

(when (featurep! :lang org)
  (load! "+org"))
(when (featurep! :lang org +jupyter)
  (load! "+org-jupyter"))


;;
;; lang/python

(when (and (featurep! :lang python +lsp)
           (featurep! :checkers syntax))
  (set-next-checker! 'python-mode 'lsp 'python-flake8))


;;
;; lang/ruby

(when (and (featurep! :lang ruby +lsp)
           (featurep! :checkers syntax))
  (set-next-checker! 'ruby-mode     'lsp 'ruby-rubocop)
  (set-next-checker! 'enh-ruby-mode 'lsp 'ruby-rubocop))


;;
;; lang/rust

(when (featurep! :lang rust +lsp)
  (setq rustic-lsp-server 'rust-analyzer
        lsp-rust-server 'rust-analyzer
        lsp-rust-clippy-preference "on"))

;;
;; lang/solidity

(when (featurep! :lang solidity)
  (setq solidity-flycheck-solc-checker-active t
        solidity-flycheck-solium-checker-active t)
  (after! solidity-mode
    (set-company-backend! 'solidity-mode
      '(company-yasnippet :separate company-solidity))))


;;
;; lang/sh

(when (featurep! :lang sh)
  ;; Enforce Google Shell Style Guide
  ;; See https://google.github.io/styleguide/shellguide.html
  (set-formatter! 'shfmt
    '("shfmt" "-i" "2" "-ci")
    :modes 'sh-mode)
  (setq-hook! 'sh-mode-hook
    tab-width 2
    fill-column 80))


;;
;; lang/web

(when (featurep! :lang web)
  (after! emmet-mode
    (setq emmet-self-closing-tag-style " /")))


;;
;; vimrc-mode

(after! vimrc-mode
  (add-hook 'vimrc-mode-local-vars-hook #'lsp!))
