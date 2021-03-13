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
(if (eq system-type 'darwin)
    (setq doom-font (font-spec :family "Operator Mono Book" :size 17.0)
          doom-variable-pitch-font (font-spec :family "ヒラギノ角ゴシック W3" :size 15.5))
  (setq doom-font (font-spec :family "Operator Mono Medium" :size 14.0)
        doom-variable-pitch-font (font-spec :family "sans-serif" :size 12.0))
  (custom-set-faces! '(font-lock-comment-face :family "Operator Mono Medium")))

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
(load! "nix-variables.el" doom-private-dir nil)

(global-auto-revert-mode +1)
(global-subword-mode +1)


;;
;; core

(load! "projectile-magit")


;;
;; completion/company

(when (featurep! :completion company)
  (setq company-box-doc-enable nil)

  (after! company
    (map! :map company-active-map
          "TAB" nil
          [tab] nil
          [backtab] nil)

    (when (featurep! :editor evil)
      (add-hook 'evil-normal-state-entry-hook #'company-abort))))


;;
;; ui/doom

(when (featurep! :ui doom)
  (after! doom-themes
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
  (load! "contrib/tabs"))


;;
;; ui/treemacs

(when (featurep! :ui treemacs)
  (setq treemacs-collapse-dirs 3)

  (load! "contrib/evil-treemacs")

  ;; Treemacs + Doom Themes
  (after! (treemacs doom-themes)
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
  (setq +evil-want-o/O-to-continue-comments nil)

  ;; retain visual-mode on selection shift
  (after! evil
    (evil-set-command-property 'evil-shift-left  :keep-visual t)
    (evil-set-command-property 'evil-shift-right :keep-visual t)))


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
                                        haskell-mode
                                        nix-mode
                                        rustic-mode)
        +format-preserve-indentation nil)
  (setq-hook! '(haskell-mode
                js2-mode-hook rjsx-mode-hook
                typescript-mode-hook typescript-tsx-mode-hook)
    +format-with-lsp nil))


;;
;; editor/multiple-cursors

(when (featurep! :editor multiple-cursors)
  (map! :nv "C-n" #'evil-mc-make-and-goto-next-match
        :nv "C-p" #'evil-mc-make-and-goto-prev-match))


;; editor/snippets

(after! yasnippet
  (competitive-programming-snippets-init)
  (gitignore-snippets-init)
  (license-snippets-init))


;;
;; emacs/vc

(when (featurep! :emacs vc)
  (load! "contrib/magit"))


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
;; tools/lookup

(setq +lookup-open-url-fn #'eww)


;;
;; tools/lsp

(when (featurep! :tools lsp)
  (setq lsp-signature-render-documentation nil
        lsp-ui-sideline-show-diagnostics nil)

  ;; LSP + Doom Themes
  (after! (lsp-ui doom-themes)
    (setq lsp-ui-imenu-colors `(,(doom-color 'dark-blue)
                                ,(doom-color 'cyan))))

  (when (featurep! :lang go +lsp)
    (setq-hook! 'go-mode-hook
      flycheck-disabled-checkers '(go-build)))
  (when (featurep! :lang ess +lsp)
    (setq-hook! 'ess-r-mode-hook
      flycheck-disabled-checkers '(r-lintr)))
  (when (featurep! :lang python +lsp)
    (add-hook! 'python-mode-hook
               :local
               (add-to-list 'flycheck-disabled-checkers 'python-pycompile)
               (when lsp-pyls-plugins-flake8-enabled
                 (add-to-list 'flycheck-disabled-checkers 'python-flake8))
               (when lsp-pyls-plugins-pylint-enabled
                 (add-to-list 'flycheck-disabled-checkers 'python-pylint))))
  (when (featurep! :lang rust +lsp)
    (add-hook! 'rustic-mode-hook
               :local
               (add-to-list 'flycheck-disabled-checkers 'rust-cargo)
               (when (string-equal lsp-rust-clippy-preference "on")
                 (add-to-list 'flycheck-disabled-checkers 'rust-clippy)))))



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

(setq lsp-go-use-placeholders nil)
(load! "contrib/go-template")


;;
;; lang/haskell

(when (featurep! :lang haskell +lsp)
  (after! flycheck
    (pushnew! flycheck-disabled-checkers 'haskell-ghc 'haskell-stack-ghc)))


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

(when (featurep! :lang javascript)
  ;; Disable legacy checkers
  (after! flycheck
    (pushnew! flycheck-disabled-checkers 'javascript-jshint 'javascript-standard))

  ;; Prefer eslint_d to ESLint
  ;; See https://github.com/mantoni/eslint_d.js
  (add-hook! '(js2-mode-hook
               rjsx-mode-hook
               typescript-mode-hook
               typescript-tsx-mode-hook)
             :append
             (defun +javascript--flycheck-prefer-eslint_d-h ()
               (when (executable-find "eslint_d")
                 (setq-local flycheck-javascript-eslint-executable "eslint_d")))))


;;
;; lang/markdown

(when (featurep! :lang markdown)
  (setq markdown-header-scaling nil
        markdown-enable-wiki-links nil))


;;
;; lang/org

(when (featurep! :lang org)
  (load! "contrib/org"))
(when (featurep! :lang org +jupyter)
  (load! "contrib/org-jupyter"))


;;
;; lang/plantuml

(setq plantuml-default-exec-mode 'jar)


;;
;; lang/python

(when (featurep! :lang python +lsp)
  (add-hook! 'python-mode-local-vars-hook
    (when (bound-and-true-p lsp-mode)
      (pushnew! flycheck-disabled-checkers 'python-pycompile)
      (when lsp-pyls-plugins-flake8-enabled
        (pushnew! flycheck-disabled-checkers 'python-flake8))
      (when lsp-pyls-plugins-pylint-enabled
        (pushnew! flycheck-disabled-checkers 'python-pylint)))))


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


;; lang/terraform

(when (featurep! :lang terraform)
  (set-docsets! 'terraform-mode "Terraform"))


;;
;; lang/web

(when (featurep! :lang web)
  (after! emmet-mode
    (setq emmet-self-closing-tag-style " /")))


;;
;; vimrc-mode

(after! vimrc-mode
  (add-hook 'vimrc-mode-local-vars-hook #'lsp!))
