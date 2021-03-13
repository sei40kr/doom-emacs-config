;;; $DOOMDIR/contrib/go.el -*- lexical-binding: t; -*-

(def-project-mode! +web-go-template-mode
  :modes '(web-mode)
  :files ("go.mod")
  :on-enter
  (when (eq major-mode 'web-mode)
    (web-mode-set-engine "go")))

(def-project-mode! +web-hugo-mode
  :modes '(web-mode)
  :files (or (and "content/" "config.toml")
             (and "layouts/" "theme.toml"))
  :on-enter
  (when (eq major-mode 'web-mode)
    (web-mode-set-engine "hugo")))

(set-formatter! 'go-template-prettier
  '(("%s" (or (executable-find "prettier") "prettier"))
    "--parser" "go-template"
    ("--stdin-filepath" "%s" (or buffer-file-name ".html")))
  :modes '((web-mode (or (bound-and-true-p +web-go-template-mode)
                         (bound-and-true-p +web-hugo-mode)))))
