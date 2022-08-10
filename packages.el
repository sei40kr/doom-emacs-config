;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! expand-region :pin "0fa7c2d349")
(package! package-lint :pin "05596996286089acc7693b700c7c31780439e39f")
(package! vimrc-mode :pin "13bc150a87")

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))
(package! atcoder-problems :recipe (:local-repo "repos/atcoder-problems"))
(package! atcoder-tools
  :recipe (:local-repo "repos/atcoder-tools"))
(package! competitive-programming-snippets
  :recipe (:local-repo "repos/competitive-programming-snippets"
           :files (:defaults "snippets")))
(package! gitignore-snippets
  :recipe (:local-repo "repos/gitignore-snippets"
           :files (:defaults "snippets")))
(package! kaggle :recipe (:local-repo "repos/kaggle"))
(package! license-snippets
  :recipe (:local-repo "repos/license-snippets"
           :files (:defaults "snippets")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)
(package! ddskk :disable t)
(package! drag-stuff :disable t)
(package! evil-markdown :disable t)
(package! osx-trash :disable t)
(package! pangu-spacing :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))
(package! emacsql-sqlite :built-in t)
(package! libgit :built-in t)
(package! vterm :built-in t)
(package! zmq :built-in t)

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")
(package! doom-themes :pin "56e8a93b2dd8f2c10a693f36c3317833211201f2")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
