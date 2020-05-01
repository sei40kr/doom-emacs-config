;;; $DOOMDIR/+atcoder-tools.el -*- lexical-binding: t; -*-

(map! :map (rustic-mode-map python-mode-map)
      :localleader
      :prefix ("a" . "atcoder-tools")
      :desc "submit" "s" (λ! (compile "atcoder-tools submit")))
