;;; $DOOMDIR/modules/completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+corfu/cape-dict-or-keywords "completion/corfu/autoload" nil t)
(fset '+corfu/cape-dict-or-keywords (cape-super-capf #'cape-dict #'cape-keyword))
