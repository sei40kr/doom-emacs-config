;;; $DOOMDIR/modules/tools/ghq/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "ghq")
  (warn! "Couldn't find ghq executable"))
