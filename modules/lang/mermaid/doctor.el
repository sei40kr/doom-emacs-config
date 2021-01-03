;;; $DOOMDIR/modules/lang/mermaid/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "mmdc")
  (warn! "mermaid-cli isn't installed."))
