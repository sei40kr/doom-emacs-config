;;; editor/spacemacs-editing/autoload/hydra.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +spacemacs-editing/er-reset ()
  (interactive)
  (er/expand-region 0))

;;;###autoload (autoload '+spacemacs-editing/expansions/body "editor/spacemacs-editing/autoload/hydra" t)
(defhydra +spacemacs-editing/expansions
  (:hint nil)
  "
                                [expansions]
     Expansion^^      Actions
  ╭──────────────────────────────────╯
     [_v_]  expand    [_/_] project
     [_V_]  contract  [_b_] buffer
     [_r_]  reset      ╭──────────────────────
     ^^                │ [_q_] quit"
  ("v" er/expand-region)
  ("V" er/contract-region)
  ("r" +spacemacs-editing/er-reset)
  ("/" +default/search-project :exit t)
  ("f" +ivy/projectile-find-file :exit t)
  ("b" +default/search-buffer :exit t)
  ("q" nil :exit t))
