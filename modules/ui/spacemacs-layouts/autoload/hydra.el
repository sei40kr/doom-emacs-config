;;; $DOOMDIR/modules/spacemacs/spacemacs-layouts/autoload/hydra.el -*- lexical-binding: t; -*-

(defhydra +spacemacs-layouts/workspaces
  (:pre (+workspace/display)
   :hint nil)
  "
                                                      [workspaces]
  Go to^^^^                   Actions^^
  ╭────────────────────────────────────────────────────────╯
     [_0_.._8_]^^     nth       [_c_/_C_] new
     [_C-0_.._C-8_]^^ nth       [_d_]^^   delete
     [_<tab>_]^^^^    final     [_l_]^^   load
     [_n_/_C-l_]^^    next      [_R_]^^   rename
     [_N_/_p_/_C-h_]  previous  [_s_]^^   save
     [_e_]^^^^        select    [_<_/_>_] move left/right
                                            ╭─────────────────────
                                            │ [_q_] quit"
  ("0" +workspace/switch-to-0)
  ("1" +workspace/switch-to-1)
  ("2" +workspace/switch-to-2)
  ("3" +workspace/switch-to-3)
  ("4" +workspace/switch-to-4)
  ("5" +workspace/switch-to-5)
  ("6" +workspace/switch-to-6)
  ("7" +workspace/switch-to-7)
  ("8" +workspace/switch-to-8)
  ("C-0" +workspace/switch-to-0)
  ("C-1" +workspace/switch-to-1)
  ("C-2" +workspace/switch-to-2)
  ("C-3" +workspace/switch-to-3)
  ("C-4" +workspace/switch-to-4)
  ("C-5" +workspace/switch-to-5)
  ("C-6" +workspace/switch-to-6)
  ("C-7" +workspace/switch-to-7)
  ("C-8" +workspace/switch-to-8)
  ("TAB" +workspace/switch-to-final)
  ("RET" nil :exit t)
  ("<tab>" +workspace/switch-to-final)
  ("<return>" nil :exit t)
  ("e" +workspace/switch-to :exit t)
  ("n" +workspace/switch-right)
  ("C-l" +workspace/switch-right)
  ("N" +workspace/switch-left)
  ("p" +workspace/switch-left)
  ("C-h" +workspace/switch-left)
  ("c" +workspace/new :exit t)
  ("C" +workspace/new)
  ("d" +workspace/delete)
  ("l" +workspace/load :exit t)
  ("R" +workspace/rename :exit t)
  ("s" +workspace/save :exit t)
  ("<" +workspace/swap-left)
  (">" +workspace/swap-right)
  ("q" nil :exit t))
