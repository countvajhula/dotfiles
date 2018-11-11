
(use-package ace-window
  :config
  (setq aw-keys '(?h ?j ?k ?l ?g ?f ?d ?s ?a)))

(use-package winner
  :config
  (winner-mode t))

;; Evil provides some good window navigation functionality, but these
;; bindings aren't available in Emacs state and also consequently in
;; Insert state if the insert mode keymap is overridden in favor of
;; Emacs native bindings. Additionally, for the most common window
;; operations, some of the evil mode defaults could be further
;; improved.  This package provides evil window navigation globally as
;; a Vim-style "mode," implemented using a hydra, and also overrides
;; some defaults to make them faster or more useful/intuitive.

(defhydra hydra-window (:idle 1.0
                        :columns 4
                        :body-pre (evil-window-state)
                        :post (evil-normal-state))
  "Window mode"
  ("h" evil-window-left "left")
  ("j" evil-window-down "down")
  ("k" evil-window-up "up")
  ("l" evil-window-right "right")
  ("H" evil-window-move-far-left "move to far left")
  ("J" evil-window-move-very-bottom "move to bottom")
  ("K" evil-window-move-very-top "move to top")
  ("L" evil-window-move-far-right "move to far right")
  ("s-c" evil-window-delete "delete")
  ("c" evil-window-delete)
  ("s-x" evil-window-delete)
  ("x" evil-window-delete)
  ("s-o" evil-window-mru "Jump to most recent (like Alt-Tab)" :exit t)
  ("o" evil-window-mru :exit t)
  ("n" other-window "next")
  ("s-w" delete-other-windows "maximize" :exit t)
  ("w" delete-other-windows :exit t)
  ("s-s" evil-window-split "split horizontally")
  ("s" evil-window-split "")
  ("_" evil-window-split "")
  ("s-v" evil-window-vsplit "split vertically")
  ("u" winner-undo "undo")
  ("C-r" winner-redo "redo")
  ("v" evil-window-vsplit "")
  ("|" evil-window-vsplit "")
  ("/" ace-window "search")
  ("+" evil-window-increase-height "expand vertically")
  ("-" evil-window-decrease-height "shrink vertically")
  (">" evil-window-increase-width "expand horizontally")
  ("<" evil-window-decrease-width "shrink horizontally")
  ("=" balance-windows "balance")
  ("r" evil-window-rotate-downwards "rotate downwards")
  ("R" evil-window-rotate-upwards "rotate upwards")
  ("s-f" ffap-other-window "go to file in other window" :exit t)
  ("f" ffap-other-window "" :exit t)
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-view/body "enter lower level" :exit t)
  ("s-<escape>" hydra-buffer/body "escape to higher level" :exit t))

(global-set-key (kbd "s-w") 'hydra-window/body)

(provide 'my-window-mode)
