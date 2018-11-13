
(defhydra hydra-symex (:idle 1.0
                       :columns 2
                       :body-pre (evil-symex-state)
                       :post (evil-normal-state))
  "SymEx mode"
  ("h" evil-window-left "left")
  ("j" evil-window-down "down")
  ("k" evil-window-up "up")
  ("l" evil-window-right "right")
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-word/body "enter lower level" :exit t)
  ("s-<escape>" hydra-view/body "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)

(provide 'my-symex-mode)
