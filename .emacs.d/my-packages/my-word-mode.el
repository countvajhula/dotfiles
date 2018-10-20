(defun my-transpose-word-backward ()
  "Transpose word backwards"
  (interactive)
  (evil-backward-word-begin nil)
  (transpose-words 1)
  (evil-backward-word-begin nil))

(defun my-transpose-word-forward ()
  "Transpose word forward"
  (interactive)
  (transpose-words 1))

(defhydra hydra-word (:idle 1.0
                      :columns 2)
  "Word mode"
  ("h" evil-backward-word-begin "backward")
  ;; ("j" evil-next-line "down")
  ;; ("k" evil-previous-line "up")
  ("l" evil-forward-word-begin "forward")
  ("H" evil-first-non-blank "beginning")
  ("L" evil-last-non-blank "end")
  ("C-h" my-transpose-word-backward "move left")
  ("C-l" my-transpose-word-forward "move right")
  ("i" my-noop "exit" :exit t)
  ("<escape>" my-noop "exit" :exit t))

(global-set-key (kbd "s-r") 'hydra-word/body)

(provide 'my-word-mode)
