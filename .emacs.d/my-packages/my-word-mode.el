(defun my-move-word-backward ()
  "Move word backwards"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (evil-backward-WORD-begin nil)
  (transpose-words 1)
  (evil-backward-WORD-begin 2))

(defun my-move-word-forward ()
  "Move word forward"
  (interactive)
  (evil-forward-WORD-begin nil)
  (transpose-words 1))

(defun my-move-word-down ()
  "Move word down"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (setq word-end-position (point))
  (evil-backward-WORD-begin nil)
  (evil-delete (point) word-end-position 'exclusive nil nil)
  (evil-next-line)
  (evil-paste-before nil nil))

(defun my-move-word-up ()
  "Move word up"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (setq word-end-position (point))
  (evil-backward-WORD-begin nil)
  (evil-delete (point) word-end-position 'exclusive nil nil)
  (evil-previous-line)
  (evil-paste-before nil nil))

(defun my-delete-word ()
  "Delete word"
  (interactive)
  (apply 'evil-delete (evil-inner-word)))

(defhydra hydra-word (:idle 1.0
                      :columns 2)
  "Word mode"
  ("h" evil-backward-WORD-begin "backward")
  ("j" evil-next-line "down")
  ("k" evil-previous-line "up")
  ("l" evil-forward-WORD-begin "forward")
  ("H" evil-first-non-blank "beginning")
  ("L" evil-last-non-blank "end")
  ("C-h" my-move-word-backward "move left")
  ("C-l" my-move-word-forward "move right")
  ("C-j" my-move-word-down "move down")
  ("C-k" my-move-word-up "move up")
  ("x" my-delete-word "delete")
  ("d" dictionary-lookup-definition "lookup in dictionary" :exit t)
  ("?" dictionary-lookup-definition "lookup in dictionary" :exit t)
  ("i" my-noop "exit" :exit t)
  ("<escape>" my-noop "exit" :exit t))

(global-set-key (kbd "s-r") 'hydra-word/body)

(provide 'my-word-mode)
