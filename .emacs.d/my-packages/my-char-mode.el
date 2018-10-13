(defun my-char-info ()
  "Info on character"
  (interactive)
  (what-cursor-position))

(defun my-delete-char ()
  "Delete character"
  (interactive)
  (evil-delete-char (point)
                    (+ (point) 1)
                    (quote exclusive) nil))

(defun my-move-char-left ()
  "Move character left"
  (interactive)
  (my-delete-char)
  (evil-backward-char)
  (evil-paste-before nil nil))

(defun my-move-char-right ()
  "Move character right"
  (interactive)
  (my-delete-char)
  (evil-paste-after nil nil))

(defun my-move-char-down ()
  "Move character down"
  (interactive)
  (my-delete-char)
  (evil-next-line)
  (evil-paste-before nil nil))

(defun my-move-char-up ()
  "Move character up"
  (interactive)
  (my-delete-char)
  (evil-previous-line)
  (evil-paste-before nil nil))

(defun my-move-char-far-left ()
  "Move character far left"
  (interactive)
  (my-delete-char)
  (evil-beginning-of-line)
  (evil-paste-before nil nil))

(defun my-move-char-very-bottom ()
  "Move character to the bottom"
  (interactive)
  (my-delete-char)
  (evil-goto-line)
  (evil-paste-before nil nil))

(defun my-move-char-very-top ()
  "Move character to the top"
  (interactive)
  (my-delete-char)
  (evil-goto-first-line)
  (evil-paste-before nil nil))

(defun my-move-char-far-right ()
  "Move character far right"
  (interactive)
  (my-delete-char)
  (evil-end-of-line)
  (evil-paste-after nil nil))

(defun my-change-char ()
  "Change character"
  (interactive)
  (evil-substitute (point)
                   (+ (point) 1)
                   (quote exclusive)
                   nil))

(defun my-yank-char ()
  "Yank (copy) character"
  (interactive)
  (evil-yank-characters (point) (+ (point) 1)))

(defhydra hydra-char (:idle 1.0
                      :columns 4)
  "Character mode"
  ("h" my-move-char-left "left")
  ("j" my-move-char-down "down")
  ("k" my-move-char-up "up")
  ("l" my-move-char-right "right")
  ("H" my-move-char-far-left "move to far left")
  ("J" my-move-char-very-bottom "move to bottom")
  ("K" my-move-char-very-top "move to top")
  ("L" my-move-char-far-right "move to far right")
  ("c" my-change-char "change character" :exit t)
  ("y" my-yank-char "yank (copy)" :exit t)
  ("x" my-delete-char "delete char")
  ("i" my-char-info "info" :exit t)
  ("<escape>" my-noop "exit" :exit t))

(global-set-key (kbd "s-x") 'hydra-char/body)

(provide 'my-char-mode)
