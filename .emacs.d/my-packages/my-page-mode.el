;; a mode for navigating pages

(defun my-scroll-half-page-up ()
  (interactive)
  (evil-scroll-line-up (/ (window-total-height) 2)))

(defun my-scroll-half-page-down ()
  (interactive)
  (evil-scroll-line-down (/ (window-total-height) 2)))

(defun my-scroll-skip-up ()
  (interactive)
  (evil-scroll-line-up 9))

(defun my-scroll-skip-down ()
  (interactive)
  (evil-scroll-line-down 9))

(defhydra hydra-page (:idle 1.0
                      :columns 6)
  "Page mode"
  ("j" my-scroll-down "down")
  ("k" my-scroll-up "up")
  ("J" evil-scroll-line-down "down fine")
  ("K" evil-scroll-line-up "up fine")
  ("h" evil-scroll-page-up "page up")
  ("l" evil-scroll-page-down "page down")
  ("H" evil-goto-first-line "beginning")
  ("L" evil-goto-line "end")
  ("C-k" my-scroll-skip-up "skip up")
  ("C-j" my-scroll-skip-down "skip down")
  ("u" my-scroll-half-page-up "leap up")
  ("d" my-scroll-half-page-down "leap down")
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-line/body "enter lower level" :exit t)
  ("s-<escape>" hydra-window/body "escape to higher level" :exit t))

(global-set-key (kbd "s-g") 'hydra-page/body)

(provide 'my-page-mode)
