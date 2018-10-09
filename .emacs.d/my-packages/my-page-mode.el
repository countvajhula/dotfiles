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
  ("n" my-scroll-down "down")
  ("p" my-scroll-up "up")
  ("j" evil-scroll-line-down "down fine")
  ("k" evil-scroll-line-up "up fine")
  ("h" evil-scroll-page-up "page up")
  ("l" evil-scroll-page-down "page down")
  ("H" evil-goto-first-line "beginning")
  ("L" evil-goto-line "end")
  ("K" my-scroll-skip-up "skip up")
  ("J" my-scroll-skip-down "skip down")
  ("u" my-scroll-half-page-up "leap up")
  ("d" my-scroll-half-page-down "leap down")
  ("i" my-noop "exit" :exit t)
  ("<escape>" my-noop "exit" :exit t))

(global-set-key (kbd "s-g") 'hydra-page/body)

(provide 'my-page-mode)
