(defun my-evaluate-symex ()
  "Evaluate Symex"
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (my-racket-eval-symex))
        ((member major-mode elisp-modes)
         (my-elisp-eval-symex))
        ((equal major-mode 'scheme-mode)
         (my-scheme-eval-symex))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defhydra hydra-symex (:idle 1.0
                       :columns 2
                       :body-pre (evil-symex-state)
                       :post (evil-normal-state))
  "Symex mode"
  ("h" backward-sexp "left")
  ("j" evil-window-down "down")
  ("k" evil-window-up "up")
  ("l" forward-sexp "right")
  ("e" my-evaluate-symex "evaluate")
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-word/body "enter lower level" :exit t)
  ("s-<escape>" hydra-view/body "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)

(provide 'my-symex-mode)
