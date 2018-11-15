;;; TODO: enter should visit numbers and symbols too
;;; TODO: ideally, would be good to have a simple POC of the AST
;;; to operate on, via semantic?
;;; TODO: move f/b/out/in, info, yank

(defun my-evaluate-symex ()
  "Evaluate Symex"
  (interactive)
  (save-excursion
    (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-eval-symex))
          ((member major-mode elisp-modes)
           (my-elisp-eval-symex))
          ((equal major-mode 'scheme-mode)
           (my-scheme-eval-symex))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun my-forward-symex ()
  "Forward symex"
  (interactive)
  (forward-sexp 2)
  (backward-sexp 1))

(defun my-backward-symex ()
  "Backward symex"
  (interactive)
  (backward-sexp 1))

(defun my-enter-symex ()
  "Enter lower symex level"
  (interactive)
  (lispy-flow 1))

(defun my-exit-symex ()
  "Exit to higher symex level"
  (interactive)
  (lispy--out-backward 1))

(defhydra hydra-symex (:idle 1.0
                       :columns 2
                       :body-pre (evil-symex-state)
                       :post (evil-normal-state))
  "Symex mode"
  ("h" my-backward-symex "left")
  ("j" my-exit-symex "enter")
  ("k" my-enter-symex "exit")
  ("l" my-forward-symex "right")
  ("e" my-evaluate-symex "evaluate")
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-word/body "enter lower level" :exit t)
  ("s-<escape>" hydra-view/body "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)

(provide 'my-symex-mode)
