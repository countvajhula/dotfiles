;;; TODO: enter should visit numbers and symbols too
;;; TODO: ideally, would be good to have a simple POC of the AST
;;; to operate on, via semantic?
;;; TODO: move f/b/out/in, info, yank
(use-package lispy)

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

(defun my-evaluate-definition ()
  "Evaluate top-level definition"
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-send-definition nil))
        ((member major-mode elisp-modes)
         (eval-defun nil))
        ((equal major-mode 'scheme-mode)
         (geiser-eval-definition nil))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

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

(defun my-select-nearest-symex ()
  "Select symex nearest to point"
  (interactive)
  (condition-case nil
      (progn (my-forward-symex)
             (my-backward-symex))
    (error (condition-case nil
               (progn (my-backward-symex)
                      (my-forward-symex))
             (error (condition-case nil
                        (my-backward-symex)
                      (error nil)))))))

(defun my-describe-symex ()
  "Lookup doc on symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
    (lispy-describe)))

(defun my-first-symex ()
  "Select first symex at present level"
  (interactive)
  (let ((previous-position (point)))
    (my-backward-symex)
    (setq current-position (point))
    (while (not (= previous-position current-position))
      (setq previous-position current-position)
      (my-backward-symex)
      (setq current-position (point)))))

(defun my-last-symex ()
  "Select last symex at present level"
  (interactive)
  (let ((previous-position (point)))
    (my-forward-symex)
    (setq current-position (point))
    (while (not (= previous-position current-position))
      (setq previous-position current-position)
      (my-forward-symex)
      (setq current-position (point)))))

(defhydra hydra-symex (:idle 1.0
                       :columns 2
                       :body-pre (progn (my-select-nearest-symex)
                                        (evil-symex-state))
                       :post (evil-normal-state))
  "Symex mode"
  ("h" my-backward-symex "left")
  ("j" my-exit-symex "enter")
  ("k" my-enter-symex "exit")
  ("l" my-forward-symex "right")
  ("C-h" lispy-move-up "move backward")
  ("C-l" lispy-move-down "move forward")
  ("C-j" lispy-raise "raise")
  ("e" my-evaluate-symex "evaluate")
  ("E" my-evaluate-definition "evaluate definition")
  ("J" lispy-join "join")
  ("0" my-first-symex "first symex")
  ("H" my-first-symex "first symex")
  ("$" my-last-symex "last symex")
  ("L" my-last-symex "last symex")
  ("?" my-describe-symex "info")
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-word/body "enter lower level" :exit t)
  ("s-<escape>" hydra-view/body "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)

(provide 'my-symex-mode)
