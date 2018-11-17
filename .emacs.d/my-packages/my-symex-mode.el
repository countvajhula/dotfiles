;;; TODO: ideally, would be good to have a simple POC of the AST
;;; to operate on, via semantic?
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

(defun lispy-comment-line-p ()
  "Checks if we're currently at the start of a comment line."
  (and (lispy-bolp)
       (looking-at ";")))

(defun lispy-empty-list-p ()
  "Checks if we're looking at an empty list."
  (save-excursion
    (and (lispy-left-p)
         (progn (forward-char 2) ;; need to go forward by 2 for some reason
                (lispy-right-p)))))

(defun my-forward-symex ()
  "Forward symex"
  (interactive)
  (if (lispy-comment-line-p)
      (forward-sexp 1)
    (forward-sexp 2))
  (backward-sexp 1))

(defun my-backward-symex ()
  "Backward symex"
  (interactive)
  (backward-sexp 1))

(defun my-enter-symex ()
  "Enter lower symex level."
  (interactive)
  (cond ((lispy-comment-line-p)
         (lispy-flow 1))
        ((and (lispy-left-p)
              (not (lispy-empty-list-p)))
         (forward-char))))

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

(defun my-delete-symex ()
  "Delete symex"
  (interactive)
  (if (lispy-left-p)
      (lispy-delete 1)
    (kill-sexp 1)))

(defun my-change-symex ()
  "Change symex"
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun my-barf-backward ()
  "Barf backward"
  (interactive)
  (save-excursion
    (my-enter-symex) ;; need to be inside the symex to barf and slurp
    (lispy-backward-barf-sexp 1)))

(defun my-barf-forward ()
  "Barf forward"
  (interactive)
  (save-excursion
    (my-enter-symex)  ;; need to be inside the symex to barf and slurp
    (lispy-forward-barf-sexp 1)))

(defun my-slurp-backward ()
  "Slurp from behind"
  (interactive)
  (save-excursion
    (my-enter-symex)  ;; need to be inside the symex to barf and slurp
    (lispy-backward-slurp-sexp 1)))

(defun my-slurp-forward ()
  "Slurp from the front"
  (interactive)
  (save-excursion
    (my-enter-symex)  ;; need to be inside the symex to barf and slurp
    (lispy-forward-slurp-sexp 1)))

(defhydra hydra-symex (:idle 1.0
                       :columns 2
                       :body-pre (progn (my-select-nearest-symex)
                                        (evil-symex-state))
                       :post (evil-normal-state))
  "Symex mode"
  ("h" my-backward-symex "previous")
  ("k" my-backward-symex "previous")
  ("J" my-exit-symex "exit")
  ("H" my-exit-symex "exit")
  ("K" my-enter-symex "enter")
  ("L" my-enter-symex "enter")
  ("j" my-forward-symex "next")
  ("l" my-forward-symex "next")
  ("f" lispy-flow "flow forward")
  ("y" lispy-new-copy "yank (copy)")
  ("x" my-delete-symex "delete")
  ("c" my-change-symex "change" :exit t)
  ("C-h" lispy-move-up "move backward")
  ("C-k" lispy-move-up "move backward")
  ("C-j" lispy-move-down "move forward")
  ("C-l" lispy-move-down "move forward")
  ("C-S-s-j" lispy-raise "raise")
  ("C-S-h" my-slurp-backward "slurp backward")
  ("C-S-j" my-barf-backward "barf backward")
  ("C-S-k" my-barf-forward "barf forward")
  ("C-S-l" my-slurp-forward "slurp backward")
  ("e" my-evaluate-symex "evaluate")
  ("E" my-evaluate-definition "evaluate definition")
  ("d" my-evaluate-definition)
  ("s" lispy-split "split")
  ("N" lispy-join "join")
  ("0" my-first-symex "first symex")
  ("H-h" my-first-symex "first symex")
  ("$" my-last-symex "last symex")
  ("H-l" my-last-symex "last symex")
  ("?" my-describe-symex "info")
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-word/body "enter lower level" :exit t)
  ("s-<escape>" hydra-view/body "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)  ;; since y looks like inverted lambda
(global-set-key (kbd "s-;") 'hydra-symex/body)  ;; since y is hard to reach

(provide 'my-symex-mode)
