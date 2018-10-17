(defun my-describe-symbol ()
  "Describe symbol at point"
  (interactive)
  (describe-symbol (symbol-at-point)))

(defun my-eval-exp-or-region ()
  "Eval region or last sexp"
  (interactive)
  (if mark-active
      (progn (eval-region (region-beginning) (region-end))
             (deactivate-mark)
             (message "Evaluated region."))
    (eval-last-sexp nil)))


(defun my-eval (what)
  "Evaluate something"
  (interactive "cwhat?")
  (cond ((equal what ?e) (my-eval-exp-or-region))
        ((equal what ?r) (my-eval-exp-or-region))
        ((equal what ?f) (eval-defun nil))
        ((equal what ?d) (edebug-defun))
        (t nil)))

(defhydra hydra-elisp (:timeout my-leader-timeout
                       :columns 2
                       :exit t)
  "Elisp menu"
  ("e" my-eval "Eval")
  ("v" my-eval "Eval")
  ("d" edebug-defun "Eval fn for debug")
  ("g" evil-jump-to-tag "Go to definition")
  ("i" my-describe-symbol "See documentation on this")
  ("?" my-describe-symbol "See documentation on this")
  ("r" my-lisp-repl "Go to elisp REPL"))

(defun register-elisp-leader ()
  "Pull up elisp hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-elisp/body))

;; register elisp leader in all elisp modes
(add-hook 'emacs-lisp-mode-hook 'register-elisp-leader)
(add-hook 'lisp-interaction-mode-hook 'register-elisp-leader)

(define-key
  ;; handy navigation to jump up the file
  evil-motion-state-map
  (kbd "C-s-k")
  'my-jump-up)

(use-package my-elisp-debugger)

(provide 'my-elisp)
