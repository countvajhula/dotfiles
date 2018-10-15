(defhydra hydra-elisp (:timeout 2.0
                       :columns 2
                       :exit t)
  "Elisp menu"
  ("e" eval-last-sexp "Eval sexp")
  ("v" eval-last-sexp "Eval sexp")
  ("f" eval-defun "Eval function")
  ("d" edebug-defun "Eval fn for debug")
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

(provide 'my-elisp)
