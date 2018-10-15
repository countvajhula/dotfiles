(defun my-noop ()
  "A function that does nothing."
  (interactive))

(defun my-lisp-repl ()
  "An elisp REPL."
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1)
  (ielm))
