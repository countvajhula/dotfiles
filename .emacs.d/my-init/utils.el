(defun my-noop ()
  "A function that does nothing."
  (interactive))

(defun my-lisp-repl ()
  "Enter elisp REPL, context-aware.

If there is only one window, open REPL in a new window. Otherwise
open in current window."
  (interactive)
  (when (= (length (window-list))
           1)
    (progn (evil-window-vsplit)
           (evil-window-right 1)))
  (ielm))

(defun my-current-dir ()
    "View current dir in dired."
    (interactive)
    (dired nil))
