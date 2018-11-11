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

(defun my-recenter-view-advice (orig-fn &rest args)
  "Depending on context, recenter screen on cursor.

Recenter if new jump location is not visible from any part of the
initial screen (when centered) -- same behavior as Vim."
  (save-excursion
    (evil-window-top)
    (setq initial-screen-top-line (line-number-at-pos))
    (evil-window-bottom)
    (setq initial-screen-bottom-line (line-number-at-pos)))
  (let ((res (apply orig-fn args)))
    (let* ((current-line-position (line-number-at-pos))
           (distance-from-screen-top (abs (- current-line-position
                                             initial-screen-top-line)))
           (distance-from-screen-bottom (abs (- current-line-position
                                                initial-screen-bottom-line)))
           (min-distance (min distance-from-screen-top
                              distance-from-screen-bottom)))
      (when (> min-distance
               (/ (window-text-height)
                  2))
        (recenter))
      res)))
