;; other possibilities:
;; - take number as arg
;; - reflect as a single evil command
;; - rotate words right/left
;; - > indent
;; - delete other lines
;; - copy line
;; similarly for "region-mode", possibly by invoking multiple cursors

(defun my-move-line-down (&optional count)
  "Move line down"
  (interactive)
  (unless count (setq count 1))
  (evil-next-line)
  (transpose-lines count)
  (evil-previous-line))

(defun my-move-line-up (&optional count)
  "Move line up"
  (interactive)
  (unless count (setq count 1))
  (transpose-lines count)
  (evil-previous-line 2))

(defun my-move-line-left (&optional count)
  "Move line left"
  (interactive)
  (unless count (setq count 1))
  (save-excursion
    (evil-first-non-blank)
    (setq starting-from (- (point) count))
    (if (< starting-from
           (line-beginning-position))
        (setq starting-from (line-beginning-position)))
    (evil-delete-backward-char starting-from
                               (point)
                               'exclusive
                               nil)))

(defun my-move-line-right (&optional count)
  "Move line right"
  (interactive)
  (unless count (setq count 1))
  (save-excursion
    (evil-first-non-blank)
    (insert-char #x20 count)))

(defun my-move-line-far-left ()
  "Move line far left"
  (interactive)
  (save-excursion
    (evil-first-non-blank)
    (evil-delete (line-beginning-position)
                 (point)
                 (quote exclusive)
                 nil
                 nil)))

(defun my-move-line-far-right ()
  "Move line far right"
  (interactive)
  (save-excursion
    (evil-beginning-of-line)
    (unless (bobp)
      (evil-previous-line)
      (evil-first-non-blank)
      (setq line-position (- (point)
                             (line-beginning-position)))
      (evil-next-line)
      (my-move-line-far-left)
      (my-move-line-right line-position))))

(defun my-move-line-very-bottom ()
  "Move line to bottom"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m$")))

(defun my-move-line-very-top ()
  "Move line to top"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m0")))

(defun my-delete-line ()
  "Delete line"
  (interactive)
  (evil-delete-whole-line (line-beginning-position)
                          (+ 1 (line-end-position))
                          (quote line)
                          nil))

(defun my-flashback ()
  "Flashback to prev line"
  (interactive)
  (evil-goto-mark-line ?'))

(defun my-split-line ()
  "Split line on word separators"
  (interactive)
  (evil-beginning-of-line)
  (while (not (eolp))
    (unless (equal (- (line-end-position)
                      (line-beginning-position))
                   1)
      (evil-forward-word-end))
    (execute-kbd-macro (kbd "a"))
    (newline)
    (evil-force-normal-state)))

(defun my-pulverize-line ()
  "Split on every character"
  (interactive)
  (evil-beginning-of-line)
  (while (not (eolp))
    (evil-forward-char)
    (newline)
    (evil-force-normal-state)))

(defun my-line-info ()
  "Info about the line"
  (interactive)

  (defun line-length (n)
    "Length of the Nth line.
From: https://emacs.stackexchange.com/questions/17846/calculating-the-length-of-a-line-in-a-buffer"
    (save-excursion
      (goto-char (point-min))
      (if (zerop (forward-line (1- n)))
          (- (line-end-position)
             (line-beginning-position)))))
  (setq current-line-number (line-number-at-pos))
  (setq current-line-length (line-length current-line-number))
  (message "Line %d, length = %d" current-line-number current-line-length))


(defun my-yank-line ()
  "Yank (copy) line"
  (interactive)
  (evil-yank-line (line-beginning-position) (line-end-position) 'line nil))

(defhydra hydra-line (:idle 1.0
                      :columns 4)
  "Line mode"
  ("h" my-move-line-left "left")
  ("j" my-move-line-down "down")
  ("k" my-move-line-up "up")
  ("l" my-move-line-right "right")
  ("H" my-move-line-far-left "move to far left")
  ("J" my-move-line-very-bottom "move to bottom")
  ("K" my-move-line-very-top "move to top")
  ("L" my-move-line-far-right "move to far right")
  ("x" my-delete-line "delete line")
  ("s-o" my-flashback "flashback")
  ("o" my-flashback)
  ("s-s" my-split-line "split horizontally")
  ("s" my-split-line)
  ("v" my-pulverize-line "pulverize")
  ("y" my-yank-line "yank (copy)")
  ("i" my-line-info "info" :exit t)
  ("<escape>" my-noop "exit" :exit t))

(global-set-key (kbd "s-l") 'hydra-line/body)

(provide 'my-line-mode)