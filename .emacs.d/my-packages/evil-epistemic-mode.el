(require 'my-char-mode)
(require 'my-word-mode)
(require 'my-line-mode)
(require 'my-page-mode)
(require 'my-window-mode)
(require 'my-file-mode)
(require 'my-buffer-mode)

(defun enter-first-level ()
  "Enter epistemic modes at first level"
  (interactive)
  (evil-force-normal-state)
  ;; start at the lowest level
  (hydra-char/body))

(defun enter-epistemic-modes ()
  "Enter epistemic modes.

Enter/Escape will now go up and down the epistemic editing levels."
  (interactive)
  ;; Esc goes to character mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'enter-first-level)
  (enter-first-level))

(defun exit-epistemic-modes ()
  "Exit epistemic modes.

Enter/Escape will go between Insert/Normal as usual."
  (interactive)
  ;; Esc goes to normal mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; Enter goes to insert mode in normal mode, for consistency with epistemic modes
  (define-key evil-normal-state-map [return] 'evil-insert-state)
  (evil-force-normal-state))

(global-set-key (kbd "s-<return>") 'enter-epistemic-modes)
(global-set-key (kbd "s-<escape>") 'exit-epistemic-modes)

(provide 'evil-epistemic-mode)
