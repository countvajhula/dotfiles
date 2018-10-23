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

(define-key evil-insert-state-map [s-escape] 'enter-first-level)
(define-key evil-normal-state-map [s-escape] 'hydra-window/body)
(define-key evil-normal-state-map [s-return] 'evil-insert-state)

(provide 'evil-epistemic-mode)
