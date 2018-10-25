;; Define evil states for each epistemic mode
(evil-define-state char
  "Char state."
  :tag " <X> "
  :message "-- CHAR --"
  ;;:cursor ;; inherit from normal
  ;;:entry-hook ;; potentially call the hydra here
  ;;:exit-hook ;; none
  ;;:suppress-keymap) ;; should be t, but probably inherits from normal
  :enable (motion normal))

(evil-define-state word
  "Word state."
  :tag " <W> "
  :message "-- WORD --"
  :enable (motion normal))

(evil-define-state line
  "Line state."
  :tag " <L> "
  :message "-- LINE --"
  :enable (motion normal))

(evil-define-state view
  "View state."
  :tag " <V> "
  :message "-- VIEW --"
  :enable (motion normal))

(evil-define-state window
  "Window state."
  :tag " <W> "
  :message "-- WINDOW --"
  :enable (motion normal))

(evil-define-state file
  "File state."
  :tag " <F> "
  :message "-- FILE --"
  :enable (motion normal))

(evil-define-state buffer
  "Buffer state."
  :tag " <B> "
  :message "-- BUFFER --"
  :enable (motion normal))


(require 'my-char-mode)
(require 'my-word-mode)
(require 'my-line-mode)
(require 'my-view-mode)
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
