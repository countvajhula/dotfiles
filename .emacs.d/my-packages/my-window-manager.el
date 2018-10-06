;; Evil provides some good window navigation functionality, but these
;; bindings aren't available in Emacs state and also consequently in
;; Insert state if the insert mode keymap is overridden in favor of
;; Emacs native bindings. Additionally, for the most common window
;; operations, some of the evil mode defaults could be further
;; improved.  This package provides evil window navigation globally
;; and also overrides some defaults to make them faster or more
;; useful/intuitive.

(define-prefix-command 'my-window-map)
(global-set-key (kbd "C-w") 'my-window-map)
(define-key my-window-map (kbd "h") 'evil-window-left)
(define-key my-window-map (kbd "j") 'evil-window-down)
(define-key my-window-map (kbd "k") 'evil-window-up)
(define-key my-window-map (kbd "l") 'evil-window-right)
(define-key my-window-map (kbd "C-c") 'evil-window-delete)
(define-key my-window-map (kbd "c") 'evil-window-delete)
(define-key my-window-map (kbd "C-x") 'evil-window-delete)
(define-key my-window-map (kbd "x") 'evil-window-delete)
(define-key my-window-map (kbd "C-o") 'delete-other-windows)
(define-key my-window-map (kbd "o") 'delete-other-windows)
(define-key my-window-map (kbd "C-w") 'delete-other-windows)
(define-key my-window-map (kbd "w") 'delete-other-windows)
(define-key my-window-map (kbd "C-s") 'evil-window-split)
(define-key my-window-map (kbd "s") 'evil-window-split)
(define-key my-window-map (kbd "C-v") 'evil-window-vsplit)
(define-key my-window-map (kbd "v") 'evil-window-vsplit)
(define-key my-window-map (kbd "=") 'balance-windows)
(define-key my-window-map (kbd "C-f") 'ffap-other-window)
(define-key my-window-map (kbd "f") 'ffap-other-window)
(global-set-key (kbd "s-w") 'evil-window-mru)
;; also override to provide the same behavior in evil states
(define-key evil-motion-state-map (kbd "C-w C-x") 'evil-window-delete)
(define-key evil-motion-state-map (kbd "C-w x") 'evil-window-delete)
(define-key evil-motion-state-map (kbd "C-w C-w") 'delete-other-windows)
(define-key evil-motion-state-map (kbd "C-w w") 'delete-other-windows)

(provide 'my-window-manager)
