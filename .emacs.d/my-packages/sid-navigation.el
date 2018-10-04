(defun my-jump-down ()
  (interactive)
  (dotimes (i 9)
    (evil-next-line))
  (evil-scroll-line-to-center nil))

(defun my-jump-up ()
  (interactive)
  (dotimes (i 9)
    (evil-previous-line))
  (evil-scroll-line-to-center nil))

(defun my-scroll-down ()
  (interactive)
  (evil-scroll-line-down 3))

(defun my-scroll-up ()
  (interactive)
  (evil-scroll-line-up 3))

(define-key
  ;; alternative to Vim's C-u (since emacs reserves C-u)
  evil-motion-state-map
  (kbd "C-S-d")
  'evil-scroll-up)
(define-key
  ;; handy navigation to jump down the file
  evil-motion-state-map
  (kbd "C-s-j")
  'my-jump-down)
(define-key
  ;; handy navigation to jump up the file
  evil-motion-state-map
  (kbd "C-s-k")
  'my-jump-up)
(define-key
  ;; handy navigation to jump up the file
  evil-motion-state-map
  (kbd "<backspace>")
  'my-jump-up)
(define-key
  ;; scroll down the file a little faster than usual
  evil-motion-state-map
  (kbd "C-e")
  'my-scroll-down)
(define-key
  ;; scroll up the file a little faster than usual
  evil-motion-state-map
  (kbd "C-y")
  'my-scroll-up)
(define-key
  ;; remap original vim scroll bindings as "fine tuning"
  ;; rather than default scroll behavior
  evil-motion-state-map
  (kbd "C-S-e")
  'evil-scroll-line-down)
(define-key
  ;; remap original vim scroll bindings as "fine tuning"
  ;; rather than default scroll behavior
  evil-motion-state-map
  (kbd "C-S-y")
  'evil-scroll-line-up)

(provide 'sid-navigation)
