;; A "mode" to navigate buffers.
;; TODO: s-b to get into mode, ivy should already be visible, hjkl has
;; calling on, s to search-> exit hydra and go to
;; counsel-switch-buffer, s-b s-b to flashback, q to return to
;; original buffer, esc to exit mode

;; (defun my-ivy-switch-buffer ()
;;   (interactive)
;;   (keyboard-quit)
;;   (ivy-switch-buffer))

;; (define-prefix-command 'my-buffer-map)
;; (define-key my-buffer-map (kbd "j") 'ivy-next-line-and-call)
;; (define-key my-buffer-map (kbd "k") 'ivy-previous-line-and-call)
;; (define-key my-buffer-map (kbd "s") 'my-ivy-switch-buffer)
;; (define-key my-buffer-map (kbd "s-b") 'keyboard-quit)

;; (defun my-buffer-select ()
;;   (interactive)
;;   (setq ll (mapcar #'buffer-name (buffer-list)))
;;   (setq ll (append (cdr ll) (list (car ll))))
;;   (switch-to-buffer (car ll))
;;   (ivy-read "My buffers: " ll
;;             :action '(1 ;; index (1 based) of the default action
;;                       ("s" (lambda (x)
;;                              (switch-to-buffer x)) "switch"))
;;             :keymap my-buffer-map))

;; (global-set-key (kbd "s-b") 'my-buffer-select)

;; alternatively, try implementing as "advice" to ivy-switch-buffer
;; and provide a custom keymap. still, seems like it needs to be
;; managed inside a hydra

(defun return-to-original-buffer ()
  (interactive)
  (switch-to-buffer original-buffer))

(defhydra hydra-buffers (:idle 1.0
                         :columns 3
			             :body-pre (setq original-buffer
                                         (current-buffer)))
  "Cycle through buffers, Alt-tab style"
  ("b" list-buffers "show all buffers")
  ("s-b" evil-switch-to-windows-last-buffer "switch to last buffer" :exit t)
  ("h" previous-buffer "previous buffer")
  ("l" next-buffer "next buffer")
  ("s" ivy-switch-buffer "search buffers" :exit t)
  ("i" ibuffer "ibuffer" :exit t)
  ("x" kill-buffer "kill buffer")
  ("q" return-to-original-buffer "return to original buffer" :exit t)
  ("<escape>" my-noop "exit" :exit t))

;; access the buffer menu via a "body" keybinding
(global-set-key (kbd "s-b") 'hydra-buffers/body)

(provide 'my-buffer-mode)
