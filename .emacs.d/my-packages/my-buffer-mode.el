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

(defun my-buffer-set-mark (mark-name)
  "Set a mark"
  (interactive "cMark name?")
  (puthash mark-name (current-buffer) my-buffer-marks-hash)
  (message "Mark '%c' set." mark-name))

(defun my-buffer-return-to-mark (mark-name)
  "Return to mark"
  (interactive "cMark name?")
  (switch-to-buffer (gethash mark-name my-buffer-marks-hash)))

(defun return-to-original-buffer ()
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (switch-to-buffer (gethash "0" my-buffer-marks-hash)))

(defun setup-buffer-marks-table ()
  "Initialize the buffer marks hashtable and add an entry for the
current ('original') buffer."
  (interactive)
  (defvar my-buffer-marks-hash
    (make-hash-table :test 'equal))
  (puthash "0" (current-buffer)
           my-buffer-marks-hash))

(defhydra hydra-buffer (:idle 1.0
                         :columns 3
			             :body-pre (setup-buffer-marks-table))
  "Buffer mode"
  ("b" list-buffers "show all")
  ("s-b" evil-switch-to-windows-last-buffer "switch to last" :exit t)
  ("h" previous-buffer "previous")
  ("l" next-buffer "next")
  ("n" xah-new-empty-buffer "new")
  ("m" my-buffer-set-mark "set mark")
  ("'" my-buffer-return-to-mark "return to mark")
  ("`" my-buffer-return-to-mark "return to mark")
  ("s" ivy-switch-buffer "search" :exit t)
  ("/" ivy-switch-buffer "search" :exit t)
  ("i" ibuffer "ibuffer" :exit t)
  ("s-i" ibuffer "ibuffer" :exit t)
  ("x" kill-buffer "delete")
  ("?" my-buffer-info "info" :exit t)
  ("q" return-to-original-buffer "return to original" :exit t)
  ("<return>" hydra-window/body "enter" :exit t)
  ("<escape>" hydra-application/body "exit" :exit t))

;; access the buffer menu via a "body" keybinding
(global-set-key (kbd "s-b") 'hydra-buffer/body)

(provide 'my-buffer-mode)
