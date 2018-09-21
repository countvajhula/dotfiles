;;;;;;;;;;;;;;;;;
;; HYDRA MENUS ;;
;;;;;;;;;;;;;;;;;


(defun return-to-original-buffer ()
  (interactive)
  (switch-to-buffer original-buffer))

(defhydra hydra-buffers (:idle 1.0
			 :body-pre (setq original-buffer (current-buffer)))
  "Cycle through buffers, Alt-tab style"
  ("b" list-buffers "show all buffers")
  ("n" next-buffer "next buffer")
  ("N" previous-buffer "previous buffer")
  ("p" previous-buffer "previous buffer")
  ("P" next-buffer "next buffer")
  ("h" previous-buffer "previous buffer")
  ("l" next-buffer "next buffer")
  ("j" next-buffer "next buffer")
  ("k" previous-buffer "previous buffer")
  ("<escape>" return-to-original-buffer "return to original buffer" :exit t))

;; access the system menu via a "body" keybinding
(global-set-key (kbd "s-b") 'hydra-buffers/body)

(defhydra hydra-system (:exit t)
  "System information"
  ("b" my-battery-life "show power info including battery life"))

;; access the buffer menu via a "body" keybinding
(global-set-key (kbd "s-i") 'hydra-system/body)
