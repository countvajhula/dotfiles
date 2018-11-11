(defun my-battery-life ()
  "Show power info including battery life
   (Mac-specific, at the moment)."
  (interactive)
  (display-message-or-buffer (shell-command-to-string "pmset -g batt")))

(defhydra hydra-system (:exit t
                        :body-pre (evil-system-state)
                        :post (evil-normal-state))
  "System information"
  ("b" my-battery-life "show power info including battery life")
  ("s-i" my-battery-life "show power info including battery life")
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-application/body "enter lower level" :exit t)
  ("s-<escape>" nil "escape to higher level (none)" :exit t))

;; access the system menu via a "body" keybinding
(global-set-key (kbd "s-i") 'hydra-system/body)

(provide 'my-system-mode)
