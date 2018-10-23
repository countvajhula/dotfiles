(defun my-battery-life ()
  "Show power info including battery life
   (Mac-specific, at the moment)."
  (interactive)
  (display-message-or-buffer (shell-command-to-string "pmset -g batt")))

(defhydra hydra-system (:exit t)
  "System information"
  ("b" my-battery-life "show power info including battery life")
  ("s-i" my-battery-life "show power info including battery life")
  ("<return>" hydra-application/body "enter" :exit t)
  ("<escape>" my-noop "exit" :exit t))

;; access the system menu via a "body" keybinding
(global-set-key (kbd "s-i") 'hydra-system/body)
