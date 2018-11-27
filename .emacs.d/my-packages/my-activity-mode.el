(defhydra hydra-activity (:idle 1.0
                          :columns 2
                          :body-pre (evil-activity-state)
                          :post (evil-normal-state))
  "Activity mode"
  ("h" goto-last-change "previous change")
  ("j" goto-last-change-reverse "next change")
  ("k" goto-last-change "previous change")
  ("l" goto-last-change-reverse "next change")
  ("i" my-noop "exit" :exit t)
  ("<escape>" my-noop "exit" :exit t))

(global-set-key (kbd "s-a") 'hydra-activity/body)

(provide 'my-activity-mode)
