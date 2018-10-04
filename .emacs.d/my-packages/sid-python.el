(defhydra hydra-python (:idle 1.0
                        :columns 2
                        :exit t)
  "Python menu"
  ("c" elpy-check "Run lint checks")
  ("d" elpy-doc "See documentation on this")
  ("o" elpy-occur-definitions "See all definitions in current buffer")
  ("p" elpy-shell-switch-to-shell "Go to Python REPL")
  ("r" elpy-shell-send-region-or-buffer "Send to REPL")
  ("t" elpy-test "Run test(s)"))

(general-define-key
 :states '(normal visual motion)
 :keymaps 'override
 "`" 'hydra-python/body)

(provide 'sid-python)
