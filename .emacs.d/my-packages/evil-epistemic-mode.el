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

(evil-define-state application
  "Application state."
  :tag " <A> "
  :message "-- APPLICATION --"
  :enable (motion normal))

(evil-define-state system
  "System state."
  :tag " <S> "
  :message "-- SYSTEM --"
  :enable (motion normal))

(evil-define-state mode
  "Mode state."
  :tag " <M> "
  :message "-- MODE --"
  :enable (motion normal))

(require 'my-char-mode)
(require 'my-word-mode)
(require 'my-line-mode)
(require 'my-view-mode)
(require 'my-window-mode)
(require 'my-file-mode)
(require 'my-buffer-mode)
(require 'my-system-mode)
(require 'my-application-mode)

(setq eem-levels (ht ('0 (ht ('name "insert")
                             ('mode-entry 'evil-insert-state)))
                     ('1 (ht ('name "char")
                             ('mode-entry 'hydra-char/body)))
                     ('2 (ht ('name "word")
                             ('mode-entry 'hydra-word/body)))
                     ('3 (ht ('name "line")
                             ('mode-entry 'hydra-line/body)))
                     ('4 (ht ('name "view")
                             ('mode-entry 'hydra-view/body)))
                     ('5 (ht ('name "window")
                             ('mode-entry 'hydra-window/body)))
                     ('6 (ht ('name "file")
                             ('mode-entry 'hydra-file/body)))
                     ('7 (ht ('name "buffer")
                             ('mode-entry 'hydra-buffer/body)))
                     ('8 (ht ('name "system")
                             ('mode-entry 'hydra-system/body)))
                     ('9 (ht ('name "application")
                             ('mode-entry 'hydra-application/body)))))

(defun enter-first-level ()
  "Enter epistemic modes at first level"
  (interactive)
  (evil-force-normal-state)
  ;; start at the lowest level
  (hydra-char/body))

(ht-get (ht-get eem-levels 0) 'name)

(defun my-enter-mode-mode ()
  "Enter a buffer containing a textual representation of the
initial epistemic tower."
  (interactive)
  (my-new-empty-buffer "EPISTEMIC")
  (dolist (key (ht-keys eem-levels))
    (insert "―――"
            (number-to-string key)
            "―――"
            "(" (ht-get (ht-get eem-levels
                                key)
                        'name) ")" "\n"))
  (my-delete-line)
  (evil-mode-state)
  ;;(setq cursor-type nil))
  (hl-line-mode)
  (blink-cursor-mode -1)
  (internal-show-cursor nil nil))

(my-enter-mode-mode)

(defun my-exit-mode-mode ()
  "Exit mode mode."
  (interactive)
  (hl-line-mode -1)
  (blink-cursor-mode 1)
  (evil-normal-state)
  (kill-buffer "EPISTEMIC"))

(define-key evil-insert-state-map [s-escape] 'enter-first-level)
(define-key evil-normal-state-map [s-escape] 'hydra-window/body)
(define-key evil-normal-state-map [s-return] 'evil-insert-state)

(defun eem-enter-level ()
  "Enter selected level"
  (interactive)
  (funcall 'hydra-word/body))
  (let* ((level-str (substring (thing-at-point 'line t) 0 7))
         (level-number (elt level-str 3)))
    (message "%s level number is %s level str is %s" "EYOOOOO" level-number level-str)
    (message "%s" (ht-get (ht-get eem-levels level-number)
                          'mode-entry))
    (funcall (ht-get (ht-get eem-levels level-number)
                     'mode-entry))))

(defhydra hydra-mode (:idle 1.0
                      :columns 4
                      :body-pre (my-enter-mode-mode)
                      :post (my-exit-mode-mode))
  "Mode mode"
  ;; Need a textual representation of the mode tower for these to operate on
  ("h" evil-previous-line "previous level")
  ("j" evil-next-line "lower level")
  ("k" evil-previous-line "higher level")
  ("l" evil-next-line "next level")
  ;; ("H" eem-highest-level "first level (recency)")
  ;; ("J" eem-lowest-level "lowest level")
  ;; ("K" eem-highest-level "highest level")
  ;; ("L" eem-lowest-level "last level (recency)")
  ;; different towers for different "major modes"
  ;; ("s-o" eem-mode-mru "Jump to most recent (like Alt-Tab)" :exit t)
  ;; ("o" eem-mode-mru :exit t)
  ;; with delete / change etc. we could construct towers and then select towers
  ;; there could be a maximal tower containing all the levels
  ;; ("/" eem-search "search")
  ;; move to change ordering of levels, an alternative to recency
  ;;
  ;; ffap other window -- open file with this other mode/tower: a formal "major mode"
  ;; the mode mode, tower mode, and so on recursively makes more sense
  ;; if we assume that keyboard shortcuts are scarce. this gives us ways to use
  ;; a small number of keys in any arbitrary configuration
  ("<return>" eem-enter-level :exit t)
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-view/body "enter lower level" :exit t)
  ("s-<escape>" hydra-buffer/body "escape to higher level" :exit t))

(global-set-key (kbd "s-m") 'hydra-mode/body)

(provide 'evil-epistemic-mode)
