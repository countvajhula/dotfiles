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

;; define face for use in epistemic mode
(make-face 'eem-face)
(set-face-font 'eem-face "-*-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-foreground 'eem-face "tomato")

(setq eem-complete-tower (ht ('name "complete")
                             ('levels (ht ('0 (ht ('name "insert")
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
                                                  ('mode-entry 'hydra-application/body)))))))

(setq eem-vim-tower (ht ('name "vim")
                        ('levels (ht ('0 (ht ('name "insert")
                                             ('mode-entry 'evil-insert-state)))
                                     ('1 (ht ('name "normal")
                                             ('mode-entry 'evil-normal-state)))))))

(setq eem-emacs-tower (ht ('name "emacs")
                          ('levels (ht ('0 (ht ('name "emacs")
                                               ('mode-entry 'evil-emacs-state)))))))

(setq eem-towers `(,eem-complete-tower ,eem-vim-tower ,eem-emacs-tower))

;; the prefix that will be used in naming all buffers used
;; in epistemic mode representations
(setq eem-buffer-prefix "EPISTEMIC")

(setq current-tower-index 0)

(defun eem--tower (tower-id)
  "The epistemic tower corresponding to the provided index."
  (interactive)
  (nth tower-id eem-towers))

(defun eem--current-tower ()
  "The epistemic editing tower we are currently in."
  (interactive)
  (eem--tower current-tower-index))

(defun eem-previous-tower ()
  "Previous tower"
  (interactive)
  (let ((tower-id (mod (- current-tower-index
                          1)
                       (length eem-towers))))
    (eem--switch-to-tower tower-id)))

(defun eem-next-tower ()
  "Next tower"
  (interactive)
  (let ((tower-id (mod (+ current-tower-index
                          1)
                       (length eem-towers))))
    (eem--switch-to-tower tower-id)))

(defun eem--switch-to-tower (tower-id)
  "Switch to the tower indicated"
  (interactive)
  (let ((tower (eem--tower tower-id)))
    (switch-to-buffer (eem--buffer-name tower))
    (setq current-tower-index tower-id)
    (eem--extract-selected-level)))

(defun enter-first-level ()
  "Enter epistemic modes at first level"
  (interactive)
  (evil-force-normal-state)
  ;; start at the lowest level
  (hydra-char/body))

(defun eem--buffer-name (tower)
  "Buffer name to use for a given tower."
  (concat eem-buffer-prefix "-" (ht-get tower 'name)))

(defun eem-render-tower (tower)
  "Render a text representation of an epistemic editing tower."
  (interactive)
  (let ((tower-buffer (my-new-empty-buffer
                       (eem--buffer-name tower)))
        (tower-levels (ht-get tower 'levels)))
    (with-current-buffer tower-buffer
      (buffer-face-set 'eem-face)
      (text-scale-set 5)
      (dolist (level-number
               (ht-keys tower-levels))
        (let ((level (ht-get tower-levels
                             level-number)))
          (insert "|―――"
                  (number-to-string level-number)
                  "―――|"
                  " " (ht-get level
                              'name) " " "\n")))
      (my-delete-line)
      ;;(setq cursor-type nil))
      (hl-line-mode)
      (blink-cursor-mode -1)
      (internal-show-cursor nil nil))
    tower-buffer))

(defun my-enter-mode-mode ()
  "Enter a buffer containing a textual representation of the
initial epistemic tower."
  (interactive)
  (dolist (tower eem-towers)
    (eem-render-tower tower))
  (switch-to-buffer (eem--buffer-name (eem--current-tower)))
  (evil-mode-state))

(defun my-exit-mode-mode ()
  "Exit mode mode."
  (interactive)
  (hl-line-mode -1)
  (blink-cursor-mode 1)
  (evil-normal-state)
  (kill-matching-buffers (concat "^" eem-buffer-prefix) nil t))

(define-key evil-insert-state-map [s-escape] 'enter-first-level)
(define-key evil-normal-state-map [s-escape] 'hydra-window/body)
(define-key evil-normal-state-map [s-return] 'evil-insert-state)

(defun eem--enter-level (level-number)
  "Enter level LEVEL-NUMBER"
  (let* ((tower (eem--current-tower))
         (levels (ht-get tower 'levels))
         (level (ht-get levels level-number)))
    (funcall (ht-get level 'mode-entry))))

(defun eem-enter-level ()
  "Enter selected level"
  (interactive)
  (eem--enter-level eem--selected-level))

(defun eem--extract-selected-level ()
  "Extract the selected level from the current representation"
  (interactive)
  (let* ((level-str (substring (thing-at-point 'line t) 0 5))
         (level-number (string-to-number (substring level-str 4 5))))
    (setq eem--selected-level level-number)))

(defun eem-select-previous-level ()
  "Select previous level"
  (interactive)
  (evil-previous-line)
  (eem--extract-selected-level))

(defun eem-select-next-level ()
  "Select next level"
  (interactive)
  (evil-next-line)
  (eem--extract-selected-level))

(defhydra hydra-mode (:idle 1.0
                      :columns 4
                      :body-pre (my-enter-mode-mode)
                      :post (my-exit-mode-mode))
  "Mode mode"
  ;; Need a textual representation of the mode tower for these to operate on
  ("h" eem-previous-tower "previous tower")
  ("j" eem-select-next-level "lower level")
  ("k" eem-select-previous-level "higher level")
  ("l" eem-next-tower "next tower")
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
