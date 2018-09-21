;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;


;; map Mac's Command key to Emacs/Lisp's Super key
(setq mac-command-modifier 'super)
;; make Fn key do Hyper [coz, why not]
(setq mac-function-modifier 'hyper)

; Note: "define-key (current-global-map)" is the same as global-set-key

(define-key
  ;; info on the current buffer
  (current-global-map)
  (kbd "C-c b")
  'my-buffer-info)
(define-key
  ;; navigation sidebar
  (current-global-map)
  (kbd "C-c t")
  'sr-speedbar-toggle)
(define-key
  ;; open a new empty buffer
  (current-global-map)
  (kbd "C-c n")
  'xah-new-empty-buffer)
(define-key
  ;; drop into a shell (preserves path)
  (current-global-map)
  (kbd "C-c s")
  'eshell)
(define-key
  ;; lookup in dictionary
  (current-global-map)
  (kbd "C-c d")
  'dictionary-lookup-definition)
(define-key
  ;; open an elisp shell
  (current-global-map)
  (kbd "C-c l")
  'my-lisp-repl)
(define-key
  ;; emulate caps lock -- alternative to an actual CAPS LOCK key
  (current-global-map)
  (kbd "C-<escape>")
  'caps-lock-mode)
(define-key
  ;; calculator mode
  (current-global-map)
  (kbd "C-+")
  'calc)
(define-key
  ;; cycle forward through tabs
  (current-global-map)
  (kbd "s-}")
  'mac-next-tab)
(define-key
  ;; cycle backward through tabs
  (current-global-map)
  (kbd "s-{")
  'mac-previous-tab)
;; TODO: the following keybindings for tabs
;; These don't work at the moment since I'm not able
;; to locate the required functions
;; (define-key
;;   ;; create a new tab
;;   (current-global-map)
;;   (kbd "s-t")
;;   'mac-make-tab)
;; (define-key
;;   ;; close current tab
;;   (current-global-map)
;;   (kbd "s-w")
;;   'mac-close-tab)
(define-key
  ;; delete current window
  (current-global-map)
  (kbd "s-w")
  'delete-window)
(define-key
  ;; save file
  (current-global-map)
  (kbd "s-s")
  'save-buffer)


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
