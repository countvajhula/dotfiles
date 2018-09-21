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
