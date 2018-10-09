(define-key
  ;; standard alternative keybinding to search file
  (current-global-map)
  (kbd "s-f")
  'evil-search-forward)

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
;;
;; (define-key
;;   ;; close current tab
;;   (current-global-map)
;;   (kbd "s-w")
;;   'mac-close-tab)

(define-key
  ;; save file
  (current-global-map)
  (kbd "s-s")
  'save-buffer)

(provide 'my-familiar)
