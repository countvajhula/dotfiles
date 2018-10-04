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


;;;;;;;;;;;;;;;;;
;; HYDRA MENUS ;;
;;;;;;;;;;;;;;;;;


(defun return-to-original-buffer ()
  (interactive)
  (switch-to-buffer original-buffer))

(defhydra hydra-buffers (:idle 1.0
			             :body-pre (setq original-buffer
                                         (current-buffer)))
  "Cycle through buffers, Alt-tab style"
  ("b" list-buffers "show all buffers")
  ("n" next-buffer "next buffer")
  ("N" previous-buffer "previous buffer")
  ("p" previous-buffer "previous buffer")
  ("P" next-buffer "next buffer")
  ("h" previous-buffer "previous buffer")
  ("l" next-buffer "next buffer")
  ("<escape>" return-to-original-buffer "return to original buffer" :exit t))

;; access the system menu via a "body" keybinding
(global-set-key (kbd "s-b") 'hydra-buffers/body)

(defhydra hydra-system (:exit t)
  "System information"
  ("b" my-battery-life "show power info including battery life")
  ("s-i" my-battery-life "show power info including battery life"))

;; access the buffer menu via a "body" keybinding
(global-set-key (kbd "s-i") 'hydra-system/body)

(defun current-transparency ()
  (nth 0
       (frame-parameter (selected-frame)
			'alpha)))

;; Set transparency of emacs
(defun transparency (value)
 "Sets the transparency of the frame window. 0=transparent/100=opaque"
 (interactive "nTransparency Value 0 - 100 opaque:")
 (set-frame-parameter (selected-frame) 'alpha (cons value value)))

(defun adjust-transparency (delta)
  "Adjust the transparency of the frame window by the configured delta,
   in the range: 0=transparent/100=opaque"
  (interactive)
  (transparency (+ (current-transparency)
		   delta)))

(defun increase-transparency ()
  "Increase frame transparency."
  (interactive)
  (adjust-transparency -3))

(defun decrease-transparency ()
  "Decrease frame transparency."
  (interactive)
  (adjust-transparency 3))

(defun return-to-original-transparency ()
  "Return to original transparency prior to making changes."
  (interactive)
  (transparency original-transparency))

(defhydra hydra-transparency (:columns 1
                              :body-pre (setq original-transparency
                                              (current-transparency)))
  "Control frame transparency"
  ("+" decrease-transparency "decrease transparency")
  ("-" increase-transparency "increase transparency")
  ("k" decrease-transparency "decrease transparency")
  ("j" increase-transparency "increase transparency")
  ("<escape>" return-to-original-transparency "return to original transparency" :exit t))

;; access the system menu via a "body" keybinding
(global-set-key (kbd "s-e t") 'hydra-transparency/body)

;; toggle line numbers
(global-set-key (kbd "s-e n") 'display-line-numbers-mode)

(provide 'my-keybindings)
