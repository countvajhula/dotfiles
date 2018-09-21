;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES AND CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;; convenient list- and functional-related macros
(use-package dash)

;; navigation sidebar
(use-package sr-speedbar)

;; handy project-related functions like grep search, find file, etc.
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map
              (kbd "s-p")
              'projectile-find-file)
  (define-key (current-global-map)
              (kbd "s-F")
              'projectile-grep))

;; Vim interface
(use-package evil
  :config
  (evil-mode 1)
  ;; use Emacs keybindings when in insert mode }:)
  (setcdr evil-insert-state-map nil)
  ;; Esc goes to normal mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; use "symbols" instead of simple words in point searches
  (defalias #'forward-evil-word #'forward-evil-symbol)

  (defun my-jump-down ()
    (interactive)
    (dotimes (i 9)
      (evil-next-line))
    (evil-scroll-line-to-center nil))

  (defun my-jump-up ()
    (interactive)
    (dotimes (i 9)
      (evil-previous-line))
    (evil-scroll-line-to-center nil))

  (defun my-scroll-down ()
    (interactive)
    (evil-scroll-line-down 3))

  (defun my-scroll-up ()
    (interactive)
    (evil-scroll-line-up 3))

  (define-key
    ;; re-bind elpy completion in evil insert mode
    ;; which apparently gets overridden by evil
    evil-insert-state-map
    (kbd "M-<tab>")
    'elpy-company-backend)
  (define-key
    ;; handy navigation to jump down the file
    evil-motion-state-map
    (kbd "SPC")
    'my-jump-down)
  (define-key
    ;; handy navigation to jump up the file
    evil-motion-state-map
    (kbd "C-SPC")
    'my-jump-up)
  (define-key
    ;; handy navigation to jump up the file
    evil-motion-state-map
    (kbd "<backspace>")
    'my-jump-up)
  (define-key
    ;; scroll down the file a little faster than usual
    evil-motion-state-map
    (kbd "C-S-e")
    'my-scroll-down)
  (define-key
    ;; scroll up the file a little faster than usual
    evil-motion-state-map
    (kbd "C-S-y")
    'my-scroll-up)
  (define-key
    ;; standard alternative keybinding to search file
    (current-global-map)
    (kbd "s-f")
    'evil-search-forward))

;; python IDE
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-modules
	(remove 'elpy-module-highlight-indentation
		elpy-modules))
  (setq elpy-rpc-python-command "python3")
  ;; use jedi for completion with elpy instead of rope
  (setq elpy-rpc-backend "jedi")
  (setq python-check-command "~/.local/bin/pyflakes")
  (add-hook 'python-mode-hook (lambda () (show-paren-mode 1))))

;; ido mode
(use-package ido
  :disabled t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

(use-package sublimity
  :disabled t
  :config
  (sublimity-mode 1))

(use-package company
  :config
  ;; enable company mode autocompletion in all buffers
  (global-company-mode 1))

(use-package ivy
  ;; company is for in-buffer auto-completion,
  ;; ivy is for application-level on-demand completion
  :bind ("M-x" . counsel-M-x)
  :bind ("C-s" . swiper)
  :bind ("C-c k" . counsel-unicode-char)
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  ;; use fuzzy-style matching in all cases except swiper (from SX)
  ;; (setq ivy-re-builders-alist
  ;; 	'((swiper . ivy--regex-plus)
  ;; 	  (t . ivy--regex-fuzzy)))
  (setq ivy-wrap t))

(use-package magit
  :config
  ;; use side-by-side view for blame -- this doesn't work atm
  ;; (setq magit-blame--style (nth 1 magit-blame-styles))
  (define-key (current-global-map)
              (kbd "C-x g")
              'magit-status)
  (define-key (current-global-map)
              (kbd "C-x M-g")
              'magit-dispatch-popup))

(use-package tabbar
  :disabled t
  :config
  ;; turn on the tabbar
  (tabbar-mode t)
  (define-key (current-global-map)
              (kbd "s-{")
              'tabbar-backward)
  (define-key (current-global-map)
              (kbd "s-}")
              'tabbar-forward))

(use-package evil-tabs
  :disabled t
  :config
  (global-evil-tabs-mode t))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package multiple-cursors
  ;; the original multiple-cursors mode, looks great but isn't
  ;; compatible with evil mode. evil-mc looks pretty similar
  :disabled t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1)
  ;; interface with multiple cursors via a hydra menu
  (defhydra hydra-cursors ()
    "Multiple cursors"
    ("a" evil-mc-make-all-cursors "make all cursors")
    ("n" evil-mc-make-and-goto-next-match "mark, go to next")
    ("N" evil-mc-make-and-goto-prev-match "mark, go to previous")
    ("s" evil-mc-skip-and-goto-next-match "skip, go to next")
    ("S" evil-mc-skip-and-goto-prev-match "skip, go to previous")
    ("h" evil-mc-make-cursor-here  "make cursor here")
    ("p" evil-mc-pause-cursors  "pause cursors")
    ("P" evil-mc-resume-cursors  "resume cursors")
    ("<escape>" evil-mc-undo-all-cursors "undo all cursors"))

  ;; access the multiple-cursors menu via a "body" keybinding
  (global-set-key (kbd "s-d") 'hydra-cursors/body))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region nil)  ; workaround for undotree bug
  (global-set-key (kbd "C-c u") 'undo-tree-visualize)
  (global-set-key (kbd "C-c U") 'undo-tree-visualizer-abort))

;; looks like smex (smart command history in M-x) is used by counsel just
;; by being installed, and doesn't need to be explicitly invoked here
;; (use-package smex
;;   :config
;;   (smex-initialize))