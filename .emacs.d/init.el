;;;;;;;;;;;;;;;;;;;
;; INITIAL SETUP ;;
;;;;;;;;;;;;;;;;;;;


;; 'require' looks in the load-path, so packages need to be
;; downloaded from melpa prior to this.
;; package.el, though, is bundled with emacs
(require 'package)

;; add some standard package repos with lots of non-bundled goodies
(setq my-package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(setq package-archives
      (append package-archives
	      my-package-archives))

;; initialize all "installed" packages
(package-initialize)
;; avoid extra call to (package-initialize) after loading init.el
(setq package-enable-at-startup nil)

;; bootstrap use-package, a macro to keep configuration
;; (e.g. in this file) organized
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))


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

  (define-key evil-insert-state-map
              (kbd "M-<tab>")
	      'elpy-company-backend)
  (define-key evil-motion-state-map
              (kbd "SPC")
              'my-jump-down)
  (define-key evil-motion-state-map
              (kbd "C-SPC")
              'my-jump-up)
  (define-key evil-motion-state-map
              (kbd "<backspace>")
              'my-jump-up)
  (define-key evil-motion-state-map
              (kbd "C-S-e")
              'my-scroll-down)
  (define-key evil-motion-state-map
              (kbd "C-S-y")
              'my-scroll-up)
  (define-key (current-global-map)
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
  :bind ("M-x" . counsel-M-x)
  :bind ("C-s" . swiper)
  :bind ("C-h f" . counsel-describe-function)
  :bind ("C-h v" . counsel-describe-variable)
  :bind ("C-h b" . counsel-descbinds)
  :bind ("C-x C-f" . counsel-find-file)
  :bind ("C-c k" . counsel-unicode-char)
  :bind ("s-o" . ivy-occur)
  :config
  (ivy-mode 1)
  ;; use fuzzy-style matching in all cases except swiper (from SX)
  ;; (setq ivy-re-builders-alist
  ;; 	'((swiper . ivy--regex-plus)
  ;; 	  (t . ivy--regex-fuzzy)))
  (setq ivy-wrap t))

(use-package magit
  :config
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
  (global-set-key (kbd "C-S-c C-S-c") 'evil-mc-make-cursor-here)
  (global-set-key (kbd "C->") 'evil-mc-make-and-goto-next-match)
  (global-set-key (kbd "C-<") 'evil-mc-make-and-goto-prev-match)
  (global-set-key (kbd "C-c C-<") 'evil-mc-make-all-cursors)
  (global-set-key (kbd "C-c C->") 'evil-mc-undo-all-cursors)
  ;; for sublime emulation
  (global-set-key (kbd "s-d") 'evil-mc-make-and-goto-next-match)
  (global-set-key (kbd "s-D") 'evil-mc-make-and-goto-prev-match)
  (global-set-key (kbd "s-<escape>") 'evil-mc-undo-all-cursors))

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


;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL BEHAVIOR ;;
;;;;;;;;;;;;;;;;;;;;;;


;; remove the toolbar at the top of the window
(tool-bar-mode -1)

;; refresh files from disk if there are changes
(global-auto-revert-mode t)

;; save autosave files in emacs folder instead of locally
;; in the folder containing the files being edited
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; show diffs side-by-side
(setq ediff-split-window-function 'split-window-horizontally)

;; evil matchit mode loads automatically for html-mode and sgml-mode
;; but not the derivative mhtml-mode, which is actually the default
;; mode for HTML. manually enable it here for mhtml mode (probably figure
;; out a cleaner way to do this, e.g. by adding mhtml to the modes
;; that trigger autoloading, or via hooks for mhtml mode)
(plist-put evilmi-plugins 'mhtml-mode '((evilmi-simple-get-tag evilmi-simple-jump)
					(evilmi-html-get-tag evilmi-html-jump)))

;;;;;;;;;;;;;;;;;;;
;; LOOK AND FEEL ;;
;;;;;;;;;;;;;;;;;;;


;; appearance
(load-theme 'tango-dark t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(set-frame-font "Menlo 12" nil t)

;; line numbers on by default
(global-linum-mode 1)
;; show (line #, column #) in mode line
(setq column-number-mode t)

;; cool transparency [from emacswiki]
;; the alpha params are "active" and "inactive" frame
;; this is if you want the in-focus and not-in-focus
;; emacs frames to have different transparencies
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;


(defun my-count-lines-page ()
  "Modified from emacs's built-in count-lines-page to return a list of
   values corresponding to the position in the page."
  (interactive)
  (save-excursion
    (let ((opoint (point)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
	    before (count-lines beg opoint)
	    after (count-lines opoint end))
      (list total before after))))

(defun my-buffer-info ()
  "get info on current buffer -- similar to Vim's C-g"
  (interactive)
  (-let [(total before after) (my-count-lines-page)]
    (if (= total 0)
	(setq bufinfo (list "-- No lines in buffer --"))
      (progn (setq percentage (floor (* (/ (float before)
					   total)
					100)))
	     (setq page-position (concat
				  "-- "
				  (number-to-string percentage)
				  "%"
				  " --"))
	     (setq total-lines (concat
				(number-to-string total)
				" lines"))
	     (setq bufinfo (list total-lines page-position))))
    (add-to-list 'bufinfo
		 (buffer-file-name))
    (display-message-or-buffer (string-join bufinfo " "))))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
   New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

   It returns the buffer (for elisp programing).

   URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
   Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(defun my-lisp-repl ()
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1)
  (ielm))


;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; map Mac's Command key to Emacs/Lisp's Super key
(setq mac-command-modifier 'super)
;; make Fn key do Hyper [coz, why not]
(setq mac-function-modifier 'hyper)

; Note: "define-key (current-global-map)" is the same as global-set-key

(define-key (current-global-map)
            (kbd "C-c b")
            'my-buffer-info)
(define-key (current-global-map)
            (kbd "C-c t")
            'sr-speedbar-toggle)
(define-key (current-global-map)
            (kbd "C-c n")
            'xah-new-empty-buffer)
(define-key (current-global-map)
            (kbd "C-c s")
            'eshell)
(define-key (current-global-map)
            (kbd "C-c d")
            'dictionary-lookup-definition)
(define-key (current-global-map)
            (kbd "C-c l")
            'my-lisp-repl)
(define-key (current-global-map)
            (kbd "s-}")
            'mac-next-tab)
(define-key (current-global-map)
            (kbd "s-{")
            'mac-previous-tab)
(define-key (current-global-map)
            (kbd "C-<escape>")
            'caps-lock-mode)
;; TODO: the following keybindings for tabs
;; These don't work at the moment since I'm not able
;; to locate the required functions
;; (define-key (current-global-map)
;;             (kbd "s-t")
;;             'mac-make-tab)
;; (define-key (current-global-map)
;;             (kbd "s-w")
;;             'mac-close-tab)
(define-key (current-global-map)
            (kbd "s-w")
            'delete-window)
(define-key (current-global-map)
            (kbd "s-s")
            'save-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATION VIA "UI" BELOW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(mac-option-modifier (quote meta))
 '(package-selected-packages
   (quote
    (caps-lock smex counsel yasnippet-snippets yasnippet evil-mc multiple-cursors minimap evil-matchit evil-tabs tabbar projectile evil-magit php-mode ivy sicp company-jedi company sr-speedbar magit dictionary sublimity evil elpy)))
 '(python-check-command "/usr/local/bin/pyflakes"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
