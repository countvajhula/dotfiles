;;
;; initial setup
;;

(require 'package)

;; add some standard package repos with lots of non-bundled goodies
(setq my-package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(setq package-archives
      (append package-archives
	      my-package-archives))

;; initialize all "installed" packages
(package-initialize)

;; other packages
(require 'sublimity-scroll)

;; remove the toolbar at the top of the window
(tool-bar-mode -1)

;;
;; general behavior
;;

;; appearance

(load-theme 'tango-dark t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(set-frame-font "Menlo 12" nil t)

;; refresh files from disk if there are changes
(global-auto-revert-mode t)

;; save autosave files in emacs folder instead of locally
;; in the folder containing the files being edited
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;;
;; choose package defaults
;;

;; Vim interface
(evil-mode 1)
;; python IDE
(elpy-enable)
(setq elpy-modules
      (remove 'elpy-module-highlight-indentation
	      elpy-modules))
;; use jedi for completion with elpy instead of rope
(setq elpy-rpc-backend "jedi")
;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;(sublimity-mode 1)
;; enable company mode autocompletion in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;;
;; navigation optimizations
;;

;; line numbers on by default
(global-linum-mode 1)
;; show (line #, column #) in mode line
(setq column-number-mode t)

(with-eval-after-load 'evil-maps

  ;; use Emacs keybindings when in insert mode }:)
  (setcdr evil-insert-state-map nil)
  ;; Esc goes to normal mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (defun my-scroll-down ()
    (interactive)
    (dotimes (i 10)
      (evil-next-line))
    (evil-scroll-line-to-center nil))

  (defun my-scroll-up ()
    (interactive)
    (dotimes (i 10)
      (evil-previous-line))
    (evil-scroll-line-to-center nil))

  (define-key evil-motion-state-map (kbd "SPC") 'my-scroll-down)
  (define-key evil-motion-state-map (kbd "C-SPC") 'my-scroll-up)
  (define-key evil-motion-state-map (kbd "<backspace>") 'my-scroll-up))

;;
;; Personal customizations
;;

;; get info on current buffer -- similar to Vim's C-g
;; TODO: improve
(defun my-buf-info ()
  (interactive)
  (setq bufinfo (buffer-file-name))
  (add-to-list 'bufinfo (count-lines-page))
  (print bufinfo))

(defun my-new-buffer-window ()
  (interactive)
  (scratch)
  (delete-other-windows))

(global-set-key (kbd "C-c C-b") 'my-buf-info)
(global-set-key (kbd "C-c C-t") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c C-n") 'my-new-buffer-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(mac-option-modifier (quote meta))
 '(package-selected-packages
   (quote
    (ivy sicp company-jedi company sr-speedbar magit dictionary sublimity evil elpy)))
 '(python-check-command "/usr/local/bin/pyflakes"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
