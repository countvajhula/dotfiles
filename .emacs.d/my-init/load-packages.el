;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES AND CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;; convenient list- and functional-related macros
(use-package dash)

;; used in some (third-party) custom themes
;; (not sure if there's a better way to indicate this dependency)
(use-package autothemer)

;; navigation sidebar
(use-package sr-speedbar)

;; intuitive "state machine" menus
(use-package hydra)

;; handy project-related functions like grep search, find file, etc.
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; for sublime emulation:
  ;; (define-key projectile-mode-map
  ;;             (kbd "s-p")
  ;;             'projectile-find-file)
  (defhydra hydra-projectile (:color teal
                              :hint nil)
    "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_f_: file            _s_: search (grep)     _i_: Ibuffer           _c_: cache clear
_e_: file dwim       _r_: replace           _b_: switch to buffer  _x_: remove known project
_l_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
_t_: recent file     _j_: jump to tag                            ^^^^_z_: cache current
_d_: dir             _g_: update gtags

"
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("e"   projectile-find-file-dwim)
    ("f"   projectile-find-file)
    ("s-f" projectile-find-file)
    ("l"   projectile-find-file-in-directory)
    ("j"   projectile-find-tag)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("s-p" projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("r"   projectile-replace)
    ("s"   projectile-grep)
    ("s-s" projectile-grep)
    ("t"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue))
  (define-key (current-global-map)
              (kbd "s-p")
              'hydra-projectile/body)
  (define-key (current-global-map)
              (kbd "s-F")
              'projectile-grep))

;; Vim interface
(use-package evil
  :init
  ;; these settings are required by evil-collection
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; use Emacs keybindings when in insert mode }:)
  (setcdr evil-insert-state-map nil)
  ;; Esc goes to normal mode in "insert" (emacs) mode
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; C-z goes to emacs mode in "insert" mode
  (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
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

  ;; Vim C-x line completion emulation,
  ;; from https://stackoverflow.com/questions/17928467/full-line-completion-in-emacs
  (defun my-expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list
	   '(try-expand-line)))
      (call-interactively 'hippie-expand)))

  (define-key evil-insert-state-map (kbd "C-x C-l") 'my-expand-lines)

  (define-key
    ;; alternative to Vim's C-u (since emacs reserves C-u)
    evil-motion-state-map
    (kbd "C-S-d")
    'evil-scroll-up)
  (define-key
    ;; re-bind elpy completion in evil insert mode
    ;; which apparently gets overridden by evil
    evil-insert-state-map
    (kbd "M-<tab>")
    'elpy-company-backend)
  (define-key
    ;; handy navigation to jump down the file
    evil-motion-state-map
    (kbd "C-s-j")
    'my-jump-down)
  (define-key
    ;; handy navigation to jump up the file
    evil-motion-state-map
    (kbd "C-s-k")
    'my-jump-up)
  (define-key
    ;; handy navigation to jump up the file
    evil-motion-state-map
    (kbd "<backspace>")
    'my-jump-up)
  (define-key
    ;; scroll down the file a little faster than usual
    evil-motion-state-map
    (kbd "C-e")
    'my-scroll-down)
  (define-key
    ;; scroll up the file a little faster than usual
    evil-motion-state-map
    (kbd "C-y")
    'my-scroll-up)
  (define-key
    ;; remap original vim scroll bindings as "fine tuning"
    ;; rather than default scroll behavior
    evil-motion-state-map
    (kbd "C-S-e")
    'evil-scroll-line-down)
  (define-key
    ;; remap original vim scroll bindings as "fine tuning"
    ;; rather than default scroll behavior
    evil-motion-state-map
    (kbd "C-S-y")
    'evil-scroll-line-up)
  (define-key
    ;; standard alternative keybinding to search file
    (current-global-map)
    (kbd "s-f")
    'evil-search-forward))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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
  (add-hook 'python-mode-hook
			(lambda () (show-paren-mode 1)))
  (add-hook 'python-mode-hook
			(lambda ()
			  (setq tab-width 4)
			  (setq python-indent-offset 4))))

;; ido mode
(use-package ido
  ;; disabled since using ivy
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
  :config
  (ivy-mode 1)
  ;; use fuzzy-style matching in all cases except swiper (from SX)
  ;; (setq ivy-re-builders-alist
  ;; 	'((swiper . ivy--regex-plus)
  ;; 	  (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t))

(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :bind ("C-c k" . counsel-unicode-char)
  :config
  (counsel-mode 1))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy-rich
  :config
  (ivy-rich-mode t))

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
  ;; by default evil-mc creates its own keymap that overrides some
  ;; useful evil mode bindings (like C-p!). Disable all default
  ;; evil-mc keybindings here (by making the keymap empty), in favor
  ;; of a custom hydra (defined below)
  (setcdr evil-mc-key-map nil)
  (global-evil-mc-mode 1)
  ;; interface with multiple cursors via a hydra menu
  (defhydra hydra-cursors (:columns 2)
    "Multiple cursors"
    ("a" evil-mc-make-all-cursors "make all cursors")
    ("n" evil-mc-make-and-goto-next-match "mark, go to next")
    ("N" evil-mc-make-and-goto-prev-match "mark, go to previous")
    ("s" evil-mc-skip-and-goto-next-match "skip, go to next")
    ("S" evil-mc-skip-and-goto-prev-match "skip, go to previous")
    ("h" evil-mc-make-cursor-here  "make cursor here")
    ("p" evil-mc-pause-cursors "pause cursors")
    ("P" evil-mc-resume-cursors "resume cursors")
    ("j" evil-mc-make-cursor-move-next-line "mark, go down")
    ("k" evil-mc-make-cursor-move-prev-line "mark, go up")
    ("f" evil-mc-make-and-goto-first-cursor "mark, goto first cursor")
    ("F" evil-mc-make-and-goto-last-cursor "mark, goto last cursor")
    ("l" evil-mc-make-and-goto-next-cursor "mark, goto next cursor")
    ("L" evil-mc-skip-and-goto-next-cursor "skip, goto next cursor")
    ("h" evil-mc-make-and-goto-prev-cursor "mark, goto previous cursor")
    ("H" evil-mc-skip-and-goto-prev-cursor "skip, goto previous cursor")
    ("<escape>" evil-mc-undo-all-cursors "undo all cursors"))

  ;; access the multiple-cursors menu via a "body" keybinding
  (global-set-key (kbd "s-d") 'hydra-cursors/body)
  ;; retain a convenient, non-hydra, escape hatch
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

;; not sure why this is necessary, but this initializes sunrise
;; commander, along with all of its extensions
(el-get-bundle sunrise-commander)

(use-package avy
  ;; although avy-goto-word-1 is probably faster,
  ;; avy-goto-char-timer is more intuitive
  :bind ("M-s" . avy-goto-char-timer)
  :ensure t)

(use-package ace-jump-buffer
  :disabled t
  :bind ("C-x b" . ace-jump-buffer))

(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 25))

(use-package popwin
  :disabled t
  :config
  (popwin-mode t))

(use-package ibuffer
  ;; replace oldschool buffer-list, as recommended here:
  ;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
	    '(lambda ()
	       (ibuffer-auto-mode 1)))
	       ;;(ibuffer-switch-to-saved-filter-groups "default"))))
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-vc
  ;; organize buffers by version-controlled repo
  :disabled t
  :init
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-vc-set-filter-groups-by-vc-root)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-sidebar
  :disabled t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :bind ("C-c b" . ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))

(use-package smart-mode-line
  :disabled t
  :config
  (sml/setup)
  (setq sml/theme 'dark))

(use-package telephone-line
  :config
  (telephone-line-mode t))

(use-package org-ibuffer
  :load-path "~/.emacs.d/my-packages/")

(use-package org
  :after org-ibuffer
  :config
  (setq org-agenda-files '("~/log/org/"))
  (setq org-todo-keywords
	'((sequence "[ ](t)" "[\\](w)" "[o](o)" "[-](-)" "[x](x)")))
  ;; interface with org-mode via a hydra menu
  (defhydra hydra-org ()
    "Org-mode Menu"
    ("l" org-store-link "store link")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("t" org-todo "todo")
    ("i" org-ibuffer "view all open org buffers")
    ("b" org-switchb "org switch buffer"))

  ;; nest this hydra inside the previous one under `a`
  (defhydra hydra-org-agenda ()
    "Agenda"
    ("a" org-agenda "agenda")
    ("+" org-agenda-file-to-front "add file to agenda")
    ("-" org-remove-file "remove file from agenda")
    ("b" org-switchb "org switch buffer"))

  ;; access the org-mode menu via a "body" keybinding
  (global-set-key (kbd "s-o") 'hydra-org/body))

;; cozy time
(use-package fireplace)

(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; this variable affects "blocking" hints, for example when deleting - the hint is displayed,
  ;; the deletion is delayed (blocked) until the hint disappers, then the hint is removed and the
  ;; deletion executed; it makes sense to have this duration short
  (setq evil-goggles-blocking-duration 0.010) ;; default is nil, i.e. use `evil-goggles-duration' which defaults to 0.200

  ;; this variable affects "async" hints, for example when indenting - the indentation
  ;; is performed with the hint visible, i.e. the hint is displayed, the action (indent) is
  ;; executed (asynchronous), then the hint is removed, highlighting the result of the indentation
  (setq evil-goggles-async-duration 0.360) ;; default is nil, i.e. use `evil-goggles-duration' which defaults to 0.200

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package tab-indentation
  :load-path "~/.emacs.d/my-packages/")

(use-package general
  ;; general is a package that provides various
  ;; resources and utilities for defining keybindings
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)

  (defhydra hydra-leader (:idle 1.0
                          :columns 2
                          :exit t)
    "Quick actions"
    ("a" org-agenda "Org agenda")
    ("s" eshell "Shell")
    ("t" sr-speedbar-toggle "Nav Sidebar")
    ("u" undo-tree-visualize "Undo tree")
    ("d" dictionary-lookup-definition "lookup in dictionary"))

  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'hydra-leader/body))

;; local packages for other custom configuration; managing this
;; via use-package gives us some flexibility and modularity

(use-package sid-general-behavior)

(use-package sid-look-and-feel)

(use-package sid-utils)

(use-package sid-keybindings)
