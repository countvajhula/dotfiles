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

;; save any customizations done via Customize or set automatically by
;; various emacs modes to a designated file, instead of cluttering init.el
(setq custom-file (concat user-customizations-directory "custom.el"))
(load custom-file 'noerror)

;; show diffs side-by-side
(setq ediff-split-window-function 'split-window-horizontally)

;; evil matchit mode loads automatically for html-mode and sgml-mode
;; but not the derivative mhtml-mode, which is actually the default
;; mode for HTML. manually enable it here for mhtml mode (probably figure
;; out a cleaner way to do this, e.g. by adding mhtml to the modes
;; that trigger autoloading, or via hooks for mhtml mode)
(plist-put evilmi-plugins 'mhtml-mode '((evilmi-simple-get-tag evilmi-simple-jump)
					(evilmi-html-get-tag evilmi-html-jump)))

;; tab width of 4
(setq tab-stop-list (number-sequence 4 120 4))
(setq-default tab-width 4)


;;;;;;;;;;;;;;;;;;;
;; LOOK AND FEEL ;;
;;;;;;;;;;;;;;;;;;;


;; appearance
(add-to-list 'custom-theme-load-path (concat user-emacs-directory
					     "themes/"))
;; some themes have several variations (e.g. light and dark)
;; and share code between these variations in common elisp modules;
;; these modules need to be on the load path so that these themes work
(add-to-list 'load-path (concat user-emacs-directory
				"themes/"))
(load-theme 'tango-dark t)
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
