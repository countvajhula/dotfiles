(defun setup-load-path ()
  "Set up load path for the initialization process"
  (progn (setq user-home-directory (getenv "HOME"))
         (setq user-customizations-directory (concat user-emacs-directory
                                                     "my-init/"))
         (setq user-packages-directory (concat user-emacs-directory
                                               "my-packages/"))
         (add-to-list 'load-path user-customizations-directory)
         (add-to-list 'load-path user-packages-directory)
         ;; el-get -- an alternative package manager to ELPA/MELPA,
         ;; used for packages not on ELPA/MELPA
         (add-to-list 'load-path (concat user-emacs-directory
                                         "el-get/el-get"))))

(setup-load-path)

;; initialize package managers and installed packages
(load "setup")
;; load any local general-purpose utilities
(load "utils")

;; load all configured packages
(load "task-environment")
(load "project-environment")
(load "application-environment")
(load "system-environment")
(load "network-environment")
(load "physical-environment")

;; load any customizations done via EMACS UI
(load custom-file 'noerror)
