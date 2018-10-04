(setq user-home-directory (getenv "HOME"))
(setq user-customizations-directory (concat user-emacs-directory
                                            "my-init/"))
(setq user-packages-directory (concat user-emacs-directory
                                      "my-packages/"))
(add-to-list 'load-path user-customizations-directory)
(add-to-list 'load-path user-packages-directory)
(load "setup")
(load "load-packages")
(load custom-file 'noerror)
