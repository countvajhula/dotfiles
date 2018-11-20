;;; TODO: ideally, would be good to have a simple POC of the AST
;;; to operate on, via semantic?
;;; TODO: consider using S for dragging and C for movement (and then across all modes)
;;; TODO: get rid of whitespace when deleting things
;;; TODO: f b for forward back using tree traversal
;;; TODO: y should preserve newlines if the symex defines an indent level, and add a newline if the symex is a leaf
;;; TODO: p/P should always add a space before/after the pasted symex
;;; TODO: newlines should indent affected symexes
;;; TODO: move back/forward through tree "at same level" without going up or down (i.e. switch branches, ideally preserving position index within branch)
;;; TODO: traverse tree with side effect (traversal-method, side-effect-fn), to use for "indent forward" on paste
;;; TODO: incorporate more clear tree-related terminology
(use-package lispy)
(use-package paredit)
(use-package evil-cleverparens)  ;; really only need cp-textobjects here

(defun my-evaluate-symex ()
  "Evaluate Symex"
  (interactive)
  (save-excursion
    (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-eval-symex))
          ((member major-mode elisp-modes)
           (my-elisp-eval-symex))
          ((equal major-mode 'scheme-mode)
           (my-scheme-eval-symex))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun my-evaluate-definition ()
  "Evaluate top-level definition"
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-send-definition nil))
        ((member major-mode elisp-modes)
         (eval-defun nil))
        ((equal major-mode 'scheme-mode)
         (geiser-eval-definition nil))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun lispy-comment-line-p ()
  "Checks if we're currently at the start of a comment line."
  (and (lispy-bolp)
       (looking-at ";")))

(defun lispy-empty-list-p ()
  "Checks if we're looking at an empty list."
  (save-excursion
    (and (lispy-left-p)
         (progn (forward-char 2) ;; need to go forward by 2 for some reason
                (lispy-right-p)))))

(defun my-forward-symex ()
  "Forward symex"
  (interactive)
  (if (not (thing-at-point 'sexp))
      (forward-sexp 1)
    (forward-sexp 2))
  (backward-sexp 1)
  (recenter)
  (point))

(defun my-backward-symex ()
  "Backward symex"
  (interactive)
  (backward-sexp 1)
  (recenter)
  (point))

(defun my-enter-symex ()
  "Enter lower symex level."
  (interactive)
  (cond ((lispy-comment-line-p)
         (lispy-flow 1))
        ((and (lispy-left-p)
              (not (lispy-empty-list-p)))
         (forward-char)))
  (point))

(defun my-exit-symex ()
  "Exit to higher symex level"
  (interactive)
  (paredit-backward-up 1)
  (point))

(defun my-select-nearest-symex ()
  "Select symex nearest to point"
  (interactive)
  (cond ((save-excursion (forward-char) (lispy-right-p))
         (forward-char)
         (lispy-different))
        ((thing-at-point 'sexp)
         (beginning-of-thing 'sexp))
        (t (condition-case nil
               (progn (forward-sexp 1)
                      (beginning-of-thing 'sexp))
             (error (condition-case nil
                        (backward-sexp 1)
                      (error nil))))))
  (recenter)
  (point))

(defun my-describe-symex ()
  "Lookup doc on symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-describe-symbol))
          ((member major-mode elisp-modes)
           (my-elisp-describe-symbol))
          ((equal major-mode 'scheme-mode)
           (my-scheme-describe-symbol))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun my-goto-first-symex ()
  "Select first symex at present level"
  (interactive)
  (let ((previous-position (point)))
    (my-backward-symex)
    (setq current-position (point))
    (while (not (= previous-position current-position))
      (setq previous-position current-position)
      (my-backward-symex)
      (setq current-position (point))))
  (recenter)
  (point))

(defun my--goto-last ()
  "Helper function to go as far as possible."
  (let ((previous-position (point)))
    (my-forward-symex)
    (setq current-position (point))
    (while (not (= previous-position current-position))
      (setq previous-position current-position)
      (my-forward-symex)
      (setq current-position (point)))
    current-position))

(defun my-goto-last-symex ()
  "Select last symex at present level"
  (interactive)
  (my--goto-last)
  (recenter)
  (point))

(defun my-goto-outermost-symex ()
  "Select outermost symex."
  (interactive)
  (let ((previous-position (point)))
    (my-exit-symex)
    (setq current-position (point))
    (while (not (= previous-position current-position))
      (setq previous-position current-position)
      (my-exit-symex)
      (setq current-position (point))))
  (recenter)
  (point))

(defun my--go-deep ()
  "Helper function to go as deep as possible."
  (let ((previous-position (point)))
    (my-enter-symex)
    (setq current-position (point))
    (while (not (= previous-position current-position))
      (setq previous-position current-position)
      (my-enter-symex)
      (setq current-position (point)))
    current-position))

(defun my--goto-innermost ()
  "Helper function to traverse deep and forward until it reaches steady state."
  (let* ((previous-position (point))
         (current-position (my--go-deep)))
    (if (= current-position previous-position)
        (let ((current-position (my-forward-symex)))
          (unless (= previous-position current-position)
            (my--goto-innermost)))
      (let ((current-position (my-forward-symex)))
        (my--goto-innermost))))
  (point))

(defun my-goto-innermost-symex ()
  "Select innermost symex."
  (interactive)
  (my--goto-innermost)
  (recenter)
  (point))

(defun my-delete-symex ()
  "Delete symex"
  (interactive)
  (if (lispy-left-p)
      (progn (sp-copy-sexp)
             (lispy-delete 1))
    (kill-sexp 1))
  (my-select-nearest-symex))

(defun my-change-symex ()
  "Change symex"
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun my-barf-backward ()
  "Barf backward"
  (interactive)
  (my-enter-symex) ;; need to be inside the symex to barf and slurp
  (lispy-backward-barf-sexp 1)
  (my-forward-symex))

(defun my-barf-forward ()
  "Barf forward"
  (interactive)
  (save-excursion
    (my-enter-symex)  ;; need to be inside the symex to barf and slurp
    (lispy-forward-barf-sexp 1)))

(defun my-slurp-backward ()
  "Slurp from behind"
  (interactive)
  (my-enter-symex)  ;; need to be inside the symex to barf and slurp
  (lispy-backward-slurp-sexp 1)
  (my-exit-symex))

(defun my-slurp-forward ()
  "Slurp from the front"
  (interactive)
  (save-excursion
    (my-enter-symex)  ;; need to be inside the symex to barf and slurp
    (lispy-forward-slurp-sexp 1)))

(defun my-indent-symex ()
  "Auto-indent symex"
  (interactive)
  (apply 'evil-indent
         (seq-take (evil-cp-a-form 1)
                   2)))

(defun my-join-symexes ()
  "Merge symexes at the same level."
  (interactive)
  (save-excursion
    (my-forward-symex)
    (paredit-join-sexps)))

(defun my-symex-join-lines ()
  "Join lines inside symex."
  (interactive)
  (save-excursion
    (evil-join (line-beginning-position)
               (line-end-position))))

(defun my-yank-symex ()
  "Yank (copy) symex."
  (interactive)
  (lispy-new-copy))

(defun my-paste-before-symex ()
  "Paste before symex"
  (interactive)
  (cond ((or (and (point-at-indentation-p)
                  (not (bolp)))
             (save-excursion (forward-sexp)
                             (eolp)))
         (setq extra-to-append "\n"))
        (t (setq extra-to-append " ")))
  (with-undo-collapse
    (save-excursion
      (save-excursion
        (evil-paste-before nil nil)
        (forward-char)
        (insert extra-to-append))
      (my-forward-symex)
      (my-indent-symex))))

(defun my-paste-after-symex ()
  "Paste after symex"
  (interactive)
  (cond ((or (and (point-at-indentation-p)
                  (not (bolp)))
             (save-excursion (forward-sexp)
                             (eolp)))
         (setq extra-to-prepend "\n"))
        (t (setq extra-to-prepend " ")))
  (with-undo-collapse
    (save-excursion
      (save-excursion
        (forward-sexp)
        (insert extra-to-prepend)
        (evil-paste-before nil nil)
        (forward-char))
      (my-forward-symex)
      (my-indent-symex))
    (my-forward-symex)))

(defun my-open-line-after-symex ()
  "Open new line after symex"
  (interactive)
  (forward-sexp)
  (newline-and-indent)
  (evil-insert-state))

(defun my-open-line-before-symex ()
  "Open new line before symex"
  (interactive)
  (newline-and-indent)
  (evil-previous-line)
  (indent-according-to-mode)
  (evil-append-line 1))

(defun my-append-after-symex ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
  (evil-insert 1 nil nil))

(defun my-insert-before-symex ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (evil-insert 1 nil nil))

(defun my-insert-at-beginning-of-symex ()
  "Insert at beginning of symex."
  (interactive)
  (if (lispy-left-p)
      (evil-append 1 nil)
    (evil-insert 1 nil nil)))

(defun my-insert-at-end-of-symex ()
  "Insert at end of symex."
  (interactive)
  (if (lispy-left-p)
      (progn (forward-sexp)
             (backward-char)
             (evil-insert 1 nil nil))
    (progn (forward-sexp)
           (evil-insert 1 nil nil))))

(defun my-create-symex (type)
  "Create new symex (list)."
  (interactive)
  (save-excursion
    (cond ((equal type 'round)
           (insert "()"))
          ((equal type 'square)
           (insert "[]"))
          ((equal type 'curly)
           (insert "{}"))
          ((equal type 'angled)
           (insert "<>")))))

(defun my-insert-symex-newline ()
  "Insert newline and reindent symex."
  (interactive)
  (newline)
  (my-indent-symex))

(defun my-swallow-symex ()
  "Swallow symex, putting its contents in the parent symex."
  (interactive)
  (lispy-flow 1)
  (paredit-splice-sexp-killing-backward))


(defhydra hydra-symex (:idle 1.0
                       :columns 5
                       :color pink
                       :body-pre (progn (my-select-nearest-symex)
                                        (evil-symex-state))
                       :post (evil-normal-state))
  "Symex mode"
  ("(" (lambda ()
         (interactive)
         (my-create-symex 'round)) "()")
  ("[" (lambda ()
         (interactive)
         (my-create-symex 'square)) "[]")
  ("{" (lambda ()
         (interactive)
         (my-create-symex 'curly)) "{}")
  ("<" (lambda ()
         (interactive)
         (my-create-symex 'angled)) "<>")
  ("h" my-backward-symex "previous")
  ("k" my-backward-symex "previous")
  ("C-k" my-exit-symex "exit")
  ("C-h" my-exit-symex "exit")
  ("C-j" my-enter-symex "enter")
  ("C-l" my-enter-symex "enter")
  ("j" my-forward-symex "next")
  ("l" my-forward-symex "next")
  ("f" lispy-flow "flow forward")
  ("y" my-yank-symex "yank (copy)")
  ("p" my-paste-after-symex "paste after")
  ("P" my-paste-before-symex "paste before")
  ("x" my-delete-symex "delete")
  ("c" my-change-symex "change" :exit t)
  ("H" lispy-move-up "move backward")
  ("K" lispy-move-up "move backward")
  ("J" lispy-move-down "move forward")
  ("L" lispy-move-down "move forward")
  ("C-S-s-k" paredit-raise-sexp "raise")
  ("C-S-j" my-slurp-backward "slurp backward")
  ("C-S-h" my-barf-backward "barf backward")
  ("C-S-l" my-barf-forward "barf forward")
  ("C-S-k" my-slurp-forward "slurp forward")
  ("z" my-swallow-symex "swallow")
  ("e" my-evaluate-symex "evaluate")
  ("E" my-evaluate-definition "evaluate definition")
  ("d" my-evaluate-definition)
  ("s" lispy-split "split")
  ("m" my-join-symexes "merge (join)")
  ("\\" lispy-splice "splice (join to higher level)")
  (")" paredit-wrap-round "wrap with ()")
  ("]" paredit-wrap-square "wrap with []")
  ("}" paredit-wrap-curly "wrap with {}")
  (">" paredit-wrap-angled "wrap with <>")
  ("o" my-open-line-after-symex "newline after" :exit t)
  ("O" my-open-line-before-symex "newline before" :exit t)
  ("n" my-insert-symex-newline "newline")
  ("N" my-symex-join-lines "merge (join) lines")
  ("0" my-goto-first-symex "go to first")
  ("H-h" my-goto-first-symex "go to first")
  ("$" my-goto-last-symex "go to last")
  ("H-l" my-goto-last-symex "go to last")
  ("M-C-k" my-goto-outermost-symex "go to outermost")
  ("M-C-j" my-goto-innermost-symex "go to innermost")
  ("=" my-indent-symex "auto-indent")
  ("A" my-append-after-symex "append after symex" :exit t)
  ("a" my-insert-at-end-of-symex nil :exit t)
  ("i" my-insert-at-beginning-of-symex nil :exit t)
  ("I" my-insert-before-symex nil :exit t)
  ;; escape hatches
  ("r" evil-replace nil :exit t)
  ("R" evil-replace-state nil :exit t)
  ("v" evil-visual-char nil :exit t)
  ("V" evil-visual-line nil :exit t)
  ("C-v" evil-visual-block nil :exit t)
  ;; standard exits
  ("?" my-describe-symex "info")
  ("<escape>" nil "exit" :exit t)
  ("s-<return>" hydra-word/body "enter lower level" :exit t)
  ("s-<escape>" hydra-view/body "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)  ;; since y looks like inverted lambda
(global-set-key (kbd "s-;") 'hydra-symex/body)  ;; since y is hard to reach

(provide 'my-symex-mode)
