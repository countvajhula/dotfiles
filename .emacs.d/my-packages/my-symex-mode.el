;;; -*- lexical-binding: t -*-
;;; TODO: ideally, would be good to have a simple POC of the AST
;;; to operate on, via semantic?
;;; TODO: consider using S for dragging and C for movement (and then across all modes)
;;; TODO: f b for forward back using tree traversal
;;; TODO: move back/forward through tree "at same level" without going up or down (i.e. switch branches, ideally preserving position index within branch)
;;; TODO: traverse tree with side effect (traversal-method, side-effect-fn), to use for "indent forward" on paste
;;; TODO: incorporate more clear tree-related terminology
;;; TODO: improve move backward / forward, H L
;;; TODO: C-j to move in greedily, going forward
;;; TODO: fix: backward-symex moves to preamble comments
;;; TODO: handle "contracts" of each abstraction level, and where conditions should go, rename functions for clarity. legitimate detours vs conditional itineraries, vs conditional motions
;;; TODO: detours should be maneuvers. define a strategy as a higher-level sequence of maneuvers, where each is tried in sequence until all fail, beginning again from the first on success
;;; TODO: take a symex and bring it out and before/after as a peer of the parent
(use-package lispy)
(use-package paredit)
(use-package evil-cleverparens)  ;; really only need cp-textobjects here
(require 'cl-lib)


(defun my-make-move (x y &optional pre-condition post-condition)
  (list x y pre-condition post-condition))

(defun my-move-x (move)
  "X (horizontal) component of move."
  (nth 0 move))

(defun my-move-y (move)
  "Y (vertical) component of move."
  (nth 1 move))

(defun my-move-pre-condition (move)
  "Pre-condition of move."
  (nth 2 move))

(defun my-move-post-condition (move)
  "Post-condition of move."
  (nth 3 move))

(defvar move-zero (my-make-move 0 0))
(defvar move-go-forward (my-make-move 1 0))
(defvar move-go-backward (my-make-move -1 0))
(defvar move-go-in (my-make-move 0 1))
(defvar move-go-out (my-make-move 0 -1))
(defvar move-go-out-avoid-root (my-make-move 0 -1
                                             nil
                                             (lambda ()
                                               (not (point-at-root-symex?)))))
(defvar move-go-out-avoid-eob (my-make-move 0 -1
                                            nil
                                            (lambda ()
                                              (not (point-at-final-symex?)))))

(defun is-null-move? (move)
  "Checks if the move specifies no movement."
  (are-moves-equivalent? move move-zero))

(defun move-exists? (move)
  "Checks if the move specifies tangible movement."
  (not (is-null-move? move)))

(defun are-moves-equal? (m1 m2)
  "Check if two moves are identical, including any conditions."
  (equal m1 m2))

(defun naive-move (move)
  "A 'naive' version of the move, not including any conditions."
  (apply 'my-make-move (seq-take move 2)))

(defun are-moves-equivalent? (m1 m2)
  "Check if two moves are equal, disregarding any conditions."
  (are-moves-equal? (naive-move m1)
                    (naive-move m2)))

(cl-defun my-make-maneuver (moves &key repeating?)
  "Construct a maneuver from the given moves."
  (list moves repeating?))

(defun my-maneuver-moves (maneuver)
  "Get the moves for a maneuver"
  (nth 0 maneuver))

(defun my-maneuver-repeating? (maneuver)
  "Whether the maneuver is repeating or not."
  (nth 1 maneuver))

(iter-defun my-maneuver-begin (maneuver)
  "Begin maneuver."
  (let* ((moves (apply 'vector (my-maneuver-moves maneuver)))
         (n (length moves))
         (i 0))
    (catch 'done
      (while t
        (iter-yield (elt moves i))
        (if (my-maneuver-repeating? maneuver)
            (setq i (% (+ i 1)
                       n))
          (setq i (+ i 1))
          (when (= i n)
            (throw 'done t)))))))

(defvar maneuver-zero (my-make-maneuver (list move-zero)))

(defun is-null-maneuver? (maneuver)
  "Checks if the maneuver specifies no movement."
  (are-maneuvers-equivalent? maneuver
                             maneuver-zero))

(defun maneuver-exists? (maneuver)
  "Checks if the maneuver is non-zero."
  (not (is-null-maneuver? maneuver)))

(defun are-maneuvers-equal? (m1 m2)
  "Check if two maneuvers are identical."
  (equal m1 m2))

(defun naive-maneuver (maneuver)
  "A 'naive' version of the maneuver, not including any conditions, and
with no repetition."
  (my-make-maneuver (seq-map 'naive-move
                             (my-maneuver-moves maneuver))
                    :repeating? nil))

(defun are-maneuvers-equivalent? (m1 m2)
  "Check if two maneuvers are equal, disregarding any conditions."
  (are-maneuvers-equal? (naive-maneuver m1)
                        (naive-maneuver m2)))

(defun my-make-strategy (&rest maneuvers)
  "Construct a strategy from the given maneuvers."
  maneuvers)

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

(defun my-symex-repl ()
  "Go to REPL."
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-repl))
        ((member major-mode elisp-modes)
         (my-lisp-repl))
        ((equal major-mode 'scheme-mode)
         (geiser-mode-switch-to-repl))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun lispy-comment-line-p ()
  "Checks if we're currently at the start of a comment line."
  (and (lispy-bolp)
       (looking-at-p ";")))

(defun lispy-empty-list-p ()
  "Checks if we're looking at an empty list."
  (save-excursion
    (and (lispy-left-p)
         (progn (forward-char 2) ;; need to go forward by 2 for some reason
                (lispy-right-p)))))

(defun my-symex-index ()
  "Get relative (from start of containing symex) index of current symex."
  (interactive)
  (save-excursion
    (my-select-nearest-symex)
    (let ((original-location (point)))
      (let ((current-location (my-goto-first-symex))
            (result 0))
        (while (< current-location original-location)
          (my-forward-symex)
          (setq current-location (point))
          (setq result (+ 1 result)))
        result))))

(defun my-refocus-on-symex (&optional smooth-scroll)
  "Move screen to put symex in convenient part of the view."
  (interactive)
  ;; Note: window-text-height is not robust to zooming
  (let* ((window-focus-line-number (/ (window-text-height)
                                       3))
         (current-line-number (line-number-at-pos))
         (top-line-number (save-excursion (evil-window-top)
                                          (line-number-at-pos)))
         (window-current-line-number (- current-line-number
                                        top-line-number))
         (window-scroll-delta (- window-current-line-number
                                 window-focus-line-number))
         (window-upper-view-bound (/ (window-text-height)
                                     9))
         (window-lower-view-bound (* (window-text-height)
                                     (/ 4.0 6))))
    (unless (and (> window-current-line-number
                    window-upper-view-bound)
                 (< window-current-line-number
                    window-lower-view-bound))
      (if smooth-scroll
          (dotimes (i (/ (abs window-scroll-delta)
                         3))
            (condition-case nil
                (evil-scroll-line-down (if (> window-scroll-delta 0)
                                           3
                                         -3))
              (error nil))
            (sit-for 0.0001))
        (recenter window-focus-line-number)))))

(defun my--forward-one-symex ()
  "Forward one symex"
  (let ((original-location (point))
        (result 0))
    (if (thing-at-point 'sexp)
        (condition-case nil
            (progn (forward-sexp 2)
                   (setq result 2))
          (error (condition-case nil
                     (progn (forward-sexp 1)
                            (setq result 1))
                   (error (setq result 0)))))
      (condition-case nil
          (progn (forward-sexp 1)
                 (setq result 1))
        (error (setq result 0))))
    (condition-case nil
        (progn (backward-sexp 1)
               (setq result (- result 1)))
      (error nil))
    (let ((current-location (point)))
      (when (= original-location current-location)
        ;; happens at end of buffer
        (setq result 0)))
    (my-refocus-on-symex)
    result))

(defun my-forward-symex (&optional count)
  "Forward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--forward-one-symex)))
        (setq result (+ res result))))
    (my-make-move result 0)))

(defun my-backward-symex (&optional count)
  "Backward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (condition-case nil
        (progn (backward-sexp count)
               (setq result (- 0 count)))
      (error nil))
    (my-refocus-on-symex)
    (my-make-move result 0)))

(defun my--enter-one-symex ()
  "Enter one lower symex level."
  (let ((result 1))
    (cond ((lispy-comment-line-p)
           (lispy-flow 1))
          ((and (lispy-left-p)
                (not (lispy-empty-list-p)))
           (forward-char))
          (t (setq result 0)))
    result))

(defun my-enter-symex (&optional count)
  "Enter lower symex level."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--enter-one-symex)))
        (setq result (+ res result))))
    (my-make-move 0 result)))

(defun my-exit-symex (&optional count)
  "Exit to higher symex level"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (condition-case nil
        (progn (paredit-backward-up count)
               (setq result count)) ;; note: paredit moves as much as possible on failure, so this may be inaccurate
      (error nil))
    (my-make-move 0 (- 0 result))))

(defun execute-tree-move (move)
  "Execute the specified MOVE at the current point location in the tree.

The move is only executed if PRE-CONDITION holds, and is reversed if
POST-CONDITION does not hold after the provisional execution of the move."
  (let ((original-location (point))
        (move-x (my-move-x move))
        (move-y (my-move-y move))
        (pre-condition (or (my-move-pre-condition move)
                           (lambda () t)))
        (post-condition (or (my-move-post-condition move)
                            (lambda () t))))
    (if (not (funcall pre-condition))
        move-zero
      (cond ((> move-x 0)
             (setq move-magnitude move-x)
             (setq move-function #'my-forward-symex))
            ((< move-x 0)
             (setq move-magnitude (abs move-x))
             (setq move-function #'my-backward-symex))
            ((> move-y 0)
             (setq move-magnitude move-y)
             (setq move-function #'my-enter-symex))
            ((< move-y 0)
             (setq move-magnitude (abs move-y))
             (setq move-function #'my-exit-symex)))
      (let ((result (funcall move-function
                             move-magnitude)))
        (if (not (funcall post-condition))
            (progn (goto-char original-location)
                   move-zero)
          result)))))

(defun my-execute-maneuver (maneuver)
  "Attempt to execute a given MANEUVER. If the entire sequence of moves
is not possible from the current location, then do nothing."
  (let ((executed-moves '()))
    (catch 'done
      (iter-do (move (my-maneuver-begin maneuver))
        (let ((executed-move (execute-tree-move move)))
          (push executed-move executed-moves)
          (unless (are-moves-equivalent? executed-move move)
            (throw 'done t)))))
    (my-make-maneuver executed-moves)))

(defun my--greedy-execute-maneuver (maneuvers)
  "Given an ordered list of maneuvers, attempt each one in turn
until one succeeds."
  (let ((executed-maneuver
         (catch 'done
           (dolist (maneuver maneuvers)
             (when (maneuver-exists? (my-execute-maneuver maneuver))
               (throw 'done maneuver)))
           maneuver-zero)))
    executed-maneuver))

(defun my--execute-maneuver-with-detour (maneuver detour)
  "Execute the maneuver, trying the indicated detour as needed.

Continues trying until the detour fails."
  (let ((detour-attempt (my-execute-maneuver detour)))
    (if (maneuver-exists? detour-attempt)
      (let ((attempt (my-execute-maneuver maneuver)))
        (if (maneuver-exists? attempt)
            t
          (my--execute-maneuver-with-detour maneuver detour)))
      nil)))

(defun my-execute-strategy (strategy)
  "Execute the provided maneuver, taking detours until successful.

This operation terminates either when the maneuver succeeds, or
when the detour fails."
  (let* ((original-location (point))
         (maneuver (car strategy))
         (detour (cadr strategy)))
    (let ((result (my--execute-maneuver-with-detour maneuver detour)))
      (unless result
        (goto-char original-location))
      result)))

(defun my-select-nearest-symex ()
  "Select symex nearest to point"
  (interactive)
  (cond ((save-excursion (forward-char) (lispy-right-p))
         (forward-char)
         (lispy-different))
        ((looking-at-p "[[:space:]]")
         (re-search-forward "[^[:space:]\n]")
         (backward-char))
        ((thing-at-point 'sexp)
         (beginning-of-thing 'sexp))
        (t (let ((previous-position (point)))
             (my-forward-symex)
             (setq current-position (point))
             (when (= current-position previous-position)
               (my-backward-symex)))))
  (my-refocus-on-symex)
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
  (my-refocus-on-symex)
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
  (my-refocus-on-symex)
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
  (my-refocus-on-symex)
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
        (progn (my-forward-symex)
               (setq current-position (point))
               (unless (= previous-position current-position)
                 (my--goto-innermost)))
      (progn (my-forward-symex)
             (setq current-position (point))
             (my--goto-innermost))))
  (point))

(defun my-goto-innermost-symex ()
  "Select innermost symex."
  (interactive)
  (my--goto-innermost)
  (my-refocus-on-symex)
  (point))

(defvar preorder-in (my-make-maneuver (list move-go-in)))
(defvar preorder-forward (my-make-maneuver (list move-go-forward)))
(defvar preorder-explore (list preorder-in preorder-forward))
(defvar detour-exit-until-root (my-make-maneuver (list move-go-out-avoid-root)))
(defvar detour-exit-until-end-of-buffer (my-make-maneuver (list move-go-out-avoid-eob)))

;; TODO: is there a way to "monadically" build the tree data structure
;; (or ideally, do an arbitrary structural computation) as part of this traversal?
;; key is, it has to be inferrable from inputs and outputs alone, i.e. specifically
;; from the result of invocation of e.g. forward-symex
(defun my-traverse-symex-forward (&optional flow)
  "Traverse symex as a tree, using pre-order traversal.

If FLOW is true, continue from one tree to another. Otherwise, stop at end of
current rooted tree."
  (interactive)
  (let ((detour (if flow
                    detour-exit-until-end-of-buffer
                  detour-exit-until-root)))
    (let ((maneuver (my--greedy-execute-maneuver preorder-explore)))
      (if (maneuver-exists? maneuver)
          t
        (my-execute-strategy (my-make-strategy preorder-forward
                                               detour))))))

(defun my--preorder-traverse-backward ()
  "Lowlevel pre-order traversal operation."
  (let ((previous-location (point))
        (current-location (my-backward-symex)))
    (if (= current-location previous-location)
        (my-exit-symex)
      current-location)))

(defun my-preorder-traverse-symex-backward ()
  "Traverse symex as a tree, using pre-order traversal."
  (interactive)
  (let ((original-location (point)))
    (my--preorder-traverse-backward)
    (let ((previous-location (point))
          (current-location (point)))
      (when (= current-location original-location)
        (catch 'done
          (while t
            (setq current-location (my-exit-symex))
            (unless (= current-location previous-location)
              (setq previous-location current-location)
              (my-backward-symex)
              (setq current-location (my-goto-innermost-symex))
              (when (not (= current-location previous-location))
                (throw 'done t)))))))))

(defmacro if-stuck (do-what operation &rest body)
  `(let ((orig-pt (point)))
     ,operation
     (if (= orig-pt (point))
         ,do-what
       ,@body)))

(defun point-at-root-symex? ()
  "Check if point is at a root symex."
  (interactive)
  (save-excursion
    (if-stuck t
              (my-exit-symex)
              nil)))

(defun point-at-first-symex? ()
  "Check if point is at the first symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (my-backward-symex)
              nil)))

(defun point-at-last-symex? ()
  "Check if point is at the last symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (my-forward-symex)
              nil)))

(defun point-at-final-symex? ()
  "Check if point is at the last symex in the buffer."
  (interactive)
  (save-excursion
    (if-stuck (progn (if-stuck t
                               (my-exit-symex)
                               nil))
              (my-forward-symex)
              nil)))

(defun my-switch-branch-backward ()
  "Switch branch backward"
  (interactive)
  (let ((symex-index (my-symex-index))
        (closest-index -1)
        (best-branch-position (point)))
    (defun switch-backward ()
      (if (point-at-root-symex?)
          (goto-char best-branch-position)
        (my-exit-symex)
        (if-stuck (switch-backward)
                  (my-backward-symex)
                  (if-stuck (switch-backward)
                            (my-enter-symex)
                            (my-forward-symex symex-index)
                            (let ((current-index (my-symex-index)))
                              (when (and (< current-index
                                            symex-index)
                                         (> current-index
                                            closest-index))
                                (setq closest-index current-index)
                                (setq best-branch-position (point)))))))))
    (switch-backward)))

(defun my-switch-branch-forward ()
  "Switch branch forward"
  (interactive)
  (let ((symex-index (my-symex-index)))
    (my-exit-symex)
    (my-forward-symex)
    (my-enter-symex)
    (my-forward-symex symex-index)))

(defun my-delete-symex ()
  "Delete symex"
  (interactive)
  (sp-kill-sexp nil)
  (if (or (current-line-empty-p)
          (save-excursion (back-to-indentation)
                          (forward-char)
                          (lispy-right-p)))
      (progn (evil-previous-line)
             (my-symex-join-lines))
    (fixup-whitespace))
  (my-select-nearest-symex))

(defun my-change-symex ()
  "Change symex"
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun my-replace-symex ()
  "Replace contents of symex"
  (interactive)
  (let ((move (my-enter-symex)))
    (if (move-exists? move)
        (apply 'evil-change (evil-inner-paren))  ;; TODO: dispatch on paren type
      (sp-kill-sexp nil)
      (evil-insert-state))))

(defun my-spit-backward ()
  "Spit backward"
  (interactive)
  (when (and (lispy-left-p)
             (not (lispy-empty-list-p)))
    (save-excursion
      (my-enter-symex) ;; need to be inside the symex to spit and slurp
      (lispy-backward-barf-sexp 1))
    (my-forward-symex)
    (when (lispy-empty-list-p)
      (fixup-whitespace)
      (re-search-forward lispy-left)
      (my-exit-symex))))

(defun my-spit-forward ()
  "Spit forward"
  (interactive)
  (when (and (lispy-left-p)
             (not (lispy-empty-list-p)))
    (save-excursion
      (my-enter-symex) ;; need to be inside the symex to spit and slurp
      (paredit-forward-barf-sexp 1))
    (when (lispy-empty-list-p)
      (my-forward-symex)
      (fixup-whitespace)
      (re-search-backward lispy-left))))

(defun my-slurp-backward ()
  "Slurp from behind"
  (interactive)
  (when (lispy-left-p)
    (if (lispy-empty-list-p)
        (forward-char)
      (my-enter-symex)) ;; need to be inside the symex to spit and slurp
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (my-exit-symex)))

(defun my-slurp-forward ()
  "Slurp from the front"
  (interactive)
  (when (lispy-left-p)
    (save-excursion
      (if (lispy-empty-list-p)
          (forward-char)
        (my-enter-symex))  ;; need to be inside the symex to spit and slurp
      (lispy-forward-slurp-sexp 1))))

(defun my-tidy-symex ()
  "Auto-indent symex and fix any whitespace."
  (interactive)
  (fixup-whitespace)
  (when (save-excursion (looking-at-p "[[:space:]]"))
      (forward-char))
  (save-excursion
    (forward-sexp)
    (fixup-whitespace))
  (save-excursion
    (apply 'evil-indent
           (seq-take (evil-cp-a-form 1)
                     2)))
  (my-select-nearest-symex))

(defun my-join-symexes ()
  "Merge symexes at the same level."
  (interactive)
  (save-excursion
    (my-forward-symex)
    (paredit-join-sexps)))

(defun my-symex-join-lines (&optional backwards)
  "Join lines inside symex."
  (interactive)
  (save-excursion
    (if backwards
        (evil-previous-line)
      (forward-sexp))
    (evil-join (line-beginning-position)
               (line-end-position)))
  (when backwards
    (forward-char)))

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
      (my-tidy-symex))))

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
      (my-tidy-symex))
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
  (newline-and-indent)
  (my-tidy-symex))

(defun my-swallow-symex ()
  "Swallow symex, putting its contents in the parent symex."
  (interactive)
  (my-enter-symex)
  (my-forward-symex)
  (paredit-splice-sexp-killing-backward))

(defun my-symex-wrap-round ()
  "Wrap with ()"
  (interactive)
  (paredit-wrap-round)
  (my-exit-symex))

(defun my-symex-wrap-square ()
  "Wrap with []"
  (interactive)
  (paredit-wrap-square)
  (my-exit-symex))

(defun my-symex-wrap-curly ()
  "Wrap with {}"
  (interactive)
  (paredit-wrap-curly)
  (evil-find-char-backward nil 123))

(defun my-symex-wrap-angled ()
  "Wrap with <>"
  (interactive)
  (paredit-wrap-angled)
  (evil-find-char-backward nil 60))

(defun my-wrap-symex ()
  "Wrap with containing symex."
  (interactive)
  (my-symex-wrap-round)
  (my-insert-at-beginning-of-symex))

(defun my-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))  ;; TODO: create in lisp interaction mode if missing)


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
  ("j" my-enter-symex "enter")
  ("k" my-exit-symex "exit")
  ("l" my-forward-symex "next")
  ("f" (lambda ()
         (interactive)
         (my-traverse-symex-forward t)) "flow forward")
  ("b" my-preorder-traverse-symex-backward "flow backward")
  ("C-k" my-switch-branch-backward "switch branch backward")
  ("C-j" my-switch-branch-forward "switch branch forward")
  ("y" my-yank-symex "yank (copy)")
  ("p" my-paste-after-symex "paste after")
  ("P" my-paste-before-symex "paste before")
  ("x" my-delete-symex "delete")
  ("c" my-change-symex "change" :exit t)
  ("s" my-replace-symex "replace" :exit t)
  ("H" lispy-move-up "move backward")
  ("L" lispy-move-down "move forward")
  ("K" paredit-raise-sexp "raise")
  ("s-J" my-slurp-backward "slurp backward")
  ("s-H" my-spit-backward "spit backward")
  ("s-L" my-spit-forward "spit forward")
  ("s-K" my-slurp-forward "slurp forward")
  ("z" my-swallow-symex "swallow")
  ("e" my-evaluate-symex "evaluate")
  ("d" my-evaluate-definition "evaluate definition")
  ("E" eval-expression "eval expression")
  ("t" my-switch-to-scratch-buffer "scratch buffer" :exit t)
  ("r" my-symex-repl "go to REPL" :exit t)
  ("|" lispy-split "split")
  ("m" my-join-symexes "merge (join)")
  ("\\" lispy-splice "splice (join to higher level)")
  (")" my-symex-wrap-round "wrap with ()")
  ("]" my-symex-wrap-square "wrap with []")
  ("}" my-symex-wrap-curly "wrap with {}")
  (">" my-symex-wrap-angled "wrap with <>")
  ("o" my-open-line-after-symex "open line after" :exit t)
  ("O" my-open-line-before-symex "open line before" :exit t)
  ("n" my-insert-symex-newline "newline")
  ("J" my-symex-join-lines "join lines")
  ("N" (lambda ()
         (interactive)
         (my-symex-join-lines t)) "join lines backwards")
  ("0" my-goto-first-symex "go to first")
  ("M-h" my-goto-first-symex "go to first")
  ("$" my-goto-last-symex "go to last")
  ("M-l" my-goto-last-symex "go to last")
  ("M-k" my-goto-outermost-symex "go to outermost")
  ("M-j" my-goto-innermost-symex "go to innermost")
  ("=" my-tidy-symex "tidy/indent")
  ("A" my-append-after-symex "append after symex" :exit t)
  ("a" my-insert-at-end-of-symex "append inside symex" :exit t)
  ("i" my-insert-at-beginning-of-symex "insert inside symex" :exit t)
  ("I" my-insert-before-symex "insert before symex" :exit t)
  ("w" my-wrap-symex "wrap with symex" :exit t)
  ;; canonical action
  ("s-;" my-evaluate-symex "evaluate")
  ;; escape hatches
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
