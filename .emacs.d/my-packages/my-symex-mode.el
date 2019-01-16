;;; -*- lexical-binding: t -*-
;;; TODO: ideally, would be good to have a simple POC of the AST
;;; to operate on, via semantic?
;;; TODO: consider using S for dragging and C for movement (and then across all modes)
;;; TODO: move back/forward through tree "at same level" without going up or down (i.e. switch branches, ideally preserving position index within branch)
;;; TODO: traverse tree with side effect (traversal-method, side-effect-fn), to use for "indent forward" on paste
;;; TODO: incorporate more clear tree-related terminology
;;; TODO: C-j to move in greedily, going forward
;;; TODO: handle "contracts" of each abstraction level, and where conditions should go, rename functions for clarity. legitimate detours vs conditional itineraries, vs conditional motions
;;; TODO: detours should be maneuvers. define a strategy as a higher-level sequence of maneuvers, where each is tried in sequence until all fail, beginning again from the first on success
;;; TODO: take a symex and bring it out and before/after as a peer of the parent
;;; TODO: my-tidy-symex has edge cases in indenting from evil-cp-a-form, where symex begins with : (keyword arg) or #'
(use-package lispy)
(use-package paredit)
(use-package evil-cleverparens)  ;; really only need cp-textobjects here
(require 'cl-lib)


(defun my-make-move (x y)
  (list x y))

(defun my-move-x (move)
  "X (horizontal) component of move."
  (nth 0 move))

(defun my-move-y (move)
  "Y (vertical) component of move."
  (nth 1 move))

(defvar move-zero (my-make-move 0 0))
(defvar move-go-forward (my-make-move 1 0))
(defvar move-go-backward (my-make-move -1 0))
(defvar move-go-in (my-make-move 0 1))
(defvar move-go-out (my-make-move 0 -1))

(defun is-null-move? (move)
  "Checks if the move specifies no movement."
  (are-moves-equal? move move-zero))

(defun move-exists? (move)
  "Checks if the move specifies tangible movement."
  (not (is-null-move? move)))

(defun are-moves-equal? (m1 m2)
  "Check if two moves are identical, including any conditions."
  (equal m1 m2))

(cl-defun my-make-precaution (traversal &key pre-condition post-condition)
  "A specification to check conditions before and/or after execution
of a traversal."
  (let ((pre-condition (or pre-condition (lambda () t)))
        (post-condition (or post-condition (lambda () t))))
    (list 'precaution
          traversal
          pre-condition
          post-condition)))

(defun my-precaution-traversal (precaution)
  "The traversal component of the precaution, i.e. the traversal to be
executed with precautions."
  (nth 1 precaution))

(defun my-precaution-pre-condition (precaution)
  "Pre-condition of precaution"
  (nth 2 precaution))

(defun my-precaution-post-condition (precaution)
  "Post-condition of precaution"
  (nth 3 precaution))

(defun is-precaution? (obj)
  "Checks if the data specifies a precaution."
  (condition-case nil
      (equal 'precaution
             (nth 0 obj))
    (error nil)))

(defun my-make-circuit (traversal &optional times)
  "A specification to repeat a TRAVERSAL TIMES times.

If TIMES is nil, repeat indefinitely until the traversal fails."
  (list 'circuit
        traversal
        times))

(defun my-circuit-traversal (circuit)
  "Get the traversal component of the circuit, i.e. the traversal
to be looped."
  (nth 1 circuit))

(defun my-circuit-times (circuit)
  "Get the times component of the circuit, i.e. the number of times
the traversal should be repeated."
  (nth 2 circuit))

(defun is-circuit? (obj)
  "Checks if the data specifies a circuit."
  (condition-case nil
      (equal 'circuit
             (nth 0 obj))
    (error nil)))

(defun symex--execute-circuit (traversal times)
  "Execute TRAVERSAL TIMES times."
  (when (or (not times)  ; loop indefinitely
            (> times 0))
    (let ((result (symex-execute-traversal traversal)))
      (when result
        (let ((times (if times
                         (- times 1)
                       times)))
          (append result
                  (symex--execute-circuit traversal
                                          times)))))))

(defun my-execute-circuit (circuit)
  "Execute a circuit.

This repeats some traversal as specified."
  (let ((traversal (my-circuit-traversal circuit))
        (times (my-circuit-times circuit)))
    (symex--execute-circuit traversal times)))

(defun my-make-maneuver (phases)
  "Construct a maneuver from the given moves."
  (list 'maneuver
        phases))

(defun my-maneuver-phases (maneuver)
  "Get the phases of a maneuver (which are themselves maneuvers or moves)."
  (nth 1 maneuver))

(defun is-maneuver? (obj)
  "Checks if the data specifies a maneuver."
  (condition-case nil
      (equal 'maneuver
             (nth 0 obj))
    (error nil)))

(defun my-make-detour (reorientation traversal)
  "Construct a detour.

A detour consists of two components -- a TRAVERSAL that we wish to execute, and
a REORIENTATION which is a transformation we want to apply prior to attempting
the traversal. Both the reorientation as well as the traversal could be any
type of traversal, for instance a detour or a maneuver.

The reorientation is applied repeatedly and the traversal is re-attempted each
time, until it succeeds. If the reorientation itself fails, then the detour fails
as well."
  (list 'detour
        reorientation
        traversal))

(defun symex--detour-reorientation (detour)
  "Get the reorientation component of the DETOUR."
  (nth 1 detour))

(defun symex--detour-traversal (detour)
  "Get the traversal component of the DETOUR."
  (nth 2 detour))

(defun is-detour? (obj)
  "Checks if the data specifies a detour."
  (condition-case nil
      (equal 'detour
             (nth 0 obj))
    (error nil)))

(defun symex--execute-traversal-with-reorientation (reorientation traversal)
  "Apply a reorientation and then attempt the maneuver.

If the maneuver fails, then the reorientation is attempted as many times as
necessary until either it succeeds, or the reorientation fails.

Evaluates to a list of maneuvers executed, if any, which could be treated
as phases of a higher-level maneuver by the caller."
  (let ((executed-reorientation (symex-execute-traversal reorientation)))
    (when executed-reorientation
      (let ((executed-traversal (symex-execute-traversal traversal)))
        (if executed-traversal
            (append (list executed-reorientation)
                    executed-traversal)
          (let ((attempt (symex--execute-traversal-with-reorientation reorientation
                                                                      traversal)))
            (when attempt
              (append (list executed-reorientation)
                      attempt))))))))

(defun my-execute-detour (detour)
  "Execute the DETOUR."
  (let ((original-location (point))
        (reorientation (symex--detour-reorientation detour))
        (traversal (symex--detour-traversal detour)))
    (let ((result (symex--execute-traversal-with-reorientation reorientation
                                                               traversal)))
      (unless result
        (goto-char original-location))
      (my-make-maneuver result))))

(defun my-make-protocol (&rest options)
  "Construct a protocol abstraction for the given OPTIONS.

An option could be either a maneuver, or a protocol itself."
  (list 'protocol
        options))

(defun symex--protocol-options (protocol)
  "Get the set of options that are part of the PROTOCOL."
  (nth 1 protocol))

(defun is-protocol? (obj)
  "Checks if the data specifies a protocol."
  (condition-case nil
      (equal 'protocol
             (nth 0 obj))
    (error nil)))

(defun symex--try-options-in-sequence (options)
  "Try options one at a time until one succeeds."
  (let ((option (car options))
        (remaining-options (cdr options)))
    (let ((executed-option (symex-execute-traversal option)))
      (if executed-option
          executed-option
        (when remaining-options
          (symex--try-options-in-sequence remaining-options))))))

(defun symex-execute-protocol (protocol)
  "Given a protocol including a set of options, attempt to execute them
in order until one succeeds.

Evaluates to the maneuver actually executed."
  (let ((options (symex--protocol-options protocol)))
    (symex--try-options-in-sequence options)))

(defun symex-execute-traversal (traversal)
  "Execute a tree traversal."
  (cond ((is-maneuver? traversal)
         (my-execute-maneuver traversal))
        ((is-circuit? traversal)
         (my-execute-circuit traversal))
        ((is-protocol? traversal)
         (symex-execute-protocol traversal))
        ((is-precaution? traversal)
         (my-execute-precaution traversal))
        ((is-detour? traversal)
         (my-execute-detour traversal))
        (t (execute-tree-move traversal))))
        ;;(t (error "Syntax error: unrecognized traversal type!"))))

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
              (symex-go-out)
              nil)))

(defun point-at-first-symex? ()
  "Check if point is at the first symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-backward)
              nil)))

(defun point-at-last-symex? ()
  "Check if point is at the last symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-forward)
              nil)))

(defun point-at-final-symex? ()
  "Check if point is at the last symex in the buffer."
  (interactive)
  (save-excursion
    (if-stuck (progn (if-stuck t
                               (symex-go-out)
                               nil))
              (symex-go-forward)
              nil)))

(defun point-at-initial-symex? ()
  "Check if point is at the first symex in the buffer."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn (backward-sexp 1)
               (not (thing-at-point 'sexp)))
      (error nil))))

(defun symex-comment-line-p ()
  "Checks if we're currently at the start of a comment line."
  (and (lispy-bolp)
       (looking-at-p ";")))

(defun symex-empty-list-p ()
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
          (symex-go-forward)
          (setq current-location (point))
          (setq result (+ 1 result)))
        result))))

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
    (when (> result 0)
      (my-make-move result 0))))

(defun my--backward-one-symex ()
  "Backward one symex."
  (let ((result 0))
    (when (not (point-at-initial-symex?))
      (condition-case nil
          (progn (backward-sexp 1)
                 (setq result (+ 1 result)))
        (error nil)))
    result))

(defun my-backward-symex (&optional count)
  "Backward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--backward-one-symex)))
        (setq result (+ res result))))
    (my-refocus-on-symex)
    (when (> result 0)
      (my-make-move (- 0 result) 0))))

(defun my--enter-one-symex ()
  "Enter one lower symex level."
  (let ((result 1))
    (cond ((symex-comment-line-p)
           (lispy-flow 1))
          ((and (lispy-left-p)
                (not (symex-empty-list-p)))
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
    (when (> result 0)
      (my-make-move 0 result))))

(defun my--exit-one-symex ()
  "Exit one level."
  (condition-case nil
      (progn (paredit-backward-up 1)
             1)
    (error 0)))

(defun my-exit-symex (&optional count)
  "Exit to higher symex level"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--exit-one-symex)))
        (setq result (+ res result))))
    (when (> result 0)
      (my-make-move 0 (- 0 result)))))

(defun execute-tree-move (move)
  "Execute the specified MOVE at the current point location in the tree.

Evaluates to the actual move executed or nil if no move was executed."
  (let ((move-x (my-move-x move))
        (move-y (my-move-y move)))
    (cond ((> move-x 0)
           (my-forward-symex move-x))
          ((< move-x 0)
           (my-backward-symex (abs move-x)))
          ((> move-y 0)
           (my-enter-symex move-y))
          ((< move-y 0)
           (my-exit-symex (abs move-y))))))

(cl-defun symex-go-forward (&optional (count 1))
  "Move forward COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move count 0)))

(cl-defun symex-go-backward (&optional (count 1))
  "Move backward COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move (- 0 count) 0)))

(cl-defun symex-go-in (&optional (count 1))
  "Move in COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move 0 count)))

(cl-defun symex-go-out (&optional (count 1))
  "Move out COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move 0 (- 0 count))))

(defun my--execute-maneuver-phases (phases)
  "Execute the phases of a maneuver, stopping if a phase fails.

Evalutes to a list of phases actually executed."
  (let ((current-phase (car phases))
        (remaining-phases (cdr phases)))
    (let ((executed-phase (symex-execute-traversal current-phase)))
      (when executed-phase
        (let ((result (list executed-phase)))
          (if remaining-phases
              (append result
                      (my--execute-maneuver-phases remaining-phases))
            result))))))

(defun my-execute-maneuver (maneuver)
  "Attempt to execute a given MANEUVER.

Attempts the maneuver in the order of its phases, accepting partial completion
of phases. If any phase fails entirely, then the maneuver it is part of is
terminated at that step.

The maneuver is only executed if PRE-CONDITION holds, and is reversed if
POST-CONDITION does not hold after the provisional execution of the maneuver.

Evaluates to the maneuver actually executed."
  (let ((phases (my-maneuver-phases maneuver)))
    (let ((executed-phases (my--execute-maneuver-phases phases)))
      (when executed-phases
        (my-make-maneuver executed-phases)))))

(defun my-execute-precaution (precaution)
  "Attempt to execute a given PRECAUTION.

The traversal is only executed if PRE-CONDITION holds, and is reversed if
POST-CONDITION does not hold after the provisional execution of the traversal.

Evaluates to the maneuver actually executed."
  (let ((original-location (point))
        (traversal (my-precaution-traversal precaution))
        (pre-condition (my-precaution-pre-condition precaution))
        (post-condition (my-precaution-post-condition precaution)))
    (when (funcall pre-condition)
      (let ((executed-traversal (symex-execute-traversal traversal)))
        (if (funcall post-condition)
            executed-traversal
          (goto-char original-location))))))

(defun my-goto-first-symex ()
  "Select first symex at present level"
  (interactive)
  (let ((traversal
         (my-make-circuit
          move-go-backward)))
    (symex-execute-traversal traversal))
  (my-refocus-on-symex)
  (point))

(defun my-goto-last-symex ()
  "Select last symex at present level"
  (interactive)
  (let ((traversal
         (my-make-circuit
          move-go-forward)))
    (symex-execute-traversal traversal))
  (my-refocus-on-symex)
  (point))

(defun my-goto-outermost-symex ()
  "Select outermost symex."
  (interactive)
  (let ((traversal
         (my-make-circuit
          move-go-out)))
    (symex-execute-traversal traversal))
  (my-refocus-on-symex)
  (point))

(defun my-goto-innermost-symex ()
  "Select innermost symex."
  (interactive)
  (let ((traversal
         (my-make-circuit
          (my-make-protocol
           (my-make-circuit
            move-go-in)
           move-go-forward))))
    (symex-execute-traversal traversal))
  (my-refocus-on-symex)
  (point))

;; TODO: is there a way to "monadically" build the tree data structure
;; (or ideally, do an arbitrary structural computation) as part of this traversal?
;; key is, it has to be inferrable from inputs and outputs alone, i.e. specifically
;; from the result of invocation of e.g. forward-symex
(defun my-traverse-symex-forward (&optional flow)
  "Traverse symex as a tree, using pre-order traversal.

If FLOW is true, continue from one tree to another. Otherwise, stop at end of
current rooted tree."
  (interactive)
  (let ((exit-until-root
         (my-make-precaution
          move-go-out
          :post-condition #'(lambda ()
                              (not (point-at-root-symex?)))))
        (exit-until-end-of-buffer
         (my-make-precaution
          move-go-out
          :post-condition #'(lambda ()
                              (not (point-at-final-symex?))))))
    (let ((traversal
           (my-make-protocol
            (my-make-protocol
             move-go-in
             move-go-forward)
            (my-make-detour
             (if flow
                 exit-until-end-of-buffer
               exit-until-root)
             move-go-forward))))
      (symex-execute-traversal traversal))))

(defun my-traverse-symex-backward (&optional flow)
  "Traverse symex as a tree, using converse post-order traversal.

If FLOW is true, continue from one tree to another. Otherwise, stop at root of
current tree."
  (interactive)
  (let* ((postorder-in
          (my-make-circuit
           (my-make-maneuver
            (list move-go-in
                  (my-make-circuit
                   move-go-forward)))))
         (postorder-backwards-in
          (my-make-maneuver (list move-go-backward
                                  postorder-in)))
         (postorder-backwards-in-tree
          (my-make-precaution
           (my-make-maneuver
            (list move-go-backward
                  postorder-in))
           :pre-condition #'(lambda ()
                              (not (point-at-root-symex?))))))
    (let* ((traversal
            (my-make-protocol
             (if flow
                 postorder-backwards-in
               postorder-backwards-in-tree)
             move-go-out)))
      (symex-execute-traversal traversal))))

(defun my-switch-branch-backward ()
  "Switch branch backward"
  (interactive)
  (let ((symex-index (my-symex-index))
        (closest-index -1)
        (best-branch-position (point)))
    (defun switch-backward ()
      (if (point-at-root-symex?)
          (goto-char best-branch-position)
        (symex-go-out)
        (if-stuck (switch-backward)
                  (symex-go-backward)
                  (if-stuck (switch-backward)
                            (symex-go-in)
                            (symex-go-forward symex-index)
                            (let ((current-index (my-symex-index)))
                              (when (and (< current-index
                                            symex-index)
                                         (> current-index
                                            closest-index))
                                (setq closest-index current-index)
                                (setq best-branch-position (point)))))))))
  (switch-backward))

(defun my-switch-branch-forward ()
  "Switch branch forward"
  (interactive)
  (let ((symex-index (my-symex-index)))
    (symex-go-out)
    (symex-go-forward)
    (symex-go-in)
    (symex-go-forward symex-index)))

(defun my-delete-symex ()
  "Delete symex"
  (interactive)
  (sp-kill-sexp nil)
  (cond ((or (current-line-empty-p)  ;; ^<>$
             (save-excursion (back-to-indentation)  ;; ^(<>
                             (forward-char)
                             (lispy-right-p)))
         (progn (evil-previous-line)
                (my-symex-join-lines)))
        ((save-excursion (evil-last-non-blank)  ;; (<>$
                         (lispy-left-p))
         (sp-next-sexp)
         (save-excursion
           (my-symex-join-lines t)))
        ((looking-at-p "\n")  ;; (abc <>
         (evil-join (line-beginning-position)
                    (line-end-position)))
        (t (fixup-whitespace)))
  (my-select-nearest-symex)
  (my-tidy-symex))

(defun my-change-symex ()
  "Change symex"
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun my-replace-symex ()
  "Replace contents of symex"
  (interactive)
  (let ((move (symex-go-in)))
    (if (move-exists? move)
        (apply 'evil-change (evil-inner-paren))  ;; TODO: dispatch on paren type
      (sp-kill-sexp nil)
      (evil-insert-state))))

(defun my-spit-backward ()
  "Spit backward"
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in) ;; need to be inside the symex to spit and slurp
      (paredit-backward-barf-sexp 1))
    (symex-go-forward)
    (when (symex-empty-list-p)
      (fixup-whitespace)
      (re-search-forward lispy-left)
      (symex-go-out))))

(defun my-spit-forward ()
  "Spit forward"
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in) ;; need to be inside the symex to spit and slurp
      (paredit-forward-barf-sexp 1))
    (when (symex-empty-list-p)
      (symex-go-forward)
      (fixup-whitespace)
      (re-search-backward lispy-left))))

(defun my-slurp-backward ()
  "Slurp from behind"
  (interactive)
  (when (lispy-left-p)
    (if (symex-empty-list-p)
        (forward-char)
      (symex-go-in)) ;; need to be inside the symex to spit and slurp
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (symex-go-out)))

(defun my-slurp-forward ()
  "Slurp from the front"
  (interactive)
  (when (lispy-left-p)
    (save-excursion
      (if (symex-empty-list-p)
          (forward-char)
        (symex-go-in))  ;; need to be inside the symex to spit and slurp
      (lispy-forward-slurp-sexp 1))))

(defun my-join-symexes ()
  "Merge symexes at the same level."
  (interactive)
  (save-excursion
    (symex-go-forward)
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
      (symex-go-forward)
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
      (symex-go-forward)
      (my-tidy-symex))
    (symex-go-forward)))

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
  (symex-go-in)
  (symex-go-forward)
  (paredit-splice-sexp-killing-backward))

(defun my-symex-wrap-round ()
  "Wrap with ()"
  (interactive)
  (paredit-wrap-round)
  (symex-go-out))

(defun my-symex-wrap-square ()
  "Wrap with []"
  (interactive)
  (paredit-wrap-square)
  (symex-go-out))

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

(defun my-move-symex-forward ()
  "Move symex forward in current tree level."
  (interactive)
  (forward-sexp)
  (condition-case nil
      (progn (transpose-sexps 1)
             (backward-sexp))
    (error (backward-sexp))))

(defun my-move-symex-backward ()
  "Move symex backward in current tree level."
  (interactive)
  (let ((move (symex-go-backward)))
    (when (move-exists? move)
      (my-move-symex-forward)
      (symex-go-backward))))

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

(defun my-eval-print-symex ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-print-last-sexp)))

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

(defun my-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))  ;; TODO: create in lisp interaction mode if missing)

(defun my-select-nearest-symex ()
  "Select symex nearest to point"
  (interactive)
  (cond ((and (not (eobp))
              (save-excursion (forward-char) (lispy-right-p))) ;; |)
         (forward-char)
         (lispy-different))
        ((looking-at-p "[[:space:]\n]")  ;; <> |<> or <> |$
         (condition-case nil
             (progn (re-search-forward "[^[:space:]\n]")
                    (backward-char))
           (error (if-stuck (symex-go-backward)
                            (symex-go-forward)))))
        ((thing-at-point 'sexp)  ;; som|ething
         (beginning-of-thing 'sexp))
        (t (if-stuck (symex-go-backward)
                     (symex-go-forward))))
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


(defhydra hydra-symex (:idle 1.0
                       :columns 5
                       :color pink
                       :body-pre (progn (my-select-nearest-symex)
                                        (evil-symex-state)))
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
  ("h" symex-go-backward "previous")
  ("j" symex-go-in "enter")
  ("k" symex-go-out "exit")
  ("l" symex-go-forward "next")
  ("f" (lambda ()
         (interactive)
         (my-traverse-symex-forward t)) "flow forward")
  ("b" (lambda ()
         (interactive)
         (my-traverse-symex-backward t)) "flow backward")
  ("C-k" my-switch-branch-backward "switch branch backward")
  ("C-j" my-switch-branch-forward "switch branch forward")
  ("y" my-yank-symex "yank (copy)")
  ("p" my-paste-after-symex "paste after")
  ("P" my-paste-before-symex "paste before")
  ("x" my-delete-symex "delete")
  ("c" my-change-symex "change" :exit t)
  ("s" my-replace-symex "replace" :exit t)
  ("H" my-move-symex-backward "move backward")
  ("L" my-move-symex-forward "move forward")
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
  ("g" evil-jump-to-tag "Go to definition")
  (";" my-eval-print-symex "eval + print")
  ;; canonical action
  ("s-;" my-evaluate-symex "evaluate" :exit t)
  ;; escape hatches
  ("R" evil-replace-state nil :exit t)
  ("v" evil-visual-char nil :exit t)
  ("V" evil-visual-line nil :exit t)
  ("C-v" evil-visual-block nil :exit t)
  ;; standard exits
  ("?" my-describe-symex "info")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)  ;; since y looks like inverted lambda
(global-set-key (kbd "s-;") 'hydra-symex/body)  ;; since y is hard to reach

(provide 'my-symex-mode)
