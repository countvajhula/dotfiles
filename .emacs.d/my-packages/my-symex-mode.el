;;; -*- lexical-binding: t -*-
;;; TODO: consider using S for dragging and C for movement (and then across all modes)
;;; TODO: move back/forward through tree "at same level" without going up or down (i.e. switch branches, ideally preserving position index within branch)
;;; TODO: traverse tree with side effect (traversal-method, side-effect-fn), to use for "indent forward" on paste
;;; TODO: incorporate more clear tree-related terminology
;;; TODO: C-j to move in greedily, going forward
;;; TODO: handle "contracts" of each abstraction level, and where conditions should go, rename functions for clarity. legitimate detours vs conditional itineraries, vs conditional motions
;;; TODO: take a symex and bring it out and before/after as a peer of the parent
;;; TODO: my-tidy-symex has edge cases in indenting from evil-cp-a-form, where symex begins with : (keyword arg) or #'
(use-package lispy)
(use-package paredit)
(use-package evil-cleverparens)  ;; really only need cp-textobjects here
(require 'cl-lib)
(require 'dash-functional)

;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGURATION ;;;
;;;;;;;;;;;;;;;;;;;;;

;; use paredit balancing behavior in insert mode
(define-key
  evil-insert-state-map
  (kbd "\(")
  'paredit-open-round)

(define-key
  evil-insert-state-map
  (kbd "\)")
  'paredit-close-round)

(define-key
  evil-insert-state-map
  (kbd "\[")
  'paredit-open-square)

(define-key
  evil-insert-state-map
  (kbd "\]")
  'paredit-close-square)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA ABSTRACTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-make-move (x y)
  "Construct a tree 'move' representing the number of steps to be taken
along the X or forward-backward axis, and the Y or in-out axis."
  (list 'move x y))

(defun symex--move-x (move)
  "X (horizontal) component of move."
  (nth 1 move))

(defun symex--move-y (move)
  "Y (vertical) component of move."
  (nth 2 move))

(defun is-move? (obj)
  "Checks if the data specifies a move."
  (condition-case nil
      (equal 'move
             (nth 0 obj))
    (error nil)))

(defconst move-zero (symex-make-move 0 0))
(defconst move-go-forward (symex-make-move 1 0))
(defconst move-go-backward (symex-make-move -1 0))
(defconst move-go-in (symex-make-move 0 1))
(defconst move-go-out (symex-make-move 0 -1))

(defun are-moves-equal? (m1 m2)
  "Check if two moves are identical, including any conditions."
  (equal m1 m2))

(cl-defun symex-make-precaution (traversal &key pre-condition post-condition)
  "A specification to check conditions before and/or after execution
of a traversal."
  (let ((pre-condition (or pre-condition (lambda () t)))
        (post-condition (or post-condition (lambda () t))))
    (list 'precaution
          traversal
          pre-condition
          post-condition)))

(defun symex--precaution-traversal (precaution)
  "The traversal component of the precaution, i.e. the traversal to be
executed with precautions."
  (nth 1 precaution))

(defun symex--precaution-pre-condition (precaution)
  "Pre-condition of precaution"
  (nth 2 precaution))

(defun symex--precaution-post-condition (precaution)
  "Post-condition of precaution"
  (nth 3 precaution))

(defun is-precaution? (obj)
  "Checks if the data specifies a precaution."
  (condition-case nil
      (equal 'precaution
             (nth 0 obj))
    (error nil)))

(defun symex-make-circuit (traversal &optional times)
  "A specification to repeat a TRAVERSAL TIMES times.

If TIMES is nil, repeat indefinitely until the traversal fails."
  (list 'circuit
        traversal
        times))

(defun symex--circuit-traversal (circuit)
  "Get the traversal component of the circuit, i.e. the traversal
to be looped."
  (nth 1 circuit))

(defun symex--circuit-times (circuit)
  "Get the times component of the circuit, i.e. the number of times
the traversal should be repeated."
  (nth 2 circuit))

(defun is-circuit? (obj)
  "Checks if the data specifies a circuit."
  (condition-case nil
      (equal 'circuit
             (nth 0 obj))
    (error nil)))

(defun symex-make-maneuver (&rest phases)
  "Construct a maneuver from the given moves."
  (list 'maneuver
        phases))

(defun symex--maneuver-phases (maneuver)
  "Get the phases of a maneuver (which are themselves maneuvers or moves)."
  (nth 1 maneuver))

(defun is-maneuver? (obj)
  "Checks if the data specifies a maneuver."
  (condition-case nil
      (equal 'maneuver
             (nth 0 obj))
    (error nil)))

(defun symex-make-detour (reorientation traversal)
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

(defun symex-make-protocol (&rest options)
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

(cl-defun symex-make-computation (&key (map #'identity)
                                       (filter #'identity)
                                       (reduce #'identity)
                                       (f-to-aggregation #'identity)
                                       (f-from-aggregation #'identity))
  "A computation to be performed as part of a traversal.
TODO: should computations be composable?

MAP - the function to be applied to the result of each traversal step.
FILTER - the function to be applied to a result prior to aggregation.
REDUCE - a binary function to be applied in combining results to perform
the final computation.
F-TO-AGGREGATION - a 'functor' to transform data of the application type (the
type used by the application) to the computation type (the type that can be
composed and otherwise manipulated to fulfill the computational task).
F-FROM-AGGREGATION - a 'functor' to transform data of the computation type (the type
that can be composed and otherwise manipulated to fulfill the computational
task) to the application type (the type used by the application)."
  (list 'computation
        map
        filter
        reduce
        f-to-aggregation
        f-from-aggregation))

(defun symex--computation-map (computation)
  "The map component (procedure) of the computation."
  (nth 1 computation))

(defun symex--computation-filter (computation)
  "The filter component (procedure) of the computation."
  (nth 2 computation))

(defun symex--computation-reduce (computation)
  "The reduce component (procedure) of the computation."
  (nth 3 computation))

(defun symex--computation-f-to-aggregation (computation)
  "The f-to-aggregation component (procedure) of the computation."
  (nth 4 computation))

(defun symex--computation-f-from-aggregation (computation)
  "The f-from-aggregation component (procedure) of the computation."
  (nth 5 computation))

(defconst computation-default
  ;; each result is wrapped in a list
  ;; the results are concatenated using list concatenation
  (symex-make-computation :f-to-aggregation #'list
                          :reduce #'append
                          :f-from-aggregation #'car))

(defun symex--simplify-maneuver-phases (phases)
  "Helper to flatten maneuver to moves."
  (when phases
    (let ((phase (car phases))
          (remaining-phases (cdr phases)))
      (let ((moves (if (is-move? phase)
                       (list phase)
                     (let ((simplified-phase (symex--simplify-maneuver phase)))
                       (symex--maneuver-phases simplified-phase)))))
        (append moves
                (symex--simplify-maneuver-phases remaining-phases))))))

(defun symex--simplify-maneuver (maneuver)
  "Reduce a complex maneuver to a flat maneuver whose phases are moves."
  (let ((phases (symex--maneuver-phases maneuver)))
    (let* ((simplified-phases (symex--simplify-maneuver-phases phases))
           (maneuver-length (length simplified-phases)))
      (cond ((= maneuver-length 1)
             ;; just return the move
             (nth 0 simplified-phases))
            ((> maneuver-length 1)
             (apply #'symex-make-maneuver simplified-phases))))))

(defun symex--interpret-traversal (traversal)
  "Interpret a traversal as a single, flat maneuver or move."
  (cond ((is-maneuver? traversal)
         (symex--simplify-maneuver traversal))
        ((is-move? traversal)
         traversal)
        (t (error "Syntax error: unrecognized traversal type!"))))

(defconst computation-account
  ;; each result is cast as a maneuver and wrapped in a list for composition
  ;; the results are concatenated using list concatenation
  (symex-make-computation :f-to-aggregation #'list
                          :map #'symex--interpret-traversal
                          :reduce #'append
                          :f-from-aggregation #'car))

;;;;;;;;;;;;;;;;;;
;;; PRIMITIVES ;;;
;;;;;;;;;;;;;;;;;;

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

(defun symex-index ()
  "Get relative (from start of containing symex) index of current symex."
  (interactive)
  (save-excursion
    (symex-select-nearest)
    (let ((original-location (point)))
      (let ((current-location (symex-goto-first))
            (result 0))
        (while (< current-location original-location)
          (symex-go-forward)
          (setq current-location (point))
          (setq result (1+ result)))
        result))))

(defun symex--forward-one ()
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
               (setq result (1- result)))
      (error nil))
    (let ((current-location (point)))
      (when (= original-location current-location)
        ;; happens at end of buffer
        (setq result 0)))
    (symex-refocus)
    result))

(defun symex-forward (&optional count)
  "Forward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--forward-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move result 0))))

(defun symex--backward-one ()
  "Backward one symex."
  (let ((result 0))
    (when (not (point-at-initial-symex?))
      (condition-case nil
          (progn (backward-sexp 1)
                 (setq result (1+ result)))
        (error nil)))
    result))

(defun symex-backward (&optional count)
  "Backward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--backward-one)))
        (setq result (+ res result))))
    (symex-refocus)
    (when (> result 0)
      (symex-make-move (- result) 0))))

(defun symex--enter-one ()
  "Enter one lower symex level."
  (let ((result 1))
    (cond ((symex-comment-line-p)
           (lispy-flow 1))
          ((and (lispy-left-p)
                (not (symex-empty-list-p)))
           (forward-char))
          (t (setq result 0)))
    result))

(defun symex-enter (&optional count)
  "Enter lower symex level."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--enter-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 result))))

(defun symex--exit-one ()
  "Exit one level."
  (condition-case nil
      (progn (paredit-backward-up 1)
             1)
    (error 0)))

(defun symex-exit (&optional count)
  "Exit to higher symex level"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (symex--exit-one)))
        (setq result (+ res result))))
    (when (> result 0)
      (symex-make-move 0 (- result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVALUATION AND EXECUTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-tree-move (move &optional computation)
  "Execute the specified MOVE at the current point location in the tree.

Evaluates to the actual move executed or nil if no move was executed."
  (let ((move-x (symex--move-x move))
        (move-y (symex--move-y move)))
    (cond ((> move-x 0)
           (symex-forward move-x))
          ((< move-x 0)
           (symex-backward (abs move-x)))
          ((> move-y 0)
           (symex-enter move-y))
          ((< move-y 0)
           (symex-exit (abs move-y))))))

(cl-defun symex-go-forward (&optional (count 1))
  "Move forward COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move count 0)))

(cl-defun symex-go-backward (&optional (count 1))
  "Move backward COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move (- count) 0)))

(cl-defun symex-go-in (&optional (count 1))
  "Move in COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move 0 count)))

(cl-defun symex-go-out (&optional (count 1))
  "Move out COUNT symexes."
  (interactive)
  (execute-tree-move (symex-make-move 0 (- count))))

(defun symex--execute-maneuver-phases (phases computation)
  "Execute the phases of a maneuver, stopping if a phase fails.

Evalutes to a list of phases actually executed."
  (when phases
    (let ((current-phase (car phases))
          (remaining-phases (cdr phases)))
      (let ((executed-phase (symex-execute-traversal current-phase)))
        (when executed-phase
          (funcall (symex--computation-reduce computation)
                   (funcall (symex--computation-f-to-aggregation computation)
                            executed-phase)
                   (symex--execute-maneuver-phases remaining-phases
                                                   computation)))))))

(defun symex-execute-maneuver (maneuver computation)
  "Attempt to execute a given MANEUVER.

Attempts the maneuver in the order of its phases, accepting partial completion
of phases. If any phase fails entirely, then the maneuver it is part of is
terminated at that step.

Evaluates to the maneuver actually executed."
  (let ((phases (symex--maneuver-phases maneuver)))
    (let ((executed-phases (symex--execute-maneuver-phases phases
                                                           computation)))
      (when executed-phases
        (apply #'symex-make-maneuver executed-phases)))))

(defun symex-execute-precaution (precaution computation)
  "Attempt to execute a given PRECAUTION.

The traversal is only executed if PRE-CONDITION holds, and is reversed if
POST-CONDITION does not hold after the provisional execution of the traversal.

Evaluates to the maneuver actually executed."
  (let ((original-location (point))
        (traversal (symex--precaution-traversal precaution))
        (pre-condition (symex--precaution-pre-condition precaution))
        (post-condition (symex--precaution-post-condition precaution)))
    (when (funcall pre-condition)
      (let ((executed-traversal (symex-execute-traversal traversal
                                                         computation)))
        (if (funcall post-condition)
            executed-traversal
          (goto-char original-location)
          nil)))))

(defun symex--execute-circuit (traversal times computation)
  "Execute TRAVERSAL TIMES times."
  (when (or (not times)  ; loop indefinitely
            (> times 0))
    (let ((result (symex-execute-traversal traversal
                                           computation)))
      (when result
        (let ((times (if times
                         (1- times)
                       times)))
          (funcall (symex--computation-reduce computation)
                   (funcall (symex--computation-f-to-aggregation computation)
                            result)
                   (symex--execute-circuit traversal
                                           times
                                           computation)))))))

(defun symex-execute-circuit (circuit computation)
  "Execute a circuit.

This repeats some traversal as specified."
  (let ((traversal (symex--circuit-traversal circuit))
        (times (symex--circuit-times circuit)))
    (let ((executed-phases (symex--execute-circuit traversal
                                                   times
                                                   computation)))
      (when executed-phases
        (apply #'symex-make-maneuver executed-phases)))))

(defun symex--execute-traversal-with-reorientation (reorientation traversal computation)
  "Apply a REORIENTATION and then attempt the TRAVERSAL.

If the traversal fails, then the reorientation is attempted as many times as
necessary until either it succeeds, or the reorientation fails.

Evaluates to a list of traversals executed, if any, which could be treated
as phases of a higher-level maneuver by the caller."
  (let ((executed-reorientation (symex-execute-traversal reorientation)))
    (when executed-reorientation
      (let ((executed-traversal (symex-execute-traversal traversal)))
        (let ((path (if executed-traversal
                        (funcall (symex--computation-f-to-aggregation computation)
                                 executed-traversal)
                      (symex--execute-traversal-with-reorientation reorientation
                                                                   traversal
                                                                   computation))))
          (when path
            (funcall (symex--computation-reduce computation)
                     (funcall (symex--computation-f-to-aggregation computation)
                              executed-reorientation)
                     path)))))))

(defun symex-execute-detour (detour computation)
  "Execute the DETOUR."
  (let ((original-location (point))
        (reorientation (symex--detour-reorientation detour))
        (traversal (symex--detour-traversal detour)))
    (let ((executed-phases (symex--execute-traversal-with-reorientation reorientation
                                                                        traversal
                                                                        computation)))
      (if executed-phases
          (apply #'symex-make-maneuver executed-phases)
        (goto-char original-location)
        nil))))

(defun symex--try-options-in-sequence (options computation)
  "Try options one at a time until one succeeds."
  (when options
    (let ((option (car options))
          (remaining-options (cdr options)))
      (let ((executed-option (symex-execute-traversal option
                                                      computation)))
        (if executed-option
            executed-option
          (symex--try-options-in-sequence remaining-options
                                          computation))))))

(defun symex-execute-protocol (protocol computation)
  "Given a protocol including a set of options, attempt to execute them
in order until one succeeds.

Evaluates to the maneuver actually executed."
  (let ((options (symex--protocol-options protocol)))
    (symex--try-options-in-sequence options
                                    computation)))

(defun symex-execute-traversal (traversal &optional computation)
  "Execute a tree traversal."
  (let ((computation (if computation
                         computation
                       computation-account)))
    (let ((executed-traversal (cond ((is-maneuver? traversal)
                                     (symex-execute-maneuver traversal
                                                             computation))
                                    ((is-circuit? traversal)
                                     (symex-execute-circuit traversal
                                                            computation))
                                    ((is-protocol? traversal)
                                     (symex-execute-protocol traversal
                                                             computation))
                                    ((is-precaution? traversal)
                                     (symex-execute-precaution traversal
                                                               computation))
                                    ((is-detour? traversal)
                                     (symex-execute-detour traversal
                                                           computation))
                                    ((is-move? traversal)
                                     (execute-tree-move traversal
                                                        computation))
                                    (t (error "Syntax error: unrecognized traversal type!")))))
      (when executed-traversal
        (if (and (not (is-protocol? traversal))
                 (not (is-precaution? traversal)))
            ;; TODO: better way to distinguish these types of traversals?
            (funcall (symex--computation-map computation)
                     executed-traversal)
          executed-traversal)))))

;;;;;;;;;;;;;;;;;;
;;; TRAVERSALS ;;;
;;;;;;;;;;;;;;;;;;

(defun symex-goto-first ()
  "Select first symex at present level"
  (interactive)
  (let ((traversal
         (symex-make-circuit
          move-go-backward)))
    (symex-execute-traversal traversal))
  (symex-refocus)
  (point))

(defun symex-goto-last ()
  "Select last symex at present level"
  (interactive)
  (let ((traversal
         (symex-make-circuit
          move-go-forward)))
    (symex-execute-traversal traversal))
  (symex-refocus)
  (point))

(defun symex-goto-outermost ()
  "Select outermost symex."
  (interactive)
  (let ((traversal
         (symex-make-circuit
          move-go-out)))
    (symex-execute-traversal traversal))
  (symex-refocus)
  (point))

(defun symex-goto-innermost ()
  "Select innermost symex."
  (interactive)
  (let ((traversal
         (symex-make-circuit
          (symex-make-protocol
           (symex-make-circuit
            move-go-in)
           move-go-forward))))
    (symex-execute-traversal traversal))
  (symex-refocus)
  (point))

;; TODO: is there a way to "monadically" build the tree data structure
;; (or ideally, do an arbitrary structural computation) as part of this traversal?
;; key is, it has to be inferrable from inputs and outputs alone, i.e. specifically
;; from the result of invocation of e.g. forward-symex
(defun symex-traverse-forward (&optional flow)
  "Traverse symex as a tree, using pre-order traversal.

If FLOW is true, continue from one tree to another. Otherwise, stop at end of
current rooted tree."
  (interactive)
  (let ((exit-until-root
         (symex-make-precaution
          move-go-out
          :post-condition (lambda ()
                            (not (point-at-root-symex?)))))
        (exit-until-end-of-buffer
         (symex-make-precaution
          move-go-out
          :post-condition (lambda ()
                            (not (point-at-final-symex?))))))
    (let ((traversal
           (symex-make-protocol
            (symex-make-protocol
             move-go-in
             move-go-forward)
            (symex-make-detour
             (if flow
                 exit-until-end-of-buffer
               exit-until-root)
             move-go-forward))))
      (symex-execute-traversal traversal))))

(defun symex-traverse-backward (&optional flow)
  "Traverse symex as a tree, using converse post-order traversal.

If FLOW is true, continue from one tree to another. Otherwise, stop at root of
current tree."
  (interactive)
  (let* ((postorder-in
          (symex-make-circuit
           (symex-make-maneuver
            move-go-in
            (symex-make-circuit
             move-go-forward))))
         (postorder-backwards-in
          (symex-make-maneuver move-go-backward
                               postorder-in))
         (postorder-backwards-in-tree
          (symex-make-precaution
           (symex-make-maneuver
            move-go-backward
            postorder-in)
           :pre-condition (lambda ()
                            (not (point-at-root-symex?))))))
    (let* ((traversal
            (symex-make-protocol
             (if flow
                 postorder-backwards-in
               postorder-backwards-in-tree)
             move-go-out)))
      (symex-execute-traversal traversal))))

(defun symex-switch-branch-backward ()
  "Switch branch backward"
  (interactive)
  (let ((index (symex-index))
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
                            (symex-go-forward index)
                            (let ((current-index (symex-index)))
                              (when (and (< current-index
                                            index)
                                         (> current-index
                                            closest-index))
                                (setq closest-index current-index)
                                (setq best-branch-position (point)))))))))
  (switch-backward))

(defun symex-switch-branch-forward ()
  "Switch branch forward"
  (interactive)
  (let ((index (symex-index)))
    (symex-go-out)
    (symex-go-forward)
    (symex-go-in)
    (symex-go-forward index)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSFORMATIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun symex-delete ()
  "Delete symex"
  (interactive)
  (sp-kill-sexp nil)
  (cond ((or (current-line-empty-p)  ; ^<>$
             (save-excursion (back-to-indentation)  ; ^(<>
                             (forward-char)
                             (lispy-right-p)))
         (progn (evil-previous-line)
                (symex-join-lines)))
        ((save-excursion (evil-last-non-blank)  ; (<>$
                         (lispy-left-p))
         (sp-next-sexp)
         (save-excursion
           (symex-join-lines t)))
        ((looking-at-p "\n")  ; (abc <>
         (evil-join (line-beginning-position)
                    (line-end-position)))
        (t (fixup-whitespace)))
  (symex-select-nearest)
  (symex-tidy))

(defun symex-change ()
  "Change symex"
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun symex-replace ()
  "Replace contents of symex"
  (interactive)
  (let ((move (symex-go-in)))
    (if move
        (apply #'evil-change (evil-inner-paren))  ; TODO: dispatch on paren type
      (sp-kill-sexp nil)
      (evil-insert-state))))

(defun symex-spit-backward ()
  "Spit backward"
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in)  ; need to be inside the symex to spit and slurp
      (paredit-backward-barf-sexp 1))
    (symex-go-forward)
    (when (symex-empty-list-p)
      (fixup-whitespace)
      (re-search-forward lispy-left)
      (symex-go-out))))

(defun symex-spit-forward ()
  "Spit forward"
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in)  ; need to be inside the symex to spit and slurp
      (paredit-forward-barf-sexp 1))
    (when (symex-empty-list-p)
      (symex-go-forward)
      (fixup-whitespace)
      (re-search-backward lispy-left))))

(defun symex-slurp-backward ()
  "Slurp from behind"
  (interactive)
  (when (lispy-left-p)
    (if (symex-empty-list-p)
        (forward-char)
      (symex-go-in))  ; need to be inside the symex to spit and slurp
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (symex-go-out)))

(defun symex-slurp-forward ()
  "Slurp from the front"
  (interactive)
  (when (lispy-left-p)
    (save-excursion
      (if (symex-empty-list-p)
          (forward-char)
        (symex-go-in))  ; need to be inside the symex to spit and slurp
      (lispy-forward-slurp-sexp 1))))

(defun symex-join ()
  "Merge symexes at the same level."
  (interactive)
  (save-excursion
    (symex-go-forward)
    (paredit-join-sexps)))

(defun symex-join-lines (&optional backwards)
  "Join lines inside symex."
  (interactive)
  (let ((original-column (current-column)))
    (save-excursion
      (if backwards
          (progn (evil-previous-line)
                 (if (current-line-empty-p)
                     (evil-join (line-beginning-position)
                                (1+ (line-beginning-position)))
                   (evil-join (line-beginning-position)
                              (line-end-position))))
        (forward-sexp)
        (evil-join (line-beginning-position)
                   (line-end-position))))
    (unless (= (current-column)
               original-column)
      (forward-char))))

(defun symex-yank ()
  "Yank (copy) symex."
  (interactive)
  (lispy-new-copy))

(defun symex-paste-before ()
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
      (symex-tidy))))

(defun symex-paste-after ()
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
      (symex-tidy))
    (symex-go-forward)))

(defun symex-open-line-after ()
  "Open new line after symex"
  (interactive)
  (forward-sexp)
  (newline-and-indent)
  (evil-insert-state))

(defun symex-open-line-before ()
  "Open new line before symex"
  (interactive)
  (newline-and-indent)
  (evil-previous-line)
  (indent-according-to-mode)
  (evil-append-line 1))

(defun symex-append-after ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (forward-sexp)  ; selected symexes will have the cursor on the starting paren
  (evil-insert 1 nil nil))

(defun symex-insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (evil-insert 1 nil nil))

(defun symex-insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (if (lispy-left-p)
      (evil-append 1 nil)
    (evil-insert 1 nil nil)))

(defun symex-insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (if (lispy-left-p)
      (progn (forward-sexp)
             (backward-char)
             (evil-insert 1 nil nil))
    (progn (forward-sexp)
           (evil-insert 1 nil nil))))

(defun symex-create (type)
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

(defun symex-insert-newline ()
  "Insert newline and reindent symex."
  (interactive)
  (newline-and-indent)
  (symex-tidy))

(defun symex-swallow ()
  "Swallow symex, putting its contents in the parent symex."
  (interactive)
  (symex-go-in)
  (symex-go-forward)
  (paredit-splice-sexp-killing-backward))

(defun symex-wrap-round ()
  "Wrap with ()"
  (interactive)
  (paredit-wrap-round)
  (symex-go-out))

(defun symex-wrap-square ()
  "Wrap with []"
  (interactive)
  (paredit-wrap-square)
  (symex-go-out))

(defun symex-wrap-curly ()
  "Wrap with {}"
  (interactive)
  (paredit-wrap-curly)
  (evil-find-char-backward nil 123))

(defun symex-wrap-angled ()
  "Wrap with <>"
  (interactive)
  (paredit-wrap-angled)
  (evil-find-char-backward nil 60))

(defun symex-wrap ()
  "Wrap with containing symex."
  (interactive)
  (symex-wrap-round)
  (symex-insert-at-beginning))

(defun symex-shift-forward ()
  "Move symex forward in current tree level."
  (interactive)
  (forward-sexp)
  (condition-case nil
      (progn (transpose-sexps 1)
             (backward-sexp))
    (error (backward-sexp))))

(defun symex-shift-backward ()
  "Move symex backward in current tree level."
  (interactive)
  (let ((move (symex-go-backward)))
    (when move
      (symex-shift-forward)
      (symex-go-backward))))

(defun symex-tidy ()
  "Auto-indent symex and fix any whitespace."
  (interactive)
  (fixup-whitespace)
  (when (save-excursion (looking-at-p "[[:space:]]"))
      (forward-char))
  (save-excursion
    (forward-sexp)
    (fixup-whitespace))
  (save-excursion
    (apply #'evil-indent
           (seq-take (evil-cp-a-form 1)
                     2)))
  (symex-select-nearest))

;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS ;;;
;;;;;;;;;;;;;;;;;;;;;

(defun symex-evaluate ()
  "Evaluate Symex"
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-eval-symex))
          ((member major-mode elisp-modes)
           (my-elisp-eval-symex))
          ((equal major-mode 'scheme-mode)
           (my-scheme-eval-symex))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-evaluate-definition ()
  "Evaluate top-level definition"
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-send-definition nil))
        ((member major-mode elisp-modes)
         (eval-defun nil))
        ((equal major-mode 'scheme-mode)
         (geiser-eval-definition nil))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-eval-print ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-print-last-sexp)))

(defun symex-describe ()
  "Lookup doc on symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-describe-symbol))
          ((member major-mode elisp-modes)
           (my-elisp-describe-symbol))
          ((equal major-mode 'scheme-mode)
           (my-scheme-describe-symbol))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-repl ()
  "Go to REPL."
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-repl))
        ((member major-mode elisp-modes)
         (my-lisp-repl))
        ((equal major-mode 'scheme-mode)
         (geiser-mode-switch-to-repl))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-select-nearest ()
  "Select symex nearest to point"
  (interactive)
  (cond ((and (not (eobp))
              (save-excursion (forward-char) (lispy-right-p)))  ; |)
         (forward-char)
         (lispy-different))
        ((looking-at-p "[[:space:]\n]")  ; <> |<> or <> |$
         (condition-case nil
             (progn (re-search-forward "[^[:space:]\n]")
                    (backward-char))
           (error (if-stuck (symex-go-backward)
                            (symex-go-forward)))))
        ((thing-at-point 'sexp)  ; som|ething
         (beginning-of-thing 'sexp))
        (t (if-stuck (symex-go-backward)
                     (symex-go-forward))))
  (symex-refocus)
  (point))

(defun symex-refocus (&optional smooth-scroll)
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
    (unless (< window-upper-view-bound
               window-current-line-number
               window-lower-view-bound)
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


(defhydra hydra-symex (:idle 1.0
                       :columns 5
                       :color pink
                       :body-pre (progn (symex-select-nearest)
                                        (evil-symex-state)))
  "Symex mode"
  ("(" (lambda ()
         (interactive)
         (symex-create 'round)) "()")
  ("[" (lambda ()
         (interactive)
         (symex-create 'square)) "[]")
  ("{" (lambda ()
         (interactive)
         (symex-create 'curly)) "{}")
  ("<" (lambda ()
         (interactive)
         (symex-create 'angled)) "<>")
  ("h" symex-go-backward "previous")
  ("j" symex-go-in "enter")
  ("k" symex-go-out "exit")
  ("l" symex-go-forward "next")
  ("f" (lambda ()
         (interactive)
         (symex-traverse-forward t)) "flow forward")
  ("b" (lambda ()
         (interactive)
         (symex-traverse-backward t)) "flow backward")
  ("C-k" symex-switch-branch-backward "switch branch backward")
  ("C-j" symex-switch-branch-forward "switch branch forward")
  ("y" symex-yank "yank (copy)")
  ("p" symex-paste-after "paste after")
  ("P" symex-paste-before "paste before")
  ("x" symex-delete "delete")
  ("c" symex-change "change" :exit t)
  ("s" symex-replace "replace" :exit t)
  ("H" symex-shift-backward "move backward")
  ("L" symex-shift-forward "move forward")
  ("K" paredit-raise-sexp "raise")
  ("s-J" symex-slurp-backward "slurp backward")
  ("s-H" symex-spit-backward "spit backward")
  ("s-L" symex-spit-forward "spit forward")
  ("s-K" symex-slurp-forward "slurp forward")
  ("z" symex-swallow "swallow")
  ("e" symex-evaluate "evaluate")
  ("d" symex-evaluate-definition "evaluate definition")
  ("E" eval-expression "eval expression")
  ("t" my-switch-to-scratch-buffer "scratch buffer" :exit t)
  ("G" my-switch-to-messages-buffer "messages buffer" :exit t)
  ("r" symex-repl "go to REPL" :exit t)
  ("|" lispy-split "split")
  ("m" symex-join "merge (join)")
  ("\\" lispy-splice "splice (join to higher level)")
  (")" symex-wrap-round "wrap with ()")
  ("]" symex-wrap-square "wrap with []")
  ("}" symex-wrap-curly "wrap with {}")
  (">" symex-wrap-angled "wrap with <>")
  ("o" symex-open-line-after "open line after" :exit t)
  ("O" symex-open-line-before "open line before" :exit t)
  ("n" symex-insert-newline "newline")
  ("J" symex-join-lines "join lines")
  ("N" (lambda ()
         (interactive)
         (symex-join-lines t)) "join lines backwards")
  ("0" symex-goto-first "go to first")
  ("M-h" symex-goto-first "go to first")
  ("$" symex-goto-last "go to last")
  ("M-l" symex-goto-last "go to last")
  ("M-k" symex-goto-outermost "go to outermost")
  ("M-j" symex-goto-innermost "go to innermost")
  ("=" symex-tidy "tidy/indent")
  ("A" symex-append-after "append after symex" :exit t)
  ("a" symex-insert-at-end "append inside symex" :exit t)
  ("i" symex-insert-at-beginning "insert inside symex" :exit t)
  ("I" symex-insert-before "insert before symex" :exit t)
  ("w" symex-wrap "wrap with symex" :exit t)
  ("g" evil-jump-to-tag "Go to definition")
  (";" symex-eval-print "eval + print")
  ;; canonical action
  ("s-;" symex-evaluate "evaluate" :exit t)
  ;; escape hatches
  ("R" evil-replace-state nil :exit t)
  ("v" evil-visual-char nil :exit t)
  ("V" evil-visual-line nil :exit t)
  ("C-v" evil-visual-block nil :exit t)
  ;; standard exits
  ("?" symex-describe "info")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)  ; since y looks like inverted lambda
(global-set-key (kbd "s-;") 'hydra-symex/body)  ; since y is hard to reach

(provide 'my-symex-mode)
