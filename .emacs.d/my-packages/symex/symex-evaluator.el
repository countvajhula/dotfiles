;;; -*- lexical-binding: t -*-

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

(defun symex-execute-move (move &optional computation)
  "Execute the move as a traversal, i.e. such that it returns a list
of moves (singleton, in this case) rather than the executed move itself."
  (let ((executed-move (execute-tree-move move computation)))
    (when executed-move
      (list executed-move))))

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

(defun symex-execute-maneuver (maneuver computation)
  "Attempt to execute a given MANEUVER.

Attempts the maneuver in the order of its phases, accepting partial completion
of phases. If any phase fails entirely, then the maneuver it is part of is
terminated at that step.

Evaluates to the maneuver actually executed."
  (let ((phases (symex--maneuver-phases maneuver)))
    (when phases
      (let ((current-phase (car phases))
            (remaining-phases (cdr phases)))
        (let ((executed-phase (symex-execute-traversal current-phase)))
          (when executed-phase
            (append executed-phase
                    (symex-execute-traversal (apply #'symex-make-maneuver
                                                    remaining-phases)
                                             computation))))))))

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

(defun symex-execute-circuit (circuit computation)
  "Execute a circuit.

This repeats some traversal as specified."
  (let ((traversal (symex--circuit-traversal circuit))
        (times (symex--circuit-times circuit)))
    (when (or (not times)  ; loop indefinitely
              (> times 0))
      (let ((result (symex-execute-traversal traversal
                                             computation)))
        (when result
          (let ((times (if times
                           (1- times)
                         times)))
            (append result
                    (symex-execute-traversal (symex-make-circuit traversal
                                                                 times)
                                             computation))))))))

(defun symex-execute-detour (detour computation)
  "Execute the DETOUR.

Apply a reorientation and then attempt the traversal.

If the traversal fails, then the reorientation is attempted as many times as
necessary until either it succeeds, or the reorientation fails."
  (let ((reorientation (symex--detour-reorientation detour))
        (traversal (symex--detour-traversal detour)))
    (let ((executed-reorientation (symex-execute-traversal reorientation)))
      (when executed-reorientation
        (let ((executed-traversal (symex-execute-traversal traversal)))
          (let ((path (if executed-traversal
                          executed-traversal
                        (symex-execute-traversal (symex-make-detour reorientation
                                                                    traversal)
                                                 computation))))
            (when path
              (append executed-reorientation
                      path))))))))

(defun symex-execute-protocol (protocol computation)
  "Given a protocol including a set of options, attempt to execute them
in order until one succeeds.

Evaluates to the maneuver actually executed."
  (let ((options (symex--protocol-options protocol)))
    (when options
      (let ((option (car options))
            (remaining-options (cdr options)))
        (let ((executed-option (symex-execute-traversal option
                                                        computation)))
          (if executed-option
              executed-option
            (symex-execute-traversal (apply #'symex-make-protocol
                                            remaining-options)
                                     computation)))))))

(defun symex-execute-traversal (traversal &optional computation)
  "Execute a tree traversal."
  (let ((computation (if computation
                         computation
                       computation-default)))
    (let ((original-location (point))
          (executed-traversal (cond ((is-maneuver? traversal)
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
                                     (symex-execute-move traversal
                                                         computation))
                                    (t (error "Syntax error: unrecognized traversal type!")))))
      (unless executed-traversal
        (goto-char original-location))
      executed-traversal)))

(provide 'symex-evaluator)
