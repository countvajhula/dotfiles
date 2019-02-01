;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;
;;; COMPUTATIONS ;;;
;;;;;;;;;;;;;;;;;;;;

(defun symex--type-integer (obj)
  "Convert an object to the integer type."
  (cond ((integerp obj)
         obj)
        ((stringp obj)
         (string-to-number obj))
        ((listp obj)
         (length obj))
        (t (error "Unexpected type %s in integer type conversion!" obj))))

(defun symex--type-list (obj)
  "Convert an object to the list type."
  (cond ((is-traversal? obj)
         (list obj))
        ((listp obj)
         obj)
        (t (list obj))))

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
  (symex-make-computation :f-to-aggregation #'symex--type-list
                          :reduce #'append
                          :f-from-aggregation #'car))

(defun symex--traversal-account (obj)
  "Represents the result of a traversal as a traversal."
  (cond ((is-traversal? obj)
         obj)
        (t (apply #'symex-make-maneuver obj))))

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

(defun symex--interpret-simple-traversal (traversal)
  "Interpret a traversal as a single, flat maneuver or move."
  (cond ((is-maneuver? traversal)
         (symex--simplify-maneuver traversal))
        ((is-move? traversal)
         traversal)
        (t (error "Syntax error: unrecognized traversal type!"))))

(defconst computation-simple-account
  ;; each result is cast as a maneuver and wrapped in a list for composition
  ;; the results are concatenated using list concatenation
  (symex-make-computation :f-to-aggregation #'symex--type-list
                          :map (-compose #'symex--interpret-simple-traversal
                                         #'symex--traversal-account)
                          :reduce #'append
                          :f-from-aggregation #'car))

(defconst computation-account
  ;; each result is cast as a maneuver and wrapped in a list for composition
  ;; the results are concatenated using list concatenation
  (symex-make-computation :f-to-aggregation #'symex--type-list
                          :map #'symex--traversal-account
                          :reduce #'append
                          :f-from-aggregation #'car))

(defun symex--streamline-to-maneuver (maneuver-or-move)
  "Streamline traversal to a representation as a maneuver.

If the argument is a maneuver, leave as is.
If it is a move, convert to the equivalent maneuver (via simple casting)."
  (if (is-move? maneuver-or-move)
      (symex-make-maneuver maneuver-or-move)
    maneuver-or-move))

(defun my-add-numbers (&rest numbers)
  "Sum numbers."
  (apply #'+ numbers))

(defconst computation-length
  ;; each result is interpreted down to a simple maneuver
  ;; the phases of this maneuver are extracted and summed using
  ;; move addition.
  ;; the results are accumulated using addition of numbers
  (symex-make-computation :f-to-aggregation #'symex--type-integer
                          :map (-compose #'symex--move-length
                                         #'symex--add-moves
                                         #'symex--maneuver-phases
                                         #'symex--streamline-to-maneuver
                                         #'symex--interpret-simple-traversal
                                         #'symex--traversal-account)
                          :reduce #'my-add-numbers
                          :f-from-aggregation #'car))

(provide 'symex-computations)
