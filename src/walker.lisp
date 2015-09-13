(in-package :check-it)

(defparameter *check-that-num* nil
  "This variable will be bound to a gensym that will hold the value of
   the check that form that should be running.")

(def layer with-tests)

(def form-class generator-form ()
  ((expr nil :ast-link t)
   (gensym (gensym "GENERATOR-VALUE"))))

(def form-class check-that-form ()
  ((num)
   (value nil :ast-link t)))

(def (walker :in with-tests) generator
  (with-form-object (form 'generator-form -parent-)
    (setf (expr-of form)
          ;; All of the nested generators should be left alone so we
          ;; deactivate the with-tests layer.
          (with-inactive-layers (with-tests)
            (hu.dwim.walker::recurse -form-)))))

;; Replace a generator form with the gensym that will be bound to it.
(def unwalker generator-form (gensym)
  gensym)

(def (walker :in with-tests) check-that
  (with-form-object (form 'check-that-form -parent-)
    (setf (value-of form) (hu.dwim.walker::recurse (cadr -form-)))))

(def unwalker check-that-form (num value)
  ;; When the number is zero we want to run all of the tests.
  `(when (or (= ,*check-that-num* 0) (= ,*check-that-num* ,num))
     (assert-true ,(unwalk-form value))))

(def macro with-tests (&body body &environment env)
  (with-active-layers (with-tests)
    ;; The code should return T if the body runs through. That means
    ;; no tests have failed since they will short circuit.
    (let* ((ast (walk-form `(progn ,@body t) :environment (make-walk-environment env)))
           ;; Collect-variable-references is poorly named. It makes it
           ;; possible to obtain all of the ast nodes of a given type.
           (generator-forms  (collect-variable-references ast :type 'generator-form))
           (generator `(generator (tuple ,@(mapcar (compose #'unwalk-form #'expr-of) generator-forms))))
           (gensyms (mapcar #'gensym-of generator-forms))
           (check-that-forms (collect-variable-references ast :type 'check-that-form)))
      (number-check-that-forms check-that-forms)
      (generate-body ast generator gensyms (length check-that-forms)))))

(defun number-check-that-forms (forms)
  "Numbers the check-that forms in FORMS."
  (loop for form in forms
        for i from 1
        do (setf (num-of form) i)))

(def function generate-body (ast generator gensyms n)
  "Generates the body of the with-tests form, given the walked AST,
   the generator that should be used to generate all of the values,
   the gensyms that those generated values should be bound to, and the
   number of check-that clauses in the body."
  (with-gensyms (garg)
    ;; If 1 <= *check-that-num <= N, the test with that specific
    ;; number is ran. If *check-that-num = 0, all of the tests are
    ;; ran.
    (let ((*check-that-num* (gensym "CHECK-THAT-NUM")))
      `(test-group (lambda (,garg ,*check-that-num*)
                     (destructuring-bind ,gensyms ,garg
                       (declare (ignorable ,@gensyms))
                       ,(unwalk-form ast)))
                   ,generator
                   ,n))))
