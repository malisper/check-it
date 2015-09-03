(in-package :check-it)

(defparameter *test-block* (gensym "TEST-BLOCK")
  "The symbol used to name the blocks for the tests.")

(defparameter *check-that-num* (gensym "CHECK-THAT-NUM")
  "The symbol that holds the value of the check that form that should
   currently be running.")

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

;; Any check-that should immediately return its value from the
;; test-block since we are only checking one check-that at a time.
(def unwalker check-that-form (num value)
  ;; We should only return when the test fails. This way it is
  ;; possible to run a test multiple times within a loop.
  `(unless (or (/= ,num ,*check-that-num*) ,(unwalk-form value))
     (return-from ,*test-block* nil)))

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
      (generate-body ast generator gensyms check-that-forms))))

(defun number-check-that-forms (forms)
  "Numbers the check-that forms in FORMS."
  (loop for form in forms
        for i from 1
        do (setf (num-of form) i)))

(def function generate-body (ast generator gensyms check-that-forms)
  "Generates the body of the with-tests form, given the walked AST,
   the generator that should be used to generate all of the values,
   the gensyms that those generated values should be bound to, and all
   of the check-that forms in the with-tests form."
  (with-gensyms (gcode gargs)
    (once-only (generator)
      `(loop for ,*check-that-num* from 1 to ,(length check-that-forms)
             for ,gcode in '(,@(mapcar #'source-of check-that-forms))
             do (check-it% ,gcode
                           ,generator
                           (lambda (,gargs)
                             (block ,*test-block*
                               (destructuring-bind ,gensyms ,gargs
                                 (declare (ignorable ,@gensyms))
                                 ;; Substitute nil for all of
                                 ;; the check-that forms that
                                 ;; are not eq to the one form
                                 ;; we are leaving.
                                 ,(unwalk-form ast)))))))))
