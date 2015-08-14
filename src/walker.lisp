(in-package :check-it)

(defparameter *test-block* (gensym "TEST-BLOCK")
  "The symbol used to name the blocks for the tests.")

(def layer with-tests)

(def form-class generator-form ()
  ((expr nil :ast-link t)
   (gensym (gensym "GENERATOR-VALUE"))))

(def form-class check-that-form ()
  ((value nil :ast-link t)))

(def (walker :in with-tests) generator
  (with-form-object (form 'generator-form -parent-)
    (setf (expr-of form)
          ;; All of the nested generators should be left alone.
          (with-inactive-layers (with-tests)
            (hu.dwim.walker::recurse -form-)))))

(def unwalker generator-form (gensym)
  gensym)

(def (walker :in with-tests) check-that
  (with-form-object (form 'check-that-form -parent-)
    (setf (value-of form) (hu.dwim.walker::recurse (cadr -form-)))))

(def unwalker check-that-form (value)
  `(return-from ,*test-block* ,(unwalk-form value)))

;; Substitute every check-that-form that remains with the gensym used
;; for it.
(def macro with-tests (&body body &environment env)
  (process-body `(progn ,@body) (make-walk-environment env)))

(def function process-body (body &optional env)
  (with-active-layers (with-tests)
    (let* ((ast (walk-form body :environment env))
           (check-that-forms (collect-variable-references ast :type 'check-that-form))
           (generator-forms (collect-variable-references ast :type 'generator-form))
           (gensyms (mapcar #'gensym-of generator-forms))
           (generator `(generator (tuple ,@(mapcar (compose #'unwalk-form #'expr-of) generator-forms)))))
      (duplicate ast generator check-that-forms gensyms))))

(def function duplicate (ast generator check-that-forms gensyms)
  "Duplicates AST N times, with only a single, but different
   check-that in each one. GENERATOR is the generator expression that
   should be used for to generate all of the values. GENSYMS are the
   symbols the values returned by GENERATOR should be bound to."
  (once-only (generator)
    `(progn ,@(loop with gargs = (gensym "ARGS")
                    for check-that-form in check-that-forms
                    collect `(check-it% ',(source-of check-that-form)
                                        ,generator
                                        (lambda (,gargs)
                                          (block ,*test-block*
                                            (destructuring-bind ,gensyms ,gargs
                                              (declare (ignorable ,@gensyms))
                                              ,(unwalk-form (substitute-ast-if
                                                             (walk-form nil)
                                                             (lambda (form)
                                                               (and (typep form 'check-that-form)
                                                                    (not (eq check-that-form form))))
                                                             ast))))))))))
