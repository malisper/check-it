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
(def unwalker check-that-form (value)
  `(return-from ,*test-block* ,(unwalk-form value)))

(def macro with-tests (&body body &environment env)
  (with-active-layers (with-tests)
    (let* ((ast (walk-form `(progn ,@body) :environment (make-walk-environment env)))
           ;; Collect-variable-references is poorly named. It makes it
           ;; possible to obtain all of the ast nodes of a given type.
           (check-that-forms (collect-variable-references ast :type 'check-that-form))
           (generator-forms  (collect-variable-references ast :type 'generator-form))
           (gensyms (mapcar #'gensym-of generator-forms))
           (generator `(generator (tuple ,@(mapcar (compose #'unwalk-form #'expr-of) generator-forms)))))
      (duplicate ast generator check-that-forms gensyms))))

(def function duplicate (ast generator check-that-forms gensyms)
  "Duplicates AST N times, with only a single, but different
   check-that in each one. GENERATOR is the generator expression that
   should be used to generate all of the values. GENSYMS are the
   symbols the values returned by GENERATOR should be bound to."
  (once-only (generator)
    `(progn
       ,@(loop for check-that-form in check-that-forms
               for gargs = (gensym "ARGS")
               collect `(check-it% ',(source-of (value-of check-that-form))
                                   ,generator
                                   (lambda (,gargs)
                                     (block ,*test-block*
                                       (destructuring-bind ,gensyms ,gargs
                                         (declare (ignorable ,@gensyms))
                                         ;; Substitute nil for all of
                                         ;; the check-that forms that
                                         ;; are not eq to the one form
                                         ;; we are leaving.
                                         ,(unwalk-form (substitute-ast-if
                                                        (walk-form nil)
                                                        (lambda (form)
                                                          (and (typep form 'check-that-form)
                                                               (not (eq check-that-form form))))
                                                        ast))))))))))
