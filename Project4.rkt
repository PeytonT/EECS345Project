;Professor Lewicki
;Project Partners: Peyton Turner (dpt14), Jack La Rue (jvl13), and Jessie Adkins (jsa70) 
;4/12/17
;Language used: Pretty Big
(require racket/trace)
(include "HelperFunctions.rkt")
(include "CurrentProject.rkt")
(load "classParser.scm")

(define createClassList
  (lambda ()
    (box (list (list 'Object) (list (void) (box (list () ())))))))

(define parentsName
  (lambda (parse)
    (if (eq? () (first_rest_rest parse)) 'Object (first_rest (first_rest_rest parse)))))

(define getForm
  (lambda (name unboxed_CDT)
    (cond
      ((null? (first unboxed_CDT)) (error "A class was extended that has not been declared."))
      ((eq? name (first_first unboxed_CDT)) (first_rest unboxed_CDT))
      (else (getForm name (removefirsts unboxed_CDT))))))

(define evaluate_class
  (lambda (first_line rest_of_program boxed_state master_return)
    (cond
      ((null? first_line) boxed_state)
      ((null? rest_of_program) (begin
                                 (M_state first_line boxed_state master_return
                                          (lambda (x) (error "Called break outside of a loop"))
                                          (lambda (y) (error "Called continue outside of a loop"))
                                          (lambda (z) (error "Threw an exception outside of a try block")))
                                 boxed_state))
      (else (begin
              (M_state first_line boxed_state master_return
                           (lambda (x) (error "Called break outside of a loop"))
                           (lambda (y) (error "Called continue outside of a loop"))
                           (lambda (z) (error "Threw an exception outside of a try block")))
                   (evaluate_class (first rest_of_program) (rest rest_of_program) boxed_state master_return))))))

;Contextualize takes a state and copies the state of the last function declared in the state to each other function in the state.
(define contextualize
  (lambda 

;Returns unboxed states because we don't want to actually modify the state stored in the CDT
(define state_from_form
  (lambda (form)
    (unbox (first_rest form))))

(define declareClass
  (lambda (parent_name parent_form parse)
    (let
        ([body (first (rest_rest_rest parse))])
      (list parent_name (evaluate_class (first body) (rest body) (state_from_form parent_form) (lambda (x) (error "Called return in a class body.")))))))

(define declareAllClasses
  (lambda (CDT parse_list)
    ()))