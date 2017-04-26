;Professor Lewicki
;Project Partners: Peyton Turner (dpt14), Jack La Rue (jvl13), and Jessie Adkins (jsa70) 
;4/12/17
;Language used: Pretty Big
(require racket/trace)
(include "HelperFunctions.rkt")
(include "CurrentProject.rkt")
(load "classParser.scm")



;General Project Notes:
;Empty states shall have the format: ((() ()))


(define copyState
  (lambda (state)
    (box (unbox state))))

(define createClassList
  (lambda ()
    (box (list (list 'Object) (list (void) (box (list () ())))))))

(define parentsName 
  (lambda (parse)
    (if (eq? () (first_rest_rest parse)) 
        'Object 
        (first_rest (first_rest_rest parse))))) 

(define getForm  
  (lambda (name unboxed_CDT)
    (cond
      ((null? (first unboxed_CDT)) (error "A class was extended that has not been declared."))
      ((eq? name (first_first unboxed_CDT)) (first_rest unboxed_CDT))
      (else (getForm name (removefirsts unboxed_CDT))))))

(define evaluate_class 
  (lambda (first_line rest_of_program boxed_state master_return)
    (cond
      ((null? first_line) (copyState boxed_state))
      ((null? rest_of_program) (begin
                                 (M_state first_line boxed_state master_return
                                          (lambda (x) (error "Called break outside of a loop"))
                                          (lambda (y) (error "Called continue outside of a loop"))
                                          (lambda (z) (error "Threw an exception outside of a try block")))
                                (copyState boxed_state)))
      (else (begin
              (M_state first_line boxed_state master_return
                           (lambda (x) (error "Called break outside of a loop"))
                           (lambda (y) (error "Called continue outside of a loop"))
                           (lambda (z) (error "Threw an exception outside of a try block")))
                   (evaluate_class (first rest_of_program) (rest rest_of_program) boxed_state master_return))))))

;Contextualize takes a state and copies the state into the context of each function in the state.
(define contextualize 
  (lambda (boxed_state)
    (set-box! boxed_state (replace_contexts boxed_state (first (unbox boxed_state))))))

(define replace_contexts 
  (lambda (boxed_state current_state)
    (cond
      ((null? (first current_state)) '(()()))
      (else (letrec ([top_L (first_first current_state)] [top_R (unbox (first_first_rest current_state))])
              (if (and (not (atom? top_R)) (eq? (length top_R) 3))
                  (newfirsts top_L (list (first top_R) boxed_state (first_rest_rest top_R)) (replace_contexts boxed_state (removefirsts current_state)))
                  (newfirsts top_L top_R (replace_contexts boxed_state (removefirsts current_state)))))))))

(define state_from_form 
  (lambda (form)
    (copyState (first_rest form))))

(define declareClass
  (lambda (parent_name parent_form parse)
    (let
        ([body (first (rest_rest_rest parse))]) 
      (list parent_name (evaluate_class (first body) (rest body) (state_from_form parent_form) (lambda (x)
                                                                                                 (error "Called return in a class body.")))))))

(define declareAllClasses
  (lambda (CDT parse_list)
    (cond
      ((null? parse_list) ) ;we want it to return nothing, just fill the CDT 
      (else (letrec ([parse (first parse_list)] [parentName (parentsName parse)] [parentForm (getForm parentName (unbox CDT))])
              (begin (set-box! CDT (cons (declareClass parentName parentForm parse) (unbox CDT))) (declareAllClasses CDT (rest parse_list))))))))