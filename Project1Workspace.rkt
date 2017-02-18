(load "simpleParser.scm")

(define empty-state
  (lambda ()
    '(() ())))

(define interpret
  (lambda (filename)
    (parse (car (parser "filename")) (parser "filename") ())))

(define parse
  (lambda (expr rest state)
    (parse (car rest) (cdr rest) (M_state expr state))))

;Takes an expression and a state and returns the value of the expression evaluated in the given state. The expression may
;contain assignments.
(define M_value
  (lambda (expr state)
    (cond
      ((atom? expr) (if (number? expr) expr (get-var-value expr state)))
      ((eq? (car expr) '=) (M_value (caddr expr) state))
      ((is_math_op? expr) ((get_math_op expr) (M_value (cadr expr) state) (M_value (caddr expr) state)))
      (else (error "You somehow called M_value on something without a value.")))))

(define M_boolean
  (lambda (expr state)
    (

(define M_state
  (lambda (expr state)
    (cond
      ((is_math_op? (car expr)) (M_state (
      ((is_bool_op? (car expr))
      ((is_comp_op? (car expr))
      ((is_assign? (car expr))
      ((is_declare? (car expr))
      ((is_if? (car expr))
      ((is_while? (car expr))
      ((is_return? (car expr))


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define is_math_op?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((atom? expr) #f)
      ((eq? (car expr) '+) #t)
      ((eq? (car expr) '-) #t)
      ((eq? (car expr) '*) #t)
      ((eq? (car expr) '/) #t)
      ((eq? (car expr) '%) #t)
      (else #f))))

(define get_math_op
  (lambda (expr)
    (cond
      ((eq? (car expr) '+) '+)
      ((eq? (car expr) '-) '-)
      ((eq? (car expr) '*) '*)
      ((eq? (car expr) '/) '/)
      ((eq? (car expr) '%) '%))))

(define is_bool_op?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((atom? expr) #f)
      ((eq? (car expr) '&&) #t)
      ((eq? (car expr) '||) #t)
      ((eq? (car expr) '!) #t)
      (else #f))))

(define is_comp_op?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((atom? expr) #f)
      ((eq? (car expr) '==) #t)
      ((eq? (car expr) '!=) #t)
      ((eq? (car expr) '<) #t)
      ((eq? (car expr) '>) #t)
      ((eq? (car expr) '<=) #t)
      ((eq? (car expr) '>=) #t)
      (else #f))))

(define is_assign?
  (lambda s
    (

(define is_declare?
  (lambda s
    (

(define is_if?
  (lambda s
    (
 
(define is_while?
  (lambda s
    (

(define is_while?
  (lambda s
    (

(define is_return?
  (lambda s
    (


;Takes two lists l1 and l2 and returns (l1 l2)
(define encapsulate
  (lambda (l1 l2)
    (cons l1 (cons l2 ()))))

;Removes the first elements of the sublists of a two list list
(define removefirsts
  (lambda (l)
    (encapsulate (cdar l) (cdadr l))))

(define get-var-value
  (lambda (var state)
    (cond
      ((null? (car state)) (error "Attempted to use an undeclared variable."))
      ((eq? var (caar state)) (caadr state))
      (else (get-var-value var (removefirsts state))))))
     


;Takes a variable and a state and returns the state where the variable has been declared. If it is being declared but not initialized, use value ()
(define declare
  (lambda (var value state)
    (newfirsts var value state)))

;Takes a variable, an expression, and a state and returns the state where the variable is assigned to the value
;of the expression if the variable is declared. Otherwise an error is returned.
(define assign
  (lambda (var expr state)
    (cond
      ((null? (car state)) (error "Variable is being assigned before it has been declared."))
      ((equal? var (caar state)) (encapsulate (car state) (cons (M_value expr state) (cdadr state))))
      (else (newfirsts (caar state) (cadr state) (assign var expr (encapsulate (cadr state) (cddr state))))))))

;Takes a condition, two expressions, and a state.
;If the condition is true in the state, if returns the result of the first expression evaluated in the resulting state of
        ;evaluating the condition in the input state
;Otherwise, if returns the result of the second expression evaluated in the resulting state of evaluating the condition
        ;in the input state
(define if
  (lambda (cond expr1 expr2 state)
    (cond
      ((M_boolean cond state) (M_state expr1 (M_state cond state)))
      (else (M_state expr2 (M_state cond state))))))

;Takes a condition, a loop body, and a state.
;If the condition is true in the state, it recursively calls itself on the condition, the loop body, and the state after
;the loop body has been called on the state after the condition has been called on the input state. Otherwise, it returns
;the state after the condition has been called on the input state.
(define while
  (lambda (condition loop state)
    (cond
      ((M_boolean condition state) (while condition loop (M_state loop (M_state condition state))))
      (else (M_state condition state)))))