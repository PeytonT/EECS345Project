(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (evaluate (car (parser filename)) (cdr (parser filename)) (empty-state))))

(define evaluate
  (lambda (first-line rest-of-program state)
    (cond
      ((null? first-line) (error "Program Completed Without A Return Statement"))
      ((evaluate (car rest-of-program) (cdr rest-of-program) (M_state first-line state))))))

;Takes an expression and a state and returns the value of the expression evaluated in the given state. The expression may contain assignments.
(define M_value
  (lambda (expr state)
    (cond
      ((atom? expr) (if (number? expr) expr (get-var-value expr state)))
      ((eq? (cddr expr) ()) (if (eq? (car expr) '-) (* -1 (cadr expr)) (error "An expression is being evaluated with too few operands."))) ;handles the unary "-" operator
      ((eq? (car expr) '=) (M_value (caddr expr) state))
      ((is_math_op? expr) ((get_math_op expr) (M_value (cadr expr) state) (M_value (caddr expr) state)))
      (else (error "You somehow called M_value on something without a value.")))))

;Takes an expression and a state and returns the boolean value of the expression evaluated in the given state. The expression may contain assignments
(define M_boolean
  (lambda (expr state)
    (cond
      ((atom? expr) expr)
      ((eq? (cddr expr) '!) (not (M_boolean (cadr expr) state)))
      ((eq? (car expr) '=) (M_boolean (caddr expr) state))
      ((is_bool_op? expr) ((get_bool_op expr) (M_boolean (cadr expr) state) (M_boolean (caddr expr) state)))
      ((is_comp_op? expr) ((get_comp_op expr) (M_value (cadr expr) state) (M_value (caddr expr) state)))
      (else (error "You somehow called M_boolean on something without a boolean value.")))))

;Checks if an object is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
    
;Creates an empty program state
(define empty-state
  (lambda ()
    '(() ())))

;Tells us if the expression is a math operation
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

;If expression is a math operation, it returns the math operation
(define get_math_op
  (lambda (expr)
    (cond
      ((eq? (car expr) '+) +)
      ((eq? (car expr) '-) -)
      ((eq? (car expr) '*) *)
      ((eq? (car expr) '/) /)
      ((eq? (car expr) '%) %))))

;Tells us if the expression is a boolean operation.
(define is_bool_op?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((atom? expr) #f)
      ((eq? (car expr) '&&) #t)
      ((eq? (car expr) '||) #t)
      ((eq? (car expr) '!) #t)
      (else #f))))

;If the expression is a boolean operation, it returns the appropriate operation. (Not is handled in M_boolean.)
(define get_bool_op
  (lambda (expr)
    (cond
      ((eq? (car expr) '&&) and_error)
      ((eq? (car expr) '||) or_error))))

;Tells us if the expression is a comparison operation.
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

;If the expression is a comparision operation, it returns the appropriate operation.
(define get_comp_op
  (lambda (expr)
    (cond

;Takes two supposedly boolean inputs and ands them if they are actually booleans. Otherwise throws an error
(define and_error
  (lambda (bool1 bool2)
    (cond
      ((and (or (eq? bool1 #t) (eq? bool1 #f)) (or (eq? bool2 #t) (eq? bool2 #f))) (and bool1 bool2))
      (else (error "Attempted to treat a non-boolean as a boolean.")))))

;Takes two supposedly boolean inputs and ors them if they are actually booleans. Otherwise throws an error
(define or_error
  (lambda (bool1 bool2)
    (cond
      ((and (or (eq? bool1 #t) (eq? bool1 #f)) (or (eq? bool2 #t) (eq? bool2 #f))) (or bool1 bool2))
      (else (error "Attempted to treat a non-boolean as a boolean.")))))

;Takes two lists l1 and l2 and returns (l1 l2)
(define encapsulate
  (lambda (l1 l2)
    (cons l1 (cons l2 ()))))

;Adds new first elements to a list of two lists
(define newfirsts
  (lambda (f1 f2 l)
    (encapsulate (cons f1 (car l)) (cons f2 (cadr l)))))

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
;of the expression if the variable is declared. Otherwise creates an error.
(define assign
  (lambda (var expr state)
    (cond
      ((null? (car state)) (error "Variable is being assigned before it has been declared."))
      ((equal? var (caar state)) (encapsulate (car state) (cons (M_value expr state) (cdadr state))))
      (else (newfirsts (caar state) (cadr state) (assign var expr (encapsulate (cadr state) (cddr state))))))))
