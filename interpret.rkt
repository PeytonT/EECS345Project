;EECS345 Programming Project Part 1
;Professor Lewicki
;Project Partners: Peyton Turner (dpt14) and Jack La Rue (jvl13)
;2/20/17
;Language used: Pretty Big
(load "simpleParser.scm")

(require racket/trace)

;Takes a file from the local directory and, using simpleParser, generates a parse tree (in list format) of the text in the file. The parse tree modifies the text of the original document
;such that it is made suitable for scheme (for example, if one line was "x + y;", then simpleParser would generate said expression as (+ x y).
;Evaluate is then called on each of these parsed expressions.
;Once return has been called throughout the evaluation of the parse tree, call/cc will automatically take the continuation return value and output it into the interactions pane.
(define interpret
  (lambda (filename)
    (call/cc (lambda (return-from-interpret) ((evaluate (car (parser filename)) (cdr (parser filename)) (empty-state) return-from-interpret))))))

;Takes the first expression, the rest of the parse tree, a state, and the continuation return. If the first expression is null, an error is thrown, as a program cannot not have a return statement.
;If the rest of the program is null, then the first expression is passed on to M_State to be evaluated further. Otherwise, the function calls M_State on the first expression and recursively calls M_State
;on the rest of the expressions in rest-of-program.
(define evaluate
  (lambda (first-line rest-of-program state master_return)
    (cond
      ((null? first-line) (error "Program Completed Without A Return Statement"))
      ((null? rest-of-program) (evaluate () () (M_state first-line state master_return) master_return))
      ((evaluate (car rest-of-program) (cdr rest-of-program) (M_state first-line state master_return) master_return)))))

;Takes an expression, a state, and the continuation return and returns the state after the inputted expression has been evaluated in said state. The expression can relate to
;variable declaration, variable assignments, returning values, if statements and while loops.
(define M_state
 (lambda (expr state master_return)
   (cond
      ((atom? expr) state)
      ((null? expr) state)
      ((eq? (car expr) 'var) (declare (cadr expr) (cddr expr) state master_return))
      ((eq? (car expr) '=) (assign (cadr expr) (caddr expr) state master_return))
      ((eq? (car expr) 'return) (return (M_value (cadr expr) state master_return) state master_return))
      ((eq? (car expr) 'if) (if* (cdr expr) state master_return))
      ((eq? (car expr) 'while) (while (cadr expr) (caddr expr) state master_return))
      (else state))))
     

;Takes an expression, a state, and the continuation return and returns the value of the expression evaluated in the given state. The evaluated expressions in M_value use math operators (i.e. + - * / %).
;to produce/declare/assign values. The expression may contain assignments.
(define M_value
  (lambda (expr state master_return)
    (cond
      ((atom? expr) (if (number? expr) expr (if (or (equal? expr 'true) (equal? expr 'false)) (if (equal? expr 'true) #t #f) (get-var-value expr state))))
      ((and (unary? expr) (eq? (car expr) '-)) (* -1 (M_value (cadr expr) state master_return)))
      ((eq? (car expr) '=) (M_value (caddr expr) state master_return))
      ((is_math_op? expr) ((get_math_op expr) (M_value (cadr expr) state master_return) (M_value (caddr expr) (M_state (cadr expr) state master_return) master_return)))
      ((is_bool_op? expr) (M_boolean expr state master_return))
      (else (error "You somehow called M_value on something without a value.")))))

;Takes an expression, a state, and the continuation return and returns the boolean value of the expression evaluated in the given state. The evaluated expressions in M_boolean use boolean operations
;(i.e. >, <, !=, ==, >=, <=, &&, ||, !) to produce/declare/assign values. The expression may contain assignments
(define M_boolean
  (lambda (expr state master_return)
    (cond
      ((atom? expr) (if (or (equal? expr 'true) (equal? expr 'false)) (if (equal? expr 'true) #t #f) expr))
      ((and (unary? expr) (eq? (car expr) '!)) (not (M_boolean (cadr expr) state master_return)))
      ((eq? (car expr) '=) (M_boolean (caddr expr) state master_return))
      ((is_bool_op? expr) ((get_bool_op expr) (M_value (cadr expr) state master_return) (M_value (caddr expr) (M_state (cadr expr) state master_return) master_return)))
      (else (error "You somehow called M_boolean on something without a boolean value.")))))

;Checks if an object is an atom. Returns true if so.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;Checks if an expression is unary. Returns true if so.
(define unary?
  (lambda (expr)
    (eq? (cddr expr) ())))
    
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
      ((eq? (car expr) '/) quotient)
      ((eq? (car expr) '%) modulo))))

;Tells us if the expression is a boolean operation.
(define is_bool_op?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((atom? expr) #f)
      ((eq? (car expr) '&&) #t)
      ((eq? (car expr) '||) #t)
      ((eq? (car expr) '!) #t)
      ((eq? (car expr) '==) #t)
      ((eq? (car expr) '!=) #t)
      ((eq? (car expr) '<) #t)
      ((eq? (car expr) '>) #t)
      ((eq? (car expr) '<=) #t)
      ((eq? (car expr) '>=) #t)
      (else #f))))

;If the expression is a boolean operation, it returns the appropriate operation. (Not is handled in M_boolean.)
(define get_bool_op
  (lambda (expr)
    (cond
      ((eq? (car expr) '&&) and_error)
      ((eq? (car expr) '||) or_error) 
      ;((eq? (car expr) '!) not_error) This should never happen, since it should be caught in M_boolean
      ((eq? (car expr) '==) eq_error) 
      ((eq? (car expr) '!=) not_eq_error) 
      ((eq? (car expr) '<) <_error) 
      ((eq? (car expr) '>) >_error) 
      ((eq? (car expr) '<=) <=_error) 
      ((eq? (car expr) '>=) >=_error)))) 

;Checks to see if inputted value is a primitive boolean.
(define is_boolean?
  (lambda (val)
    (cond
      ((null? val) #f)
      ((or (eq? val #t) (eq? val #f)) #t)
      (else #f))))      

;Takes two supposedly boolean inputs and ands them if they are actually booleans. Otherwise throws an error
(define and_error
  (lambda (bool1 bool2)
    (cond
      ((or (null? bool1) (null? bool2)) (error "Either one or both of your values are null."))
      ((and (is_boolean? bool1) (is_boolean? bool2)) (and bool1 bool2))
      (else (error "Attempted to treat a non-boolean as a boolean.")))))

;Takes two supposedly boolean inputs and ors them if they are actually booleans. Otherwise throws an error
(define or_error
  (lambda (bool1 bool2)
    (cond
      ((or (null? bool1) (null? bool2)) (error "Either one or both of your values are null."))
      ((and (is_boolean? bool1) (is_boolean? bool2)) (or bool1 bool2))
      (else (error "Attempted to treat a non-boolean as a boolean.")))))

;Takes two values and determines whether or not they are the same value. These values can be of type int or boolean.
(define eq_error
  (lambda (val1 val2)
    (cond
      ((or (null? val1) (null? val2)) (error "Either one or both of your values are null."))
      ((or (and (is_boolean? val1) (is_boolean? val2)) (and (not (is_boolean? val1)) (not (is_boolean? val2)))) (eq? val1 val2))
      (else (error "Values are of different types. Condition cannot be determined!")))))

;Takes two values and determines whether or not they are not equal. These values can be of type int or boolean.
(define not_eq_error
  (lambda (val1 val2)
    (cond
      ((or (null? val1) (null? val2)) (error "Either one or both of your values are null."))
      ((or (and (is_boolean? val1) (is_boolean? val2)) (and (not (is_boolean? val1)) (not (is_boolean? val2)))) (not (eq? val1 val2)))
      (else (error "Values are of different types. Condition cannot be determined!")))))

;Takes two values and determines whether or not val1 < val2. Values must be of type int.
(define <_error
  (lambda (val1 val2)
    (cond
      ((or (null? val1) (null? val2)) (error "Either one or both of your values are null."))
      ((and (not (is_boolean? val1)) (not (is_boolean? val2))) (< val1 val2))
      (else (error "One value is not of type int. Condition cannot be determined!")))))

;Takes two values and determines whether or not val1 > val2. Values must be of type int.
(define >_error
  (lambda (val1 val2)
    (cond
      ((or (null? val1) (null? val2)) (error "Either one or both of your values are null."))
      ((and (not (is_boolean? val1)) (not (is_boolean? val2))) (> val1 val2))
      (else (error "One value is not of type int. Condition cannot be determined!")))))

;Takes two values and determines whether or not val1 <= val2. Values must be of type int.
(define <=_error
  (lambda (val1 val2)
    (cond
      ((or (null? val1) (null? val2)) (error "Either one or both of your values are null."))
      ((and (not (is_boolean? val1)) (not (is_boolean? val2))) (<= val1 val2))
      (else (error "One value is not of type int. Condition cannot be determined!")))))

;Takes two values and determines whether or not val1 >= val2. Values must be of type int.
(define >=_error
  (lambda (val1 val2)
    (cond
      ((or (null? val1) (null? val2)) (error "Either one or both of your values are null."))
      ((and (not (is_boolean? val1)) (not (is_boolean? val2))) (>= val1 val2))
      (else (error "One value is not of type int. Condition cannot be determined!")))))

;Takes two lists l1 and l2 and returns (l1 l2)
(define encapsulate
  (lambda (l1 l2)
    (cons l1 (cons l2 ()))))

;Takes three inputs: two values and a list. Adds new first elements to a list of two lists.
(define newfirsts
  (lambda (f1 f2 l)
    (encapsulate (cons f1 (car l)) (cons f2 (cadr l)))))

;Removes the first elements of the sublists of a list. Said list is comprised of two lists.
(define removefirsts
  (lambda (l)
    (encapsulate (cdar l) (cdadr l))))

;Takes two inputs and makes them the first two values in the lists in an input list of two lists.
(define replacefirsts
 (lambda (f1 f2 l)
   (newfirsts f1 f2 (removefirsts l))))

;Gets the value of a given variable in a given state, or errors if no such variable exists.
(define get-var-value
  (lambda (var state)
    (cond
      ((null? (car state)) (error "Attempted to use an undeclared variable."))
      ((eq? var (caar state)) (if (not (null? (caadr state))) (caadr state) (error "Attempting to use unassigned variable.")))
      (else (get-var-value var (removefirsts state))))))

;Takes a variable, a list containing a value, a state, and the continuation return value and
;returns the state where the variable has been declared. If it is being declared but not initialized, use value ()
(define declare
  (lambda (var value state master_return)
    (cond
      ((null? value) (newfirsts var value state))
      (else (newfirsts var (M_value (car value) state master_return) state)))))

;Takes a variable, a value, and a state and sets the value of that variable in the state to be the given value. If the variable is not in the state an error is thrown.
(define update_state
  (lambda (var value state)
    (cond
      ((null? (car state)) (error "Variable is being assigned before it has been declared."))
      ((equal? var (caar state)) (replacefirsts var value state))
      (else (newfirsts (caar state) (caadr state) (update_state var value (removefirsts state)))))))

;Takes a variable, an expression, a state, and the continuation return value and returns the state where the variable is assigned to the value
;of the expression if the variable is declared. Otherwise creates an error.
(define assign
  (lambda (var expr state master_return)
    (update_state var (M_value expr state master_return) (M_state expr state master_return))))

;Takes an expression, a state, and the current continuation return value. The method then returns the value of the expression in the state. Once return is called, the program
;terminates, as the final value of the continuation return has been found.
(define return
  (lambda (value state master_return)
    (cond
      ((is_boolean? value) (if value (master_return 'true) (master_return 'false)))
      (else (master_return value)))))

;Takes an expression, containing a condition, a state and the continuation return.
;If the condition is true in the state, if returns the result of the first expression evaluated in the resulting state of
        ;evaluating the condition in the input state
;Otherwise, if returns the result of the second expression evaluated in the resulting state of evaluating the condition
        ;in the input state
(define if*
  (lambda (expr state master_return)
    (cond
      ((M_boolean (car expr) state master_return) (M_state (cadr expr) (M_state (car expr) state master_return) master_return))
      ((and (not (M_boolean (car expr) state master_return)) (not (eq? (cddr expr) ()))) (M_state (caddr expr) (M_state (car expr) state master_return) master_return))
      (else (M_state (car expr) state master_return)))))

;Takes a condition, a loop body, a state, and the continuation return value.
;If the condition is true in the state, it recursively calls itself on the condition, the loop body, and the state after
;the loop body has been called on the state after the condition has been called on the input state. Otherwise, it returns
;the state after the condition has been called on the input state.
(define while
  (lambda (condition loop state master_return)
    (cond
      ((M_boolean condition state master_return) (while condition loop (M_state loop (M_state condition state master_return) master_return) master_return))
      (else (M_state condition state master_return)))))