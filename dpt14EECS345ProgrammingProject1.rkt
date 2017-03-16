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
    (call/cc (lambda (return-from-interpret) ((evaluate (first (parser filename)) (rest (parser filename)) (empty-state) return-from-interpret))))))

;Takes the first expression, the rest of the parse tree, a state, and the continuation return. If the first expression is null, an error is thrown, as a program cannot not have a return statement.
;If the rest of the program is null, then the first expression is passed on to M_State to be evaluated further. Otherwise, the function calls M_State on the first expression and recursively calls M_State
;on the rest of the expressions in rest-of-program.
(define evaluate
  (lambda (first-line rest-of-program state master_return)
    (cond
      ((null? first-line) (error "Program Completed Without A Return Statement"))
      ((null? rest-of-program) (evaluate () () (M_state first-line state master_return) master_return))
      ((evaluate (first rest-of-program) (rest rest-of-program) (M_state first-line state master_return) master_return)))))

;Takes an expression, a state, and the continuation return and returns the state after the inputted expression has been evaluated in said state. The expression can relate to
;variable declaration, variable assignments, returning values, if statements and while loops.
(define M_state
 (lambda (expr state master_return)
   (cond
      ((atom? expr) state)
      ((null? expr) state)
      ((eq? (first expr) 'var) (declare (first-of-rest expr) (rest-of-rest expr) state master_return))
      ((eq? (first expr) '=) (assign (first-of-rest expr) (first-of-rest-of-rest expr) state master_return))
      ((eq? (first expr) 'return) (return (M_value (first-of-rest expr) state master_return) state master_return))
      ((eq? (first expr) 'if) (if* (rest expr) state master_return))
      ((eq? (first expr) 'while) (while (first-of-rest expr) (first-of-rest-of-rest expr) state master_return))
      (else state))))
     

;Takes an expression, a state, and the continuation return and returns the value of the expression evaluated in the given state. The evaluated expressions in M_value use math operators (i.e. + - * / %).
;to produce/declare/assign values. The expression may contain assignments.
(define M_value
  (lambda (expr state master_return)
    (cond
      ((atom? expr) (if (number? expr) expr (if (or (equal? expr 'true) (equal? expr 'false)) (if (equal? expr 'true) #t #f) (get-var-value expr state))))
      ((and (unary? expr) (eq? (first expr) '-)) (* -1 (M_value (first-of-rest expr) state master_return)))
      ((eq? (first expr) '=) (M_value (first-of-rest-of-rest expr) state master_return))
      ((is_math_op? expr) ((get_math_op expr) (M_value (first-of-rest expr) state master_return) (M_value (first-of-rest-of-rest expr) (M_state (first-of-rest expr) state master_return) master_return)))
      ((is_bool_op? expr) (M_boolean expr state master_return))
      (else (error "You somehow called M_value on something without a value.")))))

;Takes an expression, a state, and the continuation return and returns the boolean value of the expression evaluated in the given state. The evaluated expressions in M_boolean use boolean operations
;(i.e. >, <, !=, ==, >=, <=, &&, ||, !) to produce/declare/assign values. The expression may contain assignments
(define M_boolean
  (lambda (expr state master_return)
    (cond
      ((atom? expr) (if (or (equal? expr 'true) (equal? expr 'false)) (if (equal? expr 'true) #t #f) expr))
      ((and (unary? expr) (eq? (first expr) '!)) (not (M_boolean (first-of-rest expr) state master_return)))
      ((eq? (first expr) '=) (M_boolean (first-of-rest-of-rest expr) state master_return))
      ((is_bool_op? expr) ((get_bool_op expr) (M_value (first-of-rest expr) state master_return) (M_value (first-of-rest-of-rest expr) (M_state (first-of-rest expr) state master_return) master_return)))
      (else (error "You somehow called M_boolean on something without a boolean value.")))))

;Checks if an object is an atom. Returns true if so.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;Checks if an expression is unary. Returns true if so.
(define unary?
  (lambda (expr)
    (eq? (rest-of-rest expr) ())))
    
;Creates an empty program state
(define empty-state
  (lambda ()
    '(() ())))

;Creates an empty program state in the layers form
(define empty-state2
  (lambda ()
    '((() ()))))

;Adds an empty layer to the top of the current state
(define new_block
  (lambda (state)
    (cons (first (empty-state2)) state)))

;Tells us if the expression is a math operation
(define is_math_op?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((atom? expr) #f)
      ((eq? (first expr) '+) #t)
      ((eq? (first expr) '-) #t)
      ((eq? (first expr) '*) #t)
      ((eq? (first expr) '/) #t)
      ((eq? (first expr) '%) #t)
      (else #f))))

;If expression is a math operation, it returns the math operation
(define get_math_op
  (lambda (expr)
    (cond
      ((eq? (first expr) '+) +)
      ((eq? (first expr) '-) -)
      ((eq? (first expr) '*) *)
      ((eq? (first expr) '/) quotient)
      ((eq? (first expr) '%) modulo))))

;Tells us if the expression is a boolean operation.
(define is_bool_op?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((atom? expr) #f)
      ((eq? (first expr) '&&) #t)
      ((eq? (first expr) '||) #t)
      ((eq? (first expr) '!) #t)
      ((eq? (first expr) '==) #t)
      ((eq? (first expr) '!=) #t)
      ((eq? (first expr) '<) #t)
      ((eq? (first expr) '>) #t)
      ((eq? (first expr) '<=) #t)
      ((eq? (first expr) '>=) #t)
      (else #f))))

;If the expression is a boolean operation, it returns the appropriate operation. (Not is handled in M_boolean.)
(define get_bool_op
  (lambda (expr)
    (cond
      ((eq? (first expr) '&&) and_error)
      ((eq? (first expr) '||) or_error) 
      ;((eq? (first expr) '!) not_error) This should never happen, since it should be caught in M_boolean
      ((eq? (first expr) '==) eq_error) 
      ((eq? (first expr) '!=) not_eq_error) 
      ((eq? (first expr) '<) <_error) 
      ((eq? (first expr) '>) >_error) 
      ((eq? (first expr) '<=) <=_error) 
      ((eq? (first expr) '>=) >=_error)))) 

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
    (encapsulate (cons f1 (first l)) (cons f2 (first-of-rest l)))))

;Takes an input list of two lists and returns a list of the first two elements of those lists
(define getfirsts
  (lambda (l)
    (cons (first-of-first l) (cons (first-of-first-of-rest l) ()))))

;Removes the first elements of the sublists of a list. Said list is comprised of two lists.
(define removefirsts
  (lambda (l)
    (encapsulate (rest-of-first l) (rest-of-first-of-rest l))))

;Takes two inputs and makes them the first two values in the lists in an input list of two lists.
(define replacefirsts
 (lambda (f1 f2 l)
   (newfirsts f1 f2 (removefirsts l))))

;Gets the value of a given variable in a given state, or errors if no such variable exists.
(define get-var-value
  (lambda (var state)
    (cond
      ((null? (first state)) (error "Attempted to use an undeclared variable."))
      ((eq? var (first-of-first state)) (if (not (null? (first-of-first-of-rest state))) (first-of-first-of-rest state) (error "Attempting to use unassigned variable.")))
      (else (get-var-value var (removefirsts state))))))

;Gets the value of a variable possibly stored in a layer. Returns (boolean value), where boolean is true and value is the var's value if the var was present, and boolean is false otherwise
(define get-var-value-layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) (cons #f '(())))
      ((eq? var (first-of-first layer)) (if (not (null? (first-of-first-of-rest layer))) (cons #t (cons (first-of-first-of-rest layer) '())) (error "Attempting to use unassigned variable.")))
      (else (get-var-value-layer var (removefirsts layer))))))

;Gets the value of a given varable in a given layer state, or errors if no such variable exists.
(define get-var-value2
  (lambda (var state)
    (cond
      ((null? state) (error "Attempting to use an undeclared variable."))
      ((first (get-var-value-layer var (first state))) (first-of-rest (get-var-value-layer var (first state))))
      (else (get-var-value2 var (rest state))))))

;Takes a variable, a list containing a value, a state, and the continuation return value and
;returns the state where the variable has been declared. If it is being declared but not initialized, use value ().
(define declare
  (lambda (var value state master_return)
    (cond
      ((null? value) (newfirsts var value state))
      ;What's going on with calling M_value on (first value). Shouldn't it just be called on value? I wrote this and I am confused. DPT
      (else (newfirsts var (M_value (first value) state master_return) state)))))

;Takes a variable, a list containing a value, a state, and the continuation return value and returns the state where
;the variable has been declared in the top layer. If it is being declared but not initialized, use value ().
(define declare2
  (lambda (var value state master_return)
    (cond
      ((null? value) (cons (newfirsts var value (first state)) (rest state)))
      (else (cons (newfirsts var (M_value value state master_return) (first state)) (rest state))))))

;Takes a variable, a value, and a state and sets the value of that variable in the state to be the given value. If the variable is not in the state an error is thrown.
(define update_state
  (lambda (var value state)
    (cond
      ((null? (first state)) (error "Variable is being assigned before it has been declared."))
      ((equal? var (first-of-first state)) (replacefirsts var value state))
      (else (newfirsts (first-of-first state) (first-of-first-of-rest state) (update_state var value (removefirsts state)))))))

;Takes a variable, a value, and a layer of a state and sets the value of that variable in the state to be the given value if it is present.
;Returns (#t updated_layer) if the value was present, and (#f layer) otherwise.
(define update_layer
  (lambda (var value layer)
    (cond
      ((null? (first layer)) (cons #f (cons layer ())))
      ((eq? var (first-of-first layer)) (cons #t (cons (replacefirsts var value layer) ())))
      (else (let ([result (update_layer var value (removefirsts layer))])
              (cons (first result) (cons (newfirsts (first (getfirsts layer)) (first-of-rest (getfirsts layer)) (first-of-rest result)) ()))))))) ;What have I done? DPT

;Takes a variable and a layer and returns true if the variable is in the layer and false otherwise.
(define in_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) #f)
      ((eq? var (first-of-first layer)) #t)
      (else (in_layer var (removefirsts layer))))))

;Takes a variable, a value, and a state and sets the value of that variable in the state to be the given value. Works on layer states.
(define update_state2
  (lambda (var value state)
    (cond
      ((null? state) (error "Variable is being assigned before it has been declared."))
      ((in_layer var (first state)) (cons (update_layer var value (first state)) (rest state)))
      (else (cons (first state) (update_state2 var value (rest state)))))))

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
      ((M_boolean (first expr) state master_return) (M_state (first-of-rest expr) (M_state (first expr) state master_return) master_return))
      ((and (not (M_boolean (first expr) state master_return)) (not (eq? (rest-of-rest expr) ()))) (M_state (first-of-rest-of-rest expr) (M_state (first expr) state master_return) master_return))
      (else (M_state (first expr) state master_return)))))

;Takes a condition, a loop body, a state, and the continuation return value.
;If the condition is true in the state, it recursively calls itself on the condition, the loop body, and the state after
;the loop body has been called on the state after the condition has been called on the input state. Otherwise, it returns
;the state after the condition has been called on the input state.
(define while
  (lambda (condition loop state master_return)
    (cond
      ((M_boolean condition state master_return) (while condition loop (M_state loop (M_state condition state master_return) master_return) master_return))
      (else (M_state condition state master_return)))))

;This section is for abstractions of the car/cdr functions. first and rest are already implemented by Pretty Big

;Functionality of caar
(define first-of-first
  (lambda (l)
    (caar l)))

;Functionality of cddr
(define rest-of-rest
  (lambda (l)
    (cddr l)))

;Functionality of cadr
(define first-of-rest
  (lambda (l)
    (cadr l)))

;Functionality of cdar
(define rest-of-first
  (lambda (l)
    (cdar l)))

;Functionality of caaar
(define first-of-first-of-first
  (lambda (l)
    (caaar l)))

;Functionality of caadr
(define first-of-first-of-rest
  (lambda (l)
    (caadr l)))

;Functionality of cadar
(define first-of-rest-of-first
  (lambda (l)
    (cadar l)))

;Functionality of caddr
(define first-of-rest-of-rest
  (lambda (l)
    (caddr l)))

;Functionality of cdaar
(define rest-of-first-of-first
  (lambda (l)
    (cdaar l)))

;Functionality of cdadr
(define rest-of-first-of-rest
  (lambda (l)
    (cdadr l)))

;Functionality of cddar
(define rest-of-rest-of-first
  (lambda (l)
    (cddar l)))

;Functionality of cdddr
(define rest-of-rest-of-rest
  (lambda (l)
    (cdddr l)))