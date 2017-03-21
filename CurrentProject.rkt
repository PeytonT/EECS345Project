;EECS345 Programming Project Part 2
;Professor Lewicki
;Project Partners: Peyton Turner (dpt14) and Jack La Rue (jvl13)
;3/20/17
;Language used: Pretty Big
(load "simpleParser.scm")

(require racket/trace)

;Takes a file from the local directory and, using simpleParser, generates a parse tree (in list format) of the text in the file. The parse tree modifies the text of the original document
;such that it is made suitable for scheme (for example, if one line was "x + y;", then simpleParser would generate said expression as (+ x y).
;Evaluate is then called on each of these parsed expressions.
;Once return has been called throughout the evaluation of the parse tree, call/cc will automatically take the continuation return value and output it into the interactions pane.
(define interpret
  (lambda (filename)
    (call/cc (lambda (return_from_interpret) ((evaluate (first (parser filename)) (rest (parser filename)) (empty_state_box) return_from_interpret))))))

;Takes the first expression, the rest of the parse tree, a boxed state, and the continuation return. If the first expression is null, an error is thrown, as a program cannot not have a return statement.
;If the rest of the program is null, then the first expression is passed on to M_State to be evaluated further. Otherwise, the function calls M_State on the first expression and recursively calls M_State
;on the rest of the expressions in rest_of_program.
(define evaluate
  (lambda (first_line rest_of_program boxed_state master_return)
    (cond
      ((null? first_line) (error "Program Completed Without A Return Statement"))
      ((null? rest_of_program) (M_state_box first_line boxed_state master_return
                                 (lambda (x) (error "Called break outside of a loop"))
                                 (lambda (y) (error "Called continue outside of a loop"))
                                 (lambda (z) (error "Threw an exception outside of a try block"))))
      (else (begin
              (M_state_box first_line boxed_state master_return
                           (lambda (x) (error "Called break outside of a loop"))
                           (lambda (y) (error "Called continue outside of a loop"))
                           (lambda (z) (error "Threw an exception outside of a try block")))
                   (evaluate (first rest_of_program) (rest rest_of_program) boxed_state master_return))))))

;Takes an expression, a state in a box, and the continuations for return, break, continue, throw, and block handling,
;and updates the state in the box to the state after the input expression has been evaluated.
(define M_state_box
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((atom? expr) )
      ((null? expr) )
      ;Handles the two possibilities of variable declaration, either assigning an empty list if now value is given, or the given value if a value is given.
      ((eq? (first expr) 'var) (if (null? (rest_of_rest expr))
                                   (handle_declare (first_of_rest expr) () boxed_state master_return break continue throw)
                                   (handle_declare (first_of_rest expr) (first_of_rest_of_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) '=) (handle_assign (first_of_rest expr) (first_of_rest_of_rest expr) boxed_state master_return break continue throw))
      ((eq? (first expr) 'return) (return (M_value (first_of_rest expr) boxed_state master_return break continue throw) master_return))
      ((eq? (first expr) 'if) (if* (rest expr) boxed_state master_return break continue throw))
      ((eq? (first expr) 'while) (call/cc (lambda (k) (while (first_of_rest expr) (first_of_rest_of_rest expr) boxed_state master_return k continue throw))))
      
      ((eq? (first expr) 'begin) (handle_begin expr boxed_state master_return break continue throw))

      ((eq? (first expr) 'try) (handle_try expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'throw) (handle_throw expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'catch) (handle_catch expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'finally) (handle_finally expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'break) (break ()))
      ((eq? (first expr) 'continue) (continue ())))))
     
;Takes an expression, a state, and the continuation return and returns the value of the expression evaluated in the given state.
;The evaluated expressions in M_value use math operators (i.e. + _ * / %) to produce/declare/assign values. The expression may contain assignments.
(define M_value
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((atom? expr) (if (number? expr) expr (if (or (equal? expr 'true) (equal? expr 'false)) (if (equal? expr 'true) #t #f) (get_var_value_box expr boxed_state))))
      ((and (unary? expr) (eq? (first expr) '-)) (* -1 (M_value (first_of_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) '=) (begin
                               (M_state_box expr boxed_state master_return break continue throw)
                               (M_value (first_of_rest_of_rest expr) boxed_state master_return break continue throw)))
      ((is_math_op? expr) ((get_math_op expr)
                           (M_value (first_of_rest expr) boxed_state master_return break continue throw)
                           (M_value (first_of_rest_of_rest expr) boxed_state master_return break continue throw)))
      ((is_bool_op? expr) (M_boolean expr boxed_state master_return break continue throw))
      (else (error "You somehow called M_value on something without a value.")))))

;Takes an expression, a state, and the continuation return and returns the boolean value of the expression evaluated in the given state.
;The evaluated expressions in M_boolean use boolean operations (i.e. >, <, !=, ==, >=, <=, &&, ||, !) to produce/declare/assign values. The expression may contain assignments
(define M_boolean
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((atom? expr) (if (or (equal? expr 'true) (equal? expr 'false)) (if (equal? expr 'true) #t #f) (get_var_value_box expr boxed_state)))
      ((and (unary? expr) (eq? (first expr) '!)) (not (M_boolean (first_of_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) '=) (M_boolean (first_of_rest_of_rest expr) boxed_state master_return break continue throw))
      ((is_bool_op? expr) ((get_bool_op expr) (M_value (first_of_rest expr) boxed_state master_return break continue throw)
                                              (M_value (first_of_rest_of_rest expr) boxed_state master_return break continue throw)))
      (else (error "You somehow called M_boolean on something without a boolean value.")))))

;Checks if an object is an atom. Returns true if so.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;Checks if an expression is unary. Returns true if so.
(define unary?
  (lambda (expr)
    (eq? (rest_of_rest expr) ())))
    
;Creates an empty program state
(define empty_state
  (lambda ()
    '(() ())))

;Creates an empty program state in a box
(define empty_state_box
  (lambda ()
    (box '((() ())))))

;Creates an empty program state in the layers form
(define empty_state2
  (lambda ()
    '((() ()))))

;Adds an empty layer to the top of the current state
(define new_block
  (lambda (state)
    (cons (first (empty_state2)) state)))

;Takes a state in a box and adds an empty layer to the top of the state
(define new_block_box
  (lambda (state)
    (set-box! state (cons (first (empty_state2)) (unbox state)))))

;Takes a boxed state and removes the top layer of said state, if it exists.
(define remove_top_layer
  (lambda (state)
    (cond
      ((null? (unbox state)) (error "Tried to remove a layer from the state, but no such layer exists!"))
      (else (set-box! state (rest (unbox state)))))))

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
    (encapsulate (cons f1 (first l)) (cons f2 (first_of_rest l)))))

;Takes an input list of two lists and returns a list of the first two elements of those lists
(define getfirsts
  (lambda (l)
    (cons (first_of_first l) (cons (first_of_first_of_rest l) ()))))

;Removes the first elements of the sublists of a list. Said list is comprised of two lists.
(define removefirsts
  (lambda (l)
    (encapsulate (rest_of_first l) (rest_of_first_of_rest l))))

;Takes two inputs and makes them the first two values in the lists in an input list of two lists.
(define replacefirsts
 (lambda (f1 f2 l)
   (newfirsts f1 f2 (removefirsts l))))

;Gets the value of a variable possibly stored in a layer. Returns (boolean value), where boolean is true and value is the var's value if the var was present, and boolean is false otherwise
(define get_var_value_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) (cons #f '(())))
      ((eq? var (first_of_first layer)) (if (not (null? (first_of_first_of_rest layer))) (cons #t (cons (first_of_first_of_rest layer) '())) (error "Attempting to use unassigned variable.")))
      (else (get_var_value_layer var (removefirsts layer))))))

;Gets the value of a given varable in a given layer state, or errors if no such variable exists.
(define get_var_value2
  (lambda (var state)
    (cond
      ((null? state) (error "Attempting to use an undeclared variable."))
      ((first (get_var_value_layer var (first state))) (first_of_rest (get_var_value_layer var (first state))))
      (else (get_var_value2 var (rest state))))))

;Gets the value of a variable possibly stored in a layer of a state stored in a box. Returns (boolean value), where boolean is true and value is the var's value if the var was present
;and boolean is false otherwise
(define get_var_value_box
  (lambda (var boxed_state)
    (let ([state (unbox boxed_state)])
     (cond
      ((null? state) (error "Attempting to use an undeclared variable."))
      ((first (get_var_value_layer var (first state))) (first_of_rest (get_var_value_layer var (first state))))
      (else (get_var_value2 var (rest state)))))))

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

;Takes a variable, a list containing a value, a state in a box, and the continuation return value and updates the state in the box to be
;the state where the variable has been declared in the top layer. If it is being declared but not initialized, use value ().
(define handle_declare
  (lambda (var value boxed_state master_return break continue throw)
    (let ([state (unbox boxed_state)])
      (cond
        ((null? value) (set-box! boxed_state (cons (newfirsts var value (first state)) (rest state))))
        (else (set-box! boxed_state (cons (newfirsts var (M_value value boxed_state master_return break continue throw) (first state)) (rest state))))))))

;Takes a variable, a value, and a state and sets the value of that variable in the state to be the given value. If the variable is not in the state an error is thrown.
(define update_state
  (lambda (var value state)
    (cond
      ((null? (first state)) (error "Variable is being assigned before it has been declared."))
      ((equal? var (first_of_first state)) (replacefirsts var value state))
      (else (newfirsts (first_of_first state) (first_of_first_of_rest state) (update_state var value (removefirsts state)))))))

;Takes a variable, a value, and a layer of a state and sets the value of that variable in the state to be the given value if it is present.
;Returns (#t updated_layer) if the value was present, and (#f layer) otherwise.
(define update_layer
  (lambda (var value layer)
    (cond
      ((null? (first layer)) (cons #f (cons layer ())))
      ((eq? var (first_of_first layer)) (cons #t (cons (replacefirsts var value layer) ())))
      (else (let ([result (update_layer var value (removefirsts layer))])
              (cons (first result) (cons (newfirsts (first (getfirsts layer)) (first_of_rest (getfirsts layer)) (first_of_rest result)) ()))))))) ;What have I done? DPT

;Takes a variable and a layer and returns true if the variable is in the layer and false otherwise.
(define in_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) #f)
      ((eq? var (first_of_first layer)) #t)
      (else (in_layer var (removefirsts layer))))))

;Takes a variable, a value, and a state and sets the value of that variable in the state to be the given value. Works on layer states.
(define update_state2
  (lambda (var value state)
    (cond
      ((null? state) (error "Variable is being assigned before it has been declared."))
      ((in_layer var (first state)) (cons (first_of_rest (update_layer var value (first state))) (rest state)))
      (else (cons (first state) (update_state2 var value (rest state)))))))

;Takes a variable, a value, and a state in a box and sets the value of that variable in the state in that box to be the given value. Works on layered states.
(define update_box
  (lambda (var value boxed_state)
    (let ([state (unbox boxed_state)])
      (cond
        ((null? state) (error "Variable is being assigned before it has been declared."))
        ((in_layer var (first state)) (set-box! boxed_state (cons (first_of_rest (update_layer var value (first state))) (rest state))))
        (else (set-box! boxed_state (cons (first state) (update_state2 var value (rest state)))))))))

;Takes a variable, an expression, a state, and the continuation return value and returns the state where the variable is assigned to the value
;of the expression if the variable is declared. Otherwise creates an error.
(define assign
  (lambda (var expr state master_return)
    (update_state var (M_value expr state master_return) (M_state expr state master_return))))

;Takes a variable, an expression, a state in a box, and the continuation return and updates the boxed state to the state where the variable is assigned to the value
;of the expression if the variable is declared. Otherwise creates an error.
(define handle_assign
  (lambda (var expr boxed_state master_return break continue throw)
    (update_box var (M_value expr boxed_state master_return break continue throw) boxed_state)))

;Takes an expression, a state, and the current continuation return value. The method then returns the value of the expression in the state. Once return is called, the program
;terminates, as the final value of the continuation return has been found.
(define return
  (lambda (value master_return)
    (cond
      ((is_boolean? value) (if value (master_return 'true) (master_return 'false)))
      (else (master_return value)))))

;Takes an expression, containing a condition, a boxed state and the M_state contitions.
;If the condition is true in the state, the M_state is called on the first expression after the condition.
;If the condition is false and the second expression after the condition is not null, then M_state is called on the second expression.
(define if*
  (lambda (expr boxed_state master_return break continue throw)
    (letrec ([truth (M_boolean (first expr) boxed_state master_return break continue throw)])
      (cond
        ((eq? truth #t) (M_state_box (first_of_rest expr) boxed_state master_return break continue throw))
        ((and (eq? truth #f) (not (eq? (rest_of_rest expr) ()))) (M_state_box (first_of_rest_of_rest expr) boxed_state master_return break continue throw))))))

;Takes a condition, a loop body, a boxed state, and the M_state conditions.
;If the condition is true in the state, it recursively calls itself on the condition, the loop body, the continuations,
;and the state, after M_state has been called on the loop body.
(define while
  (lambda (condition loop boxed_state master_return break continue throw)
    (letrec ([truth (M_boolean condition boxed_state master_return break continue throw)])
      (cond
        ((eq? truth #t) (begin (call/cc (lambda (k) (M_state_box loop boxed_state master_return break k throw))) (while condition loop boxed_state master_return break continue throw)))))))

;Takes a block expression, a boxed state, master return, break, continue and throw, and processes said block statement in the boxed state.
(define handle_begin
  (lambda (expr boxed_state master_return break continue throw)
    (begin (new_block_box boxed_state) (begin_helper expr boxed_state master_return break continue throw) (remove_top_layer boxed_state))))

;Helper method for handle_begin. Takes a list of expressions, a state with a new layer, master_return, break,
;continue and throw and processes the code block within the new layer of the boxed state.
(define begin_helper
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? (rest expr)) )
      (else
       (M_state_box (first_of_rest expr) boxed_state master_return break continue throw)
       (begin_helper (rest expr) boxed_state master_return break continue throw)))))

;Skeleton code for other helper methods:
(define handle_try
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) ))))

(define handle_catch
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) ))))

(define handle_finally
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) ))))
;end of skeleton code.

;This section is for abstractions of the car/cdr functions. first and rest are already implemented by Pretty Big

;Functionality of caar
(define first_of_first
  (lambda (l)
    (caar l)))

;Functionality of cddr
(define rest_of_rest
  (lambda (l)
    (cddr l)))

;Functionality of cadr
(define first_of_rest
  (lambda (l)
    (cadr l)))

;Functionality of cdar
(define rest_of_first
  (lambda (l)
    (cdar l)))

;Functionality of caaar
(define first_of_first_of_first
  (lambda (l)
    (caaar l)))

;Functionality of caadr
(define first_of_first_of_rest
  (lambda (l)
    (caadr l)))

;Functionality of cadar
(define first_of_rest_of_first
  (lambda (l)
    (cadar l)))

;Functionality of caddr
(define first_of_rest_of_rest
  (lambda (l)
    (caddr l)))

;Functionality of cdaar
(define rest_of_first_of_first
  (lambda (l)
    (cdaar l)))

;Functionality of cdadr
(define rest_of_first_of_rest
  (lambda (l)
    (cdadr l)))

;Functionality of cddar
(define rest_of_rest_of_first
  (lambda (l)
    (cddar l)))

;Functionality of cdddr
(define rest_of_rest_of_rest
  (lambda (l)
    (cdddr l)))