;EECS345 Programming Project Part 2
;Professor Lewicki
;Project Partners: Peyton Turner (dpt14) and Jack La Rue (jvl13)
;4/10/17
;Language used: Pretty Big
(load "functionParser.scm")

(require racket/trace)
(include "HelperFunctions.rkt")

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
      ((null? rest_of_program) (begin
                                 (M_state first_line boxed_state master_return
                                          (lambda (x) (error "Called break outside of a loop"))
                                          (lambda (y) (error "Called continue outside of a loop"))
                                          (lambda (z) (error "Threw an exception outside of a try block")))
                                 (M_state '(funcall main) boxed_state master_return
                                          (lambda (x) (error "Called break outside of a loop"))
                                          (lambda (y) (error "Called continue outside of a loop"))
                                          (lambda (z) (error "Threw an exception outside of a try block")))))
      (else (begin
              (M_state first_line boxed_state master_return
                           (lambda (x) (error "Called break outside of a loop"))
                           (lambda (y) (error "Called continue outside of a loop"))
                           (lambda (z) (error "Threw an exception outside of a try block")))
                   (evaluate (first rest_of_program) (rest rest_of_program) boxed_state master_return))))))

;Takes an expression, a state in a box, and the continuations for return, break, continue, throw, and block handling,
;and updates the state in the box to the state after the input expression has been evaluated.
(define M_state
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((atom? expr) )
      ((null? expr) )
      ;Handles the two possibilities of variable declaration, either assigning an empty list if now value is given, or the given value if a value is given.
      ((eq? (first expr) 'var) (if (null? (rest_of_rest expr))
                                   (handle_declare (first_of_rest expr) '() boxed_state master_return break continue throw)
                                   (handle_declare (first_of_rest expr) (first_of_rest_of_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) 'function) (handle_function_declare (rest expr) boxed_state master_return break continue throw))
      ((eq? (first expr) '=) (handle_assign (first_of_rest expr) (first_of_rest_of_rest expr) boxed_state master_return break continue throw))
      ((eq? (first expr) 'return) (return (M_value (first_of_rest expr) boxed_state master_return break continue throw) master_return))
      ((eq? (first expr) 'if) (if* (rest expr) boxed_state master_return break continue throw))
      ((eq? (first expr) 'while) (call/cc (lambda (k) (while (first_of_rest expr) (first_of_rest_of_rest expr) boxed_state master_return k continue throw))))
      ((eq? (first expr) 'break) (break (remove_top_layer boxed_state)))
      ((eq? (first expr) 'continue) (continue (remove_top_layer boxed_state)))
      ((eq? (first expr) 'begin) (handle_begin expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'try) (handle_try expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'throw) (handle_throw expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'catch) (handle_catch expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'finally) (handle_finally expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'funcall) (handle_function_call (rest expr) boxed_state master_return break continue throw)))))
     
;Takes an expression, a state, and the continuation return and returns the value of the expression evaluated in the given state.
;The evaluated expressions in M_value use math operators (i.e. + _ * / %) to produce/declare/assign values. The expression may contain assignments.
(define M_value
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((is_boolean? expr) expr)
      ((atom? expr) (if (number? expr) expr (if (or (equal? expr 'true) (equal? expr 'false)) (if (equal? expr 'true) #t #f) (get_var_value_box expr boxed_state))))
      ((and (unary? expr) (eq? (first expr) '-)) (* -1 (M_value (first_of_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) '=) (begin
                               (M_state expr boxed_state master_return break continue throw)
                               (M_value (first_of_rest_of_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) 'funcall) (handle_function_call (rest expr) boxed_state master_return break continue throw))
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
      ((is_boolean? expr) expr)
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
    (eq? (rest_of_rest expr) '())))

;Creates an empty program state in a box
(define empty_state_box
  (lambda ()
    (box '((() ())))))

;Creates an empty program state in the layers form
(define empty_state
  (lambda ()
    '((() ()))))

;Adds an empty layer to the top of the current state
(define new_block
  (lambda (state)
    (cons (first (empty_state)) state)))

;Takes a state in a box and adds an empty layer to the top of the state
(define new_block_box
  (lambda (state) (set-box! state (cons (first (empty_state)) (unbox state)))))

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

;Takes three inputs: two values and a list. Adds new first elements to a list of two lists.
(define newfirsts
  (lambda (f1 f2 l)
    (list (cons f1 (first l)) (cons f2 (first_of_rest l)))))

;Takes an input list of two lists and returns a list of the first two elements of those lists
(define getfirsts
  (lambda (l)
    (cons (first_of_first l) (cons (first_of_first_of_rest l) '()))))

;Removes the first elements of the sublists of a list. Said list is comprised of two lists.
(define removefirsts
  (lambda (l)
    (list (rest_of_first l) (rest_of_first_of_rest l))))

;Takes two inputs and makes them the first two values in the lists in an input list of two lists.
(define replacefirsts
 (lambda (f1 f2 l)
   (newfirsts f1 f2 (removefirsts l))))

;Gets the value of a variable possibly stored in a layer of a state stored in a box. Returns (boolean value), where boolean is true and value is the var's value if the var was present
;and boolean is false otherwise.
(define get_var_value_box
  (lambda (var boxed_state)
    (let ([state (unbox boxed_state)])
     (cond
      ((null? state) (error "Attempting to use an undeclared variable or function."))
      ((first (get_var_value_layer var (first state))) (first_of_rest (get_var_value_layer var (first state))))
      (else (get_var_value var (rest state)))))))

;Helper of get_var_value_box
;Gets the value of a given varable in a given layer state, or errors if no such variable exists.
(define get_var_value
  (lambda (var state)
    (cond
      ((null? state) (error "Attempting to use an undeclared variable or function."))
      ((first (get_var_value_layer var (first state))) (first_of_rest (get_var_value_layer var (first state))))
      (else (get_var_value var (rest state))))))

;Helper of get_var_value_box
;Gets the value of a variable possibly stored in a layer. Returns (boolean value), where boolean is true and value is the var's value if the var was present, and boolean is false otherwise
(define get_var_value_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) (list #f '()))
      ((eq? var (first_of_first layer)) (if (not (null? (unbox (first_of_first_of_rest layer)))) (list #t (unbox (first_of_first_of_rest layer))) (error "Attempting to use unassigned variable.")))
      (else (get_var_value_layer var (removefirsts layer))))))

;Takes a variable, a list containing a value, a state in a box, and the continuation return value and updates the state in the box to be
;the state where the variable has been declared in the top layer in a box. If it is being declared but not initialized, use value ().
(define handle_declare
  (lambda (var value boxed_state master_return break continue throw)
    (let ([state (unbox boxed_state)])
      (cond
        ((null? value) (set-box! boxed_state (cons (newfirsts var (box value) (first state)) (rest state))))
        (else (set-box! boxed_state (cons (newfirsts var (box (M_value value boxed_state master_return break continue throw)) (first state)) (rest state))))))))

;Takes a function expression (name (parameters) (body)) and binds it in the state. The binding of a function looks much like the binding of a variable, but instead of the
;name being bound to a value in a box, the bound box contains (1) a list of parameters, (2) the state at the time of binding (which, since it is composed of boxes, will be updated
;as the values of variables in the state are updated, (3) and the body of the function.
(define handle_function_declare
  (lambda (func boxed_state master_return break continue throw)
    (let ([state (unbox boxed_state)] [name (get_function_name func)] [parameters (get_function_params func)] [body (get_function_body func)])
      (set-box! boxed_state (cons (newfirsts name (box (list parameters state body)) (first state)) (rest state))))))

;Gets the name of an input function expression.
(define get_function_name
  (lambda (func)
    (first func)))

;Gets the parameter list of an input function expression.
(define get_function_params
  (lambda (func)
    (first_of_rest func)))

;Gets the body of an input function expression.
(define get_function_body
  (lambda (func)
    (first_of_rest_of_rest func)))

;Takes a variable, an expression, a state in a box, and the continuation return and updates the boxed state to the state where the variable is assigned to the value
;of the expression if the variable is declared. Otherwise creates an error.
(define handle_assign
  (lambda (var expr boxed_state master_return break continue throw)
    (update_box var (M_value expr boxed_state master_return break continue throw) boxed_state)))

;Takes a variable, a value, and a state in a box and sets the value of that variable in the state in that box to be the given value. Works on layered states.
(define update_box
  (lambda (var value boxed_state)
    (let ([state (unbox boxed_state)])
      (cond
        ((null? state) (error "Variable is being assigned before it has been declared."))
        (else (update_state var value state))))))

;Takes a variable, a value, and a state and sets the value of that variable in the state to be the given value. Works on layer states.
(define update_state
  (lambda (var value state)
    (cond
      ((null? state) (error "Variable is being assigned before it has been declared."))
      ((in_layer var (first state)) (set-box! (get_box_from_layer var (first state)) value))
      (else (update_state var value (rest state))))))

;Takes a variable and a layer and returns true if the variable is in the layer and false otherwise.
(define in_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) #f)
      ((eq? var (first_of_first layer)) #t)
      (else (in_layer var (removefirsts layer))))))

;Takes a variable and a layer and returns the box bound to the variable name, or errors if no such box exists.
(define get_box_from_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) (error "Variable is being assigned before it has been declared."))
      ((eq? var (first_of_first layer)) (first_of_first_of_rest layer))
      (else (get_box_from_layer var (removefirsts layer))))))

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
        ((eq? truth #t) (M_state (first_of_rest expr) boxed_state master_return break continue throw))
        ((and (eq? truth #f) (not (eq? (rest_of_rest expr) '()))) (M_state (first_of_rest_of_rest expr) boxed_state master_return break continue throw))))))

;Takes a condition, a loop body, a boxed state, and the M_state conditions.
;If the condition is true in the state, it recursively calls itself on the condition, the loop body, the continuations,
;and the state, after M_state has been called on the loop body.
(define while
  (lambda (condition loop boxed_state master_return break continue throw)
    (letrec ([truth (M_boolean condition boxed_state master_return break continue throw)])
      (cond
        ((eq? truth #t) (begin (call/cc (lambda (k) (M_state loop boxed_state master_return break k throw))) (while condition loop boxed_state master_return break continue throw)))))))

;Takes a block expression, a boxed state, master return, break, continue and throw, and processes said block statement in the boxed state.
(define handle_begin
  (lambda (expr boxed_state master_return break continue throw)
    (begin (new_block_box boxed_state) (begin_helper expr boxed_state master_return break continue throw) (remove_top_layer boxed_state))))

;Helper method for handle_begin. Takes a list of expressions, a state with a new layer, master_return, break,
;continue and throw and processes the code block within the new layer of the boxed state.
(define begin_helper
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? (rest expr)))
      (else
        (M_state (first_of_rest expr) boxed_state master_return break continue throw)
        (begin_helper (rest expr) boxed_state master_return break continue throw)))))

;Takes an expression in the form of a try statement and the M_state continuations and calls M_state on the body of the try block.
;Catches exceptions raised and stores them as a variable, then calls M_state on the catch and finally blocks.
;If no exception is raised, calls M_state on the finally block.
(define handle_try
  (lambda (expr boxed_state master_return break continue throw)
    (letrec ([result (call/cc (lambda (k) (try_helper (first_of_rest expr) boxed_state master_return break continue k)))])
      (cond
        ((eq? result (void)) (M_state (first (rest_of_rest_of_rest expr)) boxed_state master_return break continue throw))
        (else
         ;I have made the implementation decision that exceptions are declared as variables.
         ;Giving an exception a name that a variable already has will produce an error.
         ;In principle a user could access an exception outside of the catch block in which it was created.
         (handle_declare (first_of_first_of_rest (first_of_rest_of_rest expr)) result boxed_state master_return break continue throw)
         (M_state (first_of_rest_of_rest expr) boxed_state master_return break continue throw)
         (M_state (first (rest_of_rest_of_rest expr)) boxed_state master_return break continue throw))))))

;Recursively calls M_state on the first entry of the input expression until it has exhausted the list
(define try_helper
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) (void))
      (else
       (M_state (first expr) boxed_state master_return break continue throw)
       (try_helper (rest expr) boxed_state master_return break continue throw)))))

;Takes an expression in the form of a catch statement and the M_state continuations and calls M_state on the body of the catch block.
(define handle_catch
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) (void))
      (else (catch_helper (first_of_rest_of_rest expr) boxed_state master_return break continue throw)))))

;Recursively calls M_state on the first entry of the input expression until it has exhausted the list
(define catch_helper
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) (void))
      (else
       (M_state (first expr) boxed_state master_return break continue throw)
       (catch_helper (rest expr) boxed_state master_return break continue throw)))))

;Takes an expression in the form of a finally statement and the M_state continuations and calls M_state on the body of the finally block.
(define handle_finally
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) )
      (else (finally_helper (first_of_rest expr) boxed_state master_return break continue throw)))))

;Recursively calls M_state on the first entry of the input expression until it has exhausted the list
(define finally_helper
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((null? expr) )
      (else
       (M_state (first expr) boxed_state master_return break continue throw)
       (finally_helper (rest expr) boxed_state master_return break continue throw)))))

;Takes an expression and the M_state continuations and passes the value of the expression to the throw continuation
(define handle_throw
  (lambda (expr boxed_state master_return break continue throw)
    (throw (M_value (first_of_rest expr) boxed_state master_return break continue throw))))

;FUNCALL! NOT THAT THERE'S ANYTHING FUN ABOUT!
;Takes a function of the form (name parameters) and returns the result of evaluating it.
(define handle_function_call
  (lambda (func boxed_state master_return break continue throw)
    (letrec (
             [name (first func)]
             [parameter_values (M_value_list (rest func) boxed_state master_return break continue throw)]
             [function_info (get_var_value_box name boxed_state)]
             [parameter_names (first function_info)]
             [scope (first_of_rest function_info)]
             [body (first_of_rest_of_rest function_info)])
      (if (atom? function_info) (error "Attempted to reference a variable as a function.")
          ;Main gets called with the master return, other functions get called with the call/cc return.
          (if (eq? name 'main)
              (evaluate_function name (first body) (rest body) (bind_parameters_in_scope parameter_names parameter_values scope) master_return throw)
           (call/cc
            (lambda (return_from_function)
              (evaluate_function name (first body) (rest body) (bind_parameters_in_scope parameter_names parameter_values scope) return_from_function throw))))))))

;Takes a list of variables/values and the inputs to M_value and returns a list of M_value called on each of the elements of the input list.
(define M_value_list
  (lambda (inputs boxed_state master_return break continue throw) 
    (cond
      ((null? inputs) '())
      (else (cons (M_value (first inputs) boxed_state master_return break continue throw) (M_value_list (rest inputs) boxed_state master_return break continue throw))))))

;Takes a list of parameter names, a list of parameter values, and the scope of a function. If the parameter lists are the same name, put each parameter value into a box,
;and add a new layer to the scope containing each parameter name bound to the box containing its respective value. Otherwise error.
(define bind_parameters_in_scope
  (lambda (parameter_names parameter_values scope)
    (if (eq? (length parameter_names) (length parameter_values))
        (box (cons (list parameter_names (boxify parameter_values)) scope))
        (error "Incorrect number of parameters in function."))))

;Takes a list and returns a same-sized list of the list entries, but in boxes
(define boxify
  (lambda (l)
    (cond
      ((null? l) ())
      (else (cons (box (first l)) (boxify (rest l)))))))

;Takes a function, given by a name, a first line, and the rest of the function, and evaluates it in the input state. Returns to functino_return, throws to throw.
(define evaluate_function
  (lambda (name first_line rest_of_function boxed_state function_return throw)
    (cond
      ((and (null? first_line) (eq? name 'main)) (error "Main Function Completed Without A Return Statement"))
      ((null? rest_of_function) (M_state first_line boxed_state function_return
                                 (lambda (x) (error "Called break outside of a loop"))
                                 (lambda (y) (error "Called continue outside of a loop"))
                                 (lambda (z) throw)))
      (else (begin
              (M_state first_line boxed_state function_return
                           (lambda (x) (error "Called break outside of a loop"))
                           (lambda (y) (error "Called continue outside of a loop"))
                           (lambda (z) throw))
                   (evaluate_function name (first rest_of_function) (rest rest_of_function) boxed_state function_return throw))))))
    

