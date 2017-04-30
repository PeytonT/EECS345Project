;Professor Lewicki
;Project Partners: Peyton Turner (dpt14), Jack La Rue (jvl13), and Jessie Adkins (jsa70) 
;4/12/17
;Language used: Pretty Big
(require racket/trace)
(include "HelperFunctions.rkt")
(include "Project3Functions.rkt")
(load "classParser.scm")



;General Project Notes:
;Empty states shall have the format: ((() ()))

;Takes a file from the local directory and a class name, and, using classParser creates a class declaration table (CDT).
;Once the CDT is created, the main function of the named class is run.
;call/cc will automatically take the continuation return value and output it into the interactions pane.
(define interpret
  (lambda (filename classname)
    (letrec ([CDT (createAndFillCDT (parser filename))] [output (call/cc
                   (lambda (return_from_interpret)
                     (evaluate_main CDT classname return_from_interpret)))])
      (cond
        ((eq? output #t) 'true)
        ((eq? output #f) 'false)
        (else output)))))

(define evaluate_main
  (lambda (CDT classname master_return)
    (letrec ([main (get_main_from_CDT CDT classname)] [boxed_state (first_rest main)] [body (first_rest_rest main)])
      (cond
        ((null? body) (error "Body of main is empty."))
        ((null? (rest body)) (evaluate_main_helper (first body) () CDT boxed_state master_return))
        (else (evaluate_main_helper (first body) (rest body) CDT boxed_state master_return))))))

(define get_main_from_CDT
  (lambda (CDT classname)
    (get_main_helper (unbox CDT) classname)))

(define get_main_helper
  (lambda (unboxed_CDT classname)
    (cond
      ((null? (first unboxed_CDT)) (error "The specified class was never declared."))
      ((eq? classname (first_first unboxed_CDT)) (get_var_value_box 'main (first_rest (first_first_rest unboxed_CDT))))
      (else (get_main_helper (removefirsts unboxed_CDT) classname)))))

(define evaluate_main_helper
  (lambda (first_line rest_of_program CDT boxed_state master_return)
    (cond
      ((null? first_line) (error "No return from main."))
      ((null? rest_of_program) (begin
                                 (M_state_CDT first_line CDT boxed_state master_return
                                          (lambda (x) (error "Called break outside of a loop"))
                                          (lambda (y) (error "Called continue outside of a loop"))
                                          (lambda (z) (error "Threw an exception outside of a try block")))
                                (evaluate_main_helper () () CDT boxed_state master_return)))
      (else (begin
              (M_state_CDT first_line CDT boxed_state master_return
                           (lambda (x) (error "Called break outside of a loop"))
                           (lambda (y) (error "Called continue outside of a loop"))
                           (lambda (z) (error "Threw an exception outside of a try block")))
                   (evaluate_main_helper (first rest_of_program) (rest rest_of_program) CDT boxed_state master_return))))))
     
;Takes a state in a box and returns a box which is a copy of the input box.
(define copyState
  (lambda (state)
    (box (unbox state))))

;The CDT is the class declaration table, it stores each of the classes defined in the file in the format ((names) (class_formats)),
;where a class format is of the form (parent_name (box containing the associated state))
(define createCDT
  (lambda ()
    (box (list (list 'Object) (list (list (void) (empty_state_box)))))))

;Extracts the name of a class from the parse.
(define getName
  (lambda (parse)
    (first_rest parse)))

;Extracts the name of the parent of a class from the parse. If it has no parent, its parent is Object.
(define parentsName 
  (lambda (parse)
    (if (eq? () (first_rest_rest parse)) 
        'Object 
        (first_rest (first_rest_rest parse))))) 

;Unusually, getForm takes an unboxed CDT because it recursively calls itself.
(define getForm  
  (lambda (name unboxed_CDT)
    (cond
      ((null? (first unboxed_CDT)) (error "A class was extended that has not been declared."))
      ((eq? name (first_first unboxed_CDT)) (first_first_rest unboxed_CDT))
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
    (set-box! boxed_state (list (replace_contexts boxed_state (first (unbox boxed_state)))))))

(define replace_contexts 
  (lambda (boxed_state current_state)
    (cond
      ((null? (first current_state)) '(()()))
      (else (letrec ([top_L (first_first current_state)] [top_R (unbox (first_first_rest current_state))])
              (if (and (not (atom? top_R)) (eq? (length top_R) 3))
                  (newfirsts top_L (box (list (first top_R) boxed_state (first_rest_rest top_R))) (replace_contexts boxed_state (removefirsts current_state)))
                  (newfirsts top_L (box top_R) (replace_contexts boxed_state (removefirsts current_state)))))))))

;Extracts the state of a class from its form.
(define state_from_form 
  (lambda (form)
    (copyState (first_rest form))))

;Takes the name of a classes parent, the form of the classes parent, and the parse of the class and returns the form of the class.
(define declareClass
  (lambda (parent_name parent_form parse)
    (letrec
        ([body (first (rest_rest_rest parse))]
         [state (copyState (first_rest parent_form))]
         [temp1_does_nothing (evaluate_class (first body) (rest body) state (lambda (x) (error "Wat")))]
         [temp2_does_nothing (contextualize state)])
      (list parent_name state))))

;Takes a CDT and a parse list and fills the CDT with the classes declared in the parse list.
(define declareAllClasses
  (lambda (CDT parse_list)
    (cond
      ((null? parse_list) ) ;we want it to return nothing, just fill the CDT 
      (else
       (letrec (
                [parse (first parse_list)]
                [name (getName parse)]
                [parentName (parentsName parse)]
                [parentForm (getForm parentName (unbox CDT))]
                [class (declareClass parentName parentForm parse)])
                (set-box! CDT (newfirsts name class (unbox CDT))) (declareAllClasses CDT (rest parse_list)))))))

;Takes a parse lists and returns a CDT in which that parse list has been declared.
(define createAndFillCDT
  (lambda (parse_list)
    (let ([CDT (createCDT)]) (begin (declareAllClasses CDT parse_list) CDT))))

;Takes an expression, a CDT, a state in a box, and the continuations for return, break, continue, throw, and block handling,
;and updates the state in the box to the state after the input expression has been evaluated.
(define M_state_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((atom? expr) )
      ((null? expr) )
      ;Handles the two possibilities of variable declaration, either assigning an empty list if now value is given, or the given value if a value is given.
      ((eq? (first expr) 'var) (if (null? (rest_rest expr))
                                   (handle_declare_CDT (first_rest expr) '() CDT boxed_state master_return break continue throw)
                                   (handle_declare_CDT (first_rest expr) (first_rest_rest expr) CDT boxed_state master_return break continue throw)))
      ((eq? (first expr) 'function) (handle_function_declare_CDT (rest expr) CDT boxed_state master_return break continue throw))
      ((eq? (first expr) '=) (handle_assign_CDT (first_rest expr) (first_rest_rest expr) CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'return) (return (M_value_CDT (first_rest expr) CDT boxed_state master_return break continue throw) master_return))
      ((eq? (first expr) 'if) (if_CDT (rest expr) CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'while) (call/cc
                                  (lambda (k)
                                    (while_CDT (first_rest expr) (first_rest_rest expr) CDT boxed_state master_return k continue throw))))
      ((eq? (first expr) 'break) (break (remove_top_layer boxed_state)))
      ((eq? (first expr) 'continue) (continue (remove_top_layer boxed_state)))
      ((eq? (first expr) 'begin) (handle_begin_CDT expr CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'try) (handle_try_CDT expr CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'throw) (handle_throw_CDT expr CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'catch) (handle_catch_CDT expr CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'finally) (handle_finally_CDT expr CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'funcall) (handle_function_call_CDT (rest expr) CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'static-function) (handle_function_declare_CDT (rest expr) CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'new) (M_value_CDT expr CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'dot) (M_value_CDT expr CDT boxed_state master_return break continue throw)))))

;Takes an expression, a CDT, a state, and the continuation return and returns the value of the expression evaluated in the given state.
;The evaluated expressions in M_value use math operators (i.e. + _ * / %) to produce/declare/assign values. The expression may contain assignments.
(define M_value_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((is_boolean? expr) expr)
      ((atom? expr) (if (number? expr)
                        expr
                        (if (or (equal? expr 'true) (equal? expr 'false))
                            (if (equal? expr 'true)
                                #t
                                #f)
                            (get_var_value_box expr boxed_state))))
      ((and (unary? expr) (eq? (first expr) '-)) (* -1 (M_value (first_rest expr) boxed_state master_return break continue throw)))
      ;Calling M_state and M_value here is likely causing some sort of horrific problem with throws, since the throw should happen in both, but will expend the continuation on the first.
      ((eq? (first expr) '=) (begin
                               (M_state expr boxed_state master_return break continue throw)
                               (M_value (first_rest_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) 'funcall) (handle_function_call_CDT (rest expr) CDT boxed_state master_return break continue throw))
      ((is_math_op? expr) ((get_math_op expr)
                           (M_value_CDT (first_rest expr) CDT boxed_state master_return break continue throw)
                           (M_value_CDT (first_rest_rest expr) CDT boxed_state master_return break continue throw)))
      ((is_bool_op? expr) (M_boolean expr boxed_state master_return break continue throw))
      ((eq? (first expr) 'new) (handle_new_class_instance (first_rest expr) CDT))
      ((eq? (first expr) 'dot) (handle_dot (get_class_from_var_or_expr (first_rest expr) CDT boxed_state master_return break continue throw)
                                           (first_rest_rest expr) CDT boxed_state master_return break continue throw))
      ((eq? (first expr) 'funcall) ((handle_function_call_CDT (rest expr) CDT boxed_state master_return break continue throw)))
      (else (error "You somehow called M_value_CDT on something without a value.")))))

;Takes an expression, a state, and the continuation return and returns the boolean value of the expression evaluated in the given state.
;The evaluated expressions in M_boolean use boolean operations (i.e. >, <, !=, ==, >=, <=, &&, ||, !) to produce/declare/assign values.
;The expression may contain assignments
(define M_boolean
  (lambda (expr boxed_state master_return break continue throw)
    (cond
      ((is_boolean? expr) expr)
      ((atom? expr) (if (or (equal? expr 'true) (equal? expr 'false))
                        (if (equal? expr 'true)
                            #t
                            #f)
                        (get_var_value_box expr boxed_state)))
      ((and (unary? expr) (eq? (first expr) '!)) (not (M_boolean (first_rest expr) boxed_state master_return break continue throw)))
      ((eq? (first expr) '=) (M_boolean (first_rest_rest expr) boxed_state master_return break continue throw))
      ((is_bool_op? expr) ((get_bool_op expr) (M_value (first_rest expr) boxed_state master_return break continue throw)
                                              (M_value (first_rest_rest expr) boxed_state master_return break continue throw)))
      (else (error "You somehow called M_boolean on something without a boolean value.")))))

;Takes a list of variables/values and the inputs to M_value and returns a list of M_value called on each of the elements of the input list.
(define M_value_list_CDT
  (lambda (inputs CDT boxed_state master_return break continue throw) 
    (cond
      ((null? inputs) '())
      (else (cons (M_value_CDT (first inputs) CDT boxed_state master_return break continue throw) (M_value_list_CDT (rest inputs) CDT boxed_state master_return break continue throw))))))


;-----------------------------------------------------------------------------------------------------------------------------------------
;Declare
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes a variable, a list containing a value, a state in a box, and the continuation return value and updates the state in the box to be
;the state where the variable has been declared in the top layer in a box. If it is being declared but not initialized, use value ().
(define handle_declare_CDT
  (lambda (var value CDT boxed_state master_return break continue throw)
    (let ([state (unbox boxed_state)])
      (cond
        ((null? value) (set-box! boxed_state (cons (newfirsts var (box value) (first state)) (rest state))))
        (else (set-box! boxed_state (cons (newfirsts var (box (M_value_CDT value CDT boxed_state master_return break continue throw)) (first state)) (rest state))))))))

;Takes a function expression (name (parameters) (body)) and binds it in the state.
;The binding of a function looks much like the binding of a variable,
;but instead of the name being bound to a value in a box, the bound box contains (1) a list of parameters,
;(2) the state at the time of binding (which, since it is composed of boxes, will be updated
;as the values of variables in the state are updated, (3) and the body of the function.
(define handle_function_declare_CDT
  (lambda (func CDT boxed_state master_return break continue throw)
    (let ([state (unbox boxed_state)] [name (get_function_name func)] [parameters (get_function_params func)] [body (get_function_body func)])
      (set-box! boxed_state (cons (newfirsts name (box (list parameters state body)) (first state)) (rest state))))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Assign
;-----------------------------------------------------------------------------------------------------------------------------------------
;Takes a variable, an expression, a state in a box, and the continuation returns and
;updates the boxed state to the state where the variable is assigned to the value
;of the expression if the variable is declared. Otherwise creates an error.
(define handle_assign_CDT
  (lambda (var expr CDT boxed_state master_return break continue throw)
    (update_box var (M_value_CDT expr CDT boxed_state master_return break continue throw) boxed_state)))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Return, If, and While
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes an expression, a state, and the current continuation return value.
;The method then returns the value of the expression in the state. Once return is called, the program
;terminates, as the final value of the continuation return has been found.
(define return
  (lambda (value master_return)
    (master_return value)))

;Takes an expression, containing a condition, a boxed state and the M_state contitions.
;If the condition is true in the state, the M_state is called on the first expression after the condition.
;If the condition is false and the second expression after the condition is not null,
;then M_state is called on the second expression.
(define if_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (letrec ([truth (M_boolean (first expr) boxed_state master_return break continue throw)])
      (cond
        ((eq? truth #t) (M_state_CDT (first_rest expr) CDT boxed_state master_return break continue throw))
        ((and (eq? truth #f) (not (eq? (rest_rest expr) '()))) (M_state_CDT (first_rest_rest expr) CDT boxed_state master_return break continue throw))))))

;Takes a condition, a loop body, a boxed state, and the M_state conditions.
;If the condition is true in the state, it recursively calls itself on the condition, the loop body, the continuations,
;and the state, after M_state has been called on the loop body.
(define while_CDT
  (lambda (condition loop CDT boxed_state master_return break continue throw)
    (letrec ([truth (M_boolean condition boxed_state master_return break continue throw)])
      (cond
        ((eq? truth #t) (begin (call/cc (lambda (k) (M_state_CDT loop CDT boxed_state master_return break k throw)))
                               (while_CDT condition loop CDT boxed_state master_return break continue throw)))))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Try
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes an expression in the form of a try statement and the M_state continuations and calls M_state on the body of the try block.
;Catches exceptions raised and stores them as a variable, then calls M_state on the catch and finally blocks.
;If no exception is raised, calls M_state on the finally block.
(define handle_try_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (letrec ([result (call/cc (lambda (k) (try_helper_CDT (first_rest expr) CDT boxed_state master_return break continue k)))])
      (cond
        ((eq? result (void)) (M_state_CDT (first (rest_rest_rest expr)) CDT boxed_state master_return break continue throw))
        (else
         ;I have made the implementation decision that exceptions are declared as variables.
         ;Giving an exception a name that a variable already has will produce an error.
         ;In principle a user could access an exception outside of the catch block in which it was created.
         (handle_declare_CDT (first_first_rest (first_rest_rest expr)) result CDT boxed_state master_return break continue throw)
         (M_state_CDT (first_rest_rest expr) CDT boxed_state master_return break continue throw)
         (M_state_CDT (first (rest_rest_rest expr)) CDT boxed_state master_return break continue throw))))))

;Recursively calls M_state on the first entry of the input expression until it has exhausted the list
(define try_helper_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((null? expr) (void))
      (else
       (M_state_CDT (first expr) CDT boxed_state master_return break continue throw)
       (try_helper_CDT (rest expr) CDT boxed_state master_return break continue throw)))))


;-----------------------------------------------------------------------------------------------------------------------------------------
;Catch
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes an expression in the form of a catch statement and the M_state continuations and calls M_state on the body of the catch block.
(define handle_catch_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((null? expr) (void))
      (else (catch_helper_CDT (first_rest_rest expr) CDT boxed_state master_return break continue throw)))))

;Recursively calls M_state on the first entry of the input expression until it has exhausted the list
(define catch_helper_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((null? expr) (void))
      (else
       (M_state_CDT (first expr) CDT boxed_state master_return break continue throw)
       (catch_helper_CDT (rest expr) CDT boxed_state master_return break continue throw)))))


;-----------------------------------------------------------------------------------------------------------------------------------------
;Finally
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes an expression in the form of a finally statement and the M_state continuations and calls M_state on the body of the finally block.
(define handle_finally_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((null? expr) )
      (else (finally_helper_CDT (first_rest expr) CDT boxed_state master_return break continue throw)))))

;Recursively calls M_state on the first entry of the input expression until it has exhausted the list
(define finally_helper_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((null? expr) )
      (else
       (M_state_CDT (first expr) CDT boxed_state master_return break continue throw)
       (finally_helper_CDT (rest expr) CDT boxed_state master_return break continue throw)))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Throw
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes an expression and the M_state continuations and passes the value of the expression to the throw continuation
(define handle_throw_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (throw (M_value_CDT (first_rest expr) CDT boxed_state master_return break continue throw))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Functions
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes a function of the form (name/expr parameters) and returns the result of evaluating it.
(define handle_function_call_CDT
  (lambda (func CDT boxed_state master_return break continue throw)
    (if
     (list? (first func))
      (letrec (
               [name 'not_main]
               [parameter_values (M_value_list_CDT (rest func) CDT boxed_state master_return break continue throw)]
               [function_info (M_value_CDT (first func) CDT boxed_state master_return break continue throw)]
               [parameter_names (first function_info)]
               [scope (first_rest function_info)]
               [body (first_rest_rest function_info)])
        (if (atom? function_info)
            (error "Attempted to reference a variable as a function.")
            ;Main gets called with the master return, other functions get called with the call/cc return.
            (if (eq? name 'main)
                (evaluate_function_CDT name (first body) (rest body) CDT
                                       (bind_parameters_in_scope parameter_names parameter_values scope) master_return throw)
                (call/cc
                 (lambda (return_from_function)
                   (evaluate_function_CDT name (first body) (rest body) CDT
                                          (bind_parameters_in_scope parameter_names parameter_values scope) return_from_function throw))))))
      (letrec (
               [name (first func)]
               [parameter_values (M_value_list_CDT (rest func) CDT boxed_state master_return break continue throw)]
               [function_info (get_var_value_box name boxed_state)]
               [parameter_names (first function_info)]
               [scope (first_rest function_info)]
               [body (first_rest_rest function_info)])
        (if (atom? function_info)
            (error "Attempted to reference a variable as a function.")
            ;Main gets called with the master return, other functions get called with the call/cc return.
            (if (eq? name 'main)
                (evaluate_function_CDT name (first body) (rest body) CDT
                                       (bind_parameters_in_scope parameter_names parameter_values scope) master_return throw)
                (call/cc
                 (lambda (return_from_function)
                   (evaluate_function_CDT name (first body) (rest body) CDT
                                          (bind_parameters_in_scope parameter_names parameter_values scope) return_from_function throw)))))))))

;Takes a list of parameter names, a list of parameter values, and the scope of a function.
;If the parameter lists are the same length, put each parameter value into a box,
;and add a new layer to the scope containing each parameter name bound to the box containing its respective value.
;Otherwise error.
(define bind_parameters_in_scope 
  (lambda (parameter_names parameter_values scope)
    (if (eq? (length parameter_names) (length parameter_values))
        (box (cons (list parameter_names (boxify parameter_values)) (unbox scope)))
        (error "Incorrect number of parameters in function."))))

;Takes a function, given by a name, a first line, and the rest of the function, and evaluates it in the input state.
;Returns to function_return, throws to throw.
(define evaluate_function_CDT
  (lambda (name first_line rest_of_function CDT boxed_state function_return throw)
    (cond
      ((and (null? first_line) (eq? name 'main)) (error "Main Function Completed Without A Return Statement"))
      ((null? rest_of_function) (M_state_CDT first_line CDT boxed_state function_return
                                 (lambda (x) (error "Called break outside of a loop"))
                                 (lambda (y) (error "Called continue outside of a loop"))
                                 throw))
      (else (begin
              (M_state_CDT first_line CDT boxed_state function_return
                           (lambda (x) (error "Called break outside of a loop"))
                           (lambda (y) (error "Called continue outside of a loop"))
                           throw)
                   (evaluate_function_CDT name (first rest_of_function) (rest rest_of_function) CDT boxed_state function_return throw))))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Boxes and States
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes a list and returns a same-sized list of the list entries, but in boxes
(define boxify
  (lambda (l)
    (cond
      ((null? l) ())
      (else (cons (box (first l)) (boxify (rest l)))))))

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

;-----------------------------------------------------------------------------------------------------------------------------------------
;Var Value
;-----------------------------------------------------------------------------------------------------------------------------------------

;Gets the value of a variable possibly stored in a layer of a state stored in a box. Returns (boolean value),
;where boolean is true and value is the var's value if the var was present and boolean is false otherwise.
(define get_var_value_box
  (lambda (var boxed_state)
    (let ([state (unbox boxed_state)])
     (cond
      ((null? state) (error "Attempting to use a variable or function not declared in the current scope."))
      ((first (get_var_value_layer var (first state))) (first_rest (get_var_value_layer var (first state))))
      (else (get_var_value var (rest state)))))))

;Helper of get_var_value_box
;Gets the value of a given varable in a given layer state, or errors if no such variable exists.
(define get_var_value
  (lambda (var state)
    (cond
      ((null? state) (begin (write var) (error "Attempting to use a variable or function not declared in the current scope.")))
      ((first (get_var_value_layer var (first state))) (first_rest (get_var_value_layer var (first state))))
      (else (get_var_value var (rest state))))))

;Helper of get_var_value_box
;Gets the value of a variable possibly stored in a layer. Returns (boolean value),
;where boolean is true and value is the var's value if the var was present, and boolean is false otherwise
(define get_var_value_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) (list #f '()))
      ((eq? var (first_first layer)) (if (not (null? (unbox (first_first_rest layer))))
                                         (list #t (unbox (first_first_rest layer)))
                                         (error "Attempting to use unassigned variable.")))
      (else (get_var_value_layer var (removefirsts layer))))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Layers
;-----------------------------------------------------------------------------------------------------------------------------------------
;Takes a variable and a layer and returns true if the variable is in the layer and false otherwise.
(define in_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) #f)
      ((eq? var (first_first layer)) #t)
      (else (in_layer var (removefirsts layer))))))

;Takes a variable and a layer and returns the box bound to the variable name, or errors if no such box exists.
(define get_box_from_layer
  (lambda (var layer)
    (cond
      ((null? (first layer)) (error "A variable or function is being referenced that does not exist in the current scope."))
      ((eq? var (first_first layer)) (first_first_rest layer))
      (else (get_box_from_layer var (removefirsts layer))))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Update
;-----------------------------------------------------------------------------------------------------------------------------------------
;Takes a variable, a value, and a state in a box and sets the value of that variable in the state in that box to be the given value.
;Works on layered states.
(define update_box
  (lambda (var value boxed_state)
    (let ([state (unbox boxed_state)])
      (cond
        ((null? state) (error "A variable or function is being referenced that does not exist in the current scope."))
        (else (update_state var value state))))))

;Takes a variable, a value, and a state and sets the value of that variable in the state to be the given value.
;Works on layer states.
(define update_state
  (lambda (var value state)
    (cond
      ((null? state) (error "A variable or function is being referenced that does not exist in the current scope."))
      ((in_layer var (first state)) (set-box! (get_box_from_layer var (first state)) value))
      (else (update_state var value (rest state))))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Begin
;-----------------------------------------------------------------------------------------------------------------------------------------

;Takes a block expression, a boxed state, master return, break, continue and throw, and processes said block statement in the boxed state.
(define handle_begin_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (begin (new_block_box boxed_state) (begin_helper_CDT expr CDT boxed_state master_return break continue throw) (remove_top_layer boxed_state))))

;Helper method for handle_begin. Takes a list of expressions, a state with a new layer, master_return, break,
;continue and throw and processes the code block within the new layer of the boxed state.
(define begin_helper_CDT
  (lambda (expr CDT boxed_state master_return break continue throw)
    (cond
      ((null? (rest expr)))
      (else
        (M_state_CDT (first_rest expr) CDT boxed_state master_return break continue throw)
        (begin_helper_CDT (rest expr) CDT boxed_state master_return break continue throw)))))

;-----------------------------------------------------------------------------------------------------------------------------------------
;Classes
;-----------------------------------------------------------------------------------------------------------------------------------------
;Takes a class name and a CDT and returns an instance of the specified class.
;An instance of a class has the form (classname parentname (box (state of class)))
(define handle_new_class_instance
         (lambda (name CDT)
           (cons name (getForm name (unbox CDT)))))

(define get_class_from_var_or_expr
  (lambda (var_or_expr CDT boxed_state master_return break continue throw)
    (cond
      ((list? var_or_expr) (first_rest_rest (M_value_CDT var_or_expr CDT boxed_state master_return break continue throw)))
      ((eq? var_or_expr 'this) (get_this (unbox boxed_state)))
      ((eq? var_or_expr 'super) boxed_state)
      (else (first_rest_rest (get_var_value_box var_or_expr boxed_state))))))

;Returns the lowest level of a state. Since any state's lowest level is a class state, the values stored
;in the lowest level are the values referenced by "this."
(define get_this
 (lambda (unboxed_state)
   (cond
     ((null? (rest unboxed_state)) (box unboxed_state))
     (else (get_this (rest unboxed_state))))))

(define handle_dot
  (lambda (class field CDT boxed_state master_return break continue throw)
    (get_var_value_box field class)))