;---------------------------------------------------------------------------------
;Abstractions of car/cdr
;---------------------------------------------------------------------------------
;first and rest are already implemented by Pretty Big

;Functionality of caar
(define first_first
  (lambda (l)
    (caar l)))

;Functionality of cddr
(define rest_rest
  (lambda (l)
    (cddr l)))

;Functionality of cadr
(define first_rest
  (lambda (l)
    (cadr l)))

;Functionality of cdar
(define rest_first
  (lambda (l)
    (cdar l)))

;Functionality of caaar
(define first_first_first
  (lambda (l)
    (caaar l)))

;Functionality of caadr
(define first_first_rest
  (lambda (l)
    (caadr l)))

;Functionality of cadar
(define first_rest_first
  (lambda (l)
    (cadar l)))

;Functionality of caddr
(define first_rest_rest
  (lambda (l)
    (caddr l)))

;Functionality of cdaar
(define rest_first_first
  (lambda (l)
    (cdaar l)))

;Functionality of cdadr
(define rest_first_rest
  (lambda (l)
    (cdadr l)))

;Functionality of cddar
(define rest_rest_first
  (lambda (l)
    (cddar l)))

;Functionality of cdddr
(define rest_rest_rest
  (lambda (l)
    (cdddr l)))

;---------------------------------------------------------------------------------
;Type Checks
;---------------------------------------------------------------------------------

;Checks if an object is an atom. Returns true if so.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;Checks if an expression is unary. Returns true if so.
(define unary?
  (lambda (expr)
    (eq? (rest_rest expr) '())))

;---------------------------------------------------------------------------------
;Function Helpers
;---------------------------------------------------------------------------------

;Gets the name of an input function expression.
(define get_function_name
  (lambda (func)
    (first func)))

;Gets the parameter list of an input function expression.
(define get_function_params
  (lambda (func)
    (first_rest func)))

;Gets the body of an input function expression.
(define get_function_body
  (lambda (func)
    (first_rest_rest func)))

;---------------------------------------------------------------------------------
;List Manipulation
;---------------------------------------------------------------------------------

;Takes three inputs: two values and a list. Adds new first elements to a list of two lists.
(define newfirsts
  (lambda (f1 f2 l)
    (list (cons f1 (first l)) (cons f2 (first_rest l)))))

;Takes an input list of two lists and returns a list of the first two elements of those lists
(define getfirsts
  (lambda (l)
    (cons (first_first l) (cons (first_first_rest l) '()))))

;Removes the first elements of the sublists of a list. Said list is comprised of two lists.
(define removefirsts
  (lambda (l)
    (list (rest_first l) (rest_first_rest l))))

;Takes two inputs and makes them the first two values in the lists in an input list of two lists.
(define replacefirsts
 (lambda (f1 f2 l)
   (newfirsts f1 f2 (removefirsts l))))

;---------------------------------------------------------------------------------
;Custom Errors
;---------------------------------------------------------------------------------

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