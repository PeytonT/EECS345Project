;This section is for abstractions of the car/cdr functions. first and rest are already implemented by Pretty Big

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