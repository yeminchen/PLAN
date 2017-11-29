;Project 4 Yemin Chen
;11:30 a.m. section

;this function calls eval-prog function to evaluate each (prog ...)
(define (myinterpreter x)
   (cond
      ((null? x) x)
      (#t (cons (eval-prog (car x)) (myinterpreter (cdr x))))
   )
)

;used for evaluate prog
(define (eval-prog x)
  (cond
    ((null? x) x)
    ;when it calls eval-expr, it also passes binding list, which is empty at the first of each prog
    (#t (eval-expr (cdr x) '(())))
   )
)

(define (eval-expr x binding)
   (cond
             ((integer? x) x)
             ((integer? (car x)) (car x))
             ;in my program, expr may be passed in by two types: () and (())
             ((equal? (car x ) 'myignore) 0)
             ((equal? (car x ) 'myadd) (eval-myadd (cdr x) binding))
             ((equal? (car x ) 'mymul) (eval-mymul (cdr x) binding))
             ((equal? (car x ) 'myneg) (eval-myneg (cdr x) binding))
             ((equal? (car x ) 'mylet) (eval-mylet (cdr x) binding))
             ((symbol? (car x )) (search-binding (car x) binding))
             
             ((equal? (car (car x )) 'myignore) 0)
             ((equal? (car (car x )) 'myadd) (eval-myadd (cdr (car x)) binding))
             ((equal? (car (car x )) 'mymul) (eval-mymul (cdr (car x)) binding))
             ((equal? (car (car x )) 'myneg) (eval-myneg (cdr (car x)) binding))
             ((equal? (car (car x )) 'mylet) (eval-mylet (cdr (car x)) binding))
             ((symbol? (car (car x )) (search-binding (car (car x)) binding)))
             (#t '(-1))
   )
)

;function for evaluate myadd
(define (eval-myadd x binding)
    (+ (eval-expr (cons (car x) '()) binding) (eval-expr (cdr x) binding))

)

;function for evaluate mymul
(define (eval-mymul x binding)
    (* (eval-expr (cons (car x) '()) binding) (eval-expr (cdr x) binding))

)

;function for evaluate myneg
(define (eval-myneg x binding)
  (* (eval-expr (cons (car x) '()) binding) -1)

)

;function for evaluate mylet
(define (eval-mylet x binding)
  ;update binding list
  (let ([binding
         (cons (cons (car x) (cons (eval-expr (car (cdr x)) binding) '())) binding)
         ])
        ;body
        (eval-expr  (cdr (cdr x)) binding))

)

;when eval-expr read a symbol, it will use this function to search for its binding value.
(define (search-binding x binding)
  (cond
    ((null? binding) -100)
    ((equal? x 
                (car (car binding)))  (car (cdr (car binding))) )
    (#t (search-binding x (cdr binding)))
  )
)
  
