#lang racket
(define (myinterpreter x)
   (cond
      ((null? x) x)
      (#t (cons (eval-prog (car x)) (myinterpreter (cdr x))))
   )
)

(define (eval-prog x)
  (cond
    ((null? x) x)
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


(define (eval-myadd x binding)
    (+ (eval-expr (cons (car x) '()) binding) (eval-expr (cdr x) binding))

)

(define (eval-mymul x binding)
    (* (eval-expr (cons (car x) '()) binding) (eval-expr (cdr x) binding))

)

(define (eval-myneg x binding)
  (* (eval-expr (cons (car x) '()) binding) -1)

)

(define (eval-mylet x binding)
  (let ([binding
         (cons (cons (car x) (cons (eval-expr (car (cdr x)) binding) '())) binding)
         ])
        ;body
        (eval-expr  (cdr (cdr x)) binding))

)

(define (search-binding x binding)
  (cond
    ((null? binding) -100)
    ((equal? x 
                (car (car binding)))  (car (cdr (car binding))) )
    (#t (search-binding x (cdr binding)))
  )
)
  
