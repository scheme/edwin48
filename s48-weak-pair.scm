;;; -*- Mode: Scheme; scheme48-package: weak-pair -*-

(define-record-type* weak-pair
  (make-weak-pair (car) (cdr))
  ())

(define (weak-cons car cdr)
  (make-weak-pair (make-weak-pointer car) cdr))

(define (weak-pair/car? pair)
  (weak-pointer-ref (weak-pair-car pair)))

(define (weak-car pair)
  (if (weak-pair/car? pair)
      (weak-pointer-ref (weak-pair-car pair))
      #f))

(define (weak-set-car! pair obj)
  (if (weak-pair? pair)
      (set-weak-pair-car! pair (make-weak-pointer obj))))

(define (weak-cdr pair) (weak-pair-cdr pair))

(define (weak-set-cdr! pair obj)
  (set-weak-pair-cdr! pair obj))

(define (weak-memq x weaks)
  (if (null? weaks)
      #f
      (if (eq? x (weak-pointer-ref (weak-car weaks)))
	  weaks
	  (weak-memq x (weak-cdr weaks)))))

