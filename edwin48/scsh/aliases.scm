;;; -*- Mode: Scheme; scheme48-package: aliases -*-
;;;
;;; Aliases for MIT Scheme
;;;

(define (without-interrupts thunk)
  (with-interrupts-inhibited thunk))

(define unspecific (unspecific))

(define  (1+ z) (+ z 1))
(define (-1+ z) (- z 1))

(define (integer-divide n d)
  (cons (quotient  n d)
        (remainder n d)))

(define (integer-divide-quotient qr)
  (if (and (pair? qr) (= 2 (length qr)))
      (car qr)))

(define (integer-divide-remainder qr)
  (if (and (pair? qr) (= 2 (length qr)))
      (cdr qr)))

(define (list-deletor! predicate)
  (lambda (items) (remove! predicate items)))

(define (vector-grow vec k)
  (let ((difference (- k (vector-length vec))))
    (vector-append vec (make-vector difference #f))))

(define (string-head string end)
  (substring string 0 end))

(define (string-tail string start)
  (substring string start (string-length string)))

(define* (string-index-right-ci string char (start 0) (end (string-length string)))
  (string-index-right string (lambda (c) (char-ci=? c char)) start end))

(define* (string-index-ci string char (start 0) (end (string-length string)))
  (string-index string (lambda (c) (string-ci= c char)) start end))

(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

(define (symbol<? symbol1 symbol2)
  (string<? (symbol->string symbol1)
            (symbol->string symbol2)))

(define (symbol-name s)
  (symbol->string s))

(define (vector-8b-ref string k)
  (char->ascii (string-ref string k)))

(define (vector-8b-set! string k code)
  (string-set! string k (ascii->char code)))

(define (vector-8b-fill! string start end ascii)
  (string-fill! string ascii start end))

(define (vector-8b-find-next-char string start end ascii)
  (string-index string ascii start end))

(define (beep) unspecific)

(define (exact-nonnegative-integer? obj)
  (and (integer? obj) (> obj 0) (exact? obj)))

(define (exact-integer? obj)
  (and (integer? obj) (exact? obj)))

(define (char->digit c)
  (if (char-set-contains? char-set:digit c)
      (- (char->integer c) (char->integer #\0))
      (error "this is not a digit" c)))

(define (char-ascii? c) (char? c))

(define (alist? object)
  (and (list? object)
       (every pair? object)))

(define (write-to-string obj)
  (let ((port (open-output-string)))
    (write obj port)
    (get-output-string port)))

(define* (write-string s (port (current-output-port)))
  (string-for-each (lambda (c) (write-char s port)) s))

(define-syntax fluid-let
  (syntax-rules ()
    ((fluid-let ((variable init) ...) body0 body1 ...)
     (*fluid-let ((variable init) ...) body0 body1 ...))))

(define-syntax *fluid-let
  (syntax-rules ()
    ((*fluid-let ((bindings ...) ...) . body)
     (let-fluids bindings ... ... . body))))

(define (integer-round n1 n2) (round (/ n1 n2)))

(define (round->exact x) (inexact->exact (round x)))

(define (real-time-clock)
  (receive (secs ticks)
      (time+ticks)
    ticks))

(define (boolean=? x y)
  (or (and (eq? x #t) (eq? y #t))
      (and (eq? x #f) (eq? y #f))))

(define* (make-circular-list k (element '()))
  (circular-list element))

(define (identity-procedure x) x)
