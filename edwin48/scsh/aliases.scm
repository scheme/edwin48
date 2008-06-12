;;; -*- Mode: Scheme; scheme48-package: aliases -*-
;;;
;;; Aliases for MIT Scheme
;;;

(define (without-interrupts thunk)
  (with-interrupts-inhibited thunk))

(define unspecific (unspecific))

(define  (1+ z) (+ z 1))
(define (-1+ z) (- z 1))

(define (list-deletor! predicate)
  (lambda (items) (remove! predicate items)))

(define (vector-grow vec k)
  (let ((difference (- k (vector-length vec))))
    (vector-append vec (make-vector difference #f))))

(define (string-head string end)
  (substring string 0 end))

(define (string-tail string start)
  (substring string start (string-length string)))

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
    ((fluid-let ((variable init)) expression ...)
     (let-fluid variable init (lambda () expression ...)))))

(define (integer-round number) (round number))

(define (real-time-clock)
  (receive (secs ticks)
      (time+ticks)
    ticks))
