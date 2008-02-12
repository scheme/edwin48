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

(define (call-with-binary-input-file filename thunk)
  (call-with-input-file filename thunk))

(define (call-with-binary-output-file filename thunk)
  (call-with-output-file filename thunk))

(define (with-input-from-binary-file filename thunk)
  (with-input-from-file filename thunk))

(define (with-output-to-binary-file filename thunk)
  (with-output-to-file filename thunk))

(define (symbol-name s)
  (symbol->string s))

(define (vector-8b-ref string k)
  (char->ascii (string-ref string k)))

(define (vector-8b-set! string k code)
  (string-set! string k (ascii->char code)))

(define (beep) unspecific)

(define (exact-nonnegative-integer? obj)
  (and (integer? obj) (> obj 0) (exact? obj)))

(define (exact-integer? obj)
  (and (integer? obj) (exact? obj)))

(define (char->digit c)
  (if (char-set-contains? char-set:digit c)
      (- (char->integer c) (char->integer #\0))
      (error "this is not a digit" c)))

(define (alist? object)
  (and (list? object)
       (every pair? object)))

