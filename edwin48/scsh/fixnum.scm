;;;
;;; Fixnum operations
;;;

(define (fix:= a b) (= a b))
(define (fix:< a b) (< a b))
(define (fix:> a b) (> a b))
(define (fix:<= a b) (<= a b))
(define (fix:>= a b) (>= a b))

(define (fix:fixnum?   n) (integer?  n))
(define (fix:zero?     n) (zero?     n))
(define (fix:positive? n) (positive? n))
(define (fix:negative? n) (negative? n))

(define (fix:+ a b) (+ a b))
(define (fix:- a b) (- a b))
(define (fix:* a b) (* a b))
(define (fix:min       a b) (min       a b))
(define (fix:max       a b) (max       a b))
(define (fix:quotient  a b) (quotient  a b))
(define (fix:remainder a b) (remainder a b))
(define (fix:1+  n) (+ n 1))
(define (fix:-1+ n) (- n 1))


(define (fix:not  n)   (bitwise-not n))
(define (fix:and  a b) (bitwise-and a b))
(define (fix:andc a b) (bitwise-and a (bitwise-not b)))
(define (fix:or   a b) (bitwise-ior a b))
(define (fix:xor  a b) (bitwise-xor a b))
(define (fix:lsh  a b) (arithmetic-shift a b))


