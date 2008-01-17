;;; -*- Mode: Scheme; scheme48-package: aliases -*-
;;;
;;; Aliases for MIT Scheme
;;;

(define (without-interrupts thunk)
  (with-interrupts-inhibited thunk))

(define unspecific (unspecific))

(define  (1+ z) (+ z 1))
(define (-1+ z) (- z 1))

(define (delq! element items)
  (remove! (lambda (x) (eq? element x)) items))

(define (list-deletor! predicate)
  (lambda (items) (remove! predicate items)))

(define (vector-grow vec k)
  (let ((difference (- k (vector-length vec))))
    (vector-append vec (make-vector difference #f))))
