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

(define (substring-move-left! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 start1 end1))

(define (substring-move-right! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 end1 start1))

(define (string-head string end)
  (substring string 0 end))

(define (string-tail string start)
  (substring string start (string-length string)))

(define (subvector-move-left! vector1 start1 end1 vector2 start2)
  (vector-copy! vector2 start2 vector1 start1 end1))

(define (subvector-move-right! vector1 start1 end1 vector2 start2)
  (vector-copy! vector2 start2 vector1 end1 start1))

(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

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

