;;;
;;; Aliases for MIT Scheme
;;;

(define (without-interrupts thunk)
  (with-interrupts-inhibited thunk))

(define unspecific (unspecific))

(define true  #t)
(define false #f)