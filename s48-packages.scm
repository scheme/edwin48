;;; -*- mode: scheme; scheme48-package: (config) -*-
;;;
;;; Scheme48 specific packages
;;;

(define-structure define-record-type*
    (export (define-record-type* :syntax)
            define-record-discloser)
  (open scheme define-record-types)
  (for-syntax (open scheme define-record-type*-expander))
  (begin (define-syntax define-record-type*
           expand-define-record-type*
           (BEGIN DEFINE DEFINE-RECORD-TYPE))))

(define-structure define-record-type*-expander
    (export expand-define-record-type*)
  (open scheme destructuring fluids signals receiving)
  (files s48-records))

(define-structure weak-pair
    (export weak-pair?
            weak-cons
            weak-pair/car?
            weak-car weak-set-car!
            weak-cdr weak-set-cdr!
            weak-memq)
  (open scheme define-record-type* weak)
  (files s48-weak-pair))

(define-structure fixnum
    (export fix:= fix:fixnum?
            fix:< fix:> fix:<= fix:>=
            fix:zero? fix:positive? fix:negative?
            fix:+ fix:- fix:*
            fix:quotient fix:remainder
            fix:1+ fix:1-
            fix:not fix:and fix:andc
            fix:or fix:xor fix:lsh)
  (open scheme bitwise)
  (files s48-fixnum))

(define-structure errors
    (export error
	    error:bad-range-argument
	    error:datum-out-of-range
	    error:file-operation
	    error:wrong-type-argument)
  (open scheme (subset signals (error)))
  (files s48-errors))

(define-structure aliases
    (export without-interrupts
	    unspecific
	    1+ -1+
	    delq! list-deletor!
	    vector-grow
            substring-move-left! substring-move-right!
            string-head string-tail)
  (open scheme interrupts util srfi-1 srfi-13 srfi-43)
  (files s48-aliases))

(define-structure define-opt
    (export (define* :syntax))
  (open scheme srfi-1 let-opt)
  (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))
  (files s48-define-opt))