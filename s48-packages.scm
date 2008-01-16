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
    (export fix:=
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
    (export error:bad-range-argument
	    error:datum-out-of-range
	    error:file-operation
	    error:wrong-type-argument)
  (open scheme signals)
  (files s48-errors))

(define-structure aliases
    (export without-interrupts
	    unspecific
	    1+ -1+
	    delq! list-deletor!)
  (open scheme interrupts util srfi-1)
  (files s48-aliases))

(define-structure literals
    (export) ;; no exports
  (open scheme ascii reading srfi-13)
  (files s48-literals))

(define-structure strings
    (export string-capitalize
	    string-find-next-char substring-find-next-char
	    string-find-next-char-ci substring-find-next-char-ci
	    string-find-next-char-in-set substring-find-next-char-in-set
	    string-find-previous-char substring-find-previous-char
	    string-find-previous-char-ci substring-find-previous-char-ci
	    string-find-previous-char-in-set substring-find-previous-char-in-set
	    substring-prefix-ci?
	    string-match-forward substring-match-forward-ci
	    substring-move-left! substring-move-right!
	    substring-fill!)
  (open (modify scheme (hide string-fill!)) srfi-13)
  (files s48-strings))
