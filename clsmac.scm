#| -*-Scheme-*-

$Id: clsmac.scm,v 1.15 2007/01/05 21:19:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Class/Object System

(declare (usual-integrations))

;;; ******************************************************************
;;; This software is intended for use in the Edwin window system only.
;;; Don't think about using it for anything else, since it is not, and
;;; likely will not ever, be supported as a part of the Scheme system.
;;; ******************************************************************

(define-syntax define-class
  (rsc-macro-transformer
   (lambda (form environment)
     (if (and (syntax-match? '(IDENTIFIER DATUM (* SYMBOL)) (cdr form))
	      (or (identifier? (caddr form))
		  (null? (caddr form))))
	 (let ((name (cadr form))
	       (superclass (if (null? (caddr form)) #f (caddr form)))
	       (variables (cadddr form)))
	   ;; Compile-time definition.
	   (make-class (identifier->symbol name)
		       (and superclass
			    (name->class (identifier->symbol superclass)))
		       variables)
	   ;; Load-time definition.
	   `(,(close-syntax 'DEFINE environment)
	     ,name
	     (,(close-syntax 'MAKE-CLASS environment)
	      ',(identifier->symbol name)
	      ,superclass
	      ',variables)))
	 (ill-formed-syntax form)))))

(define-syntax define-method
  (rsc-macro-transformer
   (lambda (form environment)
     (let ((finish
	    (lambda (name operation expression)
	      `(,(close-syntax 'CLASS-METHOD-DEFINE environment)
		,name
		',operation
		,expression))))
       (cond ((syntax-match? '(IDENTIFIER SYMBOL EXPRESSION) (cdr form))
	      (finish (cadr form) (caddr form) (cadddr form)))
	     ((and (syntax-match? '(IDENTIFIER (SYMBOL . MIT-BVL) + EXPRESSION)
				  (cdr form))
		   (pair? (cdr (caddr form)))
		   (identifier? (cadr (caddr form))))
	      (finish (cadr form)
		      (car (caddr form))
		      `(,(close-syntax 'NAMED-LAMBDA environment)
			,(caddr form)
			(,(close-syntax 'WITH-INSTANCE-VARIABLES environment)
			 ,(cadr form)
			 ,(cadr (caddr form))
			 ()
			 ,@(cdddr form)))))
	     (else
	      (ill-formed-syntax form)))))))

(define with-instance-variables
  (make-unmapped-macro-reference-trap
   (make-compiler-item
    (lambda (form environment history)
      (if (syntax-match? '(IDENTIFIER EXPRESSION (* IDENTIFIER) + EXPRESSION)
			 (cdr form))
	  (let ((class-name (cadr form))
		(self (caddr form))
		(free-names (cadddr form))
		(body (cddddr form)))
	    (transform-instance-variables
	     (class-instance-transforms
	      (name->class (identifier->symbol class-name)))
	     (compile/subexpression self environment history select-caddr)
	     free-names
	     (compile/subexpression
	      `(,(close-syntax 'BEGIN system-global-environment) ,@body)
	      environment
	      history
	      select-cddddr)))
	  (ill-formed-syntax form))))))

(define-syntax ==>
  (syntax-rules ()
    ((==> object operation argument ...)
     (let ((temp object))
       ((object-method temp 'operation) temp argument ...)))))

(define-syntax usual==>
  (syntax-rules ()
    ((usual==> object operation argument ...)
     (let ((temp object))
       ((usual-method (object-class temp) 'operation) temp argument ...)))))