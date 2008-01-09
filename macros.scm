;;; -*-Scheme-*-
;;;
;;; $Id: macros.scm,v 1.80 2007/01/05 21:19:23 cph Exp $
;;;
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;;     2006, 2007 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;;; USA.
;;;
;;;

;;;; Editor Macros


;; Upwards compatibility:
(define edwin-syntax-table (->environment '(EDWIN)))

(define-syntax define-editor-alias
  (sc-macro-transformer
   (lambda (form env)
     env
     (if (syntax-match? '(SYMBOL SYMBOL SYMBOL) (cdr form))
	 (let ((type (cadr form))
	       (new (caddr form))
	       (old (cadddr form)))
	   (receive (table name-map)
	       (case type
		 ((MODE)
		  (values 'EDITOR-MODES mode-name->scheme-name))
		 ((COMMAND)
		  (values 'EDITOR-COMMANDS command-name->scheme-name))
		 ((VARIABLE)
		  (values 'EDITOR-VARIABLES variable-name->scheme-name))
		 (else
		  (error "Unknown alias type:" type)))
	   `(BEGIN
	      (DEFINE ,(name-map new) ,(name-map old))
	      (STRING-TABLE-PUT! ,table
				 ,(symbol->string new)
				 ,(name-map old)))))
	 (ill-formed-syntax form)))))

(define-syntax define-command
  (rsc-macro-transformer
   (lambda (form environment)
     (capture-syntactic-environment
      (lambda (instance-environment)
	(if (syntax-match? '(SYMBOL EXPRESSION EXPRESSION EXPRESSION)
			   (cdr form))
	    (let ((name (list-ref form 1))
		  (description (list-ref form 2))
		  (interactive (list-ref form 3))
		  (procedure (list-ref form 4)))
	      (let ((scheme-name (command-name->scheme-name name)))
		`(,(close-syntax 'DEFINE environment)
		  ,scheme-name
		   (,(close-syntax 'MAKE-COMMAND environment)
		    ',name
		    ,description
		    ,interactive
		    ,(if (and (pair? procedure)
			      (identifier=?
			       instance-environment (car procedure)
			       environment 'LAMBDA)
			      (pair? (cdr procedure)))
			 `(,(close-syntax 'NAMED-LAMBDA environment)
			   (,scheme-name ,@(cadr procedure))
			   ,@(cddr procedure))
			 procedure)))))
	    (ill-formed-syntax form)))))))

(define-syntax ref-command-object
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (close-syntax (command-name->scheme-name (cadr form)) environment)
	 (ill-formed-syntax form)))))

(define (command-name->scheme-name name)
  (symbol-append 'EDWIN-COMMAND$ name))

(define-syntax ref-command
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL) (cdr form))
	 `(COMMAND-PROCEDURE (REF-COMMAND-OBJECT ,(cadr form)))
	 (ill-formed-syntax form)))))

(define-syntax command-defined?
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (let ((variable-name (command-name->scheme-name (cadr form))))
	   `(LET ((_ENV (->ENVIRONMENT '(EDWIN))))
	      (AND (ENVIRONMENT-BOUND? _ENV ',variable-name)
		   (ENVIRONMENT-ASSIGNED? _ENV ',variable-name))))
	 (ill-formed-syntax form)))))

(define-syntax define-variable
  (rsc-macro-transformer
   (lambda (form environment)
     (expand-variable-definition form environment `#F))))

(define-syntax define-variable-per-buffer
  (rsc-macro-transformer
   (lambda (form environment)
     (expand-variable-definition form environment `#T))))

(define (expand-variable-definition form environment buffer-local?)
  (if (and (syntax-match? '(SYMBOL + EXPRESSION) (cdr form))
	   (<= (length form) 6))
      `(,(close-syntax 'DEFINE environment)
	,(variable-name->scheme-name (list-ref form 1))
	(,(close-syntax 'MAKE-VARIABLE environment)
	 ',(list-ref form 1)
	 ,(list-ref form 2)
	 ,(if (> (length form) 3) (list-ref form 3) '#F)
	 ,buffer-local?
	 ,(if (> (length form) 4) (list-ref form 4) '#F)
	 ,(if (> (length form) 5) (list-ref form 5) '#F)))
      (ill-formed-syntax form)))

(define-syntax ref-variable-object
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (close-syntax (variable-name->scheme-name (cadr form)) environment)
	 (ill-formed-syntax form)))))

(define (variable-name->scheme-name name)
  (symbol-append 'EDWIN-VARIABLE$ name))

(define-syntax ref-variable
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL ? EXPRESSION) (cdr form))
	 (let ((name `(REF-VARIABLE-OBJECT ,(cadr form))))
	   (if (pair? (cddr form))
	       `(VARIABLE-LOCAL-VALUE ,(close-syntax (caddr form) environment)
				      ,name)
	       `(VARIABLE-VALUE ,name)))
	 (ill-formed-syntax form)))))

(define-syntax set-variable!
  (sc-macro-transformer
   (lambda (form environment)
     (expand-variable-assignment form environment
       (lambda (name value buffer)
	 (if buffer
	     `(SET-VARIABLE-LOCAL-VALUE! ,buffer ,name ,value)
	     `(SET-VARIABLE-VALUE! ,name ,value)))))))

(define-syntax local-set-variable!
  (sc-macro-transformer
   (lambda (form environment)
     (expand-variable-assignment form environment
       (lambda (name value buffer)
	 `(DEFINE-VARIABLE-LOCAL-VALUE! ,(or buffer `(CURRENT-BUFFER)) ,name
	    ,value))))))

(define (expand-variable-assignment form environment generator)
  (if (and (syntax-match? '(SYMBOL * EXPRESSION) (cdr form))
	   (<= (length form) 4))
      (generator `(REF-VARIABLE-OBJECT ,(list-ref form 1))
		 (if (> (length form) 2)
		     (close-syntax (list-ref form 2) environment)
		     `#F)
		 (if (> (length form) 3)
		     (close-syntax (list-ref form 3) environment)
		     #f))
      (ill-formed-syntax form)))

(define-syntax define-major-mode
  (sc-macro-transformer
   (let ((pattern
	  `(SYMBOL ,(lambda (x) (or (not x) (symbol? x)))
		   ,(lambda (x) (or (not x) (string? x)))
		   EXPRESSION
		   ? EXPRESSION)))
     (lambda (form environment)
       (if (syntax-match? pattern (cdr form))
	   (let ((name (list-ref form 1))
		 (super-mode-name (list-ref form 2)))
	     (let ((scheme-name (mode-name->scheme-name name)))
	       `(DEFINE ,scheme-name
		  (MAKE-MODE ',name
			     #T
			     ',(or (list-ref form 3)
				   (symbol->string name))
			     ,(if super-mode-name
				  `(->MODE ',super-mode-name)
				  `#F)
			     ,(close-syntax (list-ref form 4) environment)
			     ,(let ((initialization
				     (if (and (> (length form) 5)
					      (list-ref form 5))
					 (close-syntax (list-ref form 5)
						       environment)
					 #f)))
				(if super-mode-name
				    `(LAMBDA (BUFFER)
				       ((MODE-INITIALIZATION
					 (MODE-SUPER-MODE
					  ,(close-syntax scheme-name
							 environment)))
					BUFFER)
				       ,@(if initialization
					     `((,initialization BUFFER))
					     `()))
				    (or initialization
					`(LAMBDA (BUFFER)
					   BUFFER
					   UNSPECIFIC))))))))
	   (ill-formed-syntax form))))))

(define-syntax define-minor-mode
  (sc-macro-transformer
   (let ((pattern
	  `(SYMBOL ,(lambda (x) (or (not x) (string? x)))
		   EXPRESSION
		   ? EXPRESSION)))
     (lambda (form environment)
       (if (syntax-match? pattern (cdr form))
	   (let ((name (list-ref form 1)))
	     `(DEFINE ,(mode-name->scheme-name name)
		(MAKE-MODE ',name
			   #F
			   ',(or (list-ref form 2)
				 (symbol->string name))
			   #F
			   ,(close-syntax (list-ref form 3) environment)
			   ,(if (and (> (length form) 4)
				     (list-ref form 4))
				(close-syntax (list-ref form 4) environment)
				`(LAMBDA (BUFFER) BUFFER UNSPECIFIC)))))
	   (ill-formed-syntax form))))))

(define-syntax ref-mode-object
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (close-syntax (mode-name->scheme-name (cadr form)) environment)
	 (ill-formed-syntax form)))))

(define (mode-name->scheme-name name)
  (symbol-append 'EDWIN-MODE$ name))
