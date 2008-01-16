#| -*-Scheme-*-

$Id: display.scm,v 1.12 2007/01/05 21:19:23 cph Exp $

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

;;;; Display-Type Abstraction
;;; package: (edwin display-type)


(define-structure (display-type
		   (conc-name display-type/)
		   (constructor %make-display-type)
		   (print-procedure
		    (unparser/standard-method 'DISPLAY-TYPE
		      (lambda (state display-type)
			(unparse-object state
					(display-type/name display-type))))))
  (name #f read-only #t)
  (multiple-screens? #f read-only #t)
  (operation/available? #f read-only #t)
  (operation/make-screen #f read-only #t)
  (operation/get-input-operations #f read-only #t)
  (operation/with-display-grabbed #f read-only #t)
  (operation/with-interrupts-enabled #f read-only #t)
  (operation/with-interrupts-disabled #f read-only #t))

(define (make-display-type name
			   multiple-screens?
			   available?
			   make-screen
			   get-input-operations
			   with-display-grabbed
			   with-interrupts-enabled
			   with-interrupts-disabled)
  (let ((display-type
	 (%make-display-type name
			     multiple-screens?
			     available?
			     make-screen
			     get-input-operations
			     with-display-grabbed
			     with-interrupts-enabled
			     with-interrupts-disabled)))
    (set! display-types (cons display-type display-types))
    display-type))

(define display-types '())

(define (display-type/available? display-type)
  ((display-type/operation/available? display-type)))

(define (display-type/make-screen display-type args)
  (apply (display-type/operation/make-screen display-type) args))

(define (display-type/get-input-operations display-type screen)
  ((display-type/operation/get-input-operations display-type) screen))

(define (display-type/with-display-grabbed display-type thunk)
  ((display-type/operation/with-display-grabbed display-type) thunk))

(define (display-type/with-interrupts-enabled display-type thunk)
  ((display-type/operation/with-interrupts-enabled display-type) thunk))

(define (display-type/with-interrupts-disabled display-type thunk)
  ((display-type/operation/with-interrupts-disabled display-type) thunk))

(define (editor-display-types)
  (filter display-type/available? display-types))

(define (name->display-type name)
  (let ((display-type
	 (find (lambda (display-type)
		 (eq? name (display-type/name display-type)))
	       display-types)))
    display-type))