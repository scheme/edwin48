;;; -*-Scheme-*-
;;;
;;; $Id: class.scm,v 1.77 2007/01/05 21:19:23 cph Exp $
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

;;;; Class/Object System


;;; ******************************************************************
;;; This software is intended for use in the Edwin window system only.
;;; Don't think about using it for anything else, since it is not, and
;;; likely will not ever, be supported as a part of the Scheme system.
;;; ******************************************************************

(define-structure (class (constructor %make-class))
  (name false read-only true)
  (superclass false read-only true)
  object-size
  instance-transforms
  (methods false read-only true))

(define (class-method class name)
  (class-methods/ref (class-methods class) name))

(define (class-methods/ref methods name)
  (or (method-lookup methods name)
      (error "Unknown method:" name)))

(define (method-lookup methods name)
  (let loop ((methods methods))
    (and methods
	 (let ((entry (assq name (car methods))))
	   (if entry
	       (cdr entry)
	       (loop (cdr methods)))))))

(define (class-method-define class name method)
  (let ((methods (class-methods class)))
    (let ((entry (assq name (car methods))))
      (if entry
	  (set-cdr! entry method)
	  (set-car! methods (cons (cons name method) (car methods))))))
  name)

(define-integrable (usual-method class name)
  (class-method (class-superclass class) name))

(define (subclass? class class*)
  (or (eq? class class*)
      (let loop ((class (class-superclass class)))
	(and class
	     (or (eq? class class*)
		 (loop (class-superclass class)))))))

(define (make-object class)
  (if (not (class? class))
      (error:wrong-type-argument class "class" 'MAKE-OBJECT))
  (let ((object (make-vector (class-object-size class) false)))
    (vector-set! object 0 class)
    object))

(define (object? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (class? (vector-ref object 0))))

(define (object-of-class? class object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? class (vector-ref object 0))))

(define-integrable (object-class object)
  (vector-ref object 0))

(define-integrable (object-methods object)
  (class-methods (object-class object)))

(define-integrable (object-method object name)
  (class-method (object-class object) name))

(define (send object operation . args)
  (apply (object-method object operation) object args))

(define (send-if-handles object operation . args)
  (let ((method (method-lookup (object-methods object) operation)))
    (and method (apply method object args))))

(define (send-usual class object operation . args)
  (apply (usual-method class operation) object args))
