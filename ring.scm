#| -*-Scheme-*-

$Id: ring.scm,v 1.17 2007/01/05 21:19:24 cph Exp $

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

;;;; Rings


(define (ring-list ring)
  (list-copy (vector-ref ring 2)))

(define (list-ref l i)
  (cond ((null? l) (error "Index too large" 'LIST-REF))
	((zero? i) (car l))
	(else (list-ref (cdr l) (-1+ i)))))

(define (list-set! l i o)
  (let loop ((l l) (i i))
    (cond ((null? l) (error "index too large" i))
	  ((zero? i) (set-car! l o))
	  (else (list-ref (cdr l) (-1+ i)))))
  unspecific)

(define (list-truncate! l i)
  (cond ((null? l) unspecific)
	((= i 1) (set-cdr! l '()))
	(else (list-truncate! (cdr l) (-1+ i))))
  unspecific)

(define (make-ring size)
  (if (< size 1)
      (error "Ring size too small" size)
      (vector "Ring" size '())))

(define (ring-size ring)
  (length (vector-ref ring 2)))

(define (ring-clear! ring)
  (vector-set! ring 2 '())
  unspecific)

(define (ring-empty? ring)
  (null? (vector-ref ring 2)))


(define (ring-push! ring object)
  (vector-set! ring 2 (cons object (vector-ref ring 2)))
  (list-truncate! (vector-ref ring 2) (vector-ref ring 1)))

(define (ring-pop! ring)
  (let ((l (vector-ref ring 2)))
    (if (null? l)
	(error "Ring empty" ring)
	(let ((object (car l)))
	  (vector-set! ring 2 (append! (cdr l) (list object)))
	  object))))

(define (ring-ref ring index)
  (list-ref (vector-ref ring 2) (modulo index (ring-size ring))))

(define (ring-set! ring index object)
  (list-set! (vector-ref ring 2) (modulo index (ring-size ring)) object))
