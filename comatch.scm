#| -*-Scheme-*-

$Id: comatch.scm,v 1.9 2007/01/05 21:19:23 cph Exp $

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

;;;; Combinatoric Matcher

(declare (usual-integrations))

;;; This matcher allows compex matching patterns to be built up from
;;; primitives using combinators.  With this implementation, the
;;; patterns are just procedures, but it is simple to change the
;;; implementation so that they use a different representation.

;;; Each pattern takes two arguments: a start mark and an end mark.
;;; The pattern matches itself against the buffer's contents between
;;; the two marks.  A successful match returns a mark to the right of
;;; the rightmost character in the match.  A failed match returns #F.

(define (comatch-apply comatcher start #!optional end)
  (comatcher start
	     (if (default-object? end) (group-end start) end)))

(define (comatch:general procedure) procedure)

(define comatch:false (comatch:general (lambda (start end) start end #f)))
(define comatch:true (comatch:general (lambda (start end) end start)))

(define comatch:to-sexp (comatch:general forward-to-sexp-start))
(define comatch:sexp (comatch:general forward-one-sexp))

(define (comatch:char char #!optional case-fold?)
  (if (or (default-object? case-fold?) (not case-fold?))
      (comatch:general
       (lambda (start end)
	 (and (mark< start end)
	      (char=? char (extract-right-char start))
	      (mark1+ start))))
      (comatch:general
       (lambda (start end)
	 (and (mark< start end)
	      (char-ci=? char (extract-right-char start))
	      (mark1+ start))))))

(define (comatch:string string #!optional case-fold?)
  (let ((case-fold? (if (default-object? case-fold?) #f case-fold?)))
    (comatch:general
     (lambda (start end)
       (match-forward string start end case-fold?)))))

(define (comatch:regexp regexp #!optional case-fold?)
  (let ((regexp
	 (if (compiled-regexp? regexp)
	     regexp
	     (re-compile-pattern regexp
				 (if (default-object? case-fold?)
				     #f
				     case-fold?)))))
    (comatch:general
     (lambda (start end)
       (re-match-forward regexp start end)))))

(define (comatch:skip-chars pattern)
  (comatch:general
   (lambda (start end)
     (skip-chars-forward pattern start end))))

;;;; Combinators

(define (comatch:* comatcher)
  (comatch:general
   (lambda (start end)
     (let loop ((start start))
       (let ((mark (comatch-apply comatcher start end)))
	 (if mark
	     (loop mark)
	     start))))))

(define (comatch:+ comatcher)
  (let ((tail (comatch:* comatcher)))
    (comatch:general
     (lambda (start end)
       (let ((mark (comatch-apply comatcher start end)))
	 (and mark
	      (tail mark end)))))))

(define (comatch:? comatcher)
  (comatch:general
   (lambda (start end)
     (or (comatch-apply comatcher start end) start))))

(define (comatch:not comatcher)
  (comatch:general
   (lambda (start end)
     (and (not (comatch-apply comatcher start end))
	  start))))

(define (comatch:combine-rest initial combine-2)
  (lambda comatchers
    (if (null? comatchers)
	initial
	(let loop ((comatchers comatchers))
	  (if (null? (cdr comatchers))
	      (car comatchers)
	      (combine-2 (car comatchers) (loop (cdr comatchers))))))))

(define comatch:append
  (comatch:combine-rest comatch:true
    (lambda (c1 c2)
      (comatch:general
       (lambda (start end)
	 (let ((start (comatch-apply c1 start end)))
	   (and start
		(comatch-apply c2 start end))))))))

(define comatch:or
  (comatch:combine-rest comatch:true
    (lambda (c1 c2)
      (comatch:general
       (lambda (start end)
	 (or (comatch-apply c1 start end)
	     (comatch-apply c2 start end)))))))

(define comatch:and
  (comatch:combine-rest comatch:true
    (lambda (c1 c2)
      (comatch:general
       (lambda (start end)
	 (and (comatch-apply c1 start end)
	      (comatch-apply c2 start end)))))))