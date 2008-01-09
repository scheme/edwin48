;;; -*-Scheme-*-
;;;
;;; $Id: argred.scm,v 1.38 2007/01/05 21:19:23 cph Exp $
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

;;;; Command Argument Reader


(define-command universal-argument
  "Begin a numeric argument for the following command.
Digits or minus sign following this command make up the numeric argument.
If no digits or minus sign follow, this command by itself provides 4 as argument.
Used more than once, this command multiplies the argument by 4 each time."
  "P"
  (lambda (argument)
    (set-command-argument! (list (* (if (pair? argument) (car argument) 1) 4))
			   (key-name (last-command-key)))))

(define-command digit-argument
  "Part of the numeric argument for the next command."
  "P"
  (lambda (argument)
    (digit-argument argument (auto-argument-mode?))))

(define (digit-argument argument mode)
  (let ((key (last-command-key)))
    (if (char? key)
	(let ((digit (char->digit (char-base key))))
	  (if digit
	      (set-command-argument!
	       (cond ((eq? '- argument) (- digit))
		     ((not (number? argument)) digit)
		     ((negative? argument) (- (* 10 argument) digit))
		     (else (+ (* 10 argument) digit)))
	       mode))))))

(define-command negative-argument
  "Begin a negative numeric argument for the next command."
  "P"
  (lambda (argument)
    (negative-argument argument (auto-argument-mode?))))

(define (negative-argument argument mode)
  (set-command-argument! (cond ((eq? '- argument) false)
			       ((number? argument) (- argument))
			       (else '-))
			 mode))

(define-command auto-argument
  "Start a command argument.
Digits following this command become part of the argument."
  "P"
  (lambda (argument)
    (let ((mode (if argument (auto-argument-mode?) true)))
      (if (let ((key (last-command-key)))
	    (and (char? key)
		 (char=? #\- (char-base key))))
	  (if (not (number? argument))
	      (negative-argument argument mode))
	  (digit-argument argument mode)))))

(define-command auto-digit-argument
  "When reading a command argument, part of the numeric argument.
Otherwise, the digit inserts itself."
  "P"
  (lambda (argument)
    (if (auto-argument-mode?)
	((ref-command digit-argument) argument)
	((ref-command self-insert-command) argument))))

(define-command auto-negative-argument
  "When reading a command argument, begin a negative argument.
Otherwise, the character inserts itself."
  "P"
  (lambda (argument)
    (if (and (auto-argument-mode?)
	     (not (number? argument)))
	((ref-command negative-argument) argument)
	((ref-command self-insert-command) argument))))

(define (command-argument-self-insert? command)
  (and (or (eq? command (ref-command-object auto-digit-argument))
	   (and (eq? command (ref-command-object auto-negative-argument))
		(not (number? (command-argument)))))
       (not (auto-argument-mode?))))

(define (command-argument-prompt)
  (let ((arg (command-argument)))
    (if (not arg)
	""
	(let ((mode (auto-argument-mode?)))
	  (string-append
	   (if (and (pair? arg) (string? mode))
	       (let loop ((n (car arg)))
		 (if (= n 4)
		     mode
		     (string-append mode " " (loop (quotient n 4)))))
	       (string-append
		(cond ((string? mode) mode)
		      (mode "Autoarg")
		      (else "Arg"))
		" "
		(if (eq? '- arg)
		    "-"
		    (number->string (if (pair? arg) (car arg) arg)))))
	   " -")))))

(define (command-argument-multiplier-only? argument)
  (pair? argument))

(define (command-argument-negative-only? argument)
  (eq? '- argument))

(define (command-argument-value argument)
  (cond ((not argument) false)
	((eq? '- argument) -1)
	((pair? argument) (car argument))
	(else argument)))

(define (command-argument-numeric-value argument)
  (or (command-argument-value argument) 1))
