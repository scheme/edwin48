;;; -*-Scheme-*-
;;;
;;; $Id: lspcom.scm,v 1.166 2007/04/18 20:27:30 riastradh Exp $
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

;;;; Lisp Commands


;;;; S-expression Commands

(define-command forward-sexp
  "Move forward across one balanced expression.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing forward-sexp argument 'ERROR)))

(define-command backward-sexp
  "Move backward across one balanced expression.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing backward-sexp argument 'ERROR)))

(define-command flash-sexp
  "Flash the char which ends the expression to the right of point.
Shows you where \\[forward-sexp] would go."
  "p"
  (lambda (argument)
    (mark-flash (forward-sexp (current-point) argument)
		(if (negative? argument) 'RIGHT 'LEFT))))

(define-command backward-flash-sexp
  "Flash the char which starts the expression to the left of point.
Shows you where \\[backward-sexp] would go."
  "p"
  (lambda (argument)
    (mark-flash (backward-sexp (current-point) argument)
		(if (negative? argument) 'LEFT 'RIGHT))))

(define-command kill-sexp
  "Kill the syntactic expression following the cursor.
With argument, kill that many expressions after (or before) the cursor."
  "p"
  (lambda (argument)
    (kill-thing forward-sexp argument 'ERROR)))

(define-command backward-kill-sexp
  "Kill the syntactic expression preceding the cursor.
With argument, kill that many expressions before (or after) the cursor."
  "p"
  (lambda (argument)
    (kill-thing backward-sexp argument 'ERROR)))

(define-command transpose-sexps
  "Transpose the sexps before and after point.
See \\[transpose-words], reading 'sexp' for 'word'."
  "p"
  (lambda (argument)
    (transpose-things forward-sexp argument)))

(define-command mark-sexp
  "Mark one or more sexps from point."
  "p"
  (lambda (argument)
    (mark-thing forward-sexp argument 'ERROR)))

;;;; List Commands

(define-command forward-list
  "Move forward across one balanced group of parentheses.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing forward-list argument 'ERROR)))

(define-command backward-list
  "Move backward across one balanced group of parentheses.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing backward-list argument 'ERROR)))

(define-command down-list
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level."
  "p"
  (lambda (argument)
    (move-thing forward-down-list argument 'ERROR)))

(define-command backward-down-list
  "Move backward down one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still go down a level."
  "p"
  (lambda (argument)
    (move-thing backward-down-list argument 'ERROR)))

(define-command up-list
  "Move forward out one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot."
  "p"
  (lambda (argument)
    (move-thing forward-up-list argument 'ERROR)))

(define-command backward-up-list
  "Move backward out one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot."
  "p"
  (lambda (argument)
    (move-thing backward-up-list argument 'ERROR)))

;;;; Definition Commands

(define-command beginning-of-defun
  "Move backward to next beginning-of-defun.
With argument, do this that many times."
  "p"
  (lambda (argument)
    (move-thing backward-definition-start argument 'ERROR)))

(define-command end-of-defun
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  "p"
  (lambda (argument)
    (move-thing forward-definition-end
		(if (zero? argument) 1 argument)
		'ERROR)))

(define-command mark-defun
  "Put mark at end of defun, point at beginning."
  ()
  (lambda ()
    (let ((point (current-point)))
      (let ((end (forward-definition-end point 1 'ERROR)))
	(let ((start (backward-definition-start end 1 'ERROR)))
	  (push-current-mark! point)
	  (push-current-mark! end)
	  (set-current-point!
	   (or (and (not (group-start? start))
                    (re-search-backward "^\n" start (mark-1+ start)))
	       start)))))))

(define-command align-defun
  "Reposition window so current defun is at the top.
If this would place point off screen, nothing happens."
  ()
  (lambda ()
    (reposition-window-top (current-definition-start))))

(define (current-definition-start)
  (or (this-definition-start (current-point))
      (error "Not inside a definition.")))

(define (this-definition-start mark)
  (let ((start (line-start mark 0)))
    (if (definition-start? start)
	start
	(backward-one-definition-start start))))

;;;; Miscellaneous Commands

(define-command lisp-insert-paren
  "Insert one or more close parens, flashing the matching open paren."
  "p"
  (lambda (argument)
    (let ((key (current-command-key)))
      (if (char? key)
	  (begin
	    (insert-chars key argument)
	    (if (positive? argument)
		(let ((point (current-point)))
		  (if (not (mark-left-char-quoted? point))
		      (mark-flash (backward-one-sexp point) 'RIGHT)))))))))

(define-command lisp-indent-line
  "Indent current line as lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  "P"
  (lambda (#!optional argument)
    (lisp-indent-line (and (not (default-object? argument)) argument))))

(define-command indent-sexp
  "Indent each line of the expression starting just after the point."
  "d"
  (lambda (mark)
    (lisp-indent-sexp mark)))

(define-command insert-parentheses
  "Put parentheses around next ARG sexps.  Leave point after open-paren.
No argument is equivalent to zero: just insert () and leave point between."
  "P"
  (lambda (argument)
    (if argument
	(set-current-point! (skip-chars-forward " \t"))
	(or (group-start? (current-point))
	    (memv (char->syntax-code standard-char-syntax-table
				     (mark-left-char (current-point)))
		  '(#\\ #\> #\( #\space #\.))
	    (insert-char #\space)))
    (insert-char #\()
    (let ((mark (mark-right-inserting-copy (current-point))))
      (insert-char #\)
		   (if (and argument
			    (exact-nonnegative-integer? argument))
		       (forward-sexp (current-point) argument 'LIMIT)
		       (current-point)))
      (or argument
	  (group-end? (current-point))
	  (memv (char->syntax-code standard-char-syntax-table
				   (mark-right-char (current-point)))
		'(#\\ #\> #\( #\) #\space))
	  (insert-char #\space))
      (set-current-point! mark))))

(define-command move-past-close-and-reindent
  "Move past next ), delete indentation before it, then indent after it."
  ()
  (lambda ()
    ((ref-command up-list) 1)
    ((ref-command backward-char) 1)
    (do () ((not (within-indentation? (current-point))))
      ((ref-command delete-indentation) #f))
    ((ref-command forward-char) 1)
    ((ref-command newline-and-indent))))

;;;; Motion Covers

(define forward-sexp)
(define backward-sexp)
(make-motion-pair forward-one-sexp backward-one-sexp
  (lambda (f b)
    (set! forward-sexp f)
    (set! backward-sexp b)
    unspecific))

(define forward-list)
(define backward-list)
(make-motion-pair forward-one-list backward-one-list
  (lambda (f b)
    (set! forward-list f)
    (set! backward-list b)
    unspecific))

(define forward-down-list)
(define backward-down-list)
(make-motion-pair forward-down-one-list backward-down-one-list
  (lambda (f b)
    (set! forward-down-list f)
    (set! backward-down-list b)
    unspecific))

(define forward-up-list)
(define backward-up-list)
(make-motion-pair forward-up-one-list backward-up-one-list
  (lambda (f b)
    (set! forward-up-list f)
    (set! backward-up-list b)
    unspecific))

(define forward-definition-start)
(define backward-definition-start)
(make-motion-pair forward-one-definition-start backward-one-definition-start
  (lambda (f b)
    (set! forward-definition-start f)
    (set! backward-definition-start b)
    unspecific))

(define forward-definition-end)
(define backward-definition-end)
(make-motion-pair forward-one-definition-end backward-one-definition-end
  (lambda (f b)
    (set! forward-definition-end f)
    (set! backward-definition-end b)
    unspecific))
