;;; -*-Scheme-*-
;;;
;;; $Id: c-mode.scm,v 1.65 2007/01/05 21:19:23 cph Exp $
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

;;;; C Mode (from GNU Emacs)


(define-major-mode c fundamental "C"
  "Major mode for editing C code.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
 c-tab-always-indent
    True means TAB in C mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 c-auto-newline
    True means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-continued-brace-offset
    Extra indent for substatements that start with open-braces.
    This is in addition to c-continued-statement-offset.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default.

Settings for K&R and BSD indentation styles are
  c-indent-level                5    8
  c-continued-statement-offset  5    8
  c-brace-offset               -5   -8
  c-argdecl-indent              0    8
  c-label-offset               -5   -8

\\{c}"
  (lambda (buffer)
    (local-set-variable! syntax-table c-syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #t buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! paragraph-ignore-fill-prefix #t buffer)
    (local-set-variable! indent-line-procedure (ref-command c-indent-command)
			 buffer)
    (local-set-variable! require-final-newline #t buffer)
    (local-set-variable! comment-start "/* " buffer)
    (local-set-variable! comment-end " */" buffer)
    (local-set-variable! comment-column 32 buffer)
    (local-set-variable! comment-locator-hook c-comment-locate buffer)
    (local-set-variable! comment-indent-hook c-comment-indent buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable c-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable c-mode-hook buffer) buffer)))

(define-command c-mode
  "Enter C mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object c))))

(define-variable c-mode-abbrev-table
  "Mode-specific abbrev table for C code.")
(define-abbrev-table 'c-mode-abbrev-table '())

(define-variable c-mode-hook
  "An event distributor that is invoked when entering C mode."
  (make-event-distributor))

(define c-syntax-table
  (let ((syntax-table (make-char-syntax-table)))
    (for-each (lambda (char) (set-char-syntax! syntax-table char "."))
	      (string->list "+-=%<>&|"))
    (set-char-syntax! syntax-table #\' "\"")
    (set-char-syntax! syntax-table #\\ "\\")
    (set-char-syntax! syntax-table #\/ ". 14")
    (set-char-syntax! syntax-table #\* ". 23")
    syntax-table))

(define (c-comment-locate start)
  (and (re-search-forward "/\\*+ *" start (line-end start 0))
       (cons (re-match-start 0) (re-match-end 0))))

(define (c-comment-indent start)
  (if (re-match-forward "^/\\*" start (line-end start 0))
      0
      (max (+ (mark-column (horizontal-space-start start)) 1)
	   (ref-variable comment-column start))))

(define-variable c-auto-newline
  "True means automatically newline before and after braces,
and after colons and semicolons, inserted in C code."
  #f
  boolean?)

(define-variable c-tab-always-indent
  "True means TAB in C mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  #t
  boolean?)

(define-key 'c #\linefeed 'reindent-then-newline-and-indent)
(define-key 'c #\) 'lisp-insert-paren)
(define-key 'c #\{ 'electric-c-brace)
(define-key 'c #\} 'electric-c-brace)
(define-key 'c #\; 'electric-c-semi)
(define-key 'c #\: 'electric-c-terminator)
(define-key 'c #\c-m-h 'mark-c-procedure)
(define-key 'c #\c-m-q 'indent-c-exp)
(define-key 'c #\rubout 'backward-delete-char-untabify)
(define-key 'c #\tab 'c-indent-command)

(define-command electric-c-brace
  "Insert character and correct line's indentation."
  "P"
  (lambda (argument)
    (let ((point (current-point))
	  (char (last-command-key)))
      (if (and (not argument)
	       (line-end? point)
	       (or (line-blank? point)
		   (and (ref-variable c-auto-newline)
			(begin
			  ((ref-command c-indent-command) #f)
			  (insert-newline)
			  #t))))
	  (begin
	    (insert-char char)
	    ((ref-command c-indent-command) #f)
	    (if (ref-variable c-auto-newline)
		(begin
		  (insert-newline)
		  ((ref-command c-indent-command) #f))))
	  ((ref-command self-insert-command) #f))
      (if (eqv? #\} char)
	  (mark-flash (backward-one-sexp (current-point)) 'RIGHT)))))

(define-command electric-c-semi
  "Insert character and correct line's indentation."
  "P"
  (lambda (argument)
    (if (ref-variable c-auto-newline)
	((ref-command electric-c-terminator) argument)
	((ref-command self-insert-command) argument))))

(define-command electric-c-terminator
  "Insert character and correct line's indentation."
  "P"
  (lambda (argument)
    (let ((point (current-point))
	  (char (last-command-key)))
      (if (and (not argument)
	       (line-end? point)
	       (not (let ((mark (indentation-end point)))
		      (or (char-match-forward #\# mark)
			  ;; Colon is special only after a label, or
			  ;; case.  So quickly rule out most other
			  ;; uses of colon and do no indentation for
			  ;; them.
			  (and (eqv? #\: char)
			       (not (re-match-forward "case\\b"
						      mark
						      (line-end mark 0)
						      #f))
			       (mark< (skip-chars-forward
				       " \t"
				       (skip-chars-forward "a-zA-Z0-9_$" mark))
				      point))
			  (let ((state
				 (parse-partial-sexp
				  (backward-definition-start point 1 'LIMIT)
				  point)))
			    (or (parse-state-in-string? state)
				(parse-state-in-comment? state)
				(parse-state-quoted? state)))))))
	  (begin
	    (insert-char char)
	    ((ref-command c-indent-command) #f)
	    (if (and (ref-variable c-auto-newline)
		     (not (c-inside-parens? point)))
		(begin
		  (insert-newline)
		  ((ref-command c-indent-command) #f))))
	  ((ref-command self-insert-command) argument)))))

(define (c-inside-parens? mark)
  (let ((container (backward-up-list mark 1 #f)))
    (and container
	 (mark>= container (backward-definition-start mark 1 'LIMIT))
	 (char-match-forward #\( container))))

(define-command mark-c-procedure
  "Put mark at end of C procedure, point at beginning."
  ()
  (lambda ()
    (push-current-mark! (current-point))
    (let ((end (forward-definition-end (current-point) 1 'LIMIT)))
      (push-current-mark! end)
      (set-current-point!
       (backward-paragraph (backward-definition-start end 1 'LIMIT)
			   1
			   'LIMIT)))))

(define-command c-indent-command
  "Indent current line as C code, or in some cases insert a tab character.
If c-tab-always-indent is true (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  "P"
  (lambda (#!optional argument)
    (let ((point (current-point)))
      (cond ((and (not (default-object? argument)) argument)
	     (let ((shift-amount (c-indent-line point))
		   (start
		    (if (ref-variable c-tab-always-indent)
			(line-start point 0)
			point)))
	       (indent-code-rigidly start
				    (forward-sexp start 1 'ERROR)
				    shift-amount
				    "#")))
	    ((or (ref-variable c-tab-always-indent)
		 (within-indentation? point))
	     (c-indent-line point)
	     (if (within-indentation? point)
		 (set-current-point! (indentation-end point))))
	    (else
	     ((ref-command insert-tab)))))))

(define-command indent-c-exp
  "Indent each line of the C grouping following point."
  ()
  (lambda ()
    (c-indent-expression (current-point))))
