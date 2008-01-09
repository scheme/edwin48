#| -*-Scheme-*-

$Id: javamode.scm,v 1.16 2007/01/05 21:19:23 cph Exp $

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

;;;; Major Mode for Java Programs

;;; This isn't a very good mode for Java, but it is good enough for
;;; some purposes and it was quickly implemented.  The major flaw is
;;; that it indents the body of a definition, such as a method or
;;; nested class, exactly the same as a continued statement.  The only
;;; way to treat these cases differently is to do more sophisticated
;;; parsing that recognizes that the contexts are different.  This
;;; could be done using the keyparser, but that would be much more
;;; work than this was.


(define-major-mode java c "Java"
  "Major mode for editing Java code.
This is just like C mode, except that
  (1) comments begin with // and end at the end of line, and
  (2) c-continued-brace-offset defaults to -2 instead of 0.

\\{java}"
  (lambda (buffer)
    (local-set-variable! syntax-table java-syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-locator-hook java-comment-locate buffer)
    (local-set-variable! comment-indent-hook java-comment-indentation buffer)
    (local-set-variable! comment-start "// " buffer)
    (local-set-variable! comment-end "" buffer)
    (local-set-variable! c-continued-brace-offset -2 buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable java-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable java-mode-hook buffer) buffer)))

(define-command java-mode
  "Enter Java mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object java))))

(define-variable java-mode-abbrev-table
  "Mode-specific abbrev table for Java code.")
(define-abbrev-table 'java-mode-abbrev-table '())

(define-variable java-mode-hook
  "An event distributor that is invoked when entering Java mode."
  (make-event-distributor))

(define java-syntax-table
  (let ((syntax-table (make-char-syntax-table c-syntax-table)))
    (set-char-syntax! syntax-table #\/ ". 1456")
    (set-char-syntax! syntax-table #\newline ">")
    syntax-table))

(define (java-comment-locate mark)
  (let ((state (parse-partial-sexp mark (line-end mark 0))))
    (and (parse-state-in-comment? state)
	 (re-match-forward "/\\(/+\\|\\*+\\)[ \t]*"
			   (parse-state-comment-start state))
	 (cons (re-match-start 0) (re-match-end 0)))))

(define (java-comment-indentation mark)
  (let ((column
	 (cond ((re-match-forward "^/\\*" mark)
		0)
	       ((and (match-forward "//" mark)
		     (within-indentation? mark))
		(c-compute-indentation mark))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ (mark-column (horizontal-space-start mark)) 1)
	     column))))

(define-major-mode php c "PHP"
  "Major mode for editing PHP code.
This is just like C mode, except that
  (1) comments begin with // and end at the end of line, and
  (2) $ is a symbol constituent rather than a word constituent.

\\{php}"
  (lambda (buffer)
    (local-set-variable! syntax-table php-syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-locator-hook java-comment-locate buffer)
    (local-set-variable! comment-indent-hook java-comment-indentation buffer)
    (local-set-variable! comment-start "// " buffer)
    (local-set-variable! comment-end "" buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable php-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable php-mode-hook buffer) buffer)))

(define-command PHP-mode
  "Enter PHP mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object php))))

(define-variable php-mode-abbrev-table
  "Mode-specific abbrev table for PHP code.")
(define-abbrev-table 'php-mode-abbrev-table '())

(define-variable php-mode-hook
  "An event distributor that is invoked when entering PHP mode."
  (make-event-distributor))

(define php-syntax-table
  (let ((syntax-table (make-char-syntax-table java-syntax-table)))
    (set-char-syntax! syntax-table #\$ ". p")
    syntax-table))