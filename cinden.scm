#| -*-Scheme-*-

$Id: cinden.scm,v 1.25 2007/01/05 21:19:23 cph Exp $

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

;;;; C Indentation (from GNU Emacs)


(define-variable c-indent-level
  "Indentation of C statements with respect to containing block."
  2
  exact-integer?)

(define-variable c-brace-imaginary-offset
  "Imagined indentation of a C open brace that actually follows a statement."
  0
  exact-integer?)

(define-variable c-brace-offset
  "Extra indentation for braces, compared with other text in same context."
  0
  exact-integer?)

(define-variable c-argdecl-indent
  "Indentation level of declarations of C function arguments."
  5
  exact-integer?)

(define-variable c-label-offset
  "Offset of C label lines and case statements relative to usual indentation."
  -2
  exact-integer?)

(define-variable c-continued-statement-offset
  "Extra indent for lines not starting new statements."
  2
  exact-integer?)

(define-variable c-continued-brace-offset
  "Extra indent for substatements that start with open-braces.
This is in addition to c-continued-statement-offset."
  0
  exact-integer?)

(define (c-indent-line start)
  (let ((old-indentation (mark-indentation start))
	(new-indentation (c-compute-indentation start)))
    (if (not (fix:= new-indentation old-indentation))
	(change-indentation new-indentation start))
    (- new-indentation old-indentation)))

(define (c-compute-indentation start)
  (let ((start (line-start start 0)))
    (let ((indentation (calculate-indentation start #f)))
      (if (exact-integer? indentation)
	  (if (char-match-forward #\# start)
	      0
	      (indent-line:adjust-indentation (horizontal-space-end start)
					      indentation))
	  (nonstandard-indentation start indentation)))))

(define (nonstandard-indentation start state)
  (cond ((parse-state-in-string? state)
	 (mark-indentation start))
	((parse-state-in-comment? state)
	 (mark-column
	  (let ((comment-start (parse-state-comment-start state)))
	    (if (and (char-match-forward #\* (indentation-end start))
		     (match-forward "/*" comment-start))
		(mark1+ comment-start)
		(or (re-match-forward "/\\(/+\\|\\*+\\)[ \t]*"
				      comment-start)
		    comment-start)))))
	(else
	 (error "Illegal non-standard indentation:" state))))

(define (indent-line:adjust-indentation start indentation)
  (cond ((or (looking-at-keyword? "case" start)
	     (and (re-match-forward "[A-Za-z]" start)
		  (char-match-forward #\: (forward-sexp start 1 'LIMIT))))
	 (max 1 (+ indentation (ref-variable c-label-offset start))))
	((looking-at-keyword? "else" start)
	 (mark-indentation
	  (backward-to-start-of-if
	   start
	   (backward-definition-start start 1 'LIMIT))))
	((char-match-forward #\} start)
	 (max 0 (- indentation (ref-variable c-indent-level start))))
	((char-match-forward #\{ start)
	 (max 0 (+ indentation (ref-variable c-brace-offset start))))
	(else (max 0 indentation))))

(define (calculate-indentation mark parse-start)
  (let ((indent-point (line-start mark 0)))
    (let ((state
	   (let loop
	       ((start
		 (or parse-start (backward-definition-start mark 1 'LIMIT))))
	     (let ((state (parse-partial-sexp start indent-point 0)))
	       (if (mark= (parse-state-location state) indent-point)
		   state
		   (loop (parse-state-location state)))))))
      (let ((container (parse-state-containing-sexp state)))
	(cond ((or (parse-state-in-string? state)
		   (parse-state-in-comment? state))
	       state)
	      ((not container)
	       (calculate-indentation:top-level indent-point
						(or parse-start
						    (group-start mark))))
	      ((char-match-forward #\{ container)
	       (calculate-indentation:statement indent-point container))
	      (else
	       ;; Line is expression, not statement: indent to just
	       ;; after the surrounding open.
	       (mark-column (mark1+ container))))))))

(define (calculate-indentation:top-level indent-point parse-start)
  (if (or (re-match-forward "[ \t]*{" indent-point)
	  (function-start? indent-point parse-start))
      ;; This appears to be the start of a function or a function
      ;; body, so indent in the left-hand column.
      0
      (let ((m (backward-to-noncomment indent-point parse-start)))
	(if (or (not m) (not (extract-left-char m)))
	    ;; This appears to be the beginning of a top-level data
	    ;; definition, so indent in the left-hand column.
	    0
	    ;; Look at the previous left-justified line to determine
	    ;; if we're in a data definition or a function preamble.
	    (let ((m (re-search-backward "^[^ \t\n\f#]" m)))
	      (if (not m)
		  0
		  (let ((m (function-start? m parse-start)))
		    (if (and m (mark< m indent-point))
			;; Previous line is a function start and we're
			;; indenting a line that follows the parameter
			;; list and precedes the body, so indent it as
			;; a parameter declaration.
			(ref-variable c-argdecl-indent indent-point)
			0))))))))

(define (function-start? lstart parse-start)
  ;; True iff LSTART points at the beginning of a function definition.
  ;; If true, returns a mark pointing to the end of the function's
  ;; parameter list.
  (and (re-match-forward "\\sw\\|\\s_" lstart)
       (re-match-forward "[^\"\n=(]*(" lstart)
       (let ((open (mark-1+ (re-match-end 0))))
	 ;; OPEN looks like the start of a parameter list.  If it
	 ;; doesn't end in comma or semicolon, and furthermore isn't
	 ;; in a comment, we conclude that it is a parameter list, and
	 ;; consequently that we're looking at a function.
	 (and (not (parse-state-in-comment?
		    (parse-partial-sexp parse-start open)))
	      (let ((m (forward-sexp open 1)))
		(and m
		     (let ((m (skip-chars-forward " \t\f" m)))
		       (and (not (re-match-forward "[,;]" m))
			    m))))))))

(define (calculate-indentation:statement indent-point container)
  (let ((mark
	 (let loop ((start indent-point))
	   (let ((m (backward-to-noncomment start container)))
	     ;; Back up over label lines, since they don't affect
	     ;; whether our line is a continuation.
	     (case (and m (mark> m container) (extract-left-char m))
	       ((#\:)
		(if (let ((m (backward-to-noncomment (mark-1+ m) container)))
		      (and m
			   (mark> m container)
			   (re-match-forward "'\\|\\sw\\|\\s_" (mark-1+ m))))
		    (loop (line-start m 0))
		    m))
	       ((#\,)
		(let ((ls (line-start m 0)))
		  (if (mark<= ls container)
		      #f
		      (loop (horizontal-space-end ls)))))
	       (else m))))))
    (if (and mark (not (memv (extract-left-char mark) '(#F #\, #\; #\{ #\}))))
	;; This line is continuation of preceding line's statement;
	;; indent c-continued-statement-offset more than the previous
	;; line of the statement.
	(+ (ref-variable c-continued-statement-offset indent-point)
	   (mark-column (backward-to-start-of-continued-exp mark container))
	   (if (re-match-forward "[ \t]*{" indent-point)
	       (ref-variable c-continued-brace-offset indent-point)
	       0))
	(or (skip-comments&labels (mark1+ container) indent-point)
	    ;; If no previous statement, indent it relative to line
	    ;; brace is on.  For open brace in column zero, don't let
	    ;; statement start there too.  If c-indent-level is zero,
	    ;; use c-brace-offset + c-continued-statement-offset
	    ;; instead.  For open-braces not the first thing in a
	    ;; line, add in c-brace-imaginary-offset.
	    (+ (if (and (line-start? container)
			(= 0 (ref-variable c-indent-level indent-point)))
		   (+ (ref-variable c-brace-offset indent-point)
		      (ref-variable c-continued-statement-offset indent-point))
		   (ref-variable c-indent-level indent-point))
	       (let ((container (skip-chars-backward " \t" container)))
		 ;; If open brace is not the first non-white thing on
		 ;; the line, add the c-brace-imaginary-offset.
		 (+ (if (line-start? container)
			0
			(ref-variable c-brace-imaginary-offset indent-point))
		    (mark-indentation
		     ;; If the open brace is preceded by a
		     ;; parenthesized expression, move to the
		     ;; beginning of that; possibly a different line.
		     (if (eqv? #\) (extract-left-char container))
			 (backward-sexp container 1 'LIMIT)
			 container)))))))))

(define (skip-comments&labels start end)
  (let ((gend (group-end start)))
    (let loop ((mark start) (colon-line-end false))
      (let ((mark (whitespace-end mark gend)))
	(cond ((mark>= mark end)
	       false)
	      ((or (char-match-forward #\# mark)
		   (match-forward "//" mark))
	       (loop (line-start mark 1 'LIMIT) colon-line-end))
	      ((match-forward "/*" mark)
	       (loop (or (search-forward "*/" mark gend) gend) colon-line-end))
	      ((re-match-forward "case[ \t\n].*:\\|[a-zA-Z0-9_$]*[ \t\n]*:"
				 mark gend false)
	       (loop (re-match-end 0) (line-end mark 0)))
	      ((and colon-line-end (mark> colon-line-end mark))
	       (- (mark-indentation mark)
		  (ref-variable c-label-offset mark)))
	      (else
	       (mark-column mark)))))))

(define (c-indent-expression expression-start)
  (let ((end
	 (mark-left-inserting-copy
	  (line-start (forward-sexp expression-start 1 'ERROR) 0))))
    (if (mark< expression-start end)
	(let loop
	    ((start expression-start)
	     (state false)
	     (indent-stack (list false))
	     (contain-stack (list expression-start))
	     (last-depth 0))
	  (call-with-values
	      (lambda () (c-indent-expression:parse-line start end state))
	    (lambda (start state)
	      (let* ((depth (parse-state-depth state))
		     (depth-delta (- depth last-depth))
		     (indent-stack (adjust-stack depth-delta indent-stack))
		     (contain-stack (adjust-stack depth-delta contain-stack))
		     (indent-line
		      (lambda ()
			(if (not (line-blank? start))
			    (c-indent-expression:per-line
			     (skip-chars-forward " \t" start)
			     expression-start
			     indent-stack
			     contain-stack)))))
		(if (not (car contain-stack))
		    (set-car! contain-stack
			      (or (parse-state-containing-sexp state)
				  (backward-sexp start 1 'LIMIT))))
		(if (mark= start end)
		    (begin
		      (mark-temporary! end)
		      (indent-line))
		    (begin
		      (indent-line)
		      (loop start state indent-stack contain-stack
			    depth))))))))))

(define (c-indent-expression:parse-line start end state)
  (let loop ((start start) (state state))
    (if (and state (parse-state-in-comment? state))
	(c-indent-line start))
    (let ((start* (line-start start 1)))
      (let ((state* (parse-partial-sexp start start* false false state)))
	(cond ((mark= start* end)
	       (values start* state*))
	      ((parse-state-in-comment? state*)
	       (if (not (and state (parse-state-in-comment? state)))
		   (c-comment-indent (parse-state-comment-start state*)))
	       (loop start* state*))
	      ((parse-state-in-string? state*)
	       (loop start* state*))
	      (else
	       (values start* state*)))))))

(define (c-indent-expression:per-line start expression-start
				      indent-stack contain-stack)
  (let ((indentation
	 (indent-line:adjust-indentation
	  start
	  (cond ((not (car indent-stack))
		 (let ((indentation
			(let ((indentation (calculate-indentation start #f)))
			  (if (exact-integer? indentation)
			      indentation
			      (nonstandard-indentation start indentation)))))
		   (set-car! indent-stack indentation)
		   indentation))
		((not (char-match-forward #\{ (car contain-stack)))
		 (car indent-stack))
		(else
		 ;; Line is at statement level.  Is it a new
		 ;; statement?  Is it an else?  Find last non-comment
		 ;; character before this line.
		 (let ((mark (backward-to-noncomment start expression-start)))
		   (cond ((not (memv (extract-left-char mark)
				     '(#F #\, #\; #\} #\: #\{)))
			  (+ (ref-variable c-continued-statement-offset mark)
			     (mark-column
			      (backward-to-start-of-continued-exp
			       mark
			       (car contain-stack)))
			     (if (char-match-forward #\{ start)
				 (ref-variable c-continued-brace-offset mark)
				 0)))
			 ((looking-at-keyword? "else" start)
			  (mark-indentation
			   (backward-to-start-of-if mark expression-start)))
			 (else
			  (car indent-stack)))))))))
    (if (not (char-match-forward #\# start))
	(maybe-change-indentation indentation start))))

(define (whitespace-start start end)
  (skip-chars-backward " \t\n" start end))

(define (whitespace-end start end)
  (skip-chars-forward " \t\n" start end))

(define (looking-at-keyword? keyword start)
  (let ((end (line-end start 0)))
    (and (re-match-forward (string-append keyword "\\b") start end false)
	 (not (re-match-forward (string-append keyword "\\s_") start end
				false)))))

(define (backward-to-noncomment start end)
  (let loop ((start start))
    (let ((mark
	   (let ((mark (whitespace-start start end)))
	     (or (match-backward "*/" mark)
		 mark))))
      (let ((state (parse-partial-sexp end mark)))
	(if (parse-state-in-comment? state)
	    (loop (parse-state-comment-start state))
	    (let ((mark* (indentation-end mark)))
	      (cond ((not (char-match-forward #\# mark*)) mark)
		    ((mark<= mark* end) mark*)
		    (else (loop mark*)))))))))

(define (backward-to-start-of-continued-exp start end)
  (let ((mark
	 (line-start (if (char-match-backward #\) start)
			 (backward-sexp start 1 'LIMIT)
			 start)
		     0)))
    (horizontal-space-end (if (mark<= mark end) (mark1+ end) mark))))

(define (backward-to-start-of-if start end)
  (let loop ((mark start) (if-level 1))
    (let ((mark (backward-sexp mark 1 'LIMIT)))
      (let ((adjust-level
	     (lambda (if-level)
	       (if (= if-level 0)
		   mark
		   (loop mark if-level)))))
	(cond ((looking-at-keyword? "else" mark)
	       (adjust-level (+ if-level 1)))
	      ((looking-at-keyword? "if" mark)
	       (adjust-level (- if-level 1)))
	      ((mark>= mark end)
	       (adjust-level if-level))
	      (else
	       end))))))

(define (adjust-stack depth-delta stack)
  (cond ((= depth-delta 0)
	 stack)
	((> depth-delta 0)
	 (let loop ((depth-delta depth-delta) (stack stack))
	   (if (= 1 depth-delta)
	       (cons false stack)
	       (loop (- depth-delta 1) (cons false stack)))))
	(else
	 (let loop ((depth-delta depth-delta) (stack stack))
	   (if (= -1 depth-delta)
	       (cdr stack)
	       (loop (+ depth-delta 1) (cdr stack)))))))