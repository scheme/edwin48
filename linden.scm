;;; -*-Scheme-*-
;;;
;;; $Id: linden.scm,v 1.134 2007/01/05 21:19:23 cph Exp $
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

;;;; Lisp Indentation


(define-variable lisp-indent-offset
  "If not false, the number of extra columns to indent a subform."
  #f
  (lambda (object) (or (not object) (exact-integer? object))))

(define-variable lisp-indent-hook
  "If not false, a procedure for modifying lisp indentation."
  #f
  (lambda (object) (or (not object) (procedure? object))))

(define-variable lisp-indent-methods
  "String table identifying special forms for lisp indentation."
  #f
  (lambda (object) (or (not object) (string-table? object))))

(define-variable lisp-indent-regexps
  "Association list specifying (REGEXP . METHOD) indentation pairs.
The first element of the list is a symbol.
The remaining elements of the list are the indentation pairs.
Each REGEXP is matched against the keyword of the form being indented.
If a match is found, the METHOD associated with the first matching REGEXP
is used to calculate the indentation for that form."
  '(LISP-INDENT-REGEXPS)
  (lambda (object)
    (and (pair? object)
	 (symbol? (car object))
	 (alist? (cdr object))
	 (for-all? (cdr object) (lambda (entry) (string? (car entry)))))))

(define-variable lisp-body-indent
  "Number of extra columns to indent the body of a special form."
  2
  exact-nonnegative-integer?)

;;; CALCULATE-LISP-INDENTATION returns either an integer, which is the
;;; column to indent to, or a pair.  In the latter case this means
;;; that subsequent forms in the same expression may not be indented
;;; the same way; so the car is the indentation, and the cdr is a mark
;;; pointing at the beginning of the containing expression.  Typically
;;; this is passed back in as PARSE-START to speed up the indentation
;;; of many forms at once.

(define (calculate-lisp-indentation mark #!optional parse-start)
  (find-outer-container (if (default-object? parse-start)
			    (or (backward-one-definition-start mark)
				(group-start mark))
			    parse-start)
			(line-start mark 0)))

(define (find-outer-container start indent-point)
  (let ((state (parse-partial-sexp start indent-point 0)))
    (if (mark= (parse-state-location state) indent-point)
	(find-inner-container state false false indent-point)
	(find-outer-container (parse-state-location state) indent-point))))

(define (find-inner-container state container last-sexp indent-point)
  (if (<= (parse-state-depth state) 0)
      (simple-indent state container last-sexp indent-point)
      (let ((container (parse-state-containing-sexp state))
	    (last-sexp (parse-state-last-sexp state)))
	(let ((after-opener (mark1+ container)))
	  (if (and last-sexp (mark> last-sexp after-opener))
	      (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
		(if (not (parse-state-containing-sexp peek))
		    (simple-indent state container last-sexp indent-point)
		    (find-inner-container peek container last-sexp
					  indent-point)))
	      (simple-indent state container last-sexp indent-point))))))

(define (simple-indent state container last-sexp indent-point)
  (cond ((parse-state-in-string? state)
	 (mark-column (horizontal-space-end indent-point)))
	((and (integer? (ref-variable lisp-indent-offset indent-point))
	      container)
	 (+ (ref-variable lisp-indent-offset indent-point)
	    (mark-column container)))
	((positive? (parse-state-depth state))
	 (if (not last-sexp)
	     (mark-column (mark1+ container))
	     (normal-indent state container last-sexp indent-point)))
	(else
	 (mark-column (parse-state-location state)))))

;;; The following are true when the indent hook is called:
;;;
;;; * CONTAINER < NORMAL-INDENT <= LAST-SEXP < INDENT-POINT
;;; * Since INDENT-POINT is a line start, LAST-SEXP is on a
;;;   line previous to that line.
;;; * NORMAL-INDENT is at the start of an expression.

(define (normal-indent state container last-sexp indent-point)
  (let ((first-sexp (forward-to-sexp-start (mark1+ container) last-sexp)))
    (let ((normal-indent
	   (if (mark> (line-end container 0) last-sexp)
	       ;; CONTAINER and LAST-SEXP are on same line.
	       ;; If FIRST-SEXP = LAST-SEXP, indent under that, else
	       ;; indent under the second expression on that line.
	       (if (mark= first-sexp last-sexp)
		   last-sexp
		   (forward-to-sexp-start (forward-one-sexp first-sexp)
					  last-sexp))
	       ;; LAST-SEXP is on subsequent line -- indent under the
	       ;; first expression on that line.
	       (forward-to-sexp-start (line-start last-sexp 0) last-sexp))))
      (if (char=? #\(
		  (char->syntax-code (ref-variable syntax-table indent-point)
				     (mark-right-char first-sexp)))
	  ;; The first expression is a list -- don't bother to call
	  ;; the indent hook.
	  (mark-column (backward-prefix-chars normal-indent))
	  (let ((normal-indent (backward-prefix-chars normal-indent)))
	    (or (let ((hook (ref-variable lisp-indent-hook indent-point)))
		  (and hook
		       (hook state indent-point normal-indent)))
		(mark-column normal-indent)))))))

;;;; Indent Hook

;;; Look at the first expression in the containing expression, and if
;;; it is an atom, look it up in the Lisp Indent Methods table.  Three
;;; types of entry are recognized:
;;;
;;; 'DEFINITION means treat this form as a definition.
;;; <n> means treat this form as a special form.
;;; Otherwise, the entry must be a procedure, which is called.

(define (standard-lisp-indent-hook state indent-point normal-indent)
  (let ((first-sexp
	 (forward-to-sexp-start (mark1+ (parse-state-containing-sexp state))
				indent-point)))
    (and (let ((syntax
		(char->syntax-code (ref-variable syntax-table indent-point)
				   (mark-right-char first-sexp))))
	   (or (char=? #\w syntax)
	       (char=? #\_ syntax)))
	 (let ((end (forward-one-sexp first-sexp)))
	   (let ((method (find-indent-method first-sexp end)))
	     (cond ((eq? method 'DEFINITION)
		    (lisp-indent-definition state indent-point normal-indent))
		   ((exact-integer? method)
		    (lisp-indent-special-form method state indent-point
					      normal-indent))
		   ((procedure-of-arity? method 3)
		    (method state indent-point normal-indent))
		   (else #f)))))))

(define (find-indent-method start end)
  (let ((name (extract-string start end)))
    (or (let ((v (name->variable (symbol 'LISP-INDENT/ name) #f)))
	  (and v
	       (variable-local-value start v)))
	(let ((methods (ref-variable lisp-indent-methods start)))
	  (and methods
	       (string-table-get methods name)))
	(let loop ((alist (cdr (ref-variable lisp-indent-regexps start))))
	  (and (pair? alist)
	       (if (re-match-forward (caar alist) start end #t)
		   (cdar alist)
		   (loop (cdr alist))))))))

;;; Indent the first subform in a definition at the body indent.
;;; Indent subsequent subforms normally.

(define (lisp-indent-definition state indent-point normal-indent)
  normal-indent		;ignore
  (let ((container (parse-state-containing-sexp state)))
    (and (mark> (line-end container 0) (parse-state-last-sexp state))
	 (+ (ref-variable lisp-body-indent indent-point)
	    (mark-column container)))))

;;; Indent the first N subforms normally, but then indent the
;;; remaining forms at the body-indent.  If this is one of the first
;;; N, a cons is returned, the cdr of which is CONTAINING-SEXP.  This
;;; is to speed up indentation of successive forms.

(define (lisp-indent-special-form n state indent-point normal-indent)
  (if (negative? n) (error "Special form indent hook negative" n))
  (let ((container (parse-state-containing-sexp state)))
    (let ((body-indent
	   (+ (mark-column container)
	      (ref-variable lisp-body-indent indent-point)))
	  (normal-indent (mark-column normal-indent)))
      (let loop ((count n) (mark (mark1+ container)))
	(let ((mark
	       (let ((mark (forward-one-sexp mark indent-point)))
		 (and mark
		      (forward-to-sexp-start mark indent-point)))))
	  (cond ((and mark (mark< mark indent-point))
		 (loop (-1+ count) mark))
		((positive? count)
		 (cons (+ body-indent
			  (ref-variable lisp-body-indent indent-point))
		       (mark-permanent! container)))
		((and (zero? count)
		      (or (zero? n)
			  (<= body-indent normal-indent)))
		 body-indent)
		(else normal-indent)))))))

;;;; Indent Line

(define (lisp-indent-line whole-sexp?)
  (let ((start (indentation-end (current-point))))
    (if (not (match-forward ";;;" start))
	(let ((indentation
	       (let ((indent (calculate-lisp-indentation start)))
		 (if (pair? indent)
		     (car indent)
		     indent))))
	  (let ((shift-amount (- indentation (mark-column start))))
	    (cond ((not (zero? shift-amount))
		   (if whole-sexp?
		       (mark-permanent! start))
		   (change-indentation indentation start)
		   (if whole-sexp?
		       (indent-code-rigidly start
					    (forward-sexp start 1 'ERROR)
					    shift-amount
					    false)))
		  ((within-indentation? (current-point))
		   (set-current-point! start))))))))

(define (indent-code-rigidly start end shift-amount nochange-regexp)
  (let ((end (mark-left-inserting end)))
    (let loop ((start start) (state false))
      (let ((start* (line-start start 1 'LIMIT)))
	(if (mark< start* end)
	    (let ((start start*)
		  (state (parse-partial-sexp start start* false false state)))
	      (if (not (or (parse-state-in-string? state)
			   (parse-state-in-comment? state)
			   (and nochange-regexp
				(re-match-forward nochange-regexp start))))
		  (let ((start (horizontal-space-end start)))
		    (cond ((line-end? start)
			   (delete-horizontal-space start))
			  ((not (match-forward ";;;" start))
			   (change-indentation (max 0
						    (+ (mark-column start)
						       shift-amount))
					       start)))))
	      (loop start state)))))))

;;;; Indent Comment

(define (lisp-comment-locate mark)
  (and (re-search-forward "\\(#;\\|;+\\)[ \t]*" mark (line-end mark 0))
       (cons (re-match-start 0) (re-match-end 0))))

(define (lisp-comment-indentation mark #!optional stack)
  (let ((column
	 (cond ((match-forward ";;;" mark)
		0)
	       ((or (match-forward ";;" mark)
		    (match-forward "#;" mark))
		(compute-indentation mark
				     (if (default-object? stack) '() stack)))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ 1 (mark-column (horizontal-space-start mark)))
	     column))))

;;;; Indent Expression

(define (lisp-indent-sexp point)
  (let ((end (mark-permanent! (line-start (forward-sexp point 1 'ERROR) 0))))
    (if (mark< point end)
	(let loop ((index point) (stack '()))
	  (let next-line-start ((index index) (state false))
	    (let ((start (mark-right-inserting-copy (line-start index 1))))
	      (let ((state (parse-partial-sexp index start false false state)))
		(let ((stack (adjust-stack (parse-state-depth state) stack)))
		  (cond ((mark= start end)
			 (if (not (or (parse-state-in-string? state)
				      (parse-state-in-comment? state)))
			     (indent-expression-line start stack state))
			 (mark-temporary! start))
			((or (parse-state-in-string? state)
			     (parse-state-in-comment? state))
			 (mark-temporary! start)
			 (next-line-start start state))
			(else
			 (if (line-blank? start)
			     (delete-horizontal-space start)
			     (indent-expression-line start stack state))
			 (mark-temporary! start)
			 (loop start stack)))))))))))

(define (indent-expression-line start stack state)
  (maybe-change-indentation (compute-indentation start stack) start)
  (let ((state (parse-partial-sexp start (line-end start 0) #f #f state)))
    (if (parse-state-in-comment? state)
	(let ((comment-start (parse-state-comment-start state)))
	  (if (match-forward ";" comment-start)
	      (maybe-change-column (lisp-comment-indentation comment-start
							     stack)
				   comment-start))))))

(define (compute-indentation start stack)
  (cond ((not (pair? stack))
	 (let ((indent (calculate-lisp-indentation start)))
	   (if (pair? indent)
	       (car indent)
	       indent)))
	((and (car stack)
	      (integer? (car stack)))
	 (car stack))
	(else
	 (let ((indent
		(calculate-lisp-indentation
		 start
		 (or (car stack)
		     (backward-one-definition-start start)
		     (group-start start)))))
	   (if (pair? indent)
	       (begin
		 (set-car! stack (cdr indent))
		 (car indent))
	       (begin
		 (set-car! stack indent)
		 indent))))))

(define (adjust-stack depth-delta stack)
  (cond ((zero? depth-delta) stack)
	((positive? depth-delta) (up-stack depth-delta stack))
	(else (down-stack depth-delta stack))))

(define (down-stack n stack)
  (if (= -1 n) (cdr stack) (down-stack (1+ n) (cdr stack))))

(define (up-stack n stack)
  (if (= 1 n) (cons false stack) (up-stack (-1+ n) (cons false stack))))
