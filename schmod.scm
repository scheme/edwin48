#| -*-Scheme-*-

$Id: schmod.scm,v 1.76 2007/01/05 21:19:24 cph Exp $

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

;;;; Scheme Mode


(define-command scheme-mode
  "Enter Scheme mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object scheme))))

(define-major-mode scheme fundamental "Scheme"
  "Major mode specialized for editing Scheme code.
\\[lisp-indent-line] indents the current line for Scheme.
\\[indent-sexp] indents the next s-expression.
\\[scheme-complete-variable] completes the variable preceding point.
\\[show-parameter-list] shows the parameters of the call surrounding point.

The following commands evaluate Scheme expressions:

\\[eval-expression] reads and evaluates an expression in minibuffer.
\\[eval-last-sexp] evaluates the expression preceding point.
\\[eval-defun] evaluates the current definition.
\\[eval-current-buffer] evaluates the buffer.
\\[eval-region] evaluates the current region.

\\{scheme}"
  (lambda (buffer)
    (local-set-variable! syntax-table scheme-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! lisp-indent-hook standard-lisp-indent-hook buffer)
    (local-set-variable! lisp-indent-methods scheme-mode:indent-methods buffer)
    (local-set-variable! lisp-indent-regexps scheme-mode:indent-regexps buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook lisp-comment-locate buffer)
    (local-set-variable! comment-indent-hook lisp-comment-indentation buffer)
    (local-set-variable! comment-start ";" buffer)
    (local-set-variable! comment-end "" buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! paragraph-ignore-fill-prefix #t buffer)
    (local-set-variable! indent-line-procedure
			 (ref-command lisp-indent-line)
			 buffer)
    (local-set-variable! mode-line-process
			 '(RUN-LIGHT (": " RUN-LIGHT) "")
			 buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable scheme-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable scheme-mode-hook buffer) buffer)))

(define-variable scheme-mode-abbrev-table
  "Mode-specific abbrev table for Scheme code.")
(define-abbrev-table 'scheme-mode-abbrev-table '())

(define-variable scheme-mode-hook
  "An event distributor that is invoked when entering Scheme mode."
  (make-event-distributor))

(define-key 'scheme #\rubout 'backward-delete-char-untabify)
(define-key 'scheme #\tab 'lisp-indent-line)
(define-key 'scheme #\) 'lisp-insert-paren)
(define-key 'scheme #\m-A 'show-parameter-list)
(define-key 'scheme #\m-g 'undefined)
(define-key 'scheme #\m-o 'eval-current-buffer)
(define-key 'scheme #\m-q 'undefined)
(define-key 'scheme #\m-s 'step-defun)
(define-key 'scheme #\m-z 'eval-defun)
(define-key 'scheme #\c-m-q 'indent-sexp)
(define-key 'scheme #\c-m-z 'eval-region)
(define-key 'scheme #\m-tab 'scheme-complete-variable)
(define-key 'scheme '(#\c-c #\c-c) 'eval-abort-top-level)

;;;; Read Syntax

(define scheme-mode:syntax-table (make-char-syntax-table))

(set-char-syntax! scheme-mode:syntax-table #\tab " ")
(set-char-syntax! scheme-mode:syntax-table #\newline "> ")
(set-char-syntax! scheme-mode:syntax-table #\page " ")
(set-char-syntax! scheme-mode:syntax-table #\space " ")

(set-char-syntax! scheme-mode:syntax-table #\! "_")
(set-char-syntax! scheme-mode:syntax-table #\" "\" ")
(set-char-syntax! scheme-mode:syntax-table #\# "_ p14")
(set-char-syntax! scheme-mode:syntax-table #\$ "_")
(set-char-syntax! scheme-mode:syntax-table #\% "_")
(set-char-syntax! scheme-mode:syntax-table #\& "_")
(set-char-syntax! scheme-mode:syntax-table #\' "  p")
(set-char-syntax! scheme-mode:syntax-table #\( "()")
(set-char-syntax! scheme-mode:syntax-table #\) ")(")
(set-char-syntax! scheme-mode:syntax-table #\* "_")
(set-char-syntax! scheme-mode:syntax-table #\+ "_")
(set-char-syntax! scheme-mode:syntax-table #\, "  p")
(set-char-syntax! scheme-mode:syntax-table #\- "_")
(set-char-syntax! scheme-mode:syntax-table #\. "_")
(set-char-syntax! scheme-mode:syntax-table #\/ "_")
(set-char-syntax! scheme-mode:syntax-table #\@ "_ p")

(set-char-syntax! scheme-mode:syntax-table #\: "_")
(set-char-syntax! scheme-mode:syntax-table #\; "< ")
(set-char-syntax! scheme-mode:syntax-table #\< "_")
(set-char-syntax! scheme-mode:syntax-table #\= "_")
(set-char-syntax! scheme-mode:syntax-table #\> "_")
(set-char-syntax! scheme-mode:syntax-table #\? "_")

(set-char-syntax! scheme-mode:syntax-table #\[ "(]")
(set-char-syntax! scheme-mode:syntax-table #\\ "\\ ")
(set-char-syntax! scheme-mode:syntax-table #\] ")[")
(set-char-syntax! scheme-mode:syntax-table #\^ "_")
(set-char-syntax! scheme-mode:syntax-table #\_ "_")
(set-char-syntax! scheme-mode:syntax-table #\` "  p")
(set-char-syntax! scheme-mode:syntax-table #\{ "(}")
(set-char-syntax! scheme-mode:syntax-table #\| "\" 23")
(set-char-syntax! scheme-mode:syntax-table #\} "){")
(set-char-syntax! scheme-mode:syntax-table #\? "_")

;;;; Indentation

(define (scheme-mode:indent-let-method state indent-point normal-indent)
  (lisp-indent-special-form
   (if (let ((start
	      (forward-to-sexp-start
	       (forward-one-sexp (mark1+ (parse-state-containing-sexp state))
				 indent-point)
	       indent-point)))
	 (and start
	      (not (re-match-forward "\\s(" start))))
       2
       1)
   state indent-point normal-indent))

(define scheme-mode:indent-methods
  (make-string-table))

(define (scheme-indent-method name method)
  (define-variable-local-value! (selected-buffer)
      (name->variable (symbol 'LISP-INDENT/ name) 'INTERN)
    method))

(for-each (lambda (entry)
	    (for-each (lambda (name)
			(string-table-put! scheme-mode:indent-methods
					   (symbol->string name)
					   (car entry)))
		      (cdr entry)))
	  `(;; R4RS keywords:
	    (0 BEGIN DELAY)
	    (1 CASE LAMBDA LET* LETREC LET-SYNTAX LETREC-SYNTAX SYNTAX-RULES)
	    (2 DO)
	    (,scheme-mode:indent-let-method LET)

	    ;; R4RS procedures:
	    (1 CALL-WITH-INPUT-FILE WITH-INPUT-FROM-FILE
	       CALL-WITH-OUTPUT-FILE WITH-OUTPUT-TO-FILE)

	    ;; SRFI keywords:
            (1 AND-LET*)
	    (2 RECEIVE)
	    (3 DEFINE-RECORD-TYPE)

	    ;; MIT/GNU Scheme keywords:
	    (1 DEFINE-STRUCTURE FLUID-LET LET*-SYNTAX LOCAL-DECLARE
	       NAMED-LAMBDA)

	    ;; MIT/GNU Scheme procedures:
	    (0 CALL-WITH-OUTPUT-STRING WITH-OUTPUT-TO-STRING)
	    (1 CALL-WITH-APPEND-FILE CALL-WITH-BINARY-APPEND-FILE
	       CALL-WITH-BINARY-INPUT-FILE CALL-WITH-BINARY-OUTPUT-FILE
	       WITH-INPUT-FROM-PORT WITH-INPUT-FROM-STRING WITH-OUTPUT-TO-PORT
	       CALL-WITH-VALUES WITH-VALUES WITHIN-CONTINUATION
	       KEEP-MATCHING-ITEMS KEEP-MATCHING-ITEMS! DELETE-MATCHING-ITEMS
	       DELETE-MATCHING-ITEMS! FIND-MATCHING-ITEM FIND-NON-MATCHING-ITEM
	       COUNT-MATCHING-ITEMS COUNT-NON-MATCHING-ITEMS
	       FOR-ALL? THERE-EXISTS? LIST-OF-TYPE? VECTOR-OF-TYPE?
	       CALL-WITH-INPUT-STRING)
	    (2 WITH-SIMPLE-RESTART BIND-CONDITION-HANDLER)
	    (3 MAKE-CONDITION-TYPE)
	    (4 WITH-RESTART)))

(define scheme-mode:indent-regexps
  `(SCHEME-MODE:INDENT-REGEXPS
    ("DEF" . DEFINITION)
    ("WITH-" . 1)))

;;;; Completion

(define (scheme-complete-symbol bound-only?)
  (let ((end
	 (let ((point (current-point)))
	   (or (re-match-forward "\\(\\sw\\|\\s_\\)+"
				 point (group-end point) #f)
	       (let ((start (group-start point)))
		 (if (not (and (mark< start point)
			       (re-match-forward "\\sw\\|\\s_"
						 (mark-1+ point)
						 point
						 #f)))
		     (editor-error "No symbol preceding point"))
		 point)))))
    (let ((start (forward-prefix-chars (backward-sexp end 1 'LIMIT) end)))
      (standard-completion (extract-string start end)
	(lambda (prefix if-unique if-not-unique if-not-found)
	  (let ((completions
		 (let ((environment (evaluation-environment #f)))
		   (let ((completions
			  (obarray-completions
			   (if (and bound-only?
				    (environment-lookup
				     environment
				     '*PARSER-CANONICALIZE-SYMBOLS?*))
			       (string-downcase prefix)
			       prefix))))
		     (if bound-only?
			 (filter
			  (lambda (name)
			    (environment-bound? environment name))
			  completions)
			 completions)))))
	    (cond ((not (pair? completions))
		   (if-not-found))
		  ((null? (cdr completions))
		   (if-unique (weak-car (car completions))))
		  (else
		   (let ((completions (map weak-car completions)))
		     (if-not-unique
		      (string-greatest-common-prefix completions)
		      (lambda () (sort completions string<=?))))))))
	(lambda (completion)
	  (delete-string start end)
	  (insert-string completion start))))))

(define (obarray-completions prefix)
  (let ((obarray (fixed-objects-item 'OBARRAY)))
    (let ((prefix-length (string-length prefix))
	  (obarray-length (vector-length obarray)))
      (let index-loop ((i 0))
	(if (fix:< i obarray-length)
	    (let bucket-loop ((symbols (vector-ref obarray i)))
	      (if (null? symbols)
		  (index-loop (fix:+ i 1))
		  (let ((string (weak-car (car symbols))))
		    (if (and (fix:<= prefix-length (string-length string))
			     (let loop ((index 0))
			       (or (fix:= index prefix-length)
				   (and (char=? (string-ref prefix index)
						(string-ref string index))
					(loop (fix:+ index 1))))))
			(cons (car symbols) (bucket-loop (cdr symbols)))
			(bucket-loop (cdr symbols))))))
	    '())))))

(define-command scheme-complete-symbol
  "Perform completion on Scheme symbol preceding point.
That symbol is compared against the symbols that exist
and any additional characters determined by what is there
are inserted.
With prefix arg, only symbols that are bound in the buffer's
environment are considered."
  "P"
  scheme-complete-symbol)

(define-command scheme-complete-variable
  "Perform completion on Scheme variable name preceding point.
That name is compared against the bound variables in the evaluation environment
and any additional characters determined by what is there are inserted.
With prefix arg, the evaluation environment is ignored and all symbols
are considered for completion."
  "P"
  (lambda (all-symbols?) (scheme-complete-symbol (not all-symbols?))))

(define-command show-parameter-list
  "Show the parameter list of the procedure in the call surrounding point.
With prefix argument, the parameter list is inserted at point.
Otherwise, it is shown in the echo area."
  "d\nP"
  (lambda (point insert?)
    (let ((start
	   (forward-down-list (backward-up-list point 1 'ERROR) 1 'ERROR))
	  (buffer (mark-buffer point)))
      (let ((end (forward-sexp start 1 'ERROR)))
	(let ((procedure
	       (let ((environment (evaluation-environment buffer)))
		 (extended-scode-eval
		  (syntax (with-input-from-region (make-region start end) read)
			  environment)
		  environment))))
	  (if (procedure? procedure)
	      (let ((argl (procedure-argl procedure)))
		(if (and insert?
			 (let loop ((argl argl))
			   (or (symbol? argl)
			       (null? argl)
			       (and (pair? argl)
				    (or (symbol? (car argl))
					(eq? (car argl) #!optional)
					(eq? (car argl) #!rest)
					(eq? (car argl) #!aux))
				    (loop (cdr argl))))))
		    (let ((point (mark-left-inserting-copy point)))
		      (let loop ((argl argl))
			(cond ((pair? argl)
			       (insert-char #\space point)
			       (insert-string (if (symbol? (car argl))
						  (symbol-name (car argl))
						  (write-to-string (car argl)))
					      point)
			       (loop (cdr argl)))
			      ((symbol? argl)
			       (insert-string " . " point)
			       (insert-string (symbol-name argl) point)))))
		    (fluid-let ((*unparse-uninterned-symbols-by-name?* #t))
		      (message argl))))
	      (editor-error "Expression does not evaluate to a procedure: "
			    (extract-string start end))))))))

(define (procedure-argl proc)
  "Returns the arg list of PROC.
Grumbles if PROC is an undocumented primitive."
  (if (primitive-procedure? proc)
      (let ((doc-string (primitive-procedure-documentation proc)))
	(if doc-string
	    (let ((newline (string-find-next-char doc-string #\newline)))
	      (if newline
		  (string-head doc-string newline)
		  doc-string))
	    (string-append (write-to-string proc)
			   " has no documentation string.")))
      (let ((code (procedure-lambda proc)))
	(if code
	    (lambda-components* code
	      (lambda (name required optional rest body)
		name body
		(append required
			(if (null? optional) '() `(#!OPTIONAL ,@optional))
			(if rest `(#!REST ,rest) '()))))
	    "No debugging information available for this procedure."))))