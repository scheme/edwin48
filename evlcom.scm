#| -*-Scheme-*-

$Id: evlcom.scm,v 1.74 2007/01/18 02:03:39 riastradh Exp $

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

;;;; Evaluation Commands
;;; Package: (edwin)

(declare (usual-integrations))

;;;; Variables

(define-variable scheme-environment
  "The environment used by the evaluation commands, or 'DEFAULT.
If 'DEFAULT, use the default (REP loop) environment."
  'DEFAULT
  #f
  (lambda (object)
    (if (or (eq? 'DEFAULT object) (list-of-type? object symbol?))
	object
	(call-with-current-continuation
	 (lambda (k)
	   (bind-condition-handler (list condition-type:error)
	       (lambda (condition)
		 condition
		 (k 'DEFAULT))
	     (lambda ()
	       (->environment object))))))))

(define-variable scheme-syntax-table
  "This variable is obsolete and its value is ignored."
  #f)

(add-variable-assignment-daemon! (ref-variable-object scheme-environment)
  (lambda (buffer variable)
    variable
    (if buffer (normal-buffer-evaluation-mode buffer))))

(define (normal-buffer-evaluation-mode buffer)
  (let ((environment (ref-variable-object scheme-environment))
	(evaluate-inferior (ref-variable-object evaluate-in-inferior-repl))
	(run-light (ref-variable-object run-light)))
    (if (and (not (repl-buffer? buffer))
	     (not (variable-local-value? buffer evaluate-inferior))
	     (and (variable-local-value? buffer environment)
		  (not (eq? 'DEFAULT
			    (variable-local-value buffer environment)))))
	(begin
	  (define-variable-local-value! buffer evaluate-inferior #f)
	  (if (not (variable-local-value? buffer run-light))
	      (define-variable-local-value! buffer run-light #f))))))

(define-variable debug-on-evaluation-error
  "True means enter debugger if an evaluation error is signalled.
False means ignore the error and resume editing.
The symbol ASK means ask what to do (this is the default value).
This does not affect editor errors or internal errors."
  'ASK
  (lambda (x) (or (boolean? x) (eq? x 'ASK))))

(define-variable evaluation-input-recorder
  "A procedure that receives each input region before evaluation.
If #F, disables input recording."
  #f)

(define-variable evaluation-output-receiver
  "Procedure to call with the value and output from evaluation.
The value is an object, and the output is a string.
If #F, the value is printed in the typein window,
and the output, if non-null, is shown in a pop-up buffer."
  #f)

(define-variable enable-transcript-buffer
  "If true, output from evaluation commands is recorded in transcript buffer."
  #f
  boolean?)

(define-variable disable-evaluation-commands
  "If true, evaluation commands signal an error."
  #f
  boolean?)

(define-variable evaluate-in-inferior-repl
  "If true, evaluation commands evaluate expressions in an inferior REPL.
Also, the inferior REPL's run light appears in all Scheme mode buffers.
Otherwise, expressions are evaluated directly by the commands."
  #t
  boolean?)

(define-variable transcript-buffer-name
  "Name of evaluation transcript buffer.
This can also be a buffer object."
  "*transcript*")

(define-variable transcript-buffer-mode
  "Mode of evaluation transcript buffer.
This can be either a mode object or the name of one."
  'scheme)

(define-variable transcript-buffer-read-only
  "If true, transcript buffer is initialized to read-only when created."
  #t
  boolean?)

(define-variable transcript-output-wrapper
  "A procedure that is called to setup transcript output.
It is passed a thunk as its only argument.
If #F, normal transcript output is done."
  #f)

(define-variable transcript-list-depth-limit
  "List depth to which evaluation results are printed.  #F means no limit."
  #f
  (lambda (object) (or (not object) (exact-nonnegative-integer? object))))

(define-variable transcript-list-breadth-limit
  "List breadth to which evaluation results are printed.  #F means no limit."
  #f
  (lambda (object) (or (not object) (exact-nonnegative-integer? object))))

(define-variable transcript-disable-evaluation
  "If true, evaluation commands are disabled in the transcript buffer."
  #t
  boolean?)

;;;; Commands

(define-command eval-defun
  "Evaluate defun that point is in or before.
Print value in minibuffer."
  ()
  (lambda () (evaluate-from-mark (current-definition-start))))

(define-command eval-next-sexp
  "Evaluate the expression following point.
Prints the result in the typein window."
  ()
  (lambda () (evaluate-from-mark (current-point))))

(define-command eval-last-sexp
  "Evaluate the expression preceding point.
Prints the result in the typein window."
  ()
  (lambda () (evaluate-from-mark (backward-sexp (current-point) 1 'ERROR))))

(define (evaluate-from-mark input-mark)
  ((ref-command eval-region)
   (make-region input-mark
		(forward-sexp input-mark 1 'ERROR))))

(define-command eval-region
  "Evaluate the region, printing the results in the typein window.
With an argument, prompts for the evaluation environment."
  "r"
  (lambda (region)
    (let ((buffer (mark-buffer (region-start region))))
      (cond ((ref-variable disable-evaluation-commands buffer)
	     (editor-error "Evaluation commands disabled in this buffer."))
	    ((ref-variable evaluate-in-inferior-repl buffer)
	     (inferior-repl-eval-region (current-repl-buffer buffer) region))
	    (else
	     (evaluate-region region (evaluation-environment buffer #f)))))))

(define-command eval-current-buffer
  "Evaluate the current buffer.
The values are printed in the typein window."
  ()
  (lambda () ((ref-command eval-region) (buffer-region (current-buffer)))))

(define-command eval-expression
  "Read and evaluate an expression in the typein window."
  "xEvaluate expression"
  (lambda (expression)
    (let ((buffer (current-buffer)))
      (cond ((ref-variable disable-evaluation-commands buffer)
	     (editor-error "Evaluation commands disabled in this buffer."))
	    ((and (ref-variable evaluate-in-inferior-repl buffer)
		  (current-repl-buffer* buffer))
	     => (lambda (buffer)
		  (inferior-repl-eval-expression buffer expression)))
	    (else
	     (if (ref-variable enable-transcript-buffer buffer)
		 (call-with-transcript-buffer
		  (lambda (buffer)
		    (insert-string
		     (fluid-let ((*unparse-with-maximum-readability?* #t))
		       (write-to-string expression))
		     (buffer-end buffer)))))
	     (editor-eval buffer
			  expression
			  (evaluation-environment buffer #f)))))))

(define-command eval-abort-top-level
  "Force the evaluation REPL up to top level.
Has no effect if evaluate-in-inferior-repl is false."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (if (ref-variable evaluate-in-inferior-repl buffer)
	  ((ref-command inferior-cmdl-abort-top-level))
	  (editor-error "Nothing to abort.")))))

(define-command set-environment
  "Make ENVIRONMENT the current evaluation environment."
  "XSet environment"
  (lambda (environment)
    (local-set-variable! scheme-environment environment)))

(define-command set-default-environment
  "Make ENVIRONMENT the default evaluation environment."
  "XSet default environment"
  (lambda (environment)
    (set-variable-default-value! (ref-variable-object scheme-environment)
				 environment)))

(define-command set-repl-environment
  "Make ENVIRONMENT the environment of the nearest REP loop."
  "XSet REPL environment"
  (lambda (environment)
    (set-repl/environment! (nearest-repl) (->environment environment))))

(define-command select-transcript-buffer
  "Select the transcript buffer."
  ()
  (lambda ()
    (call-with-transcript-buffer select-buffer)))

;;;; Expression Prompts

(define (prompt-for-expression-value prompt #!optional default environment
				     . options)
  (let ((environment
	 (if (default-object? environment)
	     (evaluation-environment)
	     (begin
	       (guarantee-environment environment 'PROMPT-FOR-EXPRESSION-VALUE)
	       environment))))
    (eval-with-history (apply prompt-for-expression
			      prompt
			      (if (or (symbol? default)
				      (pair? default)
				      (vector? default))
				  `',default
				  default)
			      environment
			      options)
		       environment)))

(define (prompt-for-expression prompt #!optional default environment . options)
  (let ((environment
	 (if (default-object? environment)
	     (evaluation-environment)
	     (begin
	       (guarantee-environment environment 'PROMPT-FOR-EXPRESSION)
	       environment))))
    (read-from-string
     (apply prompt-for-string
	    prompt
	    (and (not (default-object? default))
		 default)
	    'MODE
	    (lambda (buffer)
	      (set-buffer-major-mode! buffer
				      (ref-mode-object prompt-for-expression))
	      ;; This sets up the correct environment in the typein buffer
	      ;; so that completion of variables works right.
	      (local-set-variable! scheme-environment environment buffer))
	    options)
     environment)))

(define (read-from-string string environment)
  (bind-condition-handler (list condition-type:error) evaluation-error-handler
    (lambda ()
      (read (open-input-string string) environment))))

(define-major-mode prompt-for-expression scheme #f
  (mode-description (ref-mode-object minibuffer-local))
  (lambda (buffer)
    ;; This kludge prevents auto-fill from being turned on.  Probably
    ;; there is a better way to do this, but I can't think of one
    ;; right now.  -- cph
    (for-each (lambda (mode)
		(disable-buffer-minor-mode! buffer mode))
	      (buffer-minor-modes buffer))))

(set-car! (mode-comtabs (ref-mode-object prompt-for-expression))
	  (car (mode-comtabs (ref-mode-object minibuffer-local))))

;;;; Evaluation

(define (evaluate-region region environment)
  (let ((buffer (->buffer region)))
    (let ((evaluation-input-recorder
	   (ref-variable evaluation-input-recorder buffer)))
      (if evaluation-input-recorder
	  (evaluation-input-recorder region)))
    (if (ref-variable enable-transcript-buffer buffer)
	(call-with-transcript-buffer
	 (lambda (buffer)
	   (insert-region (region-start region)
			  (region-end region)
			  (buffer-end buffer)))))
    (bind-condition-handler (list condition-type:error)
	evaluation-error-handler
      (lambda ()
	(let loop
	    ((expressions (read-expressions-from-region region environment))
	     (result unspecific))
	  (if (null? expressions)
	      result
	      (loop (cdr expressions)
		    (editor-eval buffer (car expressions) environment))))))))

(define (read-expressions-from-region region #!optional environment)
  (let ((environment
	 (if (default-object? environment)
	     (evaluation-environment region)
	     environment)))
    (call-with-input-region region
      (lambda (port)
	(let loop ()
	  (let ((expression (read port environment)))
	    (if (eof-object? expression)
		'()
		(cons expression (loop)))))))))

(define (evaluation-environment #!optional buffer global-ok?)
  (let ((buffer
	 (if (default-object? buffer)
	     (current-buffer)
	     (->buffer buffer)))
	(non-default
	 (lambda (object)
	   (if (environment? object)
	       object
	       (let ((package (name->package object)))
		 (cond (package
			(package/environment package))
		       ((if (default-object? global-ok?) #t global-ok?)
			system-global-environment)
		       (else
			(editor-error "Package not loaded: " object))))))))
    (let ((environment (ref-variable scheme-environment buffer)))
      (if (eq? 'DEFAULT environment)
	  (let ((repl-buffer
		 (and (ref-variable evaluate-in-inferior-repl buffer)
		      (current-repl-buffer* buffer))))
	    (if repl-buffer
		(let ((environment
		       (ref-variable scheme-environment repl-buffer)))
		  (if (eq? 'DEFAULT environment)
		      (nearest-repl/environment)
		      (non-default environment)))
		(nearest-repl/environment)))
	  (non-default environment)))))

(define-variable run-light
  "Scheme run light.  Not intended to be modified by users.
Set by Scheme evaluation code to update the mode line."
  #f
  (lambda (object) (or (not object) (string? object))))

(define-variable enable-run-light?
  "If true, Scheme evaluation commands display a run light in the mode line."
  #t
  boolean?)

(define (editor-eval buffer sexp environment)
  (let ((core
	 (lambda ()
	   (with-input-from-port dummy-i/o-port
	     (lambda ()
	       (let ((value))
		 (let ((output-string
			(with-output-to-string
			  (lambda ()
			    (set! value (eval-with-history sexp environment))
			    unspecific))))
		   (let ((evaluation-output-receiver
			  (ref-variable evaluation-output-receiver buffer)))
		     (if evaluation-output-receiver
			 (evaluation-output-receiver value output-string)
			 (with-output-to-transcript-buffer
			  (lambda ()
			    (write-string output-string)
			    (transcript-write
			     value
			     (and (ref-variable enable-transcript-buffer
						buffer)
				  (transcript-buffer))))))))
		 value))))))
    (if (ref-variable enable-run-light? buffer)
	(let ((run-light (ref-variable-object run-light))
	      (outside)
	      (inside "eval"))
	  (dynamic-wind
	   (lambda ()
	     (set! outside (variable-local-value buffer run-light))
	     (set-variable-local-value! buffer run-light inside)
	     (set! inside)
	     (global-window-modeline-event!)
	     (update-screens! #f))
	   core
	   (lambda ()
	     (set! inside (variable-local-value buffer run-light))
	     (set-variable-local-value! buffer run-light outside)
	     (set! outside)
	     (global-window-modeline-event!)
	     (update-screens! #f))))
	(core))))

(define (eval-with-history expression environment)
  (bind-condition-handler (list condition-type:error)
      evaluation-error-handler
    (lambda ()
      (repl-eval expression environment))))

(define (evaluation-error-handler condition)
  (maybe-debug-scheme-error 'EVALUATION condition)
  (standard-error-report 'EVALUATION condition #f)
  (editor-beep)
  (return-to-command-loop condition))

;;;; Transcript Buffer

(define (with-output-to-transcript-buffer thunk)
  (if (ref-variable enable-transcript-buffer)
      (let ((output-wrapper (ref-variable transcript-output-wrapper)))
	(if output-wrapper
	    (output-wrapper thunk)
	    (call-with-transcript-buffer
	     (lambda (buffer)
	       (let ((output-port
		      (mark->output-port (buffer-end buffer) buffer)))
		 (fresh-line output-port)
		 (with-output-to-port output-port thunk))))))
      (let ((value))
	(let ((output
	       (with-output-to-string
		 (lambda ()
		   (set! value (thunk))
		   unspecific))))
	  (if (and (not (string-null? output))
		   (not (ref-variable evaluation-output-receiver)))
	      (string->temporary-buffer output "*Unsolicited-Output*" '())))
	value)))

(define (transcript-write value buffer)
  (let ((value-string
	 (string-append
	  (transcript-value-prefix-string value #f)
	  (transcript-value-string value))))
    (if buffer
	(let ((point (mark-left-inserting-copy (buffer-end buffer))))
	  (with-read-only-defeated point
	    (lambda ()
	      (guarantee-newlines 1 point)
	      (insert-string value-string point)
	      (insert-newlines 2 point)))
	  (mark-temporary! point)))
    (if (or (not buffer) (null? (buffer-windows buffer)))
	(message value-string))))

(define (transcript-value-prefix-string value hash-number?)
  (if (undefined-value? value)
      ";No value"
      (string-append
       ";Value"
       (if (and hash-number?
		(object-pointer? value)
		(not (interned-symbol? value))
		(not (number? value)))
	   (string-append
	    " "
	    (write-to-string (object-hash value)))
	   "")
       ": ")))

(define (transcript-value-string value)
  (if (undefined-value? value)
      ""
      (fluid-let ((*unparser-list-depth-limit*
		   (ref-variable transcript-list-depth-limit))
		  (*unparser-list-breadth-limit*
		   (ref-variable transcript-list-breadth-limit)))
	(write-to-string value))))

(define (call-with-transcript-buffer procedure)
  (let ((buffer (transcript-buffer)))
    (let ((group (buffer-group buffer))
	  (outside)
	  (inside #f))
      (dynamic-wind (lambda ()
		      (set! outside (group-read-only? group))
		      (if inside
			  (set-group-read-only! group)
			  (set-group-writeable! group)))
		    (lambda ()
		      (procedure buffer))
		    (lambda ()
		      (set! inside (group-read-only? group))
		      (if outside
			  (set-group-read-only! group)
			  (set-group-writeable! group)))))))

(define (transcript-buffer)
  (let ((name (ref-variable transcript-buffer-name)))
    (if (buffer? name)
	name
	(or (find-buffer name)
	    (let ((buffer (create-buffer name)))
	      (set-buffer-major-mode!
	       buffer
	       (->mode (ref-variable transcript-buffer-mode)))
	      (if (ref-variable transcript-buffer-read-only)
		  (set-buffer-read-only! buffer))
	      (if (ref-variable transcript-disable-evaluation)
		  (local-set-variable! disable-evaluation-commands #t buffer)
		  (if (eq? (buffer-major-mode buffer)
			   (ref-mode-object scheme))
		      (begin
			(local-set-variable! evaluate-in-inferior-repl #f
					     buffer)
			(local-set-variable! run-light #f buffer))))
	      buffer)))))