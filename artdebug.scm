#| -*-Scheme-*-

$Id: artdebug.scm,v 1.37 2007/01/05 21:19:23 cph Exp $

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

;;;; Continuation Browser


#| TO DO

Make environment browsing mode; the debugger mode can be a superset
of that mode: Add optional marker lines for environments.  If you do
the C-c C-a command to describe the environment frames in the current
subproblem or reduction, the debugger should use the correct
environment when you do evaluations in those environment frames.
Make commands for moving by environment level.  Later, change this to
execute Where in another buffer depending on the state of a flag.

Make a variable that specifies whether to prompt the user if more
than a certain number of variables are about to be printed during an
environment-browsing command.

By default, when the debugger starts, don't show history levels
inside the system.  To detect system code, see
~arthur/new6001/detect.scm.  Predicate SYSTEM-FRAME? is already
in place.

MarkF has code to use the correct syntax tables for evaluation.

Add limits to the depth and breadth of objects printed by the
debugger, to avoid problems caused by displaying circular objects.
Note $se/evlcom.scm: TRANSCRIPT-LIST-DEPTH-LIMIT and
TRANSCRIPT-LIST-BREADTH-LIMIT.

Make C-c C-k evaluate in the environment in which the error occurred.
Otherwise, the "Define x to a given value" restart for unbound
variable errors won't work.  This seems to be a bug in the regular
debugger, too.

Make C-c C-z work in the case where an error happens during
evaluation of the return expression, the debugger starts on the new
error, and return is done from the second debugger straight through
the first back into the original computation.  The restart itself
works, but the message "Scheme error" is printed upon starting the
second debugger.

Jinx: Depending on the state of a flag, never invoke debugger on
unbound variable errors from the expression you eval in the
interaction buffer (or debugger buffer).  Actually, how about a
general filter on conditions that will start the debugger?  Provide a
default filter for ignoring unbound variables.

Jinx: Display the offending expression evaluated by the user.  Display
it just above the error message line.

Make a way to restrict the possible restarts to not include restarts
that could stop Edwin.

Make a narrow interface between Edwin and the debugger so it will be
easy to write this debugger for Emacs.

Number input lines so that it is possible to tell the order in which
you evaluated your expressions.  This could be particularly useful
for TAs looking over students' shoulders.

Once outline mode has been written for Edwin, add commands to expand
and contract subproblems and reductions.

|#

(define-variable debugger-confirm-return?
  "True means to prompt for confirmation in RETURN-FROM and RETURN-TO
commands before returning the value."
  #t
  boolean?)

(define-variable debugger-split-window?
  "True means use another window for the debugger buffer; false means
use the current window."
  #t
  boolean?)

(define-variable debugger-one-at-a-time?
  "True means delete an existing debugger buffer before before
starting a new debugger, ASK means ask the user, and false means
always create a new debugger buffer.  If there is more than one
debugger buffer at the time a new debugger is started, the debugger
will always create a new buffer."
  'ASK
  (lambda (value) (or (boolean? value) (eq? value 'ASK))))

(define-variable debugger-quit-on-return?
  "True means quit debugger when executing a \"return\" command."
  #t
  boolean?)

(define-variable debugger-quit-on-restart?
  "True means quit debugger when executing a \"restart\" command."
  #t
  boolean?)

(define-variable debugger-open-markers?
  "True means newlines are inserted between marker lines."
  #t
  boolean?)

(define-variable debugger-verbose-mode?
  "True means display extra information without the user requesting it."
  #f
  boolean?)

(define-variable debugger-expand-reductions?
  "True says to insert reductions when reduction motion commands are used
in a subproblem whose reductions aren't already inserted."
  #t
  boolean?)

(define-variable debugger-max-subproblems
  "Maximum number of subproblems displayed when debugger starts,
or #F meaning no limit."
  3
  (lambda (number)
    (or (not number)
	(and (exact-integer? number)
	     (> number 0)))))

(define-variable debugger-hide-system-code?
  "True means don't show subproblems created by the runtime system."
  #t
  boolean?)

(define-variable debugger-show-help-message?
  "True means show a help message in the debugger buffer."
  #t
  boolean?)

(define-variable debugger-debug-evaluations?
  "True means evaluation errors in a debugger buffer start new debuggers."
  #f
  boolean?)

(define starting-debugger? #f)
(define in-debugger-evaluation? #f)

(define (debug-scheme-error error-type condition ask?)
  (cond (starting-debugger?
	 (quit-editor-and-signal-error condition))
	((and in-debugger-evaluation?
	      (not (ref-variable debugger-debug-evaluations? #f)))
	 unspecific)
	(else
	 (let ((start-debugger
		(lambda ()
		  (fluid-let ((starting-debugger? #t))
		    ((if (ref-variable debugger-split-window? #f)
			 select-buffer-other-window
			 select-buffer)
		     (continuation-browser-buffer condition))))))
	   (if ask?
	       (if (cleanup-pop-up-buffers
		    (lambda ()
		      (standard-error-report error-type condition #t)
		      (editor-beep)
		      (prompt-for-confirmation? "Start debugger")))
		   (start-debugger))
	       (begin
		 (start-debugger)
		 (message (string-capitalize (symbol->string error-type))
			  " error")
		 (editor-beep))))
	 (return-to-command-loop condition))))

(define-command browse-continuation
  "Invoke the continuation-browser on CONTINUATION."
  "XBrowse Continuation"
  (lambda (continuation)
    (let ((buffer (continuation-browser-buffer continuation)))
      ((if (ref-variable debugger-split-window?)
	   select-buffer-other-window
	   select-buffer)
       buffer))))

(define-integrable (buffer-dstate buffer)
  (buffer-get buffer 'DEBUG-STATE))

;;;; Main Entry

(define (continuation-browser-buffer object)
  (let ((buffers (find-debugger-buffers)))
    (if (and (not (null? buffers))
	     (null? (cdr buffers))
	     (ref-variable debugger-one-at-a-time?)
	     (or (eq? #t (ref-variable debugger-one-at-a-time?))
		 (prompt-for-confirmation?
		  "Another debugger buffer exists.  Delete it")))
	(kill-buffer (car buffers))))
  (let ((buffer (new-buffer "*debug*"))
	(dstate (make-initial-dstate object)))
    (set-buffer-major-mode! buffer (ref-mode-object continuation-browser))
    (buffer-put! buffer 'DEBUG-STATE dstate)
    (let ((top-subproblem
	   (let ((previous-subproblems (dstate/previous-subproblems dstate)))
	     (if (null? previous-subproblems)
		 (dstate/subproblem dstate)
		 (car (last-pair previous-subproblems)))))
	  (max-subproblems (ref-variable debugger-max-subproblems buffer))
	  (hide-system-code? (ref-variable debugger-hide-system-code? buffer)))
      (with-group-undo-disabled (buffer-group buffer)
	(lambda ()
	  (let ((port (mark->output-port (buffer-start buffer))))
	    (if (ref-variable debugger-show-help-message? buffer)
		(print-help-message buffer port))
	    (if (condition? object)
		(begin
		  (write-string "The error that started the debugger is:" port)
		  (newline port)
		  (write-string "  " port)
		  (write-condition-report object port)
		  (newline port)
		  (newline port)
		  (print-restarts object buffer port)))
	    (if (let loop ((frame top-subproblem) (level 0))
		  (and frame
		       (or (and max-subproblems (= level max-subproblems))
			   (and hide-system-code? (system-frame? frame))
			   (begin
			     (newline port)
			     (print-subproblem level frame port)
			     (loop (stack-frame/next-subproblem frame)
				   (+ level 1))))))
		(display-more-subproblems-message buffer)))))
      (let ((point (forward-subproblem (buffer-start buffer) 1)))
	(set-buffer-point! buffer point)
	(if (ref-variable debugger-verbose-mode? buffer)
	    (invoke-debugger-command mark
				     command/print-subproblem-or-reduction))
	(push-buffer-mark! buffer point)
	(buffer-not-modified! buffer)
	buffer))))

(define (find-debugger-buffers)
  (let ((debugger-mode (ref-mode-object continuation-browser)))
    (let loop ((buffers (buffer-list)))
      (cond ((null? buffers)
	     buffers)
	    ((eq? (buffer-major-mode (car buffers)) debugger-mode)
	     (cons (car buffers) (loop (cdr buffers))))
	    (else
	     (loop (cdr buffers)))))))

(define (print-help-message buffer port)
  (write-string (substitute-command-keys debugger-help-message buffer) port)
  (newline port)
  (newline port))

(define debugger-help-message
  "This is a debugger buffer:

  Marker lines identify stack frames, most recent first.
  Expressions are evaluated in the environment of the line above the point.

  In the marker lines,

    -C- means frame was generated by Compiled code
    -I- means frame was generated by Interpreted code

    S=x means frame is in subproblem number x
    R=y means frame is reduction number y
    #R=z means there are z reductions in the subproblem;
      use \\[continuation-browser-forward-reduction] to see them

  \\[continuation-browser-print-subproblem-or-reduction] describes the current subproblem or reduction.
  \\[describe-mode] shows information about debugger commands.
  Use \\[kill-buffer] to quit the debugger.")

(define (print-restarts condition buffer port)
  (let ((restarts (condition/restarts condition)))
    (if (not (null? restarts))
	(begin
	  (write-string "Restart options:" port)
	  (write-restarts restarts port
	    (lambda (index port)
	      (write-string (string-pad-left (number->string index) 3) port)
	      (write-string ":" port)))
	  (write-string
	   (substitute-command-keys
	    "Use \\[continuation-browser-condition-restart] to invoke any of these restarts."
	    buffer)
	   port)
	  (newline port)))))

(define (system-frame? frame)
  frame					;ignore
  #f)

(define-major-mode continuation-browser scheme "Debug"
  "Major mode for debugging Scheme programs and browsing Scheme continuations.
Evaluation commands are similar to those of Scheme Interaction mode.

  Marker lines identify stack frames, most recent first.
  Expressions are evaluated in the environment of the line above the point.

  In the marker lines,

    -C- means frame was generated by Compiled code
    -I- means frame was generated by Interpreted code

    S=x means frame is in subproblem number x
    R=y means frame is reduction number y
    #R=z means there are z reductions in the subproblem;
      use \\[continuation-browser-forward-reduction] to see them

Evaluate expressions

  \\[continuation-browser-eval-last-sexp] evaluates the expression preceding point in the
    environment of the current frame.
  \\[continuation-browser-eval-last-sexp/dynamic] evaluates the expression preceding point in the
    environment AND DYNAMIC STATE of the current frame.

Move between subproblems and reductions

  \\[continuation-browser-forward-reduction] moves forward one reduction (earlier in time).
  \\[continuation-browser-backward-reduction] moves backward one reduction (later in time).

  \\[continuation-browser-forward-subproblem] moves forward one subproblem (earlier in time).
  \\[continuation-browser-backward-subproblem] moves backward one subproblem (later in time).

  \\[continuation-browser-go-to] moves directly to a subproblem (given its number).

Display debugging information

  \\[continuation-browser-show-all-frames] shows All bindings of the current environment and its ancestors.
  \\[continuation-browser-show-current-frame] shows bindings of identifiers in the Current environment.
  \\[continuation-browser-print-environment] describes the current Environment.
  \\[continuation-browser-print-expression] pretty prints the current expression.
  \\[continuation-browser-print-environment-procedure] pretty prints the procedure that created the current environment.
  \\[continuation-browser-expand-reductions] shows the Reductions of the current subproblem level.
  \\[continuation-browser-print-subproblem-or-reduction] describes the current subproblem or reduction.
  \\[continuation-browser-expand-subproblems] shows subproblems not already displayed.
  \\[continuation-browser-frame] displays the current stack frame in internal format.

Miscellany

  \\[continuation-browser-condition-restart] continues the program using a standard restart option.
  \\[continuation-browser-return-from] returns from the current subproblem with the value of the expression
    preceding the point.
  \\[continuation-browser-return-to] returns to the current subproblem with the value of the expression
    preceding the point.
  \\[continuation-browser-retry] retries the offending expression, returning from the current
    subproblem with its value.

Use \\[kill-buffer] to quit the debugger."
  (lambda (buffer)
    (define-variable-local-value! buffer
      (ref-variable-object comint-input-ring)
      (make-ring (ref-variable comint-input-ring-size)))
    (define-variable-local-value! buffer
      (ref-variable-object evaluation-input-recorder)
      continuation-browser-input-recorder)
    (define-variable-local-value! buffer
      (ref-variable-object evaluation-output-receiver)
      continuation-browser-output-receiver)))

(define (continuation-browser-input-recorder region)
  (ring-push! (ref-variable comint-input-ring) (region->string region)))

(define (continuation-browser-output-receiver value output)
  (let ((point (mark-left-inserting-copy (current-point))))
    (insert-string output point)
    (guarantee-newlines 1 point)
    (insert-string (transcript-value-prefix-string value #t) point)
    (insert-string (transcript-value-string value) point)
    (insert-newlines 2 point)
    (mark-temporary! point)))

;;; Disable EVAL-CURRENT-BUFFER in Debugger Mode.  It is inherited
;;; from Scheme mode but does not make sense here:

(define-key 'continuation-browser #\M-o
  'undefined)

;;; Comint History
(define-key 'continuation-browser #\M-p
  'comint-previous-input)
(define-key 'continuation-browser #\M-n
  'comint-next-input)
(define-key 'continuation-browser '(#\C-c #\C-r)
  'comint-history-search-backward)
(define-key 'continuation-browser '(#\C-c #\C-s)
  'comint-history-search-forward)

;;; Evaluation Commands
(define-key 'continuation-browser '(#\C-x #\C-e)
  'continuation-browser-eval-last-sexp)
(define-key 'continuation-browser '(#\C-x #\C-r)
  'continuation-browser-eval-last-sexp/dynamic)
(define-key 'continuation-browser #\M-z
  'continuation-browser-eval-defun)
(define-key 'continuation-browser '(#\M-C-z)
  'continuation-browser-eval-region)

;;; Motion Commands
(define-key 'continuation-browser '(#\C-c #\C-f)
  'continuation-browser-forward-reduction)
(define-key 'continuation-browser '(#\C-c #\C-n)
  'continuation-browser-forward-subproblem)
(define-key 'continuation-browser '(#\C-c #\C-b)
  'continuation-browser-backward-reduction)
(define-key 'continuation-browser '(#\C-c #\C-p)
  'continuation-browser-backward-subproblem)
(define-key 'continuation-browser '(#\C-c #\C-w)
  'continuation-browser-go-to)

;;; Information-display Commands
(define-key 'continuation-browser '(#\C-c #\C-a)
  'continuation-browser-show-all-frames)
(define-key 'continuation-browser '(#\C-c #\C-c)
  'continuation-browser-show-current-frame)
(define-key 'continuation-browser '(#\C-c #\C-e)
  'continuation-browser-print-environment)
(define-key 'continuation-browser '(#\C-c #\C-l)
  'continuation-browser-print-expression)
(define-key 'continuation-browser '(#\C-c #\C-o)
  'continuation-browser-print-environment-procedure)
(define-key 'continuation-browser '(#\C-c #\C-m)
  'continuation-browser-expand-reductions)
(define-key 'continuation-browser '(#\C-c #\C-t)
  'continuation-browser-print-subproblem-or-reduction)
(define-key 'continuation-browser '(#\C-c #\C-x)
  'continuation-browser-expand-subproblems)
(define-key 'continuation-browser '(#\C-c #\C-y)
  'continuation-browser-frame)

;;; Miscellaneous Commands
(define-key 'continuation-browser '(#\C-c #\C-k)
  'continuation-browser-condition-restart)
(define-key 'continuation-browser '(#\C-c #\C-j)
  'continuation-browser-return-to)
(define-key 'continuation-browser '(#\C-c #\C-z)
  'continuation-browser-return-from)
(define-key 'continuation-browser '(#\C-c #\C-d)
  'continuation-browser-retry)
(define-key 'continuation-browser '(#\C-c #\C-g)
  'continuation-browser-abort-all)
(define-key 'continuation-browser '(#\C-c #\C-u)
  'continuation-browser-abort-previous)
(define-key 'continuation-browser '(#\C-c #\C-M-y)
  'continuation-browser-display-stack-elements)

(define (debugger-command-invocation command)
  (lambda ()
    (invoke-debugger-command (current-point) command)))

;;;; Evaluation Commands

(define-command continuation-browser-eval-region
  "Evaluate the region."
  "r"
  (lambda (region)
    (let ((environment
	   (dstate-evaluation-environment (start-evaluation region))))
      (fluid-let ((in-debugger-evaluation? #t))
	(evaluate-region region environment)))))

(define (start-evaluation region)
  (if (region-contains-marker? region)
      (editor-error "Can't evaluate region containing markers."))
  (set-current-point! (region-end region))
  (debug-dstate (region-start region)))

(define-command continuation-browser-eval-defun
  "Evaluate definition that point is in or before."
  ()
  (lambda ()
    ((ref-command continuation-browser-eval-region)
     (let ((input-mark (current-definition-start)))
       (make-region input-mark (forward-sexp input-mark 1 'ERROR))))))

(define-command continuation-browser-eval-last-sexp
  "Evaluate the expression preceding point."
  ()
  (lambda ()
    ((ref-command continuation-browser-eval-region)
     (let ((input-mark (backward-sexp (current-point) 1 'ERROR)))
       (make-region input-mark (forward-sexp input-mark 1 'ERROR))))))

(define-command continuation-browser-eval-region/dynamic
  "Evaluate the region.
The evaluation occurs in the dynamic state of the current frame."
  "r"
  (lambda (region)
    (let ((dstate (start-evaluation region)))
      (let ((environment (dstate-evaluation-environment dstate))
	    (continuation
	     (stack-frame->continuation (dstate/subproblem dstate)))
	    (old-hook hook/repl-eval))
	(fluid-let
	    ((in-debugger-evaluation? #t)
	     (hook/repl-eval
	      (lambda (expression environment repl)
		(let ((unique (cons 'unique 'id)))
		  (let ((result
			 (call-with-current-continuation
			  (lambda (continuation*)
			    (within-continuation continuation
			      (lambda ()
				(bind-condition-handler
				    '()
				    (lambda (condition)
				      (continuation* (cons unique condition)))
				  (lambda ()
				    (continuation*
				     (old-hook expression
					       environment
					       repl))))))))))
		    (if (and (pair? result)
			     (eq? unique (car result)))
			(error (cdr result))
			result))))))
	  (evaluate-region region environment))))))

(define-command continuation-browser-eval-last-sexp/dynamic
  "Evaluate the expression preceding point.
The evaluation occurs in the dynamic state of the current frame."
  ()
  (lambda ()
    ((ref-command continuation-browser-eval-region/dynamic)
     (let ((input-mark (backward-sexp (current-point) 1 'ERROR)))
       (make-region input-mark (forward-sexp input-mark 1 'ERROR))))))

;;;; Motion Commands

;;; The subproblem and reduction motion commands rely, in many
;;; places, on the assumption that subproblem and reduction numbers
;;; increase downward in the buffer, and that no subproblem/reduction
;;; marker line is repeated.  Of course, the user can violate this
;;; assumption by constructing or copying a marker, but the program
;;; is robust with respect to such conniving, as long as the
;;; reduction and subproblem specified by the numbers in the marker
;;; exist.  The only time it should be possible to notice an effect
;;; of this assumption is when a reduction or subproblem that is
;;; already displayed is automatically redisplayed because the
;;; existing one appeared out of order.

(define-command continuation-browser-forward-subproblem
  "Move one or more subproblems forward."
  "p"
  (lambda (argument) (move-thing forward-subproblem argument 'ERROR)))

(define-command continuation-browser-backward-subproblem
  "Move one or more subproblems backward."
  "p"
  (lambda (argument) (move-thing backward-subproblem argument 'ERROR)))

(define-command continuation-browser-forward-reduction
  "Move one or more reductions forward.
Display reductions that exist but are not yet displayed.
If there are no more reductions for the current subproblem,
move to the first reduction shown in the next subproblem."
  "p"
  (lambda (argument) (move-thing forward-reduction argument 'ERROR)))

(define-command continuation-browser-backward-reduction
  "Move one or more reductions backward.
Display reductions that exist but are not yet displayed.
If there are no more reductions for the current subproblem,
move to the last reduction shown in the previous subproblem."
  "p"
  (lambda (argument) (move-thing backward-reduction argument 'ERROR)))

(define-command continuation-browser-go-to
  "Move to an arbitrary subproblem.
Prompt for the subproblem number if not given as an argument.
Move to the last subproblem if the subproblem number is too high."
  "NSubproblem number"
  (lambda (destination-subproblem-number)
    (set-current-point!
     (let ((end (group-end (current-point)))
	   (not-found
	    (lambda ()
	      (editor-error "Cannot find subproblem"
			    destination-subproblem-number))))
       (let ((last-subproblem-number (current-subproblem-number end)))
	 (if (not last-subproblem-number)
	     (not-found))
	 (cond ((< destination-subproblem-number last-subproblem-number)
		(let loop ((point (backward-subproblem end 1)))
		  (if (not point)
		      (not-found))
		  (let ((subproblem (current-subproblem-number point)))
		    (if (not subproblem)
			(not-found))
		    (if (= subproblem destination-subproblem-number)
			point
			(loop (backward-subproblem point 1))))))
	       ((> destination-subproblem-number last-subproblem-number)
		(forward-subproblem
		 end
		 (- destination-subproblem-number last-subproblem-number)
		 'LIMIT))
	       (else end)))))))

;;;; Information-display Commands

(define-command continuation-browser-show-all-frames
  "Print the bindings of all frames of the current environment."
  ()
  (debugger-command-invocation command/show-all-frames))

(define-command continuation-browser-show-current-frame
  "Print the bindings of the current frame of the current environment."
  ()
  (debugger-command-invocation command/show-current-frame))

(define-command continuation-browser-print-environment
  "Identify the environment of the current frame."
  ()
  (debugger-command-invocation
   (lambda (dstate port)
     (debugger-presentation port
       (lambda ()
	 (print-subproblem-environment dstate port))))))


(define-command continuation-browser-print-expression
  "Pretty print the current expression."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (call-with-interface-port
       point
       (lambda (port)
	 (push-current-mark! point)
	 (let ((dstate (debug-dstate point))
	       (message
		(lambda (string)
		  (fresh-line port)
		  (write-string "; " port)
		  (write-string string port)))
	       (pp (lambda (obj)
		     (fresh-line port)
		     (pp obj port #t))))
		     
	   (if (dstate/reduction-number dstate)
	       (pp (reduction-expression (dstate/reduction dstate)))
	       (let ((exp (dstate/expression dstate))
		     (sub (dstate/subexpression dstate)))
		 (define (do-hairy)
		   (pp (unsyntax-with-substitutions
			exp
			(list
			 (cons sub
			       (make-pretty-printer-highlight
				(unsyntax sub)
				(ref-variable subexpression-start-marker)
				(ref-variable subexpression-end-marker)))))))

		 (cond ((not (invalid-expression? exp))
			(if (or argument
				(invalid-subexpression? sub))
			    (pp exp)
			    (fluid-let ((*pp-no-highlights?* #f))
			      (do-hairy))))
		       ((debugging-info/noise? exp)
			(message ((debugging-info/noise exp) #t)))
		       (else
			(message "Unknown expression")))))))))))

(define-command continuation-browser-print-environment-procedure
  "Pretty print the procedure that created the current environment."
  ()
  (debugger-command-invocation command/print-environment-procedure))

(define-command continuation-browser-expand-reductions
  "Expand all the reductions of the current subproblem.
If already expanded, move the point to one of the reductions."
  ()
  (lambda ()
    (let ((point (current-point)))
      (if (reductions-expanded? point)
	  (temporary-message
	   "Reductions for this subproblem already expanded.")
	  (expand-reductions point)))))

(define (command/print-subproblem-or-reduction dstate port)
  (debugger-presentation port
    (lambda ()
      (if (dstate/reduction-number dstate)
	  (print-reduction-expression (dstate/reduction dstate) port)
	  (print-subproblem-expression dstate port)))))

(define-command continuation-browser-print-subproblem-or-reduction
  "Print the current subproblem or reduction in the standard format."
  ()
  (debugger-command-invocation command/print-subproblem-or-reduction))

(define-command continuation-browser-expand-subproblems
  "Expand all subproblems, or ARG more subproblems if argument is given."
  "P"
  (lambda (argument)
    (let ((subproblem-number
	   (if argument
	       (+ (or (current-subproblem-number (group-end (current-point)))
		      (editor-error "Can't find subproblem marker"))
		  (command-argument-numeric-value argument))
	       (- (count-subproblems (current-buffer)) 1))))
      (let ((point (mark-right-inserting-copy (current-point))))
	((ref-command continuation-browser-go-to) subproblem-number)
	(mark-temporary! point)
	(set-current-point! point)))))

(define-command continuation-browser-frame
  "Show the current subproblem's stack frame in internal format."
  ()
  (debugger-command-invocation command/frame))

;;;; Miscellaneous Commands

(define-command continuation-browser-condition-restart
  "Continue the program using a standard restart option.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (fluid-let ((hook/invoke-restart
		 (lambda (continuation arguments)
		   (invoke-continuation continuation
					arguments
					avoid-deletion?))))
      (invoke-debugger-command (current-point) command/condition-restart))))

(define-command continuation-browser-return-to
  "Return TO the current subproblem with a value.
Invoke the continuation corresponding to this subproblem on the value
of the expression before the point.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (let ((subproblem (dstate/subproblem (debug-dstate (current-point)))))
      (subproblem-enter subproblem
			((ref-command continuation-browser-eval-last-sexp))
			avoid-deletion?))))

(define-command continuation-browser-return-from
  "Return FROM the current subproblem with a value.
Invoke the continuation that is waiting for the value of the current
subproblem on the value of the expression before the point.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (let ((next (guarantee-next-subproblem (debug-dstate (current-point)))))
      (subproblem-enter next
			((ref-command continuation-browser-eval-last-sexp))
			avoid-deletion?))))

(define-command continuation-browser-retry
  "Retry the expression of the current subproblem.
Prefix argument means do not kill the debugger buffer."
  "P"
  (lambda (avoid-deletion?)
    (let* ((dstate (debug-dstate (current-point)))
	   (next (guarantee-next-subproblem dstate)))
      (subproblem-enter
       next
       (let ((expression (dstate/expression dstate)))
	 (if (invalid-expression? expression)
	     (editor-error "Can't retry; invalid expression" expression))
	 (extended-scode-eval expression
			      (dstate-evaluation-environment dstate)))
       avoid-deletion?))))

(define-command continuation-browser-abort-all
  "Insert restarts"
  ()
  (lambda ()
    (continuation-browser-abort (reverse (current-restarts)))))

(define-command continuation-browser-abort-previous
  "Insert restarts"
  ()
  (lambda ()
    (continuation-browser-abort (current-restarts))))


(define-command continuation-browser-display-stack-elements
  "Show the elements on the current stack frame"
  "P"
  (lambda (argument)
    (let* ((point (current-point))
	   (dstate (debug-dstate point))
	   (sub (dstate/subproblem dstate)))
      (if (and (dstate/reduction-number dstate)
	       (not argument))
	  (editor-error "Reductions have no stack frames")
	  (call-with-interface-port
	   point
	   (lambda (port)
	     (push-current-mark! point)
	     (fresh-line port)
	     (let* ((vec (stack-frame/elements sub))
		    (depth (-1+ (vector-length vec)))
		    (mlen (string-length (number->string depth)))
		    (pad-len (max 5 mlen))
		    (padded
		     (lambda (s)
		       (string-pad-left s pad-len #\Space)))
		    (blanks (make-string pad-len #\Space)))

	       (write-string ";; " port)
	       (write-string (padded "Depth") port)
	       (write-string "  Bottom of stack frame" port)
	       (newline port)
	       (write-string ";;" port)
	       (let ((pad (if (= pad-len mlen)
			      padded
			      (let* ((right (quotient (- pad-len mlen) 2))
				     (rest (- pad-len right))
				     (blanks (make-string right #\Space)))
				(lambda (s)
				  (string-append
				   (string-pad-left s rest #\Space)
				   blanks))))))

		 (do ((elements (reverse! (vector->list vec))
				(cdr elements))
		      (depth depth (-1+ depth)))
		     ((null? elements))
		   (newline port)
		   (write-string ";; " port)
		   (write-string (pad (number->string depth)) port)
		   (write-string "  " port)
		   (write (car elements) port)))
	       (newline port)
	       (write-string ";;" port)
	       (newline port)
	       (write-string ";; " port)
	       (write-string blanks port)
	       (write-string "  Top of stack frame" port))
	     (newline port)
	     (newline port)))))))

(define (subproblem-enter subproblem value avoid-deletion?)
  (if (or (not (ref-variable debugger-confirm-return?))
	  (prompt-for-confirmation? "Continue with this value"))
      (invoke-continuation (stack-frame->continuation subproblem)
			   (list value)
			   avoid-deletion?)))

(define (invoke-continuation continuation arguments avoid-deletion?)
  (let ((buffer (current-buffer)))
    (if (and (not avoid-deletion?)
	     (ref-variable debugger-quit-on-return?))
	(kill-buffer-interactive buffer))
    ((or (buffer-get buffer 'INVOKE-CONTINUATION) apply)
     continuation arguments)))

(define (guarantee-next-subproblem dstate)
  (or (stack-frame/next-subproblem (dstate/subproblem dstate))
      (editor-error "Can't continue; no earlier subproblem")))

(define (current-restarts)
  (let* ((dstate (debug-dstate (current-point)))
	 (condition (dstate/condition dstate)))
    (if condition
	(condition/restarts condition)
	(bound-restarts))))

(define (continuation-browser-abort restarts)
  (let ((restart
	 (list-search-positive restarts
	   (lambda (restart)
	     (eq? (restart/name restart) 'abort)))))
    (if (not restart)
	(editor-error "Can't find an abort restart")
	(fluid-let ((hook/invoke-restart
		     (lambda (continuation arguments)
		       (invoke-continuation continuation
					    arguments
					    #f))))
	  (invoke-restart restart)))))

;;;; Marker Generation

(define (expand-subproblem mark)
  (let ((buffer (mark-buffer mark))
	(number (current-subproblem-number mark)))
    (if (not number)
	(editor-error "No subproblem or reduction marks"))
    (let ((number (+ number 1))
	  (count (count-subproblems buffer)))
      (if (>= number count)
	  (editor-error "No more subproblems or reductions"))
      (remove-more-subproblems-message buffer)
      (let ((port (mark->output-port mark)))
	(newline port)
	(print-subproblem number (nth-subproblem buffer number) port))
      (if (< number (- count 1))
	  (display-more-subproblems-message buffer)))))

(define (display-more-subproblems-message buffer)
  (define-variable-local-value! buffer (ref-variable-object mode-line-process)
    '(RUN-LIGHT (": more-subproblems " RUN-LIGHT) ": more-subproblems"))
  (buffer-modeline-event! buffer 'PROCESS-STATUS))

(define (remove-more-subproblems-message buffer)
  (let ((variable (ref-variable-object mode-line-process)))
    (define-variable-local-value! buffer variable
      (variable-default-value variable)))
  (buffer-modeline-event! buffer 'PROCESS-STATUS))

(define (perhaps-expand-reductions mark)
  (if (and (ref-variable debugger-expand-reductions?)
	   (not (reductions-expanded? mark)))
      (begin
	(message "Expanding reductions...")
	(expand-reductions (end-of-subproblem mark))
	(temporary-message "Expanding reductions...done"))))

(define (expand-reductions mark)
  (let ((port (mark->output-port mark))
	(subproblem-number (current-subproblem-number mark)))
    (do ((reductions (stack-frame/reductions
		      (dstate/subproblem (debug-dstate mark)))
		     (cdr reductions))
	 (reduction-number 0 (+ reduction-number 1)))
	((not (pair? reductions)))
      (newline port)
      (print-reduction subproblem-number
		       reduction-number
		       (car reductions)
		       port))))

(define (reductions-expanded? mark)
  ;; Return true whenever expansion is impossible at MARK, even if
  ;; because MARK is outside any subproblem or because there are no
  ;; reductions for the subproblem.  If only some of the reductions
  ;; appear already (e.g. if the others have been deleted by the
  ;; user), still return true.
  (let ((subproblem-above (find-previous-subproblem-marker mark)))
    (or (not subproblem-above)
	(let ((subproblem-number-above (re-match-extract-subproblem))
	      (reduction-count (re-match-extract-reduction-count)))
	  (and reduction-count
	       (let ((reduction-below
		      (find-next-marker
		       (line-end subproblem-above 0))))
		 (and reduction-below
		      (= (re-match-extract-subproblem)
			 subproblem-number-above))))))))


(define-structure (unparser-literal
		   (conc-name unparser-literal/)
		   (print-procedure
		    (lambda (state instance)
		      (unparse-string state
				      (unparser-literal/string instance))))
		   (constructor unparser-literal/make))
  string)

(define-variable subexpression-start-marker
  "Subexpressions are preceeded by this value."
  "#"
  string?)

(define-variable subexpression-end-marker
  "Subexpressions are followed by this value."
  "#"
  string?)

(define (print-subproblem number frame port)
  (with-values (lambda () (stack-frame/debugging-info frame))
    (lambda (expression environment subexpression)
      subexpression
      (print-history-level
       (stack-frame/compiled-code? frame)
       number
       (let ((reductions
	      (improper-list-length (stack-frame/reductions frame))))
	 (if (zero? reductions)
	     " -------- "
	     (string-append " #R=" (number->string reductions) " --- ")))
       (lambda ()
	 (cond ((debugging-info/compiled-code? expression)
		(write-string ";compiled code"))
	       ((not (debugging-info/undefined-expression? expression))
		(print-with-subexpression expression subexpression))
	       ((debugging-info/noise? expression)
		(write-string ((debugging-info/noise expression) #f)))
	       (else
		(write-string ";undefined expression"))))
       environment
       port))))

(define (print-with-subexpression expression subexpression)
  (fluid-let ((*unparse-primitives-by-name?* #t))
    (if (invalid-subexpression? subexpression)
	(write (unsyntax expression))
	(let ((sub (write-to-string (unsyntax subexpression))))
	  (write (unsyntax-with-substitutions
		  expression
		  (list
		   (cons subexpression
			 (unparser-literal/make
			  (string-append
			   (ref-variable subexpression-start-marker)
			   sub
			   (ref-variable subexpression-end-marker)))))))))))

(define (invalid-subexpression? subexpression)
  (or (debugging-info/undefined-expression? subexpression)
      (debugging-info/unknown-expression? subexpression)))

(define (print-reduction subproblem-number reduction-number reduction port)
  (print-history-level
   #f
   subproblem-number
   (string-append ", R=" (number->string reduction-number) " --- ")
   (lambda ()
     (print-reduction-as-subexpression (reduction-expression reduction)))
   (reduction-environment reduction)
   port))

(define (print-reduction-as-subexpression expression)
  (fluid-let ((*unparse-primitives-by-name?* #t))
    (write-string (ref-variable subexpression-start-marker))
    (write (unsyntax expression))
    (write-string (ref-variable subexpression-end-marker))))

(define (print-history-level compiled? subproblem-number reduction-id
			     expression-thunk environment port)
  (fresh-line port)
  (let ((level-identification
	 (string-append (if compiled? "-C- S=" "-I- S=")
			(number->string subproblem-number)
			reduction-id)))
    (write-string level-identification port)
    (let ((pad-width (max 0 (- 78 (string-length level-identification)))))
      (write-string
       (string-pad-right
	(string-append
	 (cdr (with-output-to-truncated-string pad-width expression-thunk))
	 " ")
	pad-width
	#\-)
       port)))
  (if (ref-variable debugger-verbose-mode?)
      (begin
	(newline port)
	(if (environment? environment)
	    (show-environment-name environment port)
	    (write-string "There is no environment stored for this frame."
			  port))))
  (if (ref-variable debugger-open-markers?)
      (newline port)))

;;;; Marker Location

(define forward-subproblem)
(define backward-subproblem)
(make-motion-pair (lambda (start)
		    (forward-one-level start find-next-subproblem-marker))
		  (lambda (start)
		    (backward-one-level start find-previous-subproblem-marker))
  (lambda (f b)
    (set! forward-subproblem f)
    (set! backward-subproblem b)
    unspecific))

(define forward-reduction)
(define backward-reduction)
(make-motion-pair (lambda (start)
		    (let ((mark (mark-right-inserting-copy start)))
		      (perhaps-expand-reductions mark)
		      (let ((result (forward-one-level mark find-next-marker)))
			(mark-temporary! mark)
			result)))
		  (lambda (start)
		    (let ((mark (mark-left-inserting-copy start)))
		      (if (below-subproblem-marker? mark)
			  (perhaps-expand-reductions
			   (backward-subproblem mark 1)))
		      (let ((result
			     (backward-one-level mark find-previous-marker)))
			(mark-temporary! mark)
			result)))
  (lambda (f b)
    (set! forward-reduction f)
    (set! backward-reduction b)
    unspecific))

(define (forward-one-level start finder)
  (let ((next-level (finder start)))
    (if next-level
	(let ((second-next-level
	       (find-next-marker
		(line-end next-level 0))))
	  (if second-next-level
	      (line-end second-next-level -1)
	      (group-end next-level)))
	(begin
	  (message "Expanding subproblem...")
	  (expand-subproblem (group-end start))
	  (temporary-message "Expanding subproblem...done")
	  (group-end start)))))

(define (backward-one-level start finder)
  (let ((level-top (finder start)))
    (if (or (not level-top) (not (finder level-top)))
	(editor-error "Can't move beyond top level"))
    (line-end level-top -1)))

(define (end-of-subproblem mark)
  (let ((subproblem-below (find-next-subproblem-marker mark)))
    (if subproblem-below
	(line-end subproblem-below -1)
	(group-end mark))))

(define (below-subproblem-marker? mark)
  (let ((mark (find-previous-marker mark)))
    (and mark
	 (re-match-forward subproblem-regexp mark))))

(define (region-contains-marker? region)
  (re-search-forward marker-regexp
		     (line-start (region-start region) 0)
		     (line-end (region-end region) 0)))

(define (current-subproblem-number mark)
  (and (find-previous-marker mark)
       (re-match-extract-subproblem)))

(define (current-reduction-number mark)
  (and (not (below-subproblem-marker? mark))
       (find-previous-reduction-marker mark)
       (re-match-extract-reduction)))

(define (find-next-subproblem-marker mark)
  (and (re-search-forward subproblem-regexp mark (group-end mark))
       (re-match-start 0)))

(define (find-next-reduction-marker mark)
  (and (re-search-forward reduction-regexp mark (group-end mark))
       (re-match-start 0)))

(define (find-next-marker mark)
  (and (re-search-forward marker-regexp mark (group-end mark))
       (re-match-start 0)))

(define (find-previous-subproblem-marker mark)
  (re-search-backward subproblem-regexp mark (group-start mark)))

(define (find-previous-reduction-marker mark)
  (re-search-backward reduction-regexp mark (group-start mark)))

(define (find-previous-marker mark)
  (re-search-backward marker-regexp mark (group-start mark)))

(define (re-match-extract-subproblem)
  (or (re-match-extract-number 1)
      (editor-error "Ill-formed subproblem marker")))

(define (re-match-extract-reduction)
  (or (re-match-extract-number 2)
      (editor-error "Ill-formed reduction marker")))

(define (re-match-extract-reduction-count)
  (re-match-extract-number 3))

(define (re-match-extract-number register-number)
  (let ((start (re-match-start register-number))
	(end (re-match-end register-number)))
    (and start
	 end
	 (string->number (extract-string end start)))))

;;; Regular expressions for finding subproblem and reduction marker
;;; lines.  After a match on REDUCTION-REGEXP, register 1 must match
;;; the subproblem number and register 2 must match the reduction
;;; number.  After a match on SUBPROBLEM-REGEXP, register 1 must
;;; match the subproblem number and register 3 must match the maximum
;;; reduction number in that subproblem.

(define subproblem-regexp
  "^-[CI]- S=\\([0-9]+\\) \\(#R=\\([0-9]+\\)\\|\\)")

(define reduction-regexp
  "^-I- S=\\([0-9]+\\), R=\\([0-9]+\\)")

(define marker-regexp
  "^-[CI]- S=\\([0-9]+\\)\\(, R=[0-9]+\\| #R=[0-9]+\\|\\)")

;;;; Debugger State

;;; UGLY BECAUSE IT MUTATES THE DSTATE.

(define (debug-dstate mark)
  (let ((dstate (buffer-dstate (mark-buffer mark))))
    (let ((subproblem-number (current-subproblem-number mark))
	  (reduction-number (current-reduction-number mark)))
      (if subproblem-number
	  (begin (change-subproblem! dstate subproblem-number)
		 (if (and reduction-number
			  (positive? (dstate/number-of-reductions dstate)))
		     (change-reduction! dstate reduction-number)
		     (set-dstate/reduction-number! dstate #f))
		 dstate)
	  (editor-error "Cannot find environment for evaluation.")))))

(define (change-subproblem! dstate subproblem-number)
  (let ((finish-move-to-subproblem!
	 (lambda (dstate)
	   (if (and (dstate/using-history? dstate)
		    (positive? (dstate/number-of-reductions dstate)))
	       (change-reduction! dstate 0)
	       (set-dstate/reduction-number! dstate #f))))
	(delta (- subproblem-number (dstate/subproblem-number dstate))))
    (if (negative? delta)
	(let ((subproblems
	       (list-tail (dstate/previous-subproblems dstate)
			  (-1+ (- delta)))))
	  (set-current-subproblem! dstate (car subproblems) (cdr subproblems))
	  (finish-move-to-subproblem! dstate))
	(let loop
	    ((subproblem (dstate/subproblem dstate))
	     (subproblems (dstate/previous-subproblems dstate))
	     (delta delta))
	  (if (zero? delta)
	      (begin
		(set-current-subproblem! dstate subproblem subproblems)
		(finish-move-to-subproblem! dstate))
	      (loop (stack-frame/next-subproblem subproblem)
		    (cons subproblem subproblems)
		    (-1+ delta)))))))

(define (change-reduction! dstate reduction-number)
  (set-dstate/reduction-number! dstate reduction-number)
  (set-dstate/environment-list!
   dstate
   (list (reduction-environment (dstate/reduction dstate)))))

(define (count-subproblems buffer)
  (do ((i 0 (1+ i))
       (subproblem (dstate/subproblem (buffer-dstate buffer))
		   (stack-frame/next-subproblem subproblem)))
      ((not subproblem) i)))

(define (nth-subproblem buffer n)
  (let ((dstate (buffer-dstate buffer)))
    (do ((frame
	  (let ((previous-subproblems (dstate/previous-subproblems dstate)))
	    (if (null? previous-subproblems)
		(dstate/subproblem dstate)
		(car (last-pair previous-subproblems))))
	  (or (stack-frame/next-subproblem frame)
	      (editor-error "No such subproblem" n)))
	 (level 0 (+ level 1)))
	((= level n) frame))))

(define (dstate-evaluation-environment dstate)
  (let ((environment-list (dstate/environment-list dstate)))
    (if (and (pair? environment-list)
	     (environment? (car environment-list)))
	(car environment-list)
	(let ((environment (ref-variable scheme-environment)))
	  (if (eq? 'DEFAULT environment)
	      (nearest-repl/environment)
	      (->environment environment))))))

;;;; Interface Port

(define (invoke-debugger-command mark command)
  (call-with-interface-port mark
    (lambda (port)
      (command (debug-dstate mark) port))))

(define (call-with-interface-port mark receiver)
  (let ((mark (mark-left-inserting-copy mark)))
    (let ((value (receiver (make-port interface-port-type mark))))
      (mark-temporary! mark)
      value)))

(define (operation/write-char port char)
  (guarantee-8-bit-char char)
  (region-insert-char! (port/state port) char))

(define (operation/write-substring port string start end)
  (region-insert-substring! (port/state port) string start end))

(define (operation/x-size port)
  (let ((buffer (mark-buffer (port/state port))))
    (and buffer
	 (let ((windows (buffer-windows buffer)))
	   (and (not (null? windows))
		(apply min (map window-x-size windows)))))))

(define (operation/debugger-failure port string)
  port
  (message string)
  (editor-beep))

(define (operation/debugger-message port string)
  port
  (message string))

(define (debugger-presentation port thunk)
  (fresh-line port)
  (fluid-let ((debugger-pp
	       (lambda (expression indentation port)
		 (pretty-print expression port #t indentation))))
    (thunk))
  (newline port)
  (newline port))

(define (operation/prompt-for-expression port environment prompt)
  port environment
  (prompt-for-expression prompt))

(define (operation/prompt-for-confirmation port prompt)
  port
  (prompt-for-confirmation? prompt))

(define interface-port-type
  (make-port-type
   `((WRITE-CHAR ,operation/write-char)
     (WRITE-SUBSTRING ,operation/write-substring)
     (X-SIZE ,operation/x-size)
     (DEBUGGER-FAILURE ,operation/debugger-failure)
     (DEBUGGER-MESSAGE ,operation/debugger-message)
     (DEBUGGER-PRESENTATION ,debugger-presentation)
     (PROMPT-FOR-EXPRESSION ,operation/prompt-for-expression)
     (PROMPT-FOR-CONFIRMATION ,operation/prompt-for-confirmation))
   #f))