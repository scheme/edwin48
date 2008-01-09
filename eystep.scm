#| -*-Scheme-*-

$Id: eystep.scm,v 1.11 2007/01/05 21:19:23 cph Exp $

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

;;;; Edwin Interface to YStep


(define-command step-expression
  "Single-step an expression."
  "xExpression to step"
  (lambda (expression)
    (with-stepper-evaluation-context
      (lambda ()
	(step-form expression (evaluation-environment #f))))))

(define-command step-last-sexp
  "Single-step the expression preceding point."
  ()
  (lambda ()
    (step-region (let ((point (current-point)))
		   (make-region (backward-sexp point 1 'ERROR) point))
		 (evaluation-environment #f))))

(define-command step-defun
  "Single-step the definition that the point is in or before."
  ()
  (lambda ()
    (step-region (let ((start (current-definition-start)))
		   (make-region start (forward-sexp start 1 'ERROR)))
		 (evaluation-environment #f))))

(define (step-region region environment)
  (with-stepper-evaluation-context
    (lambda ()
      (step-form (with-input-from-region region read) environment))))

(define (with-stepper-evaluation-context thunk)
  (bind-condition-handler (list condition-type:error)
      evaluation-error-handler
    (lambda ()
      (with-input-from-port dummy-i/o-port
	(lambda ()
	  (with-output-to-transcript-buffer thunk))))))

;;;; Stepper Mode

(define-major-mode stepper read-only-noarg "Stepper"
  "Major mode for the stepper.
space	advances the computation by one step
o	steps over the current expression
u	step over, but show the intervening events
U	step over, show and animate intervening events
r	run current expression to completion without stepping
e	expand the step under the cursor
c	contract the step under the cursor")

(define-key 'stepper #\space 'stepper-step)
(define-key 'stepper #\o 'stepper-step-over)
(define-key 'stepper #\u 'stepper-step-until)
(define-key 'stepper #\U 'stepper-step-until-visible)
(define-key 'stepper #\r 'stepper-run)
(define-key 'stepper #\e 'stepper-expand)
(define-key 'stepper #\c 'stepper-contract)
(define-key 'stepper #\? 'stepper-summary)

(define-command stepper-summary
  "Summarize the stepper commands in the typein window."
  ()
  (lambda ()
    (message "Space: single step, o: step over, e: expand, c: contract")))

(define-command stepper-step
  "Single step.  With argument, step multiple times."
  "p"
  (lambda (argument) (step-n (current-stepper-state) argument)))

(define-command stepper-run
  "Run current eval to completion without stepping."
  ()
  (lambda () (step-run (current-stepper-state))))

(define-command stepper-step-until
  "Step until current eval completes."
  ()
  (lambda () (step-until (current-stepper-state))))

(define-command stepper-step-until-visibly
  "Step until current eval completes, showing each step as it happens."
  ()
  (lambda () (step-until-visibly (current-stepper-state))))

(define-command stepper-step-over
  "Step over the current eval."
  ()
  (lambda () (step-over (current-stepper-state))))

(define-command stepper-expand
  "Expand the current step."
  ()
  (lambda ()
    (let ((state (current-stepper-state))
	  (node (current-node)))
      (ynode-expand! node)
      (edwin-step-output state #f `(EXPAND ,node)))))

(define-command stepper-contract
  "Contract the current step."
  ()
  (lambda () 
    (let ((state (current-stepper-state))
	  (node (current-node)))
      (ynode-contract! node)
      (edwin-step-output state #f `(CONTRACT ,node)))))

;;;; Stepper Output Interface

(define (initialize-package!)
  ;; Load the stepper and grab its output hooks.
  (load-option 'STEPPER)
  (set! step-output-initialize edwin-step-output-initialize)
  (set! step-output edwin-step-output)
  (set! step-output-final-result edwin-step-output-final-result)
  unspecific)

(define (edwin-step-output-initialize state)
  (select-buffer-other-window (get-stepper-buffer state)))

(define (get-stepper-buffer state)
  (let ((buffer (new-buffer "*Stepper*")))
    (add-kill-buffer-hook buffer kill-stepper-buffer)
    (buffer-put! buffer 'STEPPER-STATE state)
    (hash-table/put! stepper-buffers state buffer)
    (set-buffer-read-only! buffer)
    (set-buffer-major-mode! buffer (ref-mode-object stepper))
    buffer))

(define (kill-stepper-buffer buffer)
  (let ((state (buffer-get buffer 'STEPPER-STATE)))
    (if state
	(hash-table/remove! stepper-buffers state)))
  (buffer-remove! buffer 'STEPPER-STATE))

(define (buffer->stepper-state buffer)
  (or (buffer-get buffer 'STEPPER-STATE)
      (error:bad-range-argument buffer 'BUFFER->STEPPER-STATE)))

(define (stepper-state->buffer state)
  (or (hash-table/get stepper-buffers state #f)
      (get-stepper-buffer state)))

(define stepper-buffers
  (make-eq-hash-table))

(define (current-stepper-state)
  (buffer->stepper-state (current-buffer)))

(define (edwin-step-output-final-result state result)
  state
  (editor-error
   (string-append "Stepping terminated with result "
		  (write-to-string result))))

(define (current-node)
  (let ((point (current-point)))
    (or (get-text-property (mark-group point)
			   (mark-index point)
			   'STEPPER-NODE
			   #f)
	(editor-error "Point not pointing to stepper node."))))

(define (get-buffer-ynode-regions buffer)
  (or (buffer-get buffer 'YNODE-REGIONS)
      (let ((table (make-eq-hash-table)))
	(buffer-put! buffer 'YNODE-REGIONS table)
	table)))

(define (clear-ynode-regions! regions)
  (for-each mark-temporary! (hash-table/datum-list regions))
  (hash-table/clear! regions))

(define (ynode-start-mark regions node)
  (hash-table/get regions node #f))

(define (save-ynode-region! regions node start end)
  (hash-table/put! regions node (mark-temporary-copy start))
  (add-text-property (mark-group start) (mark-index start) (mark-index end)
		     'STEPPER-NODE node))

(define (edwin-step-output state redisplay? #!optional last-event)
  (let ((buffer (stepper-state->buffer state))
	(last-event
	 (if (default-object? last-event)
	     (stepper-last-event state)
	     last-event)))
    (let ((regions (get-buffer-ynode-regions buffer)))
      (clear-ynode-regions! regions)
      (with-read-only-defeated (buffer-start buffer)
	(lambda ()
	  (delete-string (buffer-start buffer) (buffer-end buffer))
	  (let ((node (stepper-root-node state))
		(start (mark-right-inserting-copy (buffer-start buffer)))
		(point (mark-left-inserting-copy (buffer-start buffer))))
	    (let loop ((node node) (level 0))
	      (if (not (eq? (ynode-type node) 'STEPPED-OVER))
		  (begin
		    (move-mark-to! start point)
		    (output-and-mung-region point
		      (lambda ()
			(let ((special (ynode-exp-special node)))
			  (if special
			      (begin
				(write-string ";")
				(write special))
			      (debugger-pp (ynode-exp node)
					   (* 2 level)
					   (current-output-port)))))
		      (and last-event
			   (eq? (car last-event) 'CALL)
			   (eq? (cadr last-event) node)
			   (lambda (region)
			     (highlight-region-excluding-indentation
			      region (highlight-face)))))
		    (insert-string (if (ynode-hidden-children? node)
				       " ===> "
				       " => ")
				   point)
		    (let ((value-node (ynode-value-node node)))
		      (output-and-mung-region point
			(lambda ()
			  (let ((node
				 (if (eq? (ynode-type node) 'STEP-OVER)
				     value-node
				     node)))
			    (let ((special (ynode-result-special node)))
			      (if special
				  (begin
				    (write-string ";")
				    (write special))
				  (write (ynode-result node))))))
			(and last-event
			     (eq? (car last-event) 'RETURN)
			     (eq? (cadr last-event) value-node)
			     (lambda (region)
			       (highlight-region region (highlight-face))))))
		    (insert-newline point)
		    (save-ynode-region! regions node start point)
		    (if (not (eq? 'STEP-OVER (ynode-type node)))
			(for-each (lambda (n) (loop n (+ level 1)))
				  (reverse (ynode-children node)))))))
	    (mark-temporary! point)
	    (mark-temporary! start))))
      (buffer-not-modified! buffer)
      (if last-event
	  (let ((start (ynode-start-mark regions (cadr last-event))))
	    (if start
		(set-buffer-point! buffer start))))))
  (if redisplay? (update-screens! '(IGNORE-INPUT))))

(define (output-and-mung-region point thunk region-munger)
  ;; Display something in the stepper buffer and then run something on
  ;; it.  REGION-MUNGER takes one argument, a region.
  (let ((start (mark-right-inserting-copy point)))
    (with-output-to-mark point thunk)
    (if region-munger (region-munger (make-region start point)))
    (mark-temporary! start)))

(initialize-package!)