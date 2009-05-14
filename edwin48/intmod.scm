#| -*-Scheme-*-

$Id: intmod.scm,v 1.129 2008/01/30 20:02:02 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Inferior REPL Mode
;;; Package: (edwin inferior-repl)


(define-variable repl-enable-transcript-buffer
  "If true, record input and output from inferior REPLs in transcript buffer.
This flag has effect only when ENABLE-TRANSCRIPT-BUFFER is also true."
  #t
  boolean?)

(define-variable repl-error-decision
  "Controls how errors in an inferior REPL are handled.
There are three meaningful values:
#F	a nested error REPL is started
PROMPT	the user is prompted to decide whether to start the debugger
6001	like PROMPT, except that the error is always aborted"
  'PROMPT
  (lambda (object) (or (boolean? object) (memv object '(6001 PROMPT)))))

(define-variable repl-mode-locked
  "If true, user cannot change the mode of REPL and CMDL buffers."
  #t
  boolean?)

(define-variable inferior-repl-write-results
  "If true, results of evaluation commands are written in the REPL buffer.
This includes evaluation of expressions in other buffers.
Otherwise, only evaluation of expressions in the REPL buffer itself do this."
  #t
  boolean?)

(define (call-with-transcript-output-mark buffer procedure)
  (if (and (ref-variable repl-enable-transcript-buffer buffer)
	   (ref-variable enable-transcript-buffer buffer))
      (call-with-transcript-buffer
       (lambda (buffer)
	 (procedure (buffer-end buffer))))
      (procedure #f)))

(define-command repl
  "Run an inferior read-eval-print loop (REPL), with I/O through a buffer.
With no arguments, selects the current evaluation buffer,
 or creates a new one if there is none.
With one C-u, creates a new REPL buffer unconditionally.
With two C-u's, creates a new REPL buffer with a new evaluation environment.
  (Otherwise USER-INITIAL-ENVIRONMENT is used.)"
  "p"
  (lambda (argument)
    (select-buffer
     (let ((buffer (current-buffer)))
       (let ((make-new
	      (lambda (environment)
		(let ((repl-buffer (new-buffer initial-buffer-name)))
		  (start-inferior-repl! repl-buffer environment #f)
		  repl-buffer))))
	 (if (>= argument 16)
	     (make-new
	      (extend-top-level-environment system-global-environment))
	     (or (and (< argument 4) (current-repl-buffer* buffer))
		 (make-new user-initial-environment))))))))

(define-command set-inferior-repl-buffer
  "Select an inferior REPL buffer for evaluating this buffer's contents.
Subsequent evaluation commands executed in the current buffer will be
evaluated in the specified inferior REPL buffer."
  (lambda ()
    (list
     (find-buffer
      (let ((buffers (repl-buffer-list)))
	(prompt-for-string-table-name "REPL buffer"
				      (and (pair? buffers)
					   (buffer-name (car buffers)))
				      (alist->string-table
				       (map (lambda (buffer)
					      (cons (buffer-name buffer)
						    buffer))
					    buffers))
				      'DEFAULT-TYPE 'VISIBLE-DEFAULT
				      'REQUIRE-MATCH? #t))
      #t)))
  (lambda (repl-buffer)
    (set-local-repl-buffer! (current-buffer) repl-buffer)))

(define (start-inferior-repl! buffer environment message)
  (set-buffer-major-mode! buffer (ref-mode-object inferior-repl))
  (if (ref-variable repl-mode-locked)
      (buffer-put! buffer 'MAJOR-MODE-LOCKED #t))
  (if (environment? environment)
      (local-set-variable! scheme-environment environment buffer))
  (create-thread editor-thread-root-continuation
    (lambda ()
      (let ((port
	     (make-interface-port buffer
				  (let ((thread (current-thread)))
				    (detach-thread thread)
				    thread))))
	(attach-buffer-interface-port! buffer port)
	(fluid-let ((%exit inferior-repl/%exit)
		    (quit inferior-repl/quit))
	  (dynamic-wind
	   (lambda () unspecific)
	   (lambda ()
	     (repl/start (make-repl #f
				    port
				    environment
				    #f
				    `((ERROR-DECISION ,error-decision))
				    user-initial-prompt)
			 (make-init-message message)))
	   (lambda ()
	     (signal-thread-event editor-thread
	       (lambda ()
		 (unwind-inferior-repl-buffer buffer))))))))))

(define (make-init-message message)
  (if message
      (cmdl-message/append cmdl-message/init-inferior message)
      cmdl-message/init-inferior))

(define cmdl-message/init-inferior
  (cmdl-message/active
   (lambda (port)
     (set-working-directory-pathname!
      (buffer-default-directory (port/buffer port))))))

(define (inferior-repl/%exit #!optional integer)
  (exit-current-thread (if (default-object? integer) 0 integer)))

(define (inferior-repl/quit)
  unspecific)

(define (current-repl-buffer #!optional buffer)
  (let ((repl-buffer (current-repl-buffer* buffer)))
    (if (not repl-buffer)
	(error "No REPL to evaluate in."))
    repl-buffer))

(define (current-repl-buffer* #!optional buffer)
  (let ((buffer (->buffer buffer)))
    (if (repl-buffer? buffer)
	buffer
	(or (local-repl-buffer buffer)
	    (global-repl-buffer)))))

(define (local-repl-buffer buffer)
  (or (let ((wp (buffer-get buffer 'REPL-BUFFER #f)))
	(and (weak-pair? wp)
	     (let ((repl-buffer (weak-car wp)))
	       (and (repl-buffer? repl-buffer)
		    (buffer-alive? repl-buffer)
		    repl-buffer))))
      (begin
	(buffer-remove! buffer 'REPL-BUFFER)
	#f)))

(define (set-local-repl-buffer! buffer repl-buffer)
  (if repl-buffer
      (begin
	(if (not (repl-buffer? repl-buffer))
	    (error:wrong-type-argument repl-buffer "REPL buffer"
				       'SET-LOCAL-REPL-BUFFER!))
	(buffer-put! buffer 'REPL-BUFFER (weak-cons repl-buffer #f)))
      (begin
	(undefine-variable-local-value! buffer (ref-variable-object run-light))
	(buffer-remove! buffer 'REPL-BUFFER))))

(define (global-repl-buffer)
  (let ((buffers (repl-buffer-list)))
    (and (pair? buffers)
	 (car buffers))))

(define (repl-buffer-list)
  (set! repl-buffers (filter buffer-alive? repl-buffers))
  repl-buffers)

(define (repl-buffer? buffer)
  (and (buffer? buffer)
       (buffer-interface-port buffer #f)))

(define repl-buffers)

(add-event-receiver! editor-initializations
  (lambda ()
    (set! repl-buffers '())
    unspecific))

(define (wait-for-input port mode ready? level)
  (signal-thread-event editor-thread
    (lambda ()
      (maybe-switch-modes! port mode)
      (let ((buffer (port/buffer port)))
	(local-set-variable!
	 mode-line-process
	 (list ": "
	       'RUN-LIGHT
	       (if (= level 1)
		   ""
		   (string-append " [level: " (number->string level) "]")))
	 buffer)
	(set-run-light! buffer #f))))
  ;; This doesn't do any output, but prods the editor to notice that
  ;; the modeline has changed and a redisplay is needed.
  (inferior-thread-output! (port/output-registration port))
  (do () ((ready? port))
    (suspend-current-thread)))

(define (end-input-wait port)
  (set-run-light! (port/buffer port) #t)
  (signal-thread-event (port/thread port) #f))

(define (standard-prompt-spacing port)
  (fresh-line port)
  (newline port)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      transcript?
      (undo-boundary! mark)
      #t)))

(define (maybe-switch-modes! port mode)
  (let ((buffer (port/buffer port)))
    (let ((mode* (buffer-major-mode buffer)))
      (if (not (eq? mode* mode))
	  (if (or (eq? mode* (ref-mode-object inferior-repl))
		  (eq? mode* (ref-mode-object inferior-cmdl)))
	      ;; Modes are compatible, so no need to reset the buffer's
	      ;; variables and properties.
	      (begin
		(without-interrupts
		 (lambda ()
		   (set-car! (buffer-modes buffer) mode)
		   (switch-comtabs! buffer mode mode*)))
		(buffer-modeline-event! buffer 'BUFFER-MODES))
	      (begin
		(set-buffer-major-mode! buffer mode)
		(attach-buffer-interface-port! buffer port)))))))

(define (switch-comtabs! buffer new-mode old-mode)
  (let ((comtabs (buffer-comtabs buffer))
	(new-comtabs (mode-comtabs new-mode))
	(old-comtabs (mode-comtabs old-mode)))
    (if (eq? comtabs old-comtabs)
	(set-buffer-comtabs! buffer new-comtabs)
	(let loop ((previous comtabs))
	  (let ((comtabs (cdr previous)))
	    (cond ((eq? comtabs old-comtabs)
		   (set-cdr! previous new-comtabs))
		  ((not (pair? comtabs))
		   (warn ";Buffer's comtabs do not match its mode:" buffer))
		  (else
		   (loop comtabs))))))))

(define (attach-buffer-interface-port! buffer port)
  (if (not (memq buffer repl-buffers))
      (set! repl-buffers (append! repl-buffers (list buffer))))
  (buffer-put! buffer 'INTERFACE-PORT port)
  (add-kill-buffer-hook buffer kill-buffer-inferior-repl)
  (buffer-put! buffer 'COMINT-PROCESS-MARK inferior-repl-process-mark)
  (local-set-variable! comint-input-ring (port/input-ring port) buffer)
  (local-set-variable! comint-last-input-end
		       (mark-right-inserting-copy (buffer-end buffer))
		       buffer)
  (local-set-variable! comint-last-input-match #f buffer)
  (set-run-light! buffer #f))

(define (buffer-interface-port buffer error?)
  (or (buffer-get buffer 'INTERFACE-PORT #f)
      (and error?
	   (error "No inferior REPL for this buffer:" buffer))))

(define (kill-buffer-inferior-repl buffer)
  (let ((port (buffer-interface-port buffer #f)))
    (if port
	(let ((thread (port/thread port)))
	  (if (not (thread-dead? thread))
	      (signal-thread-event thread
		(lambda ()
		  (exit-current-thread unspecific)))))))
  (unwind-inferior-repl-buffer buffer))

(define (unwind-inferior-repl-buffer buffer)
  (without-interrupts
   (lambda ()
     (let ((port (buffer-interface-port buffer #f)))
       (if port
	   (begin
	     (deregister-inferior-thread! (port/output-registration port))
	     (if (eq? buffer (global-run-light-buffer))
		 (set-global-run-light! #f))
	     (set! repl-buffers (delete! buffer repl-buffers eq?))
	     (let ((buffer (global-run-light-buffer)))
	       (if buffer
		   (set-global-run-light! (local-run-light buffer))))
	     (buffer-remove! buffer 'INTERFACE-PORT)))))))

(define (set-run-light! buffer run?)
  (let ((value (if run? "eval" "listen")))
    (if (eq? buffer (global-run-light-buffer))
	(set-global-run-light! value))
    (set-local-run-light! buffer value)
    (for-each (lambda (buffer*)
		(if (eq? buffer (local-repl-buffer buffer*))
		    (set-local-run-light! buffer* value)))
	      (buffer-list))))

(define (global-run-light-buffer)
  (and (evaluate-in-inferior-repl? #f)
       (global-repl-buffer)))

(define (set-global-run-light! value)
  (set-variable-default-value! (ref-variable-object run-light) value)
  (global-window-modeline-event!))

(define (local-run-light buffer)
  (ref-variable run-light buffer))

(define (set-local-run-light! buffer value)
  (local-set-variable! run-light value buffer)
  (buffer-modeline-event! buffer 'RUN-LIGHT))

(add-variable-assignment-daemon!
 (ref-variable-object evaluate-in-inferior-repl)
 (lambda (buffer variable) buffer variable (reset-run-light!)))

(define (reset-run-light!)
  (set-global-run-light!
   (let ((buffer (global-run-light-buffer)))
     (and buffer
	  (local-run-light buffer)))))

(define (error-decision repl condition)
  (let ((port (cmdl/port repl)))
    (if (interface-port? port)
	(let ((start-debugger
	       (lambda ()
		 (enqueue-output-operation! port
		   (lambda (mark transcript?)
		     mark
		     (if (not transcript?)
			 (start-continuation-browser port
						     condition))
		     #t)))))
	  (case (ref-variable repl-error-decision)
	    ((6001 #T)
	     (enqueue-output-operation! port
	       (lambda (mark transcript?)
		 (if (and (not transcript?)
			  (not (buffer-visible? (mark-buffer mark))))
		     (begin
		       (message "Evaluation error in "
				(buffer-name (mark-buffer mark))
				" buffer")
		       (editor-beep)))
		 #t))
	     (dynamic-wind
	      (lambda () unspecific)
	      (lambda ()
		(let loop ()
		  (fresh-line port)
		  (write-string
		   ";Type D to debug error, Q to quit back to REP loop: "
		   port)
		  (let ((char (read-command-char port (cmdl/level repl))))
		    (write-char char port)
		    (cond ((char-ci=? char #\d)
			   (fresh-line port)
			   (write-string ";Starting debugger..." port)
			   (start-debugger))
			  ((not (char-ci=? char #\q))
			   (beep port)
			   (loop))))))
	      cmdl-interrupt/abort-top-level))
	    ((PROMPT)
	     (if (let ((start? (ref-variable debug-on-evaluation-error #f)))
		   (if (eq? 'ASK start?)
		       (let loop ()
			 (fresh-line port)
			 (write-string ";Start debugger? (y or n): " port)
			 (let ((char
				(read-command-char port
						   (cmdl/level repl))))
			   (write-char char port)
			   (cond ((or (char-ci=? char #\y)
				      (char-ci=? char #\space))
				  (fresh-line port)
				  (write-string ";Starting debugger..."
						port)
				  #t)
				 ((or (char-ci=? char #\n)
				      (char-ci=? char #\rubout))
				  #f)
				 (else
				  (beep port)
				  (loop)))))
		       start?))
		 (start-debugger))))))))

;;;; Modes

(define-major-mode inferior-repl scheme "REPL"
  "Major mode for communicating with an inferior read-eval-print loop (REPL).
Editing and evaluation commands are like Scheme mode:

\\[lisp-indent-line] indents the current line for Scheme.
\\[indent-sexp] indents the next s-expression.
\\[scheme-complete-variable] completes the variable preceding point.
\\[show-parameter-list] shows the parameters of the call surrounding point.

\\[inferior-repl-eval-last-sexp] evaluates the expression preceding point.
\\[inferior-repl-eval-defun] evaluates the current definition.
\\[inferior-repl-eval-region] evaluates the current region.

When an error occurs, you can run the command-line debugger with (debug),
or you can run the windowed debugger:

\\[inferior-repl-debug] enters windowed debugger on the current error.

Expressions submitted for evaluation are saved in an expression history.
The history may be accessed with the following commands:

\\[comint-previous-input] cycles backwards through the history;
\\[comint-next-input] cycles forwards.
\\[comint-history-search-backward] searches backwards for a matching string;
\\[comint-history-search-forward] searches forwards.

The REPL may be controlled by the following commands:

\\[inferior-cmdl-abort-top-level] aborts evaluation, returns to top level.
\\[inferior-cmdl-abort-nearest] aborts evaluation, returns to current level.
\\[inferior-cmdl-abort-previous] aborts evaluation, goes up one level.
\\[inferior-cmdl-breakpoint] interrupts evaluation, enters a breakpoint.

\\{inferior-repl}"
  (lambda (buffer)
    (event-distributor/invoke! (ref-variable inferior-repl-mode-hook buffer)
			       buffer)))

(define-variable inferior-repl-mode-hook
  "An event distributor that is invoked when entering Inferior REPL mode."
  (make-event-distributor))

(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\b)) 'inferior-cmdl-breakpoint)
(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\c)) 'inferior-cmdl-abort-top-level)
(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\u)) 'inferior-cmdl-abort-previous)
(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\x)) 'inferior-cmdl-abort-nearest)

(define-key 'inferior-repl (kbd (meta #\o)) 'undefined)
(define-key 'inferior-repl (kbd (meta #\z)) 'inferior-repl-eval-defun)
(define-key 'inferior-repl (kbd ctrl meta #\z) 'inferior-repl-eval-region)
(define-key 'inferior-repl (kbd (ctrl #\x) (ctrl #\e)) 'inferior-repl-eval-last-sexp)

(define-key 'inferior-repl (kbd (meta #\p)) 'comint-previous-input)
(define-key 'inferior-repl (kbd (meta #\n)) 'comint-next-input)
(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\l)) 'comint-show-output)
(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\o)) 'inferior-repl-flush-output)
(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\r)) 'comint-history-search-backward)
(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\s)) 'comint-history-search-forward)
;;(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\u)) 'comint-kill-input)

(define-key 'inferior-repl (kbd (ctrl #\c) (ctrl #\d)) 'inferior-repl-debug)

(define-major-mode inferior-cmdl scheme "CMDL"
  "Major mode for communicating with an inferior command loop.
Like Scheme mode except that the evaluation commands are disabled,
and characters that would normally be self inserting are commands.
Typing ? will show you which characters perform useful functions.

Additionally, these commands abort the command loop:

\\[inferior-cmdl-abort-top-level] returns to the top-level REPL.
\\[inferior-cmdl-abort-previous] goes up one level to the previous REPL.
\\[inferior-cmdl-abort-nearest] returns to the current REPL.
\\[inferior-cmdl-breakpoint] enters a breakpoint REPL."
  (lambda (buffer)
    (event-distributor/invoke! (ref-variable inferior-cmdl-mode-hook buffer)
			       buffer)))

(define-variable inferior-cmdl-mode-hook
  "An event distributor that is invoked when entering Inferior CMDL mode."
  (make-event-distributor))

(define-key 'inferior-cmdl (kbd (ctrl #\c) (ctrl #\b)) 'inferior-cmdl-breakpoint)
(define-key 'inferior-cmdl (kbd (ctrl #\c) (ctrl #\c)) 'inferior-cmdl-abort-top-level)
(define-key 'inferior-cmdl (kbd (ctrl #\c) (ctrl #\u)) 'inferior-cmdl-abort-previous)
(define-key 'inferior-cmdl (kbd (ctrl #\c) (ctrl #\x)) 'inferior-cmdl-abort-nearest)

(define-key 'inferior-cmdl (kbd (meta #\o)) 'undefined)
(define-key 'inferior-cmdl (kbd (meta #\z)) 'undefined)
(define-key 'inferior-cmdl (kbd ctrl meta #\z) 'undefined)
(define-key 'inferior-cmdl (kbd (ctrl #\x) (ctrl #\e)) 'undefined)

(define-key 'inferior-cmdl (kbd (meta #\p)) 'undefined)
(define-key 'inferior-cmdl (kbd (meta #\n)) 'undefined)
(define-key 'inferior-cmdl (kbd (ctrl #\c) (ctrl #\r)) 'undefined)
(define-key 'inferior-cmdl (kbd (ctrl #\c) (ctrl #\s)) 'undefined)

(define-key 'inferior-cmdl char-set:graphic 'inferior-cmdl-self-insert)

;;;; Commands

(define (interrupt-command interrupt flush-queue?)
  (lambda ()
    (let ((port (buffer-interface-port (current-repl-buffer) #t)))
      (signal-thread-event (port/thread port) interrupt)
      (if flush-queue?
	  (flush-queue! (port/expression-queue port))))))

(define-command inferior-cmdl-breakpoint
  "Force the inferior REPL into a breakpoint."
  ()
  (interrupt-command cmdl-interrupt/breakpoint #f))

(define-command inferior-cmdl-abort-nearest
  "Force the inferior REPL back to the current level."
  ()
  (interrupt-command cmdl-interrupt/abort-nearest #t))

(define-command inferior-cmdl-abort-previous
  "Force the inferior REPL up to the previous level."
  ()
  (interrupt-command cmdl-interrupt/abort-previous #t))

(define-command inferior-cmdl-abort-top-level
  "Force the inferior REPL up to top level."
  ()
  (interrupt-command cmdl-interrupt/abort-top-level #t))

(define-command inferior-repl-eval-defun
  "Evaluate defun that point is in or before."
  ()
  (lambda ()
    (inferior-repl-eval-from-mark (current-definition-start))))

(define-command inferior-repl-eval-last-sexp
  "Evaluate the expression preceding point."
  ()
  (lambda ()
    (inferior-repl-eval-from-mark (backward-sexp (current-point) 1 'ERROR))))

(define (inferior-repl-eval-from-mark mark)
  ((ref-command inferior-repl-eval-region)
   (make-region mark (forward-sexp mark 1 'ERROR))))

(define-command inferior-repl-eval-region
  "Evaluate the region."
  "r"
  (lambda (region)
    (let ((buffer (mark-buffer (region-start region))))
      (comint-record-input (port/input-ring (buffer-interface-port buffer #t))
			   (region->string region))
      (inferior-repl-eval-region buffer region))))

(define-command inferior-repl-debug
  "Select a debugger buffer to examine the current REPL state.
If this is an error, the debugger examines the error condition."
  ()
  (lambda ()
    (temporary-message "Starting continuation browser...")
    (let ((port (buffer-interface-port (current-buffer) #t)))
      (start-continuation-browser
       port
       (let ((object
	      (let ((cmdl (port/inferior-cmdl port)))
		(or (and (repl? cmdl)
			 (repl/condition cmdl))
		    (thread-continuation (port/thread port))))))
	 (if (not object)
	     (editor-error "No error condition to debug."))
	 object)))))

(define (start-continuation-browser port condition)
  ((ref-command browse-continuation) condition)
  (buffer-put! (current-buffer) 'INVOKE-CONTINUATION
    (lambda (continuation arguments)
      (if (not (buffer-alive? (port/buffer port)))
	  (editor-error
	   "Can't continue; REPL buffer no longer exists!"))
      (signal-thread-event (port/thread port)
	(lambda ()
	  (apply continuation arguments))))))

(define (buffer/inferior-cmdl buffer)
  (let ((port (buffer-interface-port buffer #f)))
    (and port
	 (port/inferior-cmdl port))))

(define (port/inferior-cmdl port)
  (let ((thread (current-thread))
	(cmdl #f))
    (signal-thread-event (port/thread port)
      (lambda ()
	(set! cmdl (nearest-cmdl))
	(signal-thread-event thread #f)))
    (do () (cmdl)
      (suspend-current-thread))
    cmdl))

(define-command inferior-cmdl-self-insert
  "Send this character to the inferior debugger process."
  ()
  (lambda ()
    (let ((port (buffer-interface-port (current-buffer) #t)))
      (set-port/command-char! port (last-command-key))
      (end-input-wait port))))

(define-command inferior-repl-flush-output
  "Kill all output from REPL since last input."
  ()
  (lambda ()
    (let ((start
	   (let ((start (ref-variable comint-last-input-end)))
	     (if (and (not (line-start? start))
		      (eqv? #\newline (extract-right-char start)))
		 (mark1+ start)
		 start)))
	  (end (port/mark (buffer-interface-port (selected-buffer) #t))))
      (let ((value-mark
	     (re-search-backward flush-output-regexp end start #f)))
	(let ((start (mark-left-inserting-copy start))
	      (end (or value-mark end)))
	  (if (mark< start end)
	      (begin
		(delete-string start end)
		(guarantee-newline start)
		(insert-string "*** output flushed ***\n" start)))
	  (if value-mark
	      (let ((m
		     (re-match-forward ";Value [0-9]+: "
				       start (group-end start) #f)))
		(if m
		    (let ((e (line-end m 0)))
		      (if (> (- (mark-index e) (mark-index m)) 70)
			  (begin
			    (delete-string m e)
			    (insert-string "*** flushed ***" m)))))))
	  (mark-temporary! start))))))

(define flush-output-regexp
  (string-append "^;"
		 "\\("
		 "Unspecified return value$"
		 "\\|"
		 "Value: "
		 "\\|"
		 "Value [0-9]+: "
		 "\\|"
		 "Quit!$"
		 "\\)"))

(define (inferior-repl-eval-region buffer region)
  (inferior-repl-eval-ok? buffer)
  (call-with-transcript-output-mark buffer
    (lambda (mark)
      (if mark
	  (insert-region (region-start region)
			 (region-end region)
			 mark))))
  (let ((port (buffer-interface-port buffer #t)))
    (let ((input-end (inferior-repl-input-end buffer region)))
      (move-mark-to! (port/mark port) input-end)
      (move-mark-to! (ref-variable comint-last-input-end buffer) input-end))
    (let ((queue (port/expression-queue port)))
      (bind-condition-handler (list condition-type:error)
	  evaluation-error-handler
	(lambda ()
	  (for-each (let ((context
			   (if (eq? (group-buffer (region-group region))
				    buffer)
			       'REPL-BUFFER
			       'OTHER-BUFFER)))
		      (lambda (expression)
			(enqueue! queue (cons expression context))))
		    (read-expressions-from-region region))))
      (if (not (queue-empty? queue))
	  (end-input-wait port)))))

(define (inferior-repl-input-end buffer region)
  (receive (mark in-buffer?)
      (let ((end (buffer-end buffer))
	    (end* (region-end region)))
	(if (mark~ end end*)
	    (values end* #t)
	    (values end #f)))
    (let ((mark
	   (cond ((eqv? #\newline (extract-right-char mark))
		  (mark1+ mark))
		 ((line-start? mark)
		  mark)
		 (else
		  (let ((mark (mark-left-inserting-copy mark)))
		    (insert-newline mark)
		    (mark-temporary! mark)
		    mark)))))
      (if in-buffer?
	  (set-buffer-point! buffer mark))
      mark)))

(define (inferior-repl-eval-expression buffer expression)
  (inferior-repl-eval-ok? buffer)
  (call-with-transcript-output-mark buffer
    (lambda (mark)
      (if mark
	  (insert-string
	   (fluid-let ((*unparse-with-maximum-readability?* #t))
	     (write-to-string expression))
	   mark))))
  (let ((port (buffer-interface-port buffer #t)))
    ;;(move-mark-to! (port/mark port) (buffer-end buffer))
    (move-mark-to! (ref-variable comint-last-input-end buffer)
		   (port/mark port))
    (enqueue! (port/expression-queue port) (cons expression 'EXPRESSION))
    (end-input-wait port)))

(define (inferior-repl-eval-ok? buffer)
  (let ((mode (buffer-major-mode buffer)))
    (if (not (eq? mode (ref-mode-object inferior-repl)))
	(editor-error
	 (if (eq? mode (ref-mode-object inferior-cmdl))
	     "REPL needs response before evaluation will be enabled."
	     "Can't evaluate -- REPL buffer in anomalous mode.")))))

(define (inferior-repl-process-mark buffer)
  (port/mark (buffer-interface-port buffer #t)))

;;;; Queue

(define (make-queue)
  (cons '() '()))

(define (queue-empty? queue)
  (null? (car queue)))

(define (enqueue!/unsafe queue object)
  (let ((next (cons object '())))
    (if (null? (cdr queue))
	(set-car! queue next)
	(set-cdr! (cdr queue) next))
    (set-cdr! queue next)))

(define (dequeue!/unsafe queue empty)
  (let ((this (car queue)))
    (if (null? this)
	empty
	(begin
	  (set-car! queue (cdr this))
	  (if (null? (cdr this))
	      (set-cdr! queue '()))
	  (car this)))))

(define (enqueue! queue object)
  (without-interrupts
   (lambda () (enqueue!/unsafe queue object))))

(define (dequeue! queue empty)
  (without-interrupts
   (lambda ()
     (let ((value (dequeue!/unsafe queue empty)))
       value))))

(define (flush-queue! queue)
  (without-interrupts
   (lambda ()
     (set-car! queue '())
     (set-cdr! queue '()))))

;;;; Interface Port

(define (make-interface-port buffer thread)
  (letrec
      ((port
	(make-port interface-port-type
		   (make-interface-port-state
		    thread
		    (mark-right-inserting-copy (buffer-end buffer))
		    (register-inferior-thread!
		     thread
		     (lambda () (process-output-queue port)))))))
    port))

(define (interface-port? object)
  (and (port? object)
       (interface-port-state? (port/state object))))

(define-structure (interface-port-state
		   (conc-name interface-port-state/)
		   (constructor make-interface-port-state
				(thread mark output-registration)))
  (thread #f read-only #t)
  (mark #f read-only #t)
  (input-ring (make-ring (ref-variable comint-input-ring-size)) read-only #t)
  (expression-queue (make-queue) read-only #t)
  (current-queue-element #f)
  (command-char #f)
  (output-queue (make-queue) read-only #t)
  (output-strings '())
  (output-registration #f read-only #t)
  (bytes-written 0))

(define (port/thread port)
  (interface-port-state/thread (port/state port)))

(define (port/mark port)
  (interface-port-state/mark (port/state port)))

(define (port/buffer port)
  (mark-buffer (port/mark port)))

(define (port/input-ring port)
  (interface-port-state/input-ring (port/state port)))

(define (port/expression-queue port)
  (interface-port-state/expression-queue (port/state port)))

(define (port/current-queue-element port)
  (interface-port-state/current-queue-element (port/state port)))

(define (set-port/current-queue-element! port element)
  (set-interface-port-state/current-queue-element! (port/state port) element))

(define (port/command-char port)
  (interface-port-state/command-char (port/state port)))

(define (set-port/command-char! port command-char)
  (set-interface-port-state/command-char! (port/state port) command-char))

(define (port/output-queue port)
  (interface-port-state/output-queue (port/state port)))

(define (port/output-strings port)
  (interface-port-state/output-strings (port/state port)))

(define (set-port/output-strings! port strings)
  (set-interface-port-state/output-strings! (port/state port) strings))

(define (port/output-registration port)
  (interface-port-state/output-registration (port/state port)))

(define (port/bytes-written port)
  (interface-port-state/bytes-written (port/state port)))

(define (set-port/bytes-written! port n)
  (set-interface-port-state/bytes-written! (port/state port) n))

;;; Output operations

(define (operation/write-char port char)
  (guarantee-8-bit-char char)
  (enqueue-output-string! port (string char))
  1)

(define (operation/write-substring port string start end)
  (enqueue-output-string! port (substring string start end))
  (fix:- end start))

(define (operation/beep port)
  (enqueue-output-operation!
   port
   (lambda (mark transcript?) mark (if (not transcript?) (editor-beep)) #t)))

(define (operation/x-size port)
  (let ((buffer (port/buffer port)))
    (and buffer
	 (let ((windows (buffer-windows buffer)))
	   (and (not (null? windows))
		(apply min (map window-x-size windows)))))))

(define (operation/write-result port expression value hash-number environment)
  (let ((buffer (port/buffer port))
	(other-buffer?
	 (memq (operation/current-expression-context port expression)
	       '(OTHER-BUFFER EXPRESSION))))
    (if (and other-buffer?
	     (not (ref-variable inferior-repl-write-results buffer)))
	(transcript-write value
			  (and (ref-variable enable-transcript-buffer buffer)
			       (transcript-buffer)))
	(begin
	  (default/write-result port expression value hash-number environment)
	  (if (and other-buffer? (not (mark-visible? (port/mark port))))
	      (transcript-write value #f))))))

(define (mark-visible? mark)
  (any (lambda (window)
	 (window-mark-visible? window mark))
       (buffer-windows (mark-buffer mark))))

(define (enqueue-output-string! port string)
  (without-interrupts
    (lambda ()
      (set-port/output-strings! port (cons string (port/output-strings port)))n
      (set-port/bytes-written! port
			       (+ (port/bytes-written port)
				  (string-length string)))
      (inferior-thread-output!/unsafe (port/output-registration port)))))

;;; We assume here that none of the OPERATORs passed to this procedure
;;; generate any output in the REPL buffer, and consequently we don't
;;; need to update bytes-written here.  Review of the current usage of
;;; this procedure confirms the assumption. 

(define (enqueue-output-operation! port operator)
  (without-interrupts
   (lambda ()
     (let ((strings (port/output-strings port)))
       (if (not (null? strings))
	   (begin
	     (set-port/output-strings! port '())
	     (enqueue!/unsafe
	      (port/output-queue port)
	      (let ((string (apply string-append (reverse! strings))))
		(lambda (mark transcript?)
		  transcript?
		  (region-insert-string! mark string)
		  #t))))))
     (enqueue!/unsafe (port/output-queue port) operator)
     (inferior-thread-output!/unsafe (port/output-registration port)))))

(define (process-output-queue port)
  (without-interrupts
   (lambda ()
     (let ((result #t))
       (let ((mark (mark-left-inserting-copy (port/mark port))))
	 (call-with-transcript-output-mark (port/buffer port)
	   (lambda (transcript-mark)
	     (let ((run-operation
		    (lambda (operation mark transcript?)
		      (let ((flag (operation mark transcript?)))
			(if (eq? flag 'FORCE-RETURN)
			    (set! result flag)))
		      unspecific)))
	       (let loop ()
		 (let ((operation (dequeue!/unsafe (port/output-queue port) #f)))
		   (if operation
		       (begin
			 (run-operation operation mark #f)
			 (if transcript-mark
			     (run-operation operation transcript-mark #t))
			 (loop))))))
	     (let ((strings (port/output-strings port)))
	       (if (not (null? strings))
		   (begin
		     (set-port/output-strings! port '())
		     (do ((strings (reverse! strings) (cdr strings)))
			 ((null? strings))
		       (region-insert-string! mark (car strings))
		       (if transcript-mark
			   (region-insert-string! transcript-mark
						  (car strings)))))))))
	 (move-mark-to! (port/mark port) mark)
	 (mark-temporary! mark))
       result))))

;;; Input operations

(define (operation/read-char port)
  (error "READ-CHAR not supported on this port:" port))

(define (operation/read port parser-table)
  parser-table
  (read-expression port (nearest-cmdl/level)))

(define read-expression
  (let ((empty (cons '() '())))
    (lambda (port level)
      (let ((queue (port/expression-queue port))
	    (mode (ref-mode-object inferior-repl))
	    (ready?
	     (lambda (port)
	       (not (queue-empty? (port/expression-queue port))))))
	(let loop ()
	  (let ((element (dequeue! queue empty)))
	    (if (eq? element empty)
		(begin
		  (wait-for-input port mode ready? level)
		  (loop))
		(begin
		  (set-port/current-queue-element! port element)
		  (car element)))))))))

(define (operation/current-expression-context port expression)
  (let ((element (port/current-queue-element port)))
    (and (pair? element)
	 (eq? (car element) expression)
	 (cdr element))))

;;; Debugger

(define (operation/debugger-failure port string)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      mark
      (if (not transcript?)
	  (begin
	    (message string)
	    (editor-beep)))
      #t)))

(define (operation/debugger-message port string)
  (enqueue-output-operation!
   port
   (lambda (mark transcript?)
     mark
     (if (not transcript?) (message string))
     #t)))

(define (operation/debugger-presentation port thunk)
  (fresh-line port)
  (thunk))

;;; Prompting

(define (operation/prompt-for-expression port environment prompt)
  (unsolicited-prompt port
		      (lambda (prompt)
			(prompt-for-expression prompt #!default environment))
		      prompt))

(define (operation/prompt-for-confirmation port prompt)
  (unsolicited-prompt port prompt-for-confirmation? prompt))

(define unsolicited-prompt
  (let ((wait-value (list #f))
	(abort-value (list #f)))
    (lambda (port procedure prompt)
      (let ((value wait-value))
	(signal-thread-event editor-thread
	  (lambda ()
	    ;; This would be even better if it could notify the user
	    ;; that the inferior REPL wanted some attention.
	    (when-buffer-selected (port/buffer port)
	      (lambda ()
		;; We're using ENQUEUE-OUTPUT-OPERATION! here solely
		;; to force KEYBOARD-READ to exit so that the command
		;; reader loop will get control and notice the command
		;; override.
		(enqueue-output-operation! port
		  (lambda (mark transcript?)
		    mark transcript?
		    (if (not transcript?)
			(override-next-command!
			 (lambda ()
			   (let ((continue
				  (lambda (v)
				    (set! value v)
				    (signal-thread-event (port/thread port)
				      #f))))
			     (bind-condition-handler
				 (list condition-type:abort-current-command)
				 (lambda (condition)
				   (continue abort-value)
				   (signal-condition condition))
			       (lambda ()
				 (continue (procedure prompt))))))))
		    'FORCE-RETURN))))))
	(let loop ()
	  (cond ((eq? value wait-value) (suspend-current-thread) (loop))
		((eq? value abort-value) (abort->nearest))
		(else value)))))))

(define (when-buffer-selected buffer thunk)
  (if (current-buffer? buffer)
      (thunk)
      (letrec ((hook (lambda (buffer window)
		       (if (current-window? window)
			   (begin
			     (thunk)
			     (remove-select-buffer-hook buffer hook))))))
	(add-select-buffer-hook buffer hook))))

(define (operation/prompt-for-command-expression port environment prompt level)
  environment
  (parse-command-prompt port prompt)
  (read-expression port level))

(define (operation/prompt-for-command-char port prompt level)
  (parse-command-prompt port prompt)
  (read-command-char port level))

(define (read-command-char port level)
  (set-port/command-char! port #f)
  (wait-for-input port (ref-mode-object inferior-cmdl) port/command-char level)
  (port/command-char port))

(define (parse-command-prompt port prompt)
  (standard-prompt-spacing port)
  (if (and (pair? prompt)
	   (eq? 'STANDARD (car prompt)))
      (if (not suppress-standard-prompts?)
	  (write-string (cdr prompt) port))
      (write-string prompt port)))

(define suppress-standard-prompts? #t)

;;; Miscellaneous

(define (operation/set-default-directory port directory)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      (if (not transcript?)
	  (begin
	    (set-buffer-default-directory! (mark-buffer mark) directory)
	    ;;(message (->namestring directory))
	    ))
      #t)))

(define (operation/set-default-environment port environment)
  (enqueue-output-operation! port
    (lambda (mark transcript?)
      (if (not transcript?)
	  (local-set-variable! scheme-environment environment
			       (mark-buffer mark)))
      #t)))

(define interface-port-type
  (make-port-type
   `((WRITE-CHAR ,operation/write-char)
     (WRITE-SUBSTRING ,operation/write-substring)
     (BEEP ,operation/beep)
     (X-SIZE ,operation/x-size)
     (BYTES-WRITTEN ,port/bytes-written)
     (DEBUGGER-FAILURE ,operation/debugger-failure)
     (DEBUGGER-MESSAGE ,operation/debugger-message)
     (DEBUGGER-PRESENTATION ,operation/debugger-presentation)
     (PROMPT-FOR-EXPRESSION ,operation/prompt-for-expression)
     (PROMPT-FOR-CONFIRMATION ,operation/prompt-for-confirmation)
     (PROMPT-FOR-COMMAND-EXPRESSION ,operation/prompt-for-command-expression)
     (PROMPT-FOR-COMMAND-CHAR ,operation/prompt-for-command-char)
     (SET-DEFAULT-DIRECTORY ,operation/set-default-directory)
     (SET-DEFAULT-ENVIRONMENT ,operation/set-default-environment)
     (READ-CHAR ,operation/read-char)
     (READ ,operation/read)
     (CURRENT-EXPRESSION-CONTEXT ,operation/current-expression-context)
     (WRITE-RESULT ,operation/write-result))
   #f))