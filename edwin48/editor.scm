#| -*-Scheme-*-

$Id: editor.scm,v 1.266 2008/01/30 20:02:00 cph Exp $

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

;;;; Editor Top Level


(define (edit . args)
  (call-with-current-continuation
   (lambda (continuation)
     (cond (within-editor?
	    (error "edwin: Editor already running"))
	   ((not edwin-editor)
	    (apply create-editor args))
	   ((not (null? args))
	    (error "edwin: Arguments ignored when re-entering editor" args))
	   (edwin-continuation
	    => (lambda (restart)
		 (set! edwin-continuation #f)
		 (within-continuation restart
		   (lambda ()
		     (set! editor-abort continuation)
		     unspecific)))))
     (fluid-let ((editor-abort continuation)
		 (current-editor edwin-editor)
		 (within-editor? #t)
		 (editor-thread (current-thread))
		 (editor-thread-root-continuation unspecific)
		 (editor-initial-threads '())
		 (inferior-thread-changes? #f)
		 (inferior-threads '())
		 (recursive-edit-continuation #f)
		 (recursive-edit-level 0))
       (editor-grab-display edwin-editor
	 (lambda (with-editor-ungrabbed operations)
	   (let ((message (cmdl-message/null)))
	     (cmdl/start
	      (make-cmdl
	       (nearest-cmdl)
	       dummy-i/o-port
	       (lambda (cmdl)
		 cmdl		;ignore
		 (bind-condition-handler (list condition-type:error)
		     internal-error-handler
		   (lambda ()
		     (call-with-current-continuation
		      (lambda (root-continuation)
			(set! editor-thread-root-continuation
			      root-continuation)
			(with-notification-output-port null-output-port
			  (lambda ()
			    (do ((thunks (let ((thunks editor-initial-threads))
					   (set! editor-initial-threads '())
					   thunks)
					 (cdr thunks)))
				((null? thunks))
			      (create-thread root-continuation (car thunks)))
			    (top-level-command-reader
			     edwin-initialization)))))))
		 message)
	       #f
	       `((START-CHILD ,(editor-start-child-cmdl with-editor-ungrabbed))
		 (CHILD-PORT ,(editor-child-cmdl-port (nearest-cmdl/port)))
		 ,@operations))
	      message))))))))

(define (edwin . args) (apply edit args))
;; (simple-command-line-parser "edit" edit)

(define edwin-editor #f)
(define editor-abort unspecific)
(define current-editor unspecific)
(define within-editor? #f)
(define editor-thread unspecific)
(define editor-thread-root-continuation unspecific)
(define editor-initial-threads unspecific)
(define edwin-continuation unspecific)

;; Set this before entering the editor to get something done after the
;; editor's dynamic environment is initialized, but before the command
;; loop is started.
(define edwin-initialization #f)

(define (queue-initial-thread thunk)
  (set! editor-initial-threads (cons thunk editor-initial-threads))
  unspecific)

(define create-editor-args
  '())

(define (create-editor . args)
  (let ((args
	 (if (null? args)
	     create-editor-args
	     (begin
	       (set! create-editor-args args)
	       args))))
    (reset-editor)
    (event-distributor/invoke! editor-initializations)
    (set! edwin-editor
	  (make-editor "Edwin"
		       (let ((name (and (not (null? args)) (car args))))
			 (if name
			     (let ((type (name->display-type name)))
			       (if (not type)
				   (error "Unknown display type name:" name))
			       (if (not (display-type/available? type))
				   (error "Requested display type unavailable:"
					  type))
			       type)
			     (default-display-type '())))
		       (if (null? args) '() (cdr args))))
    (set! edwin-initialization
	  (lambda ()
	    (set! edwin-initialization #f)
	    (standard-editor-initialization)))
    (set! edwin-continuation #f)
    unspecific))

(define editor-initializations
  (make-event-distributor))

(define (default-display-type preferences)
  (define (fail)
    (error "Can't find any usable display type"))

  (define (find-any)
    (let ((types (editor-display-types)))
      (if (null? types)
	  (fail)
	  (car types))))

  (define (find-preferred display-type-names)
    (if (null? display-type-names)
	(find-any)
	(let ((next (name->display-type (car display-type-names))))
	  (if (and next 
		   (display-type/available? next))
	      next
	      (find-preferred (cdr display-type-names))))))

  (find-preferred preferences))

(define (standard-editor-initialization)
  (with-editor-interrupts-disabled
   (lambda ()
     (if (and (not init-file-loaded?)
	      (not inhibit-editor-init-file?))
	 (begin
	   (let ((filename (os/init-file-name)))
	     (if (file-exists? filename)
		 (load-edwin-file filename '(EDWIN) #t)))
	   (set! init-file-loaded? #t)
	   unspecific))))
  (let ((buffer (find-buffer initial-buffer-name)))
    (if (and buffer
	     (not inhibit-initial-inferior-repl?))
	(start-inferior-repl!
	 buffer
	 (nearest-repl/environment)
	 (and (not (ref-variable inhibit-startup-message))
	      (cmdl-message/append
	       (cmdl-message/active
		(lambda (port)
		  (identify-world port)
		  (newline port)))
	       (cmdl-message/strings
		"You are in an interaction window of the Edwin editor."
                "Type `C-h' for help, or `C-h t' for a tutorial."
                "`C-h m' will describe some commands."
                "`C-h' means: hold down the Ctrl key and type `h'.")))))))

(define inhibit-editor-init-file? #f)
(define init-file-loaded? #f)
(define inhibit-initial-inferior-repl? #f)

(define-variable inhibit-startup-message
  "True inhibits the initial startup messages.
This is for use in your personal init file, once you are familiar
with the contents of the startup message."
  #f
  boolean?)

(define (reset-editor)
  (without-interrupts
   (lambda ()
     (if edwin-editor
	 (begin
	   (for-each (lambda (screen)
		       (screen-discard! screen))
		     (editor-screens edwin-editor))
	   (set! edwin-editor #f)
	   (set! edwin-continuation unspecific)
	   (set! init-file-loaded? #f)
	   (weak-set-car! *previous-popped-up-window* #f)
	   (weak-set-car! *previous-popped-up-buffer* #f)
	   (weak-set-car! *minibuffer-scroll-window* #f)
	   unspecific)))))

(define (reset-editor-windows)
  (for-each (lambda (screen)
	      (send (screen-root-window screen) ':salvage!))
	    (editor-screens edwin-editor)))

(define (enter-recursive-edit)
  (let ((value
	 (call-with-current-continuation
	   (lambda (continuation)
	     (fluid-let ((recursive-edit-continuation continuation)
			 (recursive-edit-level (1+ recursive-edit-level)))
	       (let ((recursive-edit-event!
		      (lambda ()
			(for-each (lambda (window)
				    (window-modeline-event! window
							    'RECURSIVE-EDIT))
				  (window-list)))))
		 (dynamic-wind recursive-edit-event!
			       command-reader
			       recursive-edit-event!)))))))
    (if (eq? value 'ABORT)
	(abort-current-command)
	(begin
	  (reset-command-prompt!)
	  value))))

(define (exit-recursive-edit value)
  (if recursive-edit-continuation
      (recursive-edit-continuation value)
      (editor-error "No recursive edit is in progress")))

(define recursive-edit-continuation unspecific)
(define recursive-edit-level unspecific)

(define (editor-gc-daemon)
  (let ((editor edwin-editor))
    (if editor
	(do ((buffers (bufferset-buffer-list (editor-bufferset editor))
		      (cdr buffers)))
	    ((null? buffers))
	  (clean-group-marks! (buffer-group (car buffers)))))))

(add-gc-daemon!/no-restore editor-gc-daemon)
(add-event-receiver! event:after-restore editor-gc-daemon)

;;;; Error handling

(define (internal-error-handler condition)
  (cond ((and (eq? condition-type:primitive-procedure-error
		   (condition/type condition))
	      (let ((operator (access-condition condition 'OPERATOR)))
		(or (eq? operator (ucode-primitive x-display-process-events 2))
		    (eq? operator (ucode-primitive x-display-flush 1)))))
	 ;; This error indicates that the connection to the X server
	 ;; has been broken.  The safest thing to do is to kill the
	 ;; editor.
	 (exit-editor))
	(debug-internal-errors?
	 (error condition))
	(else
	 (maybe-debug-scheme-error 'INTERNAL condition))))

(define (maybe-debug-scheme-error error-type condition)
  (let ((p
	 (variable-default-value
	  (or (name->variable (symbol-append 'DEBUG-ON- error-type '-ERROR) #f)
	      (ref-variable-object debug-on-internal-error)))))
    (if p
	(debug-scheme-error error-type condition (eq? p 'ASK))))
  (standard-error-report error-type condition #f)
  (editor-beep)
  (return-to-command-loop condition))

(define-variable debug-on-internal-error
  "True means enter debugger if an internal error is signalled.
False means ignore the error and resume editing (this is the default value).
The symbol ASK means ask what to do.
This does not affect editor errors or evaluation errors."
  #f
  (lambda (x) (or (boolean? x) (eq? x 'ASK))))

(define debug-internal-errors? #f)

(define condition-type:editor-error
  (make-condition-type 'EDITOR-ERROR condition-type:error '(STRINGS)
    (lambda (condition port)
      (write-string (message-args->string (editor-error-strings condition))
		    port))))

(define editor-error
  (let ((signaller
	 (condition-signaller condition-type:editor-error
			      '(STRINGS)
			      standard-error-handler)))
    (lambda strings
      (signaller strings))))

(define editor-error-strings
  (condition-accessor condition-type:editor-error 'STRINGS))

(define (editor-error-handler condition)
  (maybe-debug-scheme-error 'EDITOR condition))

(define-variable debug-on-editor-error
  "True means enter debugger if an editor error is signalled.
False means ignore the error and resume editing (this is the default value).
The symbol ASK means ask what to do.
This does not affect internal errors or evaluation errors."
  #f
  (lambda (x) (or (boolean? x) (eq? x 'ASK))))

(define (standard-error-report error-type condition in-prompt?)
  (let ((type-string
	 (string-append (string-titlecase (symbol->string error-type))
			" error"))
	(report-string (condition/report-string condition))
	(get-error-buffer
	 (lambda strings
	   (string->temporary-buffer (apply string-append strings)
				     "*error*"
				     '(SHRINK-WINDOW)))))
    (let ((typein-report
	   (lambda ()
	     (if (eq? error-type 'EDITOR)
		 (message report-string)
		 (message type-string ": " report-string))))
	  (error-buffer-report
	   (lambda ()
	     (if in-prompt?
		 (if (eq? error-type 'EDITOR)
		     (get-error-buffer report-string)
		     (get-error-buffer type-string ":\n" report-string))
		 (begin
		   (get-error-buffer report-string)
		   (message type-string)))
	     (update-screens! #f)))
	  (transcript-report
	   (lambda ()
	     (and (ref-variable enable-transcript-buffer)
		  (begin
		    (with-output-to-transcript-buffer
			(lambda ()
			  (fresh-line)
			  (write-string ";")
			  (write-string type-string)
			  (write-string ": ")
			  (write-string report-string)
			  (newline)
			  (newline)))
		    #t)))))
      (let ((fit-report
	     (lambda ()
	       (if (and (not in-prompt?)
			(not (string-index report-string #\newline))
			(< (string-columns report-string 0 8
					   (ref-variable char-image-strings
							 #f))
			   (window-x-size (typein-window))))
		   (typein-report)
		   (error-buffer-report)))))
	(case (ref-variable error-display-mode)
	  ((STANDARD) (transcript-report) (fit-report))
	  ((TRANSCRIPT) (or (transcript-report) (fit-report)))
	  ((ERROR-BUFFER) (error-buffer-report))
	  ((TYPEIN) (if in-prompt? (error-buffer-report) (typein-report)))
	  ((FIT) (fit-report)))))))

(define-variable error-display-mode
  "Value of this variable controls the way evaluation error messages
are displayed:
STANDARD      like FIT, except messages also appear in transcript buffer,
                if it is enabled (this is the default value).
FIT           messages appear in typein window if they fit;
                in \"*error*\" buffer if they don't.
TYPEIN        messages appear in typein window.
ERROR-BUFFER  messages appear in \"*error*\" buffer.
TRANSCRIPT    messages appear in transcript buffer, if it is enabled;
                otherwise this is the same as FIT."
  'STANDARD
  (lambda (value) (memq value '(STANDARD TRANSCRIPT ERROR-BUFFER TYPEIN FIT))))

;;;; Abort and quit

(define condition-type:abort-current-command
  (make-condition-type 'ABORT-CURRENT-COMMAND #f '(INPUT)
    (lambda (condition port)
      (write-string "Abort current command" port)
      (let ((input (abort-current-command/input condition)))
	(if input
	    (begin
	      (write-string " with input: " port)
	      (write input port))))
      (write-string "." port))))

(define condition/abort-current-command?
  (condition-predicate condition-type:abort-current-command))

(define abort-current-command/input
  (condition-accessor condition-type:abort-current-command 'INPUT))

(define abort-current-command
  (let ((signaller
	 (condition-signaller condition-type:abort-current-command
			      '(INPUT)
			      standard-error-handler)))
    (lambda input
      (let ((input (if (null? input) #f input)))
	(if (not (or (not input) (input-event? input)))
	    (error:wrong-type-argument input "input event"
				       'ABORT-CURRENT-COMMAND))
	(signaller input)))))

(define condition-type:^G
  (make-condition-type '^G condition-type:abort-current-command '()
    (lambda (condition port)
      condition
      (write-string "Signal editor ^G." port))))

(define condition/^G?
  (condition-predicate condition-type:^G))

(define ^G-signal
  (let ((signaller
	 (condition-signaller condition-type:^G
			      '(INPUT)
			      standard-error-handler)))
    (lambda ()
      (signaller #f))))

(define (quit-editor-and-signal-error condition)
  (quit-editor-and (lambda () (error condition))))

(define (quit-editor)
  (quit-editor-and (lambda () *the-non-printing-object*)))

(define (quit-scheme)
  (let ((dir (buffer-default-directory (current-buffer))))
    (quit-editor-and (lambda () (os/quit dir) (edit)))))

(define (quit-editor-and thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (within-continuation editor-abort
       (lambda ()
	 (set! edwin-continuation continuation)
	 (thunk))))))

(define (exit-editor)
  (within-continuation editor-abort reset-editor))

(define (exit-scheme)
  (within-continuation editor-abort %exit))

(define (editor-grab-display editor receiver)
  (display-type/with-display-grabbed (editor-display-type editor)
    (lambda (with-display-ungrabbed operations)
      (with-current-local-bindings!
	(lambda ()
	  (let ((enter
		 (lambda ()
		   (let ((screen (selected-screen)))
		     (screen-enter! screen)
		     (update-screen! screen #t))))
		(exit
		 (lambda ()
		   (screen-exit! (selected-screen)))))
	    (dynamic-wind enter
			  (lambda ()
			    (receiver
			     (lambda (thunk)
			       (dynamic-wind exit
					     (lambda ()
					       (with-display-ungrabbed thunk))
					     enter))
			      operations))
			  exit)))))))

(define dummy-i/o-port
  (make-port (make-port-type
	      (map (lambda (name)
		     (list name
			   (lambda (port . ignore)
			     ignore
			     (error "Attempt to perform a"
				    name
				    (error-irritant/noise
				     " operation on dummy I/O port:")
				    port))))
		   '(CHAR-READY? READ-CHAR WRITE-CHAR))
	      #f)
	     #f))

(define null-output-port
  (make-port (make-port-type
	      `((WRITE-CHAR ,(lambda (port char)
			       port char
			       ;; Return the number of characters written.
			       1)))
	      #f)
	     #f))

(define (editor-start-child-cmdl with-editor-ungrabbed)
  (lambda (cmdl thunk) cmdl (with-editor-ungrabbed thunk)))

(define (editor-child-cmdl-port port)
  (lambda (cmdl) cmdl port))

;;;; Inferior threads

(define inferior-thread-changes? unspecific)
(define inferior-threads unspecific)

(define (register-inferior-thread! thread output-processor)
  (let ((flags (cons #f output-processor)))
    (without-interrupts
     (lambda ()
       (set! inferior-threads
	     (cons (weak-cons thread flags)
		   inferior-threads))
       unspecific))
    flags))

(define (deregister-inferior-thread! flags)
  (without-interrupts
   (lambda ()
     (let loop ((threads inferior-threads) (prev #f))
       (if (pair? threads)
	   (if (eq? flags (weak-cdr (car threads)))
	       (begin
		 (if prev
		     (set-cdr! prev (cdr threads))
		     (set! inferior-threads (cdr threads)))
		 (weak-set-car! (car threads) #f)
		 (weak-set-cdr! (car threads) #f))
	       (loop (cdr threads) threads)))))))

(define (start-standard-polling-thread interval output-processor)
  (let ((holder (list #f)))
    (set-car! holder
	      (register-inferior-thread!
	       (let ((thread
		      (create-thread editor-thread-root-continuation
			(lambda ()
			  (do () (#f)
			    (let ((registration (car holder)))
			      (cond ((eq? registration 'KILL-THREAD)
				     (exit-current-thread unspecific))
				    (registration
				     (inferior-thread-output! registration))))
			    (sleep-current-thread interval))))))
		 (detach-thread thread)
		 thread)
	       output-processor))
    holder))

(define (stop-standard-polling-thread holder)
  (without-interrupts
   (lambda ()
     (let ((registration (car holder)))
       (if (and registration (not (eq? registration 'KILL-THREAD)))
	   (deregister-inferior-thread! registration)))
     (set-car! holder 'KILL-THREAD))))

(define (inferior-thread-output! flags)
  (without-interrupts (lambda () (inferior-thread-output!/unsafe flags))))

(define (inferior-thread-output!/unsafe flags)
  (set-car! flags #t)
  (if (not inferior-thread-changes?)
      (begin (set! inferior-thread-changes? #t)
             (signal-thread-event editor-thread #f))))

(define (accept-thread-output)
  (with-interrupt-mask interrupt-mask/gc-ok
    (lambda (interrupt-mask)
      (and inferior-thread-changes?
	   (begin
	     (set! inferior-thread-changes? #f)
	     (let loop ((threads inferior-threads) (prev #f) (output? #f))
	       (if (null? threads)
		   output?
		   (let ((record (car threads))
			 (next (cdr threads)))
		     (let ((thread (weak-car record))
			   (flags (weak-cdr record)))
		       (if (and thread (not (thread-dead? thread)))
			   (loop next
				 threads
				 (if (car flags)
				     (begin
				       (set-car! flags #f)
				       (let ((result
					      (invoke-thread-output-processor
					       (cdr flags)
					       interrupt-mask)))
					 (if (eq? output? 'FORCE-RETURN)
					     output?
					     (or result output?))))
				     output?))
			   (begin
			     (if prev
				 (set-cdr! prev next)
				 (set! inferior-threads next))
			     (loop next prev output?))))))))))))

(define (invoke-thread-output-processor processor interrupt-mask)
  (call-with-current-continuation
   (lambda (k)
     (with-restart 'ABORT "Return to ACCEPT-THREAD-OUTPUT."
	 (lambda () (k #t))
	 values
       (lambda ()
	 (with-interrupt-mask interrupt-mask
	   (lambda (interrupt-mask)
	     interrupt-mask
	     (processor))))))))