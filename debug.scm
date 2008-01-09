;;; -*-Scheme-*-
;;;
;;; $Id: debug.scm,v 1.72 2007/01/05 21:19:23 cph Exp $
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

;;;; Browser-style Debug and Where
;;; Package: (edwin debugger)


;;;; Text prop setup stuff

(define (with-output-highlighted port thunk)
  (let ((start (mark-temporary-copy (port/mark port))))
    (thunk)
    (highlight-region (make-region start (port/mark port)) (highlight-face))))

(define (read-only-between start end)
  (region-read-only (make-region start end)))

(define (readable-between start end)
  (region-writeable (make-region start end)))

(define (dehigh-between start end)
  (highlight-region (make-region start end) (default-face)))

(define (debugger-pp-highlight-subexpression expression subexpression
 					     indentation port)
  (let ((start-mark #f)
 	(end-mark #f))
    (fluid-let ((*pp-no-highlights?* #f))
      (debugger-pp
       (unsyntax-with-substitutions
 	expression
 	(list (cons subexpression
 		    (make-pretty-printer-highlight
 		     (unsyntax subexpression)
 		     (lambda (port)
 		       (set! start-mark
 			     (mark-right-inserting-copy
 			      (output-port->mark port)))
 		       unspecific)
 		     (lambda (port)
 		       (set! end-mark
 			     (mark-right-inserting-copy
 			      (output-port->mark port)))
 		       unspecific)))))
       indentation
       port))
    (if (and start-mark end-mark)
 	(highlight-region-excluding-indentation
	 (make-region start-mark end-mark)
	 (highlight-face)))
    (if start-mark (mark-temporary! start-mark))
    (if end-mark (mark-temporary! end-mark))))

;;;; Browsers

(define-record-type <browser>
    (%make-browser buffer object name lines selected-line buffers properties)
    browser?

  ;; The browser's buffer.
  (buffer browser/buffer)

  ;; The object being browsed.
  (object browser/object)

  ;; Name of this browser, a string.  Not necessarily unique.
  (name browser/name)

  ;; Vector of BLINE objects, sorted in order of increasing INDEX.
  (lines browser/lines set-browser/lines!)

  ;; The current selected BLINE object.
  (selected-line browser/selected-line set-browser/selected-line!)

  ;; List of buffers associated with this browser.
  (buffers browser/buffers set-browser/buffers!)

  (properties browser/properties))

(define (make-browser name mode object)
  (let ((buffer (new-buffer name)))
    (buffer-reset! buffer)
    (set-buffer-read-only! buffer)
    (set-buffer-major-mode! buffer mode)
    (add-kill-buffer-hook buffer kill-browser-buffer)
    (let ((browser
	   (%make-browser buffer
			  object
			  name
			  (vector)
			  #f
			  '()
			  (make-1d-table))))
      (buffer-put! buffer 'BROWSER browser)
      browser)))

(define (kill-browser-buffer buffer)
  (let ((browser (buffer-get buffer 'BROWSER)))
    (if browser
	(for-each kill-buffer (browser/buffers browser)))))

(define (buffer-browser buffer)
  (let ((browser (buffer-get buffer 'BROWSER)))
    (if (not browser)
	(error "This buffer has no associated browser:" buffer))
    browser))

(define (browser/new-buffer browser initializer)
  (let ((buffer
	 (create-buffer
	  (let ((prefix (browser/name browser)))
	    (let loop ((index 1))
	      (let ((name
		     (string-append
		      (if (1d-table/get (browser/properties browser)
					'VISIBLE-SUB-BUFFERS?
					#f)
			  ""
			  " ")
		      prefix
		      "-"
		      (number->string index))))
		(if (find-buffer name)
		    (loop (+ index 1))
		    name)))))))
    (if initializer
	(initializer buffer))
    (add-browser-buffer! browser buffer)
    buffer))

(define (add-browser-buffer! browser buffer)
  (add-rename-buffer-hook
   buffer
   (letrec
       ((hook
	 (lambda (buffer name)
	   name
	   (set-browser/buffers! browser
				 (delq! buffer (browser/buffers browser)))
	   (remove-rename-buffer-hook buffer hook))))
     hook))
  (add-kill-buffer-hook
   buffer
   (lambda (buffer)
     (set-browser/buffers! browser
			   (delq! buffer (browser/buffers browser)))))
  (set-browser/buffers! browser (cons buffer (browser/buffers browser)))
  (buffer-put! buffer 'ASSOCIATED-WITH-BROWSER browser))

(define (browser/new-screen browser)
  (let ((pair (1d-table/get (browser/properties browser) 'NEW-SCREEN #f)))
    (and pair
	 (weak-car pair))))

(define (set-browser/new-screen! browser screen)
  (1d-table/put! (browser/properties browser)
		 'NEW-SCREEN
		 (weak-cons screen #f)))

;;;; Browser Commands

(define-command browser-select-line
  "Select the current browser line."
  "d"
  (lambda (point)
    (let ((bline (mark->bline point)))
      (if (not bline)
	  (editor-error "Nothing to select on this line."))
      (select-bline bline))))

;;; If the mouse clicks on a bline, select it.
(define-command debugger-mouse-select-bline
  "Select a bline when mouse clicked there."
  ()
  (lambda ()
    ((ref-command mouse-set-point))
    (let ((bline (mark->bline (current-point))))
      (if bline
	  (select-bline bline)))))

(define-command browser-next-line
  "Move down to the next line."
  "p"
  (lambda (argument)
    (let* ((browser (buffer-browser (current-buffer)))
	   (bline
	    (letrec
		((loop
		  (lambda (index argument)
		    (let ((bline (browser/line browser index)))
		      (cond ((bline/continuation? bline)
			     (replace-continuation-bline bline)
			     (loop index argument))
			    ((= argument 0)
			     bline)
			    ((> argument 0)
			     (let ((index (+ index 1)))
			       (if (< index (browser/n-lines browser))
				   (loop index (- argument 1))
				   (begin
				     (select-bline bline)
				     #f))))
			    (else
			     (let ((index (- index 1)))
			       (if (<= 0 index)
				   (loop index (+ argument 1))
				   (begin
				     (select-bline bline)
				     #f)))))))))
	      (let ((point (current-point)))
		(let ((index (mark->bline-index point)))
		  (cond (index
			 (loop index argument))
			((= argument 0)
			 #f)
			(else
			 (let ((n (if (< argument 0) -1 1)))
			   (let find-next ((mark point))
			     (let ((mark (line-start mark n #f)))
			       (and mark
				    (let ((index (mark->bline-index mark)))
				      (if index
					  (loop index (- argument n))
					  (find-next mark))))))))))))))
      (cond (bline
	     (select-bline bline))
	    ((= argument 0)
	     (editor-failure "Nothing to select on this line."))
	    (else
	     (editor-failure))))))

(define-command browser-previous-line
  "Move up to the previous line."
  "p"
  (lambda (argument)
    ((ref-command browser-next-line) (- argument))))

(define (select-bline bline)
  (let ((bline
	 (if (bline/continuation? bline)
	     (replace-continuation-bline bline)
	     bline)))
    (let ((browser (bline/browser bline)))
      (unselect-bline browser)
      (let ((mark (bline/start-mark bline)))
	(with-buffer-open mark
	  (lambda ()
	    (insert-char #\> (mark1+ mark))
	    (delete-right-char mark)
	    (highlight-the-number mark)))
	(set-browser/selected-line! browser bline)
	(set-buffer-point! (mark-buffer mark) mark)))
    (let ((buffer (bline/description-buffer bline)))
      (if buffer
	  (pop-up-buffer buffer #f)))))

(define (highlight-the-number mark)
  (let ((end (re-search-forward "[RSE][0-9]+ " mark (line-end mark 0))))
    (highlight-region (make-region mark
				   (if (mark? end)
				       (mark- end 1)
				       (line-end mark 0)))
		      (highlight-face))))

(define (unselect-bline browser)
  (let ((bline (browser/selected-line browser)))
    (if bline
	(let ((mark (bline/start-mark bline)))
	  (with-buffer-open mark
	    (lambda ()
	      (dehigh-between mark (line-end mark 0))
	      (insert-char #\space (mark1+ mark))
	      (delete-right-char mark)))))))

;;; For any frame with an environment (excluding the mark frame) an
;;; inferior repl is started below the other descriptions.

(define (bline/description-buffer bline)
  (let* ((system?
	  (and (subproblem? (bline/object bline))
	       (system-frame? (subproblem/stack-frame (bline/object bline)))))
	 (buffer
	  (1d-table/get (bline/properties bline) 'DESCRIPTION-BUFFER #f))
	 (get-environment
	  (1d-table/get (bline-type/properties (bline/type bline))
			'GET-ENVIRONMENT
			#f))
	 (env-exists? (if (and get-environment (not system?))
			  (let ((environment* (get-environment bline)))
			    (environment? environment*))
			  #f))
	 (environment (if env-exists? (get-environment bline) #f)))
    (if (and buffer (buffer-alive? buffer))
	buffer
	(let ((write-description
	       (bline-type/write-description (bline/type bline))))
	  ((message-wrapper #t "Computing, please wait")
	   (lambda ()
	     (and write-description
		  (let ((buffer (browser/new-buffer (bline/browser bline) #f)))
		    (call-with-output-mark (buffer-start buffer)
		      (lambda (port)
			(write-description bline port)
			(if env-exists?
			    (begin
			      (debugger-newline port)
			      (write-string evaluation-line-marker port)
			      (debugger-newline port)))))
			(set-buffer-point! buffer (buffer-start buffer))
		    (1d-table/put! (bline/properties bline)
				   'DESCRIPTION-BUFFER
				   buffer)
		    (read-only-between (buffer-start buffer)
				       (buffer-end buffer))
		    (buffer-not-modified! buffer)
		    (if env-exists?
			(start-inferior-repl! buffer environment #f))
		    buffer))))))))

(define evaluation-line-marker
  ";EVALUATION may occur below in the environment of the selected frame.")

(define-command browser-quit
  "Exit the current browser, deleting its buffer."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((browser (buffer-browser buffer))
	    (screen (selected-screen)))
	;; Delete all windows that are currently showing buffers that
	;; are associated with this browser.
	(let ((window (screen-selected-window screen))
	      (buffers (browser/buffers browser)))
	  (for-each (lambda (window*)
		      (if (and (not (eq? window* window))
			       (not (typein-window? window*))
			       (memq (window-buffer window*) buffers))
			  (window-delete! window*)))
		    (screen-window-list screen)))
	;; If the browser was popped up in a new screen, and that
	;; screen is the current screen, delete it too.
	(let ((new-screen (browser/new-screen browser)))
	  (if (and (eq? new-screen screen)
		   (other-screen screen 1 #t))
	      (delete-screen! screen))))
      ;; Kill the buffer, then maybe select another browser.
      (let ((browser (get-buffer-browser buffer 'ASSOCIATED-WITH-BROWSER)))
	(kill-buffer-interactive buffer)
	(let ((browser
	       (or browser
		   (let ((buffer (current-buffer)))
		     (or (get-buffer-browser buffer 'BROWSER)
			 (get-buffer-browser buffer
					     'ASSOCIATED-WITH-BROWSER))))))
	  (if browser
	      (let ((buffer (browser/buffer browser)))
		(select-buffer buffer)
		((ref-command browser-select-line) (buffer-point buffer))))))
      (clear-current-message!)
      (maybe-restart-buffer-thread buffer))))

(define (get-buffer-browser buffer key)
  (let ((browser (buffer-get buffer key)))
    (and (browser? browser)
	 (buffer-alive? (browser/buffer browser))
	 browser)))

(define (maybe-restart-buffer-thread buffer)
  (let ((cont (maybe-get-continuation buffer))
	(thread (buffer-get buffer 'THREAD)))
    (if (and thread cont)
	(if (eq? thread editor-thread)
	    (signal-thread-event editor-thread (lambda () (cont unspecific)))
	    (restart-thread thread #f #f)))))

;;;addition for when debugger is called from a break
;;;should quit the debugger, and give the continuation
;;;a value to proceed with (restarting that thread)
;;;if in a normal error debug it will envoke the standard
;;;restarts
(define-command quit-with-restart-value
  "Quit the breakpoint, exiting with a specified value."
  ()
  (lambda ()
    (let* ((buffer (current-buffer))
	   (thread (buffer-get buffer 'THREAD)))
      (if (thread? thread)
	  (let ((value (prompt-for-expression-value
			"Please enter a value to continue with"))
		(cont (maybe-get-continuation buffer)))
	    (buffer-remove! buffer 'THREAD)
	    ((ref-command browser-quit))
	    (cond ((eq? thread editor-thread)
		   (signal-thread-event editor-thread (lambda ()
							(cont value))))
		  (else
		   (set! value? #t)
		   (restart-thread thread #t (lambda ()
					       (cont value))))))
	  (invoke-restarts #f)))))

(define (maybe-get-continuation buffer)
  (let ((object (browser/object (buffer-get buffer 'BROWSER))))
    (and (continuation? object)
	 object)))

;;;Method for invoking the standard restarts from within the
;;;debugger.
(define (invoke-restarts avoid-deletion?)
    (let* ((mark (current-point))
	   (bline (mark->bline mark))
	   (browser (bline/browser bline))
	   (buffer
	    (1d-table/get (bline/properties bline) 'DESCRIPTION-BUFFER #f))
	   (condition
	    (browser/object browser)))
      (if (condition? condition)
	  (fluid-let ((prompt-for-confirmation
		       (lambda (prompt #!optional port)
			 port
			 (call-with-interface-port (buffer-end buffer)
			   (lambda (port)
			     port
			     (prompt-for-yes-or-no? prompt)))))
		      (prompt-for-evaluated-expression
		       (lambda (prompt #!optional environment port)
			 port
			 (call-with-interface-port (buffer-end buffer)
			   (lambda (port)
			     port
			     (repl-eval (prompt-for-expression prompt)
					environment)))))
		      (hook/invoke-restart
		       (lambda (continuation arguments)
			 (invoke-continuation continuation
					      arguments
					      avoid-deletion?))))
	    (call-with-interface-port
	     (let ((buff (new-buffer " *debug*-RESTARTS")))
	       (add-browser-buffer! browser buff)
	       (pop-up-buffer buff #f)
	       (buffer-start buff))
	     (lambda (port)
	       (write-string "  " port)
	       (write-condition-report condition port)
	       (debugger-newline port)
	       (command/condition-restart
		(make-initial-dstate condition)
		port))))
	  (message "No condition to restart from."))))

(define (call-with-interface-port mark receiver)
  (let ((mark (mark-left-inserting-copy mark)))
    (let ((value (receiver (make-port interface-port-type mark))))
      (mark-temporary! mark)
      value)))

(define interface-port-type
  (make-port-type
   `((WRITE-CHAR
      ,(lambda (port char)
	 (guarantee-8-bit-char char)
	 (region-insert-char! (port/state port) char)))
     (PROMPT-FOR-CONFIRMATION
      ,(lambda (port prompt) port (prompt-for-confirmation? prompt)))
     (PROMPT-FOR-EXPRESSION
      ,(lambda (port environment prompt)
	 port environment
	 (prompt-for-expression prompt))))
   #f))

(define (invoke-continuation continuation arguments avoid-deletion?)
  (let ((buffer (current-buffer)))
    (if (and (not avoid-deletion?)
	     (ref-variable debugger-quit-on-return?))
	((ref-command browser-quit)))
    ((or (buffer-get buffer 'INVOKE-CONTINUATION) apply)
     continuation arguments)))

;;;; Where

(define-command browser-where
  "Select an environment browser for this line's environment."
  ()
  (lambda ()
    (select-buffer
     (bline/environment-browser-buffer (current-selected-line)))))

(define (bline/environment-browser-buffer bline)
  (let ((environment (bline/evaluation-environment bline)))
    (bline/attached-buffer bline 'ENVIRONMENT-BROWSER
      (lambda ()
	(or (list-search-positive (buffer-list)
	      (lambda (buffer)
		(let ((browser (buffer-get buffer 'BROWSER)))
		  (and browser (eq? environment (browser/object browser))))))
	    (environment-browser-buffer environment))))))

(define (bline/attached-buffer bline type make-buffer)
  (let ((buffer (1d-table/get (bline/properties bline) type #f)))
    (if (and buffer (buffer-alive? buffer))
	buffer
	(let ((buffer (make-buffer)))
	  (1d-table/put! (bline/properties bline) type buffer)
	  (add-browser-buffer! (bline/browser bline) buffer)
	  buffer))))

(define (current-selected-line)
  (let ((bline (browser/selected-line (buffer-browser (current-buffer)))))
    (if (not bline)
	(editor-error "There is no selected line; please select one."))
    bline))

(define (bline/evaluation-environment bline)
  (let ((get-environment
	 (1d-table/get (bline-type/properties (bline/type bline))
		       'GET-ENVIRONMENT
		       #f))
	(lose
	 (lambda () (editor-error "The selected line has no environment."))))
    (if get-environment
	(let ((environment (get-environment bline)))
	  (if (environment? environment)
	      environment
	      (lose)))
	(lose))))

;;;; Browser Lines

(define-record-type <browser-line>
    (%make-bline start-mark object type parent depth next prev offset
		 properties)
    bline?

  ;; Index of this bline within browser lines vector.  #F if line is
  ;; invisible.
  (index bline/index set-bline/index!)

  ;; Line start within browser buffer.  #F if line is invisible.
  (start-mark bline/start-mark set-bline/start-mark!)

  ;; Object that this line represents.
  (object bline/object)

  ;; Type of OBJECT.  This type is specific to the browser; it tells
  ;; the browser how to manipulate OBJECT.
  (type bline/type)

  ;; BLINE representing the object that this object is a component of,
  ;; or #F if none.
  (parent bline/parent)

  ;; Nonnegative integer indicating the depth of this object in the
  ;; component nesting.
  (depth bline/depth)

  ;; BLINEs representing the objects that are adjacent to this one in
  ;; the component ordering, or #F if none.
  (next bline/next set-bline/next!)
  (prev bline/prev)

  ;; Nonnegative integer indicating the position of this object in the
  ;; component ordering.
  (offset bline/offset)

  (properties bline/properties))

(define (make-bline object type parent prev)
  (let ((bline
	 (%make-bline #f
		      object
		      type
		      parent
		      (if parent (+ (bline/depth parent) 1) 0)
		      #f
		      prev
		      (if prev (+ (bline/offset prev) 1) 0)
		      (make-1d-table))))
    (if prev
	(set-bline/next! prev bline))
    bline))

(define (bline/browser bline)
  (buffer-browser (mark-buffer (bline/start-mark bline))))

;;;; Browser Line Editing

(define (browser/n-lines browser)
  (vector-length (browser/lines browser)))

(define (browser/line browser index)
  (vector-ref (browser/lines browser) index))

(define (mark->bline mark)
  (let ((blines (browser/lines (buffer-browser (mark-buffer mark))))
	(group (mark-group mark))
	(index (mark-index mark)))
    (let loop ((low 0) (high (vector-length blines)))
      (and (fix:< low high)
	   (let ((middle (fix:quotient (fix:+ low high) 2)))
	     (let ((bline (vector-ref blines middle)))
	       (let ((ls (mark-index (bline/start-mark bline))))
		 (cond ((fix:< index ls) (loop low middle))
		       ((fix:<= index (line-end-index group ls)) bline)
		       (else (loop (fix:+ middle 1) high))))))))))

(define (mark->bline-index mark)
  (let ((bline (mark->bline mark)))
    (and bline
	 (bline/index bline))))

(define (delete-blines browser start end)
  (if (< start end)
      (let ((bv (browser/lines browser)))
	(if (subvector-find-next-element bv start end
					 (browser/selected-line browser))
	    (unselect-bline browser))
	(let ((nbv (vector-length bv)))
	  (let ((bv* (make-vector (- nbv (- end start)))))
	    (do ((i 0 (+ i 1)))
		((= i start))
	      (vector-set! bv* i (vector-ref bv i)))
	    (do ((i end (+ i 1))
		 (j start (+ j 1)))
		((= i nbv))
	      (let ((bline (vector-ref bv i)))
		(set-bline/index! bline j)
		(vector-set! bv* j bline)))
	    (let ((start-mark (bline/start-mark (vector-ref bv start))))
	      (with-buffer-open start-mark
		(lambda ()
		  (delete-string
		   start-mark
		   (if (< end nbv)
		       (bline/start-mark (vector-ref bv end))
		       (buffer-end (browser/buffer browser)))))))
	    (set-browser/lines! browser bv*))))))

(define (insert-blines browser index blines)
  (if (not (null? blines))
      (let ((bv (browser/lines browser))
	    (n-blines (length blines)))
	(let ((nbv (vector-length bv)))
	  (let ((bv* (make-vector (+ nbv n-blines))))
	    (do ((i 0 (+ i 1)))
		((= i index))
	      (vector-set! bv* i (vector-ref bv i)))
	    (do ((blines blines (cdr blines))
		 (i index (+ i 1)))
		((null? blines))
	      (let ((bline (car blines)))
		(set-bline/index! bline i)
		(vector-set! bv* i bline)))
	    (do ((i index (+ i 1))
		 (j (+ index n-blines) (+ j 1)))
		((= i nbv))
	      (let ((bline (vector-ref bv i)))
		(set-bline/index! bline j)
		(vector-set! bv* j bline)))
	    (let ((start-mark
		   (if (< index nbv)
		       (bline/start-mark (vector-ref bv index))
		       (buffer-end (browser/buffer browser)))))
	      (with-buffer-open start-mark
		(lambda ()
		  (let ((mark (mark-left-inserting-copy start-mark))
			(columns 79))
		    (for-each
		     (lambda (bline)
		       (let ((index (mark-index mark))
			     (indentation
			      (+ 1
				 (* summary-indentation-increment
				    (bline/depth bline)))))
			 (insert-horizontal-space indentation mark)
			 (let ((summary
				(with-output-to-truncated-string
				    (max summary-minimum-columns
					 (- columns indentation 4))
				  (lambda ()
				    ((bline-type/write-summary
				      (bline/type bline))
				     bline
				     (current-output-port))))))
			   (insert-string (cdr summary) mark)
			   (if (car summary)
			       (insert-string " ..." mark)))
			 (insert-newline mark)
			 (set-bline/start-mark!
			  bline
			  (make-permanent-mark (mark-group mark) index #t))))
		     blines)
		    (mark-temporary! mark)))))
	    (set-browser/lines! browser bv*))))))

(define summary-indentation-increment 3)
(define summary-minimum-columns 10)

;;;; Browser Line Types

(define-record-type <browser-line-type>
    (%make-bline-type write-summary write-description selection-mark
		      properties)
    bline-type?

  ;; Procedure that is called to generate the browser line that
  ;; represents this object.  Two arguments: BLINE and PORT.  The
  ;; summary of BLINE is written to PORT.  The summary should fit on
  ;; one line; PORT will limit the number of characters that can be
  ;; printed so that it fits.
  (write-summary bline-type/write-summary)

  ;; Procedure that is called to generate a full description of the
  ;; object.  Two arguments: BLINE and PORT.  This description may use
  ;; multiple lines; it will be presented in its own buffer, so the
  ;; presentation style is not very constrained.  This component may
  ;; be #F to indicate that the object is not normally viewed.
  (write-description bline-type/write-description)

  ;; Procedure that generates the standard mark at which the point
  ;; should be placed when this object is selected.  One argument:
  ;; BLINE.  This component may be a nonnegative exact integer meaning
  ;; an offset from the START-MARK of the bline.
  (selection-mark bline-type/selection-mark)

  (properties bline-type/properties))

(define (make-bline-type write-summary write-description selection-mark)
  (%make-bline-type write-summary
		    write-description
		    selection-mark
		    (make-1d-table)))

(define (make-continuation-bline expander parent prev)
  (make-bline expander bline-type:continuation-line parent prev))

(define (continuation-line/write-summary bline port)
  bline
  (write-string "--more--" port))

(define bline-type:continuation-line
  (make-bline-type continuation-line/write-summary #f 0))

(define (bline/continuation? bline)
  (eq? (bline/type bline) bline-type:continuation-line))

(define (replace-continuation-bline bline)
  (let ((browser (bline/browser bline))
	(index (bline/index bline))
	(expansion ((bline/object bline))))
    (delete-blines browser index (+ index 1))
    (insert-blines browser index expansion)
    (car expansion)))

;;;; Control Variables

(define (boolean-or-ask? object)
  (or (boolean? object)
      (eq? 'ASK object)))

(define-variable debugger-one-at-a-time?
  "Allow only one debugger buffer to exist at a given time.
#T means delete an existing debugger buffer before making a new one.
#F means leave existing buffers alone.
'ASK means ask user what to do each time."
  'ASK
  boolean-or-ask?)

(define-variable debugger-max-subproblems
  "Maximum number of subproblems displayed when debugger starts.
Set this variable to #F to disable this limit."
  10
  (lambda (object)
    (or (not object)
	(and (exact-integer? object)
	     (> object 0)))))

(define-variable debugger-confirm-return?
  "True means prompt for confirmation in \"return\" commands.
The prompting occurs prior to returning the value."
  #t
  boolean?)

(define-variable debugger-quit-on-return?
  "True means quit debugger when executing a \"return\" command.
Quitting the debugger kills the debugger buffer and any associated buffers."
  #t
  boolean?)

(define-variable debugger-quit-on-restart?
  "True means quit debugger when executing a \"restart\" command.
Quitting the debugger kills the debugger buffer and any associated buffers."
  #t
  boolean?)

;;; Limited this because the bindings are now pretty-printed.

(define-variable environment-package-limit
  "Packages with more than this number of bindings will be abbreviated.
Set this variable to #F to disable this abbreviation."
  10
  (lambda (object)
    (or (not object)
	(exact-nonnegative-integer? object))))

(define-variable debugger-show-help-message?
  "True means show the help message, #f means don't."
  #T
  boolean?)

(define-variable debugger-start-new-frame?
  "#T means create a new frame whenever the debugger is invoked.
#F means continue in same frame.
'ASK means ask user."
  #T
  boolean-or-ask?)
(define edwin-variable$debugger-start-new-screen?
  edwin-variable$debugger-start-new-frame?)

(define-variable debugger-hide-system-code?
  "True means don't show subproblems created by the runtime system."
  #T
  boolean?)

(define-variable debugger-show-frames?
  "If true show the environment frames in the description buffer.
If false show the bindings without frames."
  #T
  boolean?)

(define-variable debugger-show-inner-frame-topmost?
  "Affects the debugger display when DEBUGGER-SHOW-FRAMES? is true.
If false, frames are displayed with the outer (most global) frame topmost,
like in a 6.001 style environment diagram.  This is the default.
If true, frames are display innermost first."
  #F
  boolean?)

(define-variable debugger-compact-display?
  "If true, the debugger omits some blank lines.
If false, more blank lines are produced between display elements.
This variable is usually set to #F, but setting it to #T is useful
to get more information in a short window, for example, when using
a fixed size terminal."
  #F
  boolean?)

;;;; Predicates

;;; Determines if a frame is marked.

(define (system-frame? stack-frame)
  (stack-frame/repl-eval-boundary? stack-frame))

;;; Bad implementation to determine for breaks if a value to proceed
;;; with is desired.

(define value? #f)

(define (invalid-subexpression? subexpression)
  (or (debugging-info/undefined-expression? subexpression)
      (debugging-info/unknown-expression? subexpression)))

(define (invalid-expression? expression)
  (or (debugging-info/undefined-expression? expression)
      (debugging-info/compiled-code? expression)))

;;;; Help Messages

;;; The help messages for the debugger

(define where-help-message
"     COMMANDS:  ? - Help  q - Quit environment browser

This is an environment-browser buffer.

Lines identify environment frames.
The buffer below shows the bindings of the selected environment.
-----------")

(define debugger-help-message
"     COMMANDS:  ? - Help  q - Quit debugger  e - Environment browser

This is a debugger buffer.

Lines identify stack frames, most recent first.

   Sx means frame is in subproblem number x.
   Ry means frame is reduction number y.

The buffer below shows the current subproblem or reduction.
-----------")

;;;; Debugger entry point

(define starting-debugger? #f)

(define (debug-scheme-error error-type condition ask?)
  (if starting-debugger?
      (quit-editor-and-signal-error condition)
      (begin
	(let ((start-debugger
	       (lambda ()
		 (fluid-let ((starting-debugger? #t))
		   (select-continuation-browser-buffer condition)))))
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

(define (select-continuation-browser-buffer object #!optional thread)
  (set! value? #f)
  (let ((buffers (find-debugger-buffers)))
    (if (and (pair? buffers)
	     (null? (cdr buffers))
	     (if (eq? 'ASK (ref-variable debugger-one-at-a-time? #f))
		 (prompt-for-confirmation?
		  "Another debugger buffer exists.  Delete it")
		 (ref-variable debugger-one-at-a-time? #f)))
	(kill-buffer (car buffers))))
  (let ((buffer (continuation-browser-buffer object)))
    (let ((thread (and (not (default-object? thread)) thread)))
      (if thread
	  (buffer-put! buffer 'THREAD thread)))
    (let ((screen (make-debug-screen buffer)))
      (if screen
	  (select-screen screen)
	  (select-buffer buffer)))
    ((ref-command browser-select-line) (buffer-point buffer))))

(define-command browse-continuation
  "Invoke the continuation-browser on CONTINUATION."
  "XBrowse Continuation"
  select-continuation-browser-buffer)

(define (make-debug-screen buffer)
  (and (multiple-screens?)
       (let ((new-screen? (ref-variable debugger-start-new-screen? buffer)))
	 (if (eq? new-screen? 'ASK)
	     (prompt-for-confirmation? "Start debugger in new screen")
	     new-screen?))
       (let ((screen (apply make-screen buffer (make-debug-screen-args))))
	 (set-browser/new-screen! (buffer-browser buffer) screen)
	 screen)))

(define (make-debug-screen-args)
  (case (display-type/name (current-display-type))
    ((X)
     (cond ((string? default-screen-geometry)
	    (list default-screen-geometry))
	   ((eq? default-screen-geometry 'ASK)
	    (let ((geometry
		   (let loop ((default default-screen-geometry))
		     (let ((geometry
			    (prompt-for-string "Please enter a geometry"
					       default)))
		       (if (geometry? geometry)
			   geometry
			   (loop geometry))))))
	      (set! default-screen-geometry geometry)
	      geometry))
	   (else '())))
    (else '())))

(define (geometry? geometry)
  (let ((geometry-pattern
	 "[0-9]+x[0-9]+\\(-[0-9]+\\|+[0-9]+\\|\\)\\(-[0-9]+\\|+[0-9]+\\|\\)"))
    (re-string-match (re-compile-pattern geometry-pattern #f) geometry)))

(define default-screen-geometry #f)

(define (continuation-browser-buffer object)
  (let ((browser
	 (make-browser "*debug*"
		       (ref-mode-object continuation-browser)
		       object))
	(blines
	 (continuation->blines
	  (cond ((continuation? object)
		 object)
		((condition? object)
		 (condition/continuation object))
		(else
		 (error:wrong-type-argument object
					    "condition or continuation"
					    'CONTINUATION-BROWSER-BUFFER)))
	  (ref-variable debugger-max-subproblems))))
    (let ((buffer (browser/buffer browser)))
      (let ((mark (buffer-end buffer)))
	(with-buffer-open mark
	  (lambda ()
	    (call-with-output-mark mark
	      (lambda (port)
		(if (ref-variable debugger-show-help-message?)
		    (write-string debugger-help-message port))
		(debugger-newline port)
		(if (condition? object)
		    (begin
		      (write-string "The " port)
		      (write-string (if (condition/error? object)
					"error"
					"condition")
				    port)
		      (write-string " that started the debugger is:" port)
		      (debugger-newline port)
		      (debugger-newline port)
		      (write-string "  " port)
		      (with-output-highlighted port
			(lambda ()
			  (write-condition-report object port)))
		      (debugger-newline port)))
		(debugger-newline port))))))
      (insert-blines browser 0 blines)
      (set-buffer-point! buffer
			 (if (null? blines)
			     (buffer-end buffer)
			     (bline/start-mark (car blines))))
      buffer)))

(define (find-debugger-buffers)
  (list-transform-positive (buffer-list)
    (let ((debugger-mode (ref-mode-object continuation-browser)))
      (lambda (buffer)
	(eq? (buffer-major-mode buffer) debugger-mode)))))

;;;; Continuation Browser Mode

(define-major-mode continuation-browser read-only "Debug"
  "                     ******* Debugger Help *******

Commands:

`mouse-button-1'
     Select a subproblem or reduction and display information in the
     description buffer.

`C-n'
`down-arrow'
     Move the cursor down the list of subproblems and reductions and
     display info in the description buffer.

`C-p'
`up-arrow'
     Move the cursor up the list of subproblems and reductions and
     display info in the description buffer.

`e'  Show the environment structure.

`q'  Quit the debugger, destroying its window.

`p'  Invoke the standard restarts.

`SPC'
     Display info on current item in the description buffer.

`?'  Display help information.

   Each line beginning with `S' represents either a subproblem or stack
frame.  A subproblem line may be followed by one or more indented lines
\(beginning with the letter `R') which represent reductions associated
with that subproblem.  The subproblems are indexed with the natural
numbers.  To obtain a more complete description of a subproblem or
reduction, click the mouse on the desired line or move the cursor to the
line using the arrow keys (or `C-n' and `C-p').  The description buffer
will display the additional information.

   The description buffer contains three major regions.  The first
region contains a pretty printed version of the current expression. The
current subproblem within the expression is highlighted.  The second
region contains a representation of the frames of the environment of the
current expression.  The bindings of each frame are listed below the
frame header.  If there are no bindings in the frame, none will be
listed.  The frame of the current expression is preceeded with ==>.

   The bottom of the description buffer contains a region for evaluating
expressions in the environment of the selected subproblem or reduction.
This is the only portion of the buffer where editing is possible.  This
region can be used to find the values of variables in different
environments; you cannot, however, use mutators (set!, etc.) on compiled
code.

   Typing  `e' creates a new buffer in which you may browse through the
current environment.  In this new buffer, you can use the mouse, the
arrows, or `C-n' and `C-p' to select lines and view different
environments.  The environments listed are the same as those in the
description buffer.  If the selected environment structure is too large
to display (if there are more than `environment-package-limit' items in
the environment) an appropriate message is displayed.  To display the
environment in this case, set the `environment-package-limit' variable
to  `#f'.  This process is initiated by the command `M-x set-variable'.
You can not use `set!' to set the variable because it is an editor
variable and does not exist in the current scheme environment.  At the
bottom of the new buffer is a region for evaluating expressions similar
to that of the description buffer.

   The appearance of environment displays is controlled by the editor
variables `debugger-show-inner-frame-topmost?' and `debugger-compact-display?'
which affect the ordering of environment frames and the line spacing
respectively.

   Type `q' to quit the debugger, killing its primary buffer and any
others that it has created.

   NOTE: The debugger creates discription buffers in which debugging
information is presented.  These buffers are given names beginning with
spaces so that they do not appear in the buffer list; they are
automatically deleted when you quit the debugger.  If you wish to keep
one of these buffers, simply rename it using `M-x rename-buffer': once
it has been renamed, it will not be deleted automatically.")

(define-key 'continuation-browser #\p 'quit-with-restart-value)
(define-key 'continuation-browser down 'browser-next-line)
(define-key 'continuation-browser up 'browser-previous-line)
(define-key 'continuation-browser button1-down 'debugger-mouse-select-bline)
(define-key 'continuation-browser #\c-n 'browser-next-line)
(define-key 'continuation-browser #\c-p 'browser-previous-line)
(define-key 'continuation-browser #\? 'describe-mode)
(define-key 'continuation-browser #\q 'browser-quit)
(define-key 'continuation-browser #\space 'browser-select-line)
(define-key 'continuation-browser #\e 'browser-where)

;;;; Subproblems

;; A continuation consists of subproblems.  A subproblem has
;; expression information that identifies what the subproblem means.
;; It additionally has reductions and an environment.  Similarly,
;; reductions have expression and environment information.
;; Environments consist of environment frames, and each frame consists
;; of bindings.  Subproblems, reductions, and environment frames are
;; ordered; bindings are not.

;;; Stops displaying subproblems past marked frame by default.

(define (continuation->blines continuation limit)
  (let ((beyond-system-code #f))
    (let loop ((frame (continuation/first-subproblem continuation))
	       (prev #f)
	       (n 0))
      (if (not frame)
	  '()
	  (let* ((next-subproblem
		  (lambda (bline)
		    (loop (stack-frame/next-subproblem frame)
			  bline
			  (+ n 1))))
		 (walk-reductions
		  (lambda (bline reductions)
		    (cons bline
			  (let loop ((reductions reductions) (prev #f))
			    (if (null? reductions)
				(next-subproblem bline)
				(let ((bline
				       (make-bline (car reductions)
						   bline-type:reduction
						   bline
						   prev)))
				  (cons bline
					(loop (cdr reductions) bline))))))))
		 (continue
		  (lambda ()
		    (let* ((subproblem (stack-frame->subproblem frame n)))
		      (if debugger:student-walk?
			  (let ((reductions
				 (subproblem/reductions subproblem)))
			    (if (null? reductions)
				(let ((bline
				       (make-bline subproblem
						   bline-type:subproblem
						   #f
						   prev)))
				  (cons bline
					(next-subproblem bline)))
				(let ((bline
				       (make-bline (car reductions)
						   bline-type:reduction
						   #f
						   prev)))
				  (walk-reductions bline
						   (if (> n 0)
						       '()
						       (cdr reductions))))))
			  (walk-reductions
			   (make-bline subproblem
				       bline-type:subproblem
				       #f
				       prev)
			   (subproblem/reductions subproblem)))))))
	    (cond ((and (not (ref-variable debugger-hide-system-code?))
			(system-frame? frame))
		   (loop (stack-frame/next-subproblem frame)
			 prev
			 n))
		  ((or (and limit (>= n limit))
		       (if (system-frame? frame)
			   (begin (set! beyond-system-code #t) #t)
			   #f)
		       beyond-system-code)
		   (list (make-continuation-bline continue #f prev)))
		  (else (continue))))))))

(define-record-type <subproblem>
    (make-subproblem stack-frame expression environment subexpression number)
    subproblem?
  (stack-frame subproblem/stack-frame)
  (expression subproblem/expression)
  (environment subproblem/environment)
  (subexpression subproblem/subexpression)
  (number subproblem/number))

(define (stack-frame->subproblem frame number)
  (receive (expression environment subexpression)
      (stack-frame/debugging-info frame)
    (make-subproblem frame expression environment subexpression number)))

(define-record-type <reduction>
    (make-reduction subproblem expression environment number)
    reduction?
  (subproblem reduction/subproblem)
  (expression reduction/expression)
  (environment reduction/environment)
  (number reduction/number))

(define (subproblem/reductions subproblem)
  (let ((frame (subproblem/stack-frame subproblem)))
    (let loop ((reductions (stack-frame/reductions frame)) (n 0))
      (if (pair? reductions)
	  (cons (make-reduction subproblem
				(caar reductions)
				(cadar reductions)
				n)
		(loop (cdr reductions) (+ n 1)))
	  '()))))

(define (subproblem/write-summary bline port)
  (let* ((subproblem (bline/object bline))
	 (frame (subproblem/stack-frame subproblem)))
    (if (system-frame? frame)
	(write-string "***************Internal System Code Follows***********"
		      port)
	(begin
	  (write-string "S" port)
	  (write-string (bline/offset-string (subproblem/number subproblem))
			port)
	  (write-string " " port)
	  (let ((expression (subproblem/expression subproblem))
		(subexpression (subproblem/subexpression subproblem)))
	    (cond ((debugging-info/compiled-code? expression)
		   (write-string ";unknown compiled code" port))
		  ((not (debugging-info/undefined-expression? expression))
		   (fluid-let ((*unparse-primitives-by-name?* #t))
		     (write
		      (unsyntax (if (invalid-subexpression? subexpression)
				    expression
				    subexpression)))))
		  ((debugging-info/noise? expression)
		   (write-string ";" port)
		   (write-string ((debugging-info/noise expression) #f)
				 port))
		  (else
		   (write-string ";undefined expression" port))))))))

(define (subproblem/write-description bline port)
  (let* ((subproblem (bline/object bline))
	 (frame (subproblem/stack-frame subproblem)))
    (cond ((system-frame? frame)
	   (write-string "The subproblems which follow are part of the " port)

	   (write-string "internal system workings." port))
	  (else
	   (write-string "                         SUBPROBLEM LEVEL: " port)
	   (write (subproblem/number subproblem) port)
	   (debugger-newline port)
	   (debugger-newline port)
	   (let ((expression (subproblem/expression subproblem))
		 (frame (subproblem/stack-frame subproblem)))
	     (cond ((not (invalid-expression? expression))
		    (write-string (if (stack-frame/compiled-code? frame)
				      "COMPILED expression"
				      "Expression")
				  port)
		    (write-string " (from stack):" port)
		    (debugger-newline port)
		    (write-string
		     " Subproblem being executed is highlighted.\n"
		     port)
		    (debugger-newline port)
		    (let ((subexpression
			   (subproblem/subexpression subproblem)))
		      (if (invalid-subexpression? subexpression)
			  (debugger-pp expression expression-indentation port)
			  (debugger-pp-highlight-subexpression
			   expression
			   subexpression
			   expression-indentation
			   port))))
		   ((debugging-info/noise? expression)
		    (write-string ((debugging-info/noise expression) #t)
				  port))
		   (else
		    (write-string (if (stack-frame/compiled-code? frame)
				      "Compiled expression unknown"
				      "Expression unknown")
				  port)
		    (debugger-newline port)
		    (write (stack-frame/return-address frame) port))))
	   (let ((environment (subproblem/environment subproblem)))
	     (if (not (debugging-info/undefined-environment? environment))
		 (begin
		   (debugger-newline port)
		   (debugger-newline port)
		   (desc-show-environment-name-and-bindings environment
							    port))))))))

(define bline-type:subproblem
  (make-bline-type subproblem/write-summary
		   subproblem/write-description
		   1))

(1d-table/put! (bline-type/properties bline-type:subproblem)
	       'GET-ENVIRONMENT
	       (lambda (bline)
		 (subproblem/environment (bline/object bline))))

;;;; Reductions

(define (reduction/write-summary bline port)
  (let ((reduction (bline/object bline)))
    (if (bline/parent bline)
	(begin
	  (write-string "R" port)
	  (write-string (bline/offset-string (reduction/number reduction))
			port))
	(begin
	  (write-string "S" port)
	  (write-string
	   (bline/offset-string
	    (subproblem/number (reduction/subproblem reduction)))
	   port)))
    (write-string " " port)
    (fluid-let ((*unparse-primitives-by-name?* #t))
      (write (unsyntax (reduction/expression reduction)) port))))

(define (reduction/write-description bline port)
  (let ((reduction (bline/object bline)))
    (write-string "              SUBPROBLEM LEVEL: " port)
    (write (subproblem/number (reduction/subproblem reduction)) port)
    (write-string "  REDUCTION NUMBER: " port)
    (write (reduction/number reduction) port)
    (debugger-newline port)
    (debugger-newline port)
    (write-string "Expression (from execution history):" port)
    (debugger-newline port)
    (debugger-newline port)
    (debugger-pp (reduction/expression reduction) expression-indentation port)
    (debugger-newline port)
    (debugger-newline port)
    (desc-show-environment-name-and-bindings (reduction/environment reduction)
					port)))

(define bline-type:reduction
  (make-bline-type reduction/write-summary
		   reduction/write-description
		   1))

(1d-table/put! (bline-type/properties bline-type:reduction)
	       'GET-ENVIRONMENT
	       (lambda (bline)
		 (reduction/environment (bline/object bline))))

;;;; Environments

(define-command browse-environment
  "Invoke the environment-browser on ENVIRONMENT."
  "XBrowse Environment"
  (lambda (environment)
    (select-buffer (environment-browser-buffer environment))))

;;; Adds a help line.

(define (environment-browser-buffer object)
  (let ((environment (->environment object)))
    (let ((browser
	   (make-browser "*where*"
			 (ref-mode-object environment-browser)
			 object))
	  (blines (environment->blines environment)))

      (let ((buffer (browser/buffer browser)))
	(let ((mark (buffer-end buffer)))
	  (with-buffer-open mark
	    (lambda ()
	      (call-with-output-mark
	       mark
	       (lambda (port)
		 (if (ref-variable debugger-show-help-message?)
		     (write-string where-help-message port))
		 (debugger-newline port))))))
	(insert-blines browser 0 blines)
	(if (null? blines)
	    (set-buffer-point! buffer (buffer-end buffer))
	    (select-bline (car blines)))
	buffer))))

(define (environment->blines environment)
  (let loop ((environment environment) (prev #f))
    (let ((bline (make-bline environment bline-type:environment #f prev)))
      (cons bline
	    (if (eq? #t (environment-has-parent? environment))
		(loop (environment-parent environment) bline)
		'())))))

(define-major-mode environment-browser read-only "Environment Browser"
  "             ********Environment Browser Help********

Commands:

`mouse-button-1'
     Select a subproblem or reduction and display information in the
     description buffer.

`C-n'
`down-arrow'
     Move the cursor down the list of subproblems and reductions and
     display info in the description buffer.

`C-p'
`up-arrow'
     Move the cursor up the list of subproblems and reductions and
     display info in the description buffer.

`q'
     Quit the environment browser, destroying its window.

`SPC'
     Display info on current item in the description buffer.

`?'
     Display help information.

   In this buffer, you can use the mouse, the arrows, or `C-n' and
`C-p' to select lines and view different environments.
If the selected environment structure is too large to display (if
there are more than `environment-package-limit' items in the
environment) an appropriate message is displayed.  To display the
environment in this case, set the `environment-package-limit' variable
to  `#f'.  This process is initiated by the command `M-x
set-variable'. You can not use `set!' to set the variable because it
is an editor variable and does not exist in the current scheme
environment.

   The bottom of the description buffer contains a region for evaluating
expressions in the environment of the selected subproblem or reduction.
This is the only portion of the buffer where editing is possible.  This
region can be used to find the values of variables in different
environments; you cannot, however, use mutators (set!, etc.) on
compiled code.

   Type `q' to quit the environment browser, killing its primary buffer
and any others that it has created.

NOTE: The environment browser creates discription buffers in which
debugging information is presented.  These buffers are given names
beginning with spaces so that they do not appear in the buffer list;
they are automatically deleted when you quit the debugger.  If you wish
to keep one of these buffers, simply rename it using `M-x rename-buffer':
once it has been renamed, it will not be deleted automatically.")

(define-key 'environment-browser down 'browser-next-line)
(define-key 'environment-browser up 'browser-previous-line)
(define-key 'environment-browser button1-down	'debugger-mouse-select-bline)
(define-key 'environment-browser #\c-n 'browser-next-line)
(define-key 'environment-browser #\c-p 'browser-previous-line)
(define-key 'environment-browser #\? 'describe-mode)
(define-key 'environment-browser #\q 'browser-quit)
(define-key 'environment-browser #\space 'browser-select-line)

(define (environment/write-summary bline port)
  (write-string "E" port)
  (write-string (bline/offset-string (bline/offset bline)) port)
  (write-string " " port)
  (show-environment-name (bline/object bline) port))

(define (environment/write-description bline port)
  (let ((environment (bline/object bline)))
    (show-environment-name-and-bindings environment port)))

(define (show-environment-name-and-bindings environment port)
  (show-environment-name environment port)
  (debugger-newline port)
  (debugger-newline port)
  (let ((names (environment-bound-names environment))
	(package (environment->package environment))
	(finish
	 (lambda (names)
	   (debugger-newline port)
	   (for-each (lambda (name)
		       (myprint-binding name
					(environment-safe-lookup environment
								 name)
					port))
	     names))))
    (cond ((null? names)
	   (write-string " has no bindings" port))
	  ((and package
		(let ((limit (ref-variable environment-package-limit)))
		  (and limit
		       (let ((n (length names)))
			 (and (>= n limit)
			      (begin
				(write-string " has " port)
				(write n port)
				(write-string " bindings (first " port)
				(write limit port)
				(write-string " shown):" port)
				(finish (list-head names limit))
				#t)))))))
	  (else
	   (write-string "  BINDINGS:" port)
	   (finish (if package (sort names symbol<?) names)))))
  (debugger-newline port)
  (debugger-newline port)
  (write-string
   "---------------------------------------------------------------------"
   port))

;;;This does some stuff who's end product is to pp the bindings
(define (myprint-binding name value port)
  (let ((x-size (output-port/x-size port)))
    (debugger-newline port)
    (let ((name1
	   (output-to-string (quotient x-size 2)
	     (lambda ()
	       (write-dbg-name name (current-output-port))))))
      (write-string name1 port)
      (cond ((unassigned-reference-trap? value)
	     (write-string " is unassigned" port))
	    ((macro-reference-trap? value)
	     (write-string " is a syntactic keyword" port))
	    (else
	     (let ((separator " = "))
	       (write-string separator port)
	       (let ((indentation 
		      (+ (string-length name1)
			 (string-length separator))))
		 (write-string (string-tail
				(with-output-to-string
				  (lambda ()
				    (pretty-print value
						  (current-output-port)
						  #t
						  indentation)))
				indentation)
			       port))))))
    (debugger-newline port)))

(define bline-type:environment
  (make-bline-type environment/write-summary
		   environment/write-description
		   1))

(1d-table/put! (bline-type/properties bline-type:environment)
	       'GET-ENVIRONMENT
	       bline/object)

(define (bline/offset-string number)
  (let ((string (number->string number)))
    (if (< (string-length string) offset-string-min)
	(string-pad-right string offset-string-min)
	string)))

(define offset-string-min
  2)

(define (with-buffer-open mark thunk)
  (with-read-only-defeated mark thunk)
  (buffer-not-modified! (mark-buffer mark)))

(define (desc-show-environment-name-and-bindings environment port)
  (write-string
   "---------------------------------------------------------------------"
   port)
  (if (ref-variable debugger-show-frames?)
      (show-frames-and-bindings environment port)
      (print-the-local-bindings environment port))
  (debugger-newline port)
  (write-string
   "---------------------------------------------------------------------"
   port))

(define (debugger-newline port)
  (if (ref-variable debugger-compact-display?)
      (fresh-line port)
      (newline port)))

(define (show-frames-and-bindings environment port)

  (define (envs environment)
    (cons environment
	  (if (environment-has-parent? environment)
	      (envs (environment-parent environment))
	      '())))

  (define (show-frames envs indents)
    (for-each (lambda (env indent)
		(debugger-newline port)
		(if (eq? env environment)
		    (let* ((pointer "==> ")
			   (pl (string-length pointer)))
		      (if (> (string-length indent) pl)
			  (write-string (string-tail indent pl) port))
		      (write-string pointer port))
		    (write-string indent port))
		(show-environment-name env port)
		(debugger-newline port)
		(show-environment-bindings-with-ind env indent port))
	      envs indents))

  (let ((env-list (envs environment)))
    (cond ((ref-variable debugger-show-inner-frame-topmost?)
	   (show-frames env-list (make-list (length env-list) "")))
	  (else
	   (show-frames (reverse env-list)
			(make-initialized-list (length env-list)
			  (lambda (i) (make-string (* i 2) #\space))))))))

(define (print-the-local-bindings environment port)
  (let ((names (get-all-local-bindings environment)))
    (let ((n-bindings (length names))
	  (finish
	   (lambda (names)
	     (for-each (lambda (name)
			 (let loop ((env environment))
			   (if (environment-bound? env name)
			       (print-binding-with-ind
				name
				(environment-safe-lookup env name)
				"  "
				port)
			       (loop (environment-parent env)))))
		       names))))
      (debugger-newline port)
      (show-environment-name environment port)
      (cond ((zero? n-bindings)
	     (debugger-newline port)
	     (write-string "    has no bindings" port)
	     (debugger-newline port))
	    ((> n-bindings (ref-variable environment-package-limit)))
	    (else
	     (debugger-newline port)
	     (debugger-newline port)
	     (write-string "  Local Bindings:" port)
	     (debugger-newline port)
	     (finish names))))))

(define (show-environment-name environment port)
  (write-string "ENVIRONMENT " port)
  (let ((package (environment->package environment)))
    (if package
	(begin
	  (write-string "named: " port)
	  (write (package/name package) port))
	(begin
	  (write-string "created by " port)
	  (print-user-friendly-name environment port)))))

(define (get-all-local-bindings environment)
  (define (envs environment)
    (if (environment-has-parent? environment)
	(cons environment (envs (environment-parent environment))) ;
	'()))
  (let* ((env-list (envs environment))
	 (names1 (map (lambda (envir)
			(let ((names (environment-bound-names envir)))
			  (if (< (length names)
				 (ref-variable environment-package-limit))
			      names
			      '())))
		      env-list))
	 (names2 (reduce-right append '() names1))
	 (names3 (let loop ((l names2))
		     (if (null? l)
			 l
			 (cons (car l) (loop (delete (car l) l))))))
	 (names4 (sort names3 symbol<?)))
    names4))

(define (show-environment-bindings-with-ind environment ind port)
  (let ((names (environment-bound-names environment)))
    (let ((n-bindings (length names))
	  (finish
	   (lambda (names)
	     (debugger-newline port)
	     (for-each (lambda (name)
			 (print-binding-with-ind
			  name
			  (environment-safe-lookup environment name)
			  ind
			  port))
		       names))))
      (cond ((environment->package environment)
	     (write-string (string-append ind "    has ") port)
	     (write n-bindings port)
	     (write-string
	      " bindings"
	      port)
	     (debugger-newline port))
	    ((zero? n-bindings)
	     #|(write-string (string-append ind "    has no bindings") port)
	     (debugger-newline port)|#)
	    ((> n-bindings (ref-variable environment-package-limit))
	     (write-string (string-append ind "    has ") port)
	     (write n-bindings port)
	     (write-string
	      " bindings (see editor variable environment-package-limit)"
	      port)
	     (debugger-newline port))
	    (else
	     (finish names))))))

(define (print-binding-with-ind name value ind port)
  (let* ((extra "    ")
	 (x-size
	  (- (output-port/x-size port)
	     (+ (string-length ind)
		(string-length extra)))))
    (write-string ind port)
    (write-string extra port)
    (let ((name
	   (output-to-string (quotient x-size 2)
	     (lambda ()
	       (write-dbg-name name (current-output-port))))))
      (write-string name port)
      (cond ((unassigned-reference-trap? value)
	     (write-string " is unassigned" port))
	    ((macro-reference-trap? value)
	     (write-string " is a syntactic keyword" port))
	    (else
	     (let ((separator " = "))
	       (write-string separator port)
	       (write-string
		(output-to-string (max 0
				       (- (- x-size 1)
					  (+ (string-length name)
					     (string-length separator))))
				  (lambda () (write value)))
		port)))))
    (debugger-newline port)))
