#| -*-Scheme-*-

$Id: bufcom.scm,v 1.112 2007/01/05 21:19:23 cph Exp $

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

;;;; Buffer Commands


(define (prompt-for-select-buffer prompt)
  (lambda ()
    (list
     (buffer-name
      (prompt-for-buffer prompt (previous-buffer))))))

(define-command switch-to-buffer
  "Select buffer with specified name.
If the variable select-buffer-create is true,
specifying a non-existent buffer will cause it to be created."
  (prompt-for-select-buffer "Switch to buffer")
  (lambda (buffer)
    (select-buffer (find-buffer buffer #t))))

(define-command switch-to-buffer-other-window
  "Select buffer in another window."
  (prompt-for-select-buffer "Switch to buffer in other window")
  (lambda (buffer)
    (select-buffer-other-window (find-buffer buffer #t))))

(define-command switch-to-buffer-other-frame
  "Select buffer in another frame."
  (prompt-for-select-buffer "Switch to buffer in other frame")
  (lambda (buffer)
    (select-buffer-other-screen (find-buffer buffer #t))))
(define edwin-command$switch-to-buffer-other-screen
  edwin-command$switch-to-buffer-other-frame)

(define-command create-buffer
  "Create a new buffer with a given name, and select it."
  "sCreate buffer"
  (lambda (name)
    (select-buffer (new-buffer name))))

(define-command create-buffer-other-frame
  "Create a new buffer with a given name, and select it in another frame."
  "sCreate buffer in other frame"
  (lambda (name)
    (select-buffer-other-screen (new-buffer name))))
(define edwin-command$create-buffer-other-screen
  edwin-command$create-buffer-other-frame)

(define-command insert-buffer
  "Insert the contents of a specified buffer at point."
  "bInsert buffer"
  (lambda (buffer)
    (let ((point (mark-right-inserting (current-point))))
      (region-insert-string!
       point
       (region->string (buffer-region (find-buffer buffer #t))))
      (push-current-mark! (current-point))
      (set-current-point! point))))

(define-command twiddle-buffers
  "Select previous buffer."
  ()
  (lambda ()
    (let ((buffer (previous-buffer)))
      (if buffer
	  (select-buffer buffer)
	  (editor-error "No previous buffer to select")))))

(define-command bury-buffer
  "Put current buffer at the end of the list of all buffers.
There it is the least likely candidate for other-buffer to return;
thus, the least likely buffer for \\[switch-to-buffer] to select by default."
  ()
  (lambda ()
    (let ((buffer (current-buffer))
	  (previous (previous-buffer)))
      (if previous
	  (begin
	    (select-buffer previous)
	    (bury-buffer buffer))))))

(define-command rename-buffer
  "Change the name of the current buffer.
Reads the new name in the echo area."
  "sRename buffer (to new name)"
  (lambda (name)
    (if (find-buffer name)
	(editor-error "Buffer named " name " already exists"))
    (rename-buffer (current-buffer) name)))

(define-command kill-buffer
  "One arg, a string or a buffer.  Get rid of the specified buffer."
  "bKill buffer"
  (lambda (buffer)
    (kill-buffer-interactive (find-buffer buffer #t))))

(define (kill-buffer-interactive buffer)
  (if (not (other-buffer buffer)) (editor-error "Only one buffer"))
  (save-buffer-changes buffer)
  (if (for-all? (ref-variable kill-buffer-query-procedures buffer)
	(lambda (procedure)
	  (procedure buffer)))
      (kill-buffer buffer)
      (message "Buffer not killed.")))

(define (kill-buffer-query-modified buffer)
  (or (not (and (buffer-pathname buffer)
		(buffer-modified? buffer)
		(buffer-writeable? buffer)))
      (prompt-for-yes-or-no?
       (string-append "Buffer "
		      (buffer-name buffer)
		      " modified; kill anyway"))))

(define (kill-buffer-query-process buffer)
  (or (not (get-buffer-process buffer))
      (prompt-for-yes-or-no?
       (string-append "Buffer "
		      (buffer-name buffer)
		      " has an active process; kill anyway"))))

(define-variable kill-buffer-query-procedures
  "List of procedures called to query before killing a buffer.
Each procedure is called with one argument, the buffer being killed.
If any procedure returns #f, the buffer is not killed."
  (list kill-buffer-query-modified kill-buffer-query-process)
  (lambda (object) (and (list? object) (for-all? object procedure?))))

(define-command kill-some-buffers
  "For each buffer, ask whether to kill it."
  ()
  (lambda ()
    (kill-some-buffers #t)))

(define (kill-some-buffers prompt?)
  (for-each (lambda (buffer)
	      (if (and (not (minibuffer? buffer))
		       (or (not prompt?)
			   (prompt-for-confirmation?
			    (string-append "Kill buffer '"
					   (buffer-name buffer)
					   "'"))))
		  (if (other-buffer buffer)
		      (kill-buffer-interactive buffer)
		      (let ((dummy (new-buffer "*Dummy*")))
			(kill-buffer-interactive buffer)
			(create-buffer initial-buffer-name)
			(kill-buffer dummy)))))
	    (buffer-list)))

(define-command normal-mode
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file.
Uses the visited file name, the -*- line, and the local variables spec."
  ()
  (lambda ()
    (normal-mode (current-buffer) #f)))

(define-command toggle-mode-lock
  "Change whether this buffer has its major mode locked.
When locked, the buffer's major mode may not be changed."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (if (buffer-get buffer 'MAJOR-MODE-LOCKED)
	  (begin
	    (buffer-remove! buffer 'MAJOR-MODE-LOCKED)
	    (message "Major mode unlocked"))
	  (begin
	    (buffer-put! buffer 'MAJOR-MODE-LOCKED #t)
	    (message "Major mode locked"))))))

(define-command not-modified
  "Pretend that this buffer hasn't been altered."
  ()
  (lambda ()
    (buffer-not-modified! (current-buffer))))

(define-command toggle-read-only
  "Change whether this buffer is visiting its file read-only."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (if (buffer-writeable? buffer)
	  (set-buffer-read-only! buffer)
	  (set-buffer-writeable! buffer)))))

(define-command no-toggle-read-only
  "Display warning indicating that this buffer may not be modified."
  ()
  (lambda ()
    (editor-failure "This buffer may not be modified.")))

(define (save-buffer-changes buffer)
  (if (and (buffer-pathname buffer)
	   (buffer-modified? buffer)
	   (buffer-writeable? buffer)
	   (prompt-for-yes-or-no?
	    (string-append "Buffer "
			   (buffer-name buffer)
			   " contains changes.  Write them out")))
      (write-buffer-interactive buffer #f)))

(define (new-buffer name)
  (create-buffer (new-buffer-name name)))

(define (new-buffer-name name)
  (if (find-buffer name)
      (let search-loop ((n 2))
	(let ((new-name (string-append name "<" (write-to-string n) ">")))
	  (if (find-buffer new-name)
	      (search-loop (1+ n))
	      new-name)))
      name))

(define (pop-up-temporary-buffer name properties initialization)
  (let ((buffer (temporary-buffer name)))
    (let ((window (pop-up-buffer buffer #f)))
      (initialization buffer window)
      (set-buffer-point! buffer (buffer-start buffer))
      (buffer-not-modified! buffer)
      (if (memq 'READ-ONLY properties)
	  (set-buffer-read-only! buffer))
      (if (and window (memq 'SHRINK-WINDOW properties))
	  (shrink-window-if-larger-than-buffer window))
      (if (and (memq 'FLUSH-ON-SPACE properties)
	       (not (typein-window? (current-window))))
	  (begin
	    (message "Hit space to flush.")
	    (reset-command-prompt!)
	    (let ((char (keyboard-peek)))
	      (if (eqv? #\space char)
		  (begin
		    (keyboard-read)
		    (kill-pop-up-buffer #f))))
	    (clear-message))))))

(define (string->temporary-buffer string name properties)
  (pop-up-temporary-buffer name properties
    (lambda (buffer window)
      window
      (insert-string string (buffer-point buffer)))))

(define (call-with-output-to-temporary-buffer name properties procedure)
  (pop-up-temporary-buffer name properties
    (lambda (buffer window)
      window
      (call-with-output-mark (buffer-point buffer) procedure))))

(define (with-output-to-temporary-buffer name properties thunk)
  (call-with-output-to-temporary-buffer name properties
    (lambda (port)
      (with-output-to-port port thunk))))

(define (call-with-temporary-buffer name procedure)
  (let ((buffer))
    (dynamic-wind (lambda ()
		    (set! buffer (temporary-buffer name)))
		  (lambda ()
		    (procedure buffer))
		  (lambda ()
		    (kill-buffer buffer)
		    (set! buffer)
		    unspecific))))

(define (temporary-buffer name)
  (let ((buffer (find-or-create-buffer name)))
    (buffer-reset! buffer)
    buffer))

(define (prompt-for-buffer prompt default-buffer . options)
  (let ((name
	 (apply prompt-for-buffer-name prompt default-buffer
		'REQUIRE-MATCH? (not (ref-variable select-buffer-create))
		options)))
    (or (find-buffer name)
	(let loop ((hooks (ref-variable select-buffer-not-found-hooks)))
	  (cond ((null? hooks)
		 (let ((buffer (create-buffer name)))
		   (temporary-message "(New Buffer)")
		   buffer))
		((let ((result ((car hooks) name)))
		   (and (buffer? result)
			result)))
		(else
		 (loop (cdr hooks))))))))

(define-variable select-buffer-create
  "If true, buffer selection commands may create new buffers."
  #t
  boolean?)

(define-variable select-buffer-not-found-hooks
  "List of procedures to be called for select-buffer on nonexistent buffer.
These procedures are called as soon as the error is detected.
The procedures are called in the order given,
until one of them returns a buffer.
This variable has no effect if select-buffer-create is false."
  '()
  list?)

(define (prompt-for-existing-buffer prompt default-buffer . options)
  (find-buffer (apply prompt-for-buffer-name prompt default-buffer
		      'REQUIRE-MATCH? #t
		      options)
	       #t))

(define (prompt-for-buffer-name prompt default-buffer . options)
  (apply prompt-for-string-table-name
	 prompt
	 (and default-buffer (buffer-name default-buffer))
	 (buffer-names)
	 'DEFAULT-TYPE (if default-buffer 'VISIBLE-DEFAULT 'NO-DEFAULT)
	 options))