#| -*-Scheme-*-

$Id: compile.scm,v 1.20 2008/01/30 20:01:59 cph Exp $

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

;;;; Compilation Subprocess


(define-variable compile-command
  "Initial contents of \\[compile] prompt."
  "make -k"
  string?)

(define-command compile
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*."
  (lambda ()
    (if (null? (prompt-history-strings 'COMPILE))
	(set-prompt-history-strings! 'COMPILE
				     (list (ref-variable compile-command))))
    (list (prompt-for-string "Compile command" #f
			     'DEFAULT-TYPE 'INSERTED-DEFAULT
			     'HISTORY 'COMPILE
			     'HISTORY-INDEX 0)))
  (lambda (command)
    (run-compilation command)))

(define-command grep
  "Run grep, with user-specified args, and collect output in a buffer."
  (lambda ()
    (list (prompt-for-string "Run grep (with args): " #f
			     'DEFAULT-TYPE 'INSERTED-DEFAULT
			     'HISTORY 'GREP
			     'HISTORY-INDEX 0)))
  (lambda (command)
    (run-compilation (string-append "grep -n " command " /dev/null"))))

(define-command fgrep
  "Run fgrep, with user-specified args, and collect output in a buffer."
  (lambda ()
    (list (prompt-for-string "Run fgrep (with args): " #f
			     'DEFAULT-TYPE 'INSERTED-DEFAULT
			     'HISTORY 'FGREP
			     'HISTORY-INDEX 0)))
  (lambda (command)
    (run-compilation (string-append "fgrep -n " command " /dev/null"))))

(define-command kill-compilation
  "Kill the process made by the \\[compile] command."
  ()
  (lambda ()
    (let ((process compilation-process))
      (if (and process (eq? (process-status process) 'RUN))
	  (interrupt-process process #t)))))

(define (run-compilation command)
  ((ref-command save-some-buffers) #f)
  (let ((process compilation-process))
    (if process
	(begin
	  (if (eq? (process-status process) 'RUN)
	      (begin
		(if (not (prompt-for-yes-or-no?
			  "A compilation process is running; kill it"))
		    (editor-error "Cannot have two compilation processes"))
		(interrupt-process process #t)
		(sit-for 1000)))
	  (delete-process process))))
  (let ((buffer (temporary-buffer "*compilation*"))
	(directory (buffer-default-directory (current-buffer))))
    (disable-group-undo! (buffer-group buffer))
    (set-buffer-default-directory! buffer directory)
    (set-buffer-major-mode! buffer (ref-mode-object fundamental))
    (local-set-variable! mode-line-process '(": %s") buffer)
    (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
      (let ((window (get-buffer-window buffer)))
	(if window
	    (set-window-start-mark! window mark #t)))
      (insert-string "cd " mark)
      (insert-string (->namestring directory) mark)
      (insert-newline mark)
      (insert-string command mark)
      (insert-newline mark)
      (mark-temporary! mark))
    (let ((process
	   (apply start-process
		  "compilation"
		  buffer
		  scheme-subprocess-environment
		  (ref-variable shell-file-name)
		  (os/form-shell-command command))))
      (set-process-sentinel! process compilation-process-sentinel)
      (set! compilation-process process))
    (pop-up-buffer buffer #f)))

(define (compilation-process-sentinel process status reason)
  (let ((buffer (process-buffer process)))
    (if buffer
	(if (memq (process-status process) '(EXIT SIGNAL))
	    (let ((mark (mark-left-inserting-copy (buffer-end buffer))))
	      (insert-newline mark)
	      (insert-string "Process " mark)
	      (insert-string (process-name process) mark)
	      (insert-string " " mark)
	      (insert-string (process-status-message status reason) mark)
	      (insert-newline mark)
	      (mark-temporary! mark)))))
  (without-interrupts
   (lambda ()
     (if (eq? process compilation-process)
	 (begin
	   (set! compilation-process #f)
	   unspecific)))))

(define compilation-process
  #f)