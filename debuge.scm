#| -*-Scheme-*-

$Id: debuge.scm,v 1.61 2007/01/21 01:10:18 riastradh Exp $

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

;;;; Debugging Stuff

(declare (usual-integrations))

(define (debug-save-files)
  (for-each debug-save-buffer
	    (bufferset-buffer-list (editor-bufferset edwin-editor))))

(define (debug-save-buffer buffer)
  (if (and (buffer-modified? buffer)
	   (buffer-writeable? buffer)
	   (not (minibuffer? buffer)))
      (let ((pathname
	     (let ((pathname (buffer-pathname buffer)))
	       (cond ((not pathname)
		      (and (y-or-n? "Save buffer "
				    (buffer-name buffer)
				    " (Y or N)? ")
			   ((access prompt-for-expression
				    system-global-environment)
			    "Filename")))
		     ((integer? (pathname-version pathname))
		      (pathname-new-version pathname 'NEWEST))
		     (else
		      pathname)))))
	(if pathname
	    (let* ((pathname (merge-pathnames pathname))
		   (filename (->namestring pathname)))
	      (if (or (not (file-exists? pathname))
		      (y-or-n? "File '"
			       filename
			       "' exists.  Write anyway (Y or N)? "))
		  (begin
		    (newline)
		    (write-string "Writing file '")
		    (write-string filename)
		    (write-string "'")
		    (let ((region (buffer-unclipped-region buffer)))
		      (group-write-to-file
		       (ref-variable translate-file-data-on-output
				     (region-group region))
		       (region-group region)
		       (region-start-index region)
		       (region-end-index region)
		       filename))
		    (write-string " -- done")
		    (set-buffer-pathname! buffer pathname)
		    (set-buffer-truename! buffer (->truename pathname))
		    (buffer-not-modified! buffer))))))))

(define-command debug-count-marks
  "Show the number of in-use and GC'ed marks for the current buffer."
  ()
  (lambda ()
    (count-marks-group (buffer-group (current-buffer))
		       (lambda (n-existing n-gced)
			 (message "Existing: " (write-to-string n-existing)
				  "; GCed: " (write-to-string n-gced))))))

(define (count-marks-group group receiver)
  (let loop ((marks (group-marks group)) (receiver receiver))
    (if (weak-pair? marks)
	(loop (weak-cdr marks)
	  (lambda (n-existing n-gced)
	    (if (weak-pair/car? marks)
		(receiver (1+ n-existing) n-gced)
		(receiver n-existing (1+ n-gced)))))
	(receiver 0 0))))

(define-command debug-show-standard-marks
  ""
  ()
  (lambda ()
    (with-output-to-temporary-buffer "*standard-marks*" '()
      (lambda ()
	(let ((buffer-frame (current-window)))
	  (let ((window
		 (vector-ref (instance-ref buffer-frame 'text-inferior) 1))
		(buffer (window-buffer buffer-frame)))
	    (let ((show-mark
		   (lambda (name mark)
		     (write-string
		      (string-pad-right (write-to-string name) 24))
		     (write mark)
		     (newline))))
	      (let ((show-instance
		     (lambda (name)
		       (show-mark name (instance-ref window name)))))
		(show-instance 'point)
		(show-instance 'current-start-mark)
		(show-instance 'start-mark)
		(show-instance 'start-line-mark)
		(show-instance 'current-end-mark))
	      (let ((group (buffer-group buffer)))
		(show-mark 'group-start-mark (group-start-mark group))
		(show-mark 'group-end-mark (group-end-mark group))
		(show-mark 'group-display-start (group-display-start group))
		(show-mark 'group-display-end (group-display-end group)))
	      (let ((marks (ring-list (buffer-mark-ring buffer))))
		(if (not (null? marks))
		    (begin
		      (write-string "mark-ring\t\t")
		      (write (car marks))
		      (newline)
		      (for-each (lambda (mark)
				  (write-string "\t\t\t")
				  (write mark)
				  (newline))
				(cdr marks))))))))))))

;;;; Object System Debugging

(define (instance-ref object name)
  (let ((entry (assq name (class-instance-transforms (object-class object)))))
    (if (not entry)
	(error "Not a valid instance-variable name:" name))
    (vector-ref object (cdr entry))))

(define (instance-set! object name value)
  (let ((entry (assq name (class-instance-transforms (object-class object)))))
    (if (not entry)
	(error "Not a valid instance-variable name:" name))
    (vector-set! object (cdr entry) value)))

;;;; Screen Trace

(define trace-output '())

(define (debug-tracer . args)
  (set! trace-output (cons args trace-output))
  unspecific)

(define (screen-trace #!optional screen)
  (let ((screen
	 (if (default-object? screen)
	     (begin
	       (if (not edwin-editor)
		   (error "No screen to trace."))
	       (editor-selected-screen edwin-editor))
	     screen)))
    (set! trace-output '())
    (for-each (lambda (window)
		(set-window-debug-trace! window debug-tracer))
	      (screen-window-list screen))
    (set-screen-debug-trace! screen debug-tracer)))

(define (screen-untrace #!optional screen)
  (let ((screen
	 (if (default-object? screen)
	     (begin
	       (if (not edwin-editor)
		   (error "No screen to trace."))
	       (editor-selected-screen edwin-editor))
	     screen)))
    (for-each (lambda (window)
		(set-window-debug-trace! window false))
	      (screen-window-list screen))
    (set-screen-debug-trace! screen false)
    (let ((result trace-output))
      (set! trace-output '())
      (map list->vector (reverse! result)))))