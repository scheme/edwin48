#| -*-Scheme-*-

$Id: pwedit.scm,v 1.14 2007/01/05 21:19:24 cph Exp $

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

;;;; Password-Database Editor

;;; This program provides editing capabilities for a text-format
;;; password database.  The primary capability of this program is to
;;; permit editing a database of passwords without having all of the
;;; passwords visible on the screen at once.  Instead, the program
;;; displays a set of key names, and the user selectively reveals the
;;; password information hidden behind those keys.

(declare (usual-integrations))

(define-command view-password-file
  "Read in a password file and show it in password-view mode.
Reads the file specified in the variable password-file.
If password-file is #f, or if prefix arg supplied, prompts for a filename."
  (lambda ()
    (list
     (let ((pathname
	    (let ((filename (ref-variable password-file)))
	      (and filename
		   (merge-pathnames filename (user-homedir-pathname)))))
	   (prompt
	    (lambda (default)
	      (prompt-for-existing-file "View password file" default))))
       (cond ((not pathname) (prompt #f))
	     ((command-argument) (prompt (list pathname)))
	     (else (->namestring pathname))))))
  (lambda (pathname)
    (let ((forms
	   (call-with-temporary-buffer " view-pw-file"
	     (lambda (buffer)
	       (read-buffer buffer pathname #f)
	       (read-pw-forms
		(make-buffer-input-port (buffer-start buffer)
					(buffer-end buffer)))))))
      (let ((buffer (new-buffer (pathname->buffer-name pathname))))
	(insert-pw-forms forms (buffer-start buffer))
	(set-buffer-major-mode! buffer (ref-mode-object password-view))
	(set-buffer-default-directory! buffer (directory-pathname pathname))
	(set-buffer-point! buffer (buffer-start buffer))
	(select-buffer buffer)))))

(define-major-mode password-view read-only "Password-View"
  "Major mode specialized for viewing password files."
  (lambda (buffer)
    (set-buffer-read-only! buffer)
    unspecific))

(define-key 'password-view #\space 'toggle-pw-form)
(define-key 'password-view button1-down 'mouse-toggle-pw-form)

(define-command toggle-pw-form
  "Toggle the body of the password form under point."
  "d"
  (lambda (point)
    (if (get-pw-form point)
	(toggle-pw-body point)
	(message "No form under point."))))

(define-command mouse-toggle-pw-form
  "Toggle the body of the password form under mouse."
  ()
  (lambda ()
    ((ref-command toggle-pw-form)
     (let ((button-event (current-button-event)))
       (let ((window (button-event/window button-event)))
	 (select-window window)
	 (or (window-coordinates->mark window
				       (button-event/x button-event)
				       (button-event/y button-event))
	     (buffer-end (window-buffer window))))))))

(define (insert-pw-forms pw-forms point)
  (let ((point (mark-left-inserting-copy point)))
    (for-each
     (lambda (form)
       (let ((type (car form))
	     (body (cdr form))
	     (start (mark-right-inserting-copy point)))
	 (case type
	   ((BLANK)
	    (insert-newline point))
	   ((COMMENT)
	    (for-each (lambda (line)
			(insert-string ";" point)
			(insert-string line point)
			(insert-newline point))
		      body))
	   ((SHORT LONG)
	    (insert-string (car body) point)
	    (insert-string ":" point)
	    (insert-newline point))
	   (else
	    (error "Unknown form type:" type)))
	 (region-put! start point 'PW-FORM form)
	 (mark-temporary! start)))
     pw-forms)
    (mark-temporary! point)))

(define (get-pw-form point)
  (let ((form (region-get point 'PW-FORM #f)))
    (and form
	 (memq (car form) '(SHORT LONG))
	 form)))

(define (toggle-pw-body point) (modify-pw-body point 'TOGGLE))
(define (insert-pw-body point) (modify-pw-body point 'INSERT))
(define (delete-pw-body point) (modify-pw-body point 'DELETE))

(define (modify-pw-body point operation)
  (with-buffer-open (mark-buffer point)
    (lambda ()
      (let ((form
	     (or (get-pw-form point)
		 (error:bad-range-argument point 'INSERT-PW-BODY)))
	    (region (pw-body-region point)))
	(if region
	    (region-delete! region))
	(if (or (eq? 'INSERT operation)
		(and (eq? 'TOGGLE operation)
		     (not region)))
	    (let ((start (mark-right-inserting-copy (line-end point 0)))
		  (end (mark-left-inserting-copy (line-end point 0))))
	      (if (eq? 'SHORT (car form))
		  (begin
		    (insert-pw-body-spacer end)
		    (insert-string (cddr form) end))
		  (for-each (lambda (line)
			      (insert-newline end)
			      (if (pair? line)
				  (begin
				    (insert-string (car line) end)
				    (insert-string ":" end)
				    (insert-pw-body-spacer end)
				    (insert-string (cdr line) end))
				  (insert-string line end)))
			    (cddr form)))
	      (region-put! start end 'PW-FORM form)
	      (mark-temporary! start)
	      (mark-temporary! end)))))))

(define (pw-body-region point)
  (let ((group (mark-group point))
	(index (mark-index point))
	(key 'PW-FORM))
    (let ((g-start (group-start-index group))
	  (g-end (group-end-index group)))
      (let ((start
	     (if (and (fix:< g-start index)
		      (eq? (get-text-property group (fix:- index 1) key #f)
			   (get-text-property group index key #f)))
		 (let ((start
			(previous-specific-property-change group g-start index
							   key)))
		   (if start
		       (fix:+ start 1)
		       g-start))
		 index))
	    (end
	     (if (and (fix:< index g-end)
		      (eq? (get-text-property group index key #f)
			   (get-text-property group (fix:+ index 1) key #f)))
		 (let ((end
			(next-specific-property-change group index g-end key)))
		   (if end
		       (fix:- end 1)
		       g-end))
		 index)))
	(let ((start
	       (group-find-next-char group start (line-end-index group start)
				     #\:)))
	  (if (not start)
	      (error "Can't find colon:" point))
	  (let ((start (fix:+ start 1)))
	    (and (fix:< start end)
		 (make-region (make-mark group start)
			      (make-mark group end)))))))))

(define (insert-pw-body-spacer point)
  (insert-string (let ((column (mark-column point)))
		   (cond ((< column 8) "\t\t")
			 ((< column 16) "\t")
			 (else " ")))
		 point))