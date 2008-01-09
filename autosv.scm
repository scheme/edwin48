;;; -*-Scheme-*-
;;;
;;; $Id: autosv.scm,v 1.39 2007/01/05 21:19:23 cph Exp $
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

;;;; Auto Save


(define-variable auto-save-visited-file-name
  "True says auto-save a buffer in the file it is visiting, when practical.
Normally auto-save files are written under other names."
  #f
  boolean?)

(define-variable auto-save-default
  "True says by default do auto-saving of every file-visiting buffer."
  #t
  boolean?)
(variable-permanent-local! (ref-variable-object auto-save-default))

(define-variable auto-save-interval
  "Number of keyboard input characters between auto-saves.
Zero means disable autosaving."
  300
  exact-nonnegative-integer?)

(define-variable delete-auto-save-files
  "True means delete a buffer's auto-save file
when the buffer is saved for real."
  #t
  boolean?)

(define-command auto-save-mode
  "Toggle auto-saving of contents of current buffer.
With arg, turn auto-saving on if arg is positive, else off."
  "P"
  (lambda (argument)
    (let ((argument (command-argument-value argument))
	  (buffer (current-buffer)))
      (if (if argument
	      (positive? argument)
	      (not (buffer-auto-save-pathname buffer)))
	  (begin
	    (enable-buffer-auto-save! buffer)
	    (message "Auto Save enabled"))
	  (begin
	    (disable-buffer-auto-save! buffer)
	    (message "Auto Save disabled"))))))

(define-command do-auto-save
  "Auto-save all buffers that need it.
This is all buffers that have auto-saving enabled
and are changed since last auto-saved.
Auto-saving writes the buffer into a file
so that your editing is not lost if the system crashes.
This file is not the file you visited; that changes only when you save."
  ()
  (lambda () (do-auto-save)))

(define (setup-buffer-auto-save! buffer)
  (if (ref-variable auto-save-default buffer)
      (enable-buffer-auto-save! buffer)
      (disable-buffer-auto-save! buffer)))

(define (enable-buffer-auto-save! buffer)
  (let ((pathname
	 (let ((pathname (buffer-pathname buffer)))
	   (if (and pathname (ref-variable auto-save-visited-file-name))
	       pathname
	       (os/auto-save-pathname pathname buffer)))))
    (without-interrupts
     (lambda ()
       (set-buffer-auto-save-pathname! buffer pathname)
       (add-group-microcode-entry (buffer-group buffer)
				  (->namestring pathname))
       (add-kill-buffer-hook buffer auto-save-kill-buffer-hook)))))

(define (disable-buffer-auto-save! buffer)
  (without-interrupts
   (lambda ()
     (set-buffer-auto-save-pathname! buffer #f)
     (remove-group-microcode-entry (buffer-group buffer))
     (remove-kill-buffer-hook buffer auto-save-kill-buffer-hook))))

(define (auto-save-kill-buffer-hook buffer)
  (without-interrupts
   (lambda ()
     (remove-group-microcode-entry (buffer-group buffer)))))

(define add-group-microcode-entry)
(define remove-group-microcode-entry)
(let ((index (fixed-objects-vector-slot 'EDWIN-AUTO-SAVE)))
  (set! add-group-microcode-entry
	(lambda (group namestring)
	  (let ((vector (get-fixed-objects-vector)))
	    (let ((alist (vector-ref vector index)))
	      (let ((entry (assq group alist)))
		(if entry
		    (set-cdr! entry namestring)
		    (vector-set! vector
				 index
				 (cons (cons group namestring) alist))))))))
  (set! remove-group-microcode-entry
	(lambda (group)
	  (let ((vector (get-fixed-objects-vector)))
	    (vector-set! vector
			 index
			 (del-assq! group (vector-ref vector index)))))))

(define (delete-auto-save-file! buffer)
  (and (ref-variable delete-auto-save-files)
       (let ((auto-save-pathname (buffer-auto-save-pathname buffer)))
	 (and auto-save-pathname
	      (not (let ((pathname (buffer-pathname buffer)))
		     (and pathname
			  (pathname=? auto-save-pathname pathname))))
	      (delete-file-no-errors auto-save-pathname)))))

(define (rename-auto-save-file! buffer)
  (let ((old-pathname (buffer-auto-save-pathname buffer)))
    (enable-buffer-auto-save! buffer)
    (let ((new-pathname (buffer-auto-save-pathname buffer)))
      (if (and old-pathname
	       new-pathname
	       (not (pathname=? new-pathname old-pathname))
	       (not (let ((pathname (buffer-pathname buffer)))
		      (and pathname
			   (or (pathname=? new-pathname pathname)
			       (pathname=? old-pathname pathname)))))
	       (file-exists? old-pathname))
	  (rename-file old-pathname new-pathname)))))

(define (do-auto-save)
  (let ((buffers
	 (list-transform-positive (buffer-list)
	   (lambda (buffer)
	     (and (buffer-auto-save-pathname buffer)
		  (buffer-auto-save-modified? buffer)
		  (<= (* 10 (buffer-save-length buffer))
		      (* 13 (buffer-length buffer))))))))
    (if (not (null? buffers))
	(begin
	  (temporary-message "Auto saving...")
	  (for-each auto-save-buffer buffers)
	  (temporary-message "Auto saving...done")))))

(define (auto-save-buffer buffer)
  (catch-file-errors
   (lambda (condition)
     condition
     (editor-beep)
     (let ((name (buffer-name buffer)))
       (message "Autosaving...error for " name)
       (sleep-for 500)
       (message "Autosaving...error!for " name)
       (sleep-for 500)
       (message "Autosaving...error for " name)
       (sleep-for 500)))
   (lambda ()
     (write-region (buffer-unclipped-region buffer)
		   (buffer-auto-save-pathname buffer)
		   #f
		   'DEFAULT)
     (set-buffer-save-length! buffer)
     (set-buffer-auto-saved! buffer))))
