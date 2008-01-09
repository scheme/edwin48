;;; -*-Scheme-*-
;;;
;;; $Id: regcom.scm,v 1.27 2007/01/05 21:19:24 cph Exp $
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

;;;; Register Commands


(define-command point-to-register
  "Store current location of point in a register."
  "cPoint to register"
  (lambda (register)
    (set-register! register
		   (make-buffer-position (current-point) (current-buffer)))))

(define-command register-to-point
  "Move point to location stored in a register."
  "cRegister to Point"
  (lambda (register)
    (let ((value (get-register register)))
      (if (not (buffer-position? value))
	  (register-error register "does not contain a buffer position."))
      (select-buffer
       (or (buffer-position-buffer value)
	   (register-error register
			   "points to a buffer which has been deleted")))
      (set-current-point! (buffer-position-mark value)))))

(define-command number-to-register
  "Store a number in a given register.
With prefix arg, stores that number in the register.
Otherwise, reads digits from the buffer starting at point."
  "cNumber to Register\nP"
  (lambda (register argument)
    (set-register! register
		   (or argument
		       (let ((start (current-point))
			     (end (skip-chars-forward "[0-9]")))
			 (if (mark= start end)
			     0
			     (with-input-from-region (make-region start end)
						     read)))))))

(define-command increment-register
  "Add the prefix arg to the contents of a given register.
The prefix defaults to one."
  "cIncrement register\np"
  (lambda (register argument)
    (let ((value (get-register register)))
      (if (not (integer? value))
	  (register-error register "does not contain a number"))
      (set-register! register (+ value argument)))))

(define-command copy-to-register
  "Copy region into given register.
With prefix arg, delete as well."
  "cCopy to register\nr\nP"
  (lambda (register region delete?)
    (set-register! register (region->string region))
    (if delete? (region-delete! region))))

(define-command insert-register
  "Insert contents of given register at point.
Normally puts point before and mark after the inserted text.
With prefix arg, puts mark before and point after."
  "cInsert Register\nP"
  (lambda (register argument)
    (let* ((start (mark-right-inserting-copy (current-point)))
	   (end (mark-left-inserting-copy start)))
      (insert-string (let ((value (get-register register)))
		       (cond ((string? value) value)
			     ((integer? value) (number->string value))
			     (else
			      (register-error register
					      "does not contain text"))))
		     start)
      (mark-temporary! end)
      (mark-temporary! start)
      (if argument
	  (begin (push-current-mark! start) (set-current-point! end))
	  (begin (push-current-mark! end) (set-current-point! start))))))

(define-command append-to-register
  "Append region to text in given register.
With prefix arg, delete as well."
  "cAppend to register\nr\nP"
  (lambda (register region argument)
    (let ((value (get-register register)))
      (if (not (string? value))
	  (register-error register "does not contain text"))
      (set-register! register (string-append value (region->string region))))
    (if argument (region-delete! region))))

(define-command prepend-to-register
  "Prepend region to text in given register.
With prefix arg, delete as well."
  "cPrepend to register\nr\nP"
  (lambda (register region argument)
    (let ((value (get-register register)))
      (if (not (string? value))
	  (editor-error register "does not contain text"))
      (set-register! register (string-append (region->string region) value)))
    (if argument (region-delete! region))))

(define-command view-register
  "Display what is contained in a given register."
  "cView register"
  (lambda (register)
    (let ((value (get-register register)))
      (if (not value)
	  (message "Register " (key-name register) " is empty")
	  (with-output-to-temporary-buffer "*Output*" '()
	    (lambda ()
	      (write-string "Register ")
	      (write-string (key-name register))
	      (write-string " contains ")
	      (cond ((integer? value)
		     (write value))
		    ((string? value)
		     (write-string "the string:\n")
		     (write-string value))
		    ((buffer-position? value)
		     (let ((buffer (buffer-position-buffer value)))
		       (if (not buffer)
			   (write-string "an invalid buffer position")
			   (begin
			     (write-string "a buffer position:\nbuffer ")
			     (write-string (buffer-name buffer))
			     (write-string ", position ")
			     (write
			      (mark-index (buffer-position-mark value)))))))
		    (else
		     (write-string "a random object:\n")
		     (write value)))))))))

(define (register-error register . strings)
  (apply editor-error "Register " (key-name register) " " strings))

(define (get-register char)
  (let ((entry (assv char register-alist)))
    (and entry
	 (cdr entry))))

(define (set-register! char value)
  (let ((entry (assv char register-alist)))
    (if entry
	(set-cdr! entry value)
	(set! register-alist
	      (cons (cons char value)
		    register-alist)))))

(define register-alist
  '())

(define (make-buffer-position mark buffer)
  (cons buffer-position-tag (cons mark (hash buffer))))

(define (buffer-position? object)
  (and (pair? object)
       (eq? buffer-position-tag (car object))))

(define buffer-position-tag
  "Buffer Position")

(define (buffer-position-mark position)
  (cadr position))

(define (buffer-position-buffer position)
  (unhash (cddr position)))
