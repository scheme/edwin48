;;; -*-Scheme-*-
;;;
;;; $Id: print.scm,v 1.25 2007/01/05 21:19:24 cph Exp $
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

;;;; Print Buffers and Regions


(define-command lpr-buffer
  "Print buffer contents as with Unix command `lpr'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  '()
  (lambda ()
    (print-region/internal (buffer-region (current-buffer)) false)))

(define-command print-buffer
  "Print buffer contents as with Unix command `lpr -p'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  '()
  (lambda ()
    (print-region/internal (buffer-region (current-buffer)) true)))

(define-command lpr-region
  "Print region contents as with Unix command `lpr'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  "r"
  (lambda  (region)
    (print-region/internal region false)))

(define-command print-region
  "Print region contents as with Unix command `lpr -p'.
Variable LPR-SWITCHES is a list of extra switches (strings) to pass to lpr."
  "r"
  (lambda  (region)
    (print-region/internal region true)))

(define (print-region/internal region print-headers?)
  ((message-wrapper #f "Spooling")
   (lambda ()
     (let ((buffer (mark-buffer (region-start region)))
	   (print-headers? (and print-headers? (not lpr-print-not-special?)))
	   (title (print-region-title-string region)))
       (let ((call-printer
	      (lambda (region)
		((or (ref-variable lpr-procedure buffer)
		     (case microcode-id/operating-system
		       ((NT) print-region/nt)
		       (else print-region/default)))
		 region print-headers? title buffer)))
	     (width (ref-variable tab-width buffer)))
	 (if (= width 8)
	     (call-printer region)
	     (call-with-temporary-buffer " *spool temp*"
	       (lambda (temp-buffer)
		 (insert-region (region-start region)
				(region-end region)
				(buffer-point temp-buffer))
		 (define-variable-local-value! temp-buffer
		   (ref-variable-object tab-width)
		   width)
		 (untabify-region (buffer-start temp-buffer)
				  (buffer-end temp-buffer))
		 (call-printer (buffer-region temp-buffer))))))))))

(define (print-region/default region print-headers? title buffer)
  (shell-command region false false false
		 (string-append
		  (ref-variable lpr-command buffer)
		  (print/assemble-switches title
					   (if print-headers? '("-p") '())))))

(define (print-region/nt region print-headers? title buffer)
  print-headers? title buffer
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (call-with-temporary-buffer " print"
       (lambda (temp)
	 (insert-region (region-start region)
			(region-end region)
			(buffer-end temp))
	 (insert-char #\page (buffer-end temp))
	 (write-region (buffer-region temp) pathname #f #t)))
     (shell-command #f #f #f #f
		    (string-append "copy "
				   (->namestring pathname)
				   " prn")))))

(define (print-region-title-string region)
  (let ((buffer-title
	 (let ((buffer (mark-buffer (region-start region))))
	   (and buffer
		(or (let ((pathname (buffer-pathname buffer)))
		      (and pathname
			   (not (directory-pathname? pathname))
			   (file-namestring pathname)))
		    (string-append "Edwin buffer " (buffer-name buffer)))))))
    (if (or (not buffer-title)
	    (and (group-start? (region-start region))
		 (group-end? (region-end region))))
	buffer-title
	(string-append "region from " buffer-title))))

(define (print/assemble-switches title additional-switches)
  (apply string-append
	 (let loop
	     ((switches
	       (let ((switches (ref-variable lpr-switches)))
		 (append additional-switches
			 (let ((job-name (or (print/job-name) title)))
			   (if job-name
			       (list (string-append "-J \"" job-name "\""))
			       '()))
			 (if (and title
				  (not (there-exists? switches
					 (lambda (switch)
					   (string-prefix? "-T" switch)))))
			     (list (string-append "-T \"" title "\""))
			     '())
			 switches))))
	   (if (null? switches)
	       (list "")
	       (cons* " "
		      (car switches)
		      (loop (cdr switches)))))))

(define print/job-name
  (let ((most-recent-name false))
    (lambda ()
      (and lpr-prompt-for-name?
	   (let ((job-name
		  (prompt-for-string "Name to print on title page"
				     most-recent-name
				     'DEFAULT-TYPE 'INSERTED-DEFAULT)))
	     (if (string-null? job-name)
		 false
		 (begin
		   (set! most-recent-name job-name)
		   job-name)))))))
