;;; -*-Scheme-*-
;;;
;;; $Id: comhst.scm,v 1.11 2007/01/05 21:19:23 cph Exp $
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

;;;; Command interpreter history
;;; Translated from "comint.el", by Olin Shivers.


(define-variable-per-buffer comint-input-ring-size
  "Size of input history ring."
  30)

(define-variable comint-input-ring "" #f)
(variable-permanent-local! (ref-variable-object comint-input-ring))

(define-variable comint-last-input-match "" #f)
(variable-permanent-local! (ref-variable-object comint-last-input-match))

(define comint-input-ring-tag '(COMINT-INPUT-RING))

(define (comint-record-input ring string)
  (if (or (ring-empty? ring)
	  (not (string=? string (ring-ref ring 0))))
      (ring-push! ring string)))

(define-command comint-previous-input
  "Cycle backwards through input history."
  "*p"
  (lambda (argument)
    (let ((point (current-point))
	  (ring (ref-variable comint-input-ring)))
      (let ((size (+ (ring-size ring) 1)))
	(let ((index
	       (modulo (+ argument
			  (command-message-receive comint-input-ring-tag
			    (lambda (index left right)
			      (delete-string left right)
			      (set-current-mark! left)
			      index)
			    (lambda ()
			      (push-current-mark! point)
			      (cond ((positive? argument) 0)
				    ((negative? argument) 2)
				    (else 1)))))
		       size)))
	  (message (number->string index))
	  (if (positive? index)
	      (without-interrupts
	       (lambda ()
		 (let ((start (mark-temporary-copy point)))
		   (insert-string (ring-ref ring (- index 1)) point)
		   (set-command-message! comint-input-ring-tag
					 index
					 (mark-left-inserting-copy start)
					 (mark-right-inserting-copy point)))))
	      (set-command-message! comint-input-ring-tag
				    index point point)))))))
	 
(define-command comint-next-input
  "Cycle forwards through input history."
  "*p"
  (lambda (argument)
    ((ref-command comint-previous-input) (- argument))))

(define-command comint-history-search-backward
  "Search backwards through the input history for a matching substring."
  (lambda ()
    (list (prompt-for-string "History search backward"
			     (ref-variable comint-last-input-match))))
  (lambda (string)
    (comint-history-search string true)))

(define-command comint-history-search-forward
  "Search forwards through the input history for a matching substring."
  (lambda ()
    (list (prompt-for-string "History search forward"
			     (ref-variable comint-last-input-match))))
  (lambda (string)
    (comint-history-search string false)))

(define (comint-history-search string backward?)
  (let ((ring (ref-variable comint-input-ring))
	(syntax-table (ref-variable syntax-table))
	(pattern (re-compile-pattern (re-quote-string string) false)))
    (let ((size (+ (ring-size ring) 1)))
      (call-with-values
	  (lambda ()
	    (command-message-receive comint-input-ring-tag
	      values
	      (lambda ()
		(let ((point (current-point)))
		  (values (if backward? 0 size) point point)))))
	(lambda (start left right)
	  (let loop ((index start))
	    (let ((index (+ index (if backward? 1 -1))))
	      (cond ((if backward? (>= index size) (< index 0))
		     (set-command-message! comint-input-ring-tag
					   start left right)
		     (editor-failure "Not found"))
		    ((re-string-search-forward pattern
					       (ring-ref ring (- index 1))
					       #f
					       syntax-table)
		     (set-variable! comint-last-input-match string)
		     ((ref-command comint-previous-input) (- index start)))
		    (else
		     (loop index))))))))))
