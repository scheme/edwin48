#| -*-Scheme-*-

$Id: strtab.scm,v 1.53 2008/01/30 20:02:06 cph Exp $

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

;;;; String Tables


(define-record-type* string-table
  (%make-string-table (vector) (size) ci?)
  ())

(define* (make-string-table (initial-size 16) (ci? #t))
  (%make-string-table (make-vector initial-size) 0 ci?))

(define* (alist->string-table alist (ci? #t))
  (let* ((compare   (if ci? string-ci<? string<?))
	 (sort-func (lambda (x y) (compare (car x) (car y))))
	 (v         (list->vector (sort alist sort-func))))
      (%make-string-table v (vector-length v) ci?)))

(define make-string-table-entry cons)
(define string-table-entry-string car)
(define string-table-entry-value cdr)
(define set-string-table-entry-string! set-car!)
(define set-string-table-entry-value! set-cdr!)

(define (string-table-search table string1 if-found if-not-found)
  (let ((vector (string-table-vector table)))
    (let loop ((low 0) (high (-1+ (string-table-size table))))
      (if (< high low)
	  (if-not-found low)
	  (let ((index (quotient (+ high low) 2)))
	    (let ((entry (vector-ref vector index)))
	      ((if (string-table-ci? table) string-compare-ci string-compare)
	       string1
	       (string-table-entry-string entry)
	       (lambda (i) (loop low (-1+ index)))
	       (lambda (i) (if-found index entry))
	       (lambda (i) (loop (1+ index) high)))))))))

(define* (string-table-get table string
			   (if-not-found (lambda (index) index #f)))
  (string-table-search table string
    (lambda (index entry)
      index				;ignore
      (string-table-entry-value entry))
    if-not-found))

(define (string-table-put! table string value)
  (string-table-search table string
    (lambda (index entry)
      index				;ignore
      (set-string-table-entry-string! entry string)
      (set-string-table-entry-value! entry value))
    (lambda (index)
      (let ((vector (string-table-vector table))
	    (size (string-table-size table))
	    (entry (make-string-table-entry string value)))
	(let ((max-size (vector-length vector)))
	  (if (= size max-size)
	      (let ((new-vector (vector-grow vector (* max-size 2))))
		(set-string-table-vector! table new-vector)
		(set! vector new-vector)))
	  (subvector-move-right! vector index size vector (1+ index))
	  (vector-set! vector index entry))
	(set-string-table-size! table (1+ size))))))

(define (string-table-remove! table string)
  (string-table-search table string
    (lambda (index entry)
      entry				;ignore
      (let ((vector (string-table-vector table))
	    (size (string-table-size table)))
	(subvector-move-left! vector (1+ index) size vector index)
	(let ((new-size (-1+ size)))
	  (vector-set! vector new-size '())
	  (set-string-table-size! table new-size)))
      #t)
    (lambda (index) index #f)))

(define (string-table-complete table string
			       if-unique if-not-unique if-not-found)
  (%string-table-complete table string
    if-unique
    (lambda (close-match gcs lower upper)
      (if-not-unique
       (substring close-match 0 gcs)
       (lambda ()
	 (let loop ((index lower))
	   (if (= index upper)
	       '()
	       (cons (string-table-entry-string
		      (vector-ref (string-table-vector table) index))
		     (loop (1+ index))))))))
    if-not-found))

(define (string-table-completions table string)
  (%string-table-complete table string
    list
    (lambda (close-match gcs lower upper)
      close-match gcs			;ignore
      (let loop ((index lower))
	(if (= index upper)
	    '()
	    (cons (string-table-entry-string
		   (vector-ref (string-table-vector table) index))
		  (loop (1+ index))))))
    (lambda () '())))

(define (string-table-apropos table regexp)
  (let ((end (string-table-size table))
	(pattern (re-compile-pattern regexp (string-table-ci? table))))
    (let loop ((index 0))
      (if (= index end)
	  '()
	  (let ((entry (vector-ref (string-table-vector table) index)))
	    (if (re-string-search-forward pattern
					  (string-table-entry-string entry))
		(cons (string-table-entry-value entry) (loop (1+ index)))
		(loop (1+ index))))))))

(define (%string-table-complete table string
				if-unique if-not-unique if-not-found)
  (let ((size (string-length string))
	(table-size (string-table-size table))
	(entry-string
	 (lambda (index)
	   (string-table-entry-string
	    (vector-ref (string-table-vector table) index))))
	(match-forward
	 (if (string-table-ci? table)
	     string-prefix-length-ci
	     string-prefix-length)))
    (let ((perform-search
	   (lambda (index)
	     (let ((close-match (entry-string index)))
	       (let ((match-entry
		      (lambda (index)
			(match-forward close-match (entry-string index)))))
		 (define (scan-up gcs receiver)
		   (let loop ((gcs gcs) (index (1+ index)))
		     (if (= index table-size)
			 (receiver gcs table-size)
			 (let ((match (match-entry index)))
			   (if (< match size)
			       (receiver gcs index)
			       (loop (min gcs match) (1+ index)))))))
		 (define (scan-down gcs receiver)
		   (let loop ((gcs gcs) (index index))
		     (if (zero? index)
			 (receiver gcs 0)
			 (let ((new-index (-1+ index)))
			   (let ((match (match-entry new-index)))
			     (if (< match size)
				 (receiver gcs index)
				 (loop (min gcs match) new-index)))))))
		 (if ((if (string-table-ci? table)
			  string-prefix-ci?
			  string-prefix?)
		      string
		      close-match)
		     (scan-up (string-length close-match)
		       (lambda (gcs upper)
			 (scan-down gcs
			   (lambda (gcs lower)
			     (if (= lower (-1+ upper))
				 (if-unique (entry-string lower))
				 (if-not-unique close-match
						gcs lower upper))))))
		     (if-not-found)))))))
      (string-table-search table string
	(lambda (index entry)
	  entry				;ignore
	  (perform-search index))
	(lambda (index)
	  (if (= index table-size)
	      (if-not-found)
	      (perform-search index)))))))