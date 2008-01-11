#| -*-Scheme-*-

$Id: motion.scm,v 1.93 2007/01/05 21:19:23 cph Exp $

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

;;;; Motion within Groups


;;;; Motion by Characters

(define (limit-mark-motion limit? limit)
  (cond ((eq? limit? 'LIMIT) limit)
	((eq? limit? 'BEEP) (editor-beep) limit)
	((eq? limit? 'FAILURE) (editor-failure) limit)
	((eq? limit? 'ERROR) (editor-error))
	((not limit?) false)
	(else (error "Unknown limit type:" limit?))))

(define (mark1+ mark #!optional limit?)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (if (group-end-index? group index)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   (group-end-mark group))
	(make-mark group (fix:+ index 1)))))

(define (mark-1+ mark #!optional limit?)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (if (group-start-index? group index)
	(limit-mark-motion (and (not (default-object? limit?)) limit?)
			   (group-start-mark group))
	(make-mark group (fix:- index 1)))))

(define (region-count-chars region)
  (fix:- (region-end-index region) (region-start-index region)))

(define (%mark+ mark n limit?)
  (let ((group (mark-group mark))
	(new-index (fix:+ (mark-index mark) n)))
    (if (fix:> new-index (group-end-index group))
	(limit-mark-motion limit? (group-end-mark group))
	(make-mark group new-index))))

(define (%mark- mark n limit?)
  (let ((group (mark-group mark))
	(new-index (fix:- (mark-index mark) n)))
    (if (fix:< new-index (group-start-index group))
	(limit-mark-motion limit? (group-start-mark group))
	(make-mark group new-index))))

(define (mark+ mark+ mark n #!optional limit?)
  (let ((limit? (and (not (default-object? limit?)) limit?)))
    (cond ((fix:positive? n) (%mark+ mark n limit?))
	  ((fix:negative? n) (%mark- mark (fix:- 0 n) limit?))
	  (else mark))))

(define (mark- mark- mark n #!optional limit?)
  (let ((limit? (and (not (default-object? limit?)) limit?)))
    (cond ((fix:positive? n) (%mark- mark n limit?))
	  ((fix:negative? n) (%mark+ mark (fix:- 0 n) limit?))
	  (else mark))))


;;;; Motion by Lines

(define (line-start-index group index)
  (let ((limit (group-start-index group)))
    (let ((index (group-find-previous-char group limit index #\newline)))
      (if index
	  (fix:+ index 1)
	  limit))))

(define (line-end-index group index)
  (let ((limit (group-end-index group)))
    (or (group-find-next-char group index limit #\newline)
	limit)))

(define (line-start-index? group index)
  (or (group-start-index? group index)
      (char=? (group-left-char group index) #\newline)))

(define (line-end-index? group index)
  (or (group-end-index? group index)
      (char=? (group-right-char group index) #\newline)))

(define (line-start mark n #!optional limit?)
  (let ((group (mark-group mark))
	(lose
	 (lambda (mark)
	   (limit-mark-motion (and (not (default-object? limit?)) limit?)
			      mark))))
    (if (fix:> n 0)
	(let ((limit (group-end-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-next-char group i limit #\newline)))
	      (cond ((not j) (lose (group-end-mark group)))
		    ((fix:= n 1) (make-mark group (fix:+ j 1)))
		    (else (loop (fix:+ j 1) (fix:- n 1)))))))
	(let ((limit (group-start-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-previous-char group limit i #\newline)))
	      (cond ((fix:= n 0) (make-mark group (if j (fix:+ j 1) limit)))
		    ((not j) (lose (group-start-mark group)))
		    (else (loop j (fix:+ n 1))))))))))

(define (line-end mark n #!optional limit?)
  (let ((group (mark-group mark))
	(lose
	 (lambda (mark)
	   (limit-mark-motion (and (not (default-object? limit?)) limit?)
			      mark))))
    (if (fix:< n 0)
	(let ((limit (group-start-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-previous-char group limit i #\newline)))
	      (cond ((not j) (lose (group-start-mark group)))
		    ((fix:= n -1) (make-mark group j))
		    (else (loop j (fix:+ n 1)))))))
	(let ((limit (group-end-index group)))
	  (let loop ((i (mark-index mark)) (n n))
	    (let ((j (group-find-next-char group i limit #\newline)))
	      (cond ((fix:= n 0) (make-mark group (or j limit)))
		    ((not j) (lose (group-end-mark group)))
		    (else (loop (fix:+ j 1) (fix:- n 1))))))))))

(define (line-start? mark)
  (line-start-index? (mark-group mark) (mark-index mark)))

(define (line-end? mark)
  (line-end-index? (mark-group mark) (mark-index mark)))

(define (region-count-lines region)
  (group-count-lines (region-group region)
		     (region-start-index region)
		     (region-end-index region)))

(define (group-count-lines group start end)
  (let loop ((start start) (n 0))
    (cond ((fix:= start end) n)
	  ((group-find-next-char group start end #\newline)
	   => (lambda (i) (loop (fix:+ i 1) (fix:+ n 1))))
	  (else (fix:+ n 1)))))

;;;; Motion by Columns

(define (mark-column mark)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (group-columns group
		   (line-start-index group index)
		   index
		   0
		   (group-tab-width group)
		   (group-char-image-strings group))))

(define (move-to-column mark column)
  (let ((group (mark-group mark))
	(index (mark-index mark)))
    (make-mark
     group
     (vector-ref (group-column->index group
				      (line-start-index group index)
				      (group-end-index group)
				      0
				      column
				      (group-tab-width group)
				      (group-char-image-strings group))
		 0))))