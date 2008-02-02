#| -*-Scheme-*-

$Id: things.scm,v 1.94 2008/01/30 20:02:06 cph Exp $

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

;;;; Textual Entities


;;;; Motion Primitives

;;; This file "defines" various kinds of things like lines, pages,
;;; words, etc.  The "definition" of a FOO entity consists of two
;;; procedures, FORWARD-FOO and BACKWARD-FOO, each of which takes
;;; three arguments: [1] a mark to start from, [2] the number of FOOs
;;; to traverse, and [3] a limit for LIMIT-MARK-MOTION.  The value of
;;; the procedure should be either a mark or #F.

;;; If the number is positive, traverse that many FOOs in the given
;;; direction; if negative, in the opposite direction; and zero means
;;; don't move.  It is assumed that no two FOOs overlap; they may or
;;; may not touch one another.  When moving forward, stop to the right
;;; of the rightmost edge of the FOO.  When moving backward, stop to
;;; the left of the leftmost edge.

;;; MAKE-MOTION-PAIR will generate these two procedures, given the
;;; simpler primitives to move forward or backward once.

(define (make-motion-pair forward-one-thing backward-one-thing receiver)
  (define (forward-thing mark n #!optional limit?)
    (let ((limit? (and (not (default-object? limit?)) limit?)))
      (cond ((positive? n) (%forward-thing mark n limit?))
	    ((negative? n) (%backward-thing mark (- n) limit?))
	    (else mark))))

  (define (backward-thing mark n #!optional limit?)
    (let ((limit? (and (not (default-object? limit?)) limit?)))
      (cond ((positive? n) (%backward-thing mark n limit?))
	    ((negative? n) (%forward-thing mark (- n) limit?))
	    (else mark))))

  (define (%forward-thing mark n limit?)
    (let loop ((mark mark) (n n))
      (let ((end (forward-one-thing mark)))
	(cond ((not end) (limit-mark-motion limit? (group-end mark)))
	      ((= n 1) end)
	      (else (loop end (-1+ n)))))))

  (define (%backward-thing mark n limit?)
    (let loop ((mark mark) (n n))
      (let ((start (backward-one-thing mark)))
	(cond ((not start) (limit-mark-motion limit? (group-start mark)))
	      ((= n 1) start)
	      (else (loop start (-1+ n)))))))

  (receiver forward-thing backward-thing))

;;;; Generic Operations

(define (move-thing forward-thing argument limit?)
  (set-current-point! (forward-thing (current-point) argument limit?)))

(define (move-thing-saving-point forward-thing argument limit?)
  (push-current-mark! (current-point))
  (move-thing forward-thing argument limit?))

(define (mark-thing forward-thing n limit?)
  (push-current-mark! (forward-thing (current-point) n limit?)))

(define (kill-thing forward-thing n limit?)
  (kill-region (forward-thing (current-point) n limit?)))

(define (transpose-things forward-thing n)
  (cond ((> n 0)
	 (do ((i 0 (+ i 1)))
	     ((= i n))
	   (let* ((m4
		   (mark-right-inserting-copy
		    (forward-thing (current-point) 1 'ERROR)))
		  (m2
		   (mark-left-inserting-copy (forward-thing m4 -1 'ERROR)))
		  (m1
		   (mark-left-inserting-copy (forward-thing m2 -1 'ERROR)))
		  (m3 (forward-thing m1 1 'ERROR)))
	     (set-current-point! m4)
	     (insert-string (extract-and-delete-string m1 m3) m4)
	     (insert-string (extract-and-delete-string m2 m4) m1)
	     (mark-temporary! m1)
	     (mark-temporary! m2)
	     (mark-temporary! m4))))
	((< n 0)
	 (do ((i 0 (- i 1)))
	     ((= i n))
	   (let* ((m2
		   (mark-left-inserting-copy
		    (forward-thing (current-point) -1 'ERROR)))
		  (m1 (mark-left-inserting-copy (forward-thing m2 -1 'ERROR)))
		  (m3 (forward-thing m1 1 'ERROR))
		  (m4 (mark-right-inserting-copy (forward-thing m2 1 'ERROR))))
	     (insert-string (extract-and-delete-string m1 m3) m4)
	     (insert-string (extract-and-delete-string m2 m4) m1)
	     (set-current-point! m1)
	     (mark-temporary! m1)
	     (mark-temporary! m2)
	     (mark-temporary! m4))))
	(else
	 (let ((normalize
		(lambda (m)
		  (forward-thing (forward-thing m 1 'ERROR) -1 'ERROR)))
	       (exchange
		(lambda (m1 m2 set-m1! set-m2!)
		  (let ((m1 (mark-right-inserting-copy m1))
			(m3 (forward-thing m1 1 'ERROR))
			(m2 (mark-left-inserting-copy m2))
			(m4
			 (mark-right-inserting-copy
			  (forward-thing m2 1 'ERROR))))
		    (insert-string (extract-and-delete-string m1 m3) m4)
		    (insert-string (extract-and-delete-string m2 m4) m1)
		    (set-m1! m4)
		    (set-m2! m1)
		    (mark-temporary! m1)
		    (mark-temporary! m2)
		    (mark-temporary! m4)))))
	   (let ((m1 (normalize (current-point)))
		 (m2 (normalize (current-mark))))
	     (cond ((mark< m1 m2)
		    (exchange m1 m2 set-current-mark! set-current-point!))
		   ((mark< m2 m1)
		    (exchange m2 m1
			      set-current-point! set-current-mark!))))))))

;;;; Horizontal Space

(define (horizontal-space-region mark)
  (make-region (horizontal-space-start mark)
	       (horizontal-space-end mark)))

(define (horizontal-space-start mark)
  (skip-chars-backward " \t" mark))

(define (horizontal-space-end mark)
  (skip-chars-forward " \t" mark))

(define (compute-horizontal-space c1 c2 tab-width)
  ;; Compute the number of tabs/spaces required to fill from column C1
  ;; to C2 with whitespace.
  (if (< c1 c2)
      (error:bad-range-argument c2 'COMPUTE-HORIZONTAL-SPACE))
  (if tab-width
      (let ((qr1 (integer-divide c1 tab-width))
	    (qr2 (integer-divide c2 tab-width)))
	(if (> (integer-divide-quotient qr1) (integer-divide-quotient qr2))
	    (values (- (integer-divide-quotient qr1)
		       (integer-divide-quotient qr2))
		    (integer-divide-remainder qr1))
	    (values 0
		    (- (integer-divide-remainder qr1)
		       (integer-divide-remainder qr2)))))
      (values 0 (- c1 c2))))

(define (insert-horizontal-space target-column #!optional point tab-width)
  (let* ((point
	  (mark-left-inserting-copy
	   (if (default-object? point) (current-point) point)))
	 (tab-width
	  (if (default-object? tab-width)
	      (let ((buffer (mark-buffer point)))
		(and buffer
		     (variable-local-value
		      buffer
		      (ref-variable-object indent-tabs-mode))
		     (variable-local-value
		      buffer
		      (ref-variable-object tab-width))))
	      tab-width)))
    (with-values
	(lambda ()
	  (compute-horizontal-space target-column
				    (mark-column point)
				    tab-width))
      (lambda (n-tabs n-spaces)
	(insert-chars #\tab n-tabs point)
	(insert-chars #\space n-spaces point)))
    (mark-temporary! point)))

(define (delete-horizontal-space #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (delete-string (horizontal-space-start point)
		   (horizontal-space-end point))))

(define (indent-to target-column #!optional minimum point)
  (let ((minimum (if (default-object? minimum) 0 minimum))
	(point (if (default-object? point) (current-point) point)))
    (insert-horizontal-space (max target-column
				  (+ (mark-column point) minimum))
			     point)))

(define (region-blank? region)
  (not (skip-chars-forward " \t"
			   (region-start region)
			   (region-end region)
			   #f)))

(define (line-blank? mark)
  (not (skip-chars-forward " \t"
			   (line-start mark 0)
			   (line-end mark 0)
			   #f)))

(define (find-previous-blank-line mark)
  (let ((start (line-start mark -1)))
    (and start
	 (let loop ((mark start))
	   (cond ((line-blank? mark) mark)
		 ((group-start? mark) #f)
		 (else (loop (line-start mark -1))))))))

(define (find-next-blank-line mark)
  (let ((start (line-start mark 1)))
    (and start
	 (let loop ((mark start))
	   (cond ((line-blank? mark) mark)
		 ((group-start? mark) #f)
		 (else (loop (line-start mark 1))))))))

(define (find-previous-non-blank-line mark)
  (let ((start (line-start mark -1)))
    (and start
	 (let loop ((mark start))
	   (cond ((not (line-blank? mark)) mark)
		 ((group-start? mark) #f)
		 (else (loop (line-start mark -1))))))))

(define (find-next-non-blank-line mark)
  (let ((start (line-start mark 1)))
    (and start
	 (let loop ((mark start))
	   (cond ((not (line-blank? mark)) mark)
		 ((group-start? mark) #f)
		 (else (loop (line-start mark 1))))))))

;;;; Indentation

(define (maybe-change-indentation indentation #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (if (not (= indentation (mark-indentation point)))
	(change-indentation indentation point))))

(define (change-indentation indentation point)
  (change-column indentation (line-start point 0)))

(define (current-indentation #!optional point)
  (mark-indentation (if (default-object? point) (current-point) point)))

(define (mark-indentation mark)
  (mark-column (indentation-end mark)))

(define (indentation-end mark)
  (horizontal-space-end (line-start mark 0)))

(define (within-indentation? mark)
  (line-start? (horizontal-space-start mark)))

(define (maybe-change-column column #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (if (not (= column (mark-column point)))
	(change-column column point))))

(define (change-column column mark)
  (let ((mark (mark-left-inserting-copy mark)))
    (delete-horizontal-space mark)
    (insert-horizontal-space column mark)
    (mark-temporary! mark)))

;;;; Lines

(define forward-line)
(define backward-line)
(let ((%backward-line
       (lambda (mark n limit?)
	 (line-start mark
		     (if (line-start? mark) (- n) (- 1 n))
		     limit?))))
  (set! forward-line
	(lambda (mark n #!optional limit?)
	  (let ((limit? (and (not (default-object? limit?)) limit?)))
	    (cond ((positive? n) (line-start mark n limit?))
		  ((negative? n) (%backward-line mark (- n) limit?))
		  (else mark)))))
  (set! backward-line
	(lambda (mark n #!optional limit?)
	  (let ((limit? (and (not (default-object? limit?)) limit?)))
	    (cond ((positive? n) (%backward-line mark n limit?))
		  ((negative? n) (line-start mark (- n) limit?))
		  (else mark)))))
  unspecific)