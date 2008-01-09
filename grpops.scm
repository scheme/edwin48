;;; -*-Scheme-*-
;;;
;;; $Id: grpops.scm,v 1.34 2007/04/01 17:33:07 riastradh Exp $
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

;;;; Group Operations

;;; These high-performance ops deal directly with groups and indices
;;; for speed and the least consing.  Since indices are not in general
;;; valid across modifications to the group, they can only be used in
;;; limited ways.  To save an index across a modification, it must be
;;; consed into a permanent mark.


;;;; Extractions

(define (group-extract-string group start end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(string (make-string (fix:- end start))))
    (cond ((fix:<= end gap-start)
	   (%substring-move! text start end string 0))
	  ((fix:>= start gap-start)
	   (%substring-move! text
			     (fix:+ start (group-gap-length group))
			     (fix:+ end (group-gap-length group))
			     string
			     0))
	  (else
	   (%substring-move! text start gap-start string 0)
	   (%substring-move! text
			     (group-gap-end group)
			     (fix:+ end (group-gap-length group))
			     string
			     (fix:- gap-start start))))
    string))

(define (group-copy-substring! group start end string start*)
  (let ((text (group-text group))
	(gap-start (group-gap-start group)))
    (cond ((fix:<= end gap-start)
	   (%substring-move! text start end string start*))
	  ((fix:>= start gap-start)
	   (%substring-move! text
			     (fix:+ start (group-gap-length group))
			     (fix:+ end (group-gap-length group))
			     string
			     start*))
	  (else
	   (%substring-move! text start gap-start string start*)
	   (%substring-move! text
			     (group-gap-end group)
			     (fix:+ end (group-gap-length group))
			     string
			     (fix:+ start* (fix:- gap-start start)))))))

(define (group-left-char group index)
  (xstring-ref (group-text group)
	       (fix:- (group-index->position-integrable group index #f) 1)))

(define (group-right-char group index)
  (xstring-ref (group-text group)
	       (group-index->position-integrable group index #t)))

(define (group-extract-and-delete-string! group start end)
  (let ((string (group-extract-string group start end)))
    (group-delete! group start end)
    string))

;;;; Insertion

(define (group-insert-char! group index char)
  (group-insert-chars! group index char 1))

(define (group-insert-chars! group index char n)
  (if (fix:< n 0)
      (error:bad-range-argument n 'GROUP-INSERT-CHARS!))
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (prepare-gap-for-insert! group index n)
    (xsubstring-fill! (group-text group) index (fix:+ index n) char)
    (finish-group-insert! group index n)
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (group-insert-string! group index string)
  (group-insert-substring! group index string 0 (string-length string)))

(define (group-insert-substring! group index string start end)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((n (fix:- end start)))
      (prepare-gap-for-insert! group index n)
      (%substring-move! string start end (group-text group) index)
      (finish-group-insert! group index n))
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (prepare-gap-for-insert! group new-start n)
  (if (or (group-read-only? group)
	  (and (group-text-properties group)
	       (text-not-insertable? group new-start)))
      (barf-if-read-only))
  (if (not (group-modified? group)) (check-first-group-modification group))
  (cond ((fix:< (group-gap-length group) n)
	 (grow-group! group new-start n))
	((fix:< new-start (group-gap-start group))
	 (let ((new-end (fix:+ new-start (group-gap-length group))))
	   (%substring-move! (group-text group)
			     new-start
			     (group-gap-start group)
			     (group-text group)
			     new-end)
	   (set-group-gap-start! group new-start)
	   (set-group-gap-end! group new-end)))
	((fix:> new-start (group-gap-start group))
	 (let ((new-end (fix:+ new-start (group-gap-length group))))
	   (%substring-move! (group-text group)
			     (group-gap-end group)
			     new-end
			     (group-text group)
			     (group-gap-start group))
	   (set-group-gap-start! group new-start)
	   (set-group-gap-end! group new-end)))))

(define (finish-group-insert! group index n)
  (set-group-gap-start! group (fix:+ index n))
  (set-group-gap-length! group (fix:- (group-gap-length group) n))
  (if (group-start-changes-index group)
      (begin
	(if (fix:< index (group-start-changes-index group))
	    (set-group-start-changes-index! group index))
	(set-group-end-changes-index!
	 group
	 (if (fix:> index (group-end-changes-index group))
	     (fix:+ index n)
	     (fix:+ (group-end-changes-index group) n))))
      (begin
	(set-group-start-changes-index! group index)
	(set-group-end-changes-index! group (fix:+ index n))))
  (do ((marks (group-marks group) (weak-cdr marks)))
      ((null? marks))
    (if (and (weak-car marks)
	     (or (fix:> (mark-index (weak-car marks)) index)
		 (and (fix:= (mark-index (weak-car marks)) index)
		      (mark-left-inserting? (weak-car marks)))))
	(set-mark-index! (weak-car marks)
			 (fix:+ (mark-index (weak-car marks)) n))))
  (set-group-modified-tick! group (fix:+ (group-modified-tick group) 1))
  (undo-record-insertion! group index (fix:+ index n))
  ;; The MODIFIED? bit must be set *after* the undo recording.
  (set-group-modified?! group #t)
  (if (group-text-properties group)
      (update-intervals-for-insertion! group index n)))

;;;; Deletion

(define (group-delete-left-char! group index)
  (group-delete! group (fix:- index 1) index))

(define (group-delete-right-char! group index)
  (group-delete! group index (fix:+ index 1)))

(define (group-delete! group start end)
  (if (not (and (fix:>= end 0) (fix:<= end (group-length group))))
      (error:bad-range-argument end 'GROUP-DELETE!))
  (if (not (and (fix:>= start 0) (fix:<= start end)))
      (error:bad-range-argument start 'GROUP-DELETE!))
  (if (not (fix:= start end))
      (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	(let ((text (group-text group))
	      (gap-length (group-gap-length group)))
	  (if (or (group-read-only? group)
		  (and (group-text-properties group)
		       (text-not-deleteable? group start end)))
	      (barf-if-read-only))
	  (if (not (group-modified? group))
	      (check-first-group-modification group))
	  ;; Guarantee that the gap is between START and END.  This is
	  ;; best done before the undo recording.
	  (cond ((fix:< (group-gap-start group) start)
		 (%substring-move! text
				   (group-gap-end group)
				   (fix:+ start gap-length)
				   text
				   (group-gap-start group)))
		((fix:> (group-gap-start group) end)
		 (%substring-move! text
				   end
				   (group-gap-start group)
				   text
				   (fix:+ end gap-length))))
	  ;; The undo recording must occur *before* the deletion.
	  (undo-record-deletion! group start end)
	  (let ((gap-end (fix:+ end gap-length)))
	    (set-group-gap-start! group start)
	    (set-group-gap-end! group gap-end)
	    (set-group-gap-length! group (fix:- gap-end start))
	    (if (and (group-shrink-length group)
		     (fix:<= (fix:- (xstring-length text)
				    (fix:- gap-end start))
			     (group-shrink-length group)))
		(shrink-group! group))))
	(let ((n (fix:- end start)))
	  (if (group-start-changes-index group)
	      (begin
		(if (fix:< start (group-start-changes-index group))
		    (set-group-start-changes-index! group start))
		(set-group-end-changes-index!
		 group
		 (if (fix:>= end (group-end-changes-index group))
		     start
		     (fix:- (group-end-changes-index group) n))))
	      (begin
		(set-group-start-changes-index! group start)
		(set-group-end-changes-index! group start)))
	  (do ((marks (group-marks group) (weak-cdr marks)))
	      ((null? marks))
	    (cond ((or (not (weak-car marks))
		       (fix:<= (mark-index (weak-car marks)) start))
		   unspecific)
		  ((fix:<= (mark-index (weak-car marks)) end)
		   (set-mark-index! (weak-car marks) start))
		  (else
		   (set-mark-index!
		    (weak-car marks)
		    (fix:- (mark-index (weak-car marks)) n))))))
	(set-group-modified-tick! group (fix:+ (group-modified-tick group) 1))
	;; The MODIFIED? bit must be set *after* the undo recording.
	(set-group-modified?! group #t)
	(if (group-text-properties group)
	    (update-intervals-for-deletion! group start end))
	(set-interrupt-enables! interrupt-mask)
	unspecific)))

;;;; Replacement

(define (group-replace-char! group index char)
  (if (not (and (fix:>= index 0) (fix:< index (group-length group))))
      (error:bad-range-argument index 'GROUP-REPLACE-CHAR!))
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok))
	(end-index (fix:+ index 1)))
    (prepare-gap-for-replace! group index end-index)
    (xstring-set! (group-text group)
		  (group-index->position-integrable group index #t)
		  char)
    (finish-group-replace! group index end-index)
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (group-replace-string! group index string)
  (group-replace-substring! group index string 0 (string-length string)))

(define (group-replace-substring! group index string start end)
  (if (fix:< start end)
      (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok))
	    (end-index (fix:+ index (fix:- end start))))
	(if (not (and (fix:>= index 0)
		      (fix:<= end-index (group-length group))))
	    (error:bad-range-argument index 'GROUP-REPLACE-SUBSTRING!))
	(prepare-gap-for-replace! group index end-index)
	(%substring-move! string start end
			  (group-text group)
			  (group-index->position-integrable group index #t))
	(finish-group-replace! group index end-index)
	(set-interrupt-enables! interrupt-mask)
	unspecific)))

(define (prepare-gap-for-replace! group start end)
  (if (or (group-read-only? group)
	  (and (group-text-properties group)
	       (text-not-replaceable? group start end)))
      (barf-if-read-only))
  (if (not (group-modified? group))
      (check-first-group-modification group))
  (if (and (fix:< start (group-gap-start group))
	   (fix:< (group-gap-start group) end))
      (let ((new-end (fix:+ end (group-gap-length group))))
	(%substring-move! (group-text group)
			  (group-gap-end group)
			  new-end
			  (group-text group)
			  (group-gap-start group))
	(set-group-gap-start! group end)
	(set-group-gap-end! group new-end)))
  (undo-record-replacement! group start end))

(define (finish-group-replace! group start end)
  (if (group-start-changes-index group)
      (begin
	(if (fix:< start (group-start-changes-index group))
	    (set-group-start-changes-index! group start))
	(if (fix:> end (group-end-changes-index group))
	    (set-group-end-changes-index! group end)))
      (begin
	(set-group-start-changes-index! group start)
	(set-group-end-changes-index! group end)))
  (set-group-modified-tick! group (fix:+ (group-modified-tick group) 1))
  ;; The MODIFIED? bit must be set *after* the undo recording.
  (set-group-modified?! group #t)
  (if (group-text-properties group)
      (update-intervals-for-replacement! group start end)))

;;;; Resizing

(define (grow-group! group new-gap-start n)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(realloc-factor (group-reallocation-factor group)))
    (let ((text-length (xstring-length text))
	  (gap-delta (- new-gap-start gap-start)))
      (let ((n-chars (- text-length (group-gap-length group))))
	(let ((new-text-length
	       (let ((minimum-text-length (+ n-chars n)))
		 (let loop ((length (if (= text-length 0) 1 text-length)))
		   (let ((length (ceiling (* length realloc-factor))))
		     (if (< length minimum-text-length)
			 (loop length)
			 length))))))
	  (let ((new-text (make-string new-text-length))
		(new-gap-length (- new-text-length n-chars)))
	    (let ((new-gap-end (+ new-gap-start new-gap-length)))
	      (cond ((= gap-delta 0)
		     (%substring-move! text 0 gap-start new-text 0)
		     (%substring-move! text gap-end text-length
				       new-text new-gap-end))
		    ((< gap-delta 0)
		     (%substring-move! text 0 new-gap-start new-text 0)
		     (%substring-move! text new-gap-start gap-start
				       new-text new-gap-end)
		     (%substring-move! text gap-end text-length
				       new-text (- new-gap-end gap-delta)))
		    (else
		     (let ((ngsp (+ gap-end gap-delta)))
		       (%substring-move! text 0 gap-start new-text 0)
		       (%substring-move! text gap-end ngsp new-text gap-start)
		       (%substring-move! text ngsp text-length
					 new-text new-gap-end))))
	      (set-group-text! group new-text)
	      (set-group-gap-start! group new-gap-start)
	      (set-group-gap-end! group new-gap-end)
	      (set-group-gap-length! group new-gap-length))))))
    (memoize-shrink-length! group realloc-factor)))

(define (shrink-group! group)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-length (group-gap-length group))
	(realloc-factor (group-reallocation-factor group)))
    (let ((text-length (xstring-length text)))
      (let ((n-chars (- text-length gap-length)))
	(let ((new-text-length
	       (if (= n-chars 0)
		   0
		   (let loop ((length text-length))
		     (let ((length (floor (/ length realloc-factor))))
		       (let ((sl
			      (compute-shrink-length length realloc-factor)))
			 (if (< sl n-chars)
			     length
			     (loop length)))))))
	      (gap-end (group-gap-end group)))
	  (let ((new-text (make-string new-text-length))
		(delta (- text-length new-text-length)))
	    (let ((new-gap-end (- gap-end delta)))
	      (%substring-move! text 0 gap-start new-text 0)
	      (%substring-move! text gap-end text-length new-text new-gap-end)
	      (set-group-gap-end! group new-gap-end)
	      (set-group-gap-length! group (- gap-length delta)))
	    (set-group-text! group new-text)))))
    (memoize-shrink-length! group realloc-factor)))

(define (memoize-shrink-length! group realloc-factor)
  (set-group-shrink-length!
   group
   (compute-shrink-length (xstring-length (group-text group)) realloc-factor)))

(define (compute-shrink-length length realloc-factor)
  (floor (/ (floor (/ length realloc-factor)) realloc-factor)))

(define (group-reallocation-factor group)
  ;; We assume the result satisfies (LAMBDA (G) (AND (REAL? G) (> G 1)))
  (inexact->exact (ref-variable buffer-reallocation-factor group)))
