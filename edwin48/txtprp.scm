#| -*-Scheme-*-

$Id: txtprp.scm,v 1.29 2008/01/30 20:02:06 cph Exp $

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

;;;; Text Properties
;;;  An improved version of a mechanism from GNU Emacs 19


(define* (add-text-property group start end key datum (no-overwrite? #f))
  (validate-region-arguments group start end 'ADD-TEXT-PROPERTY)
  (validate-key-argument key 'ADD-TEXT-PROPERTY)
  (modify-text-properties group start end
    (if (not no-overwrite?)
	(lambda (properties)
	  (not (eq? (properties/lookup properties key no-datum) datum)))
	(lambda (properties)
	  (eq? (properties/lookup properties key no-datum) no-datum)))
    (lambda (interval)
      (properties/insert! (interval-properties interval) key datum))))

(define (remove-text-property group start end key)
  (validate-region-arguments group start end 'REMOVE-TEXT-PROPERTY)
  (validate-key-argument key 'REMOVE-TEXT-PROPERTY)
  (modify-text-properties group start end
    (lambda (properties)
      (not (eq? (properties/lookup properties key no-datum) no-datum)))
    (lambda (interval)
      (properties/delete! (interval-properties interval) key))))

(define (get-text-properties group index)
  (validate-point-arguments group index 'GET-TEXT-PROPERTIES)
  (if (and (group-text-properties group) (fix:< index (group-length group)))
      (properties->alist (interval-properties (find-interval group index)))
      '()))

(define (get-text-property group index key default)
  (validate-point-arguments group index 'GET-TEXT-PROPERTY)
  (validate-key-argument key 'GET-TEXT-PROPERTY)
  (if (and (group-text-properties group) (fix:< index (group-length group)))
      (interval-property (find-interval group index) key default)
      default))

(define (next-property-change group start end)
  (validate-region-arguments group start end 'NEXT-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (fix:< start end)
       (let ((end* (interval-end (find-interval group start))))
	 (and (fix:< end* end)
	      end*))))

(define (previous-property-change group start end)
  (validate-region-arguments group start end 'PREVIOUS-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (fix:< start end)
       (let ((start* (interval-start (find-interval group (fix:- end 1)))))
	 (and (fix:< start start*)
	      start*))))

(define (next-specific-property-change group start end key)
  (validate-region-arguments group start end 'NEXT-SPECIFIC-PROPERTY-CHANGE)
  (validate-key-argument key 'NEXT-SPECIFIC-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (fix:< start end)
       (let ((interval (find-interval group start)))
	 (let ((datum (interval-property interval key no-datum)))
	   (let loop ((interval interval))
	     (let ((end* (interval-end interval)))
	       (and (fix:< end* end)
		    (let ((next (next-interval interval)))
		      (if (datum=? datum (interval-property next key no-datum))
			  (loop next)
			  end*)))))))))

(define (previous-specific-property-change group start end key)
  (validate-region-arguments group start end 'PREV-SPECIFIC-PROPERTY-CHANGE)
  (validate-key-argument key 'PREV-SPECIFIC-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (fix:< start end)
       (let ((interval (find-interval group (fix:- end 1))))
	 (let ((datum (interval-property interval key no-datum)))
	   (let loop ((interval interval))
	     (let ((start* (interval-start interval)))
	       (and (fix:< start start*)
		    (let ((prev (previous-interval interval)))
		      (if (datum=? datum (interval-property prev key no-datum))
			  (loop prev)
			  start*)))))))))

(define (modify-text-properties group start end modify? modify!)
  (call-with-values
      (lambda () (intervals-to-modify group start end modify?))
    (lambda (start-interval end-interval)
      (if start-interval
	  (without-interrupts
	   (lambda ()
	     (prepare-to-modify-intervals group start-interval end-interval)
	     (let loop ((interval start-interval))
	       (modify! interval)
	       (if (not (eq? interval end-interval))
		   (loop (next-interval interval))))
	     (let ((end (interval-end end-interval)))
	       (let loop
		   ((interval
		     (or (previous-interval start-interval)
			 start-interval)))
		 (let ((next
			(let ((next (next-interval interval)))
			  (if (and next
				   (properties=? (interval-properties interval)
						 (interval-properties next)))
			      (begin
				(increment-interval-length
				 next
				 (interval-length interval))
				(delete-interval interval group))
			      next))))
		   (if (and next
			    (not (fix:= end (interval-start next))))
		       (loop next)))))))))))

(define (intervals-to-modify group start end modify?)
  (letrec
      ((find-start
	(lambda (interval)
	  (if (fix:<= end (interval-end interval))
	      (values #f #f)
	      (let ((interval (next-interval interval)))
		(if (modify? (interval-properties interval))
		    (find-end interval)
		    (find-start interval))))))
       (find-end
	(lambda (start-interval)
	  (let loop ((prev start-interval) (interval start-interval))
	    (let ((end* (interval-end interval)))
	      (if (fix:< end end*)
		  (if (modify? (interval-properties interval))
		      (let ((end-interval
			     (split-interval-left interval end group)))
			(values (if (eq? interval start-interval)
				    end-interval
				    start-interval)
				end-interval))
		      (values start-interval prev))
		  (let ((prev
			 (if (modify? (interval-properties interval))
			     interval
			     prev)))
		    (if (fix:= end end*)
			(values start-interval prev)
			(loop prev (next-interval interval))))))))))
    (if (fix:< start end)
	(let ((interval
	       (if (group-text-properties group)
		   (find-interval group start)
		   (make-initial-interval group))))
	  (if (modify? (interval-properties interval))
	      (find-end
	       (if (fix:= start (interval-start interval))
		   interval
		   (split-interval-right interval start group)))
	      (find-start interval)))
	(values #f #f))))

(define (prepare-to-modify-intervals group start-interval end-interval)
  (undo-record-intervals group start-interval end-interval)
  (let ((start (interval-start start-interval))
	(end (interval-end end-interval)))
    (if (group-start-changes-index group)
	(begin
	  (if (fix:< start (group-start-changes-index group))
	      (set-group-start-changes-index! group start))
	  (if (fix:> end (group-end-changes-index group))
	      (set-group-end-changes-index! group end)))
	(begin
	  (set-group-start-changes-index! group start)
	  (set-group-end-changes-index! group end))))
  (set-group-modified?! group #t)
  (set-group-modified-tick! group (fix:+ (group-modified-tick group) 1)))

(define (validate-region-arguments group start end procedure)
  (validate-group group procedure)
  (validate-group-index group start procedure)
  (validate-group-index group end procedure)
  (if (not (fix:<= start end))
      (error "Indexes incorrectly related:" start end procedure)))

(define (validate-point-arguments group index procedure)
  (validate-group group procedure)
  (validate-group-index group index procedure))

(define (validate-group-index group index procedure)
  (if (not (fix:fixnum? index))
      (error:wrong-type-argument index "fixnum" procedure))
  (if (not (and (fix:<= 0 index) (fix:<= index (group-length group))))
      (error:bad-range-argument index procedure)))

(define (validate-group group procedure)
  (if (not (group? group))
      (error:wrong-type-argument group "group" procedure)))

(define (validate-key-argument key procedure)
  (if (not (or (symbol? key) (variable? key)))
      (error:wrong-type-argument key "key" procedure)))

(define no-datum
  (list 'NO-DATUM))

;;;; READ-ONLY Property

;;; The READ-ONLY property is applied to a contiguous region of
;;; characters.  No insertions are allowed within that region, and no
;;; deletions may intersect that region.  However, insertions may
;;; occur at either end of the region.

;;; This behavior is implemented by using a unique datum for the
;;; READ-ONLY property of a given contiguous region.  The code for
;;; insertion checks the READ-ONLY properties to the left and right of
;;; the insertion point, and disallows insertion only when they are
;;; the same.  If two different READ-ONLY regions are placed
;;; immediately adjacent to one another, insertions may occur in
;;; between the regions, but not inside of them.

(define (subgroup-read-only group start end)
  (add-text-property group start end 'READ-ONLY (list 'READ-ONLY)))

(define (subgroup-writeable group start end)
  (remove-text-property group start end 'READ-ONLY))

(define (region-read-only region)
  (subgroup-read-only (region-group region)
		      (region-start-index region)
		      (region-end-index region)))

(define (region-writeable region)
  (subgroup-writeable (region-group region)
		      (region-start-index region)
		      (region-end-index region)))

(define (text-not-insertable? group start)
  ;; Assumes that (GROUP-TEXT-PROPERTIES GROUP) is not #F.
  (and (not (eq? 'FULLY (group-writeable? group)))
       (not (fix:= start 0))
       (not (fix:= start (group-length group)))
       (let ((interval (find-interval group start)))
	 (let ((datum (interval-property interval 'READ-ONLY #f)))
	   (and datum
		(if (fix:= start (interval-start interval))
		    (eq? datum
			 (interval-property (previous-interval interval)
					    'READ-ONLY #f))
		    (or (fix:< start (interval-end interval))
			(eq? datum
			     (interval-property (next-interval interval)
						'READ-ONLY #f)))))))))

(define (text-not-deleteable? group start end)
  ;; Assumes that (GROUP-TEXT-PROPERTIES GROUP) is not #F.
  (and (not (eq? 'FULLY (group-writeable? group)))
       (fix:< start end)
       (let loop ((interval (find-interval group start)))
	 (or (interval-property interval 'READ-ONLY #f)
	     (and (not (fix:<= end (interval-end interval)))
		  (let ((next (next-interval interval)))
		    (and next
			 (loop next))))))))

(define text-not-replaceable?
  text-not-deleteable?)

;;;; Miscellaneous Properties

(define (highlight-subgroup group start end highlight)
  (if highlight
      (add-text-property group start end 'HIGHLIGHTED highlight)
      (remove-text-property group start end 'HIGHLIGHTED)))

(define (highlight-region region highlight)
  (highlight-subgroup (region-group region)
		      (region-start-index region)
		      (region-end-index region)
		      highlight))

(define (highlight-region-excluding-indentation region highlight)
  (let ((start (region-start region))
	(end (region-end region)))
    (let loop ((start start))
      (let ((start (horizontal-space-end start))
	    (lend (line-end start 0)))
	(if (mark<= lend end)
	    (begin
	      (let ((end (horizontal-space-start lend)))
		(if (mark< start end)
		    (highlight-region (make-region start end) highlight)))
	      (if (not (group-end? lend))
		  (loop (mark1+ lend))))
	    (let ((end (horizontal-space-start end)))
	      (if (mark< start end)
		  (highlight-region (make-region start end) highlight))))))))

(define (local-comtabs mark)
  (get-text-property (mark-group mark) (mark-index mark) 'COMMAND-TABLE #f))

(define (set-subgroup-local-comtabs! group start end comtabs)
  (if comtabs
      (add-text-property group start end 'COMMAND-TABLE comtabs)
      (remove-text-property group start end 'COMMAND-TABLE)))

(define (set-region-local-comtabs! region comtabs)
  (set-subgroup-local-comtabs! (region-group region)
			       (region-start-index region)
			       (region-end-index region)
			       comtabs))

;;;; Buffer modification

(define (update-intervals-for-insertion! group start length)
  ;; Assumes that (GROUP-TEXT-PROPERTIES GROUP) is not #F.
  ;; Depends on FIND-INTERVAL returning the rightmost interval when
  ;; START is GROUP-LENGTH.
  (let ((interval (find-interval group start)))
    (increment-interval-length interval length)
    (if (not (properties/empty? (interval-properties interval)))
	(set-interval-properties!
	 (let ((interval
		(if (fix:= start (interval-start interval))
		    interval
		    (split-interval-right interval start group)))
	       (end (fix:+ start length)))
	   (if (fix:= end (interval-end interval))
	       interval
	       (split-interval-left interval end group)))
	 (make-empty-properties)))))

(define (update-intervals-for-deletion! group start end)
  ;; Assumes that (GROUP-TEXT-PROPERTIES GROUP) is not #F.
  ;; Assumes that (FIX:< START END).
  (letrec
      ((deletion-loop
	(lambda (interval length)
	  (let ((length* (interval-length interval)))
	    (cond ((fix:< length length*)
		   (decrement-interval-length interval length))
		  ((fix:= length length*)
		   (delete-interval interval group))
		  (else
		   (deletion-loop (delete-interval interval group)
				  (fix:- length length*))))))))
    (let ((interval (find-interval group start))
	  (length (fix:- end start)))
      (let ((start* (interval-start interval)))
	(if (fix:= start start*)
	    (deletion-loop interval length)
	    (let ((length* (interval-length interval)))
	      (if (fix:<= end (fix:+ start* length*))
		  (decrement-interval-length interval length)
		  (let ((delta (fix:- (fix:+ start* length*) start)))
		    (decrement-interval-length interval delta)
		    (deletion-loop (next-interval interval)
				   (fix:- length delta))))))))))

(define (update-intervals-for-replacement! group start end)
  ;; Assumes that (GROUP-TEXT-PROPERTIES GROUP) is not #F.
  ;; Assumes that (FIX:< START END).
  group start end
  ;; Not sure what to do about this right now.  For current uses of
  ;; replacement, it's reasonable to leave the properties alone.
  unspecific)

;;;; Undo

(define (group-extract-properties group start end)
  ;; Assumes that (GROUP-TEXT-PROPERTIES GROUP) is not #F.
  ;; Assumes that (FIX:< START END).
  (let loop ((interval (find-interval group start)) (start start))
    (let ((end* (interval-end interval)))
      (if (fix:<= end end*)
	  (cons (vector start
			end
			(properties->alist (interval-properties interval)))
		'())
	  (cons (vector start
			end*
			(properties->alist (interval-properties interval)))
		(let ((next (next-interval interval)))
		  (loop next
			(interval-start next))))))))

(define (undo-record-intervals group start-interval end-interval)
  (if (not (eq? #t (group-undo-data group)))
      (undo-record-property-changes!
       group
       (let loop ((interval start-interval))
	 (cons (vector (interval-start interval)
		       (interval-end interval)
		       (properties->alist (interval-properties interval)))
	       (if (eq? interval end-interval)
		   '()
		   (loop (next-interval interval))))))))

(define (group-reinsert-properties! group plist)
  (do ((plist plist (cdr plist)))
      ((null? plist))
    (let ((properties* (alist->properties (vector-ref (car plist) 2))))
      (modify-text-properties group
			      (vector-ref (car plist) 0)
			      (vector-ref (car plist) 1)
	(lambda (properties)
	  (not (properties=? properties properties*)))
	(lambda (interval)
	  (set-interval-properties! interval properties*))))))

(define (reinsert-properties-size plist)
  (let loop ((plist plist) (size 0))
    (if (null? plist)
	size
	(loop (cdr plist)
	      (fix:+ (fix:+ (vector-length (car plist)) 1)
		     (fix:* (length (vector-ref (car plist) 2)) 4))))))

;;;; Properties

(define properties->alist rb-tree->alist)
(define properties/copy rb-tree/copy)
(define properties/delete! rb-tree/delete!)
(define properties/empty? rb-tree/empty?)
(define properties/insert! rb-tree/insert!)
(define properties/lookup rb-tree/lookup)

(define (make-empty-properties)
  (make-rb-tree key=? key<?))

(define (alist->properties alist)
  (alist->rb-tree alist key=? key<?))

(define (properties=? x y)
  (rb-tree/equal? x y datum=?))

(define key=? eq?)
(define datum=? eqv?)

(define (key<? k1 k2)
  (let ((lose
	 (lambda (k)
	   (error:wrong-type-argument k "key" 'KEY<?))))
    (cond ((symbol? k1)
	   (cond ((symbol? k2) (symbol<? k1 k2))
		 ((variable? k2) #t)
		 (else (lose k2))))
	  ((variable? k1)
	   (cond ((symbol? k2) #f)
		 ((variable? k2)
		  (symbol<? (variable-name k1) (variable-name k2)))
		 (else (lose k2))))
	  (else (lose k1)))))

;;;; Intervals

;;; These are balanced using the red-black tree balancing algorithm.
;;; See Cormen, Leiserson, and Rivest, "Introduction to Algorithms",
;;; Chapter 14, "Red-Black Trees".

(define-record-type* interval
  (make-interval)
  (up
   left
   right
   color
   total-length
   start
   properties))

(define (make-initial-interval group)
  (let ((interval
	 (make-interval #f
			#f
			#f
			'BLACK
			(group-length group)
			0
			(make-empty-properties))))
    (set-group-text-properties! group interval)
    interval))

(define (interval-length interval)
  (if (interval-left interval)
      (if (interval-right interval)
	  (fix:- (interval-total-length interval)
		 (fix:+ (interval-total-length (interval-left interval))
			(interval-total-length (interval-right interval))))
	  (fix:- (interval-total-length interval)
		 (interval-total-length (interval-left interval))))
      (if (interval-right interval)
	  (fix:- (interval-total-length interval)
		 (interval-total-length (interval-right interval)))
	  (interval-total-length interval))))

(define (interval-end interval)
  (fix:+ (interval-start interval)
	 (interval-length interval)))

(define (increment-interval-length interval length)
  (do ((interval interval (interval-up interval)))
      ((not interval))
    (set-interval-total-length! interval
				(fix:+ (interval-total-length interval)
				       length))))

(define (decrement-interval-length interval length)
  (do ((interval interval (interval-up interval)))
      ((not interval))
    (set-interval-total-length! interval
				(fix:- (interval-total-length interval)
				       length))))

(define (interval-property interval key default)
  (properties/lookup (interval-properties interval) key default))

;;;; Interval Tree Search

(define (find-interval group index)
  ;; Find the interval in GROUP that contains INDEX.  Assumes that
  ;; GROUP has non-empty GROUP-TEXT-PROPERTIES and that INDEX is at
  ;; most GROUP-LENGTH.  The interval returned has a valid
  ;; INTERVAL-START, and INDEX is guaranteed to be between
  ;; INTERVAL-START (inclusive) and INTERVAL-END (exclusive).
  ;; Exception: if INDEX is GROUP-LENGTH, the interval returned is the
  ;; rightmost interval, and INDEX is its INTERVAL-END.
  (let loop
      ((relative-index index)
       (interval (group-text-properties group)))
    (if (and (interval-left interval)
	     (fix:< relative-index
		    (interval-total-length (interval-left interval))))
	(loop relative-index (interval-left interval))
	(if (and (interval-right interval)
		 (fix:>= relative-index
			 (fix:- (interval-total-length interval)
				(interval-total-length
				 (interval-right interval)))))
	    (loop (fix:- relative-index
			 (fix:- (interval-total-length interval)
				(interval-total-length
				 (interval-right interval))))
		  (interval-right interval))
	    (begin
	      (set-interval-start!
	       interval
	       (if (interval-left interval)
		   (fix:+ (fix:- index relative-index)
			  (interval-total-length (interval-left interval)))
		   (fix:- index relative-index)))
	      interval)))))

(define (next-interval interval)
  (let ((finish
	 (lambda (interval*)
	   (set-interval-start! interval* (interval-end interval))
	   interval*)))
    (if (interval-right interval)
	(let loop ((interval (interval-right interval)))
	  (if (interval-left interval)
	      (loop (interval-left interval))
	      (finish interval)))
	(let loop ((interval interval))
	  (let ((up (interval-up interval)))
	    (and up
		 (if (eq? interval (interval-left up))
		     (finish up)
		     (loop up))))))))

(define (previous-interval interval)
  (let ((finish
	 (lambda (interval*)
	   (set-interval-start! interval*
				(fix:- (interval-start interval)
				       (interval-length interval*)))
	   interval*)))
    (if (interval-left interval)
	(let loop ((interval (interval-left interval)))
	  (if (interval-right interval)
	      (loop (interval-right interval))
	      (finish interval)))
	(let loop ((interval interval))
	  (let ((up (interval-up interval)))
	    (and up
		 (if (eq? interval (interval-right up))
		     (finish up)
		     (loop up))))))))

;;;; Interval Tree Modification

(define (split-interval-right interval index group)
  (split-interval-left interval index group)
  interval)

(define (split-interval-left interval index group)
  (let ((delta (fix:- index (interval-start interval)))
	(start (interval-start interval)))
    (set-interval-start! interval index)
    (let ((new
	   (lambda (parent d)
	     (let ((interval*
		    (make-interval parent #f #f 'RED delta start
				   (properties/copy
				    (interval-properties interval)))))
	       (set-link+! parent d interval*)
	       (insert-fixup! group interval*)
	       interval*))))
      (if (not (interval-left interval))
	  (new interval 'LEFT)
	  (let loop ((parent (interval-left interval)))
	    (set-interval-total-length! parent
					(fix:+ (interval-total-length parent)
					       delta))
	    (if (not (interval-right parent))
		(new parent 'RIGHT)
		(loop (interval-right parent))))))))

(define (insert-fixup! group x)
  ;; Assumptions: X is red, and the only possible violation of the
  ;; tree properties is that (INTERVAL-UP X) is also red.
  (let loop ((x x))
    (let ((u (interval-up x)))
      (if (and u (eq? 'RED (interval-color u)))
	  (let ((d (b->d (eq? u (interval-left (interval-up u))))))
	    (let ((y (get-link- (interval-up u) d)))
	      (if (and y (eq? 'RED (interval-color y)))
		  ;; case 1
		  (begin
		    (set-interval-color! u 'BLACK)
		    (set-interval-color! y 'BLACK)
		    (set-interval-color! (interval-up u) 'RED)
		    (loop (interval-up u)))
		  (let ((x
			 (if (eq? x (get-link- u d))
			     ;; case 2
			     (begin
			       (rotate+! group u d)
			       u)
			     x)))
		    ;; case 3
		    (let ((u (interval-up x)))
		      (set-interval-color! u 'BLACK)
		      (set-interval-color! (interval-up u) 'RED)
		      (rotate-! group (interval-up u) d)))))))))
  (set-interval-color! (group-text-properties group) 'BLACK))

(define (delete-interval interval group)
  ;; Returns the next interval after INTERVAL.  This might be EQ? to
  ;; INTERVAL because the algorithm might swap INTERVAL with its next
  ;; node.
  (decrement-interval-length interval (interval-length interval))
  (let ((finish
	 (lambda (z n)
	   (let ((x (or (interval-left z) (interval-right z)))
		 (u (interval-up z)))
	     (if x (set-interval-up! x u))
	     (cond ((not u) (set-group-text-properties! group x))
		   ((eq? z (interval-left u)) (set-interval-left! u x))
		   (else (set-interval-right! u x)))
	     (if (eq? 'BLACK (interval-color z))
		 (delete-fixup! group x u)))
	   n)))
    (let ((y (next-interval interval)))
      (if (and (interval-left interval)
	       (interval-right interval))
	  (begin
	    (let ((length (interval-length y)))
	      (do ((y y (interval-up y)))
		  ((eq? y interval))
		(set-interval-total-length! y
					    (fix:- (interval-total-length y)
						   length))))
	    (set-interval-start! interval (interval-start y))
	    (set-interval-properties! interval (interval-properties y))
	    (finish y interval))
	  (finish interval y)))))

(define (delete-fixup! group x u)
  (let loop ((x x) (u u))
    (if (or (not u)
	    (and x (eq? 'RED (interval-color x))))
	(if x (set-interval-color! x 'BLACK))
	(let ((d (b->d (eq? x (interval-left u)))))
	  (let ((w
		 (let ((w (get-link- u d)))
		   (if (eq? 'RED (interval-color w))
		       ;; case 1
		       (begin
			 (set-interval-color! w 'BLACK)
			 (set-interval-color! u 'RED)
			 (rotate+! group u d)
			 (get-link- u d))
		       w)))
		(case-4
		 (lambda (w)
		   (set-interval-color! w (interval-color u))
		   (set-interval-color! u 'BLACK)
		   (set-interval-color! (get-link- w d) 'BLACK)
		   (rotate+! group u d)
		   (set-interval-color! (group-text-properties group)
					'BLACK))))
	    (if (let ((n- (get-link- w d)))
		  (and n-
		       (eq? 'RED (interval-color n-))))
		(case-4 w)
		(let ((n+ (get-link+ w d)))
		  (if (or (not n+)
			  (eq? 'BLACK (interval-color (get-link+ w d))))
		      ;; case 2
		      (begin
			(set-interval-color! w 'RED)
			(loop u (interval-up u)))
		      ;; case 3
		      (begin
			(set-interval-color! n+ 'BLACK)
			(set-interval-color! w 'RED)
			(rotate-! group w d)
			(case-4 (get-link- u d)))))))))))

;;; The algorithms are left/right symmetric, so abstract "directions"
;;; permit code to be used for either symmetry:

(define (b->d left?)
  (if left? 'LEFT 'RIGHT))

(define (-d d)
  (if (eq? 'LEFT d) 'RIGHT 'LEFT))

(define (get-link+ p d)
  (if (eq? 'LEFT d)
      (interval-left p)
      (interval-right p)))

(define (set-link+! p d l)
  (if (eq? 'LEFT d)
      (set-interval-left! p l)
      (set-interval-right! p l)))

(define (get-link- p d)
  (if (eq? 'RIGHT d)
      (interval-left p)
      (interval-right p)))

(define (set-link-! p d l)
  (if (eq? 'RIGHT d)
      (set-interval-left! p l)
      (set-interval-right! p l)))

(define (rotate+! group x d)
  ;; Assumes (NOT (NOT (GET-LINK- X D))).
  (let ((y (get-link- x d)))
    (let ((beta (get-link+ y d)))
      (set-link-! x d beta)
      (if beta (set-interval-up! beta x))
      (let ((u (interval-up x)))
	(set-interval-up! y u)
	(cond ((not u)
	       (set-group-text-properties! group y))
	      ((eq? x (get-link+ u d))
	       (set-link+! u d y))
	      (else
	       (set-link-! u d y))))
      (set-link+! y d x)
      (set-interval-up! x y)
      (let ((tlx (interval-total-length x)))
	(set-interval-total-length!
	 x
	 (fix:+ (fix:- tlx (interval-total-length y))
		(if beta (interval-total-length beta) 0)))
	(set-interval-total-length! y tlx)))))

(define (rotate-! group x d)
  (rotate+! group x (-d d)))