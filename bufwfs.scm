#| -*-Scheme-*-

$Id: bufwfs.scm,v 1.25 2007/01/05 21:19:23 cph Exp $

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

;;;; Buffer Windows: Fill and Scroll

(declare (usual-integrations))

(define (fill-top window start)
  (let ((group (%window-group window))
	(start-column 0)
	(char-image-strings (%window-char-image-strings window))
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window)))
    (let loop
	((outline (o3-outline start))
	 (index (o3-index start))
	 (y (o3-y start)))
      (if (fix:<= y 0)
	  (begin
	    (set-o3-outline! start outline)
	    (set-o3-index! start index)
	    (set-o3-y! start y))
	  (let* ((end-index (fix:- index 1))
		 (start-index (%window-line-start-index window end-index))
		 (end-column
		  (group-columns group start-index end-index
				 start-column tab-width
				 char-image-strings))
		 (y-size (column->y-size end-column x-size truncate-lines?))
		 (y (fix:- y y-size)))
	    (draw-region! window
			  group start-index end-index
			  start-column
			  y y-size)
	    (loop (make-outline window (fix:- end-index start-index) y-size
				false outline)
		  start-index
		  y))))))

(define (fill-middle window top-end bot-start)
  (let ((group (%window-group window))
	(start-column 0)
	(char-image-strings (%window-char-image-strings window))
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window))
	(bot-start-index (o3-index bot-start)))
    (let loop
	((outline (o3-outline top-end))
	 (index (o3-index top-end))
	 (y (o3-y top-end)))
      (let ((start-index (fix:+ index 1)))
	(if (fix:< start-index bot-start-index)
	    (let ((index&column
		   (group-line-columns group start-index bot-start-index
				       start-column tab-width
				       char-image-strings)))
	      (let ((end-index (car index&column))
		    (end-column (cdr index&column)))
		(let ((y-size
		       (column->y-size end-column x-size truncate-lines?)))
		  (draw-region! window
				group start-index end-index
				start-column
				y y-size)
		  (loop (make-outline window
				      (fix:- end-index start-index)
				      y-size
				      outline
				      false)
			end-index
			(fix:+ y y-size)))))
	    (begin
	      (if (not (fix:= start-index bot-start-index))
		  (error "Mismatched indexes:" start-index bot-start-index))
	      (if (not (fix:= y (o3-y bot-start)))
		  (error "Mismatched y coordinates:" y (o3-y bot-start)))
	      (set-outline-next! outline (o3-outline bot-start))
	      (set-outline-previous! (o3-outline bot-start) outline)))))))

(define (fill-bottom window end)
  (let ((group (%window-group window))
	(start-column 0)
	(char-image-strings (%window-char-image-strings window))
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window))
	(y-size (window-y-size window))
	(group-end (%window-group-end-index window)))
    (let loop
	((outline (o3-outline end))
	 (index (o3-index end))
	 (y (o3-y end)))
      (if (or (fix:>= index group-end) (fix:>= y y-size))
	  (begin
	    (set-o3-outline! end outline)
	    (set-o3-index! end index)
	    (set-o3-y! end y))
	  (let ((start-index (fix:+ index 1)))
	    (let ((index&column
		   (group-line-columns group start-index group-end
				       start-column tab-width
				       char-image-strings)))
	      (let ((end-index (car index&column))
		    (end-column (cdr index&column)))
		(let ((y-size
		       (column->y-size end-column x-size truncate-lines?)))
		  (draw-region! window
				group start-index end-index
				start-column
				y y-size)
		  (loop (make-outline window
				      (fix:- end-index start-index)
				      y-size
				      outline
				      false)
			end-index
			(fix:+ y y-size))))))))))

(define (generate-outlines window start end)
  (let ((group (%window-group window))
	(start-column 0)
	(char-image-strings (%window-char-image-strings window))
	(tab-width (%window-tab-width window))
	(truncate-lines? (%window-truncate-lines? window))
	(x-size (window-x-size window))
	(y-size (window-y-size window))
	(group-end (%window-group-end-index window)))
    (let loop ((outline false) (start-index (o3-index start)) (y (o3-y start)))
      (let ((index&column
	     (group-line-columns group start-index group-end
				 start-column tab-width char-image-strings)))
	(let ((end-index (car index&column))
	      (end-column (cdr index&column)))
	  (let ((line-y (column->y-size end-column x-size truncate-lines?)))
	    (draw-region! window
			  group start-index end-index
			  start-column
			  y line-y)
	    (let ((outline*
		   (make-outline window
				 (fix:- end-index start-index)
				 line-y
				 outline
				 false))
		  (y (fix:+ y line-y)))
	      (if (not outline)
		  (set-o3-outline! start outline*))
	      (if (or (fix:>= end-index group-end) (fix:>= y y-size))
		  (begin
		    (set-o3-outline! end outline*)
		    (set-o3-index! end end-index)
		    (set-o3-y! end y))
		  (loop outline* (fix:+ end-index 1) y)))))))))

(define (draw-region! window group start-index end-index start-column
		      y y-size)
  (clip-window-region-1
   (fix:- (%window-saved-yl window) y) (fix:- (%window-saved-yu window) y)
   y-size
   (lambda (yl yu)
     (let ((screen (%window-saved-screen window))
	   (xl (fix:+ (%window-saved-x-start window)
		      (%window-saved-xl window)))
	   (xu (fix:+ (%window-saved-x-start window)
		      (%window-saved-xu window)))
	   (y-start (fix:+ (%window-saved-y-start window) y))
	   (char-image-strings (%window-char-image-strings window))
	   (truncate-lines? (%window-truncate-lines? window))
	   (tab-width (%window-tab-width window))
	   (results substring-image-results))
       (let ((xm (fix:- xu 1))
	     (yl (fix:+ y-start yl)) (yu (fix:+ y-start yu)))
	 (let ((columns (fix:- xm xl)))
	   (let line-loop
	       ((index start-index)
		(column-offset (fix:- start-column xl))
		(partial 0)
		(y y-start))
	     (if (fix:< y yu)
		 (let loop
		     ((column-offset column-offset)
		      (xl* xl)
		      (index index))
		   (let ((end-index*
			  (or (next-specific-property-change group
							     index
							     end-index
							     'HIGHLIGHTED)
			      end-index))
			 ;; If line is clipped off top of window, draw it 
			 ;; anyway so that index and column calculations
			 ;; get done. Use first visible line for image
			 ;; output so as to avoid consing a dummy image
			 ;; buffer.
			 (line
			  (screen-get-output-line
			   screen
			   (if (fix:< y yl) yl y)
			   xl* xu
			   (get-text-property group index 'HIGHLIGHTED #f))))
		     (let ((fill-line
			    (lambda (index xl*)
			      (group-image! group index end-index*
					    line xl* xm
					    tab-width column-offset results
					    char-image-strings)
			      (cond ((fix:= (vector-ref results 0) end-index)
				     (let ((xl* (vector-ref results 1)))
				       (let ((line
					      (screen-get-output-line
					       screen
					       (if (fix:< y yl) yl y)
					       xl* xu false)))
					 (do ((x xl* (fix:+ x 1)))
					     ((fix:= x xu))
					   (string-set! line x #\space)))))
				    ((fix:= (vector-ref results 0) end-index*)
				     (loop (fix:+ column-offset
						  (fix:- (vector-ref results 1)
							 xl*))
					   (vector-ref results 1)
					   (vector-ref results 0)))
				    (truncate-lines?
				     (string-set! line xm #\$))
				    (else
				     (string-set! line xm #\\)
				     (line-loop (vector-ref results 0)
						(fix:+ column-offset columns)
						(vector-ref results 2)
						(fix:+ y 1)))))))
		       (if (fix:= partial 0)
			   (fill-line index xl*)
			   (begin
			     (partial-image! (group-right-char group index)
					     partial
					     line xl* xm
					     tab-width
					     char-image-strings)
			     (if (fix:> partial columns)
				 (begin
				   (string-set! line xm #\\)
				   (line-loop index
					      (fix:+ column-offset columns)
					      (fix:- partial columns)
					      (fix:+ y 1)))
				 (fill-line (fix:+ index 1)
					    (fix:+ xl* partial))))))))))))))))

(define (scroll-lines-up window start end new-start-y)
  (if (fix:>= new-start-y 0)
      (%scroll-lines-up window start end new-start-y)
      (let ((start-outline (o3-outline start))
	    (amount (fix:- (o3-y start) new-start-y)))
	(if (if (fix:> (o3-y end) (window-y-size window))
		(let ((outline (o3-outline end)))
		  (or (eq? start-outline outline)
		      (fix:<= (fix:- (o3-y end) (outline-y-size outline))
			      amount)))
		(fix:<= (o3-y end) amount))
	    (begin
	      (deallocate-outlines! window start-outline (o3-outline end))
	      (deallocate-o3! window start)
	      (deallocate-o3! window end)
	      false)
	    (begin
	      (if (fix:> (o3-y end) (window-y-size window))
		  (let ((outline (o3-outline end)))
		    (set-o3-outline! end (outline-previous outline))
		    (set-o3-index!
		     end
		     (fix:- (o3-index end)
			    (fix:+ (outline-index-length outline) 1)))
		    (set-o3-y! end (fix:- (o3-y end) (outline-y-size outline)))
		    (deallocate-outlines! window outline outline)))
	      (let loop
		  ((outline start-outline)
		   (index (o3-index start))
		   (new-start-y new-start-y))
		(let ((new-end-y (fix:+ new-start-y (outline-y-size outline))))
		  (cond ((fix:< new-end-y 0)
			 (loop (outline-next outline)
			       (fix:+ index
				      (fix:+ (outline-index-length outline) 1))
			       new-end-y))
			((fix:> new-end-y 0)
			 (set-o3-outline! start outline)
			 (set-o3-index! start index)
			 (set-o3-y! start (fix:+ new-start-y amount))
			 (if (not (eq? start-outline outline))
			     (deallocate-outlines! window
						   start-outline
						   (outline-previous outline)))
			 (%scroll-lines-up window start end new-start-y))
			(else
			 (set-o3-outline! start (outline-next outline))
			 (set-o3-index!
			  start
			  (fix:+ (fix:+ index (outline-index-length outline))
				 1))
			 (set-o3-y! start amount)
			 (deallocate-outlines! window start-outline outline)
			 (%scroll-lines-up window start end new-end-y))))))))))

(define (%scroll-lines-up window start end new-start-y)
  (let ((yl (o3-y start))
	(yu (o3-y end)))
    (let ((amount (fix:- yl new-start-y)))
      (if (and (fix:< yl (%window-saved-yu window))
	       (fix:< (%window-saved-yl window) yu)
	       (let ((yl (fix:max (%window-saved-yl window) new-start-y))
		     (yu (fix:min (%window-saved-yu window) yu)))
		 (and (fix:< amount (fix:- yu yl))
		      (screen-scroll-lines-up
		       (%window-saved-screen window)
		       (fix:+ (%window-saved-xl window)
			      (%window-saved-x-start window))
		       (fix:+ (%window-saved-xu window)
			      (%window-saved-x-start window))
		       (fix:+ yl (%window-saved-y-start window))
		       (fix:+ yu (%window-saved-y-start window))
		       amount))))
	  (begin
	    (set-o3-y! start new-start-y)
	    (set-o3-y! end (fix:- yu amount))
	    true)
	  (begin
	    (deallocate-outlines! window (o3-outline start) (o3-outline end))
	    (deallocate-o3! window start)
	    (deallocate-o3! window end)
	    false)))))

(define (scroll-lines-down window start end new-start-y)
  (let ((y-size (window-y-size window)))
    (if (or (fix:>= new-start-y y-size)
	    (and (fix:< (o3-y start) 0)
		 (eq? (o3-outline start) (o3-outline end))))
	(begin
	  (deallocate-outlines! window (o3-outline start) (o3-outline end))
	  (deallocate-o3! window start)
	  (deallocate-o3! window end)
	  false)
	(let ((new-start-y
	       (if (fix:< (o3-y start) 0)
		   (let ((outline (o3-outline start)))
		     (let ((y-size (outline-y-size outline)))
		       (set-o3-outline! start (outline-next outline))
		       (set-o3-index!
			start
			(fix:+ (o3-index start)
			       (fix:+ (outline-index-length outline) 1)))
		       (set-o3-y! start (fix:+ (o3-y start) y-size))
		       (deallocate-outlines! window outline outline)
		       (fix:+ new-start-y y-size)))
		   new-start-y)))
	  (let loop
	      ((outline (o3-outline start))
	       (start-index (o3-index start))
	       (start-y new-start-y))
	    (let ((end-y (fix:+ start-y (outline-y-size outline))))
	      (cond ((fix:>= end-y y-size)
		     (if (not (eq? outline (o3-outline end)))
			 (deallocate-outlines! window
					       (outline-next outline)
					       (o3-outline end)))
		     (set-o3-outline! end outline)
		     (set-o3-index! end
				    (fix:+ start-index
					   (outline-index-length outline)))
		     (set-o3-y! end
				(fix:- end-y
				       (fix:- new-start-y (o3-y start)))))
		    ((not (eq? outline (o3-outline end)))
		     (loop (outline-next outline)
			   (fix:+ (fix:+ start-index
					 (outline-index-length outline))
				  1)
			   end-y)))))
	  (%scroll-lines-down window start end new-start-y)))))

(define (%scroll-lines-down window start end new-start-y)
  (let ((yl (o3-y start))
	(yu (o3-y end)))
    (let ((amount (fix:- new-start-y yl)))
      (if (and (fix:< yl (%window-saved-yu window))
	       (fix:< (%window-saved-yl window) yu)
	       (let ((yl (fix:max (%window-saved-yl window) yl))
		     (yu
		      (fix:min (%window-saved-yu window) (fix:+ yu amount))))
		 (and (fix:< amount (fix:- yu yl))
		      (screen-scroll-lines-down
		       (%window-saved-screen window)
		       (fix:+ (%window-saved-xl window)
			      (%window-saved-x-start window))
		       (fix:+ (%window-saved-xu window)
			      (%window-saved-x-start window))
		       (fix:+ yl (%window-saved-y-start window))
		       (fix:+ yu (%window-saved-y-start window))
		       amount))))
	  (begin
	    (set-o3-y! start new-start-y)
	    (set-o3-y! end (fix:+ yu amount))
	    true)
	  (begin
	    (deallocate-outlines! window (o3-outline start) (o3-outline end))
	    (deallocate-o3! window start)
	    (deallocate-o3! window end)
	    false)))))