;;; -*-Scheme-*-
;;;
;;; $Id: bufwmc.scm,v 1.23 2007/01/05 21:19:23 cph Exp $
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

;;;; Buffer Windows: Mark <-> Coordinate Maps


(define (buffer-window/mark->x window mark)
  (buffer-window/index->x window (mark-index mark)))

(define (buffer-window/mark->y window mark)
  (buffer-window/index->y window (mark-index mark)))

(define (buffer-window/mark->coordinates window mark)
  (buffer-window/index->coordinates window (mark-index mark)))

(define (buffer-window/point-x window)
  (buffer-window/index->x window (%window-point-index window)))

(define (buffer-window/point-y window)
  (buffer-window/index->y window (%window-point-index window)))

(define (buffer-window/point-coordinates window)
  (buffer-window/index->coordinates window (%window-point-index window)))

(define (buffer-window/index->x window index)
  (let ((start (%window-line-start-index window index))
	(group (%window-group window))
	(char-image-strings (%window-char-image-strings window))
	(tab-width (%window-tab-width window)))
    (column->x (group-columns group start index 0 tab-width char-image-strings)
	       (window-x-size window)
	       (%window-truncate-lines? window)
	       (%window-line-end-index? window index))))

(define (buffer-window/index->y window index)
  (with-values (lambda () (start-point-for-index window index))
    (lambda (start-index start-y line-start-index)
      line-start-index
      (predict-y window start-index start-y index))))

(define (buffer-window/index->coordinates window index)
  (with-values (lambda () (start-point-for-index window index))
    (lambda (start-index start-y line-start-index)
      (let ((group (%window-group window))
	    (char-image-strings (%window-char-image-strings window))
	    (tab-width (%window-tab-width window)))
	(let ((xy
	       (column->coordinates
		(group-columns group line-start-index index 0 tab-width
			       char-image-strings)
		(window-x-size window)
		(%window-truncate-lines? window)
		(%window-line-end-index? window index))))
	  (cons (car xy)
		(fix:+ (cdr xy)
		       (predict-y window
				  start-index
				  start-y
				  line-start-index))))))))

(define (buffer-window/coordinates->mark window x y)
  (let ((index (buffer-window/coordinates->index window x y)))
    (and index
	 (make-mark (%window-group window) index))))

(define (buffer-window/coordinates->index window x y)
  (with-values (lambda () (start-point-for-y window y))
    (lambda (start-index start-y)
      (predict-index window start-index start-y x y))))

(define (buffer-window/mark-visible? window mark)
  ;; True iff cursor at this position would be on-screen.
  (let ((index (mark-index mark)))
    (and (fix:<= (%window-group-start-index window) index)
	 (fix:<= index (%window-group-end-index window))
	 (with-values (lambda () (start-point-for-index window index))
	   (lambda (start-index start-y line-start-index)
	     line-start-index
	     (predict-index-visible? window start-index start-y index))))))

(define (start-point-for-index window index)
  (if (outlines-valid? window)
      (let ((start-index (%window-current-start-index window))
	    (start-y (%window-current-start-y window)))
	(if (and (fix:<= start-index index)
		 (fix:<= index (%window-current-end-index window)))
	    (let loop
		((outline (%window-start-outline window))
		 (index* start-index)
		 (y start-y))
	      (let ((index**
		     (fix:+ index* (fix:+ (outline-index-length outline) 1))))
		(if (fix:< index index**)
		    (values index* y index*)
		    (loop (outline-next outline)
			  index**
			  (fix:+ y (outline-y-size outline))))))
	    (values start-index
		    start-y
		    (%window-line-start-index window index))))
      (begin
	(guarantee-start-mark! window)
	(values (%window-start-line-index window)
		(%window-start-line-y window)
		(%window-line-start-index window index)))))

(define (start-point-for-y window y)
  (if (outlines-valid? window)
      (let ((start-index (%window-current-start-index window))
	    (start-y (%window-current-start-y window)))
	(if (fix:< y start-y)
	    (values start-index start-y)
	    (let loop
		((outline (%window-start-outline window))
		 (index start-index)
		 (y* start-y))
	      (let ((y** (fix:+ y* (outline-y-size outline))))
		(cond ((fix:< y y**)
		       (values index y*))
		      ((not (outline-next outline))
		       (values start-index start-y))
		      (else
		       (loop (outline-next outline)
			     (fix:+ index
				    (fix:+ (outline-index-length outline) 1))
			     y**)))))))
      (begin
	(guarantee-start-mark! window)
	(values (%window-start-line-index window)
		(%window-start-line-y window)))))

(define (outlines-valid? window)
  (and (fix:= (group-modified-tick (%window-group window))
	      (%window-modified-tick window))
       (not (%window-start-clip-mark window))
       (not (%window-point-moved? window))
       (not (%window-force-redraw? window))
       (%window-start-line-mark window)
       (fix:= (%window-start-line-index window)
	      (%window-current-start-index window))))

(define (predict-y window start y index)
  ;; Assuming that the character at index START appears at coordinate
  ;; Y, return the coordinate for the character at INDEX.  START is
  ;; assumed to be a line start.
  (if (fix:= index start)
      y
      (let ((group (%window-group window))
	    (char-image-strings (%window-char-image-strings window))
	    (tab-width (%window-tab-width window))
	    (x-size (window-x-size window))
	    (truncate-lines? (%window-truncate-lines? window)))
	(if (fix:< index start)
	    (let ((group-start (%window-group-start-index window)))
	      (let loop ((start start) (y y))
		(let* ((end (fix:- start 1))
		       (start
			(or (%find-previous-newline group end group-start)
			    group-start))
		       (columns (group-columns group start end 0 tab-width
					       char-image-strings))
		       (y
			(fix:- y
			       (column->y-size columns
					       x-size
					       truncate-lines?))))
		  (if (fix:< index start)
		      (loop start y)
		      (fix:+ y
			     (column->y (group-columns group start index
						       0 tab-width
						       char-image-strings)
					x-size
					truncate-lines?
					(%window-line-end-index? window
								 index)))))))
	    (let ((group-end (%window-group-end-index window)))
	      (let loop ((start start) (y y))
		(let ((e&c
		       (group-line-columns group start group-end 0 tab-width
					   char-image-strings)))
		  (if (fix:> index (car e&c))
		      (loop (fix:+ (car e&c) 1)
			    (fix:+ y
				   (column->y-size (cdr e&c)
						   x-size
						   truncate-lines?)))
		      (fix:+ y
			     (column->y (group-columns group start index
						       0 tab-width
						       char-image-strings)
					x-size
					truncate-lines?
					(%window-line-end-index?
					 window
					 index)))))))))))

(define (predict-y-limited window start y index yl yu)
  ;; Like PREDICT-Y, except returns #F if the result is not in the
  ;; range specified by YL and YU.  Prevents long search to find INDEX
  ;; when it is far away from the window.
  (if (fix:= index start)
      (and (fix:<= yl y)
	   (fix:< y yu)
	   y)
      (let ((group (%window-group window))
	    (char-image-strings (%window-char-image-strings window))
	    (tab-width (%window-tab-width window))
	    (x-size (window-x-size window))
	    (truncate-lines? (%window-truncate-lines? window)))
	(if (fix:< index start)
	    (let ((group-start (%window-group-start-index window)))
	      (let loop ((start start) (y y))
		(and (fix:<= yl y)
		     (let* ((end (fix:- start 1))
			    (start
			     (or (%find-previous-newline group end group-start)
				 group-start))
			    (columns
			     (group-columns group start end 0 tab-width
					    char-image-strings))
			    (y
			     (fix:- y
				    (column->y-size columns
						    x-size
						    truncate-lines?))))
		       (if (fix:< index start)
			   (loop start y)
			   (let ((y
				  (fix:+
				   y
				   (column->y (group-columns group
							     start
							     index
							     0
							     tab-width
							     char-image-strings)
					      x-size
					      truncate-lines?
					      (%window-line-end-index?
					       window
					       index)))))
			     (and (fix:<= yl y)
				  (fix:< y yu)
				  y)))))))
	    (let ((group-end (%window-group-end-index window)))
	      (let loop ((start start) (y y))
		(and (fix:< y yu)
		     (let ((e&c
			    (group-line-columns group start group-end 0
						tab-width char-image-strings)))
		       (if (fix:> index (car e&c))
			   (loop (fix:+ (car e&c) 1)
				 (fix:+ y
					(column->y-size (cdr e&c)
							x-size
							truncate-lines?)))
			   (let ((y
				  (fix:+
				   y
				   (column->y (group-columns group
							     start
							     index
							     0
							     tab-width
							     char-image-strings)
					      x-size
					      truncate-lines?
					      (%window-line-end-index?
					       window
					       index)))))
			     (and (fix:<= yl y)
				  (fix:< y yu)
				  y)))))))))))

(define (predict-index-visible? window start y index)
  (and (fix:>= index start)
       (let ((x-size (window-x-size window))
	     (y-size (window-y-size window))
	     (group (%window-group window))
	     (char-image-strings (%window-char-image-strings window))
	     (tab-width (%window-tab-width window))
	     (truncate-lines? (%window-truncate-lines? window))
	     (group-end (%window-group-end-index window)))
	 (let loop ((start start) (y y))
	   (and (fix:< y y-size)
		(let ((e&c
		       (group-line-columns group start group-end 0 tab-width
					   char-image-strings)))
		  (if (fix:> index (car e&c))
		      (loop (fix:+ (car e&c) 1)
			    (fix:+ y
				   (column->y-size (cdr e&c)
						   x-size
						   truncate-lines?)))
		      (let ((y
			     (fix:+ y
				    (column->y (group-columns group
							      start
							      index
							      0
							      tab-width
							      char-image-strings)
					       x-size
					       truncate-lines?
					       (%window-line-end-index?
						window
						index)))))
			(and (fix:<= 0 y)
			     (fix:< y y-size))))))))))

(define (predict-index window start y-start x y)
  ;; Assumes that START is a line start.
  (let ((group (%window-group window))
	(char-image-strings (%window-char-image-strings window))
	(tab-width (%window-tab-width window))
	(x-size (window-x-size window))
	(truncate-lines? (%window-truncate-lines? window)))
    (if (fix:< y y-start)
	(let ((group-start (%window-group-start-index window)))
	  (let loop ((start start) (y-start y-start))
	    (and (fix:< group-start start)
		 (let* ((end (fix:- start 1))
			(start
			 (or (%find-previous-newline group end group-start)
			     group-start))
			(columns (group-columns group start end 0 tab-width
						char-image-strings))
			(y-start
			 (fix:- y-start
				(column->y-size columns
						x-size
						truncate-lines?))))
		   (if (fix:< y y-start)
		       (loop start y-start)
		       (vector-ref
			(group-column->index
			 group start end 0
			 (let ((column
				(coordinates->column x
						     (fix:- y y-start)
						     x-size)))
			   (if (fix:< column columns)
			       column
			       columns))
			 tab-width
			 char-image-strings)
			0))))))
	(let ((group-end (%window-group-end-index window)))
	  (let loop ((start start) (y-start y-start))
	    (let ((e&c (group-line-columns group start group-end 0 tab-width
					   char-image-strings)))
	      (let ((y-end
		      (fix:+ y-start
			     (column->y-size (cdr e&c)
					     x-size
					     truncate-lines?))))
		(if (fix:>= y y-end)
		    (and (fix:< (car e&c) group-end)
			 (loop (fix:+ (car e&c) 1) y-end))
		    (vector-ref (group-column->index
				 group start (car e&c) 0
				 (let ((column
					(coordinates->column x
							     (fix:- y y-start)
							     x-size)))
				   (if (fix:< column (cdr e&c))
				       column
				       (cdr e&c)))
				 tab-width
				 char-image-strings)
				0)))))))))

(define (compute-window-start window index y-index)
  ;; INDEX is an index into WINDOW's buffer, and Y-INDEX is the
  ;; desired y coordinate, in WINDOW's coordinate space, at which
  ;; INDEX is desired to appear.  Returns a vector of values:
  ;; 0 START-LINE, index at start of first line that is visible in the
  ;;   window.
  ;; 1 Y-START, coordinate at which START-LINE will appear.  Negative
  ;;   if START-LINE is less than START, otherwise zero.
  ;; 2 START, index of first visible char (in upper left corner).
  ;; 3 START-COLUMN, first visible column of window.  Positive if
  ;;   START is greater than START-LINE, otherwise zero.
  ;; 4 START-PARTIAL.  If START char is fully visible, this is zero.
  ;;   Otherwise this is positive and indicates the number of columns
  ;;   that *are* visible.
  ;; 5 #F means that it's not possible to place the INDEX char at
  ;;   Y-INDEX, but that the other values are a starting point that
  ;;   gets the INDEX char as close as possible to Y-INDEX.
  ;;   Otherwise, this is #T indicating that the starting point is
  ;;   satisfactory.
  (if (%window-truncate-lines? window)
      (compute-window-start-tl window index y-index)
      (compute-window-start-ntl window index y-index)))

(define (compute-window-start-tl window index y-index)
  (let ((group (%window-group window)))
    (let ((group-start (group-display-start-index group))
	  (group-end (group-display-end-index group)))
      (let ((start
	     (let ((index
		    (group-find-previous-char group group-start index
					      #\newline)))
	       (if index
		   (fix:+ index 1)
		   group-start))))
	(cond ((fix:= y-index 0)
	       (vector start y-index start 0 0 #t))
	      ((fix:< y-index 0)
	       (let loop ((start start) (y-start y-index))
		 (let ((nl
			(group-find-next-char group start group-end
					      #\newline)))
		   (if nl
		       (let ((start (fix:+ nl 1))
			     (y-start (fix:+ y-start 1)))
			 (if (fix:= y-start 0)
			     (vector start y-start start 0 0 #t)
			     (loop start y-start)))
		       (vector start 0 start 0 0 #f)))))
	      ((fix:= start group-start)
	       (vector start 0 start 0 0 #f))
	      (else
	       (let loop ((end (fix:- start 1)) (y-start y-index))
		 (let ((nl
			(group-find-previous-char group group-start end
						  #\newline))
		       (y-start (fix:- y-start 1)))
		   (cond ((fix:= y-start 0)
			  (let ((start (if nl (fix:+ nl 1) group-start)))
			    (vector start y-start start 0 0 #t)))
			 ((not nl)
			  (vector group-start 0 group-start 0 0 #f))
			 (else
			  (loop nl y-start)))))))))))

(define (compute-window-start-ntl window index y-index)
  (let ((group (%window-group window))
	(char-image-strings (%window-char-image-strings window))
	(tab-width (%window-tab-width window))
	(x-size (window-x-size window)))
    (let ((group-start (group-display-start-index group))
	  (group-end (group-display-end-index group))
	  (x-max (fix:- x-size 1)))
      (let ((start
	     (let ((index
		    (group-find-previous-char group group-start index
					      #\newline)))
	       (if index
		   (fix:+ index 1)
		   group-start))))
	(let ((y-start
	       (fix:- y-index
		      (column->y (group-columns group start index 0 tab-width
						char-image-strings)
				 x-size
				 #f
				 (%window-line-end-index? window index)))))
	  (cond ((fix:= y-start 0)
		 (vector start y-start start 0 0 #t))
		((fix:< y-start 0)
		 (let loop ((start start) (y-start y-start))
		   (let* ((column (fix:* (fix:- 0 y-start) x-max))
			  (icp
			   (group-column->index group start group-end
						0 column tab-width
						char-image-strings)))
		     (cond ((fix:= (vector-ref icp 1) column)
			    (vector start
				    y-start
				    (vector-ref icp 0)
				    (vector-ref icp 1)
				    (vector-ref icp 2)
				    #t))
			   ((fix:= (vector-ref icp 0) group-end)
			    (vector start 0 start 0 0 #f))
			   (else
			    (loop (fix:+ (vector-ref icp 0) 1)
				  (fix:+
				   y-start
				   (column->y-size (vector-ref icp 1)
						   x-size
						   #f))))))))
		((fix:= start group-start)
		 (vector start 0 start 0 0 #f))
		(else
		 (let loop ((end (fix:- start 1)) (y-start y-start))
		   (let ((nl
			  (group-find-previous-char group group-start end
						    #\newline)))
		     (let ((start (if nl (fix:+ nl 1) group-start)))
		       (let ((y-start
			      (fix:-
			       y-start
			       (column->y-size (group-columns group start end
							      0 tab-width
							      char-image-strings)
					       x-size
					       #f))))
			 (cond ((fix:= y-start 0)
				(vector start y-start start 0 0 #t))
			       ((fix:< y-start 0)
				(let ((icp
				       (group-column->index
					group start end
					0 (fix:* (fix:- 0 y-start) x-max)
					tab-width char-image-strings)))
				  (vector start
					  y-start
					  (vector-ref icp 0)
					  (vector-ref icp 1)
					  (vector-ref icp 2)
					  #t)))
			       ((not nl)
				(vector group-start 0 group-start 0 0 #f))
			       (else
				(loop nl y-start))))))))))))))

;;;; Column<->Coordinate Utilities

(define (column->y-size column-size x-size truncate-lines?)
  ;; Assume X-SIZE > 1.
  (cond ((or truncate-lines? (fix:< column-size x-size))
	 1)
	((fix:= (fix:remainder column-size (fix:- x-size 1)) 0)
	 (fix:quotient column-size (fix:- x-size 1)))
	(else
	 (fix:+ (fix:quotient column-size (fix:- x-size 1)) 1))))

(define (column->coordinates column x-size truncate-lines? line-end?)
  (let ((x-max (fix:- x-size 1)))
    (cond ((fix:< column x-max)
	   (cons column 0))
	  (truncate-lines?
	   (cons x-max 0))
	  ((and line-end? (fix:= (fix:remainder column x-max) 0))
	   (cons x-max (fix:- (fix:quotient column x-max) 1)))
	  (else
	   (cons (fix:remainder column x-max)
		 (fix:quotient column x-max))))))

(define (column->x column x-size truncate-lines? line-end?)
  (let ((x-max (fix:- x-size 1)))
    (cond ((fix:< column x-max)
	   column)
	  (truncate-lines?
	   x-max)
	  ((and line-end? (fix:= (fix:remainder column x-max) 0))
	   x-max)
	  (else
	   (fix:remainder column x-max)))))

(define (column->y column x-size truncate-lines? line-end?)
  (let ((x-max (fix:- x-size 1)))
    (cond ((or truncate-lines? (fix:< column x-max))
	   0)
	  ((and line-end? (fix:= (fix:remainder column x-max) 0))
	   (fix:- (fix:quotient column x-max) 1))
	  (else
	   (fix:quotient column x-max)))))

(define (coordinates->column x y x-size)
  (fix:+ x (fix:* y (fix:- x-size 1))))
