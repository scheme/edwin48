#| -*-Scheme-*-

$Id: simple.scm,v 1.59 2007/01/05 21:19:24 cph Exp $

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

;;;; Simple Editing Procedures


(define (insert-char char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-char! (mark-group point) (mark-index point) char)))

(define (insert-chars char n #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-chars! (mark-group point) (mark-index point) char n)))

(define (insert-newline #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-char! (mark-group point) (mark-index point) #\newline)))

(define (insert-newlines n #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-chars! (mark-group point) (mark-index point) #\newline n)))

(define (guarantee-newline #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (if (not (line-start? point))
	(insert-newline point))))

(define (guarantee-newlines n #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let loop ((n n) (mark point))
      (if (> n 0)
	  (if (and (line-start? mark) (not (group-start? mark)))
	      (loop (- n 1) (mark-1+ mark))
	      (insert-newlines n point))))))

(define (extract-left-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (and (not (group-start-index? group index))
	   (group-left-char group index)))))

(define (extract-right-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (and (not (group-end-index? group index))
	   (group-right-char group index)))))

(define (delete-left-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (if (group-start-index? group index)
	  (editor-error "Attempt to delete past start of buffer")
	  (group-delete-left-char! group index)))))

(define (delete-right-char #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point)))
      (if (group-end-index? group index)
	  (editor-error "Attempt to delete past end of buffer")
	  (group-delete-right-char! group index)))))

(define (insert object #!optional point)
  (insert-string (write-to-string object)
		 (if (default-object? point) (current-point) point)))

(define (insert-string string #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-string! (mark-group point) (mark-index point) string)))

(define (insert-substring string start end #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-insert-substring! (mark-group point) (mark-index point)
			     string start end)))

(define (insert-string-pad-left string n-columns #!optional char point)
  (insert-substring-pad-left
   string 0 (string-length string)
   n-columns
   (if (default-object? char) #\space char)
   (if (default-object? point) (current-point) point)))

(define (insert-substring-pad-left string start end n-columns
				   #!optional char point)
  (let ((char (if (default-object? char) #\space char))
	(point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group point))
	  (index (mark-index point))
	  (n (fix:- n-columns (fix:- end start))))
      (if (fix:> n 0)
	  (begin
	    (group-insert-chars! group index char n)
	    (group-insert-substring! group (fix:+ index n) string start end))
	  (group-insert-substring! group index string start end)))))

(define (insert-string-pad-right string n-columns #!optional char point)
  (insert-substring-pad-right
   string 0 (string-length string)
   n-columns
   (if (default-object? char) #\space char)
   (if (default-object? point) (current-point) point)))

(define (insert-substring-pad-right string start end n-columns
				    #!optional char point)
  (let ((char (if (default-object? char) #\space char))
	(point (if (default-object? point) (current-point) point))
	(length (fix:- end start)))
    (let ((group (mark-group point))
	  (index (mark-index point))
	  (n (fix:- n-columns length)))
      (if (fix:> n 0)
	  (begin
	    (group-insert-substring! group index string start end)
	    (group-insert-chars! group (fix:+ index length) char n))
	  (group-insert-substring! group index string start end)))))

(define (insert-region start end #!optional point)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (let ((point (if (default-object? point) (current-point) point)))
    (if (mark~ start point)
	(insert-string (extract-string start end) point)
	(let ((group (mark-group start))
	      (start (mark-index start))
	      (end (mark-index end)))
	  (let ((text (group-text group))
		(gap-start (group-gap-start group))
		(gap-end (group-gap-end group))
		(gap-length (group-gap-length group)))
	    (cond ((<= end gap-start)
		   (group-insert-substring! (mark-group point)
					    (mark-index point)
					    text start end))
		  ((<= gap-start start)
		   (group-insert-substring! (mark-group point)
					    (mark-index point)
					    text
					    (+ start gap-length)
					    (+ end gap-length)))
		  (else
		   (let ((point (mark-left-inserting-copy point)))
		     (group-insert-substring! (mark-group point)
					      (mark-index point)
					      text start gap-start)
		     (group-insert-substring! (mark-group point)
					      (mark-index point)
					      text gap-end
					      (+ end gap-length))
		     (mark-temporary! point)))))))))

(define (extract-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (< index1 index2)
	  (group-extract-string group index1 index2)
	  (group-extract-string group index2 index1)))))

(define (delete-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (< index1 index2)
	  (group-delete! group index1 index2)
	  (group-delete! group index2 index1)))))

(define (extract-and-delete-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (< index1 index2)
	  (group-extract-and-delete-string! group index1 index2)
	  (group-extract-and-delete-string! group index2 index1)))))

(define (mark-flash mark #!optional type)
  (cond (*executing-keyboard-macro?* unspecific)
	((not mark) (editor-beep))
	((window-mark-visible? (current-window) mark)
	 (with-current-point mark
	   (lambda ()
	     (sit-for 500))))
	(else
	 (temporary-message
	  "Matches "
	  (let ((start (line-start mark 0))
		(end (line-end mark 0)))
	    (case (and (not (default-object? type)) type)
	      ((RIGHT) (extract-string mark end))
	      ((LEFT) (extract-string start mark))
	      (else (extract-string start end))))))))

(define (sit-for interval)
  (let ((time-limit (+ (real-time-clock) interval)))
    (let loop ()
      (if (and (not (keyboard-peek-no-hang))
	       (< (real-time-clock) time-limit)
	       (update-screens! #f))
	  (loop)))))

(define sleep-for
  sleep-current-thread)

(define (reposition-window-top mark)
  (if (not (and mark (set-window-start-mark! (current-window) mark #f)))
      (editor-beep)))

(define (narrow-to-region mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (let ((group (mark-group mark))
	  (index1 (mark-index mark))
	  (index2 (mark-index point)))
      (if (not (eq? group (mark-group point)))
	  (error "Marks not related:" mark point))
      (if (<= index1 index2)
	  (group-narrow! group index1 index2)
	  (group-narrow! group index2 index1)))))

(define (widen #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (group-widen! (mark-group point))))

(define (region-put! start end key datum #!optional no-overwrite?)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (add-text-property (mark-group start)
		     (mark-index start)
		     (mark-index end)
		     key
		     datum
		     (if (default-object? no-overwrite?) #f no-overwrite?)))

(define (region-remove! start end key)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (remove-text-property (mark-group start)
			(mark-index start)
			(mark-index end)
			key))

(define (region-get mark key default)
  (get-text-property (mark-group mark)
		     (mark-index mark)
		     key
		     default))

(define (find-next-property-change start end)
  (let ((index
	 (next-property-change (mark-group start)
			       (mark-index start)
			       (mark-index end))))
    (and index
	 (make-mark (mark-group start) index))))

(define (find-previous-property-change start end)
  (let ((index
	 (previous-property-change (mark-group start)
				   (mark-index start)
				   (mark-index end))))
    (and index
	 (make-mark (mark-group start) index))))

(define (find-next-specific-property-change start end key)
  (let ((index
	 (next-specific-property-change (mark-group start)
					(mark-index start)
					(mark-index end)
					key)))
    (and index
	 (make-mark (mark-group start) index))))

(define (find-previous-specific-property-change start end key)
  (let ((index
	 (previous-specific-property-change (mark-group start)
					    (mark-index start)
					    (mark-index end)
					    key)))
    (and index
	 (make-mark (mark-group start) index))))

(define (specific-property-region mark key #!optional predicate)
  (let ((default (list 'DEFAULT))
	(predicate
	 (if (or (default-object? predicate) (not predicate))
	     (lambda (x y) (eq? x y))
	     predicate)))
    (let ((datum (region-get mark key default)))
      (and (not (eq? datum default))
	   (make-region
	    (let ((start (group-start mark)))
	      (let loop ((mark mark))
		(if (mark< start mark)
		    (if (let ((datum* (region-get (mark-1+ mark) key default)))
			  (and (not (eq? datum* default))
			       (predicate datum* datum)))
			(let ((m
			       (find-previous-specific-property-change
				start mark key)))
			  (if m
			      (loop m)
			      start))
			mark)
		    start)))
	    (let ((end (group-end mark)))
	      (let loop ((mark mark))
		(if (mark< mark end)
		    (if (let ((datum* (region-get (mark1+ mark) key default)))
			  (and (not (eq? datum* default))
			       (predicate datum* datum)))
			(let ((m
			       (find-next-specific-property-change
				mark end key)))
			  (if m
			      (loop m)
			      end))
			mark)
		    end))))))))