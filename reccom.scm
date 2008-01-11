#| -*-Scheme-*-

$Id: reccom.scm,v 1.19 2007/01/05 21:19:24 cph Exp $

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

;;;; Rectangle Commands


(define rectangle-ring (list 'RECTANGLE))

(define (delete-rectangle mark1 mark2 #!optional fill-flag move?) ;mark2 is always "point"
  (let ((fill-flag (and (not (default-object? fill-flag)) fill-flag))
	(move? (and (not (default-object? move?)) move?)))
    (let* ((mark-order (if (mark> mark1 mark2)
			   (cons mark2 mark1)
			   (cons mark1 mark2)))
	   (first (car mark-order))
	   (last (cdr mark-order))
	   (column-order (let ((c1 (mark-column first))
			       (c2 (mark-column last)))
			   (if (< c1 c2) (cons c1 c2) (cons c2 c1))))
	   (column1 (car column-order))
	   (column2 (cdr column-order))
	   (spacenum (- column2 column1))
	   (spacenum$ (make-string spacenum #\space)))
      (define (iter line-mark ring-list)
	(let ((perm-mark (if line-mark (mark-left-inserting line-mark) #f)))
	  (if (or (not perm-mark) (mark> perm-mark last))
	      ring-list
	      (let* ((mark-1
		      (mark-permanent! (move-to-column perm-mark column1)))
		     (mark-2
		      (mark-permanent! (move-to-column perm-mark column2)))
		     (line$ (extract-string mark-1 mark-2)))
		(if (not move?) (delete-string mark-1 mark-2))
		(if fill-flag
		    (let ((colend (mark-column (line-end mark-1 0))))
		      (if (< colend column1)
			  (set! mark-1 (make-space-to-column column1 mark-1)))
		      (insert-string spacenum$ mark-1)))
		(iter (line-start perm-mark 1) (append ring-list (list line$)))))))
      (iter first (list spacenum)))))

(define-command kill-rectangle
  "Delete rectangle with corners at point and mark; save as last killed one."
  ()
  (lambda ()
    (set-cdr! rectangle-ring (delete-rectangle (current-mark) (current-point)))))

(define-command delete-rectangle
  "Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends."
  ()
  (lambda ()
    (delete-rectangle (current-mark) (current-point))))

(define-command open-rectangle
  "Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle."
  ()
  (lambda ()
    (delete-rectangle (current-mark) (current-point) #t #t)))

(define-command clear-rectangle
  "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks."
  ()
  (lambda ()
    (delete-rectangle (current-mark) (current-point) #t)))

(define (make-space-to-column column mark)
  (let ((mark (mark-permanent! mark)))
    (change-column column mark)
    (line-end mark 0)))

(define (yank-rectangle rectangle point)
  (let ((goal (mark-column point)))
    (if (null? (cdr rectangle))
	(editor-error "No rectangle to yank.")
	(let ((columns (cadr rectangle)))
	  (define (iter line-mark before-line-mark insert$)
	    (if (not (null? insert$))
		(let* ((next$ (car insert$))
		       (sl (string-length next$))
		       (final$ (if (< sl columns) (string-append next$
								 (Make-string (- columns sl) #\space))
				   next$)) 
		       (end-of-line (if line-mark (mark-left-inserting line-mark)
					 (let () (insert-newline before-line-mark)
					      before-line-mark)))
		       (current-col (mark-column end-of-line)))
		  (insert-string final$
				 (if (< current-col goal)
				     (make-space-to-column goal end-of-line)
				     (move-to-column end-of-line goal)))
		  (iter (line-end end-of-line 1)
			end-of-line
			(cdr insert$)))))
	  (iter (line-end point 0) point (cddr rectangle))))))

(define-command yank-rectangle
  "Yank the last killed rectangle with upper left corner at point."
  ()
  (lambda ()
    (yank-rectangle rectangle-ring (current-point))))