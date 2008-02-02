#| -*-Scheme-*-

$Id: regops.scm,v 1.94 2008/01/30 20:02:05 cph Exp $

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

;;;; Region/Mark Operations


(define (region-insert! mark region)
  (let ((string (region->string region))
	(group (mark-group mark))
	(start (mark-index mark)))
    (let ((n (string-length string)))
      (group-insert-substring! group start string 0 n)
      (%make-region (make-temporary-mark group start #f)
		    (make-temporary-mark group (+ start n) #t)))))

(define (region-insert-string! mark string)
  (group-insert-substring! (mark-group mark) (mark-index mark)
			   string 0 (string-length string)))

(define (region-insert-substring! mark string start end)
  (group-insert-substring! (mark-group mark) (mark-index mark)
			   string start end))

(define (region-insert-newline! mark)
  (group-insert-char! (mark-group mark) (mark-index mark) #\newline))

(define (region-insert-char! mark char)
  (group-insert-char! (mark-group mark) (mark-index mark) char))

(define (region->string region)
  (group-extract-string (region-group region)
			(region-start-index region)
			(region-end-index region)))

(define (region-delete! region)
  (group-delete! (region-group region)
		 (region-start-index region)
		 (region-end-index region)))

(define (mark-left-char mark)
  (and (not (group-start? mark))
       (group-left-char (mark-group mark) (mark-index mark))))

(define (mark-right-char mark)
  (and (not (group-end? mark))
       (group-right-char (mark-group mark) (mark-index mark))))

(define (mark-delete-left-char! mark)
  (if (group-start? mark)
      (error "No left char:" mark))
  (group-delete-left-char! (mark-group mark) (mark-index mark)))

(define (mark-delete-right-char! mark)
  (if (group-end? mark)
      (error "No right char:" mark))
  (group-delete-right-char! (mark-group mark) (mark-index mark)))

(define (region-transform! region operation)
  (let ((start (region-start region)))
    (group-replace-string! (mark-group start)
			   (mark-index start)
			   (operation (region->string region)))))

;;;; Clipping

(define (group-narrow! group start end)
  (record-clipping! group start end)
  (%group-narrow! group start end))

(define (%group-narrow! group start end)
  (let ((start (make-permanent-mark group start #f))
	(end (make-permanent-mark group end #t)))
    (set-group-start-mark! group start)
    (set-group-end-mark! group end)
    (set-group-display-start! group start)
    (set-group-display-end! group end)))

(define (group-widen! group)
  (record-clipping! group 0 (group-length group))
  (%group-widen! group))

(define (%group-widen! group)
  (%group-narrow! group 0 (group-length group)))

(define (region-clip! region)
  (let ((group (region-group region))
	(start (region-start region))
	(end (region-end region)))
    (let ((point (group-point group)))
      (cond ((mark< point start) (set-group-point! group start))
	    ((mark> point end) (set-group-point! group end))))
    (let ((buffer (group-buffer group)))
      (if buffer
	  (for-each
	   (lambda (window)
	     (let ((point (window-point window)))
	       (cond ((mark< point start) (set-window-point! window start))
		     ((mark> point end) (set-window-point! window end)))))
	   (buffer-windows buffer))))
    (group-narrow! group (mark-index start) (mark-index end))))

(define (with-region-clipped! new-region thunk)
  (let ((group (region-group new-region))
	(old-region (unspecific)))
    (dynamic-wind (lambda ()
		    (set! old-region (group-region group))
		    (region-clip! new-region)
		    (set! new-region (unspecific))
		    unspecific)
		  thunk
		  (lambda ()
		    (region-clip! old-region)))))

(define (without-group-clipped! group thunk)
  (let ((old-region (unspecific)))
    (dynamic-wind (lambda ()
		    (set! old-region (group-region group))
		    (group-widen! group))
		  thunk
		  (lambda ()
		    (region-clip! old-region)))))

(define (group-clipped? group)
  (not (and (zero? (group-start-index group))
	    (= (group-end-index group) (group-length group)))))

(define (group-unclipped-region group)
  (make-region (make-mark group 0)
	       (make-mark group (group-length group))))