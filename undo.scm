#| -*-Scheme-*-

$Id: undo.scm,v 1.68 2007/01/05 21:19:24 cph Exp $

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

;;;; Undo, translated from the GNU Emacs implementation in C/Emacs-Lisp.

(declare (usual-integrations))

(define (enable-group-undo! group)
  (set-group-undo-data! group '()))

(define (disable-group-undo! group)
  (set-group-undo-data! group #t))

(define (with-group-undo-disabled group thunk)
  (let ((outside-data)
	(inside-data #t))
    (dynamic-wind (lambda ()
		    (set! outside-data (group-undo-data group))
		    (set-group-undo-data! group inside-data)
		    (set! inside-data)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! inside-data (group-undo-data group))
		    (set-group-undo-data! group outside-data)
		    (set! outside-data)
		    unspecific))))

(define (undo-done! point)
  ;; Called to say that POINT's group should have no undo data,
  ;; usually because it has just been filled from a file.
  (let ((group (mark-group point)))
    (if (undo-enabled? group)
	(set-group-undo-data! group '()))))

(define (undo-boundary! point)
  ;; Called to say that M-x undo should consider this the boundary of
  ;; a single undoable sequence of changes.
  (group-undo-boundary! (mark-group point)))

(define (undo-leave-window! window)
  ;; Called to say that WINDOW is being deselected, and that therefore
  ;; this is a good point at which to mark an undo boundary.
  (group-undo-boundary! (buffer-group (window-buffer window))))

(define (group-undo-boundary! group)
  (if (not (let ((items (group-undo-data group)))
	     (or (eq? #t items)
		 ;; Don't allow a boundary to be inserted as the last
		 ;; element of the list.
		 (not (pair? items))
		 ;; Don't allow two boundaries to be adjacent.
		 (eq? #f (car items)))))
      (record-item! group #f)))

(define (undo-enabled? group)
  (not (eq? #t (group-undo-data group))))

(define (record-item! group item)
  (set-group-undo-data! group (cons item (group-undo-data group))))

;;;; Recording Hooks

;;; These recording hooks must be called before GROUP-MODIFIED? is
;;; updated, so that they can read its old value.  In addition, the
;;; deletion recording hook must be called before the deletion is
;;; performed, so that it can extract the characters being deleted.

(define (undo-record-insertion! group start end)
  (if (undo-enabled? group)
      (let ((data (group-undo-data group)))
	;; Optimize for two successive insertions.
	(if (and (group-modified? group)
		 (pair? data)
		 (pair? (car data))
		 (fix:fixnum? (caar data))
		 (fix:fixnum? (cdar data))
		 (fix:= (cdar data) start))
	    (set-cdr! (car data) end)
	    (begin
	      (record-first-change! group)
	      (record-item! group (cons start end)))))))

(define (undo-record-deletion! group start end)
  (if (undo-enabled? group)
      (begin
	(record-first-change! group)
	(if (group-text-properties group)
	    (record-properties! group
				(group-extract-properties group start end)))
	(record-item! group
		      (let ((point (mark-index (group-point group))))
			(cons (group-extract-string group start end)
			      ;; Optimize undo storage when point is
			      ;; at edge of deletion.
			      (cond ((fix:= point start)
				     start)
				    ((and (fix:= point end)
					  (fix:> start 0))
				     (fix:- 0 start))
				    (else
				     (record-point! group)
				     start))))))))

(define (undo-record-replacement! group start end)
  (if (undo-enabled? group)
      (begin
	(record-first-change! group)
	(record-point! group)
	(record-item! group
		      (cons* 'REPLACEMENT
			     (group-extract-string group start end)
			     start)))))

(define (undo-record-property-changes! group properties)
  (if (undo-enabled? group)
      (begin
	(record-first-change! group)
	(record-properties! group properties))))

(define (record-first-change! group)
  (let ((buffer (group-buffer group)))
    (if (and buffer (not (group-modified? group)))
	(record-item! group (cons #t (buffer-modification-time buffer))))))

(define (record-point! group)
  (record-item! group (mark-index (group-point group))))

(define (record-properties! group properties)
  (record-item! group (cons 'REINSERT-PROPERTIES properties)))

;;;; Truncation

(define-variable undo-limit
  "Keep no more undo information once it exceeds this size.
This limit is applied when garbage collection happens.
The size is counted as the number of bytes occupied,
which includes both the saved text and other data."
  20000
  exact-nonnegative-integer?)

(define-variable undo-strong-limit
  "Don't keep more than this much size of undo information.
A command that pushes past this size is itself forgotten.
This limit is applied when garbage collection happens.
The size is counted as the number of bytes occupied,
which includes both the saved text and other data."
  30000
  exact-nonnegative-integer?)

(define (truncate-buffer-undo-lists!)
  ;; This procedure must be careful about accessing editor data
  ;; structures because it is a GC daemon and can be run at times when
  ;; the editor does not exist or is not running.  It would actually
  ;; prefer to be run *before* the GC, but that's not possible now.
  (if edwin-editor
      (let ((bytes/word (vector-ref (gc-space-status) 0)))
	(let ((words->bytes
	       (lambda (words)
		 (round (/ words bytes/word)))))
	(do ((buffers (bufferset-buffer-list (editor-bufferset edwin-editor))
		      (cdr buffers)))
	    ((not (pair? buffers)))
	  (let ((buffer (car buffers)))
	    (truncate-undo-data!
	     (group-undo-data (buffer-group buffer))
	     (words->bytes (ref-variable undo-limit buffer))
	     (words->bytes (ref-variable undo-strong-limit buffer)))))))))

(add-gc-daemon!/no-restore truncate-buffer-undo-lists!)
(add-event-receiver! event:after-restore truncate-buffer-undo-lists!)

(define (truncate-undo-data! items min-size max-size)
  (if (pair? items)
      (letrec
	  ((loop
	    (lambda (items prev size boundary)
	      (if (and boundary (fix:> size max-size))
		  ;; If we've exceeded MAX-SIZE, truncate at the
		  ;; previous boundary.
		  (set-cdr! boundary '())
		  (if (pair? items)
		      (if (eq? #f (car items))
			  ;; If this is the first boundary, continue
			  ;; regardless of size, otherwise continue
			  ;; only if we haven't yet reached MIN-SIZE.
			  (if (and boundary (fix:> size min-size))
			      (set-cdr! prev '())
			      (continue items size prev))
			  (continue items size boundary))))))
	   (continue
	    (lambda (items size boundary)
	      (loop (cdr items)
		    items
		    (fix:+ size (undo-item-size (car items)))
		    boundary))))
	(if (eq? #f (car items))
	    ;; If list starts with a boundary, skip over it.  We want
	    ;; to include the first undo operation in the result.
	    (continue items 0 #f)
	    (loop items #f 0 #f)))))

(define (undo-item-size item)
  (if (pair? item)
      (fix:+ 4
	     (let ((a (car item))
		   (b (cdr item)))
	       (cond ((eq? 'REINSERT-PROPERTIES a)
		      (reinsert-properties-size b))
		     ((eq? 'REPLACEMENT a)
		      (fix:+ 2 (system-vector-length (car b))))
		     ((string? a)
		      (fix:+ 1 (system-vector-length a)))
		     (else 0))))
      2))

;;;; M-x undo

(define-command undo
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  "*p"
  (let ((command-tag (string-copy "undo")))
    (lambda (argument)
      (if (> argument 0)
	  (let ((buffer (current-buffer)))
	    (let ((auto-saved? (buffer-auto-saved? buffer)))
	      (set-command-message!
	       command-tag
	       (command-message-receive command-tag
		 (lambda (undo-data)
		   (undo-more buffer undo-data argument))
		 (lambda ()
		   (undo-more buffer (undo-start buffer) (+ argument 1)))))
	      (if (and auto-saved? (not (buffer-modified? buffer)))
		  (delete-auto-save-file! buffer))
	      (if (not (typein-window? (current-window)))
		  (message "Undo!"))))))))

(define (undo-start buffer)
  (let ((undo-data (group-undo-data (buffer-group buffer))))
    (if (eq? #t undo-data)
	(editor-error "No undo information in this buffer: "
		      (buffer-name buffer)))
    undo-data))

(define (undo-more buffer undo-data n)
  (let loop ((undo-data undo-data) (n n))
    (if (> n 0)
	(begin
	  (if (not (pair? undo-data))
	      (editor-error "No further undo information: "
			    (buffer-name buffer)))
	  (loop (undo-one-step buffer undo-data) (- n 1)))
	undo-data)))

(define (undo-one-step buffer data)
  ;; Perform one undo step on BUFFER, returning the unused portion of DATA.
  (let ((group (buffer-group buffer))
	(point (mark-temporary-copy (buffer-point buffer)))
	(outside-visible-range
	 (lambda ()
	   (editor-error
	    "Changes to be undone are outside visible portion of buffer: "
	    (buffer-name buffer)))))
    (let ((finish
	   (lambda (data)
	     (set-buffer-point! buffer point)
	     data)))
      (let loop ((data data))
	(if (pair? data)
	    (let ((element (car data))
		  (data (cdr data)))
	      (cond ((not element)
		     ;; #F means boundary: this step is done.
		     (finish data))
		    ((fix:fixnum? element)
		     ;; Fixnum is a point position.
		     (set-mark-index! point element)
		     (loop data))
		    ((pair? element)
		     (let ((a (car element))
			   (b (cdr element)))
		       (cond ((eq? #t a)
			      ;; (#t . MOD-TIME) means first modification
			      (if (eqv? b (buffer-modification-time buffer))
				  (buffer-not-modified! buffer)))
			     ((eq? 'REINSERT-PROPERTIES a)
			      (group-reinsert-properties! group b))
			     ((eq? 'REPLACEMENT a)
			      (let ((string (car b))
				    (start (cdr b)))
				(if (or (fix:< start (group-start-index group))
					(fix:> (fix:+ start
						      (string-length string))
					       (group-end-index group)))
				    (outside-visible-range))
				;; No need to set point, set explicitly.
				(group-replace-string! group start string)))
			     ((fix:fixnum? a)
			      ;; (START . END) means insertion
			      (if (or (fix:< a (group-start-index group))
				      (fix:> a (group-end-index group))
				      (fix:> b (group-end-index group)))
				  (outside-visible-range))
			      (set-mark-index! point a)
			      (group-delete! group a b))
			     ((string? a)
			      ;; (STRING . START) means deletion
			      (if (fix:< b 0)
				  ;; negative START means set point at end
				  (let ((b (fix:- 0 b)))
				    (if (or (fix:< b (group-start-index group))
					    (fix:> b (group-end-index group)))
					(outside-visible-range))
				    (set-mark-index! point b)
				    (group-insert-string! group b a))
				  ;; nonnegative START means set point at start
				  (begin
				    (if (or (fix:< b (group-start-index group))
					    (fix:> b (group-end-index group)))
					(outside-visible-range))
				    (group-insert-string! group b a)
				    (set-mark-index! point b))))
			     (else
			      (error "Malformed undo element:" element))))
		     (loop data))
		    (else
		     (error "Malformed undo element:" element))))
	    (finish data))))))
