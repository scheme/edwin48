;;; -*-Scheme-*-
;;;
;;; $Id: occur.scm,v 1.13 2007/01/05 21:19:24 cph Exp $
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

;;;; Occurrence Commands


(define-command keep-lines
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
Applies to all lines after point."
  "sKeep lines (containing match for regexp)"
  (lambda (regexp)
    (let ((point (current-point)))
      (keep-lines point (group-end point) regexp))))

(define-command delete-non-matching-lines
  (command-description (ref-command-object keep-lines))
  (command-interactive-specification (ref-command-object keep-lines))
  (command-procedure (ref-command-object keep-lines)))

(define (keep-lines start end regexp)
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(start (mark-index start))
	(anchor (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end)))
    (letrec
	((loop
	  (lambda (start point)
	    (let ((point
		   (re-search-buffer-forward pattern syntax-table
					     group point (mark-index end))))
	      (if point
		  (begin
		    (set-mark-index! anchor point)
		    (let ((end
			   (line-start-index group (re-match-start-index 0))))
		      (if (< start end)
			  (group-delete! group start end)))
		    (continue (mark-index anchor)))
		  (group-delete! group start (mark-index end))))))
	 (continue
	  (lambda (point)
	    (let ((start (line-end-index group point)))
	      (if (< start (mark-index end))
		  (loop (+ start 1) point))))))
      (if (line-start-index? group start)
	  (loop start start)
	  (continue start)))
    (mark-temporary! anchor)
    (mark-temporary! end)))

(define-command flush-lines
  "Delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point."
  "sFlush lines (containing match for regexp)"
  (lambda (regexp)
    (let ((point (current-point)))
      (flush-lines point (group-end point) regexp))))

(define-command delete-matching-lines
  (command-description (ref-command-object flush-lines))
  (command-interactive-specification (ref-command-object flush-lines))
  (command-procedure (ref-command-object flush-lines)))

(define (flush-lines start end regexp)
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(start (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end)))
    (do ()
	((not (re-search-buffer-forward pattern
					syntax-table
					group
					(mark-index start)
					(mark-index end))))
      (let ((point (line-end-index group (re-match-end-index 0))))
	(set-mark-index! start point)
	(group-delete! group
		       (line-start-index group (re-match-start-index 0))
		       (if (< point (mark-index end)) (+ point 1) point))))
    (mark-temporary! start)
    (mark-temporary! end)))

(define-command count-matches
  "Print number of matches for REGEXP following point."
  "sCount matches for (regexp)"
  (lambda (regexp)
    (message (let ((point (current-point)))
	       (count-matches point (group-end point) regexp))
	     " occurrences")))

(define-command how-many
  (command-description (ref-command-object count-matches))
  (command-interactive-specification (ref-command-object count-matches))
  (command-procedure (ref-command-object count-matches)))

(define (count-matches start end regexp)
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(end (mark-index end)))
    (let loop ((start (mark-index start)) (result 0))
      (let ((match
	     (re-search-buffer-forward pattern syntax-table group start end)))
	(if match
	    (loop match (+ result 1))
	    result)))))

(define-major-mode occur fundamental "Occur"
  "Major mode for output from \\[occur].
Move point to one of the occurrences in this buffer,
then use \\[occur-mode-goto-occurrence] to go to the same occurrence
in the buffer that the occurrences were found in.")

(define-key 'occur '(#\c-c #\c-c) 'occur-mode-goto-occurrence)

(define-command occur-mode-goto-occurrence
  "Go to the line this occurrence was found in, in the buffer it was found in."
  ()
  (lambda ()
    (let ((mark
	   (let ((point (current-point)))
	     (let ((r (region-get point 'OCCURRENCE #f)))
	       (if (not r)
		   (editor-error "No occurrence selected."))
	       (region-start r)))))
      (let ((buffer (mark-buffer mark)))
	(if (not (buffer-alive? buffer))
	    (editor-error "Buffer in which occurences were found is deleted."))
	(pop-up-buffer buffer #t)
	(set-buffer-point! buffer mark)))))

(define-variable list-matching-lines-default-context-lines
  "Default number of context lines to include around a list-matching-lines
match.  A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  0
  exact-integer?)

(define-command occur
  "Show all lines following point containing a match for REGEXP.
Display each line with NLINES lines before and after,
 or -NLINES before if NLINES is negative.
NLINES defaults to list-matching-lines-default-context-lines.
Interactively it is the prefix arg.

The lines are shown in a buffer named *Occur*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  "sList lines matching regexp\nP"
  (lambda (regexp argument)
    (pop-up-occur-buffer (current-point)
			 (buffer-end (selected-buffer))
			 regexp
			 (and argument
			      (command-argument-numeric-value argument)))))

(define-command list-matching-lines
  (command-description (ref-command-object occur))
  (command-interactive-specification (ref-command-object occur))
  (command-procedure (ref-command-object occur)))

(define (pop-up-occur-buffer start end regexp nlines)
  (let ((occurrences (re-occurrences start end regexp))
	(occur-buffer (temporary-buffer "*Occur*")))
    (let ((output (mark-left-inserting-copy (buffer-start occur-buffer))))
      (insert-string (write-to-string (length occurrences)) output)
      (insert-string " lines matching " output)
      (insert-string (write-to-string regexp) output)
      (insert-string " in buffer " output)
      (insert-string (buffer-name (mark-buffer start)) output)
      (insert-string ".\n" output)
      (set-buffer-major-mode! occur-buffer (ref-mode-object occur))
      (format-occurrences occurrences
			  (or nlines
			      (ref-variable
			       list-matching-lines-default-context-lines
			       start))
			  output)
      (mark-temporary! output))
    (set-buffer-point! occur-buffer (buffer-start occur-buffer))
    (pop-up-buffer occur-buffer #f)))

(define (re-occurrences start end regexp)
  (let ((pattern
	 (re-compile-pattern regexp (ref-variable case-fold-search start)))
	(syntax-table (ref-variable syntax-table start))
	(group (mark-group start))
	(end (mark-index end)))
    (let loop ((start (mark-index start)) (occurrences '()))
      (let ((match
	     (re-search-buffer-forward pattern syntax-table group start end)))
	(if match
	    (loop (line-end-index group (re-match-end-index 0))
		  (cons (make-region (mark-right-inserting (re-match-start 0))
				     (mark-left-inserting (re-match-end 0)))
			occurrences))
	    (reverse! occurrences))))))

(define (format-occurrences occurrences nlines output)
  (if (pair? occurrences)
      (let loop
	  ((occurrences occurrences)
	   (prev-ls #f)
	   (line 1))
	(let ((r (car occurrences))
	      (m (mark-right-inserting-copy output)))
	  (let ((ls (line-start (region-start r) 0)))
	    (let ((line
		   (+ line (count-lines (or prev-ls (group-start ls)) ls))))
	      (format-occurrence ls (line-start (region-end r) 0)
				 line nlines
				 output)
	      (region-put! m output 'OCCURRENCE r)
	      (if (pair? (cdr occurrences))
		  (begin
		    (if (not (= nlines 0))
			(insert-string "--------\n" output))
		    (loop (cdr occurrences) ls line)))))))))

(define (format-occurrence rs re line nlines output)
  (let ((empty "       "))
    (if (not (= nlines 0))
	(let loop ((ls (line-start rs (- (abs nlines)) 'LIMIT)))
	  (if (mark< ls rs)
	      (let ((ls* (line-start ls 1 'ERROR)))
		(insert-string empty output)
		(insert-string ":" output)
		(insert-region ls ls* output)
		(loop ls*)))))
    (let loop ((ls rs))
      (let ((le (line-end ls 0)))
	(insert-string (if (mark= ls rs)
			   (pad-on-left-to (number->string line) 7)
			   empty)
		       output)
	(insert-string ":" output)
	(insert-region ls le output)
	(insert-newline output)
	(if (mark< le re)
	    (loop (line-start ls 1 'LIMIT)))))
    (if (> nlines 0)
	(let ((ls (line-start rs 1 #f)))
	  (if ls
	      (let loop ((ls ls) (n nlines))
		(let ((le (line-end ls 0)))
		  (insert-string empty output)
		  (insert-string ":" output)
		  (insert-region ls le output)
		  (insert-newline output)
		  (if (and (not (group-end? le)) (> n 1))
		      (loop (mark1+ le) (- n 1))))))))))
