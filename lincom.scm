;;; -*-Scheme-*-
;;;
;;; $Id: lincom.scm,v 1.129 2007/01/05 21:19:23 cph Exp $
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

;;;; Line/Indentation Commands


;;;; Lines

(define-command count-lines-region
  "Type number of lines from point to mark."
  "r"
  (lambda (region)
    (message "Region has "
	     (region-count-lines region)
	     " lines, "
	     (- (region-end-index region) (region-start-index region))
	     " characters.")))

(define-command what-line
  "Print the current line number (in the buffer) of point."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (without-group-clipped! (buffer-group buffer)
	(lambda ()
	  (message "Line "
		   (+ (count-lines (buffer-start buffer) (current-point))
		      1)))))))

(define (count-lines start end)
  (region-count-lines (make-region start end)))

(define-command goto-line
  "Goto line ARG, counting from line 1 at beginning of buffer."
  "NGoto line"
  (lambda (n)
    (set-current-point!
     (line-start (buffer-start (current-buffer)) (- n 1) 'ERROR))))

(define-command transpose-lines
  "Transpose the lines before and after the cursor.
With a positive argument it transposes the lines before and after the
cursor, moves right, and repeats the specified number of times,
dragging the line to the left of the cursor right.

With a negative argument, it transposes the two lines to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.

With a zero argument, it transposes the lines at point and mark.

At the end of a buffer, with no argument, the preceding two lines are
transposed."
  "p"
  (lambda (argument)
    (cond ((and (= argument 1) (group-end? (current-point)))
	   (if (not (line-start? (current-point)))
	       (insert-newlines 1))
	   (insert-string (extract-and-delete-string
			   (forward-line (current-point) -2 'ERROR)
			   (forward-line (current-point) -1 'ERROR))
			  (current-point)))
	  (else
	   (transpose-things forward-line argument)))))

;;;; Pages

(define-command forward-page
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any line whose beginning matches the regexp page-delimiter."
  "p"
  (lambda (argument)
    (move-thing forward-page argument 'FAILURE)))

(define-command backward-page
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any line whose beginning matches the regexp page-delimiter."
  "p"
  (lambda (argument)
    (move-thing backward-page argument 'FAILURE)))

(define-command mark-page
  "Put mark at end of page, point at beginning."
  "P"
  (lambda (argument)
    (let ((end
	   (forward-page (current-point)
			 (1+ (or (command-argument-value argument) 0))
			 'LIMIT)))
      (set-current-region! (make-region (backward-page end 1 'LIMIT) end)))))

(define-command narrow-to-page
  "Make text outside current page invisible."
  "d"
  (lambda (mark)
    (region-clip! (page-interior-region mark))))

(define (page-interior-region point)
  (if (and (group-end? point)
	   (let ((break (re-match-forward
			 (ref-variable page-delimiter)
			 (line-start point 0)
			 point)))
	     (and break
		  (mark= break point))))
      (make-region point point)
      (let ((end (forward-page point 1 'LIMIT)))
	(make-region (let ((start (backward-page end 1 'LIMIT)))
		       (if (and (line-end? start)
				(not (group-start? start))
				(not (group-end? start)))
			   (mark1+ start)
			   start))
		     (let ((end* (line-start end 0)))
		       (if (and (mark<= point end*)
				(let ((break (re-match-forward
					      (ref-variable page-delimiter)
					      end*
					      end)))
				  (and break
				       (mark= break end))))
			   end*
			   end))))))

(define-command count-lines-page
  "Report number of lines on current page."
  "d"
  (lambda (point)
    (let ((end
	   (let ((end (forward-page point 1 'LIMIT)))
	     (if (group-end? end)
		 end
		 (line-start end 0)))))
      (let ((start (backward-page end 1 'LIMIT)))
	(message "Page has " (count-lines start end)
		 " lines (" (count-lines start point)
		 " + " (count-lines point end) ")")))))

(define-command what-page
  "Report page and line number of point."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (without-group-clipped! (buffer-group buffer)
	(lambda ()
	  (let ((point (current-point)))
	    (message "Page "
		     (let loop ((count 0) (start (buffer-start buffer)))
		       (if (or (not start)
			       (mark> start point))
			   count
			   (loop (+ count 1) (forward-page start 1))))
		     ", Line "
		     (+ (count-lines
			 (backward-page (forward-page point 1 'LIMIT) 1 'LIMIT)
			 point)
			1))))))))

(define-command goto-page
  "Goto page ARG, counting from page 1 at beginning of buffer."
  "NGoto page"
  (lambda (n)
    (set-current-point!
     (forward-page (buffer-start (current-buffer)) (- n 1) 'ERROR))))

;;;; Indentation

(define (indent-to-left-margin)
  (maybe-change-indentation (ref-variable left-margin)
			    (line-start (current-point) 0)))

(define-variable indent-line-procedure
  "Procedure used to indent current line.
If this is the procedure indent-to-left-margin,
\\[indent-for-tab-command] will insert tab characters rather than indenting."
  indent-to-left-margin)

(define-command indent-according-to-mode
  "Indent line in proper way for current major mode.
The exact behavior of this command is determined
by the variable indent-line-procedure."
  ()
  (lambda ()
    ((ref-variable indent-line-procedure))))

(define-command indent-for-tab-command
  "Indent line in proper way for current major mode.
The exact behavior of this command is determined
by the variable indent-line-procedure."
  "p"
  (lambda (argument)
    (let ((indent-line-procedure (ref-variable indent-line-procedure)))
      (if (eq? indent-line-procedure indent-to-left-margin)
	  (let ((insert-tab (ref-command insert-tab)))
	    (do ((i argument (- i 1)))
		((<= i 0))
	      (insert-tab)))
	  (indent-line-procedure)))))

(define-command newline-and-indent
  "Insert a newline, then indent according to major mode.
Indentation is done using the current indent-line-procedure,
except that if there is a fill-prefix it is used to indent.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
specified left-margin column."
  ()
  (lambda ()
    (delete-horizontal-space)
    (insert-newline)
    (let ((fill-prefix (ref-variable fill-prefix)))
      (if fill-prefix
	  (region-insert-string! (current-point) fill-prefix)
	  ((ref-command indent-according-to-mode))))))

(define-command reindent-then-newline-and-indent
  "Reindent the current line according to mode (like
\\[indent-according-to-mode]), then insert a newline,
and indent the new line indent according to mode."
  ()
  (lambda ()
    ((ref-command indent-according-to-mode))
    ((ref-command newline-and-indent))))

(define-variable-per-buffer indent-tabs-mode
  "If false, do not use tabs for indentation or horizontal spacing."
  #t
  boolean?)

(define-command indent-tabs-mode
  "Enables or disables use of tabs as indentation.
A positive argument turns use of tabs on;
zero or negative, turns it off.
With no argument, the mode is toggled."
  "P"
  (lambda (argument)
    (set-variable! indent-tabs-mode
		   (if argument
		       (positive? (command-argument-value argument))
		       (not (ref-variable indent-tabs-mode))))))

(define-command insert-tab
  "Insert a tab character."
  ()
  (lambda ()
    (if (ref-variable indent-tabs-mode)
	(insert-char #\tab)
	(maybe-change-column
	 (let ((tab-width (ref-variable tab-width)))
	   (* tab-width (1+ (quotient (current-column) tab-width))))))))

(define-command indent-relative
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace."
  ()
  (lambda ()
    (let ((point (current-point)))
      (let ((indentation (indentation-of-previous-non-blank-line point)))
	(cond ((not (= indentation (current-indentation point)))
	       (change-indentation indentation point))
	      ((line-start? (horizontal-space-start point))
	       (set-current-point! (horizontal-space-end point))))))))

(define (indentation-of-previous-non-blank-line mark)
  (let ((start (find-previous-non-blank-line mark)))
    (if start
	(current-indentation start)
	0)))

(define-variable indent-region-procedure
  "Function which is short cut to indent each line in region with Tab.
#F means really call Tab on each line."
  #f
  (lambda (object)
    (or (false? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 2)))))

(define-command indent-region
  "Indent each nonblank line in the region.
With no argument, indent each line with Tab.
With argument COLUMN, indent each line to that column."
  "r\nP"
  (lambda (region argument)
    (let ((start (region-start region))
	  (end (region-end region)))
      (cond (argument
	     (indent-region start end argument))
	    ((ref-variable indent-region-procedure)
	     ((ref-variable indent-region-procedure) start end))
	    (else
	     (for-each-line-in-region start end
	       (let ((indent-line (ref-variable indent-line-procedure)))
		 (lambda (start)
		   (set-current-point! start)
		   (indent-line)))))))))

(define (indent-region start end n-columns)
  (if (exact-nonnegative-integer? n-columns)
      (for-each-line-in-region start end
	(lambda (start)
	  (delete-string start (horizontal-space-end start))
	  (insert-horizontal-space n-columns start)))))

(define-command indent-rigidly
  "Indent all lines starting in the region sideways by ARG columns."
  "r\nP"
  (lambda (region argument)
    (let ((argument (command-argument-numeric-value argument)))
      (if argument
	  (indent-rigidly (region-start region)
			  (region-end region)
			  argument)))))

(define (indent-rigidly start end n-columns)
  (for-each-line-in-region start end
    (lambda (start)
      (let ((end (horizontal-space-end start)))
	(if (line-end? end)
	    (delete-string start end)
	    (let ((new-column (max 0 (+ n-columns (mark-column end)))))
	      (delete-string start end)
	      (insert-horizontal-space new-column start)))))))

(define (for-each-line-in-region start end procedure)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (let ((start (mark-right-inserting-copy (line-start start 0))))
    (let ((end
	   (mark-left-inserting-copy
	    (if (and (line-start? end) (mark< start end))
		(mark-1+ end)
		(line-end end 0)))))
      (let loop ()
	(procedure start)
	(let ((m (line-end start 0)))
	  (if (mark< m end)
	      (begin
		(move-mark-to! start (mark1+ m))
		(loop)))))
      (mark-temporary! start)
      (mark-temporary! end))))

(define-command newline
  "Insert a newline.  With arg, insert that many newlines."
  "*P"
  (lambda (argument)
    (self-insert #\newline
		 (command-argument-numeric-value argument)
		 ;; Don't do auto-fill if argument supplied.
		 (not argument))))

(define-command split-line
  "Move rest of this line vertically down.
Inserts a newline, and then enough tabs/spaces so that
what had been the rest of the current line is indented as much as
it had been.  Point does not move, except to skip over indentation
that originally followed it. 
With argument, makes extra blank lines in between."
  "p"
  (lambda (argument)
    (set-current-point! (horizontal-space-end (current-point)))
    (let ((m* (mark-right-inserting (current-point))))
      (insert-newlines (max argument 1))
      (insert-horizontal-space (mark-column m*))
      (set-current-point! m*))))

(define-command back-to-indentation
  "Move to end of this line's indentation."
  ()
  (lambda ()
    (set-current-point!
     (horizontal-space-end (line-start (current-point) 0)))))

(define-command delete-horizontal-space
  "Delete all spaces and tabs around point."
  ()
  delete-horizontal-space)

(define-command just-one-space
  "Delete all spaces and tabs around point, leaving just one space."
  ()
  (lambda ()
    (delete-horizontal-space)
    (insert-chars #\Space 1)))

(define-command delete-blank-lines
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete all blank lines that follow it."
  "*"
  (lambda ()
    (let ((point (current-point)))
      (let ((end
	     (let loop ((m point))
	       (let ((m (line-end m 0)))
		 (if (group-end? m)
		     m
		     (let ((m* (mark1+ m)))
		       (if (line-blank? m*)
			   (loop m*)
			   m)))))))
	(if (line-blank? point)
	    (let ((start
		   (let loop ((m (line-start point 0)))
		     (let ((m* (line-start m -1)))
		       (if (and m* (line-blank? m*))
			   (loop m*)
			   m)))))
	      (delete-string start
			     (if (or (mark< (line-end start 0) end)
				     (group-end? end))
				 end
				 (mark1+ end))))
	    (let ((start (line-end point 0)))
	      (if (mark< start end)
		  (delete-string (mark1+ start)
				 (if (group-end? end)
				     end
				     (mark1+ end))))))))))

(define-command delete-indentation
  "Join this line to previous and fix up whitespace at join.
With argument, join this line to following line."
  "*P"
  (lambda (argument)
    (let ((point
	   (mark-left-inserting-copy
	    (horizontal-space-start
	     (line-end (current-point) (if (not argument) -1 0) 'ERROR))))
	  (fill-prefix (ref-variable fill-prefix)))
      (delete-string point (line-start point 1 'ERROR))
      (if fill-prefix
	  (let ((m
		 (match-forward fill-prefix point (line-end point 0) #f)))
	    (if m
		(delete-string point m))))
      (mark-temporary! point)
      (set-current-point! point))
    ((ref-command fixup-whitespace))))

(define-command fixup-whitespace
  "Fix up white space between objects around point.
Leave one space or none, according to the context."
  "*"
  (lambda ()
    (delete-horizontal-space)
    (let ((point (current-point)))
      (if (not (or (re-match-forward "^\\|\\s)" point)
		   (re-match-forward "$\\|\\s(\\|\\s'" (mark-1+ point))))
	  (insert-char #\space point)))))

;;;; Tabification

(define-command untabify
  "Convert all tabs in region to multiple spaces, preserving columns.
The variable tab-width controls the action."
  "r"
  (lambda (region)
    (untabify-region (region-start region) (region-end region))))

(define (untabify-region start end)
  (let ((start (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end)))
    (let loop ()
      (let ((m (char-search-forward #\tab start end)))
	(if m
	    (begin
	      (move-mark-to! start m)
	      (let ((tab (mark-1+ start)))
		(let ((n-spaces (- (mark-column start) (mark-column tab))))
		  (delete-string tab start)
		  (insert-chars #\space n-spaces start)))
	      (loop)))))
    (mark-temporary! start)
    (mark-temporary! end)))

(define-command tabify
  "Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
The variable tab-width controls the action."
  "r"
  (lambda (region)
    (tabify-region (region-start region) (region-end region))))

(define (tabify-region start end)
  (let ((start (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end))
	(tab-width (group-tab-width (mark-group start))))
    (do ()
	((not (re-search-forward "[ \t][ \t]+" start end #f)))
      (move-mark-to! start (re-match-start 0))
      (let ((end-column (mark-column (re-match-end 0))))
	(delete-string start (re-match-end 0))
	(insert-horizontal-space end-column start tab-width)))
    (mark-temporary! start)
    (mark-temporary! end)))
