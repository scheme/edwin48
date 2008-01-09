#| -*-Scheme-*-

$Id: fill.scm,v 1.75 2007/01/05 21:19:23 cph Exp $

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

;;;; Text Fill Commands

(declare (usual-integrations))

(define-variable-per-buffer fill-column
  "Column beyond which automatic line-wrapping should happen.
Automatically becomes local when set in any fashion."
  70
  exact-nonnegative-integer?)

(define-variable-per-buffer fill-prefix
  "String for filling to insert at front of new line, or #f for none.
Setting this variable automatically makes it local to the current buffer."
  #f
  string-or-false?)

(define-variable-per-buffer left-margin
  "Column for the default indent-line-function to indent to.
Linefeed indents to this column in Fundamental mode.
Automatically becomes local when set in any fashion."
  0
  exact-nonnegative-integer?)

(define-variable adaptive-fill-mode
  "True means determine a paragraph's fill prefix from its text."
  #t
  boolean?)

(define-variable adaptive-fill-regexp
  "Regexp to match text at start of line that constitutes indentation.
If Adaptive Fill mode is enabled, a prefix matching this pattern
on the first and second lines of a paragraph is used as the
standard indentation for the whole paragraph.

If the paragraph has just one line, the indentation is taken from that
line, but in that case `adaptive-fill-first-line-regexp' also plays
a role."
  "[ \t]*\\([-|#;>*]+ *\\|(?[0-9]+[.)] *\\)*"
  string?)

(define-variable adaptive-fill-first-line-regexp
  "Regexp specifying whether to set fill prefix from a one-line paragraph.
When a paragraph has just one line, then after `adaptive-fill-regexp'
finds the prefix at the beginning of the line, if it doesn't
match this regexp, it is replaced with whitespace.

By default, this regexp matches sequences of just spaces and tabs.

However, we never use a prefix from a one-line paragraph
if it would act as a paragraph-starter on the second line."
  "\\`[ \t]*\\'"
  string?)

(define-variable adaptive-fill-procedure
  "Procedure to call to choose a fill prefix for a paragraph.
This procedure is used when `adaptive-fill-regexp' does not match."
  #f
  (lambda (object) (or (not object) (procedure? object))))

(define-command set-fill-column
  "Set fill-column to current column, or to argument if given.
fill-column's value is separate for each buffer."
  "P"
  (lambda (argument)
    (let ((column
	   (or (command-argument-value argument)
	       (current-column))))
      (set-variable! fill-column column)
      (message "fill-column set to " column))))

(define-command set-fill-prefix
  "Set the fill-prefix to the current line up to point.
Filling expects lines to start with the fill prefix
and reinserts the fill prefix in each resulting line."
  "d"
  (lambda (point)
    (let ((string (extract-string (line-start point 0) point)))
      (if (string-null? string)
	  (begin
	    (set-variable! fill-prefix #f)
	    (message "fill-prefix cancelled"))
	  (begin
	    (set-variable! fill-prefix string)
	    (message "fill-prefix: \"" string "\""))))))

(define-command fill-paragraph
  "Fill paragraph at or after point.
Prefix arg means justify as well."
  "d\nP"
  (lambda (point justify?)
    (let ((region (paragraph-text-region point)))
      (if (not region)
	  (editor-error))
      ((ref-command fill-region-as-paragraph) region justify?))))

(define-command fill-region-as-paragraph
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too."
  "r\nP"
  (lambda (region justify?)
    (let ((start (region-start region)))
      (fill-region-as-paragraph
       start
       (region-end region)
       (ref-variable fill-prefix start)
       (ref-variable fill-column start)
       justify?))))

(define-command fill-individual-paragraphs
  "Fill each paragraph in region according to its individual fill prefix."
  "r\nP"
  (lambda (region justify?)
    (let ((start (region-start region)))
      (fill-individual-paragraphs start
				  (region-end region)
				  (ref-variable fill-column start)
				  justify?
				  #f))))

(define-command fill-region
  "Fill each of the paragraphs in the region.
Prefix arg means justify as well."
  "r\nP"
  (lambda (region justify?)
    (let ((start (region-start region)))
      (fill-region start
		   (region-end region)
		   (ref-variable fill-prefix start)
		   (ref-variable fill-column start)
		   justify?))))

(define-command justify-current-line
  "Add spaces to line point is in, so it ends at fill-column."
  "d"
  (lambda (point)
    (justify-line point
		  (ref-variable fill-prefix point)
		  (ref-variable fill-column point))))

(define-command center-line
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation to match
the distance between the end of the text and `fill-column'."
  "d"
  (lambda (mark) (center-line mark)))

(define (center-line mark)
  (let ((mark (mark-permanent! mark)))
    (delete-horizontal-space (line-start mark 0))
    (delete-horizontal-space (line-end mark 0))
    (let ((d
	   (- (- (ref-variable fill-column mark)
		 (ref-variable left-margin mark))
	      (mark-column (line-end mark 0)))))
      (if (positive? d)
	  (insert-horizontal-space (+ (ref-variable left-margin mark)
				      (quotient d 2))
				   (line-start mark 0))))))

(define (fill-region-as-paragraph start end fill-prefix fill-column justify?)
  (let ((start (mark-right-inserting-copy (skip-chars-forward "\n" start end)))
	(end (mark-left-inserting-copy (skip-chars-backward "\n" end start))))
    (let ((fill-prefix
	   (or fill-prefix
	       (and (ref-variable adaptive-fill-mode start)
		    (fill-context-prefix start end))))
	  (point (mark-left-inserting-copy start)))
      ;; Delete the fill prefix from every line except the first.
      (if fill-prefix
	  (begin
	    (if (>= (string-length fill-prefix) fill-column)
		(editor-error "fill-prefix too long for specified width"))
	    (let ((m (match-forward fill-prefix start end #f)))
	      (if m
		  (begin
		    (move-mark-to! point m)
		    (move-mark-to! start m))))
	    (let loop ()
	      (let ((m (char-search-forward #\newline point end)))
		(if m
		    (begin
		      (move-mark-to! point m)
		      (let ((m (match-forward fill-prefix point end #f)))
			(if m
			    (delete-string point m)))
		      (loop)))))
	    (move-mark-to! point start)))
      ;; Make sure sentences ending at end of line get an extra space.
      (let loop ()
	(let ((m (re-search-forward "[.?!][])\"']*$" point end #f)))
	  (if m
	      (begin
		(move-mark-to! point m)
		(insert-char #\space point)
		(loop)))))
      ;; Change all newlines to spaces.
      (move-mark-to! point start)
      (let loop ()
	(let ((m (char-search-forward #\newline point end)))
	  (if m
	      (begin
		(move-mark-to! point m)
		(delete-left-char point)
		(insert-char #\space point)
		(loop)))))
      ;; Flush excess spaces, except in the paragraph indentation.
      (move-mark-to! point (skip-chars-forward " \t" start end))
      (let loop ()
	(if (re-search-forward "   *" point end #f)
	    (begin
	      (move-mark-to! point (delete-match))
	      (insert-string (if (fill:sentence-end? point start) "  " " ")
			     point)
	      (loop))))
      (delete-string (horizontal-space-start end) end)
      (insert-string "  " end)
      (move-mark-to! point start)
      (let loop ()
	(let ((target (move-to-column point fill-column)))
	  (if (mark>= target (horizontal-space-start end))
	      (delete-horizontal-space end)
	      (begin
		(move-mark-to!
		 point
		 (if (char=? #\space (extract-right-char target))
		     target
		     (let ((m (skip-chars-backward "^ \n" target point)))
		       (if (mark> m point)
			   m
			   (skip-chars-forward "^ \n" target end)))))
		(if (mark< point end)
		    (begin
		      (delete-horizontal-space point)
		      (if (mark< point end) (insert-newline point))
		      (if justify?
			  (fill:call-with-line-marks
			   (if (mark< point end) (mark-1+ point) point)
			   fill-prefix
			   (lambda (start end)
			     (fill:justify-line start end fill-column))))
		      (if fill-prefix (insert-string fill-prefix point))))
		(if (mark< point end)
		    (loop))))))
      (mark-temporary! point)
      (mark-temporary! end)
      (mark-temporary! start))))

(define (fill-region start end fill-prefix fill-column justify?)
  (let ((start (mark-right-inserting-copy start))
	(end (mark-left-inserting-copy end))
	(point (mark-left-inserting-copy start))
	(pend (mark-left-inserting-copy start)))
    (let loop ()
      (if (mark< point end)
	  (begin
	    (move-mark-to! pend
			   (or (forward-one-paragraph point end fill-prefix)
			       end))
	    (if (mark>= (or (backward-one-paragraph pend start fill-prefix)
			    start)
			point)
		(fill-region-as-paragraph point
					  pend
					  fill-prefix
					  fill-column
					  justify?))
	    (move-mark-to! point pend)
	    (loop))))
    (mark-temporary! pend)
    (mark-temporary! point)
    (mark-temporary! end)
    (mark-temporary! start)))

(define (fill-individual-paragraphs start end fill-column justify? mail?)
  (operate-on-individual-paragraphs start end mail?
    (lambda (p-start p-end fill-prefix)
      (fill-region-as-paragraph p-start p-end fill-prefix
				fill-column justify?))))

(define (operate-on-individual-paragraphs start end mail? operator)
  (let ((point (mark-left-inserting-copy start))
	(pend (mark-left-inserting-copy start))
	(end (mark-left-inserting-copy end))
	(paragraph-separate (ref-variable paragraph-separate start))
	(compute-prefix
	 (lambda (point)
	   (let ((ls (line-start point 0)))
	     (or (and (ref-variable adaptive-fill-mode point)
		      (fill-context-prefix ls (line-end point 0) ""))
		 (extract-string ls point))))))
    (if (and mail? (re-search-forward "^[ \t]*[^ \t\n]*:" point end #f))
	(move-mark-to! point (or (search-forward "\n\n" point end #f) end)))
    (let loop ()
      (move-mark-to!
       point
       (let skip-separators ((mark point))
	 (cond ((mark>= mark end) end)
	       ((let* ((ls (skip-chars-forward " \t" mark end))
		       (fill-prefix
			(and (ref-variable adaptive-fill-mode ls)
			     (fill-context-prefix ls (line-end ls 0) ""))))
		  (if fill-prefix
		      (let ((fp (match-forward fill-prefix ls end #f)))
			(or (not fp)
			    (re-match-forward paragraph-separate fp end #f)))
		      (re-match-forward paragraph-separate ls end #f)))
		(skip-separators (line-start mark 1 'LIMIT)))
	       (else mark))))
      (move-mark-to! point (skip-chars-forward " \t" point end))
      (if (mark< point end)
	  (let ((fill-prefix (compute-prefix point)))
	    (let find-end ((le (line-end point 0 'ERROR)))
	      (let ((ls
		     (and (mark< le end)
			  (skip-chars-forward " \t"
					      (line-start le 1 'ERROR)
					      end))))
		(if (and ls
			 (mark< ls end)
			 (let ((m (match-forward fill-prefix ls)))
			   (and m
				(not (paragraph-start? m end))))
			 (string=? (compute-prefix ls) fill-prefix))
		    (find-end (line-end ls 0 'ERROR))
		    (move-mark-to! pend le))))
	    (operator point pend fill-prefix)
	    (let ((ls (line-start pend 1 #f)))
	      (if (and ls (mark< ls end))
		  (begin
		    (move-mark-to! point ls)
		    (loop)))))))
    (mark-temporary! pend)
    (mark-temporary! point)
    (mark-temporary! end)
    (mark-temporary! start)))

(define (justify-line mark fill-prefix fill-column)
  (fill:call-with-line-marks mark fill-prefix
    (lambda (start end)
      (let ((point (mark-left-inserting-copy start)))
	(let loop ()
	  (if (re-search-forward "   *" point end #f)
	      (begin
		(move-mark-to! point (delete-match))
		(insert-string (if (fill:sentence-end? point start) "  " " ")
			       point)
		(loop))))
	(mark-temporary! point))
      (fill:justify-line start end fill-column))))

(define (fill:call-with-line-marks mark fill-prefix procedure)
  (let ((end (mark-left-inserting-copy (line-end mark 0))))
    (let ((start
	   (mark-right-inserting-copy
	    (skip-chars-forward
	     " \t"
	     (let ((start (line-start end 0)))
	       (or (and fill-prefix
			(match-forward fill-prefix start end #f))
		   start))
	     end))))
      (procedure start end)
      (mark-temporary! start)
      (mark-temporary! end))))

(define (fill:justify-line start end fill-column)
  (let ((point (mark-right-inserting-copy end)))
    (do ((ncols (- fill-column (mark-column end)) (- ncols 1)))
	((<= ncols 0))
      (do ((i (+ 3 (random 3)) (- i 1)))
	  ((= i 0))
	(move-mark-to!
	 point
	 (skip-chars-backward " "
			      (or (char-search-backward #\space point start)
				  (char-search-backward #\space end start)
				  start)
			      start)))
      (insert-char #\space point))
    (mark-temporary! point)))

(define (fill:sentence-end? point start)
  (let ((m (skip-chars-backward "])\"'" point start)))
    (and (not (group-start? m))
	 (memv (extract-left-char m) '(#\. #\? #\!)))))

(define (fill-context-prefix start end #!optional first-line-regexp)
  ;; Assume that START is at the start of the first line, and END is at the
  ;; end of the last line.
  (let ((first-line-regexp
	 (if (or (default-object? first-line-regexp) (not first-line-regexp))
	     (ref-variable adaptive-fill-first-line-regexp start)
	     first-line-regexp))
	(test-line
	 (lambda (start)
	   (cond ((re-match-forward (ref-variable paragraph-start start) start)
		  #f)
		 ((and (ref-variable adaptive-fill-regexp start)
		       (re-match-forward (ref-variable adaptive-fill-regexp
						       start)
					 start))
		  (extract-string start (re-match-end 0)))
		 ((ref-variable adaptive-fill-procedure start)
		  ((ref-variable adaptive-fill-procedure start) start end))
		 (else #f)))))
    (let ((first-line-prefix (test-line start))
	  (multi-line? (mark< (line-end start 0) end)))
      (and first-line-prefix
	   (if multi-line?
	       ;; If we get a fill prefix from the second line, make sure it
	       ;; or something compatible is on the first line too.
	       (let ((second-line-prefix (test-line (line-start start 1))))
		 (cond ((not second-line-prefix)
			#f)
		       ((re-string-match
			 (string-append (re-quote-string second-line-prefix)
					"\\(\\'\\|[ \t]\\)")
			 first-line-prefix)
			;; If the first line has the second line prefix too,
			;; use it.
			second-line-prefix)
		       ((re-string-match "[ \t]+\\'" second-line-prefix)
			;; If the second line prefix is whitespace, use it.
			second-line-prefix)
		       ((re-string-match
			 (string-append (re-quote-string first-line-prefix)
					"[ \t]*\\'")
			 second-line-prefix)
			;; If the second line has the first line prefix, plus
			;; whitespace, use the part that the first line shares.
			first-line-prefix)
		       (else #f)))
	       ;; If we get a fill prefix from a one-line paragraph, maybe
	       ;; change it to whitespace, and check that it isn't a paragraph
	       ;; starter.
	       (let ((result
		      ;; If first-line-prefix comes from the first line, see
		      ;; if it seems reasonable to use for all lines.  If not,
		      ;; replace it with whitespace.
		      (if (or (and first-line-regexp
				   (re-string-search-forward
				    first-line-regexp
				    first-line-prefix))
			      (fill-prefix-is-comment? first-line-prefix
						       start))
			  first-line-prefix
			  (make-string (string-length first-line-prefix)
				       #\space))))
		 ;; But either way, reject it if it indicates the start of a
		 ;; paragraph when text follows it.
		 (and (not (re-string-match (ref-variable paragraph-start
							  start)
					    (string-append result "a")))
		      result)))))))

(define (fill-prefix-is-comment? prefix mark)
  (let ((locator (ref-variable comment-locator-hook mark)))
    (and locator
	 (call-with-temporary-buffer " adaptive fill"
	   (lambda (buffer)
	     (insert-string prefix (buffer-start buffer))
	     (let ((com (locator (buffer-start buffer))))
	       (and com
		    (within-indentation? (car com))
		    (group-end? (cdr com)))))))))

;;;; Auto Fill

(define-command auto-fill-mode
  "Toggle auto-fill mode.
With argument, turn auto-fill mode on iff argument is positive."
  "P"
  (lambda (argument)
    (let ((mode (ref-mode-object auto-fill)))
      (if (if argument
	      (positive? (command-argument-value argument))
	      (not (current-minor-mode? mode)))
	  (enable-current-minor-mode! mode)
	  (disable-current-minor-mode! mode)))))

(define-minor-mode auto-fill "Fill"
  "Minor mode in which lines are automatically wrapped when long enough.")

(define (auto-fill-break point)
  (and (auto-fill-break? point)
       (let ((prefix
	      (or (and (not
			(ref-variable paragraph-ignore-fill-prefix point))
		       (ref-variable fill-prefix point))
		  (and (ref-variable adaptive-fill-mode point)
		       (fill-context-prefix (or (paragraph-text-start point)
						(line-start point 0))
					    (or (paragraph-text-end point)
						(line-end point 0)))))))
	 (and (re-search-backward "[^ \t][ \t]+"
				  (move-to-column
				   point
				   (+ (ref-variable fill-column) 1))
				  (line-start point 0))
	      (let ((break (re-match-end 0)))
		(and (let ((pe
			    (and prefix
				 (mark+ (line-start point 0)
					(string-length prefix)
					#f))))
		       (or (not pe)
			   (mark> break pe)))
		     (begin
		       (indent-new-comment-line break prefix)
		       #t)))))))

(define (auto-fill-break? point)
  (> (mark-column point) (ref-variable fill-column)))

;;;; Wrap lines

(define (wrap-individual-paragraphs start end fill-column mail?)
  (operate-on-individual-paragraphs start end mail?
    (lambda (p-start p-end fill-prefix)
      (wrap-region-as-paragraph p-start p-end fill-prefix fill-column))))

(define (wrap-region-as-paragraph p-start p-end fill-prefix fill-column)
  (let ((m (mark-left-inserting-copy (line-end p-start 0)))
	(group (mark-group p-start))
	(fp-length (and fill-prefix (string-length fill-prefix)))
	(target-column (fix:+ fill-column 1)))
    (let ((tab-width (group-tab-width group))
	  (image-strings (group-char-image-strings group)))
      (let loop ()
	(delete-horizontal-space m)
	(let inner ()
	  (let* ((index (mark-index m))
		 (ls-index (line-start-index group index))
		 (v
		  (group-column->index group
				       ls-index
				       index
				       0
				       target-column
				       tab-width
				       image-strings)))
	    (if (and (fix:>= (vector-ref v 1) target-column)
		     (fix:<= (vector-ref v 0) index)
		     (re-search-backward "[^ \t][ \t]+"
					 (make-mark group (vector-ref v 0))
					 (make-mark group ls-index)))
		(let ((break (re-match-end 0)))
		  (if (or (not fill-prefix)
			  (fix:> (fix:- (mark-index break) ls-index)
				 fp-length))
		      (begin
			(indent-new-comment-line break fill-prefix)
			(inner)))))))
	(if (mark< m p-end)
	    (begin
	      (move-mark-to! m (line-end m 1 'ERROR))
	      (loop)))))
    (mark-temporary! m)))