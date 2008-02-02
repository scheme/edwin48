#| -*-Scheme-*-

$Id: dabbrev.scm,v 1.10 2008/01/30 20:01:59 cph Exp $

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

;;;; Dynamic Abbrev


(define-variable dabbrevs-backward-only
  "If true, dabbrevs-expand only looks backwards."
  #f
  boolean?)

(define-variable-per-buffer dabbrevs-limit
  "Limits region searched by dabbrevs-expand to that many chars away (local)."
  #f
  (lambda (object)
    (or (not object)
	(exact-integer? object))))

(define-variable-per-buffer last-dabbrev-table
  "Table of expansions seen so far (local)."
  '())

(define-variable-per-buffer last-dabbrevs-abbreviation
  "Last string we tried to expand (local)."
  ""
  string?)

(define-variable-per-buffer last-dabbrevs-direction
  "Direction of last dabbrevs search (local)."
  0
  exact-integer?)

(define-variable-per-buffer last-dabbrevs-abbrev-location
  "Location last abbreviation began (local)."
  #f)

(define-variable-per-buffer last-dabbrevs-expansion
  "Last expansion of an abbreviation (local)."
  #f)

(define-variable-per-buffer last-dabbrevs-expansion-location
  "Location the last expansion was found (local)."
  #f)

(define dabbrev-tag "Dabbrev")

(define-command dabbrev-expand
  "Expand previous word \"dynamically\".
Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are considered.

A positive prefix argument, N, says to take the Nth backward DISTINCT
possibility.  A negative argument says search forward.  The variable
dabbrev-backward-only may be used to limit the direction of search to
backward if set non-nil.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried."
  "*P"
  (lambda (arg)
    (define (do-abbrev loc abbrev old which)
      (let ((do-case (and (ref-variable case-fold-search)
			  (ref-variable case-replace)))
	    (pattern (string-append "\\b"
				    (re-quote-string abbrev)
				    "\\(\\sw\\|\\s_\\)+")))

	(define (search&setup-table count direction)
	  (let loop ((n count)
		     (expansion #f)
		     (start
		      (or (ref-variable last-dabbrevs-expansion-location)
			  (current-point))))
	    (if (zero? n)
		(values (mark-permanent-copy start) expansion)
		(with-values
		    (lambda ()
		      (dabbrevs-search start pattern direction do-case))
		  (lambda (loc expansion)
		    (if (not expansion)
			(values #f #f)
			(begin
			  (set-variable!
			   last-dabbrev-table
			   (cons expansion
				 (ref-variable last-dabbrev-table)))
			  (loop (-1+ n) expansion loc))))))))

	(define (step3 loc expansion)
	  (if (not expansion)
	      (let ((first (string=? abbrev old)))
		(set-variable! last-dabbrevs-abbrev-location #f)
		(if (not first)
		    (let* ((end (current-point))
			   (start (mark- end (string-length old))))
		      (delete-string start end)
		      (insert-string abbrev start)))
		(editor-error (if first
				  "No dynamic expansion found for "
				  "No further dynamic expansions found for ")
			      abbrev))
	      ;; Success: stick it in and return.
	      ;; Make case of replacement conform to case of abbreviation
	      ;; provided (1) that kind of thing is enabled in this buffer
	      ;; and (2) the replacement itself is all lower case
	      ;; except perhaps for the first character.
	      (let ((place (search-backward old
					    (current-point)
					    (buffer-start (current-buffer))))
		    (do-case (and do-case
				  (substring-lower-case?
				   expansion
				   1
				   (string-length expansion)))))
		;; First put back the original abbreviation with its original
		;; case pattern.
		(replace-match abbrev #f #t)
		(search-forward abbrev
				place
				(buffer-end (current-buffer)))
		(replace-match (if do-case
				   (string-downcase expansion)
				   expansion)
			       do-case
			       #t)
		;; Save state for re-expand.
		(set-variable! last-dabbrevs-abbreviation abbrev)
		(set-variable! last-dabbrevs-expansion expansion)
		(set-variable! last-dabbrevs-expansion-location loc)
		;; Chain invocations
		(set-command-message! dabbrev-tag))))

	(define (step2 loc expansion)
	  (if (or expansion (> which 0))
	      (step3 loc expansion)
	      ;; Look forward
	      (with-values (lambda ()
			     (search&setup-table (max 1 (- which)) #f))
		(lambda (loc expansion)
		  (set-variable! last-dabbrevs-direction -1)
		  (step3 loc expansion)))))

	;; Try looking backward unless inhibited.
	(if (< which 0)
	    (step2 loc #f)
	    (with-values (lambda ()
			   (search&setup-table (max 1 which) #t))
	      (lambda (loc expansion)
		(if (not expansion)
		    (set-variable! last-dabbrevs-expansion-location
				   #f))
		(set-variable! last-dabbrevs-direction (min 1 which))
		(step2 loc expansion))))))

    (define (do-from-scratch)
      (let* ((loc (current-point))
	     (start (backward-word loc 1 'ERROR))
	     (abbrev (extract-string start loc)))
	(set-variable! last-dabbrevs-abbrev-location start)
	(set-variable! last-dabbrevs-expansion-location #f)
	(set-variable! last-dabbrev-table '())
	(do-abbrev loc
		   abbrev
		   abbrev
		   (cond (arg
			  (command-argument-value arg))
			 ((ref-variable dabbrevs-backward-only)
			  1)
			 (else
			  0)))))

    (if (and (not arg)
	     (command-message-receive dabbrev-tag
				      (lambda () #t)
				      (lambda () #f))
	     (ref-variable last-dabbrevs-abbrev-location))
	(do-abbrev #f
		   (ref-variable last-dabbrevs-abbreviation)
		   (ref-variable last-dabbrevs-expansion)
		   (ref-variable last-dabbrevs-direction))
	(do-from-scratch))))

;; Search function used by dabbrevs library.  
;; pattern is string to find as prefix of word.
;; reverse? is true for reverse search, #f for forward.
;; Variable abbrevs-limit controls the maximum search region size.

;; Table of expansions already seen is examined in buffer last-dabbrev-table,
;; so that only distinct possibilities are found by dabbrevs-re-expand.
;; Note that to prevent finding the abbrev itself it must have been
;; entered in the table.

;; Values are #f if no expansion found.
;; After a succesful search, values are a mark right after the expansion,
;; and the expansion itself.

(define (dabbrevs-search start pattern reverse? do-case)
  ;; (values loc expansion)
  (let ((limit (let ((limit (ref-variable dabbrevs-limit)))
		 (if limit
		     ((if reverse? mark- mark+)
		      start
		      limit)
		     ((if reverse? buffer-start buffer-end)
		      (current-buffer))))))
    (let loop ((posn start))
      (if (not ((if reverse? re-search-backward re-search-forward)
		pattern posn limit))
	  (values #f #f)
	  (let ((start (re-match-start 0))
		(end (re-match-end 0)))
	    (let* ((result (extract-string start end))
		   (test (if do-case
			     (string-downcase result)
			     result)))
	      (if (any (lambda (example)
			 (string=? test
				   (if do-case
				       (string-downcase example)
				       example)))
		       (ref-variable last-dabbrev-table))
		  (loop (if reverse? start end))
		  (values end result))))))))