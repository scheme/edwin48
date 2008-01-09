#| -*-Scheme-*-

$Id: regexp.scm,v 1.86 2007/01/05 21:19:24 cph Exp $

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

;;;; Regular Expressions


(define hash-of-false (object-hash #f))
(define match-group hash-of-false)

(define (re-match-start i)
  (let ((index (re-match-start-index i)))
    (and index
	 (make-mark (re-match-group) index))))

(define (re-match-end i)
  (let ((index (re-match-end-index i)))
    (and index
	 (make-mark (re-match-group) index))))

(define (re-match-extract-string i)
  (group-extract-string (re-match-group)
			(or (re-match-start-index i)
			    (error "No such register:" i))
			(re-match-end-index i)))

(define (re-match-group)
  (let ((group (object-unhash match-group)))
    (if (not group)
	(error "No match group"))
    group))

(define (re-match-data)
  (let ((group (object-unhash match-group)))
    (cons group
	  (if group
	      (let ((v (make-vector 20)))
		(do ((i 0 (fix:+ i 1)))
		    ((fix:= i 20))
		  (vector-set!
		   v i
		   (let ((index (vector-ref registers i)))
		     (and index
			  ;; Start marks are right-inserting,
			  ;; end marks are left-inserting.
			  (make-permanent-mark group index (fix:>= i 10))))))
		v)
	      (vector-copy registers)))))

(define (set-re-match-data! data)
  (let ((group (car data))
	(marks (cdr data)))
    (if group
	(begin
	  (set! match-group (group-hash-number group))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i 20))
	    (vector-set! registers i
			 (let ((mark (vector-ref marks i)))
			   (and mark
				(let ((index (mark-index mark)))
				  (mark-temporary! mark)
				  index))))))
	(begin
	  (set! match-group hash-of-false)
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i 20))
	    (vector-set! registers i (vector-ref marks i)))))))

(define (preserving-match-data thunk)
  (let ((data unspecific))
    (dynamic-wind (lambda () (set! data (re-match-data)) unspecific)
		  thunk
		  (lambda () (set-re-match-data! data)))))

(define (replace-match replacement #!optional preserve-case? literal?)
  (let ((start (re-match-start 0))
	(end (mark-left-inserting-copy (re-match-end 0))))
    (let ((replacement
	   (let ((replacement
		  (if (and (not (default-object? literal?)) literal?)
		      replacement
		      (re-substitute-registers replacement))))
	     (if (and (not (default-object? preserve-case?)) preserve-case?)
		 ;; Emacs uses a more complicated algorithm here,
		 ;; which breaks the replaced string into words,
		 ;; makes the decision based on examining all the
		 ;; words, and then changes each word in the
		 ;; replacement to match the pattern.
		 (let ((replaced (extract-string start end)))
		   (cond ((string-capitalized? replaced)
			  (string-capitalize replacement))
			 ((string-upper-case? replaced)
			  (string-upcase replacement))
			 (else replacement)))
		 replacement))))
      (delete-string start end)
      (insert-string replacement start))
    (mark-temporary! end)
    end))

(define (re-substitute-registers pattern)
  (let ((end (string-length pattern)))
    (if (substring-find-next-char pattern 0 end #\\)
	(apply
	 string-append
	 (let loop ((start 0))
	   (let ((slash (substring-find-next-char pattern start end #\\)))
	     (if slash
		 (cons (substring pattern start slash)
		       (let ((next (+ slash 1)))
			 (cons (let ((char
				      (if (< next end)
					  (string-ref pattern next)
					  #\\)))
				 (let ((n
					(if (char=? #\& char)
					    0
					    (char->digit char))))
				   (cond ((not n)
					  (string char))
					 ((not (re-match-start-index n))
					  (error "No match for register" n))
					 (else
					  (extract-string
					   (re-match-start n)
					   (re-match-end n))))))
			       (if (< next end)
				   (loop (+ next 1))
				   '()))))
		 (list (substring pattern start end))))))
	pattern)))

(define (delete-match)
  (let ((group (re-match-group))
	(start (re-match-start-index 0)))
    (group-delete! group start (re-match-end-index 0))
    (make-mark group start)))

(define (search-forward string start #!optional end case-fold-search)
  (%re-search string start (default-end-mark start end)
	      (default-case-fold-search case-fold-search start)
	      re-compile-string
	      re-search-buffer-forward))

(define (search-backward string end #!optional start case-fold-search)
  (%re-search string (default-start-mark start end) end
	      (default-case-fold-search case-fold-search end)
	      re-compile-string
	      re-search-buffer-backward))

(define (re-search-forward regexp start #!optional end case-fold-search)
  (%re-search regexp start (default-end-mark start end)
	      (default-case-fold-search case-fold-search start)
	      re-compile-pattern
	      re-search-buffer-forward))

(define (re-search-backward regexp end #!optional start case-fold-search)
  (%re-search regexp (default-start-mark start end) end
	      (default-case-fold-search case-fold-search end)
	      re-compile-pattern
	      re-search-buffer-backward))

(define (%re-search string start end case-fold-search compile-string search)
  (let ((group (mark-group start)))
    (let ((index
	   (search (if (compiled-regexp? string)
		       string
		       (compile-string string case-fold-search))
		   (group-syntax-table group)
		   group
		   (mark-index start)
		   (mark-index end))))
      (and index
	   (make-mark group index)))))

(define (re-match-forward regexp start #!optional end case-fold-search)
  (let ((end (default-end-mark start end))
	(case-fold-search (default-case-fold-search case-fold-search start))
	(group (mark-group start)))
    (let ((index
	   (re-match-buffer-forward (if (compiled-regexp? regexp)
					regexp
					(re-compile-pattern regexp
							    case-fold-search))
				    (group-syntax-table group)
				    group
				    (mark-index start)
				    (mark-index end))))
      (and index
	   (make-mark group index)))))

(define (re-search-buffer-forward regexp syntax-table group start end)
  (and (fix:< start end)
       (let ((index
	      ((ucode-primitive re-search-buffer-forward)
	       (compiled-regexp/byte-stream regexp)
	       (compiled-regexp/translation-table regexp)
	       (syntax-table-argument syntax-table)
	       registers group start end)))
	 (set! match-group (compute-match-group group index))
	 index)))

(define (re-search-buffer-backward regexp syntax-table group start end)
  (and (fix:< start end)
       (let ((index
	      ((ucode-primitive re-search-buffer-backward)
	       (compiled-regexp/byte-stream regexp)
	       (compiled-regexp/translation-table regexp)
	       (syntax-table-argument syntax-table)
	       registers group start end)))
	 (set! match-group (compute-match-group group index))
	 index)))

(define (re-match-buffer-forward regexp syntax-table group start end)
  (let ((index
	 ((ucode-primitive re-match-buffer)
	  (compiled-regexp/byte-stream regexp)
	  (compiled-regexp/translation-table regexp)
	  (syntax-table-argument syntax-table)
	  registers group start end)))
    (set! match-group (compute-match-group group index))
    index))

(define-integrable (syntax-table-argument syntax-table)
  (char-syntax-table/entries (or syntax-table standard-char-syntax-table)))

(define (compute-match-group group index)
  (if index
      (group-hash-number group)
      hash-of-false))