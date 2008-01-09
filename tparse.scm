#| -*-Scheme-*-

$Id: tparse.scm,v 1.80 2007/01/05 21:19:24 cph Exp $

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

;;;; Text Parsing


;;;; Pages

(define (%forward-page start end page-delimiter)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (or (re-search-forward page-delimiter start end)
	   end)))

(define (%backward-page end start page-delimiter)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (if (re-search-backward page-delimiter (mark-1+ end) start)
	   (re-match-end 0)
	   start)))

(define (%at-page-delimiter? mark page-delimiter)
  (re-match-forward page-delimiter (line-start mark 0) mark))

(define-variable page-delimiter
  "Regexp describing line-beginnings that separate pages."
  "^\f"
  string?)

(define (forward-one-page mark)
  (%forward-page mark
		 (group-end mark)
		 (mark-local-ref mark (ref-variable-object page-delimiter))))

(define (backward-one-page mark)
  (%backward-page mark
		  (group-start mark)
		  (mark-local-ref mark (ref-variable-object page-delimiter))))

(define (page-start mark)
  (let ((page-delimiter
	 (mark-local-ref mark (ref-variable-object page-delimiter))))
    (or (%at-page-delimiter? mark page-delimiter)
	(%backward-page mark (group-start mark) page-delimiter))))

(define forward-page)
(define backward-page)
(make-motion-pair forward-one-page backward-one-page
  (lambda (f b)
    (set! forward-page f)
    (set! backward-page b)
    unspecific))

;;;; Paragraphs

(define-variable paragraph-start
  "Regexp for beginning of a line that starts OR separates paragraphs."
  "[ \t\n\f]"
  string?)

(define-variable paragraph-separate
  "Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change paragraph-start also."
  "[ \t\f]*$"
  string?)

(define-variable paragraph-ignore-fill-prefix
  "True means the paragraph commands are not affected by fill-prefix.
This is desirable in modes where blank lines are the paragraph delimiters."
  #f
  boolean?)

(define-integrable (mark/paragraph-start mark)
  (mark-local-ref mark (ref-variable-object paragraph-start)))

(define-integrable (mark/paragraph-separate mark)
  (mark-local-ref mark (ref-variable-object paragraph-separate)))

(define (mark/paragraph-fill-prefix mark)
  (if (mark-local-ref mark (ref-variable-object paragraph-ignore-fill-prefix))
      #f
      (mark-local-ref mark (ref-variable-object fill-prefix))))

(define (standard-alternate-paragraph-style! buffer)
  (let ((regexp "[ \t\f]*$"))
    (local-set-variable! paragraph-start regexp buffer)
    (local-set-variable! paragraph-separate regexp buffer)))

(define (paragraph-start? start end)
  (or (re-match-forward (ref-variable paragraph-start start) start end #f)
      (re-match-forward (ref-variable paragraph-separate start) start end #f)))

(define (prefixed-paragraph-start? start end fill-prefix)
  (let ((fp
	 (if fill-prefix
	     (match-forward fill-prefix start end #f)
	     start)))
    (or (not fp)
	(paragraph-start? fp end))))

(define (paragraph-separator? start end)
  (re-match-forward (ref-variable paragraph-separate start) start end #f))

(define (prefixed-paragraph-separator? start end fill-prefix)
  (let ((fp
	 (if fill-prefix
	     (match-forward fill-prefix start end #f)
	     start)))
    (or (not fp)
	(paragraph-separator? fp end))))

(define (forward-one-paragraph mark #!optional limit fill-prefix)
  (let ((limit
	 (if (default-object? limit)
	     (group-end mark)
	     (begin
	       (if (not (mark<= mark limit))
		   (error "Marks incorrectly related:" mark limit))
	       limit)))
	(fill-prefix
	 (if (default-object? fill-prefix)
	     (mark/paragraph-fill-prefix mark)
	     fill-prefix)))
    (and (mark< mark limit)
	 (let ((end (group-end mark))
	       (next-ls
		(lambda (ls)
		  (let ((le (line-end ls 0)))
		    (if (mark< le limit)
			(mark1+ le)
			limit)))))
	   (let ((separator?
		  (lambda (ls)
		    (prefixed-paragraph-separator? ls end fill-prefix))))
	     (letrec
		 ((skip-separators
		   (lambda (ls)
		     (cond ((mark= ls limit) #f)
			   ((separator? ls) (skip-separators (next-ls ls)))
			   (else (skip-body ls)))))
		  (skip-body
		   (lambda (ls)
		     (let ((ls (next-ls ls)))
		       (if (or (mark= ls limit)
			       (prefixed-paragraph-start? ls end fill-prefix))
			   ls
			   (skip-body ls))))))
	       (if (separator? (line-start mark 0))
		   (skip-separators (next-ls mark))
		   (skip-body mark))))))))

(define (backward-one-paragraph mark #!optional limit fill-prefix)
  (let ((limit
	 (if (default-object? limit)
	     (group-start mark)
	     (begin
	       (if (not (mark<= limit mark))
		   (error "Marks incorrectly related:" limit mark))
	       limit)))
	(fill-prefix
	 (if (default-object? fill-prefix)
	     (mark/paragraph-fill-prefix mark)
	     fill-prefix)))
    (let ((prev-ls
	   (lambda (ls)
	     (let ((ls (line-start ls -1 'LIMIT)))
	       (if (mark< ls limit)
		   limit
		   ls))))
	  (end (group-end mark)))
      (let ((separator?
	     (lambda (ls)
	       (prefixed-paragraph-separator? ls end fill-prefix))))
	(letrec ((skip-separators
		  (lambda (ls)
		    (and (mark< limit ls)
			 (let ((ls (prev-ls ls)))
			   (cond ((separator? ls) (skip-separators ls))
				 ((mark= ls limit) ls)
				 (else (skip-body ls)))))))
		 (skip-body
		  (lambda (ls)
		    (if (mark<= ls limit)
			limit
			(let ((ls* (prev-ls ls)))
			  (cond ((separator? ls*) ls*)
				((prefixed-paragraph-start? ls* end
							    fill-prefix)
				 (let ((ls** (prev-ls ls*)))
				   (if (separator? ls**)
				       ls**
				       ls*)))
				(else (skip-body ls*))))))))
	  (and (mark< limit mark)
	       (let ((ls (line-start mark (if (line-start? mark) -1 0))))
		 (and (mark<= limit ls)
		      (cond ((separator? ls) (skip-separators ls))
			    ((mark= limit ls) ls)
			    (else (skip-body ls)))))))))))

(define forward-paragraph)
(define backward-paragraph)
(make-motion-pair forward-one-paragraph backward-one-paragraph
  (lambda (f b)
    (set! forward-paragraph f)
    (set! backward-paragraph b)
    unspecific))

(define (paragraph-text-region mark)
  (let ((end (paragraph-text-end mark)))
    (and end
	 (let ((start (paragraph-text-start end)))
	   (and start
		(make-region start end))))))

(define (paragraph-text-start mark)
  (let ((start (group-start mark))
	(fill-prefix (mark/paragraph-fill-prefix mark)))
    (let ((prev-ls
	   (lambda (ls)
	     (let ((ls (line-start ls -1 'LIMIT)))
	       (if (mark< ls start)
		   start
		   ls))))
	  (end (group-end mark)))
      (let ((separator?
	     (lambda (ls)
	       (prefixed-paragraph-separator? ls end fill-prefix))))
	(letrec ((skip-separators
		  (lambda (ls)
		    (cond ((not (separator? ls)) (skip-body ls))
			  ((mark<= ls start) #f)
			  (else (skip-separators (prev-ls ls))))))
		 (skip-body
		  (lambda (ls)
		    (if (mark<= ls start)
			start
			(let ((ls* (prev-ls ls)))
			  (cond ((separator? ls*) ls)
				((prefixed-paragraph-start? ls* end
							    fill-prefix)
				 ls*)
				(else (skip-body ls*))))))))
	  (skip-separators (line-start mark 0)))))))

(define (paragraph-text-end mark)
  (let ((end (group-end mark))
	(fill-prefix (mark/paragraph-fill-prefix mark)))
    (let ((next-ls
	   (lambda (ls)
	     (let ((le (line-end ls 0)))
	       (if (mark< le end)
		   (mark1+ le)
		   end)))))
      (let ((separator?
	     (lambda (ls)
	       (prefixed-paragraph-separator? ls end fill-prefix))))
	(letrec
	    ((skip-separators
	      (lambda (ls)
		(cond ((mark= ls end) #f)
		      ((separator? ls) (skip-separators (next-ls ls)))
		      (else (skip-body ls)))))
	     (skip-body
	      (lambda (ls)
		(finish
		 (let loop ((ls ls))
		   (let ((ls (next-ls ls)))
		     (if (or (mark= ls end)
			     (prefixed-paragraph-start? ls end fill-prefix))
			 ls
			 (loop ls)))))))
	     (finish
	      (lambda (ls)
		(if (and (mark< mark ls) (line-start? ls))
		    (mark-1+ ls)
		    ls))))
	  (if (separator? (line-start mark 0))
	      (skip-separators (next-ls mark))
	      (skip-body mark)))))))

;;;; Sentences

(define-variable sentence-end
  "Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless."
  "[.?!][]\"')}]*\\($\\|\t\\|  \\)[ \t\n]*"
  string?)

(define-integrable (mark/sentence-end mark)
  (mark-local-ref mark (ref-variable-object sentence-end)))

(define (forward-one-sentence mark)
  (let ((para-end
	 (let loop ((mark mark))
	   (let ((end (paragraph-text-end mark)))
	     (if (or (not end) (mark< mark end))
		 end
		 (and (not (group-end? mark))
		      (loop (mark1+ mark))))))))
    (let ((mark
	   (re-search-forward (mark/sentence-end mark)
			      mark
			      (or para-end (group-end mark)))))
      (if mark
	  (skip-chars-backward " \t\n" mark (re-match-start 0) #f)
	  para-end))))

(define (backward-one-sentence mark)
  (let ((para-start
	 (let loop ((mark mark))
	   (let ((start (paragraph-text-start mark)))
	     (if (or (not start) (mark< start mark))
		 start
		 (and (not (group-start? mark))
		      (loop (mark-1+ mark))))))))
    (if (re-search-backward (string-append (mark/sentence-end mark) "[^ \t\n]")
			    (let ((para-end
				   (and para-start
					(paragraph-text-end para-start))))
			      (if (and para-end (mark< para-end mark))
				  para-end
				  mark))
			    (or para-start (group-start mark)))
	(mark-1+ (re-match-end 0))
	para-start)))

(define forward-sentence)
(define backward-sentence)
(make-motion-pair forward-one-sentence backward-one-sentence
  (lambda (f b)
    (set! forward-sentence f)
    (set! backward-sentence b)
    unspecific))