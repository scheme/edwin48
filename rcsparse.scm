#| -*-Scheme-*-

$Id: rcsparse.scm,v 1.8 2007/01/05 21:19:24 cph Exp $

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

;;;; RCS Parser


(define (parse-rcs-admin filename)
  (call-with-input-file filename
    (lambda (port)
      (let* ((line-port (make-line-port port))
	     (head
	      (let ((head (parse-required line-port "head" '(NUM))))
		(and (not (null? (cdr head)))
		     (rcs-num-string (cadr head)))))
	     (branch (parse-optional line-port "branch" '(NUM)))
	     (access-list (parse-required line-port "access" '(* ID)))
	     (symbols (parse-required line-port "symbols" '(* ID COLON NUM)))
	     (locks (parse-required line-port "locks" '(* ID COLON NUM)))
	     (strict (parse-optional line-port "strict" '()))
	     (comment (parse-optional line-port "comment" '(? STRING)))
	     (expand (parse-optional line-port "expand" '(? STRING)))
	     (deltas (parse-deltas line-port))
	     (desc (parse-desc line-port))
	     (num->delta (make-delta-map deltas))
	     (rcs-id-alist
	      (lambda (syms)
		(let loop ((syms syms))
		  (if (null? syms)
		      '()
		      (cons (cons (rcs-id-string (car syms))
				  (num->delta (rcs-num-string (caddr syms))))
			    (loop (cdddr syms))))))))
	(make-rcs-admin (and head (num->delta head))
			(and branch
			     (not (null? (cdr branch)))
			     (rcs-num-string (cadr branch)))
			(map rcs-id-string (cdr access-list))
			(rcs-id-alist (cdr symbols))
			(rcs-id-alist (cdr locks))
			(and strict #t)
			(and comment
			     (not (null? (cdr comment)))
			     (rcs-string-contents (cadr comment)))
			(and expand
			     (not (null? (cdr expand)))
			     (rcs-string-contents (cadr expand)))
			desc)))))

(define-structure (rcs-admin (conc-name rcs-admin/))
  (head #f read-only #t)
  (branch #f read-only #t)
  (access-list #f read-only #t)
  (symbols #f read-only #t)
  (locks #f read-only #t)
  (strict? #f read-only #t)
  (comment #f read-only #t)
  (expand #f read-only #t)
  (description #f read-only #t))

(define (parse-required line-port head tail)
  (let ((line (line-read line-port)))
    (if (not (and (rcs-match head (car line))
		  (rcs-match tail (cdr line))))
	(error "Ill-formed RCS file:" head tail line))
    line))

(define (parse-optional line-port head tail)
  (let ((line (line-peek line-port)))
    (and line
	 (rcs-match head (car line))
	 (begin
	   (line-discard line-port)
	   (if (not (rcs-match tail (cdr line)))
	       (error "Ill-formed RCS file:" head tail line))
	   line))))

(define (make-delta-map deltas)
  (let ((table (make-string-hash-table)))
    (for-each (lambda (delta)
		(let ((key (vector-ref delta 0)))
		  (let ((entry (hash-table/get table key #f)))
		    (if entry
			(error "duplicate delta entry" delta entry)))
		  (hash-table/put! table key
				   (make-rcs-delta key
						   (vector-ref delta 1)
						   (vector-ref delta 2)
						   (vector-ref delta 3)
						   (vector-ref delta 4)
						   (vector-ref delta 5)))))
	      deltas)
    (let ((num->delta
	   (lambda (key)
	     (let ((delta (hash-table/get table key #f)))
	       (if (not delta)
		   (error "unknown delta number" key))
	       delta))))
      (hash-table/for-each table
	(lambda (key delta)
	  key
	  (do ((branches (rcs-delta/branches delta) (cdr branches)))
	      ((null? branches))
	    (set-car! branches (num->delta (car branches))))
	  (let ((next (rcs-delta/next delta)))
	    (if next
		(set-rcs-delta/next! delta (num->delta next))))))
      num->delta)))

(define-structure (rcs-delta (conc-name rcs-delta/))
  (number #f read-only #t)
  (date #f read-only #t)
  (author #f read-only #t)
  (state #f read-only #t)
  (branches #f read-only #t)
  next)

(define (parse-deltas line-port)
  (discard-newphrases line-port)
  (let ((delta (parse-delta line-port)))
    (if delta
	(cons delta (parse-deltas line-port))
	'())))

(define (parse-delta line-port)
  (let ((number (parse-optional line-port 'num '())))
    (and number
	 (let* ((date (parse-required line-port "date" '(num)))
		(author (parse-required line-port "author" '(id)))
		(state (parse-required line-port "state" '(? id)))
		(branches (parse-required line-port "branches" '(* num)))
		(next (parse-required line-port "next" '(? num))))
	   (discard-newphrases line-port)
	   (vector (rcs-num-string (car number))
		   (number->integer-list (rcs-num-string (cadr date)))
		   (rcs-id-string (cadr author))
		   (and (not (null? (cdr state)))
			(rcs-id-string (cadr state)))
		   (map rcs-num-string (cdr branches))
		   (and (not (null? (cdr next)))
			(rcs-num-string (cadr next))))))))

(define (number->integer-list string)
  (let ((end (string-length string)))
    (let loop ((start 0) (index 0))
      (cond ((= index end)
	     (if (= start end)
		 (error "Trailing decimal in number"))
	     (list (string->number (substring string start end))))
	    ((char=? #\. (string-ref string index))
	     (cons (string->number (substring string start index))
		   (let ((start (1+ index)))
		     (loop start start))))
	    (else
	     (loop start (1+ index)))))))

(define (parse-desc line-port)
  (rcs-string-contents (cadr (parse-required line-port "desc" '(string)))))

(define (discard-newphrases line-port)
  (let ((line (line-peek line-port)))
    (if (and line
	     (rcs-match 'id (car line))
	     (not (string=? "desc" (rcs-id-string (car line))))
	     (rcs-match '(* word) (cdr line)))
	(begin
	  (line-discard line-port)
	  (discard-newphrases line-port)))))

;;;; Delta Search

(define (rcs-find-delta admin number error?)
  (if number
      (let ((n-fields (rcs-number-length number))
	    (head (rcs-admin/head admin)))
	(if (fix:= n-fields 1)
	    (let loop ((delta head))
	      (if delta
		  (if (string-prefix? number (rcs-delta/number delta))
		      delta
		      (loop (rcs-delta/next delta)))
		  (and error?
		       (error:bad-range-argument number 'RCS-FIND-DELTA))))
	    (let loop ((branch head) (i 1))
	      (let* ((i (fix:+ i 1))
		     (delta
		      (find-revision branch
				     (rcs-number-head number i error?)
				     error?)))
		(and delta
		     (if (fix:= n-fields i)
			 delta
			 (let* ((i (fix:+ i 1))
				(branch
				 (find-branch delta
					      (rcs-number-head number i error?)
					      error?)))
			   (and branch
				(if (fix:= n-fields i)
				    (last-revision branch)
				    (loop branch i))))))))))
      (if (rcs-admin/branch admin)
	  (rcs-find-delta admin (rcs-admin/branch admin) error?)
	  (rcs-admin/head admin))))

(define (last-revision delta)
  (if (rcs-delta/next delta)
      (last-revision (rcs-delta/next delta))
      delta))

(define (find-revision delta number error?)
  (and number
       (let loop ((delta delta))
	 (if delta
	     (if (string=? number (rcs-delta/number delta))
		 delta
		 (loop (rcs-delta/next delta)))
	     (and error?
		  (error:bad-range-argument number 'RCS-FIND-DELTA))))))

(define (find-branch delta number error?)
  (and number
       (let loop ((branches (rcs-delta/branches delta)))
	 (if (not (null? branches))
	     (if (string-prefix? number (rcs-delta/number (car branches)))
		 (car branches)
		 (loop (cdr branches)))
	     (and error?
		  (error:bad-range-argument number 'RCS-FIND-DELTA))))))

(define (rcs-number-head number n-fields error?)
  (let ((end (string-length number)))
    (let loop ((i 0) (n-fields n-fields))
      (if (fix:= i end)
	  (if (fix:> n-fields 1)
	      (and error?
		   (error:bad-range-argument n-fields 'RCS-FIND-DELTA))
	      number)
	  (let ((i* (fix:+ i 1)))
	    (if (char=? #\. (string-ref number i))
		(let ((n-fields (fix:- n-fields 1)))
		  (if (fix:= n-fields 0)
		      (if (fix:= i* end)
			  number
			  (string-head number i))
		      (loop i* n-fields)))
		(loop i* n-fields)))))))

(define (rcs-number-length number)
  (let ((end (string-length number)))
    (do ((i 0 (fix:+ i 1))
	 (n-fields 1
		   (if (char=? #\. (string-ref number i))
		       (fix:+ n-fields 1)
		       n-fields)))
	((fix:= i end) n-fields))))

;;;; Matcher for Tokenized Input

(define (rcs-match pattern instance)
  (cond ((string? pattern)
	 (and (rcs-id? instance)
	      (string=? pattern (rcs-id-string instance))))
	((symbol? pattern)
	 (case pattern
	   ((ID) (rcs-id? instance))
	   ((STRING) (rcs-string? instance))
	   ((NUM) (rcs-num? instance))
	   ((COLON) (rcs-colon? instance))
	   ((SEMICOLON) (rcs-semicolon? instance))
	   (else (error "Ill-formed pattern:" pattern))))
	((null? pattern)
	 (null? instance))
	((list? pattern)
	 (case (car pattern)
	   ((?)
	    (or (null? instance)
		(rcs-match-list (cdr pattern) instance null?)))
	   ((*)
	    (let loop ((instance instance))
	      (or (null? instance)
		  (rcs-match-list (cdr pattern) instance loop))))
	   ((+)
	    (letrec
		((loop
		  (lambda (instance)
		    (or (null? instance)
			(rcs-match-list (cdr pattern) instance loop)))))
	      (rcs-match-list (cdr pattern) instance loop)))
	   (else
	    (rcs-match-list pattern instance null?))))
	(else
	 (error "Ill-formed pattern:" pattern))))

(define (rcs-match-list pattern instance if-match)
  (let loop ((pattern pattern) (instance instance))
    (if (null? pattern)
	(if-match instance)
	(and (pair? instance)
	     (rcs-match (car pattern) (car instance))
	     (loop (cdr pattern) (cdr instance))))))

;;;; Tokenizer

(define (make-line-port port)
  (cons 'EMPTY port))

(define (line-peek line-port)
  (if (eq? 'EMPTY (car line-port))
      (set-car! line-port (parse-line (cdr line-port))))
  (car line-port))

(define (line-discard line-port)
  (if (car line-port)
      (set-car! line-port 'EMPTY)))

(define (line-read line-port)
  (let ((line (line-peek line-port)))
    (line-discard line-port)
    line))

(define (parse-line port)
  (let ((word (parse-word port)))
    (and word
	 (cond ((rcs-id? word)
		(if (let ((string (rcs-id-string word)))
		      (or (string=? "desc" string)
			  (string=? "log" string)
			  (string=? "text" string)))
		    (let ((string (parse-word port)))
		      (if (not (rcs-string? string))
			  (error "Illegal word sequence:" word string))
		      (list word string))
		    (cons word
			  (let loop ()
			    (let ((word (parse-word port)))
			      (if (rcs-semicolon? word)
				  '()
				  (cons word (loop))))))))
	       ((rcs-num? word)
		(list word))
	       (else
		(error "Illegal line-starting word:" word))))))

(define parse-word
  (let ((delimiters
	 (char-set-invert
	  (char-set-union (ascii-range->char-set #o010 #o016)
			  (ascii-range->char-set #o040 #o041)))))
    (lambda (port)
      (input-port/discard-chars port delimiters)
      (let ((char (input-port/peek-char port)))
	(and (not (eof-object? char))
	     ((vector-ref parse-word/dispatch-table (char->integer char))
	      port))))))

(define parse-string
  (let ((delimiters (char-set #\@)))
    (lambda (port)
      (input-port/discard-char port)
      (let ((strings
	     (let loop ()
	       (let ((head (input-port/read-string port delimiters)))
		 (if (eof-object? (input-port/peek-char port))
		     (error "End of file while reading string."))
		 (input-port/discard-char port)
		 (if (char=? #\@ (input-port/peek-char port))
		     (begin
		       (input-port/discard-char port)
		       (cons head (cons "@" (loop))))
		     (list head))))))
	(make-rcs-string
	 (if (null? (cdr strings))
	     (car strings)
	     (apply string-append strings)))))))

(define parse-id
  (let ((delimiters
	 (char-set-invert
	  (char-set-difference
	   (char-set-union (ascii-range->char-set #o041 #o177)
			   (ascii-range->char-set #o240 #o400))
	   (char-set #\$ #\, #\. #\: #\; #\@)))))
    (lambda (port)
      (make-rcs-id (input-port/read-string port delimiters)))))

(define parse-num
  (let ((delimiters
	 (char-set-invert (char-set-union char-set:numeric (char-set #\.)))))
    (lambda (port)
      (make-rcs-num (input-port/read-string port delimiters)))))

(define (parse-colon port)
  (input-port/discard-char port)
  (make-rcs-colon))

(define (parse-semicolon port)
  (input-port/discard-char port)
  (make-rcs-semicolon))

(define parse-word/dispatch-table
  (let ((table
	 (make-vector 256
		      (lambda (port)
			port
			(error "Illegal word-starting character.")))))
    (subvector-fill! table #o101 #o133 parse-id)
    (subvector-fill! table #o141 #o173 parse-id)
    (subvector-fill! table #o300 #o327 parse-id)
    (subvector-fill! table #o330 #o366 parse-id)
    (subvector-fill! table #o370 #o400 parse-id)
    (subvector-fill! table #o060 #o072 parse-num)
    (vector-set! table (char->integer #\@) parse-string)
    (vector-set! table (char->integer #\:) parse-colon)
    (vector-set! table (char->integer #\;) parse-semicolon)
    table))

;;;; Tokens

(define-integrable (make-rcs-id string)
  (cons 'IDENTIFIER string))

(define (rcs-id? word)
  (and (pair? word)
       (eq? 'IDENTIFIER (car word))))

(define-integrable (rcs-id-string rcs-id)
  (cdr rcs-id))

(define-integrable (make-rcs-string contents)
  (cons 'STRING contents))

(define (rcs-string? word)
  (and (pair? word)
       (eq? 'STRING (car word))))

(define-integrable (rcs-string-contents rcs-string)
  (cdr rcs-string))

(define-integrable (make-rcs-num string)
  (cons 'NUMBER string))

(define (rcs-num? word)
  (and (pair? word)
       (eq? 'NUMBER (car word))))

(define-integrable (rcs-num-string rcs-num)
  (cdr rcs-num))

(define-integrable (make-rcs-colon)
  '(COLON))

(define (rcs-colon? word)
  (and (pair? word)
       (eq? 'COLON (car word))))

(define-integrable (make-rcs-semicolon)
  '(SEMICOLON))

(define (rcs-semicolon? word)
  (and (pair? word)
       (eq? 'SEMICOLON (car word))))