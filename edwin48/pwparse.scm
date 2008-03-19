#| -*-Scheme-*-

$Id: pwparse.scm,v 1.7 2008/01/30 20:02:04 cph Exp $

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

;;;; Password-Database Parser

;;; This program implements I/O for a text-format password database.


(define (read-pw-forms port)
  (parse/neutral port '()))

(define (parse/neutral port forms)
  (let ((line (read-line port)))
    (if (eof-object? line)
	(finish-parsing forms)
	(dispatch/neutral port line forms))))

(define (finish-parsing forms)
  (reverse! forms))

(define (dispatch/neutral port line forms)
  ((dispatch line
	     parse-neutral/blank
	     parse-neutral/comment
	     parse-neutral/short-form
	     parse-neutral/long-form)
   port line forms))

(define (dispatch line
		  parse-blank
		  parse-comment
		  parse-short-form
		  parse-long-form)
  (let ((start (string-index line char-set:not-whitespace))
	(end (string-length line)))
    (cond ((not start)
	   parse-blank)
	  ((char=? #\; (string-ref line start))
	   parse-comment)
	  (else
	   (let ((colon (string-index-right line #\: start end)))
	     (if colon
		 (if (string-index line char-set:not-whitespace
				   (+ colon 1) end)
		     parse-short-form
		     parse-long-form)
		 parse-long-form))))))

(define (comment-line? line)
  (let ((start (string-index line char-set:not-whitespace)))
    (and start
	 (char=? #\; (string-ref line start)))))

(define (long-form-separator-line? line)
  ;; blank
  (not (string-index line char-set:not-whitespace)))

(define (split-colon-line line)
  (let ((colon (string-index line #\:)))
    (if colon
	(cons (string-trim (string-head line colon))
	      (string-trim (string-tail line (+ colon 1))))
	(strip-semicolons line))))

(define strip-semicolons
  (let ((char-set (char-set-invert (char-set #\;))))
    (lambda (line)
      (string-trim-left (string-trim line) char-set))))

(define (parse-neutral/blank port line forms)
  line
  (parse/neutral port (cons '(BLANK) forms)))

(define (parse-neutral/comment port line forms)
  (let ((finish-comment
	 (lambda (accumulator)
	   (cons (cons 'COMMENT (reverse! (map strip-semicolons accumulator)))
		 forms))))
    (let loop ((accumulator (list line)))
      (let ((line (read-line port)))
	(cond ((eof-object? line)
	       (finish-parsing (finish-comment accumulator)))
	      ((comment-line? line)
	       (loop (cons line accumulator)))
	      (else
	       (dispatch/neutral port
				 line
				 (finish-comment accumulator))))))))

(define (parse-neutral/short-form port line forms)
  (parse/neutral port (cons (cons 'SHORT (split-colon-line line)) forms)))

(define (parse-neutral/long-form port line forms)
  (let* ((header
	  (string-trim
	   (let ((colon (string-index-right line #\:)))
	     (if colon
		 (string-head line colon)
		 line))))
	 (finish-long-form
	  (lambda (accumulator)
	    (cons (cons* 'LONG
			 header
			 (reverse! (map split-colon-line accumulator)))
		  forms))))
    (let loop ((accumulator '()))
      (let ((line (read-line port)))
	(cond ((eof-object? line)
	       (finish-parsing (finish-long-form accumulator)))
	      ((long-form-separator-line? line)
	       (dispatch/neutral port
				 line
				 (finish-long-form accumulator)))
	      (else
	       (loop (cons line accumulator))))))))

(define (write-pw-forms forms port)
  (let ((write-two-part
	 (lambda (line)
	   (write-string (car line) port)
	   (write-char #\: port)
	   (let ((n
		  (+ (string-length (car line))
		     1)))
	     (if (< n 8)
		 (write-string "\t\t" port)
		 (write-char (if (< n 16) #\tab #\space) port)))
	   (write-string (cdr line) port))))
    (for-each (lambda (form)
		(let ((type (car form))
		      (body (cdr form)))
		  (case type
		    ((BLANK)
		     (newline port))
		    ((COMMENT)
		     (for-each (lambda (line)
				 (write-char #\; port)
				 (write-string line port)
				 (newline port))
			       body))
		    ((SHORT)
		     (write-two-part body)
		     (newline port))
		    ((LONG)
		     (write-string (car body) port)
		     (write-char #\: port)
		     (newline port)
		     (for-each (lambda (line)
				 (if (pair? line)
				     (write-two-part line)
				     (begin
				       (write-char #\; port)
				       (write-string line port)))
				 (newline port))
			       (cdr body)))
		    (else
		     (error "Illegal form type:" form)))))
	      forms)))