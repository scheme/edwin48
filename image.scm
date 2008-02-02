#| -*-Scheme-*-

$Id: image.scm,v 1.144 2008/01/30 20:02:02 cph Exp $

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

;;;; Display Imaging


(define (group-columns group start end column tab-width char-image-strings)
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-columns text start end column
			      tab-width char-image-strings))
	  ((fix:<= gap-start start)
	   (substring-columns text
			      (fix:+ start gap-length)
			      (fix:+ end gap-length)
			      column
			      tab-width
			      char-image-strings))
	  (else
	   (substring-columns text
			      gap-end
			      (fix:+ end gap-length)
			      (substring-columns text start gap-start
						 column tab-width
						 char-image-strings)
			      tab-width
			      char-image-strings)))))

(define (string-columns string column tab-width char-image-strings)
  (substring-columns string 0 (string-length string) column tab-width
		     char-image-strings))

(define (substring-columns string start end column tab-width
			   char-image-strings)
  (if tab-width
      (do ((index start (fix:+ index 1))
	   (column column
		   (fix:+ column
			  (let ((char (string-ref string index)))
			    (if (char=? char #\tab)
				(fix:- tab-width
				       (fix:remainder column tab-width))
				(string-length
				 (vector-ref char-image-strings
					     (char->integer char))))))))
	  ((fix:= index end) column))
      (do ((index start (fix:+ index 1))
	   (column column
		   (fix:+ column
			  (string-length
			   (vector-ref char-image-strings
				       (char->integer
					(string-ref string index)))))))
	  ((fix:= index end) column))))

(define default-char-image-strings/original-emacs
  (let ((strings (make-vector 256)))
    (do ((i #x00 (+ i 1)))
	((= #x20 i))
      (vector-set! strings i (string #\^ (integer->char (+ #x40 i)))))
    (do ((i #x20 (+ i 1)))
	((= #x7f i))
      (vector-set! strings i (string (integer->char i))))
    (vector-set! strings #x7f "^?")
    (do ((i #x80 (+ i 1)))
	((= #x100 i))
      (vector-set! strings i (string-append "\\" (number->string i 8))))
    strings))

(define default-char-image-strings/ansi
  (let ((strings (vector-copy default-char-image-strings/original-emacs)))
    (do ((i #x91 (+ i 1)))
	((= #x93 i))
      (vector-set! strings i (string (integer->char i))))
    (do ((i #xA0 (+ i 1)))
	((= #x100 i))
      (vector-set! strings i (string (integer->char i))))
    strings))

(define default-char-image-strings/ascii
  (let ((strings (vector-copy default-char-image-strings/original-emacs)))
    (subvector-move-left!
     '#("[NUL]" "[SOH]" "[STX]" "[ETX]" "[EOT]" "[ENQ]" "[ACK]" "[BEL]"
	"[BS]"  "[HT]"  "[NL]"  "[VT]" "[FF]" "[CR]"  "[SO]"  "[SI]"
	"[DLE]" "[DC1]" "[DC2]" "[DC3]" "[DC4]" "[NAK]" "[SYN]" "[ETB]"
	"[CAN]" "[EM]"  "[SUB]" "[ESC]" "[FS]"  "[GS]"  "[RS]"  "[US]")
     0 #x20 strings 0)
    strings))

(define default-char-image-strings default-char-image-strings/ansi)

(define (group-line-columns group start end column
			    tab-width char-image-strings)
  ;; Like GROUP-COLUMNS, but stops at line end.
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-line-columns text start end column
				   tab-width char-image-strings))
	  ((fix:<= gap-start start)
	   (let ((i&c
		  (substring-line-columns text
					  (fix:+ start gap-length)
					  (fix:+ end gap-length)
					  column
					  tab-width
					  char-image-strings)))
	     (cons (fix:- (car i&c) gap-length) (cdr i&c))))
	  (else
	   (let ((i&c
		  (substring-line-columns text start gap-start
					  column tab-width
					  char-image-strings)))
	     (if (fix:< (car i&c) gap-start)
		 i&c
		 (let ((i&c
			(substring-line-columns text
						gap-end
						(fix:+ end gap-length)
						(cdr i&c)
						tab-width
						char-image-strings)))
		   (cons (fix:- (car i&c) gap-length) (cdr i&c)))))))))

(define (string-line-columns string column tab-width char-image-strings)
  (substring-line-columns string 0 (string-length string) column tab-width
			  char-image-strings))

(define (substring-line-columns string start end column tab-width
				char-image-strings)
  (if tab-width
      (let loop ((index start) (column column))
	(if (fix:= index end)
	    (cons index column)
	    (let ((char (string-ref string index)))
	      (if (char=? char #\newline)
		  (cons index column)
		  (loop (fix:+ index 1)
			(fix:+ column
			       (if (char=? char #\tab)
				   (fix:- tab-width
					  (fix:remainder column tab-width))
				   (string-length
				    (vector-ref char-image-strings
						(char->integer char))))))))))
      (let loop ((index start) (column column))
	(if (fix:= index end)
	    (cons index column)
	    (let ((char (string-ref string index)))
	      (if (char=? char #\newline)
		  (cons index column)
		  (loop (fix:+ index 1)
			(fix:+ column
			       (string-length
				(vector-ref char-image-strings
					    (char->integer char)))))))))))

(define (group-column->index group start end start-column column tab-width
			     char-image-strings)
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-column->index text start end start-column column
				    tab-width char-image-strings))
	  ((fix:<= gap-start start)
	   (let ((result
		  (substring-column->index text
					   (fix:+ start gap-length)
					   (fix:+ end gap-length)
					   start-column
					   column
					   tab-width
					   char-image-strings)))
	     (vector-set! result 0 (fix:- (vector-ref result 0) gap-length))
	     result))
	  (else
	   (let ((result
		  (substring-column->index text start gap-start
					   start-column column tab-width
					   char-image-strings)))
	     (if (and (fix:< (vector-ref result 1) column)
		      (fix:= (vector-ref result 0) gap-start))
		 (let ((result
			(substring-column->index text
						 gap-end
						 (fix:+ end gap-length)
						 (fix:+ (vector-ref result 1)
							(vector-ref result 2))
						 column
						 tab-width
						 char-image-strings)))
		   (vector-set! result 0
				(fix:- (vector-ref result 0) gap-length))
		   result)
		 result))))))

(define (substring-column->index string start end start-column column
				 tab-width char-image-strings)
  ;; If COLUMN falls in the middle of a multi-column character, the
  ;; index returned is that of the character.  Thinking of the index
  ;; as a pointer between characters, the value is the pointer to the
  ;; left of the multi-column character.  Only if COLUMN reaches
  ;; across the character will the right-hand pointer be returned.
  ;; Various things depend on this.
  (if tab-width
      (let loop ((index start) (c start-column))
	(if (or (fix:= c column)
		(fix:= index end)
		(char=? #\newline (string-ref string index)))
	    (vector index c 0)
	    (let ((c
		   (fix:+ c
			  (let ((char (string-ref string index)))
			    (if (char=? char #\tab)
				(fix:- tab-width (fix:remainder c tab-width))
				(string-length
				 (vector-ref char-image-strings
					     (char->integer char))))))))
	      (if (fix:> c column)
		  (vector index column (fix:- c column))
		  (loop (fix:+ index 1) c)))))
      (let loop ((index start) (c start-column))
	(if (or (fix:= c column)
		(fix:= index end)
		(char=? #\newline (string-ref string index)))
	    (vector index c 0)
	    (let ((c
		   (fix:+ c
			  (string-length
			   (vector-ref char-image-strings
				       (char->integer
					(string-ref string index)))))))
	      (if (fix:> c column)
		  (vector index column (fix:- c column))
		  (loop (fix:+ index 1) c)))))))

(define (substring-image! string string-start string-end
			  image image-start image-end
			  tab-width column-offset results
			  char-image-strings)
  (let loop ((string-index string-start) (image-index image-start))
    (if (or (fix:= image-index image-end)
	    (fix:= string-index string-end))
	(begin
	  (vector-set! results 0 string-index)
	  (vector-set! results 1 image-index)
	  (vector-set! results 2 0))
	(let ((char (string-ref string string-index))
	      (partial
	       (lambda (partial)
		 (vector-set! results 0 string-index)
		 (vector-set! results 1 image-end)
		 (vector-set! results 2 partial))))
	  (if (and (char=? char #\tab) tab-width)
	      (let ((n
		     (fix:- tab-width
			    (fix:remainder (fix:+ column-offset
						  image-index)
					   tab-width))))
		(let ((end (fix:+ image-index n)))
		  (if (fix:<= end image-end)
		      (begin
			(do ((image-index image-index
					  (fix:+ image-index 1)))
			    ((fix:= image-index end))
			  (string-set! image image-index #\space))
			(loop (fix:+ string-index 1) end))
		      (begin
			(do ((image-index image-index
					  (fix:+ image-index 1)))
			    ((fix:= image-index image-end))
			  (string-set! image image-index #\space))
			(partial (fix:- end image-end))))))
	      (let* ((image-string  (vector-ref char-image-strings
						(char->integer char)))
		     (image-len     (string-length image-string)))
		(string-set! image image-index (string-ref image-string 0))
		(if (fix:= image-len 1)
		    (loop (fix:+ string-index 1) (fix:+ image-index 1))
		    (if (fix:< (fix:+ image-index image-len) image-end)
			(let copy-image-loop ((i 1))
			  (string-set! image (fix:+ image-index i)
				       (string-ref image-string i))
			  (if (fix:= (fix:+ i 1) image-len)
			      (loop (fix:+ string-index 1)
				    (fix:+ image-index image-len))
			      (copy-image-loop (fix:+ i 1))))
			(let copy-image-loop ((i 1))
			  (cond ((fix:= i image-len)
				 (loop (fix:+ string-index 1)
				       (fix:+ image-index image-len)))
				((fix:= (fix:+ image-index i) image-end)
				 (partial (fix:- image-len i)))
				(else
				 (string-set! image (fix:+ image-index i)
					      (string-ref image-string i))
				 (copy-image-loop (fix:+ i 1)))))))))))))

(define (string-image string start-column tab-width char-image-strings)
  (substring-image string 0 (string-length string) start-column tab-width
		   char-image-strings))

(define (substring-image string start end start-column tab-width
			 char-image-strings)
  (let ((columns
	 (fix:- (substring-columns string start end start-column tab-width
				   char-image-strings)
		start-column)))
    (let ((image (make-string columns)))
      (substring-image! string start end
			image 0 columns
			tab-width start-column substring-image-results
			char-image-strings)
      image)))

(define substring-image-results
  (make-vector 3))

(define (group-image! group start end
		      image image-start image-end
		      tab-width column-offset results
		      char-image-strings)
  (let ((text       (group-text group))
	(gap-start  (group-gap-start group))
	(gap-end    (group-gap-end group))
	(gap-length (group-gap-length group)))
    (cond ((fix:<= end gap-start)
	   (substring-image! text start end
			     image image-start image-end
			     tab-width column-offset results
			     char-image-strings))
	  ((fix:<= gap-start start)
	   (substring-image! text
			     (fix:+ start gap-length) (fix:+ end gap-length)
			     image image-start image-end
			     tab-width column-offset results
			     char-image-strings)
	   (vector-set! results 0 (fix:- (vector-ref results 0) gap-length)))
	  (else
	   (substring-image! text start gap-start
			     image image-start image-end
			     tab-width column-offset results
			     char-image-strings)
	   (if (fix:< (vector-ref results 1) image-end)
	       (begin
		 (substring-image! text gap-end (fix:+ end gap-length)
				   image (vector-ref results 1) image-end
				   tab-width column-offset results
				   char-image-strings)
		 (vector-set! results 0
			      (fix:- (vector-ref results 0) gap-length))))))))

(define (partial-image! char n image image-start image-end tab-width
			char-image-strings)
  ;; Assume that (< IMAGE-START IMAGE-END) and that N is less than the
  ;; total width of the image for the character.
  (let ((ascii (char->integer char)))
    (if (and (fix:= ascii (char->integer #\tab)) tab-width)
	(let ((end
	       (let ((end (fix:+ image-start n)))
		 (if (fix:< end image-end) end image-end))))
	  (do ((image-index image-start (fix:+ image-index 1)))
	      ((fix:= image-index end))
	    (string-set! image image-index #\space)))
	(let ((picture (vector-ref char-image-strings ascii)))
	  (let ((end
		 (let ((end (fix:+ image-start n)))
		   (if (fix:< end image-end) end image-end))))
	    (string-set! image image-start (string-ref picture 1))
	    (let loop ((i           (fix:- (string-length picture) n))
		       (image-index image-start))
	      (if (fix:< image-index end)
		  (begin
		    (string-set! image image-index (string-ref picture i))
		    (loop (fix:+ i 1) (fix:+ image-index 1))))))))))

