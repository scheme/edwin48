#| -*-Scheme-*-

$Id: utils.scm,v 1.63 2008/01/30 20:02:07 cph Exp $

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

;;;; Editor Utilities


#|
(define-syntax chars-to-words-shift
  (sc-macro-transformer
   (lambda (form environment)
     form environment
     ;; This is written as a macro so that the shift will be a constant
     ;; in the compiled code.
     ;; It does not work when cross-compiled!
     (let ((chars-per-word (vector-ref (gc-space-status) 0)))
       (case chars-per-word
	 ((4) -2)
	 ((8) -3)
	 (else (error "Can't support this word size:" chars-per-word)))))))

(define (chars->words n-chars)
  (fix:lsh (fix:+ (fix:+ n-chars 1)	;Add 1 for NUL termination.
		  (fix:not (fix:lsh -1 (fix:- 0 (chars-to-words-shift)))))
	   (chars-to-words-shift)))

(define (edwin-set-string-maximum-length! string n-chars)
  (if (not (string? string))
      (error:wrong-type-argument string "string" 'SET-STRING-MAXIMUM-LENGTH!))
  (if (not (fix:fixnum? n-chars))
      (error:wrong-type-argument n-chars "fixnum" 'SET-STRING-MAXIMUM-LENGTH!))
  (if (not (and (fix:>= n-chars 0)
		(fix:< n-chars
		       (fix:lsh (fix:- (system-vector-length string) 1)
				(fix:- 0 (chars-to-words-shift))))))
      (error:bad-range-argument n-chars 'SET-STRING-MAXIMUM-LENGTH!))
  (let ((mask (set-interrupt-enables! interrupt-mask/none)))
    ((ucode-primitive primitive-object-set! 3)
     string
     0
     ((ucode-primitive primitive-object-set-type 2)
      (ucode-type manifest-nm-vector)
      (fix:+ 1 (chars->words (fix:+ n-chars 1)))))
    (set-string-length! string (fix:+ n-chars 1))
    (string-set! string n-chars #\nul)
    (set-string-length! string n-chars)
    (set-interrupt-enables! mask)
    unspecific))

(define set-string-maximum-length!
  (if (compiled-procedure? edwin-set-string-maximum-length!)
      edwin-set-string-maximum-length!
      (ucode-primitive set-string-maximum-length!)))
|#

(define (%substring-move! source start-source end-source
			  target start-target)
  (cond ((not (fix:< start-source end-source))
	 unspecific)
	((not (eq? source target))
	 (if (fix:< (fix:- end-source start-source) 32)
	     (do ((scan-source start-source (fix:+ scan-source 1))
		  (scan-target start-target (fix:+ scan-target 1)))
		 ((fix:= scan-source end-source) unspecific)
	       (string-set! target
			    scan-target
			    (string-ref source scan-source)))
	     (string-copy! target start-target
			   source start-source end-source)))
	((fix:< start-source start-target)
	 (if (fix:< (fix:- end-source start-source) 32)
	     (do ((scan-source end-source (fix:- scan-source 1))
		  (scan-target
		   (fix:+ start-target (fix:- end-source start-source))
		   (fix:- scan-target 1)))
		 ((fix:= scan-source start-source) unspecific)
	       (string-set! source
			    (fix:- scan-target 1)
			    (string-ref source (fix:- scan-source 1))))
	     (string-copy! source start-target
			   source start-source end-source)))
	((fix:< start-target start-source)
	 (if (fix:< (fix:- end-source start-source) 32)
	     (do ((scan-source start-source (fix:+ scan-source 1))
		  (scan-target start-target (fix:+ scan-target 1)))
		 ((fix:= scan-source end-source) unspecific)
	       (string-set! source
			    scan-target
			    (string-ref source scan-source)))
	     (string-copy! source start-target
			   source start-source end-source)))))

(define (string-greatest-common-prefix strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-prefix-length string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-greatest-common-prefix-ci strings)
  (let loop
      ((strings (cdr strings))
       (string (car strings))
       (index (string-length (car strings))))
    (if (null? strings)
	(substring string 0 index)
	(let ((string* (car strings)))
	  (let ((index* (string-prefix-length-ci string string*)))
	    (if (< index* index)
		(loop (cdr strings) string* index*)
		(loop (cdr strings) string index)))))))

(define (string-append-separated x y)
  (cond ((string-null? x) y)
	((string-null? y) x)
	(else (string-append x " " y))))

(define (substring->nonnegative-integer line start end)
  (let loop ((index start) (n 0))
    (if (fix:= index end)
	n
	(let ((k (fix:- (vector-8b-ref line index) (char->integer #\0))))
	  (and (fix:>= k 0)
	       (fix:< k 10)
	       (loop (fix:+ index 1) (+ (* n 10) k)))))))

(define char-set:null
  (char-set))

(define char-set:return
  (char-set #\return))

(define char-set:not-space
  (char-set-complement char-set:whitespace))


(define (y-or-n? . strings)
  (define (loop)
    (let ((char (char-upcase (read-char)))
	  (write-string (lambda (s)
			  (write-string s (current-output-port)))))
      (cond ((or (char=? char #\Y)
		 (char=? char #\Space))
	     (write-string "Yes")
	     #t)
	    ((or (char=? char #\N)
		 (char=? char #\Rubout))
	     (write-string "No")
	     #f)
	    (else
	     (if (not (char=? char #\newline))
		 (beep))
	     (loop)))))
  (newline)
  (for-each write-string strings)
  (loop))
#|
(define (delete-directory-no-errors filename)
  (catch-file-errors (lambda (condition) condition #f)
		     (lambda () (remove-directory filename) #t)))
|#
(define (string-or-false? object)
  ;; Useful as a type for option variables.
  (or (not object)
      (string? object)))

(define (list-of-strings? object)
  (list-of-type? object string?))

(define (list-of-pathnames? object)
  (list-of-type? object
		 (lambda (object) (or (pathname? object) (string? object)))))

(define (list-of-type? object predicate)
  (and (list? object)
       (every predicate object)))

(define (dotimes n procedure)
  (define (loop i)
    (if (< i n)
	(begin (procedure i)
	       (loop (1+ i)))))
  (loop 0))

(define (split-list elements predicate)
  (let loop ((elements elements) (satisfied '()) (unsatisfied '()))
    (if (pair? elements)
	(if (predicate (car elements))
	    (loop (cdr elements) (cons (car elements) satisfied) unsatisfied)
	    (loop (cdr elements) satisfied (cons (car elements) unsatisfied)))
	(values satisfied unsatisfied))))

#|
(define make-strong-eq-hash-table
  (strong-hash-table/constructor eq-hash-mod eq? #t))

(define make-weak-equal-hash-table
  (weak-hash-table/constructor equal-hash-mod equal? #t))
|#

#|
(define (file-time->ls-string time #!optional now)
  ;; Returns a time string like that used by unix `ls -l'.
  (let ((time (file-time->universal-time time))
	(now
	 (if (or (default-object? now) (not now))
	     (get-universal-time)
	     now)))
    (let ((dt (decode-universal-time time))
	  (d2 (lambda (n c) (string-pad-left (number->string n) 2 c))))
      (string-append (month/short-string (decoded-time/month dt))
		     " "
		     (d2 (decoded-time/day dt) #\space)
		     " "
		     (if (<= 0 (- now time) (* 180 24 60 60))
			 (string-append (d2 (decoded-time/hour dt) #\0)
					":"
					(d2 (decoded-time/minute dt) #\0))
			 (string-append " "
					(number->string
					 (decoded-time/year dt))))))))

(define (catch-file-errors if-error thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:file-error
				   condition-type:port-error)
	 (lambda (condition)
	   (continuation (if-error condition)))
       thunk))))
|#