#| -*-Scheme-*-

$Id: diros2.scm,v 1.10 2008/01/30 20:02:00 cph Exp $

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

;;;; Directory Editor (OS/2 Customizations)
;;; package: (edwin dired)


(define-key 'dired #\Z 'dired-do-compress)
(define-key 'dired #\S 'dired-hidden-toggle)
(define-key 'dired #\M 'dired-chmod)

(define-command dired-do-compress
  "Compress or uncompress marked (or next ARG) files.
The files are compressed or uncompressed using gzip."
  "P"
  (lambda (argument)
    (let ((n
	   (dired-change-files "compress" argument
	     (let ((gzip (os/find-program "gzip" #f (ref-variable exec-path)))
		   (directory (buffer-default-directory (current-buffer))))
	       (lambda (pathname lstart)
		 (let ((type (pathname-type pathname))
		       (namestring (->namestring pathname)))
		   (let ((decompress? (equal? type "gz")))
		     (message (if decompress? "Unc" "C")
			      "ompressing file `" namestring "'...")
		     (run-synchronous-process #f #f directory #f
					      gzip
					      (if decompress? "-d" "")
					      namestring)
		     (dired-redisplay
		      (pathname-new-type
		       pathname
		       (and (not decompress?)
			    (if (string? type)
				(string-append type ".gz")
				"gz")))
		      lstart))))))))
      (if (positive? n)
	  (message "Compressed or uncompressed " n " files.")))))

(define-command dired-hidden-toggle
  "Toggle display of hidden/system files on and off."
  ()
  (lambda () (dired-toggle-switch #\a)))

(define-command dired-chmod
  "Change mode of this file."
  "sChange to Mode\nP"
  (lambda (spec argument)
    (call-with-values (lambda () (os2/parse-attributes-spec spec))
      (lambda (plus minus)
	(dired-change-files "change attributes of" argument
	  (lambda (pathname lstart)
	    (set-file-modes! pathname
			     (fix:or (fix:andc (file-modes pathname)
					       minus)
				     plus))
	    (dired-redisplay pathname lstart)))))))

(define (os2/parse-attributes-spec spec)
  (let ((end (string-length spec))
	(plus '())
	(minus '()))
    (let loop ((index 0) (state #f))
      (if (< index end)
	  (let ((char (char-downcase (string-ref spec index)))
		(index (+ index 1)))
	    (case char
	      ((#\+ #\-)
	       (loop index char))
	      ((#\a #\h #\r #\s)
	       (set! plus (delv! char plus))
	       (set! minus (delv! char minus))
	       (case state
		 ((#\+)
		  (set! plus (cons char plus))
		  (loop index state))
		 ((#\-)
		  (set! minus (cons char minus))
		  (loop index state))
		 (else #f)))
	      (else #f)))
	  (values (os2/attribute-letters-to-mask plus)
		  (os2/attribute-letters-to-mask minus))))))

(define (os2/attribute-letters-to-mask letters)
  (let ((mask 0))
    (for-each (lambda (letter)
		(set! mask
		      (fix:or (case letter
				((#\a) os2-file-mode/archived)
				((#\d) os2-file-mode/directory)
				((#\h) os2-file-mode/hidden)
				((#\r) os2-file-mode/read-only)
				((#\s) os2-file-mode/system)
				(else (error "Unknown mode letter:" letter)))
			      mask))
		unspecific)
	      letters)
    mask))