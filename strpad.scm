#| -*-Scheme-*-

$Id: strpad.scm,v 1.13 2007/01/05 21:19:24 cph Exp $

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

;;;; String Padding Stuff


(define (pad-on-right-to string n)
  (let ((l (string-length string)))
    (if (> n l)
	(let ((result (string-allocate n)))
	  (substring-move-right! string 0 l result 0)
	  (substring-fill! result l n #\space)
	  result)
	string)))

(define (pad-on-left-to string n)
  (let ((l (string-length string)))
    (let ((delta (- n l)))
      (if (positive? delta)
	  (let ((result (string-allocate n)))
	    (substring-fill! result 0 delta #\space)
	    (substring-move-right! string 0 l result delta)
	    result)
	  string))))

(define (write-strings-densely strings #!optional port x-size)
  (let ((port (if (default-object? port) (current-output-port) port))
	(n (reduce max 0 (map string-length strings))))
    (let ((x-size
	   (if (default-object? x-size) (output-port/x-size port) x-size)))
      (let ((n-per-line (max 1 (quotient (+ x-size 1) (+ 2 n)))))
	(if (not (null? strings))
	    (let loop ((strings strings) (i 0))
	      (write-string (pad-on-right-to (car strings) n) port)
	      (let ((strings (cdr strings))
		    (i (+ i 1)))
		(if (not (null? strings))
		    (if (< i n-per-line)
			(begin
			  (write-string "  " port)
			  (loop strings i))
			(begin
			  (newline port)
			  (loop strings 0)))))))))))