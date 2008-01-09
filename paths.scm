#| -*-Scheme-*-

$Id: paths.scm,v 1.22 2007/07/07 17:20:12 cph Exp $

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

;;;; Edwin Pathnames


(define (edwin-library-directory-pathname envvar required?)
  (let ((envval (get-environment-variable envvar)))
    (if envval
	(pathname-as-directory (merge-pathnames envval))
	(or (system-library-directory-pathname "edwin")
	    (and required?
		 (error "Can't find edwin library directory."))))))

(define (edwin-binary-directory)
  (edwin-library-directory-pathname "EDWIN_BINARY_DIRECTORY" #t))

(define (edwin-info-directory)
  (edwin-library-directory-pathname "EDWIN_INFO_DIRECTORY" #f))

(define (edwin-etc-directory)
  (edwin-library-directory-pathname "EDWIN_ETC_DIRECTORY" #t))

(define (edwin-etc-pathname filename)
  (let ((pathname (merge-pathnames filename (edwin-etc-directory))))
    (if (not (file-exists? pathname))
	(error "Unable to find file:" (->namestring pathname)))
    pathname))

(define (edwin-tutorial-pathname)
  (edwin-etc-pathname "TUTORIAL"))

(define default-homedir-pathname
  ;; This binding exists to allow uses of the "home" directory as a
  ;; default directory to be overridden.
  user-homedir-pathname)