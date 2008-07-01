#| -*-Scheme-*-

$Id: utlwin.scm,v 1.66 2008/01/30 20:02:07 cph Exp $

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

;;;; Utility Windows


;;;; Blank Window

(define-class blank-window vanilla-window
  ())

(define (blank-window:update-display! window screen x-start y-start
				      xl xu yl yu display-style)
  window display-style			;ignore
  (screen-clear-rectangle screen
			  (fix:+ x-start xl) (fix:+ x-start xu)
			  (fix:+ y-start yl) (fix:+ y-start yu)
			  #f)
  #t)

(define-method blank-window :update-display!
  blank-window:update-display!)

;;;; Vertical Border Window

(define-class vertical-border-window vanilla-window
  ())

(define-method vertical-border-window (:initialize! window window*)
  (usual==> window :initialize! window*)
  (set! x-size 1))

(define-method vertical-border-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the x-size of a vertical border window" x))

(define-method vertical-border-window (:set-size! window x y)
  (if (not (fix:= x 1))
      (error "Can't change the x-size of a vertical border window" x))
  (set! x-size x)
  (set! y-size y)
  (setup-redisplay-flags! redisplay-flags))

(define (vertical-border-window:update-display! window screen x-start y-start
						xl xu yl yu display-style)
  display-style				;ignore
  (if (fix:< xl xu)
      (clip-window-region-1 yl yu (window-y-size window)
	(lambda (yl yu)
	  (let ((xl (fix:+ x-start xl))
		(yu (fix:+ y-start yu)))
	    (let loop ((y (fix:+ y-start yl)))
	      (if (fix:< y yu)
		  (begin
		    (screen-output-char screen xl y #\| #f)
		    (loop (fix:+ y 1)))))))))
  #t)

(define-method vertical-border-window :update-display!
  vertical-border-window:update-display!)

;;;; Cursor Window

(define-class cursor-window vanilla-window
  (enabled?))

(define-method cursor-window (:initialize! window window*)
  (usual==> window :initialize! window*)
  (set! x-size 1)
  (set! y-size 1)
  (set! enabled? #f))

(define-method cursor-window (:set-x-size! window x)
  window				;ignore
  (error "Can't change the size of a cursor window" x))

(define-method cursor-window (:set-y-size! window y)
  window				;ignore
  (error "Can't change the size of a cursor window" y))

(define-method cursor-window (:set-size! window x y)
  window				;ignore
  (error "Can't change the size of a cursor window" x y))

(define (cursor-window:update-display! window screen x-start y-start
				       xl xu yl yu display-style)
  display-style				;ignore
  (if (and (with-instance-variables cursor-window window () enabled?)
	   (fix:< xl xu)
	   (fix:< yl yu))
      (screen-move-cursor screen x-start y-start))
  #t)

(define-method cursor-window :update-display!
  cursor-window:update-display!)

(define-method cursor-window (:enable! window)
  (set! enabled? #t)
  (setup-redisplay-flags! redisplay-flags))

(define-method cursor-window (:disable! window)
  (set! enabled? #f)
  (set-car! redisplay-flags #f))

;;
;; Local Variables:
;; eval: (put 'with-instance-variables 'scheme-indent-function 3)
;; End:
;;