;;; -*-Scheme-*-
;;;
;;; $Id: bios.scm,v 1.9 2007/01/05 21:19:23 cph Exp $
;;;
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;;     2006, 2007 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;;; USA.
;;;
;;;

;;;; IBM-PC BIOS Screen Implementation
;;; package: (edwin screen console-screen)


(define (make-bios-screen)
  ;; What is the baud rate needed for?  It's not even meaningful.
  (let ((baud-rate (output-port/baud-rate console-output-port))
	(x-size (output-port/x-size console-output-port))
	(y-size (output-port/y-size console-output-port)))
    (make-screen (cons (fix:-1+ y-size) (fix:-1+ x-size))
		 bios-console-beep
		 bios-console-clear-line!
		 bios-console-clear-rectangle!
		 bios-console-clear-screen!
		 bios-console-discard!
		 bios-console-enter!
		 bios-console-exit!
		 bios-console-flush!
		 bios-console-modeline-event!
		 bios-console-discretionary-flush
		 bios-console-scroll-lines-down!
		 bios-console-scroll-lines-up!
		 bios-console-wrap-update!
		 bios-console-write-char!
		 bios-console-write-cursor!
		 bios-console-write-substring!
		 (fix:1+ (fix:quotient baud-rate 2400))
		 x-size
		 y-size)))

(define (bios-available?)
  (and (implemented-primitive-procedure? bios:can-use-bios?)
       (bios:can-use-bios?)
       (let ((term (get-environment-variable "TERM")))
	 (and term
	      (string-ci=? term "ibm_pc_bios")))))

(define bios-display-type)

(define (bios-initialize-package!)
  (set! bios-display-type
	(make-display-type 'IBM-PC-BIOS
			   #f
			   bios-available?
			   make-bios-screen
			   (lambda (screen)
			     screen
			     (get-console-input-operations #F))
			   with-console-grabbed
			   with-console-interrupts-enabled
			   with-console-interrupts-disabled))
  unspecific)

;;;; Customized IBM-PC BIOS console operations

(define-primitives
  (bios:beep 0)
  (bios:can-use-bios? 0)
  (bios:clear-line! 3)
  (bios:clear-rectangle! 5)
  (bios:clear-screen! 0)
  (bios:exit! 0)
  (bios:initialize! 2)
  (bios:scroll-lines-up! 5)
  (bios:scroll-lines-down! 5)
  (bios:write-char! 2)
  (bios:write-cursor! 2)
  (bios:write-substring! 4))

(define (bios-console-discard! screen)
  screen
  unspecific)

(define (bios-console-enter! screen)
  (define (default-attribute variable default)
    (let ((val (get-environment-variable variable)))
      (cond ((not val) default)
	    ((string? val) (string->number val))
	    (else val))))

  (bios:initialize!
   (default-attribute "EDWIN_FOREGROUND" 37)	; white foreground
   (default-attribute "EDWIN_BACKGROUND" 40))	; black background
  (bios:clear-screen!)
  (bios-move-cursor screen 0 0)
  unspecific)

(define (bios-console-exit! screen)
  (bios-move-cursor screen 0 (fix:-1+ (screen-y-size screen)))
  (bios:exit!))

(define (bios-console-modeline-event! screen window type)
  screen window type
  unspecific)

(define (bios-console-wrap-update! screen thunk)
  screen
  (thunk))

(define (bios-console-discretionary-flush screen)
  screen
  unspecific)

(define (bios-console-beep screen)
  screen
  (bios:beep))

(define (bios-console-flush! screen)
  screen
  unspecific)

(define (bios-move-cursor screen x y)
  screen
  (bios:write-cursor! x y))

(define (bios-console-write-cursor! screen x y)
  (bios-move-cursor screen x y))

(define (bios-console-write-char! screen x y char highlight)
  (if (not (and (fix:= y (car (screen-state screen)))
		(fix:= x (cdr (screen-state screen)))))
      (begin
	(bios-move-cursor screen x y)
	(bios:write-char! char highlight))))

(define (bios-console-write-substring! screen x y string start end highlight)
  (let ((end
	 (let ((delta (fix:- end start)))
	   (if (and (fix:= y (car (screen-state screen)))
		    (fix:= (fix:+ x delta)
			   (screen-x-size screen)))
	       (fix:-1+ end)
	       end))))
    (if (fix:< start end)
	(begin
	  (bios-move-cursor screen x y)
	  (bios:write-substring! string start end highlight)))))

(define (bios-console-clear-line! screen x y first-unused-x)
  (bios:clear-line! x y (fix:-1+ first-unused-x))
  (bios-move-cursor screen x y))

(define (bios-console-clear-screen! screen)
  (bios:clear-screen!)
  (bios-move-cursor screen 0 0))

(define (bios-console-clear-rectangle! screen xl xu yl yu highlight)
  screen
  (bios:clear-rectangle! xl xu yl yu highlight))

(define (bios-console-scroll-lines-down! screen xl xu yl yu amount)
  screen
  (bios:scroll-lines-down! xl (fix:-1+ xu) yl (fix:-1+ yu) amount)
  'CLEARED)

(define (bios-console-scroll-lines-up! screen xl xu yl yu amount)
  screen
  (bios:scroll-lines-up! xl (fix:-1+ xu) yl (fix:-1+ yu) amount)
  'CLEARED)
