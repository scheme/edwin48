#| -*-Scheme-*-

$Id: dos.scm,v 1.58 2007/01/05 21:19:23 cph Exp $

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

;;;; Win32 Customizations for Edwin


(define (os/set-file-modes-writeable! pathname)
  (set-file-modes! pathname
		   (fix:andc (file-modes pathname) nt-file-mode/read-only)))

(define (os/restore-modes-to-updated-file! pathname modes)
  (set-file-modes! pathname (fix:or modes nt-file-mode/archive)))

(define (os/scheme-can-quit?)
  #t)

(define (os/quit dir)
  (with-real-working-directory-pathname dir %quit))

(define (with-real-working-directory-pathname dir thunk)
  (let ((inside (->namestring (directory-pathname-as-file dir)))
	(outside false))
    (dynamic-wind
     (lambda ()
       (stop-thread-timer)
       (set! outside
	     (->namestring
	      (directory-pathname-as-file (working-directory-pathname))))
       (set-working-directory-pathname! inside)
       ((ucode-primitive set-working-directory-pathname! 1) inside))
     thunk
     (lambda ()
       (set! inside
	     (->namestring
	      (directory-pathname-as-file (working-directory-pathname))))
       ((ucode-primitive set-working-directory-pathname! 1) outside)
       (set-working-directory-pathname! outside)
       (start-thread-timer)))))

(define (dos/read-dired-files file all-files?)
  (map (lambda (entry) (cons (file-namestring (car entry)) (cdr entry)))
       (let ((entries (directory-read file #f #t)))
	 (if all-files?
	     entries
	     (list-transform-positive entries
	       (let ((mask
		      (fix:or nt-file-mode/hidden nt-file-mode/system)))
		 (lambda (entry)
		   (fix:= (fix:and (file-attributes/modes (cdr entry)) mask)
			  0))))))))

;;;; Win32 Clipboard Interface

(define cut-and-paste-active?
  #t)

(define (os/interprogram-cut string context)
  context
  (if cut-and-paste-active?
      (win32-clipboard-write-text
       (let ((string (convert-newline-to-crlf string)))
	 ;; Some programs can't handle strings over 64k.
	 (if (fix:< (string-length string) #x10000) string "")))))

(define (os/interprogram-paste context)
  context
  (if cut-and-paste-active?
      (let ((text (win32-clipboard-read-text)))
	(and text
	     (convert-crlf-to-newline text)))))

(define (convert-newline-to-crlf string)
  (let ((end (string-length string)))
    (let ((n-newlines
	   (let loop ((start 0) (n-newlines 0))
	     (let ((newline
		    (substring-find-next-char string start end #\newline)))
	       (if newline
		   (loop (fix:+ newline 1) (fix:+ n-newlines 1))
		   n-newlines)))))
      (if (fix:= n-newlines 0)
	  string
	  (let ((copy (make-string (fix:+ end n-newlines))))
	    (let loop ((start 0) (cindex 0))
	      (let ((newline
		     (substring-find-next-char string start end #\newline)))
		(if newline
		    (begin
		      (%substring-move! string start newline copy cindex)
		      (let ((cindex (fix:+ cindex (fix:- newline start))))
			(string-set! copy cindex #\return)
			(string-set! copy (fix:+ cindex 1) #\newline)
			(loop (fix:+ newline 1) (fix:+ cindex 2))))
		    (%substring-move! string start end copy cindex))))
	    copy)))))

(define (convert-crlf-to-newline string)
  (let ((end (string-length string)))
    (let ((n-crlfs
	   (let loop ((start 0) (n-crlfs 0))
	     (let ((cr
		    (substring-find-next-char string start end #\return)))
	       (if (and cr
			(not (fix:= (fix:+ cr 1) end))
			(char=? (string-ref string (fix:+ cr 1)) #\linefeed))
		   (loop (fix:+ cr 2) (fix:+ n-crlfs 1))
		   n-crlfs)))))
      (if (fix:= n-crlfs 0)
	  string
	  (let ((copy (make-string (fix:- end n-crlfs))))
	    (let loop ((start 0) (cindex 0))
	      (let ((cr
		     (substring-find-next-char string start end #\return)))
		(if (not cr)
		    (%substring-move! string start end copy cindex)
		    (let ((cr
			   (if (and (not (fix:= (fix:+ cr 1) end))
				    (char=? (string-ref string (fix:+ cr 1))
					    #\linefeed))
			       cr
			       (fix:+ cr 1))))
		      (%substring-move! string start cr copy cindex)
		      (loop (fix:+ cr 1) (fix:+ cindex (fix:- cr start)))))))
	    copy)))))

;;;; Mail Customization

(define (os/rmail-spool-directory) #f)
(define (os/rmail-primary-inbox-list system-mailboxes) system-mailboxes '())
(define (os/sendmail-program) "sendmail.exe")
(define (os/rmail-pop-procedure) #f)