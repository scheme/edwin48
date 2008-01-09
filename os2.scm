#| -*-Scheme-*-

$Id: os2.scm,v 1.56 2007/01/05 21:19:24 cph Exp $

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

;;;; OS/2 Customizations for Edwin

(declare (usual-integrations))

(define (os/set-file-modes-writeable! pathname)
  (set-file-modes! pathname
		   (fix:andc (file-modes pathname) os2-file-mode/read-only)))

(define (os/restore-modes-to-updated-file! pathname modes)
  (set-file-modes! pathname (fix:or modes os2-file-mode/archived)))

(define (os/scheme-can-quit?)
  #f)

(define (os/quit dir)
  dir
  (error "Can't quit."))

(define (dos/read-dired-files file all-files?)
  (let loop
      ((pathnames
	(let ((pathnames (directory-read file #f)))
	  (if all-files?
	      pathnames
	      (list-transform-positive pathnames
		(let ((mask
		       (fix:or os2-file-mode/hidden os2-file-mode/system)))
		  (lambda (pathname)
		    (fix:= (fix:and (file-modes pathname) mask) 0)))))))
       (result '()))
    (if (null? pathnames)
	result
	(loop (cdr pathnames)
	      (let ((attr (file-attributes (car pathnames))))
		(if attr
		    (cons (cons (file-namestring (car pathnames)) attr) result)
		    result))))))

;;;; OS/2 Clipboard Interface

(define (os/interprogram-cut string context)
  context
  (os2-clipboard-write-text
   (let ((string (convert-newline-to-crlf string)))
     ;; Some programs can't handle strings over 64k.
     (if (fix:< (string-length string) #x10000) string ""))))

(define (os/interprogram-paste context)
  context
  (let ((text (os2-clipboard-read-text)))
    (and text
	 (convert-crlf-to-newline text))))

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

(define (os/sendmail-program)
  "sendmail")

(define (os/rmail-spool-directory)
  (or (let ((etc (get-environment-variable "ETC")))
	(and etc
	     (file-directory? etc)
	     (let ((mail
		    (merge-pathnames "mail/" (pathname-as-directory etc))))
	       (and (file-directory? mail)
		    (->namestring mail)))))
      "c:\\mptn\\etc\\mail\\"))

(define (os/rmail-primary-inbox-list system-mailboxes)
  system-mailboxes)

(define (os/rmail-pop-procedure)
  (and (os/find-program "popclient" #f (ref-variable exec-path) #f)
       (lambda (server user-name password directory)
	 (os2-pop-client server user-name password directory))))

(define (os2-pop-client server user-name password directory)
  (let ((target
	 (->namestring
	  (merge-pathnames (if (dos/fs-long-filenames? directory)
			       ".popmail"
			       "popmail.tmp")
			   directory))))
    (let ((buffer (temporary-buffer "*popclient*")))
      (cleanup-pop-up-buffers
       (lambda ()
	 (pop-up-buffer buffer #f)
	 (let ((status.reason
		(let ((args
		       (list "-u" user-name
			     "-p" (os2-pop-client-password password)
			     "-o" target
			     server)))
		  (apply run-synchronous-process
			 #f (cons (buffer-end buffer) #t) #f #f
			 "popclient"
			 "-3"
			 (if (ref-variable rmail-pop-delete)
			     args
			     (cons "-k" args))))))
	   (if (and (eq? 'EXITED (car status.reason))
		    (memv (cdr status.reason) '(0 1)))
	       (kill-pop-up-buffer buffer)
	       (begin
		 (keep-pop-up-buffer buffer)
		 (editor-error "Error getting mail from POP server.")))))))
    target))

(define (os2-pop-client-password password)
  (cond ((string? password)
	 password)
	((and (pair? password) (eq? 'FILE (car password)))
	 (call-with-input-file (cadr password)
	   (lambda (port)
	     (read-string (char-set #\newline) port))))
	(else
	 (error "Illegal password:" password))))

(define-variable rmail-pop-delete
  "If true, messages are deleted from the POP server after being retrieved.
Otherwise, messages remain on the server and will be re-fetched later."
  #t
  boolean?)