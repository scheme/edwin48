;;; -*-Scheme-*-
;;;
;;; $Id: docstr.scm,v 1.9 2007/01/05 21:19:23 cph Exp $
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

;;;; Documentation Strings


(define *external-doc-strings?* #t)
(define *external-doc-strings-file* #f)
(define *doc-strings* #f)
(define *doc-string-posn* 0)
(define *doc-string-channel* #f)
(define *doc-string-buffer* #f)

(define (doc-string->posn name str)
  (if (and *external-doc-strings?* (string? str))
      (let ((nlen (string-length name))
	    (dslen (string-length str))
	    (slen (if (not *doc-strings*)
		      0
		      (string-length *doc-strings*)))
	    (posn *doc-string-posn*))
	(let* ((next (fix:+ posn nlen))
	       (end (fix:+ next (fix:+ dslen 6))))
	  (if (> end slen)
	      (let ((new (string-allocate
			  (max end
			       (if (fix:zero? slen)
				   4096
				   (fix:+ slen (fix:quotient slen 2)))))))
		(if *doc-strings*
		    (substring-move-right! *doc-strings* 0 posn new 0))
		(set! *doc-strings* new)))
	  (let ((doc-strings *doc-strings*))
	    (vector-8b-set! doc-strings posn (fix:remainder dslen 256))
	    (vector-8b-set! doc-strings
			    (fix:+ posn 1)
			    (fix:quotient dslen 256))
	    (string-set! doc-strings (fix:+ posn 2) #\Newline)
	    (substring-move-right! name 0 nlen doc-strings (fix:+ posn 3))
	    (string-set! doc-strings (fix:+ next 3) #\Newline)
	    (substring-move-right! str 0 dslen doc-strings (fix:+ next 4))
	    (string-set! doc-strings (fix:- end 2) #\Newline)
	    (string-set! doc-strings (fix:- end 1) #\Newline)
	    (set! *doc-string-posn* end)
	    posn)))
      str))

(define doc-string-buffer-length 512)

(define (->doc-string name posn)
  (define (out-of-range)
    (editor-error "->doc-string: Out of range argument" posn))

  (define (fill-buffer channel buffer posn blen)
    (let fill-loop ((posn posn))
      (if (fix:< posn blen)
	  (let ((n (channel-read-block channel buffer posn blen)))
	    (fill-loop (fix:+ posn n))))))

  (define (verify-and-extract buffer nlen dslen nposn)
    (let ((nend (fix:+ nposn nlen)))
      (if (not (string=? (substring buffer nposn nend) name))
	  (editor-error "->doc-string: Inconsistency" posn)
	  (let ((dstart (fix:+ nend 1)))
	    (substring buffer dstart (fix:+ dstart dslen))))))

  (cond ((string? posn)
	 posn)
	((not (fix:fixnum? posn))
	 (editor-error "->doc-string: Wrong type argument" posn))
	((fix:< posn 0)
	 (out-of-range))
	(*doc-strings*
	 (let ((slen (string-length *doc-strings*))
	       (nlen (string-length name)))
	   (if (fix:> (fix:+ posn 2) slen)
	       (out-of-range))
	   (let ((dslen
		  (fix:+ (vector-8b-ref *doc-strings* posn)
			 (fix:lsh (vector-8b-ref *doc-strings* (fix:+ posn 1))
				  8))))
	     (if (fix:> (fix:+ (fix:+ posn 6) (fix:+ nlen dslen)) slen)
		 (out-of-range)
		 (verify-and-extract *doc-strings* nlen dslen
				     (fix:+ posn 3))))))
	(else
	 (guarantee-doc-string-state)
	 (let* ((channel *doc-string-channel*)
		(buffer *doc-string-buffer*)
		(flen (channel-file-length channel))
		(nlen (string-length name))
		(delta (fix:- flen (fix:+ posn 2))))
	   (if (fix:< delta 0)
	       (out-of-range))
	   (channel-file-set-position channel posn)
	   (let ((blen (min doc-string-buffer-length delta)))
	     (fill-buffer channel buffer 0 blen)
	     (let* ((dslen (fix:+ (vector-8b-ref buffer 0)
				  (fix:lsh (vector-8b-ref buffer 1)
					   8)))
		    (end (fix:+ (fix:+ dslen nlen) 6)))
	       (cond ((not (fix:> end blen))
		      (verify-and-extract buffer nlen dslen 3))
		     ((fix:> (fix:+ end posn) flen)
		      (out-of-range))
		     (else
		      (let* ((rlen (fix:+ (fix:+ nlen dslen) 1))
			     (result (string-allocate rlen)))
			(substring-move-right! buffer 3 blen result 0)
			(fill-buffer channel result (fix:- blen 3) rlen)
			(verify-and-extract result nlen dslen 0))))))))))

(define (dump-doc-strings output #!optional permanent)
  (if (not *doc-strings*)
      (error "dump-doc-strings: No doc strings to dump!"))
  (set! *external-doc-strings-file*
	(if (or (default-object? permanent)
		(not permanent))
	    output
	    permanent))	
  (set-string-length! *doc-strings* *doc-string-posn*)
  (call-with-binary-output-file
   output
   (lambda (port)
     (output-port/write-string port *doc-strings*)))
  (set! *external-doc-strings?* #f)
  (set! *doc-string-posn* 0)
  (set! *doc-strings* #f)
  unspecific)

(define (guarantee-doc-string-state)
  (if (not *doc-string-buffer*)
      (set! *doc-string-buffer* (string-allocate doc-string-buffer-length)))
  (cond (*doc-string-channel*)
	((not *external-doc-strings-file*)
	 (editor-error
	  "guarantee-doc-string-channel: Undeclared doc-string file"))
	(else
	 (let ((doc-strings
		(if (or (pathname-absolute? *external-doc-strings-file*)
			(file-exists? *external-doc-strings-file*))
		    *external-doc-strings-file*
		    (merge-pathnames *external-doc-strings-file*
				     (edwin-etc-directory)))))
	   (if (not (file-exists? doc-strings))
	       (editor-error
		"guarantee-doc-string-channel: Non-existent doc-string file")
	       (begin
		 (set! *doc-string-channel*
		       (file-open-input-channel
			(->namestring doc-strings)))
		 unspecific))))))

(add-event-receiver! event:after-restart
		     (lambda () (set! *doc-string-channel* #f)))

;;;; Abstraction of help descriptions

(define (description? description)
  (or (string? description)
      (and (procedure? description)
	   (procedure-arity-valid? description 0))))

(define (description->string description)
  (cond ((string? description) description)
	((procedure? description) (description))
	(else
	 (error:wrong-type-argument description "description"
				    'DESCRIPTION->STRING))))

(define (description-first-line description)
  (let ((string (description->string description)))
    (let ((index (string-find-next-char string #\newline)))
      (if index
	  (substring string 0 index)
	  string))))

(define (description-append . descriptions)
  (lambda () (apply string-append (map description->string descriptions))))
