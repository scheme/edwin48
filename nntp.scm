#| -*-Scheme-*-

$Id: nntp.scm,v 1.34 2007/01/05 21:19:23 cph Exp $

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

;;;; NNTP Interface

;;; This program provides a high-level interface to an NNTP server.
;;; It implements a database abstraction that gives the impression
;;; that the news database is in memory and can be manipulated
;;; directly.  This abstraction largely hides the underlying server
;;; communication on which it is built.

;;; The abstraction provides models for the server, each of the groups
;;; it contains, and the headers in each group.  It also provides a
;;; method for combining headers into conversation threads.

(declare (usual-integrations))

(load-option 'GDBM)

;;;; NNTP Connection

(define-structure (nntp-connection
		   (conc-name nntp-connection:)
		   (constructor make-nntp-connection
				(server proxy change-hook)))
  (server #f read-only #t)
  (proxy #f read-only #t)
  (change-hook #f read-only #t)
  (port #f)
  (banner #f)
  (group-table (make-string-hash-table) read-only #t)
  (reader-hook #f)
  (current-group #f))

(define (nntp-connection:reopen connection)
  (let ((msg
	 (string-append "Opening connection to "
			(nntp-connection:server connection)
			"... ")))
    (message msg)
    (let ((port
	   (open-tcp-stream-socket (or (nntp-connection:proxy connection)
				       (nntp-connection:server connection))
				   "nntp")))
      (set-nntp-connection:port! connection port)
      (set-nntp-connection:banner! connection (input-port/read-line port)))
    (set-nntp-connection:current-group! connection #f)
    (if (nntp-connection:change-hook connection)
	((nntp-connection:change-hook connection) connection))
    (message msg "done")))

(define (nntp-connection:closed? connection)
  (let ((port (nntp-connection:port connection)))
    (or (not port)
	(input-port/eof? port))))

(define (nntp-connection:close connection)
  (let ((msg
	 (string-append "Closing connection to "
			(nntp-connection:server connection)
			"... ")))
    (message msg)
    (if (not (nntp-connection:closed? connection))
	(begin
	  (nntp-write-command connection "quit")
	  (nntp-drain-output connection)))
    (nntp-connection:close-1 connection)
    (message msg "done")))

(define (nntp-connection:close-1 connection)
  (if (not (nntp-connection:closed? connection))
      (begin
	(close-port (nntp-connection:port connection))
	(set-nntp-connection:port! connection #f)))
  (set-nntp-connection:current-group! connection #f)
  (if (nntp-connection:change-hook connection)
      ((nntp-connection:change-hook connection) connection)))

(define (nntp-connection:current-group? connection group-name)
  (and (nntp-connection:current-group connection)
       (string=? (nntp-connection:current-group connection) group-name)))

;;;; Groups-List Cache

(define (nntp-connection:active-groups connection re-read?)
  (call-with-values
      (lambda () (nntp-connection:active-groups-vector connection re-read?))
    (lambda (time lines)
      time
      (convert-groups-list lines))))

(define (nntp-connection:new-groups connection)
  (call-with-values
      (lambda () (nntp-connection:active-groups-vector connection #f))
    (lambda (time lines)
      (let ((new-lines
	     (call-with-temporary-file-pathname
	      (lambda (pathname)
		(call-with-output-file pathname
		  (lambda (port)
		    (nntp-newsgroups-command connection port time)))
		(call-with-input-file pathname read-newsgroup-lines)))))
	(let* ((table (make-string-hash-table))
	       (add-line
		(lambda (line)
		  (hash-table/put! table (string-first-token line) line))))
	  (for-each-vector-element lines add-line)
	  (for-each-vector-element new-lines add-line)
	  (write-init-file-atomically
	   (nntp-connection:active-groups-pathname connection)
	   (lambda (port)
	     (write (get-universal-time) port)
	     (newline port)
	     (for-each (lambda (line)
			 (write-string line port)
			 (newline port))
		       (hash-table/datum-list table)))))
	(convert-groups-list new-lines)))))

(define (nntp-connection:active-groups-vector connection re-read?)
  (let ((pathname (nntp-connection:active-groups-pathname connection)))
    (if (or re-read? (not (file-readable? pathname)))
	(write-init-file-atomically pathname
	  (lambda (port)
	    (write (get-universal-time) port)
	    (newline port)
	    (nntp-list-command connection port))))
    (let ((msg "Reading list of news groups... "))
      (message msg)
      (call-with-input-file pathname
	(lambda (port)
	  (let ((time (read port)))
	    (if (eqv? #\newline (input-port/peek-char port))
		(input-port/discard-char port))
	    (let ((lines (read-newsgroup-lines port)))
	      (message msg "done")
	      (values time lines))))))))

(define (convert-groups-list lines)
  (let ((msg "Parsing list of news groups... "))
    (message msg)
    (let ((end (vector-length lines)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i end))
	(vector-set! lines i (string-first-token (vector-ref lines i)))))
    (sort! lines string<?)
    (message msg "done"))
  lines)

(define (read-newsgroup-lines port)
  (let loop ((lines '()))
    (let ((line (input-port/read-line port)))
      (if (eof-object? line)
	  (list->vector (reverse! lines))
	  (loop (cons line lines))))))

(define (nntp-connection:active-groups-pathname connection)
  (init-file-specifier->pathname
   (list "snr" (nntp-connection:server connection) "all-groups")))

;;;; Group Cache

(define (find-news-group connection name)
  (hash-table/get (nntp-connection:group-table connection) name #f))

(define (nntp-connection:remember-group! connection name group)
  (hash-table/put! (nntp-connection:group-table connection) name group))

(define (nntp-connection:purge-group-cache connection predicate)
  (let ((table (nntp-connection:group-table connection)))
    (if table
	(hash-table/for-each table
	  (lambda (name group)
	    (if (predicate group)
		(hash-table/remove! table name)))))))

;;;; NNTP Commands

(define (nntp-group-command connection group-name)
  (nntp-protect connection
    (lambda ()
      (nntp-group-request connection group-name)
      (nntp-drain-output connection)
      (nntp-group-reply connection))))

(define (nntp-group-request connection group-name)
  (nntp-write-command connection "group" group-name)
  (set-nntp-connection:current-group! connection group-name))

(define (nntp-group-reply connection)
  (let ((response (nntp-read-line connection)))
    (case (nntp-response-number response)
      ((211)
       (let ((tokens (string-tokenize response)))
	 (vector (token->number (cadr tokens))
		 (token->number (caddr tokens))
		 (token->number (cadddr tokens)))))
      ((411) 'NO-SUCH-GROUP)
      (else (nntp-error response)))))

;; This says how many pending HEAD requests may be sent before it's
;; necessary to starting reading the replies, to avoid deadlock.
(define nntp-maximum-request 400)

;; This is an estimate of the number of bytes per HEAD request.  This
;; is sufficiently large to allow 9-digit message numbers.
(define nntp-head-request-size 16)

(define (nntp-head-request-count)
  ;; This returns the maximum number of head requests to transmit,
  ;; limited so that at least twice this number can be initially sent
  ;; to fill the request window.
  (let loop
      ((n-chunk (quotient nntp-socket-buffer-size nntp-head-request-size)))
    (if (< (quotient nntp-maximum-request n-chunk) 2)
	(loop (quotient n-chunk 2))
	n-chunk)))

(define (nntp-head-request connection key)
  (nntp-write-command connection "head" key))

(define (nntp-head-reply connection prune?)
  (let ((response (nntp-read-line connection)))
    (case (nntp-response-number response)
      ((221)
       (let ((tokens (string-tokenize response)))
	 (vector (cadr tokens)
		 (caddr tokens)
		 (if prune?
		     (header-lines->text (nntp-read-text-lines connection))
		     (call-with-output-string
		      (lambda (port)
			(nntp-read-text connection port #f)))))))
      ((423 430)
       'NO-SUCH-ARTICLE)
      (else
       (nntp-error response)))))

(define (nntp-body-command connection key port)
  (nntp-protect connection
    (lambda ()
      (nntp-write-command connection "body" key)
      (nntp-drain-output connection)
      (let ((response (nntp-read-line connection)))
	(case (nntp-response-number response)
	  ((222) (nntp-read-text connection port #f) #t)
	  ((423 430) #f)
	  (else (nntp-error response)))))))

(define (nntp-list-command connection port)
  (%nntp-list-command connection port
		      (string-append "Reading list of news groups from "
				     (nntp-connection:server connection)
				     "... ")
		      (list "list")
		      215))

(define (nntp-newsgroups-command connection port time)
  (%nntp-list-command connection port
		      (string-append "Reading new news groups from "
				     (nntp-connection:server connection)
				     "... ")
		      (cons "newgroups" (nntp-newsgroups-time time))
		      231))

(define (nntp-newsgroups-time time)
  (let ((dt (decode-universal-time time))
	(d2 (lambda (n) (string-pad-left (number->string n) 2 #\0))))
    (list (string-append (d2 (decoded-time/year dt))
			 (d2 (decoded-time/month dt))
			 (d2 (decoded-time/day dt)))
	  (string-append (d2 (decoded-time/hour dt))
			 (d2 (decoded-time/minute dt))
			 (d2 (decoded-time/second dt))))))

(define (%nntp-list-command connection port msg command valid-response)
  (nntp-protect connection
    (lambda ()
      (message msg)
      (apply nntp-write-command connection command)
      (nntp-drain-output connection)
      (let ((response (nntp-read-line connection)))
	(if (fix:= (nntp-response-number response) valid-response)
	    (let ((n 0))
	      (nntp-read-text connection port
		(lambda ()
		  (set! n (fix:+ n 1))
		  (if (fix:= (fix:remainder n 128) 0)
		      (message msg n)))))
	    (nntp-error response)))
      (message msg "done"))))

(define (nntp-connection:post-article connection port)
  (nntp-protect connection
    (lambda ()
      (nntp-write-command connection "post")
      (nntp-drain-output connection)
      (let ((response (nntp-read-line connection)))
	(if (fix:= 340 (nntp-response-number response))
	    (let loop ()
	      (let ((line (input-port/read-line port)))
		(if (eof-object? line)
		    (begin
		      (nntp-write-command connection ".")
		      (nntp-drain-output connection)
		      (let ((response (nntp-read-line connection)))
			(and (not (fix:= 240 (nntp-response-number response)))
			     response)))
		    (begin
		      (nntp-write-line connection line)
		      (loop)))))
	    response)))))

;;;; NNTP Errors

(define condition-type:nntp-error
  (make-condition-type 'NNTP-ERROR condition-type:error
      '(RESPONSE)
    (lambda (condition port)
      (write-string "NNTP error: " port)
      (let ((response (nntp-error/response condition)))
        (write-string (if (eof-object? response)
                          "connection lost"
                          response)
                      port)))))

(define nntp-error/response
  (condition-accessor condition-type:nntp-error 'RESPONSE))

(define nntp-error
  (condition-signaller condition-type:nntp-error
		       '(RESPONSE)
		       standard-error-handler))

(define (nntp-protect connection thunk)
  (let ((try
	 (lambda ()
	   (let ((abort? #t))
	     (dynamic-wind (lambda ()
			     (set! abort? #t)
			     unspecific)
			   (lambda ()
			     (if (nntp-connection:closed? connection)
				 (nntp-connection:reopen connection))
			     (let ((value (thunk)))
			       (set! abort? #f)
			       value))
			   (lambda ()
			     (if abort?
				 (nntp-connection:close-1 connection))))))))
    (call-with-current-continuation
     (lambda (k)
       (bind-condition-handler (list condition-type:nntp-error)
	   (lambda (condition)
	     ;; If the server closed the connection, try again.  This
	     ;; should automatically re-open the connection.
             (let ((response (nntp-error/response condition)))
               (if (or (eof-object? response)
                       (memv (nntp-response-number response)
                             '(205 503)))
                   (within-continuation k try))))
	 try)))))

;;;; NNTP I/O

(define nntp-socket-buffer-size 4096)

(define (nntp-write-command connection string . strings)
  (let ((port (nntp-connection:port connection)))
    (output-port/write-string port string)
    (do ((strings strings (cdr strings)))
	((null? strings))
      (output-port/write-char port #\space)
      (output-port/write-string port (car strings)))
    (output-port/write-char port #\newline)))

(define (nntp-write-line connection string)
  (let ((port (nntp-connection:port connection)))
    (if (and (not (string-null? string))
	     (char=? (string-ref string 0) #\.))
	(output-port/write-char port #\.))
    (output-port/write-string port string)
    (output-port/write-char port #\newline)))

(define (nntp-drain-output connection)
  (output-port/flush-output (nntp-connection:port connection)))

(define (nntp-read-line connection)
  (let ((line (input-port/read-line (nntp-connection:port connection))))
    (if (eof-object? line)
        (nntp-error line))
    line))

(define (nntp-response-number line)
  (if (fix:< (string-length line) 3)
      (error "Malformed NNTP response:" line))
  (substring->nonnegative-integer line 0 3))

(define (nntp-read-text connection port per-line)
  (let loop ()
    (let ((line (nntp-read-line connection)))
      (if per-line (per-line))
      (let ((length (string-length line)))
	(cond ((fix:= 0 length)
	       (output-port/write-char port #\newline)
	       (loop))
	      ((char=? #\. (string-ref line 0))
	       (if (not (fix:= 1 length))
		   (begin
		     (output-port/write-substring port line 1 length)
		     (output-port/write-char port #\newline)
		     (loop))))
	      (else
	       (output-port/write-substring port line 0 length)
	       (output-port/write-char port #\newline)
	       (loop)))))))

(define (nntp-read-text-lines connection)
  (let loop ((lines '()))
    (let ((line (nntp-read-line connection)))
      (let ((length (string-length line)))
	(cond ((or (fix:= 0 length)
		   (not (char=? #\. (string-ref line 0))))
	       (loop (cons line lines)))
	      ((fix:= 1 length)
	       (reverse! lines))
	      (else
	       (loop (cons (string-tail line 1) lines))))))))

;;;; News-Group Data Structure

(define-structure (news-group
		   (conc-name news-group:)
		   (constructor %make-news-group (connection name)))
  (connection #f read-only #t)
  (name #f read-only #t)
  (%header-table #f)
  (%header-gdbf #f)
  (%body-gdbf #f)
  (%estimated-n-articles #f)
  (%first-article #f)
  (%last-article #f)
  (reader-hook #f)
  (%use-gdbm? 'UNDECIDED))

(define (make-news-group connection name)
  (or (find-news-group connection name)
      (let ((group (%make-news-group connection name)))
	(nntp-connection:remember-group! connection name group)
	group)))

(define-integrable (news-group:server group)
  (nntp-connection:server (news-group:connection group)))

(define (news-group:< x y)
  (string<? (news-group:name x) (news-group:name y)))

(define (find-active-news-group connection name)
  (let ((group (find-news-group connection name)))
    (if group
	(and (news-group:active? group) group)
	(let ((server-info (nntp-group-command connection name)))
	  (and (not (eq? 'NO-SUCH-GROUP server-info))
	       (let ((group (make-news-group connection name)))
		 (news-group:maybe-save-server-info! group server-info)
		 group))))))

(define (news-group:active? group)
  (if (not (news-group:%estimated-n-articles group))
      (news-group:update-server-info! group))
  (not (eq? 'NO-SUCH-GROUP (news-group:%estimated-n-articles group))))

(define (news-group:estimated-n-articles group)
  (and (news-group:active? group) (news-group:%estimated-n-articles group)))

(define (news-group:first-article group)
  (and (news-group:active? group) (news-group:%first-article group)))

(define (news-group:last-article group)
  (and (news-group:active? group) (news-group:%last-article group)))

(define (news-group:update-server-info! group)
  (set-news-group:server-info!
   group
   (nntp-group-command (news-group:connection group)
		       (news-group:name group))))

(define (news-group:maybe-save-server-info! group server-info)
  (if (not (news-group:%estimated-n-articles group))
      (set-news-group:server-info! group server-info)))

(define (set-news-group:server-info! group info)
  (if (vector? info)
      (begin
	(set-news-group:%estimated-n-articles! group (vector-ref info 0))
	(set-news-group:%first-article! group (vector-ref info 1))
	(set-news-group:%last-article! group (vector-ref info 2)))
      (begin
	(set-news-group:%estimated-n-articles! group info)
	(set-news-group:%first-article! group #f)
	(set-news-group:%last-article! group #f))))

(define (news-group:server-info group)
  (if (eq? 'NO-SUCH-GROUP (news-group:%estimated-n-articles group))
      (news-group:%estimated-n-articles group)
      (vector (news-group:%estimated-n-articles group)
	      (news-group:%first-article group)
	      (news-group:%last-article group))))

(define (news-group:use-gdbm? group type)
  (and (gdbm-available?)
       (memq type (news-group:%use-gdbm? group))))

(define (set-news-group:use-gdbm! group types)
  (set-news-group:%use-gdbm?! group types))

;;;; Header Cache

(define (news-group:header-table group)
  (or (news-group:%header-table group)
      (let ((table (make-header-hash-table)))
	(set-news-group:%header-table! group table)
	table)))

(define make-header-hash-table
  (strong-hash-table/constructor remainder = #f))

(define (news-group:header group number)
  (let ((table (news-group:header-table group)))
    (or (hash-table/get table number #f)
	(let ((header (parse-header group (get-header group number))))
	  (if (news-header? header)
	      (hash-table/put! table number header))
	  header))))

(define (news-group:id->header group id allow-server-probes?)
  (let ((reply (news-group:id->pre-read-header group id)))
    (if reply
	(parse-header group reply)
	(and allow-server-probes?
	     (let ((header (parse-header group (read-header group id #t))))
	       (and (news-header? header)
		    (let ((table (news-group:header-table group))
			  (number (news-header:number header)))
		      (or (hash-table/get table number #f)
			  (begin
			    (hash-table/put! table number header)
			    header)))))))))

(define (news-group:id->pre-read-header group id)
  (let ((gdbf (news-group:header-gdbf group #f)))
    (and gdbf
	 (let ((key (gdbm-fetch gdbf id)))
	   (and key
		(get-pre-read-header gdbf key))))))

(define (news-group:cached-header group number)
  (and (news-group:%header-table group)
       (hash-table/get (news-group:%header-table group) number #f)))

(define (news-group:purge-header-cache group predicate)
  (let ((table (news-group:%header-table group)))
    (if table
	(if (eq? 'ALL predicate)
	    (hash-table/clear! table)
	    (hash-table/for-each table
	      (lambda (number header)
		(if (and (news-header? header) (predicate header #f))
		    (hash-table/remove! table number))))))))

(define (news-group:discard-cached-header! header)
  (let ((group (news-header:group header)))
    (if (news-group:%header-table group)
	(hash-table/remove! (news-group:%header-table group)
			    (news-header:number header)))))

(define (news-group:cached-headers group)
  (let ((table (news-group:%header-table group)))
    (if table
	(hash-table/datum-list table)
	'())))

(define (news-group:headers group numbers ignore?)
  (call-with-values (lambda () (cached-headers group numbers ignore?))
    (lambda (headers numbers)
      (cond ((null? numbers)
	     headers)
	    ((news-group:use-gdbm? group 'HEADERS)
	     (news-group:headers-gdbm group numbers headers ignore?))
	    (else
	     (news-group:headers-no-gdbm group numbers headers ignore?))))))

(define (cached-headers group numbers ignore?)
  (let ((table (news-group:%header-table group)))
    (if table
	(let loop ((numbers numbers) (headers '()) (numbers* '()))
	  (if (null? numbers)
	      (values headers (reverse! numbers*))
	      (let ((header (hash-table/get table (car numbers) #f)))
		(if (not header)
		    (loop (cdr numbers)
			  headers
			  (cons (car numbers) numbers*))
		    (loop (cdr numbers)
			  (cons (if (ignore? header)
				    (begin
				      (hash-table/remove! table (car numbers))
				      (cons 'IGNORED-ARTICLE (car numbers)))
				    header)
				headers)
			  numbers*)))))
	(values '() numbers))))

(define (news-group:headers-gdbm group numbers headers ignore?)
  (if (not (nntp-connection:closed? (news-group:connection group)))
      (news-group:pre-read-headers group numbers))
  (let* ((n-to-parse (length numbers))
	 (msg
	  (string-append "Parsing "
			 (number->string n-to-parse)
			 " header"
			 (if (fix:= n-to-parse 1) "" "s")
			 " from "
			 (news-group:name group)
			 "... "))
	 (gdbf (news-group:header-gdbf group #t)))
    (message msg)
    (let loop ((numbers numbers) (n 0) (headers headers))
      (if (null? numbers)
	  (begin
	    (message msg "done")
	    headers)
	  (let ((number (car numbers))
		(n (fix:+ n 1)))
	    (if (fix:= 0 (fix:remainder n 128))
		(message msg n " (" (integer-round (* n 100) n-to-parse) "%)"))
	    (loop (cdr numbers)
		  n
		  (adjoin-header group
				 number
				 (get-pre-read-header
				  gdbf
				  (number->string number))
				 ignore?
				 headers)))))))

(define (news-group:headers-no-gdbm group numbers headers ignore?)
  (read-headers group numbers #t headers
		(lambda (number reply headers)
		  (adjoin-header group number reply ignore? headers))))

(define (adjoin-header group number reply ignore? headers)
  (let ((header (parse-header group reply)))
    (cond ((not (news-header? header))
	   (cons (cons header number) headers))
	  ((ignore? header)
	   headers)
	  (else
	   (hash-table/put! (news-group:header-table group) number header)
	   (cons header headers)))))

;;;; Header Database

(define (news-group:header-gdbf group create?)
  (let ((gdbf (news-group:%header-gdbf group)))
    (if gdbf
	(if (eq? 'UNAVAILABLE gdbf) #f gdbf)
	(let ((gdbf
	       (and (news-group:use-gdbm? group 'HEADERS)
		    (let ((pathname
			   (news-group:header-gdbf-pathname group)))
		      (guarantee-init-file-directory pathname)
		      (and (or create? (file-exists? pathname))
			   (gdbm-open pathname
				      0
				      (fix:+ GDBM_WRCREAT GDBM_FAST)
				      #o666))))))
	  (set-news-group:%header-gdbf! group gdbf)
	  gdbf))))

(define (news-group:header-gdbf-pathname group)
  (init-file-specifier->pathname
   (list "snr" (news-group:server group) "headers" (news-group:name group))))

(define (news-group:pre-read-headers group numbers)
  (let ((gdbf (news-group:header-gdbf group #t)))
    (if gdbf
	(let ((keys
	       (list-transform-negative (map ->key numbers)
		 (lambda (key)
		   (gdbm-exists? gdbf key)))))
	  (if (not (null? keys))
	      (read-headers group keys #t '()
			    (lambda (key reply replies)
			      (store-header gdbf key reply)
			      replies)))))))

(define (get-header group number)
  (let ((gdbf (news-group:header-gdbf group #t)))
    (if gdbf
	(let ((key (->key number)))
	  (or (get-pre-read-header gdbf key)
	      (if (nntp-connection:closed? (news-group:connection group))
		  'UNREACHABLE-ARTICLE
		  (let ((reply (read-header group number #t)))
		    (store-header gdbf key reply)
		    reply))))
	(read-header group number #t))))

(define (get-pre-read-header gdbf key)
  (let ((datum (gdbm-fetch gdbf key)))
    (and datum
	 (let ((length (string-length datum)))
	   (if (fix:= length 0)
	       'NO-SUCH-ARTICLE
	       (let* ((n1 (find-next-newline datum 0 length))
		      (n1+1 (fix:+ n1 1))
		      (n2 (find-next-newline datum n1+1 length)))
		 (vector (substring datum 0 n1)
			 (substring datum n1+1 n2)
			 (substring datum (fix:+ n2 1) length))))))))

(define (store-header gdbf key reply)
  (if (vector? reply)
      (begin
	(gdbm-store gdbf
		    key
		    (string-append (vector-ref reply 0)
				   "\n"
				   (vector-ref reply 1)
				   "\n"
				   (vector-ref reply 2))
		    GDBM_REPLACE)
	(gdbm-store gdbf
		    (vector-ref reply 1)
		    (vector-ref reply 0)
		    GDBM_REPLACE))
      (gdbm-store gdbf key "" GDBM_REPLACE)))

;;;; Body Database

(define (news-group:body-gdbf group create?)
  (let ((gdbf (news-group:%body-gdbf group)))
    (if gdbf
	(if (eq? 'UNAVAILABLE gdbf) #f gdbf)
	(let ((gdbf
	       (and (news-group:use-gdbm? group 'BODIES)
		    (let ((pathname
			   (news-group:body-gdbf-pathname group)))
		      (guarantee-init-file-directory pathname)
		      (and (or create? (file-exists? pathname))
			   (gdbm-open pathname
				      0
				      (fix:+ GDBM_WRCREAT GDBM_FAST)
				      #o666))))))
	  (set-news-group:%body-gdbf! group gdbf)
	  gdbf))))

(define (news-group:body-gdbf-pathname group)
  (init-file-specifier->pathname
   (list "snr" (news-group:server group) "bodies" (news-group:name group))))

(define (news-header:read-body header port)
  (let ((group (news-header:group header))
	(number (get-header-number header)))
    (if number
	(let ((gdbf (news-group:body-gdbf group #t)))
	  (if gdbf
	      (let ((body
		     (or (gdbm-fetch gdbf number)
			 (pre-read-body group number))))
		(and body
		     (begin
		       (write-string body port)
		       #t)))
	      (begin
		(maybe-switch-groups group)
		(nntp-body-command (news-group:connection group)
				   number
				   port))))
	(nntp-body-command (news-group:connection group)
			   (news-header:message-id header)
			   port))))

(define (news-header:pre-read-body header)
  (let ((group (news-header:group header)))
    (let ((gdbf (news-group:body-gdbf group #t)))
      (if gdbf
	  (let ((key (get-header-number header)))
	    (if (not (gdbm-exists? gdbf key))
		(pre-read-body group key)))))))

(define (news-header:pre-read-body? header)
  (let ((gdbf (news-group:body-gdbf (news-header:group header) #f)))
    (and gdbf
	 (gdbm-exists? gdbf (get-header-number header)))))

(define (get-header-number header)
  (let ((number (news-header:number header)))
    (if number
	(number->string number)
	(let ((gdbf (news-group:header-gdbf (news-header:group header) #f)))
	  (and gdbf
	       (gdbm-fetch gdbf (news-header:message-id header)))))))

(define (pre-read-body group key)
  (let ((valid?))
    (let ((datum
	   (call-with-output-string
	    (lambda (port)
	      (maybe-switch-groups group)
	      (set! valid?
		    (nntp-body-command (news-group:connection group)
				       key
				       port))
	      unspecific))))
      (and valid?
	   (begin
	     (gdbm-store (news-group:body-gdbf group #t) key datum
			 GDBM_REPLACE)
	     datum)))))

(define (news-group:purge-pre-read-headers group predicate)
  (if (news-group:use-gdbm? group 'HEADERS)
      (if (eq? predicate 'ALL)
	  (begin
	    (set-news-group:%header-gdbf! group #f)
	    (set-news-group:%body-gdbf! group #f)
	    (delete-file-no-errors (news-group:header-gdbf-pathname group))
	    (delete-file-no-errors (news-group:body-gdbf-pathname group)))
	  (purge-pre-read-headers-1 group predicate))))

(define (purge-pre-read-headers-1 group predicate)
  (let ((header-gdbf (news-group:header-gdbf group #f))
	(body-gdbf (news-group:body-gdbf group #f)))
    (cond (header-gdbf
	   ;; Purge all headers satisfying PREDICATE.
	   (gdbm-purge header-gdbf
	     (lambda (key)
	       (and (string->number key)
		    (let ((header (parse-header group (get-header group key))))
		      (or (not (news-header? header))
			  (predicate header))))))
	   ;; Purge all orphaned message-id entries.
	   (gdbm-purge header-gdbf
	     (lambda (key)
	       (and (not (string->number key))
		    (not (gdbm-fetch header-gdbf
				     (gdbm-fetch header-gdbf key))))))
	   (gdbm-reorganize header-gdbf)
	   (if body-gdbf
	       (begin
		 ;; Purge all orphaned bodies.
		 (gdbm-purge body-gdbf
		   (lambda (key)
		     (not (gdbm-fetch header-gdbf key))))
		 (gdbm-reorganize body-gdbf)
		 (gdbm-close body-gdbf)
		 (set-news-group:%body-gdbf! group #f)))
	   (gdbm-close header-gdbf)
	   (set-news-group:%header-gdbf! group #f))
	  (body-gdbf
	   (gdbm-close body-gdbf)
	   (set-news-group:%body-gdbf! group #f)
	   (delete-file-no-errors (news-group:body-gdbf-pathname group))))))

(define (gdbm-purge gdbf predicate)
  (let loop ((key (gdbm-firstkey gdbf)) (keys '()))
    (if (not key)
	(for-each (lambda (key) (gdbm-delete gdbf key)) keys)
	(loop (gdbm-nextkey gdbf key)
	      (if (predicate key) (cons key keys) keys)))))

(define (news-group:close-database group)
  (let ((header-gdbf (news-group:header-gdbf group #f)))
    (if header-gdbf
	(begin
	  (gdbm-close header-gdbf)
	  (set-news-group:%header-gdbf! group #f))))
  (let ((body-gdbf (news-group:body-gdbf group #f)))
    (if body-gdbf
	(begin
	  (gdbm-close body-gdbf)
	  (set-news-group:%body-gdbf! group #f)))))

;;;; Read Headers

(define (read-header group specifier prune?)
  (let ((connection (news-group:connection group)))
    (nntp-protect connection
      (lambda ()
	(let ((switch? (maybe-request-group-switch connection group)))
	  (nntp-head-request connection (->key specifier))
	  (nntp-drain-output connection)
	  (maybe-reply-group-switch connection group switch?)
	  (nntp-head-reply connection prune?))))))

(define (->key object)
  (if (string? object)
      object
      (number->string object)))

(define (maybe-switch-groups group)
  (let ((connection (news-group:connection group)))
    (let ((switch? (maybe-request-group-switch connection group)))
      (if switch?
	  (nntp-drain-output connection))
      (maybe-reply-group-switch connection group switch?))))

(define (maybe-request-group-switch connection group)
  (if (nntp-connection:current-group? connection (news-group:name group))
      #f
      (nntp-protect connection
	(lambda ()
	  (nntp-group-request connection (news-group:name group))
	  #t))))

(define (maybe-reply-group-switch connection group switch?)
  (if switch?
      (news-group:maybe-save-server-info!
       group
       (nntp-protect connection
	 (lambda ()
	   (nntp-group-reply connection))))))

(define (read-headers group numbers prune? replies combine-replies)
  (let ((n-to-read (length numbers))
	(connection (news-group:connection group))
	(n-received 0)
	(n-chunk (nntp-head-request-count)))
    (let ((msg
	   (string-append "Reading "
			  (number->string n-to-read)
			  " header"
			  (if (fix:= n-to-read 1) "" "s")
			  " from "
			  (news-group:name group)
			  "... ")))

      (define (send-requests numbers n)
	(do ((numbers numbers (cdr numbers))
	     (n n (fix:- n 1)))
	    ((fix:= n 0)
	     (nntp-drain-output connection)
	     numbers)
	  (nntp-head-request connection (->key (car numbers)))))

      (define (receive-replies numbers numbers* replies)
	(do ((numbers numbers (cdr numbers))
	     (replies replies
		      (combine-replies (car numbers)
				       (nntp-head-reply connection prune?)
				       replies)))
	    ((eq? numbers numbers*) replies)
	  (if (fix:= 0 (fix:remainder n-received 16))
	      (message msg
		       n-received
		       " ("
		       (integer-round (* n-received 100) n-to-read)
		       "%)"))
	  (set! n-received (fix:+ n-received 1))))

      (message msg)
      (nntp-protect connection
	(lambda ()
	  (let ((switch? (maybe-request-group-switch connection group))
		(n
		 (min n-to-read
		      (* n-chunk (quotient nntp-maximum-request n-chunk)))))
	    (let ((txlist (send-requests numbers n)))
	      (maybe-reply-group-switch connection group switch?)
	      (let loop
		  ((txn (- n-to-read n))
		   (txlist txlist)
		   (rxn n-to-read)
		   (rxlist numbers)
		   (replies replies))
		(if (null? rxlist)
		    (begin
		      (message msg "done")
		      (reverse! replies))
		    (let* ((rxd (min rxn n-chunk))
			   (rxlist* (list-tail rxlist rxd))
			   (replies (receive-replies rxlist rxlist* replies))
			   (txd (min txn n-chunk)))
		      (loop (- txn txd)
			    (send-requests txlist txd)
			    (- rxn rxd)
			    rxlist*
			    replies)))))))))))

;;;; Parse Headers

(define (parse-header group reply)
  (if (vector? reply)
      (let ((header
	     (make-news-header group
			       (parse-message-number (vector-ref reply 0))
			       (vector-ref reply 1)
			       (vector-ref reply 2))))
	(if (news-header:number header)
	    header
	    (let ((entry
		   (assoc (news-group:name group) (news-header:xref header))))
	      (and entry
		   (let ((n (parse-message-number (cdr entry))))
		     (and n
			  (begin
			    (set-news-header:number! header n)
			    header)))))))
      reply))

(define (header-lines->text lines)
  (header-alist->text (parse-header-lines lines)))

(define (parse-header-lines lines)
  (cond ((null? lines)
	 '())
	((and (not (string-null? (car lines)))
	      (not (or (char=? #\space (string-ref (car lines) 0))
		       (char=? #\tab (string-ref (car lines) 0))))
	      (string-find-next-char (car lines) #\:))
	 => (lambda (colon)
	      (let ((unfold
		     (lambda (rest)
		       (let ((end (string-length (car lines))))
			 (cons (substring-trim (car lines) 0 colon)
			       (let ((value
				      (substring-trim (car lines)
						      (fix:+ colon 1)
						      end)))
				 (if (null? rest)
				     value
				     (apply string-append
					    value
					    (append-map
					     (lambda (string)
					       (list " "
						     (string-trim string)))
					     (reverse! rest))))))))))
		(let loop ((lines (cdr lines)) (rest '()))
		  (cond ((null? lines)
			 (list (unfold rest)))
			((and (not (string-null? (car lines)))
			      (or (char=? #\space (string-ref (car lines) 0))
				  (char=? #\tab (string-ref (car lines) 0)))
			      (string-find-next-char-in-set
			       (car lines) char-set:not-whitespace))
			 (loop (cdr lines) (cons (car lines) rest)))
			(else
			 (cons (unfold rest) (parse-header-lines lines))))))))
	(else
	 (parse-header-lines (cdr lines)))))

(define (header-alist->text alist)
  (apply string-append
	 (cons "\n"
	       (append-map (lambda (entry)
			     (list (car entry) ": " (cdr entry) "\n"))
			   (prune-header-alist alist)))))

(define (prune-header-alist alist)
  (list-transform-positive alist
    (lambda (entry)
      (or (string-ci=? (car entry) "subject")
	  (string-ci=? (car entry) "references")
	  (string-ci=? (car entry) "from")
	  (string-ci=? (car entry) "lines")
	  (string-ci=? (car entry) "xref")))))

(define (header-text-parser name)
  (let ((key (string-append name ":")))
    (lambda (text)
      (let ((start (find-header text key)))
	(if start
	    (apply string-append
		   (reverse!
		    (let ((end (string-length text)))
		      (let loop ((start start) (strings '()))
			(let ((index (find-next-newline text start end))
			      (accum
			       (lambda (end)
				 (cons (substring-trim text start end)
				       (if (null? strings)
					   strings
					   (cons " " strings))))))
			  (if index
			      (let ((strings (accum index))
				    (index (fix:+ index 1)))
				(if (or (fix:= index end)
					(not
					 (let ((char (string-ref text index)))
					   (or (char=? char #\space)
					       (char=? char #\tab)))))
				    strings
				    (loop index strings)))
			      (accum end)))))))
	    "")))))

(define (find-header text key)
  (let ((end (string-length text))
	(n (string-length key)))
    (let loop ((start 0))
      (let ((end* (fix:+ start n)))
	(if (and (fix:<= end* end)
		 (substring-ci=? text start end* key 0 n))
	    (substring-skip-leading-space string end* end)
	    (let ((nl (find-next-newline text start end)))
	      (and nl
		   (loop (fix:+ nl 1)))))))))

(define (find-next-newline string start end)
  (and (fix:< start end)
       (if (char=? #\newline (string-ref string start))
	   start
	   (find-next-newline string (fix:+ start 1) end))))

(define (parse-message-number n)
  (let ((n (substring->nonnegative-integer n 0 (string-length n))))
    (and n
	 (> n 0)
	 n)))

(define (valid-article-number? string)
  (let ((end (string-length string)))
    (and (let loop ((index 0))
	   (and (not (fix:= index end))
		(or (not (char=? #\0 (string-ref string index)))
		    (loop (fix:+ index 1)))))
	 (let loop ((index 0))
	   (or (fix:= index end)
	       (and (fix:<= (char->integer #\0)
			    (char->integer (string-ref string index)))
		    (fix:<= (char->integer (string-ref string index))
			    (char->integer #\9)))
	       (loop (fix:+ index 1)))))))

(define (valid-message-id? string)
  (let ((end (string-length string)))
    (and (fix:> end 2)
	 (char=? #\< (string-ref string 0))
	 (let loop ((index 1))
	   (and (not (fix:= index end))
		(if (char=? #\> (string-ref string index))
		    (fix:= (fix:+ index 1) end)
		    (and (not (char=? #\space (string-ref string index)))
			 (not (char=? #\< (string-ref string index)))
			 (loop (fix:+ index 1)))))))))

;;;; News-Header Data Structure

(define-structure (news-header
		   (conc-name news-header:)
		   (constructor make-news-header
				(group number message-id text)))
  (group #f read-only #t)
  number
  (message-id #f read-only #t)
  (text #f)
  (followup-to #f)
  (followups '())
  (thread #f)
  (reader-hook #f))

(define (dummy-news-header group message-id)
  (make-news-header group #f message-id #f))

(define-integrable news-header:real? news-header:text)

(define (field-value-accessor name)
  (let ((parser (header-text-parser name)))
    (lambda (header)
      (parser (news-header:text header)))))

(define news-header:subject (field-value-accessor "subject"))
(define news-header:references (field-value-accessor "references"))
(define news-header:from (field-value-accessor "from"))
(define news-header:n-lines (field-value-accessor "lines"))
(define news-header:%xref (field-value-accessor "xref"))

(define (news-header:field-value header name)
  ((header-text-parser name) (news-header:text header)))

(define (news-header:< x y)
  (< (news-header:number x) (news-header:number y)))

(define (news-header:xref header)
  (let loop ((tokens (string-tokenize (news-header:%xref header))))
    (if (null? tokens)
	tokens
	(let ((colon (string-find-next-char (car tokens) #\:))
	      (rest (loop (cdr tokens))))
	  (if colon
	      (cons (cons (string-head (car tokens) colon)
			  (string-tail (car tokens) (fix:+ colon 1)))
		    rest)
	      rest)))))

(define (news-header:guarantee-full-text! header)
  (let ((text (news-header:text header)))
    (if (pruned-header-text? text)
	(let ((reply
	       (get-full-header (news-header:group header)
				(news-header:number header))))
	   (if (vector? reply)
	       (set-news-header:text! header (vector-ref reply 2)))))))

(define (get-full-header group number)
  (let ((gdbf (news-group:header-gdbf group #t)))
    (if gdbf
	(let ((key (->key number)))
	  (let ((reply (get-pre-read-header gdbf key)))
	    (if (and (vector? reply)
		     (pruned-header-text? (vector-ref reply 2)))
		(let ((reply (read-header group number #f)))
		  (store-header gdbf key reply)
		  reply)
		reply)))
	(read-header group number #f))))

(define (pruned-header-text? text)
  (and (not (string-null? text))
       (char=? (string-ref text 0) #\newline)))

;;;; Conversation Threads

;;; This is by far the hairiest part of this implementation.  Headers
;;; are first organized into trees based on their "references" fields.
;;; The tree structure is reflected in their FOLLOWUP-TO and FOLLOWUPS
;;; fields.  These trees are then gathered into threads by means of
;;; subject matching.  Each resulting thread consists of a list of
;;; these trees, represented by the tree roots.  The list is sorted by
;;; the header order of the roots.

;;; Considerable additional hair is required because there are
;;; numerous broken posting agents in the world.  In principle, the
;;; references fields of News messages contains an ordered list of
;;; message IDs, but in practice, each of these IDs must be checked
;;; for syntactic validity, and the order must be ignored since some
;;; posting agents mangle it.  The only property that seems valid is
;;; that referenced message IDs are predecessors in the thread, but
;;; even this must be qualified by a graph algorithm that detects
;;; cycles and breaks them.

(define-structure (news-thread
		   (conc-name news-thread:)
		   (constructor make-news-thread (root)))
  (root #f)
  (reader-hook #f))

(define (news-thread:< x y)
  (news-header:< (news-thread:root x) (news-thread:root y)))

(define (news-thread:for-each-header thread procedure)
  (let loop ((header (news-thread:root thread)))
    (procedure header)
    (for-each loop (news-header:followups header))))

(define (organize-headers-into-threads headers
				       show-context?
				       allow-server-probes?
				       split-different-subjects?
				       join-same-subjects?)
  (sort (let ((threads
	       (associate-threads-with-trees
		(build-followup-trees! headers
				       show-context?
				       allow-server-probes?
				       split-different-subjects?))))
	  (if join-same-subjects?
	      (map make-threads-equivalent!
		   (build-equivalence-classes
		    threads
		    (find-subject-associations threads)))
	      threads))
	news-thread:<))

;;; Organize headers into heterarchies based on References: fields.

(define (build-followup-trees! headers
			       show-context?
			       allow-server-probes?
			       split-different-subjects?)
  (call-with-values
      (lambda ()
	(map-references-to-headers headers show-context? allow-server-probes?))
    (lambda (headers dummy-headers)
      (let ((headers (append dummy-headers headers)))
	(convert-header-graphs-to-trees headers)
	(simplify-followup-to-links headers)
	(canonicalize-tree-ordering headers))
      (if split-different-subjects?
	  (split-trees-on-subject-changes headers))
      (append! (discard-useless-dummy-headers dummy-headers) headers))))

(define (map-references-to-headers headers show-context? allow-server-probes?)
  (let ((id-table (make-string-hash-table))
	(queue (make-queue))
	(dummy-headers '()))

    (define (init-header header)
      (set-news-header:followup-to! header (news-header:reference-list header))
      (set-news-header:followups! header '())
      (set-news-header:thread! header #f)
      (hash-table/put! id-table (news-header:message-id header) header))

    (for-each init-header headers)
    (for-each (lambda (header) (enqueue!/unsafe queue header)) headers)
    (queue-map!/unsafe queue
      (lambda (header)
	(let ((group (news-header:group header)))
	  (set-news-header:followup-to!
	   header
	   (remove-duplicates
	    (map
	     (lambda (id)
	       (or (hash-table/get id-table id #f)
		   (and show-context?
			(let ((header
			       (news-group:id->header
				group id allow-server-probes?)))
			  (and (news-header? header)
			       (begin
				 (if (eq? (hash-table/get id-table id #t)
					  #t)
				     (begin
				       (set! headers (cons header headers))
				       (init-header header)
				       (if (not (queued?/unsafe queue header))
					   (enqueue!/unsafe queue header))))
				 header))))
		   (let ((header (dummy-news-header group id)))
		     (set! dummy-headers (cons header dummy-headers))
		     (init-header header)
		     header)))
	     (news-header:followup-to header)))))))
    (for-each
     (lambda (header)
       (for-each
	(lambda (ref)
	  (set-news-header:followups!
	   ref
	   (cons header (news-header:followups ref))))
	(news-header:followup-to header)))
     headers)
    (values headers dummy-headers)))

(define (news-header:reference-list header)
  (if (news-header:real? header)
      ;; Check the references header field to guarantee that it's
      ;; well-formed, and discard it entirely if it isn't.  This paranoia
      ;; is reasonable since I've already seen bad references during the
      ;; first few days of testing.
      (let ((tokens (parse-references-list (news-header:references header))))
	(if (for-all? tokens valid-message-id?)
	    tokens
	    '()))
      '()))

(define (parse-references-list refs)
  (let ((end (string-length refs)))

    (define (find-ref-start index)
      (and (fix:< index end)
	   (if (char=? #\< (string-ref refs index))
	       index
	       (find-ref-start (fix:+ index 1)))))

    (define (find-ref-end index)
      (and (fix:< index end)
	   (if (char=? #\> (string-ref refs index))
	       (fix:+ index 1)
	       (find-ref-end (fix:+ index 1)))))

    (let loop ((index 0) (result '()))
      (let ((start (find-ref-start index)))
	(if start
	    (let ((end (find-ref-end (fix:+ start 1))))
	      (if end
		  (loop end (cons (substring refs start end) result))
		  (reverse! result)))
	    (reverse! result))))))

;;; Convert the header heterarchies into trees by eliminating
;;; redundant paths to the ancestors of a header.

(define (convert-header-graphs-to-trees headers)
  (let ((tables (cons (make-eq-hash-table) (make-eq-hash-table))))
    (for-each (lambda (header)
		(if (eq? (hash-table/get (car tables) header 'NONE) 'NONE)
		    (eliminate-redundant-relatives tables header)))
	      headers)
    (let loop ()
      (let ((changes? #f))
	(for-each (lambda (header)
		    (if (eliminate-extra-parent tables header)
			(begin (set! changes? #t) unspecific)))
		  headers)
	(if changes? (loop))))))

(define (eliminate-redundant-relatives tables header)
  (let ((do-header
	 (lambda (header)
	   (for-each
	    (lambda (parent) (unlink-headers! parent header))
	    (compute-redundant-relatives news-header:followup-to
					 (car tables)
					 header))
	   (for-each
	    (lambda (child) (unlink-headers! header child))
	    (compute-redundant-relatives news-header:followups
					 (cdr tables)
					 header)))))
    (let loop ((header header))
      (do-header header)
      (for-each loop (news-header:followup-to header)))
    (let loop ((header header))
      (do-header header)
      (for-each loop (news-header:followups header)))))

(define (eliminate-extra-parent tables header)
  (let ((parents (news-header:followup-to header)))
    (and (not (null? parents))
	 (not (null? (cdr parents)))
	 (let ((a (car parents))
	       (b (cadr parents))
	       (parent-is-ancestor?
		(lambda (a b)
		  (and (not (null? (news-header:followup-to a)))
		       (null? (cdr (news-header:followup-to a)))
		       (memq (car (news-header:followup-to a))
			     (compute-header-relatives news-header:followup-to
						       (car tables)
						       b)))))
	       (move-under
		(lambda (a b)
		  (unlink-headers! (car (news-header:followup-to a)) a)
		  (unlink-headers! b header)
		  (link-headers! b a)
		  (reset-caches! tables a)
		  (eliminate-redundant-relatives tables a)
		  #f)))
	   (cond ((parent-is-ancestor? a b)
		  (move-under a b))
		 ((parent-is-ancestor? b a)
		  (move-under b a))
		 (else
		  ;; Heuristic: because the followup-to field is in
		  ;; the same order that the original References:
		  ;; header was, unless a poster has munged the order,
		  ;; the leftmost entry is the oldest reference.
		  (let ((parents (list-copy (news-header:followup-to b))))
		    (for-each (lambda (p) (unlink-headers! p b)) parents)
		    (for-each (lambda (p) (link-headers! p a)) parents))
		  (unlink-headers! a header)
		  (link-headers! a b)
		  (reset-caches! tables a)
		  (eliminate-redundant-relatives tables a)
		  #t))))))

(define (compute-redundant-relatives step table header)
  (let ((relatives (step header)))
    (list-transform-positive relatives
      (lambda (child)
	(there-exists? relatives
	  (lambda (child*)
	    (and (not (eq? child* child))
		 (memq child
		       (compute-header-relatives step table child*)))))))))

(define (compute-header-relatives step table header)
  (let loop ((header header))
    (let ((cache (hash-table/get table header 'NONE)))
      (case cache
	((NONE)
	 (hash-table/put! table header 'PENDING)
	 (let ((result
		(reduce
		 unionq
		 '()
		 (let ((headers (step header)))
		   (cons headers
			 (map (lambda (header*)
				(let ((result (loop header*)))
				  (if (eq? 'CYCLE result)
				      (begin
					(if (eq? step news-header:followups)
					    (unlink-headers! header header*)
					    (unlink-headers! header* header))
					'())
				      result)))
			      headers))))))
	   (hash-table/put! table header result)
	   result))
	((PENDING)
	 ;;(error "Cycle detected in header graph:" header)
	 'CYCLE)
	(else cache)))))

(define (reset-caches! tables header)
  (let ((do-header
	 (lambda (header)
	   (hash-table/remove! (car tables) header)
	   (hash-table/remove! (cdr tables) header))))
    (let loop ((header header))
      (do-header header)
      (for-each loop (news-header:followup-to header)))
    (let loop ((header header))
      (do-header header)
      (for-each loop (news-header:followups header)))))

(define (unlink-headers! p c)
  (set-news-header:followups! p (delq! c (news-header:followups p)))
  (set-news-header:followup-to! c (delq! p (news-header:followup-to c))))

(define (link-headers! p c)
  (if (not (memq c (news-header:followups p)))
      (begin
	(set-news-header:followups! p (cons c (news-header:followups p)))
	(set-news-header:followup-to! c
				      (cons p (news-header:followup-to c))))))

;;; Change followup-to slots to point to a single header rather than a
;;; list of headers.  Eliminate dummy headers that have zero or one
;;; children.

(define (simplify-followup-to-links headers)
  (for-each (lambda (header)
	      (set-news-header:followup-to!
	       header
	       (let ((parents (news-header:followup-to header)))
		 (if (null? parents)
		     #f
		     (car parents)))))
	    headers))

(define (discard-useless-dummy-headers dummy-headers)
  (for-each maybe-discard-dummy-header dummy-headers)
  (list-transform-negative dummy-headers
    (lambda (header)
      (null? (news-header:followups header)))))

(define (maybe-discard-dummy-header header)
  (let ((children (news-header:followups header)))
    (cond ((null? children)
	   (let ((parent (news-header:followup-to header)))
	     (if parent
		 (begin
		   (disassociate-header-from-parent header parent)
		   (if (not (news-header:real? parent))
		       (maybe-discard-dummy-header parent))))))
	  ((null? (cdr children))
	   (let ((parent (news-header:followup-to header)))
	     (set-news-header:followup-to! (car children) parent)
	     (set-news-header:followups! header '())
	     (if parent
		 (begin
		   (set-car! (memq header (news-header:followups parent))
			     (car children))
		   (set-news-header:followup-to! header #f)
		   (if (not (news-header:real? parent))
		       (maybe-discard-dummy-header parent)))))))))

(define (canonicalize-tree-ordering headers)
  (for-each
   (lambda (header)
     (if (not (news-header:followup-to header))
	 (let loop ((header header))
	   (let ((followups (news-header:followups header)))
	     (for-each loop followups)
	     (set-news-header:followups! header
					 (sort followups news-header:<)))
	   (if (and (not (news-header:real? header))
		    (not (news-header:number header)))
	       (set-news-header:number!
		header
		(news-header:number (car (news-header:followups header))))))))
   headers))

(define (split-trees-on-subject-changes headers)
  (for-each
   (lambda (header)
     (if (news-header:real? header)
	 (let ((parent (news-header:followup-to header)))
	   (if (and parent
		    (not
		     (let ((subject
			    (if (news-header:real? parent)
				(news-header:subject parent)
				(find-tree-subject header))))
		       (memq
			(compare-subjects
			 (canonicalize-subject (news-header:subject header))
			 (canonicalize-subject subject))
			'(EQUAL LEFT-PREFIX)))))
	       (disassociate-header-from-parent header parent)))))
   headers))

(define (find-tree-subject header)
  (let ((parent (news-header:followup-to header)))
    (if parent
	(find-tree-subject parent)
	(let loop ((header header))
	  (if (news-header:real? header)
	      (news-header:subject header)
	      (let ((followups (news-header:followups header)))
		(if (null? followups)
		    (error "Thread tree has no subject!"))
		(loop (car followups))))))))

(define (disassociate-header-from-parent header parent)
  (set-news-header:followups! parent
			      (delq! header (news-header:followups parent)))
  (set-news-header:followup-to! header #f))

;;; Create a thread to represent each header tree, and mark the
;;; tree's headers as members of that thread.

(define (associate-threads-with-trees headers)
  (let ((threads '()))
    (for-each (lambda (header)
		(if (not (news-header:thread header))
		    (let ((root
			   (let loop ((header header))
			     (if (news-header:followup-to header)
				 (loop (news-header:followup-to header))
				 header))))
		      (let ((thread (make-news-thread root)))
			(set! threads (cons thread threads))
			(news-thread:for-each-header thread
			  (lambda (header)
			    (set-news-header:thread! header thread)))))))
	      headers)
    threads))

;;; Build a mapping from header subjects to threads.

(define (find-subject-associations threads)
  (let ((subject-alist '()))
    (for-each
     (lambda (thread)
       (news-thread:for-each-header thread
	 (lambda (header)
	   (if (news-header:real? header)
	       (let ((subject
		      (canonicalize-subject
		       (news-header:subject header))))
		 (if (not (string-null? subject))
		     (let ((entry (assoc-subject subject subject-alist)))
		       (cond ((not entry)
			      (set! subject-alist
				    (cons (list subject thread)
					  subject-alist))
			      unspecific)
			     ((not (memq thread (cdr entry)))
			      (set-cdr! entry
					(cons thread (cdr entry))))))))))))
     threads)
    subject-alist))

(define (canonicalize-subject subject)
  ;; This is optimized by assuming that the subject lines have no
  ;; leading or trailing white space.  The news-header parser makes
  ;; that guarantee.
  (let ((end (string-length subject)))
    (if (and (not (fix:= 0 end))
	     (or (char=? #\r (string-ref subject 0))
		 (char=? #\R (string-ref subject 0))))
	(let loop ((start 0))
	  (if (substring-prefix-ci? "re:" 0 3 subject start end)
	      (loop (substring-skip-leading-space subject
						  (fix:+ start 3)
						  end))
	      (if (fix:= start 0)
		  subject
		  (substring subject start end))))
	subject)))

(define (assoc-subject subject alist)
  (let loop ((alist alist))
    (and (not (null? alist))
	 (if (eq? 'EQUAL (compare-subjects subject (caar alist)))
	     (car alist)
	     (loop (cdr alist))))))

(define (compare-subjects x y)
  (let ((xe (string-length x))
	(ye (string-length y)))
    (let ((i (substring-match-forward-ci x 0 xe y 0 ye)))
      (if (fix:= i xe)
	  (if (fix:= i ye)
	      'EQUAL
	      (and (>= (/ xe ye) 3/4) 'LEFT-PREFIX))
	  (if (fix:= i ye)
	      (and (>= (/ ye xe) 3/4) 'RIGHT-PREFIX)
	      #f)))))

;;; Merge threads that have shared subjects, even though they lack
;;; common references.

(define (build-equivalence-classes threads subject-alist)
  (let ((equivalences (make-eq-hash-table)))
    (for-each (lambda (thread)
		(hash-table/put! equivalences
				 thread
				 (let ((t (list thread)))
				   (set-cdr! t (list t))
				   t)))
	      threads)
    (let ((equivalence!
	   (lambda (x y)
	     (let ((x (hash-table/get equivalences x #f))
		   (y (hash-table/get equivalences y #f)))
	       (if (not (eq? (cdr x) (cdr y)))
		   (let ((k
			  (lambda (x y)
			    (for-each (lambda (y) (set-cdr! y x)) y)
			    (set-cdr! (last-pair x) y))))
		     (if (news-thread:< (car x) (car y))
			 (k (cdr x) (cdr y))
			 (k (cdr y) (cdr x)))))))))
      (for-each (lambda (entry)
		  (let ((thread (cadr entry)))
		    (for-each (lambda (thread*) (equivalence! thread thread*))
			      (cddr entry))))
		subject-alist))
    (map (lambda (class) (map car class))
	 (remove-duplicates
	  (map cdr (hash-table/datum-list equivalences))))))

(define (make-threads-equivalent! threads)
  (let ((threads (sort threads news-thread:<)))
    (let ((thread (car threads))
	  (threads (cdr threads)))
      (if (not (null? threads))
	  (begin
	    (for-each (lambda (thread*)
			(news-thread:for-each-header thread*
			  (lambda (header)
			    (set-news-header:thread! header thread))))
		      threads)
	    (let ((dummy
		   (dummy-news-header
		    (news-header:group (news-thread:root thread))
		    #f))
		  (roots
		   (cons (news-thread:root thread)
			 (map news-thread:root threads))))
	      (set-news-header:thread! dummy thread)
	      (set-news-header:followups! dummy roots)
	      (for-each (lambda (header)
			  (set-news-header:followup-to! header dummy))
			roots)
	      (set-news-header:number! dummy (news-header:number (car roots)))
	      (set-news-thread:root! thread dummy))))
      thread)))

;;;; Miscellaneous

(define (input-port/discard-line port)
  (input-port/discard-chars port char-set:newline)
  (input-port/discard-char port))

(define char-set:newline (char-set #\newline))

(define (input-port/eof? port)
  ((port/operation port 'EOF?) port))

(define (write-init-file-atomically pathname procedure)
  (guarantee-init-file-directory pathname)
  (write-file-atomically pathname procedure))

(define (write-file-atomically pathname procedure)
  (let ((finished? #f))
    (dynamic-wind (lambda ()
		    unspecific)
		  (lambda ()
		    (let ((value (call-with-output-file pathname procedure)))
		      (set! finished? #t)
		      value))
		  (lambda ()
		    (if (not finished?)
			(delete-file-no-errors pathname))))))

(define (string-tokenize string #!optional white not-white)
  (let ((white (if (default-object? white) char-set:whitespace white))
	(not-white
	 (if (default-object? white) char-set:not-whitespace not-white))
	(end (string-length string)))
    (let loop ((start 0) (tokens '()))
      (if (fix:= start end)
	  (reverse! tokens)
	  (let ((delimiter
		 (or (substring-find-next-char-in-set string start end white)
		     end)))
	    (loop (or (substring-find-next-char-in-set
		       string delimiter end not-white)
		      end)
		  (cons (substring string start delimiter) tokens)))))))

(define (string-first-token string)
  (let ((index (string-find-next-char-in-set string char-set:whitespace)))
    (if index
	(string-head string index)
	string)))

(define (token->number token)
  (or (substring->nonnegative-integer token 0 (string-length token))
      (error:bad-range-argument token #f)))

(define (substring-skip-leading-space string start end)
  (let loop ((index start))
    (if (and (fix:< index end)
	     (or (char=? #\space (string-ref string index))
		 (char=? #\tab (string-ref string index))))
	(loop (fix:+ index 1))
	index)))

(define (substring-skip-trailing-space string start end)
  (let loop ((index end))
    (if (fix:< start index)
	(let ((index* (fix:- index 1)))
	  (if (or (char=? #\space (string-ref string index*))
		  (char=? #\tab (string-ref string index*)))
	      (loop index*)
	      index))
	index)))

(define (substring-trim string start end)
  (let ((start (substring-skip-leading-space string start end)))
    (substring string start (substring-skip-trailing-space string start end))))

(define (unionq x y)
  (if (null? y)
      x
      (let loop ((x x) (y y))
	(if (null? x)
	    y
	    (loop (cdr x) (if (memq (car x) y) y (cons (car x) y)))))))

(define (differenceq x y)
  (if (null? y)
      x
      (let loop ((x x) (z '()))
	(if (null? x)
	    (reverse! z)
	    (loop (cdr x) (if (memq (car x) y) z (cons (car x) z)))))))

(define (subsetq? x y)
  (or (null? x)
      (and (memq (car x) y)
	   (subsetq? (cdr x) y))))

(define (remove-duplicates items)
  (let loop ((items items) (result '()))
    (if (null? items)
	(reverse! result)
	(loop (cdr items)
	      (if (memq (car items) result)
		  result
		  (cons (car items) result))))))

(define (hash-table/modify! table key default modifier)
  (hash-table/put! table key (modifier (hash-table/get table key default))))

(define (map! procedure items)
  (do ((items items (cdr items)))
      ((null? items))
    (set-car! items (procedure (car items))))
  items)