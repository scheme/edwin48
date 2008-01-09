;;; -*-Scheme-*-
;;;
;;; $Id: replaz.scm,v 1.85 2007/01/05 21:19:24 cph Exp $
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

;;;; Replacement Commands


(define-variable case-replace
  "If true, means replacement commands should preserve case."
  #t
  boolean?)

(define (replace-string-arguments name)
  (let ((source (prompt-for-string name #f)))
    (list source
	  (prompt-for-string (string-append name " " source " with")
			     #f
			     'DEFAULT-TYPE 'NULL-DEFAULT)
	  (command-argument))))

(define-command replace-string
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if  case-replace  and  case-fold-search
are #t and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) #t means replace
only matches surrounded by word boundaries."
  (lambda () (replace-string-arguments "Replace string"))
  (lambda (from-string to-string delimited)
    (replace-string from-string to-string delimited #f #f)
    (message "Done")))

(define-command replace-regexp
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if case-replace and case-fold-search
are #t and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) #t means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (lambda () (replace-string-arguments "Replace regexp"))
  (lambda (regexp to-string delimited)
    (replace-string regexp to-string delimited #f #t)
    (message "Done")))

(define-command query-replace
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserve case in each replacement if  case-replace  and  case-fold-search
are #t and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) #t means replace
only matches surrounded by word boundaries."
  (lambda () (replace-string-arguments "Query replace"))
  (lambda (from-string to-string delimited)
    (replace-string from-string to-string delimited #t #f)
    (message "Done")))

(define-command query-replace-regexp
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserve case in each replacement if  case-replace  and  case-fold-search
are #t and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) #t means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (lambda () (replace-string-arguments "Query replace regexp"))
  (lambda (regexp to-string delimited)
    (replace-string regexp to-string delimited #t #t)
    (message "Done")))

(define (replace-string source target delimited? query? regexp?)
  ;; Returns TRUE iff the query loop was exited at the user's request,
  ;; FALSE iff the loop finished by failing to find an occurrence.
  (let ((preserve-case?
	 (and (ref-variable case-replace)
	      (ref-variable case-fold-search)
	      (string-lower-case? source)
	      (not (string-null? target))
	      (string-lower-case? target)))
	(source*
	 (if delimited?
	     (string-append "\\b"
			    (if regexp? source (re-quote-string source))
			    "\\b")
	     source))
	(message-string
	 (string-append "Query replacing " source " with " target)))

    (define (replacement-loop point)
      (undo-boundary! point)
      (let ((done
	     (lambda ()
	       (set-current-point! point)
	       (done #f))))
	(cond ((not (find-next-occurrence point))
	       (done))
	      ((mark< point (re-match-end 0))
	       (replacement-loop (perform-replacement #f)))
	      ((not (group-end? point))
	       (replacement-loop (mark1+ point)))
	      (else
	       (done)))))

    (define (query-loop point)
      (undo-boundary! point)
      (cond ((not (find-next-occurrence point))
	     (done #f))
	    ((mark< point (re-match-end 0))
	     (set-current-mark! point)
	     (set-current-point! (re-match-end 0))
	     (perform-query #f (re-match-data)))
	    ((not (group-end? point))
	     (query-loop (mark1+ point)))
	    (else
	     (done #f))))

    (define (find-next-occurrence start)
      (if (or regexp? delimited?)
	  (re-search-forward source* start (group-end start))
	  (search-forward source* start (group-end start))))

    (define (perform-replacement match-data)
      (if match-data (set-re-match-data! match-data))
      (replace-match target preserve-case? (not regexp?)))

    (define (done value)
      (pop-current-mark!)
      value)

    (define (perform-query replaced? match-data)
      (message message-string ":")
      (let ((key (with-editor-interrupts-disabled keyboard-peek)))
	(let ((test-for
	       (lambda (key*)
		 (and (char? key)
		      (char=? key (remap-alias-key key*))
		      (begin
			(keyboard-read)
			#t)))))
	  (cond ((test-for #\C-h)
		 (with-output-to-help-display
		  (lambda ()
		    (write-string message-string)
		    (write-string ".

Type space to replace one match, Rubout to skip to next,
Altmode to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-R to enter recursive edit, C-W to delete match and recursive edit,
! to replace all remaining matches with no more questions,
^ to move point back to previous match.")))
		 (perform-query replaced? match-data))
		((or (test-for #\altmode)
		     (test-for #\q))
		 (done #t))
		((test-for #\^)
		 (set-current-point! (current-mark))
		 (perform-query #t match-data))
		((or (test-for #\space)
		     (test-for #\y))
		 (if (not replaced?) (perform-replacement match-data))
		 (query-loop (current-point)))
		((test-for #\.)
		 (if (not replaced?) (perform-replacement match-data))
		 (done #t))
		((test-for #\,)
		 (if (not replaced?) (perform-replacement match-data))
		 (perform-query #t match-data))
		((test-for #\!)
		 (if (not replaced?) (perform-replacement match-data))
		 (replacement-loop (current-point)))
		((or (test-for #\rubout)
		     (test-for #\n))
		 (query-loop (current-point)))
		((test-for #\C-l)
		 ((ref-command recenter) #f)
		 (perform-query replaced? match-data))
		((test-for #\C-r)
		 (edit)
		 (perform-query replaced? match-data))
		((test-for #\C-w)
		 (if (not replaced?) (delete-match))
		 (edit)
		 (perform-query #t match-data))
		(else
		 (done #t))))))

    (define (edit)
      (clear-message)
      (save-excursion enter-recursive-edit))

    (let ((point (current-point)))
      (push-current-mark! point)
      (push-current-mark! point)
      (if query?
	  (query-loop point)
	  (replacement-loop point)))))
