;;; -*-Scheme-*-
;;;
;;; $Id: rmailsrt.scm,v 1.18 2007/01/05 21:19:24 cph Exp $
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

;;;; RMAIL Sorting Utilities


;; GNUS compatible key bindings.
(define-key 'rmail (list #\C-c #\C-s #\C-d) 'rmail-sort-by-date)
(define-key 'rmail (list #\C-c #\C-s #\C-s) 'rmail-sort-by-subject)
(define-key 'rmail (list #\C-c #\C-s #\C-a) 'rmail-sort-by-author)
(define-key 'rmail (list #\C-c #\C-s #\C-r) 'rmail-sort-by-recipient)
(define-key 'rmail (list #\C-c #\C-s #\C-l) 'rmail-sort-by-size-lines)

(define-command rmail-sort-by-date 
  "Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (lambda (memo)
       (fetch-first-field "date" (msg-memo/start memo) (msg-memo/end memo)))
     (lambda (x y)
       (string<? (rmail-sortable-date-string x)
		 (rmail-sortable-date-string y))))))

(define-command rmail-sort-by-subject 
  "Sort messages of current Rmail file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (let ((re-pattern (re-compile-pattern "^\\(re:[ \t]+\\)*" true)))
       (lambda (memo)
	 (let ((key
		(or (fetch-first-field "subject" 
				       (msg-memo/start memo)
				       (msg-memo/end memo))
		    "")))
	   ;; Remove `Re:'
	   (let ((r (re-string-match re-pattern key)))
	     (if r
		 (string-tail key (re-match-end-index 0 r))
		 key)))))
     string<?)))

(define-command rmail-sort-by-author 
  "Sort messages of current Rmail file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (lambda (memo)
       (let ((start (msg-memo/start memo))
	     (end (msg-memo/end memo)))
	 (mail-strip-quoted-names
	  (or (fetch-first-field "from" start end)
	      (fetch-first-field "sender" start end)))))
     string<?)))

(define-command rmail-sort-by-recipient 
  "Sort messages of current Rmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages 
     reverse
     (lambda (memo)
       (let ((start (msg-memo/start memo))
	     (end (msg-memo/end memo)))
	 (mail-strip-quoted-names
	  (or (fetch-first-field "to" start end)
	      (fetch-first-field "apparently-to" start end)))))
     string<?)))

(define-command rmail-sort-by-size-lines 
  "Sort messages of current Rmail file by message size.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  "P"
  (lambda (reverse)
    (rmail-sort-messages
     reverse
     (lambda (memo) (count-lines (msg-memo/start memo) (msg-memo/end memo)))
     <)))

(define rmail-sort-messages
  (lambda (reverse keyfunc cmpfunc)
    (let* ((current-msg-num (msg-memo/number (current-msg-memo)))
	   (nummsg (-1+ (msg-memo/number (last-msg-memo))))
	   (sort-vect (make-vector (1+ nummsg))))
      (message "Finding sort keys...")
      (widen)
      (set-buffer-writeable! (current-buffer))
      (let loop ((n 0)
		 (the-memo (msg-memo/first (current-msg-memo))))
	(let ((next (msg-memo/next the-memo)))
	  (if (= 9 (modulo n 10))
	      (message "Finding sort keys..." (1+ n)))
	  (vector-set! 
	   sort-vect n
	   (list (keyfunc the-memo)
		 (extract-string
		  (msg-memo/start the-memo)
		  (msg-memo/end the-memo))
		 the-memo))
	  (delete-string
	   (msg-memo/start the-memo)
	   (msg-memo/end the-memo))
	  (if next (loop (1+ n) next))))
      (if reverse
	  (set! sort-vect
		(list->vector (reverse (vector->list sort-vect)))))
      (sort! sort-vect
	     (lambda (x y)
	       (cmpfunc (car x) (car y))))
      (message "Reordering buffer...")
      (delete-string
       (msg-memo/start (msg-memo/first (current-msg-memo)))
       (msg-memo/end (msg-memo/last (current-msg-memo))))
      (let loop ((n 0)
		 (previous false)
		 (the-memo (caddr (vector-ref sort-vect 0)))
		 (next (if (>= nummsg 2)
			   (caddr (vector-ref sort-vect 1))
			   false)))
	(set-msg-memo/previous! the-memo previous)
	(set-msg-memo/next! the-memo next)
	(if (< n nummsg)
	    (begin
	      (insert-string (cadr (vector-ref sort-vect n)))
	      (vector-set! sort-vect n #f)
	      (if (= 9 (modulo n 10))
		  (message "Reordering buffer..." (1+ n)))
	      (loop (1+ n) the-memo next
		    (if (< (1+ n) nummsg)
			(caddr (vector-ref sort-vect (1+ n)))
			false)))
	    (insert-string (cadr (vector-ref sort-vect n)))))
      (set-buffer-read-only! (current-buffer))
      (set-buffer-msg-memo! (current-buffer) false)
      (memoize-buffer (current-buffer))
      (show-message (current-buffer) current-msg-num))))

;; Copy of the function gnus-comparable-date in gnus.el

(define rmail-sortable-date-string
  (lambda (date)
    (let ((month '(("JAN" . "01")
		   ("FEB" . "02")("MAR" . "03")
		   ("APR" . "04")("MAY" . "05")("JUN" . "06")
		   ("JUL" . "07")("AUG" . "08")("SEP" . "09")
		   ("OCT" . "10")("NOV" . "11")("DEC" . "12")
		   ("JANUARY" . "01")("FEBRUARY" . "02")("MARCH" . "03")
		   ("APRIL" . "04")("JUNE" . "06")("JULY" . "07")
		   ("AUGUST" . "08")("SEPTEMBER" . "09")("OCTOBER" . "10")
		   ("NOVEMBER" . "11")("DECEMBER" . "12")))
	  (date (or date "")))
      ;; Can understand the following styles:
      ;; (1) 14 Apr 89 03:20:12 GMT
      ;; (2) Fri, 17 Mar 89 4:01:33 GMT
      ;; (3) Fri, 3 Apr 92 18:55 EST
      ;;
      ;; added [ ]+ to the regexp to handle date string put out
      ;; by hx.lcs.mit.edu (they use 2 spaces instead of 1)
      ;; made seconds optional since research.att.com doesn't send it out
      (let ((r
	     (re-string-search-forward
	      "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\)[ ]+\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\):?\\([0-9]*\\)"
	      date)))
	(if r
	    (string-append
	     ;; Year
	     (let ((year
		    (string->number
		     (substring date
				(re-match-start-index 3 r)
				(re-match-end-index 3 r)))))
	       (let ((y1 (modulo year 100)))
		 (string-pad-left (number->string y1) 2)))
	     ;; Month
	     (cdr
	      (assoc
	       (string-upcase
		(substring (substring date
				      (re-match-start-index 2 r)
				      (re-match-end-index 2 r))
			   0 3))
	       month))
	     ;; Day
	     (let ((day
		    (substring date
			       (re-match-start-index 1 r)
			       (re-match-end-index 1 r))))
	       (string-pad-left day 2 #\0))
	     ;; Time
	     (string-pad-left
	      (substring date
			 (re-match-start-index 4 r)
			 (re-match-end-index 4 r))
	      2 #\0)
	     (substring date
			(re-match-start-index 5 r)
			(re-match-end-index 5 r))
	     (substring date
			(re-match-start-index 6 r)
			(re-match-end-index 6 r)))
	    ;; Cannot understand DATE string.
	    date)))))

(define mail-string-delete
  (lambda (string start end)
    (string-append
     (string-head string start)
     (string-tail string end))))

(define mail-strip-quoted-names
  (lambda (address)
    (let ((r (re-string-search-forward "\\`[ \t\n]*" address)))
      (if r
	  (set! address (string-tail address (re-match-end-index 0 r)))))
    ;; strip surrounding whitespace
    (let ((r (re-string-search-forward "[ \t\n]*\\'" address)))
      (if r
	  (set! address (string-head address (re-match-start-index 0 r)))))
    (let loop ()
      (let ((r
	     (re-string-search-forward
	      "[ \t]*(\\([^)\"\\]\\|\\\\.\\|\\\\\n\\)*)"
	      address)))
	(if r
	    (begin
	      (set! address
		    (mail-string-delete address 
					(re-match-start-index 0 r)
					(re-match-end-index 0 r)))
	      (loop)))))
    ;; strip `quoted' names (This is supposed to hack `"Foo Bar" <bar@host>')
    (let loop ((the-pos 0))
      (let ((r
	     (re-substring-match
	      "[ \t]*\"\\([^\"\\]\\|\\\\.\\|\\\\\n\\)*\"[ \t\n]*"
	      address the-pos (string-length address))))
	(if r
	    (let ((pos (re-match-end-index 0 r)))
	      (if (and (> (string-length address) pos)
		       (char=? (string-ref address pos) #\@))
		  (loop pos)
		  (begin
		    (set! address (mail-string-delete address the-pos pos))
		    (loop the-pos)))))))
    ;; Retain only part of address in <> delims, if there is such a thing.
    (let loop ()
      (let ((r
	     (re-string-search-forward "\\(,\\|\\`\\)[^,]*<\\([^>,]*>\\)"
				       address)))
	(if r
	    (let ((junk-beg (re-match-end-index 1 r))
		  (junk-end (re-match-start-index 2 r))
		  (close (re-match-end-index 0 r)))
	      (set! address (mail-string-delete address (-1+ close) close))
	      (set! address (mail-string-delete address junk-beg junk-end))
	      (loop)))))
    address))
