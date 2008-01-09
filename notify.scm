;;; -*-Scheme-*-
;;;
;;; $Id: notify.scm,v 1.26 2007/01/05 21:19:24 cph Exp $
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

;;;; Mode-line notifications (e.g. presence of mail, load average)


(define-variable notify-show-time
  "If true, the notifier displays the current time."
  #t
  boolean?)

(define (notifier:time)
  (let ((time (get-decoded-time)))
    (let ((hour (decoded-time/hour time))
	  (minute (decoded-time/minute time)))
      (string-append (write-to-string
		      (cond ((zero? hour) 12)
			    ((< hour 13) hour)
			    (else (- hour 12))))
		     (if (< minute 10) ":0" ":")
		     (write-to-string minute)
		     (if (< hour 12) "am" "pm")))))

(define-variable notify-show-date
  "If true, the notifier displays the current date."
  #f
  boolean?)

(define (notifier:date)
  (let ((time (get-decoded-time)))
    (string-append (vector-ref
		    '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
		    (decoded-time/day-of-week time))
		   (vector-ref
		    '#("??" " Jan " " Feb " " Mar " " Apr " " May " " Jun "
			    " Jul " " Aug " " Sep " " Oct " " Nov " " Dec ")
		    (decoded-time/month time))
		   (write-to-string (decoded-time/day time)))))

(define-variable notify-show-load
  "If true, the notifier displays the load average."
  #f
  boolean?)

(define (notifier:load-average)
  (let ((temporary-buffer (temporary-buffer "*uptime*")))
    (let ((start (buffer-start temporary-buffer)))
      (shell-command #f start #f #f "uptime")
      (let ((result
	     (if (re-search-forward
		  ".*load average:[ ]*\\([0-9.]*\\),"
		  start 
		  (buffer-end temporary-buffer))
		 (extract-string (re-match-start 1)
				 (re-match-end 1))
		 "")))
	(kill-buffer temporary-buffer)
	result))))

(define-variable notify-show-mail
  "If true, the notifier displays your mail status."
  #t
  boolean?)

(define-variable notify-mail-present
  "A string to be displayed in the modeline when mail is present.
Ignored if notify-show-mail is false."
  "Mail"
  string?)

(define-variable notify-mail-not-present
  "A string to be displayed in the modeline when mail is not present.
Ignored if notify-show-mail is false."
  ""
  string?)

(define-variable mail-notify-directory
  "Directory in which MAIL-NOTIFY checks for mail."
  #f
  (lambda (object) (or (not object) (file-directory? object))))

(define (notifier:mail-present)
  (if (not (ref-variable mail-notify-directory))
      (begin
	(guarantee-rmail-variables-initialized)
	(set-variable! mail-notify-directory rmail-spool-directory)))
  (if (let ((pathname
	     (merge-pathnames (ref-variable mail-notify-directory)
			      (current-user-name))))
	(and (file-exists? pathname)
	     (> (file-length pathname) 0)))
      (ref-variable notify-mail-present)
      (ref-variable notify-mail-not-present)))

(define (notifier:set-mail-string! string)
  ;; STRING is either #F, meaning use the internal mail notifier, or a
  ;; string.  A null string means no mail, and a non-null string means
  ;; new mail is available.
  (without-interrupts
   (lambda ()
     (set! override-notifier-mail-string string)
     (if (not notifier-thread-registration)
	 (set-variable! global-mode-string string #f))
     (global-window-modeline-event!))))

(define-variable notify-interval
  "How often the notifier updates the modeline, in seconds."
  60
  exact-nonnegative-integer?)

(define notifier-elements
  (list (cons (ref-variable-object notify-show-date) notifier:date)
	(cons (ref-variable-object notify-show-time) notifier:time)
	(cons (ref-variable-object notify-show-load) notifier:load-average)))

(define (notifier:get-string window)
  window
  (string-append-separated notifier-element-string
			   (if override-notifier-mail-string
			       (if (string-null? override-notifier-mail-string)
				   (ref-variable notify-mail-not-present)
				   (ref-variable notify-mail-present))
			       notifier-mail-string)))

(define (update-notifier-strings! element mail)
  (set! notifier-element-string element)
  (set! notifier-mail-string mail)
  (global-window-modeline-event!))

(define notifier-element-string "")
(define notifier-mail-string "")
(define override-notifier-mail-string #f)
(define mail-notify-hook-installed? #f)
(define notifier-thread-registration #f)

(define-command run-notifier
  "Run the notifier.
The notifier maintains a simple display in the modeline,
which can show various things including time, load average, and mail status."
  ()
  (lambda ()
    (if (and (not mail-notify-hook-installed?)
	     (command-defined? rmail))
	(begin
	  (add-event-receiver!
	   (ref-variable rmail-new-mail-hook)
	   (lambda ()
	     (update-notifier-strings!
	      notifier-element-string
	      (if (ref-variable notify-show-mail)
		  (ref-variable notify-mail-not-present)
		  ""))))
	  (set! mail-notify-hook-installed? #t)
	  unspecific))
    ((ref-command kill-notifier))
    (set-variable! global-mode-string `("" ,notifier:get-string))
    (set! notifier-thread-registration
	  (start-standard-polling-thread (* (ref-variable notify-interval #f)
					    1000)
					 notifier))
    unspecific))

(define (notifier)
  (update-notifier-strings!
   (reduce-right string-append-separated
		 ""
		 (map (lambda (element)
			(if (and (car element)
				 (variable-value (car element)))
			    ((cdr element))
			    ""))
		      notifier-elements))
   (if (and mail-notify-hook-installed?
	    (ref-variable notify-show-mail))
       (notifier:mail-present)
       ""))
  #t)

(define-command kill-notifier
  "Kill the current notifier, if any."
  ()
  (lambda ()
    (without-interrupts
     (lambda ()
       (if notifier-thread-registration
	   (begin
	     (stop-standard-polling-thread notifier-thread-registration)
	     (set! notifier-thread-registration #f)
	     unspecific))))
    (update-notifier-strings! "" "")
    (set-variable! global-mode-string override-notifier-mail-string #f)))
