#| -*-Scheme-*-

$Id: rmailsum.scm,v 1.41 2007/01/05 21:19:24 cph Exp $

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

;;;; RMAIL Summary Mode


(define-variable rmailsum-rcs-header
  "The RCS header of the rmailsum.scm file."
  "$Id: rmailsum.scm,v 1.41 2007/01/05 21:19:24 cph Exp $"
  string?)

(define-variable-per-buffer rmail-buffer
  "Corresponding RMAIL buffer for a summary buffer.
FALSE means buffer is not a summary buffer."
  #f
  (lambda (x) (or (not x) (buffer? x))))

(define-variable-per-buffer rmail-summary-buffer
  "Corresponding RMAIL-summary buffer for an RMAIL buffer."
  #f
  (lambda (x) (or (not x) (buffer? x))))

(define-variable-per-buffer rmail-summary-vector
  "Vector of header lines."
  #f
  (lambda (x) (or (not x) (vector? x))))

(define-variable rmail-summary-mode-hook
  "An event distributor that is invoked when entering RMAIL Summary mode."
  (make-event-distributor))

;;; (define-variable rmail-last-multi-labels
;;;   ""
;;;   ""
;;;   list-of-strings?)

(define-command rmail-summary
  "Display a summary of all messages, one line per message."
  '()
  (lambda ()
    (rmail-new-summary "All" #f)))
;;;
;;; (define rmail-summary-by-labels
;;;   "Display a summary of all messages with one or more LABELS.
;;;  LABELS should be a string containing the desired labels, separated by commas."
;;;   "sLabels to summarize by: "
;;;   (lambda (labels)
;;;     (if (string=? labels "")
;;;  	(set! labels (or rmail-last-multi-labels
;;;  			 (error "No label specified"))))
;;;     (set! rmail-last-multi-labels labels)
;;;     (rmail-new-summary (string-append "labels " labels)
;;;  		       rmail-message-labels?
;;;  		       (string-append ", \\("
;;; 				      (mail-comma-list-regexp labels)
;;; 				      "\\),"))))
;;;
;;; (define rmail-summary-by-recipients
;;;   "Display a summary of all messages with the given RECIPIENTS.
;;; Normally checks the To, From and Cc fields of headers;
;;; but if PRIMARY-ONLY is non-nil (prefix arg given),
;;;  only look in the To and From fields.
;;; RECIPIENTS is a string of names separated by commas."
;;;   (interactive "sRecipients to summarize by: \nP")
;;;   (lambda (recipients primary-only)
;;;     (rmail-new-summary
;;;      (string-append "recipients " recipients)
;;;      rmail-message-recipients?
;;;      (mail-comma-list-regexp recipients) primary-only)))
;;;

(define-command rmail-summary-by-recipients
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
only look in the To and From fields.
RECIPIENTS is a string of names separated by commas."
  "sRecipients to summarize by: \nP"
  (lambda (recipients primary-only)
    (rmail-new-summary
     (string-append "recipients " recipients)
     rmail-message-recipients?
     (mail-comma-list-regexp recipients)
     primary-only)))

(define (mail-comma-list-regexp the-string)
  (let loop ((the-string (string-trim the-string))
	     (the-new-list '()))
    (let ((pos (string-find-next-char the-string #\,)))
      (if pos
	  (loop (string-tail the-string (1+ pos))
		(cons (string-trim (string-head the-string pos))
		      the-new-list))
	  (re-compile-pattern
	   (apply string-append
		  (reverse
		   (cons (string-trim the-string)
			 (map (lambda (x) (string-append x "\\|"))
			      the-new-list))))
	   #t)))))

(define (rmail-message-recipients? memo recip-regexp primary-only)
  (without-clipping
   (current-buffer)
   (lambda ()
     (let* ((start (msg-memo/start memo))
	    (end (msg-memo/end memo))
	    (inner-start (search-forward "\n*** EOOH ***\n" start end))
	    (inner-end (header-end inner-start end)))
       (let ((the-to-field (fetch-first-field "to" inner-start inner-end))
	     (the-from-field (fetch-first-field "from" inner-start inner-end))
	     (the-cc-fields  (fetch-all-fields "cc" inner-start inner-end)))
	 (or (and the-to-field
		  (re-string-search-forward recip-regexp the-to-field))
	     (and the-from-field
		  (re-string-search-forward recip-regexp the-from-field))
	     (and (and (not primary-only) the-cc-fields)
		  (re-string-search-forward recip-regexp the-cc-fields))))))))

(define rmail-new-summary
  (lambda (description function . args)
    (let ((the-rmail-buffer (current-buffer))
	  (number-of-messages
	   (msg-memo/number (msg-memo/last (current-msg-memo)))))
      (message "Computing summary lines...")
      (let ((the-rmail-summary-buffer
	     (or (ref-variable rmail-summary-buffer)
		 (temporary-buffer
		  (string-append (buffer-name (current-buffer)) "-summary")))))
	(set-buffer-major-mode! the-rmail-summary-buffer
				(ref-mode-object rmail-summary))
	(associate-rmail-summary-buffer! the-rmail-buffer
					 the-rmail-summary-buffer)
	(define-variable-local-value! the-rmail-summary-buffer
	    (ref-variable-object rmail-summary-vector)
	  (make-vector number-of-messages #f))
	(select-buffer-other-window the-rmail-summary-buffer)
	(select-buffer-other-window the-rmail-buffer))
      (let ((summary-msgs ())
	    (the-current-message-number (msg-memo/number (current-msg-memo))))
	(let loop ((the-memo (msg-memo/first (current-msg-memo))))
	  (let ((next-memo (msg-memo/next the-memo)))
	    (if (or (not function)
		    (apply function (cons the-memo args)))
		(set! summary-msgs
		      (cons (rmail-make-summary-line the-memo)
			    summary-msgs)))
	    (if next-memo
		(loop next-memo))))
	(select-buffer-other-window (ref-variable rmail-summary-buffer))
	(set-buffer-writeable! (current-buffer))
	(set-current-point! (buffer-start (current-buffer)))
	(kill-string (buffer-start (current-buffer))
		     (buffer-end (current-buffer)))
	(let loop ((the-summary-list (reverse summary-msgs)))
	  (if (not (null? the-summary-list))
	      (begin
		(insert-string (car the-summary-list))
		(loop (cdr the-summary-list)))))
	(set-buffer-read-only! (current-buffer))
	(set-current-point! (buffer-start (current-buffer)))
	;;(set-current-major-mode! (ref-mode-object rmail-summary))
	(set-variable! mode-line-process (list ": " description))
	(let ((the-current-msg-line
	       (re-search-forward
		(string-append "^[ ]*"
			       (number->string the-current-message-number))
		(buffer-start (current-buffer))
		(buffer-end (current-buffer)))))
	  (if the-current-msg-line
	      (set-current-point!
	       (line-start the-current-msg-line 0))))
	(rmail-summary-goto-message-current-line)
	(message "Computing summary lines...done")))))

(define (associate-rmail-summary-buffer! rmail-buffer rmail-summary-buffer)
  (define-variable-local-value! rmail-summary-buffer
      (ref-variable-object rmail-buffer)
    rmail-buffer)
  (define-variable-local-value! rmail-buffer
      (ref-variable-object rmail-summary-buffer)
    rmail-summary-buffer)
  (add-kill-buffer-hook rmail-summary-buffer rmail-summary-buffer-kill-hook))

(define (rmail-summary-buffer-kill-hook rmail-summary-buffer)
  (let ((rmail-buffer (ref-variable rmail-buffer rmail-summary-buffer)))
    (if (and rmail-buffer
	     (eq? (ref-variable rmail-summary-buffer rmail-buffer)
		  rmail-summary-buffer))
	(define-variable-local-value! rmail-buffer
	    (ref-variable-object rmail-summary-buffer)
	  #f))))

(define (rmail-make-summary-line memo)
  (let ((new-summary-line-count 0))
    (let ((line
	   (or (vector-ref (ref-variable rmail-summary-vector
					 (ref-variable rmail-summary-buffer))
			   (-1+ (msg-memo/number memo)))
	       (begin
		 (set! new-summary-line-count (1+ new-summary-line-count))
		 (if (= 0 (modulo new-summary-line-count 10))
		     (message "Computing summary lines..."
			      new-summary-line-count))
		 (rmail-make-summary-line-1 memo)
		 (vector-ref (ref-variable rmail-summary-vector
					   (ref-variable rmail-summary-buffer))
			     (-1+ (msg-memo/number memo)))))))
      ;; Fix up the part of the summary that says "deleted" or "unseen".
      (string-set!
       line 4
       (if (msg-memo/deleted? memo) #\D
	   (if (char=? #\0 (string-ref (extract-string (msg-memo/start memo)
						       (msg-memo/end memo))
				       2))
	       #\- #\space)))
      line)))

(define (rmail-make-summary-line-1 memo)
  (with-buffer-open (current-buffer)
    (lambda ()
      (let ((old-point (current-point))
	    (start (msg-memo/start memo))
	    (end (msg-memo/end memo)))
	(let ((pos)
	      (labels
	       (begin
		 (set-current-point! start)
		 (move-thing mark+ 3 'ERROR)
		 (if (and (search-forward ",," start end)
			  (line-end? (current-point)))
		     (let ((point (current-point)))
		       (string-append
			"{"
			(extract-string point (line-end point 0))
			"} "))
		     "")))
	      (line
	       (let ((point (line-start start 2)))
		 (if (string-prefix?
		      "Summary-line: "
		      (extract-string point (line-end point 0)))
		     (begin
		       (string-tail
			(extract-string point (line-start point 1))
			14))
		     #f))))
	  ;; If we didn't get a valid status line from the message,
	  ;; make a new one and put it in the message.
	  (or line
	      (let ((inner-begin
		     (let ((foo (search-forward "\n*** EOOH ***\n" start end)))
		       (if foo
			   foo
			   (begin
			     ((ref-command next-line) 1)
			     (current-point))))))
		(set! line (rmail-make-basic-summary-line inner-begin end))
		(insert-string (string-append "Summary-line: " line)
			       (line-start start 2))))
	  (set! pos (string-find-next-char line #\#))
	  (let ((num (msg-memo/number memo)))
	    (vector-set! (ref-variable rmail-summary-vector
				       (ref-variable rmail-summary-buffer))
			 (-1+ num)
			 (string-append
			  (string-pad-left (number->string num) 4)
			  "  "
			  (string-head line pos)
			  labels
			  (string-tail line (1+ pos))))))
	(set-current-point! old-point)))))

(define (rmail-make-basic-summary-line the-begin the-end)
  (string-append
   (let ((the-mark
	  (re-search-forward "^Date:" the-begin the-end)))
     (if (not the-mark)
	 "      "
	 (let ((the-end-of-line (line-end the-mark 0)))
	   (cond
	    ((re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)"
				the-mark the-end-of-line)
	     (string-append
	      (let ((date-string
		     (extract-string (re-match-start 2) (re-match-end 2))))
		(if (char=? #\0 (string-ref date-string 0))
		    (string-set! date-string 0 #\space))
		(string-pad-left date-string 2))
	      "-"
	      (extract-string (re-match-start 4) (re-match-end 4))))
	    ((re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)"
				the-mark the-end-of-line)
	     (string-append
	      (string-pad-left (extract-string (re-match-start 4)
					       (re-match-end 4))
			       2)
	      "-"
	      (extract-string (re-match-start 2) (re-match-end 2))))
	    (else
	     "??????")))))
   "  "
   (let ((the-mark
	  (re-search-forward "^From:[ \t]*" the-begin the-end)))
     (if (not the-mark)
	 "                         "
	 #|
	 The following hair is required because From: lines can extend
	 over multiple text lines in the message, so long as the
	 first characters of each continuation line is a #\space or #\tab
	 character.  Previously, we assumed that the field of the From: line
	 terminated at the end of the text line, thus we could use:

	 (mail-extract-real-name
	   (skip-chars-forward " \t" the-mark)
   	   (skip-chars-backward " " (line-end the-mark 0)))

	 to extract the message.  Now, according to RFC822, we have to allow
	 the presence of newlines in the From: field, so long as the first
	 character after the newline is a space or tab.
	 |#
	 (let* ((from
		 (let* ((the-new-mark (skip-chars-forward " \t\n" the-mark))
			(the-new-end-mark
			 (skip-chars-backward " " (line-end the-new-mark 0))))
		   (if (mark= the-new-mark (line-start the-new-mark 0))
		       "                         "
		       (mail-extract-real-name the-new-mark
					       the-new-end-mark))))
		(len (string-length from))
		(mch (string-find-next-char-in-set from (char-set #\@ #\%))))
	   (string-pad-right
	    (if (not mch)
		(string-head from (if (> len 25) 25 len))
		(string-head from (min mch 25)))
	    25))))
   "  #"
   (let ((the-mark
	  (re-search-forward "^Subject:" the-begin the-end)))
     (if the-mark
	 (let ((the-start (skip-chars-forward " \t" the-mark)))
	   (extract-string the-start (line-end the-start 0)))
	 (let ((the-start
		(re-search-forward "[\n][\n]+"
				   the-begin
				   (group-end the-begin))))
	   (extract-string the-start (line-end the-start 0)))))
   "\n"))

(define (mail-extract-real-name address-start address-end)
  (cond ((re-search-forward "[ \t\"]*\\<\\(.*\\)\\>[\" \t]*<.*>"
			    address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	;; Chris VanHaren (Athena User Consultant) <vanharen>
	((re-search-forward "[ \t\"]*\\<\\(.*\\)\\>.*(.*).*<.*>.*"
			    address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	((re-search-forward ".*(\\(.*\\))" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	((re-search-forward ".*<\\(.*\\)>.*" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	((re-search-forward " *\\<\\(.*\\)\\> *" address-start address-end)
	 (extract-string (re-match-start 1) (re-match-end 1)))
	(else
	 (extract-string address-start address-end))))

(define-major-mode rmail-summary read-only "RMAIL Summary"
  "Major mode in effect in Rmail summary buffer.
A subset of the Rmail mode commands are supported in this mode.
As commands are issued in the summary buffer the corresponding
mail message is displayed in the rmail buffer.

n       Move to next undeleted message, or arg messages.
p       Move to previous undeleted message, or arg messages.
M-n	Move to next, or forward arg messages.
M-p	Move to previous, or previous arg messages.
j       Jump to the message at the cursor location.
d       Delete the message at the cursor location and move to next message.
D       Delete the message at the cursor location and move to previous message.
M-d     Delete the message at the cursor location.
u	Undelete this or previous deleted message.
U	Undelete this or next deleted message.
M-u     Undelete this message.
q	Quit Rmail.
x	Exit and kill the summary window.
space   Scroll message in other window forward.
delete  Scroll message in other window backward.
.       Go to top of message.
e       Expunge deleted messages.
s       Expunge and save RMAIL buffer.
t       Toggle header of message in RMAIL buffer.
C-o     Output message to Unix mail file.
o       Output message to RMAIL file.
g       Get new mail.
i       Run RMAIL on another file.
m       Send a mail message.
r       Reply to this mail message.

Entering this mode calls value of hook variable rmail-summary-mode-hook."
  (lambda (buffer)
    (set-buffer-read-only! buffer)
    (event-distributor/invoke! (ref-variable rmail-summary-mode-hook buffer)
			       buffer)))

(define-key 'rmail-summary #\j		'rmail-summary-show-message)
(define-key 'rmail-summary #\n		'rmail-summary-next-undeleted-message)
(define-key 'rmail-summary #\p
  'rmail-summary-previous-undeleted-message)
(define-key 'rmail-summary #\m-n	'rmail-summary-next-message)
(define-key 'rmail-summary #\m-p	'rmail-summary-previous-message)
(define-key 'rmail-summary #\c-m-n	'rmail-summary-next-labeled-message)
(define-key 'rmail-summary #\c-m-p
  'rmail-summary-previous-labeled-message)
(define-key 'rmail-summary #\space	'rmail-summary-scroll-message-up)
(define-key 'rmail-summary #\rubout	'rmail-summary-scroll-message-down)
(define-key 'rmail-summary #\d		'rmail-summary-delete-message-forward)
(define-key 'rmail-summary #\D    	'rmail-summary-delete-message-backward)
(define-key 'rmail-summary #\M-d        'rmail-summary-delete-message)
(define-key 'rmail-summary #\u
  'rmail-summary-undelete-message-backward)
(define-key 'rmail-summary #\U
  'rmail-summary-undelete-message-forward)
(define-key 'rmail-summary #\M-u	'rmail-summary-undelete-message)
(define-key 'rmail-summary #\q		'rmail-summary-quit)
(define-key 'rmail-summary #\x		'rmail-summary-exit)
(define-key 'rmail-summary #\.		'rmail-summary-beginning-of-buffer)
(define-key 'rmail-summary #\e		'rmail-summary-expunge)
(define-key 'rmail-summary #\s		'rmail-summary-expunge-and-save)
(define-key 'rmail-summary #\t		'rmail-summary-toggle-header)
(define-key 'rmail-summary #\c-o	'rmail-summary-output)
(define-key 'rmail-summary #\o		'rmail-summary-output-to-rmail-file)
(define-key 'rmail-summary #\g		'rmail-summary-get-new-mail)
(define-key 'rmail-summary #\i		'rmail-summary-input)
(define-key 'rmail-summary #\?		'describe-mode)
(define-key 'rmail-summary #\m		'rmail-summary-mail)
(define-key 'rmail-summary #\r		'rmail-summary-reply)

;;; (define-key 'rmail #\a		'rmail-add-label)
;;; (define-key 'rmail #\k		'rmail-kill-label)
;;; (define-key 'rmail #\l		'rmail-summary-by-labels)
;;; (define-key 'rmail #\c		'rmail-continue)
;;; (define-key 'rmail #\m-s	'rmail-search)
;;; (define-key 'rmail #\>		'rmail-last-message)
;;; (define-key 'rmail #\w		'rmail-edit-current-message)

(define (make-rmail-summary-handler-prefix-arg key)
  (lambda (arg)
    (select-buffer-other-window (ref-variable rmail-buffer))
    ((command-procedure
      (comtab-entry (mode-comtabs (current-major-mode)) key)) arg)
    (select-buffer-other-window (ref-variable rmail-summary-buffer))))

(define-command rmail-summary-show-message
  ""
  "P"
  (lambda (arg)
    (if arg
	(let ((the-new-mark
	       (re-search-forward
		(string-append "^[ ]*" (number->string arg))
		(buffer-start (current-buffer))
		(buffer-end (current-buffer)))))
	  (if the-new-mark
	      (begin
		(set-current-point! (line-start the-new-mark 0))
		(rmail-summary-goto-message-current-line))
	      (message (string-append "Message "
				      (number->string arg)
				      " not found."))))
	(rmail-summary-goto-message-current-line))))

(define (rmail-summary-goto-message-current-line)
  (let ((start (line-start (current-point) 0)))
    (let ((end (mark+ start 4)))
      (if end
	  (let ((the-message-number
		 (string->number (string-trim (extract-string start end)))))
	    (if (not (null? the-message-number))
		(begin
		  (if (char=? (mark-right-char end) #\-)
		      (begin
			(set-buffer-writeable! (current-buffer))
			(mark-delete-right-char! end)
			(insert-char #\space end)
			(set-buffer-read-only! (current-buffer))))
		  (let ((rmail-buffer (ref-variable rmail-buffer)))
		    (show-message rmail-buffer the-message-number)
		    (pop-up-buffer rmail-buffer #f)))))))))

(define-command rmail-summary-next-message
  "Goto ARGth previous message."
  "p"
  (lambda (arg)
    (set-current-point! (line-start (current-point) arg))
    (rmail-summary-goto-message-current-line)))

(define-command rmail-summary-previous-message
  "Goto ARGth next message."
  "p"
  (lambda (arg)
    (set-current-point! (line-start (current-point) (- arg)))
    (rmail-summary-goto-message-current-line)))

(define-command rmail-summary-next-undeleted-message
  "Goto ARGth next undeleted message."
  "p"
  (lambda (arg)
    (let ((the-buf-end (buffer-end (current-buffer))))
      (let loop ((count arg)
		 (the-mark (line-end (current-point) 0)))
	(if (> count 0)
	    (let ((the-new-mark
		   (re-search-forward "^....[^D]" the-mark the-buf-end)))
	      (if the-new-mark
		  (loop (-1+ count) the-new-mark)
		  (begin
		    (set-current-point! (line-start the-mark 0))
		    (rmail-summary-goto-message-current-line))))
	    (begin
	      (set-current-point! (line-start the-mark 0))
	      (rmail-summary-goto-message-current-line)))))))

(define-command rmail-summary-previous-undeleted-message
  "Goto ARGth previous undeleted message."
  "p"
  (lambda (arg)
    (let ((the-buf-start (buffer-start (current-buffer))))
      (let loop ((count arg)
		 (the-mark (line-start (current-point) 0)))
	(if (> count 0)
	    (let ((the-new-mark
		   (re-search-backward "^....[^D]" the-mark the-buf-start)))
	      (if the-new-mark
		  (loop (-1+ count) the-new-mark)
		  (begin
		    (set-current-point! (line-start the-mark 0))
		    (rmail-summary-goto-message-current-line))))
	    (begin
	      (set-current-point! (line-start the-mark 0))
	      (rmail-summary-goto-message-current-line)))))))

(define-command rmail-summary-scroll-message-up
  "Scroll RMAIL window up.
If the line the cursor is on does not correspond to the message
shown in the RMAIL buffer, warp to the appropriate message."
  "P"
  (lambda (arg)
    (let ((start (line-start (current-point) 0)))
      (let ((end (mark+ start 4)))
	(if end
	    (let ((the-message-number
		   (string->number (string-trim (extract-string start end)))))
	      (if (not (null? the-message-number))
		  (if (= the-message-number
			 (msg-memo/number
			  (buffer-msg-memo (ref-variable rmail-buffer))))
		      (begin
			(select-buffer-other-window
			 (ref-variable rmail-buffer))
			(let ((window (current-window)))
			  (scroll-window
			   window
			   (standard-scroll-window-argument window arg 1)
			   (lambda () #t)))
			(select-buffer-other-window
			 (ref-variable rmail-summary-buffer)))
		      (begin
			(if (char=? (mark-right-char end) #\-)
			    (begin
			      (set-buffer-writeable! (current-buffer))
			      (mark-delete-right-char! end)
			      (insert-char #\space end)
			      (set-buffer-read-only! (current-buffer))))
			(select-buffer-other-window
			 (ref-variable rmail-buffer))
			((command-procedure
			  (comtab-entry (mode-comtabs (current-major-mode))
					#\j))
			 the-message-number)
			(select-buffer-other-window
			 (ref-variable rmail-summary-buffer)))))))))))

(define-command rmail-summary-scroll-message-down
  "Scroll RMAIL window down."
  "P"
  (lambda (arg)
    (select-buffer-other-window (ref-variable rmail-buffer))
    (let ((window (current-window)))
      (scroll-window window
		     (standard-scroll-window-argument window arg -1)
		     (lambda () #t)))
    (select-buffer-other-window (ref-variable rmail-summary-buffer))))

(define-command rmail-summary-delete-message
  "Delete this message and stay on it."
  '()
  (lambda ()
    (rmail-summary-goto-message-current-line)
    (let ((the-memo (buffer-msg-memo (ref-variable rmail-buffer))))
      (set-attribute! the-memo 'DELETED))
    (let ((the-mark1
	   (skip-chars-forward " " (line-start (current-point) 0))))
      (let ((the-mark
	     (skip-chars-forward "[0-9]" the-mark1)))
	(set-buffer-writeable! (current-buffer))
	(delete-string the-mark (mark1+ the-mark))
	(insert-string "D" the-mark)
	(set-buffer-read-only! (current-buffer))))))

(define-command rmail-summary-delete-message-forward
  "Delete ARG undeleted messages and move to next undeleted message."
  "p"
  (lambda (arg)
    (let loop ((count arg))
      (if (> count 0)
	  (begin
	    ((ref-command rmail-summary-delete-message))
	    ((ref-command rmail-summary-next-undeleted-message) 1)
	    (loop (-1+ count)))))))

(define-command rmail-summary-delete-message-backward
  "Delete ARG undeleted messages and move to previous undeleted message."
  "p"
  (lambda (arg)
    (let loop ((count arg))
      (if (> count 0)
	  (begin
	    ((ref-command rmail-summary-delete-message))
	    ((ref-command rmail-summary-previous-undeleted-message) 1)
	    (loop (-1+ count)))))))

(define-command rmail-summary-undelete-message
  "Undelete this message and stay here."
  '()
  (lambda ()
    (rmail-summary-goto-message-current-line)
    (let ((the-memo (buffer-msg-memo (ref-variable rmail-buffer))))
      (if (msg-memo/deleted? the-memo)
	  (clear-attribute! the-memo 'DELETED))
      (let ((the-mark1
	     (skip-chars-forward " " (line-start (current-point) 0))))
	(let ((the-mark
	       (skip-chars-forward "[0-9]" the-mark1)))
	  (set-buffer-writeable! (current-buffer))
	  (delete-string the-mark (mark1+ the-mark))
	  (insert-string " " the-mark)
	  (set-buffer-read-only! (current-buffer)))))))

(define-command rmail-summary-undelete-message-backward
  "Search backwards from current message for first ARG deleted
messages, and undelete them."
  "p"
  (lambda (arg)
    (let loop ((count arg))
      (if (> count 0)
	  (let ((the-mark
		 (re-search-backward "^....D"
				     (line-end (current-point) 0)
				     (buffer-start (current-buffer)))))
	    (if the-mark
		(begin
		  (set-current-point! (line-start the-mark 0))
		  (rmail-summary-goto-message-current-line)
		  ((ref-command rmail-summary-undelete-message))))
	    (loop (-1+ count)))))))

(define-command rmail-summary-undelete-message-forward
  "Search forward from current message for first ARG deleted
messages, and undelete them."
  "p"
  (lambda (arg)
    (let loop ((count arg))
      (if (> count 0)
	  (let ((the-mark
		 (re-search-forward "^....D"
				    (line-start (current-point) 0)
				    (buffer-end (current-buffer)))))
	    (if the-mark
		(begin
		  (set-current-point! (line-start the-mark 0))
		  (rmail-summary-goto-message-current-line)
		  ((ref-command rmail-summary-undelete-message))))
	    (loop (-1+ count)))))))

(define-command rmail-summary-exit
  "Exit RMAIL Summary mode, remaining within RMAIL."
  '()
  (lambda ()
    (bury-buffer (current-buffer))
    (if (window-has-no-neighbors? (current-window))
	(select-buffer (ref-variable rmail-buffer))
	((ref-command delete-window)))))

(define-command rmail-summary-quit
  "Exit RMAIL Summary mode and RMAIL mode."
  '()
  (lambda ()
    ((ref-command rmail-summary-exit))
    ((ref-command rmail-quit))))

(define-command rmail-summary-beginning-of-buffer
  "Go to top of message currently being displayed."
  '()
  (lambda ()
    (select-buffer-other-window (ref-variable rmail-buffer))
    ((ref-command beginning-of-buffer) 0)
    (select-buffer-other-window (ref-variable rmail-summary-buffer))))

(define-command rmail-summary-expunge
  "Remove deleted messages, and recompute header lines.
Calls whatever function is bound to #\e in RMAIL mode."
  '()
  (lambda ()
    (select-buffer-other-window (ref-variable rmail-buffer))
    ((command-procedure
      (comtab-entry (mode-comtabs (current-major-mode)) #\e)))
    ((ref-command rmail-summary))))

(define-command rmail-summary-expunge-and-save
  "Expunge and save RMAIL file.
Calls whatever function is bound to #\s in RMAIL mode."
  '()
  (lambda ()
    (select-buffer-other-window (ref-variable rmail-buffer))
    ((command-procedure
      (comtab-entry (mode-comtabs (current-major-mode)) #\s)))
    ((ref-command rmail-summary))))

(define-command rmail-summary-toggle-header
  "Toggle between pruned and full header for message.
Calls whatever function is bound to #\t in RMAIL mode."
  '()
  (lambda ()
    (select-buffer-other-window (ref-variable rmail-buffer))
    ((command-procedure
      (comtab-entry (mode-comtabs (current-major-mode)) #\t)))
    (select-buffer-other-window (ref-variable rmail-summary-buffer))))

(define-command rmail-summary-output
  "Append this message to Unix mail file named FILE-NAME.
Calls whatever function is bound to #\c-o in RMAIL mode."
  '()
  (lambda ()
    (rmail-summary-goto-message-current-line)
    (select-buffer-other-window (ref-variable rmail-buffer))
    (let ((the-command
	   (comtab-entry (mode-comtabs (current-major-mode)) #\c-o)))
      (execute-command the-command))
    (select-buffer-other-window (ref-variable rmail-summary-buffer))
    (if (ref-variable rmail-delete-after-output)
	((ref-command rmail-summary-delete-message-forward) 1))))

(define-command rmail-summary-output-to-rmail-file
  "Append this message to RMAIL file named FILE-NAME.
Calls whatever function is bound to #\o in RMAIL mode."
  '()
  (lambda ()
    (rmail-summary-goto-message-current-line)
    (select-buffer-other-window (ref-variable rmail-buffer))
    (let ((the-command
	   (comtab-entry (mode-comtabs (current-major-mode)) #\o)))
      (execute-command the-command))
    (select-buffer-other-window (ref-variable rmail-summary-buffer))
    (if (ref-variable rmail-delete-after-output)
	((ref-command rmail-summary-delete-message-forward) 1))))

(define-command rmail-summary-get-new-mail
  "Get new mail.
Calls whatever function is bound to #\g in RMAIL mode."
  '()
  (lambda ()
    (select-buffer-other-window (ref-variable rmail-buffer))
    (let ((the-command
	   (comtab-entry (mode-comtabs (current-major-mode)) #\g)))
      (execute-command the-command))
    ((ref-command rmail-summary))))

(define-command rmail-summary-input
  "Run RMAIL on file FILENAME.
Calls whatever function is bound to #\i in RMAIL mode."
  '()
  (lambda ()
    (select-buffer-other-window (ref-variable rmail-buffer))
    (let ((the-command
	   (comtab-entry (mode-comtabs (current-major-mode)) #\i)))
      (execute-command the-command))
    ((ref-command rmail-summary))))

(define-command rmail-summary-mail
  "Send mail in another window.
Calls whatever function is bound to #\m in RMAIL mode."
  '()
  (lambda ()
    (select-buffer-other-window (ref-variable rmail-buffer))
    ((command-procedure
      (comtab-entry (mode-comtabs (current-major-mode)) #\m)))))

(define-command rmail-summary-reply
  "Reply to the current message.
Calls whatever function is bound to #\r in RMAIL mode."
  "P"
  (lambda (arg)
    (select-buffer-other-window (ref-variable rmail-buffer))
    ((command-procedure
      (comtab-entry (mode-comtabs (current-major-mode)) #\r))
     arg)))