#| -*-Scheme-*-

$Id: iserch.scm,v 1.28 2007/01/05 21:19:23 cph Exp $

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

;;;; Incremental Search


(define (isearch forward? regexp?)
  (reset-command-prompt!)
  (let ((window (current-window)))
    (let ((point (window-point window))
	  (y-point (window-point-y window)))
      (let ((result
	     (dynamic-wind
	      (lambda () unspecific)
	      (lambda ()
		(with-editor-interrupts-disabled
		 (lambda ()
		   (isearch-loop
		    (initial-search-state #f forward? regexp? point)))))
	      clear-message)))
	(cond ((eq? result 'ABORT)
	       (set-window-point! window point)
	       (window-scroll-y-absolute! window y-point))
	      ((command? result)
	       (dispatch-on-command result))
	      (else
	       (push-current-mark! point)
	       (if result (execute-key (current-comtabs) result))))))))

(define (isearch-loop state)
  (if (not (keyboard-peek-no-hang))
      (begin
	(set-current-point! (search-state-point state))
	(message (search-state-message state))))
  (let ((char (keyboard-read)))
    (let ((test-for
	   (lambda (char*)
	     (char=? char (remap-alias-key char*)))))
      (cond ((not (char? char))
	     (isearch-exit state)
	     char)
	    ((test-for (ref-variable search-quote-char))
	     (let ((char
		    (prompt-for-typein
		     (string-append (search-state-message state) "^Q")
		     #f
		     keyboard-read)))
	       (if (char? char)
		   (isearch-append-char state char)
		   (begin
		     (isearch-exit state)
		     char))))
	    ((test-for (ref-variable search-exit-char))
	     (if (string-null? (search-state-text state))
		 (nonincremental-search (search-state-forward? state)
					(search-state-regexp? state))
		 (begin
		   (isearch-exit state)
		   #f)))
	    ((test-for #\C-g)
	     (editor-beep)
	     (isearch-pop state))
	    ((test-for (ref-variable search-repeat-char))
	     (isearch-continue (search-state-next state #t)))
	    ((test-for (ref-variable search-reverse-char))
	     (isearch-continue (search-state-next state #f)))
	    ((test-for (ref-variable search-delete-char))
	     (isearch-loop (or (search-state-parent state) (editor-error))))
	    ((test-for (ref-variable search-yank-word-char))
	     (isearch-append-string
	      state
	      (extract-next-word (search-state-end-point state))))
	    ((test-for (ref-variable search-yank-line-char))
	     (isearch-append-string
	      state
	      (extract-rest-of-line (search-state-end-point state))))
	    ((char=? char #\linefeed)
	     (isearch-append-char state #\newline))
	    ((or (not (zero? (char-bits char)))
		 (and (ref-variable search-exit-option)
		      (< (char-code char) #x20)))
	     (isearch-exit state)
	     char)
	    (else
	     (isearch-append-char state char))))))

(define (nonincremental-search forward? regexp?)
  (let ((yank-word (remap-alias-key (ref-variable search-yank-word-char)))
	(not-word-search
	 (lambda ()
	   (if regexp?
	       (if forward?
		   (ref-command-object re-search-forward)
		   (ref-command-object re-search-backward))
	       (if forward?
		   (ref-command-object search-forward)
		   (ref-command-object search-backward))))))
    (if (char? yank-word)
	(let ((char
	       (prompt-for-typein
		(if regexp?
		    (prompt-for-string/prompt
		     (if forward? "RE search" "RE search backward")
		     (write-to-string (ref-variable search-last-regexp)))
		    (prompt-for-string/prompt
		     (if forward? "Search" "Search backward")
		     (write-to-string (ref-variable search-last-string))))
		#f
		keyboard-peek)))
	  (cond ((not (char? char))
		 char)
		((char=? yank-word char)
		 (if forward?
		     (ref-command-object word-search-forward)
		     (ref-command-object word-search-backward)))
		(else
		 (not-word-search))))
	(not-word-search))))

(define (isearch-append-char state char)
  (isearch-append-string state (string char)))

(define (isearch-append-string state string)
  (isearch-continue (search-state-append-string state string)))

(define (isearch-continue state)
  (if (and (not (search-state-successful? state))
	   (let ((parent (search-state-parent state)))
	     (or (search-state-successful? parent)
		 (not (eq? (search-state-forward? state)
			   (search-state-forward? parent))))))
      (editor-failure))
  (isearch-loop state))

(define (isearch-pop state)
  (let ((success (most-recent-successful-search-state state)))
    (if (eq? success state)
	'ABORT
	(isearch-loop success))))

(define (isearch-exit state)
  (set-current-point!
   (search-state-point (most-recent-successful-search-state state)))
  (if (not (string-null? (search-state-text state)))
      (let ((text (search-state-text state)))
	(if (search-state-regexp? state)
	    (set-variable! search-last-regexp text)
	    (set-variable! search-last-string text)))))

(define (extract-next-word mark)
  (extract-string mark (forward-word mark 1 'LIMIT)))

(define (extract-rest-of-line mark)
  (extract-string mark (line-end mark (if (line-end? mark) 1 0) 'LIMIT)))

(define (search-state-message state)
  (let ((invalid-regexp (search-state-invalid-regexp state)))
    (let ((m
	   (string-append
	    (if (search-state-successful? state) "" "failing ")
	    (if (search-state-wrapped? state) "wrapped " "")
	    (if (search-state-regexp? state) "regexp " "")
	    "I-search"
	    (if (search-state-forward? state) "" " backward")
	    ": "
	    (string-image (search-state-text state) 0 #f
			  (ref-variable char-image-strings
					(search-state-start-point state)))
	    (if invalid-regexp (string-append " [" invalid-regexp "]") ""))))
      (string-set! m 0 (char-upcase (string-ref m 0)))
      m)))

(define (search-state-append-string state string)
  (let ((text (string-append (search-state-text state) string)))
    (if (search-state-successful? state)
	(next-search-state
	 state
	 text
	 (search-state-forward? state)
	 (cond ((and (search-state-regexp? state)
		     (string-index string regexp-retry-chars))
		(search-state-initial-point state))
	       ((search-state-forward? state)
		(search-state-start-point state))
	       (else
		(let ((end
		       (mark+ (search-state-end-point state)
			      (string-length string)))
		      (initial-point (search-state-initial-point state)))
		  (if (and end (mark< end initial-point))
		      end
		      initial-point))))
	 (search-state-initial-point state))
	(unsuccessful-search-state state
				   text
				   (search-state-forward? state)))))

(define regexp-retry-chars
  ;; If one of these characters is entered, retry the regexp search
  ;; from the initial point since it may now match something that it
  ;; didn't match before.
  (char-set #\* #\? #\|))

(define (search-state-next state forward?)
  (cond ((not (string-null? (search-state-text state)))
	 (let ((start
		(cond ((search-state-successful? state)
		       (if (search-state-forward? state)
			   (search-state-end-point state)
			   (search-state-start-point state)))
		      (forward?
		       (if (search-state-forward? state)
			   (buffer-start (current-buffer))
			   (search-state-point state)))
		      (else
		       (if (search-state-forward? state)
			   (search-state-point state)
			   (buffer-end (current-buffer)))))))
	   (next-search-state state
			      (search-state-text state)
			      forward?
			      start
			      start)))
	((eq? forward? (search-state-forward? state))
	 (next-search-state state
			    (if (search-state-regexp? state)
				(ref-variable search-last-regexp)
				(ref-variable search-last-string))
			    forward?
			    (search-state-initial-point state)
			    (search-state-initial-point state)))
	(else
	 (initial-search-state state
			       forward?
			       (search-state-regexp? state)
			       (search-state-initial-point state)))))

(define-structure (search-state)
  (text #f read-only #t)
  (parent #f read-only #t)
  (forward? #f read-only #t)
  (regexp? #f read-only #t)
  (successful? #f read-only #t)
  (wrapped? #f read-only #t)
  (invalid-regexp #f read-only #t)
  (start-point #f read-only #t)
  (end-point #f read-only #t)
  (point #f read-only #t)
  (initial-point #f read-only #t))

(define (most-recent-successful-search-state state)
  (if (search-state-successful? state)
      state
      (most-recent-successful-search-state
       (or (search-state-parent state)
	   (error "Search state chain terminated improperly")))))

(define (initial-search-state parent forward? regexp? point)
  (make-search-state ""
		     parent
		     forward?
		     regexp?
		     #t
		     #f
		     #f
		     point
		     point
		     point
		     point))

(define (unsuccessful-search-state parent text forward?)
  (let ((start-point (search-state-start-point parent)))
    (make-search-state text
		       parent
		       forward?
		       (search-state-regexp? parent)
		       #f
		       (search-state-wrapped? parent)
		       #f
		       start-point
		       (mark+ start-point (string-length text))
		       (search-state-point parent)
		       (search-state-initial-point parent))))

(define (next-search-state parent text forward? start initial-point)
  (let ((regexp? (search-state-regexp? parent)))
    (let ((result (perform-search forward? regexp? text start)))
      (cond ((not result)
	     (unsuccessful-search-state parent text forward?))
	    ((eq? result 'ABORT)
	     (most-recent-successful-search-state parent))
	    ((string? result)
	     (make-search-state text
				parent
				forward?
				regexp?
				(search-state-successful? parent)
				(search-state-wrapped? parent)
				(if (or (string-prefix? "Premature " result)
					(string-prefix? "Unmatched " result))
				    "incomplete input"
				    result)
				(search-state-start-point parent)
				(search-state-end-point parent)
				(search-state-point parent)
				(search-state-initial-point parent)))
	    (else
	     (make-search-state
	      text
	      parent
	      forward?
	      regexp?
	      #t
	      (and (boolean=? forward? (search-state-forward? parent))
		   (or (search-state-wrapped? parent)
		       (not (search-state-successful? parent))))
	      #f
	      (re-match-start 0)
	      (re-match-end 0)
	      result
	      initial-point))))))

(define (perform-search forward? regexp? text start)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:re-compile-pattern)
	 (lambda (condition)
	   (continuation (access-condition condition 'MESSAGE)))
       (lambda ()
	 (bind-condition-handler (list condition-type:^G)
	     (lambda (condition) condition (continuation 'ABORT))
	   (lambda ()
	     (with-editor-interrupts-enabled
	      (lambda ()
		(if forward?
		    (let ((end (group-end start)))
		      (if regexp?
			  (re-search-forward text start end)
			  (search-forward text start end)))
		    (let ((end (group-start start)))
		      (if regexp?
			  (re-search-backward text start end)
			  (search-backward text start end)))))))))))))