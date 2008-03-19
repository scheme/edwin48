#| -*-Scheme-*-

$Id: webster.scm,v 1.12 2008/01/30 20:02:07 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Webster Server Interface

;;; Translated from Noah Friedman's Emacs implementation:
;;; webster.el,v 1.2 1995/01/04 00:41:51


(define (webster-send request word)
  (guarantee-webster-server-port (selected-buffer))
  (write-string request webster-server-port)
  (write-string " " webster-server-port)
  (write-string word webster-server-port)
  (newline webster-server-port)
  (flush-output webster-server-port)
  (let ((line (read-line webster-server-port)))
    (cond ((string=? "SPELLING 0" line)
	   (message "Word not found."))
	  ((string=? "SPELLING 1" line)
	   (message "Word spelled correctly."))
	  ((string=? "MATCHS 0" line)
	   (message "No endings for this word."))
	  ((or (string=? "SPELLING" line)
	       (string=? "MATCHS" line)
	       (string-prefix? "DEFINITION " line))
	   (let loop ((lines '()))
	     (receive (line end?) (webster-read-line webster-server-port)
	       (cond ((not end?)
		      (loop (cons line lines)))
		     ((pair? lines)
		      (webster-show-output (reverse! (cons line lines))))
		     (else
		      (message line))))))
	  (else
	   (error "Unrecognized response from Webster server:" line)))))

(define (webster-read-line port)
  (let ((line (read-string webster-line-delimiters port)))
    (values line
	    (let ((delim (read-char port)))
	      (or (eof-object? delim)
		  (not (char=? delim #\newline)))))))

(define webster-line-delimiters
  (char-set #\newline #\U+00 #\U+80))

(define webster-server-port #f)

(define (guarantee-webster-server-port buffer)
  (if (or (not webster-server-port)
	  (input-port/eof? webster-server-port))
      (let ((server
	     (or (ref-variable webster-server buffer)
		 (editor-error "Variable webster-server not set."))))
	((message-wrapper #f "Opening webster connection to " server)
	 (lambda ()
	   (set! webster-server-port
		 (open-tcp-stream-socket server
					 (ref-variable webster-port buffer)))
	   unspecific))
	(global-window-modeline-event!
	 (lambda (window) window 'WEBSTER-CONNECTION-STATUS)))))

(define (close-webster-server-port)
  (let ((port webster-server-port))
    (set! webster-server-port #f)
    (if port (close-port port)))
  (global-window-modeline-event!
   (lambda (window) window 'WEBSTER-CONNECTION-STATUS)))

(define (webster-show-output lines)
  (let ((buffer (find-or-create-buffer (ref-variable webster-buffer-name))))
    (set-buffer-major-mode! buffer (ref-mode-object webster))
    (add-kill-buffer-hook buffer webster-kill-buffer-hook)
    (if (not (fix:= 0 (buffer-length buffer)))
	(guarantee-newlines 2 (buffer-end buffer)))
    (let ((m (mark-right-inserting-copy (buffer-end buffer)))
	  (p (mark-left-inserting-copy (buffer-end buffer))))
      (for-each (lambda (line)
		  (insert-string line p)
		  (insert-newline p))
		lines)
      (mark-temporary! p)
      (let ((window
	     (let ((windows (buffer-windows buffer)))
	       (if (null? windows)
		   (begin
		     (pop-up-buffer buffer #f)
		     (car (buffer-windows buffer)))
		   (car windows)))))
	(set-window-point! window m)
	(set-window-start-mark! window m #t))
      (mark-temporary! m))))

(define (webster-kill-buffer-hook buffer)
  buffer
  (close-webster-server-port))

(define-major-mode webster read-only "Webster"
  "Major mode for interacting with webster server.
Commands:

\\[webster-define]	look up the definition of a word
\\[webster-spellings]	look up possible correct spellings for a word
\\[webster-define]	look up possible endings for a word
\\[webster-quit]	close connection to the Webster server

Use webster-mode-hook for customization."
  (lambda (buffer)
    (local-set-variable!
     mode-line-process
     (lambda (window)
       window
       (if (and webster-server-port
		(not (input-port/eof? webster-server-port)))
	   ": connected"
	   ": disconnected"))
     buffer)
    (event-distributor/invoke! (ref-variable webster-mode-hook buffer)
			       buffer)))

(define-key 'webster #\? 'describe-mode)
(define-key 'webster #\d 'webster-define)
(define-key 'webster #\e 'webster-endings)
(define-key 'webster #\h 'describe-mode)
(define-key 'webster #\q 'webster-quit)
(define-key 'webster #\s 'webster-spellings)

(define (webster-prompt prompt)
  (lambda ()
    (list (prompt-for-string prompt (webster-current-word)))))

(define (webster-current-word)
  (let* ((p (current-point))
	 (s (backward-word p 1 'LIMIT))
	 (e (forward-word s 1 'LIMIT)))
    (if (mark>= e p)
	(extract-string s e)
	(let* ((e* (forward-word p 1 'LIMIT))
	       (s* (backward-word e* 1 'LIMIT)))
	  (if (mark<= s* p)
	      (extract-string s* e*)
	      #f)))))

(define-command webster-define
  "Look up a word in Webster's dictionary."
  (webster-prompt "Look up word")
  (lambda (word) (webster-send "DEFINE" word)))
(copy-command 'webster (ref-command-object webster-define))

(define-command webster-endings
  "Look up possible endings for a word in Webster's dictionary."
  (webster-prompt "Find endings for word")
  (lambda (word) (webster-send "ENDINGS" word)))

(define-command webster-spellings
  "Look up possible correct spellings for a word in Webster's dictionary."
  (webster-prompt "Possible correct spellings for word")
  (lambda (word) (webster-send "SPELL" word)))

(define-command webster-quit
  "Close connection to webster server.
Buffer is not deleted."
  ()
  (lambda () (close-webster-server-port)))