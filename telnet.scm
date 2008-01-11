#| -*-Scheme-*-

$Id: telnet.scm,v 1.19 2007/01/05 21:19:24 cph Exp $

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

;;;; Run Telnet in a buffer


(define-variable telnet-prompt-pattern
  "#f or Regexp to match prompts in telnet buffers."
  #f)				    

(define-major-mode telnet comint "Telnet"
  "Major mode for interacting with the telnet program.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.

Customization: Entry to this mode runs the hooks on comint-mode-hook
and telnet-mode-hook, in that order."
  (lambda (buffer)
    (define-variable-local-value! buffer
	(ref-variable-object comint-prompt-regexp)
      (or (ref-variable telnet-prompt-pattern buffer)
	  (ref-variable shell-prompt-pattern buffer)))
    (event-distributor/invoke! (ref-variable telnet-mode-hook buffer) buffer)))

(define-key 'telnet #\C-m 'telnet-send-input)
(define-key 'telnet '(#\C-c #\C-c) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-d) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-g) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-q) 'telnet-send-character)
(define-key 'telnet '(#\C-c #\C-z) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-\\) 'telnet-self-send)

;;;moved to "loadef.scm".
;;;(define-variable telnet-mode-hook
;;;  "An event distributor that is invoked when entering Telnet mode."
;;;  (make-event-distributor))

(define-command telnet
  "Run telnet in a buffer.
With a prefix argument, it unconditionally creates a new telnet connection.
If port number is typed after hostname (separated by a space),
use it instead of the default."
  "sTelnet to host\nP"
  (lambda (host new-process?)
    (select-buffer
     (let ((mode (ref-mode-object telnet))
	   (buffer-name
	     (let ((buffer-name (string-append "*" host "-telnet*")))
	       (if (not new-process?)
		   buffer-name
		   (new-buffer buffer-name)))))
       (let ((r (re-string-match "\\([^ ]+\\) \\([^ ]+\\)" host)))
	 (if r
	     (let ((host
		    (substring host
			       (re-match-start-index 1 r)
			       (re-match-end-index 1 r)))
		   (port
		    (substring host
			       (re-match-start-index 2 r)
			       (re-match-end-index 2 r))))
	       (if (not (exact-nonnegative-integer? (string->number port)))
		   (editor-error "Port must be a positive integer: " port))
	       (make-comint mode buffer-name "telnet" host port))
	     (make-comint mode buffer-name "telnet" host)))))))

(add-event-receiver! (ref-variable telnet-mode-hook)
		     comint-strip-carriage-returns)

(define-command telnet-send-input
  "Send input to telnet process.
The input is entered in the history ring."
  ()
  (lambda () (comint-send-input "\n" #t)))

(define-command telnet-self-send
  "Immediately send the last command key to the telnet process.
Typically bound to C-c <char> where char is an interrupt key for the process
running remotely."
  ()
  (lambda () (process-send-char (current-process) (last-command-key))))

(define-command telnet-send-character
  "Read a character and send it to the telnet process.
With prefix arg, the character is repeated that many times."
  "p"
  (lambda (argument)
    (let ((char (read-quoted-char "Send Character: "))
	  (process (current-process)))
      (cond ((= argument 1)
	     (process-send-char process char))
	    ((> argument 1)
	     (process-send-string process (make-string argument char)))))))