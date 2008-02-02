#| -*-Scheme-*-

$Id: input.scm,v 1.108 2008/01/30 20:02:02 cph Exp $

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

;;;; Keyboard Input


#|

The interaction between command prompts and messages is complicated.
Here is a description of the state transition graph.

State variables:

a : there is a command prompt
b : the command prompt is displayed
c : there is a message
d : the message should be erased (also implies it is displayed)

Constraints:

b implies a
d implies c
b implies (not d)
c implies (not b)

Valid States:

abcd  Hex  Description
0000  0  : idle state
0010  2  : message
0011  3  : temporary message
1000  8  : undisplayed command prompt
1010  A  : message with undisplayed command prompt
1011  B  : temporary message with undisplayed command prompt
1100  C  : displayed command prompt

Transition operations:

0: reset-command-prompt
1: set-command-prompt
2: message
3: temporary-message
4: clear-message
5: timeout

Transition table.  Each row is labeled with initial state, each column
with a transition operation.  Each element is the new state for the
given starting state and transition operation.

  012345
0 082300
8 08238C
C *C23CC	* is special -- see the code.
2 2A2302
3 3B2300
A 2AAB8C
B 3BAB8C

|#

(define command-prompt-string)
(define command-prompt-displayed?)
(define message-string)
(define message-should-be-erased?)
(define auto-save-keystroke-count)

(add-event-receiver! editor-initializations
  (lambda ()
    (set! command-prompt-string #f)
    (set! command-prompt-displayed? #f)
    (set! message-string #f)
    (set! message-should-be-erased? #f)
    (set! auto-save-keystroke-count 0)
    unspecific))

(define (reset-command-prompt!)
  ;; Should only be called by the command reader.  This prevents
  ;; carryover from one command to the next.
  (set! command-prompt-string #f)
  (if command-prompt-displayed?
      ;; To make it more visible, the command prompt is erased after
      ;; timeout instead of right away.
      (begin
	(set! command-prompt-displayed? #f)
	(set! message-should-be-erased? #t)))
  unspecific)

(define (command-prompt)
  (or command-prompt-string ""))

(define (set-command-prompt! string)
  (if (not (string-null? string))
      (begin
	(set! command-prompt-string string)
	(if command-prompt-displayed?
	    (set-current-message! string)))))

(define (append-command-prompt! string)
  (if (not (string-null? string))
      (set-command-prompt! (string-append (command-prompt) string))))

(define (message . args)
  (%message (message-args->string args) #f))

(define (temporary-message . args)
  (%message (message-args->string args) #t))

(define (message-wrapper temporary? . args)
  (let ((msg (message-args->string args)))
    (lambda (thunk)
      (%message (string-append msg "...") temporary?)
      (let ((value (thunk)))
	(%message (string-append msg "...done") temporary?)
	value))))

(define (%message string temporary?)
  (if command-prompt-displayed?
      (begin
	(set! command-prompt-string #f)
	(set! command-prompt-displayed? #f)))
  (set! message-string string)
  (set! message-should-be-erased? temporary?)
  (set-current-message! string))

(define (message-args->string args)
  (apply string-append
	 (map (lambda (x) (if (string? x) x (write-to-string x)))
	      args)))

(define (append-message . args)
  ;; Deprecated.  Don't use this.
  (if (not message-string)
      (error "Attempt to append to nonexistent message"))
  (let ((string (string-append message-string (message-args->string args))))
    (set! message-string string)
    (set-current-message! string)))

(define (clear-message)
  (if message-string
      (begin
	(set! message-string #f)
	(set! message-should-be-erased? #f)
	(if (not command-prompt-displayed?)
	    (clear-current-message!)))))

(define (keyboard-peek)
  (if *executing-keyboard-macro?*
      (keyboard-macro-peek-key)
      (keyboard-read-1 (editor-peek current-editor) #t)))

(define (keyboard-read #!optional no-save?)
  (set! keyboard-keys-read (+ keyboard-keys-read 1))
  (if *executing-keyboard-macro?*
      (keyboard-macro-read-key)
      (let ((key (keyboard-read-1 (editor-read current-editor) #f)))
	(cond ((key? key)
	       (set! auto-save-keystroke-count
		     (fix:+ auto-save-keystroke-count 1))
	       (if (not (and (not (default-object? no-save?)) no-save?))
		   (ring-push! (current-char-history) key))
	       (if *defining-keyboard-macro?* (keyboard-macro-write-key key)))
	      (*defining-keyboard-macro?*
	       ((ref-command end-kbd-macro) 1)))
	key)))

(define (keyboard-read-char)
  (let loop ()
    (let ((key (keyboard-read)))
      (if (char? key)
	  key
	  (begin
	    (if (input-event? key)
		(apply-input-event key))
	    (loop))))))

(define (keyboard-peek-no-hang)
  (handle-simple-events (editor-peek-no-hang current-editor) #t))

(define (handle-simple-events thunk discard?)
  (let loop ()
    (let ((input (thunk)))
      (if (and (input-event? input)
	       (let ((type (input-event/type input)))
		 (or (eq? type 'UPDATE)
		     (eq? type 'SET-SCREEN-SIZE)
		     (and (eq? type 'DELETE-SCREEN)
			  (eq? (input-event/operator input) delete-screen!)
			  (not (selected-screen?
				(car (input-event/operands input))))))))
	  (begin
	    (apply-input-event input)
	    (if discard? ((editor-read current-editor)))
	    (loop))
	  input))))

(define read-key-timeout/fast 500)
(define read-key-timeout/slow 2000)

(define (keyboard-read-1 reader discard?)
  (remap-alias-key
   (handle-simple-events
    (lambda ()
      (let ((peek-no-hang (editor-peek-no-hang current-editor)))
	(if (not (peek-no-hang))
	    (begin
	      (if (let ((interval (ref-variable auto-save-interval))
			(count auto-save-keystroke-count))
		    (and (fix:> count 20)
			 (> interval 0)
			 (> count interval)))
		  (begin
		    (do-auto-save)
		    (set! auto-save-keystroke-count 0)))
	      (update-screens! #f)))
	(let ((wait
	       (lambda (timeout)
		 (let ((t (+ (real-time-clock) timeout)))
		   (let loop ()
		     (cond ((peek-no-hang) #f)
			   ((>= (real-time-clock) t) #t)
			   (else (loop))))))))
	  ;; Perform the appropriate juggling of the minibuffer message.
	  (cond ((within-typein-edit?)
		 (if message-string
		     (begin
		       (wait read-key-timeout/slow)
		       (set! message-string #f)
		       (set! message-should-be-erased? #f)
		       (clear-current-message!))))
		((and (or message-should-be-erased?
			  (and command-prompt-string
			       (not command-prompt-displayed?)))
		      (wait read-key-timeout/fast))
		 (set! message-string #f)
		 (set! message-should-be-erased? #f)
		 (if command-prompt-string
		     (begin
		       (set! command-prompt-displayed? #t)
		       (set-current-message! command-prompt-string))
		     (clear-current-message!)))))
	(reader)))
    discard?)))