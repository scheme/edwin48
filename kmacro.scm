;;; -*-Scheme-*-
;;;
;;; $Id: kmacro.scm,v 1.50 2007/01/05 21:19:23 cph Exp $
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

;;;; Keyboard Macros


(define *defining-keyboard-macro?* #f)
(define *executing-keyboard-macro?* #f)
(define *keyboard-macro-position*)
(define last-keyboard-macro #f)
(define keyboard-macro-buffer)
(define keyboard-macro-buffer-end)
(define named-keyboard-macros (make-string-table))

(define (with-keyboard-macro-disabled thunk)
  (fluid-let ((*executing-keyboard-macro?* #f)
	      (*defining-keyboard-macro?* #f))
    (dynamic-wind keyboard-macro-event
		  thunk
		  keyboard-macro-event)))

(define (keyboard-macro-disable)
  (set! *defining-keyboard-macro?* #f)
  (keyboard-macro-event))

(define (abort-keyboard-macro)
  (if *executing-keyboard-macro?*
      (*executing-keyboard-macro?* #f)))

(define (keyboard-macro-event)
  (window-modeline-event! (current-window) 'KEYBOARD-MACRO-EVENT))

(define (keyboard-macro-read-key)
  (if (pair? *keyboard-macro-position*)
      (let ((key (car *keyboard-macro-position*)))
	(set! *keyboard-macro-position* (cdr *keyboard-macro-position*))
	key)
      (*executing-keyboard-macro?* #t)))

(define (keyboard-macro-peek-key)
  (if (pair? *keyboard-macro-position*)
      (car *keyboard-macro-position*)
      (*executing-keyboard-macro?* #t)))

(define (keyboard-macro-write-key key)
  (set! keyboard-macro-buffer (cons key keyboard-macro-buffer))
  unspecific)

(define (keyboard-macro-finalize-keys)
  (set! keyboard-macro-buffer-end keyboard-macro-buffer)
  unspecific)

(define (keyboard-macro-execute *macro repeat)
  (fluid-let ((*executing-keyboard-macro?* *executing-keyboard-macro?*)
	      (*keyboard-macro-position*))
    (call-with-current-continuation
     (lambda (c)
       (let ((n repeat))
	 (set! *executing-keyboard-macro?*
	       (lambda (v)
		 (if (and v (> n 0))
		     (begin
		       (set! *keyboard-macro-position* *macro)
		       (set! n (- n 1))
		       (command-reader #f))
		     (c unspecific))))
	 (*executing-keyboard-macro?* #t))))))

(define (keyboard-macro-define name *macro)
  (string-table-put! named-keyboard-macros name last-keyboard-macro)
  (make-command (intern name)
		"Command defined by keyboard macro"
		"P"
		(lambda (#!optional argument)
		  (keyboard-macro-execute *macro
					  (if (or (default-object? argument)
						  (not argument))
					      1
					      argument)))))

(define-command start-kbd-macro
  "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[end-kbd-macro] to finish recording and make the macro available.
Use \\[name-last-kbd-macro] to give it a permanent name.
With argument, append to last keyboard macro defined;
 this begins by re-executing that macro as if you typed it again."
  "P"
  (lambda (argument)
    (if *defining-keyboard-macro?*
	(editor-error "Already defining keyboard macro"))
    (cond ((not argument)
	   (set! keyboard-macro-buffer '())
	   (set! keyboard-macro-buffer-end '())
	   (set! *defining-keyboard-macro?* #t)
	   (keyboard-macro-event)
	   (message "Defining keyboard macro..."))
	  ((not last-keyboard-macro)
	   (editor-error "No keyboard macro has been defined"))
	  (else
	   (set! *defining-keyboard-macro?* #t)
	   (keyboard-macro-event)
	   (message "Appending to keyboard macro...")
	   (keyboard-macro-execute last-keyboard-macro 1)))))

(define-command end-kbd-macro
  "Finish defining a keyboard macro.
The definition was started by \\[start-kbd-macro].
The macro is now available for use via \\[call-last-kbd-macro],
 or it can be given a name with \\[name-last-kbd-macro] and then invoked
 under that name.
With numeric argument, repeat macro now that many times,
 counting the definition just completed as the first repetition."
  "p"
  (lambda (argument)
    (if *defining-keyboard-macro?*
	(begin
	  (keyboard-macro-disable)
	  (set! last-keyboard-macro (reverse keyboard-macro-buffer-end))
	  (message "Keyboard macro defined")))
    (cond ((= argument 0)
	   (keyboard-macro-execute last-keyboard-macro 0))
	  ((> argument 1)
	   (keyboard-macro-execute last-keyboard-macro (- argument 1))))))

(define-command call-last-kbd-macro
  "Call the last keyboard macro that you defined with \\[start-kbd-macro].
To make a macro permanent so you can call it even after
 defining others, use \\[name-last-kbd-macro]."
  "p"
  (lambda (argument)
    (if *defining-keyboard-macro?*
	(editor-error "Can execute anonymous macro while defining one."))
    (if (not last-keyboard-macro)
	(editor-error "No keyboard macro has been defined"))
    (keyboard-macro-execute last-keyboard-macro argument)))

(define-command name-last-kbd-macro
  "Assign a name to the last keyboard macro defined."
  "sName last keyboard macro"
  (lambda (name)
    (if *defining-keyboard-macro?*
	(editor-error "Can't name a keyboard macro while defining one."))
    (if (not last-keyboard-macro)
	(editor-error "No keyboard macro has been defined"))
    (keyboard-macro-define name last-keyboard-macro)))

(define-command write-kbd-macro
  "Save keyboard macro in file.
Use \\[load-file] to load the file.
With argument, also record the keys it is bound to."
  "P"
  (lambda (argument)
    (let ((name
	   (prompt-for-string-table-name "Write keyboard macro"
					 #f
					 named-keyboard-macros
					 'DEFAULT-TYPE 'NO-DEFAULT
					 'REQUIRE-MATCH? #t)))
      (let ((pathname
	     (prompt-for-pathname (string-append "Write keyboard macro "
						 name
						 " to file")
				  #f))
	    (buffer (temporary-buffer "*write-keyboard-macro-temp*")))
	(call-with-output-mark (buffer-point buffer)
	  (lambda (port)
	    (pp `(KEYBOARD-MACRO-DEFINE
		  ',name
		  ',(string-table-get named-keyboard-macros name))
		port
		#t)
	    (if argument
		(for-each (lambda (key)
			    (pp `(DEFINE-KEY 'FUNDAMENTAL ',key ',name)
				port
				#t))
			  (comtab-key-bindings
			   (mode-comtabs (ref-mode-object fundamental))
			   (name->command name))))))
	(set-buffer-pathname! buffer pathname)
	(write-buffer buffer)
	(kill-buffer buffer)))))

(define-command kbd-macro-query
  "Query user during keyboard macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a keyboard macro.
 You can give different commands each time the macro executes.
Without argument, reads a character.  Your options are:
 Space -- execute the rest of the macro.
 Rubout -- skip the rest of the macro; start next repetition.
 C-d -- skip the rest of the macro and don't repeat it any more.
 C-r -- Enter a recursive edit, then on exit ask again for a character
 C-l -- redisplay screen and ask again."
  "P"
  (lambda (argument)
    (cond ((and (not *defining-keyboard-macro?*)
		(not *executing-keyboard-macro?*))
	   (editor-error "Not defining or executing kbd macro"))
	  (argument
	   (with-keyboard-macro-disabled enter-recursive-edit))
	  (*executing-keyboard-macro?*
	   (let loop ()
	     (let ((char
		    (with-keyboard-macro-disabled
		     (lambda ()
		       (set-command-prompt!
			"Proceed with macro? (Space, DEL, C-d, C-r or C-l)")
		       (keyboard-read)))))
	       (let ((test-for
		      (lambda (char*)
			(char=? char (remap-alias-key char*)))))
		 (cond ((input-event? char)
			(abort-current-command char))
		       ((not (char? char))
			(editor-beep)
			(loop))
		       ((test-for #\space)
			unspecific)
		       ((test-for #\rubout)
			(*executing-keyboard-macro?* #t))
		       ((test-for #\C-d)
			(*executing-keyboard-macro?* #f))
		       ((test-for #\C-r)
			(with-keyboard-macro-disabled enter-recursive-edit)
			(loop))
		       ((test-for #\C-l)
			((ref-command recenter) #f)
			(loop))
		       (else
			(editor-beep)
			(loop))))))))))
