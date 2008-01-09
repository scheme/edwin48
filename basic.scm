;;; -*-Scheme-*-
;;;
;;; $Id: basic.scm,v 1.147 2007/01/05 21:19:23 cph Exp $
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

;;;; Basic Commands


(define-command self-insert-command
  "Insert the character you type.
Whichever character you type to run this command is inserted."
  "P"
  (lambda (argument)
    (let ((char (last-command-key)))
      (if (not (char? char))
	  (editor-error "self-insert-command only works on character keys."))
      (self-insert char (command-argument-numeric-value argument) #t))))

(define (self-insert char n allow-auto-fill?)
  (and (> n 0)
       (let ((point (current-point))
	     (hairy? #f))
	 (if (and (not (group-start? point))
		  (buffer-minor-mode? (mark-buffer point)
				      (ref-mode-object abbrev))
		  (not (char=? #\w (char-syntax char)))
		  (char=? #\w (char-syntax (extract-left-char point))))
	     (let ((t (group-modified-tick (mark-group point))))
	       ((ref-command expand-abbrev) point)
	       (if (not (fix:= t (group-modified-tick (mark-group point))))
		   (set! hairy? #t))))
	 (if (and allow-auto-fill?
		  (or (char=? #\space char)
		      (char=? #\newline char))
		  (current-minor-mode? (ref-mode-object auto-fill)))
	     (let ((t (group-modified-tick (mark-group point))))
	       (auto-fill-break (current-point))
	       (if (not (fix:= t (group-modified-tick (mark-group point))))
		   (set! hairy? #t))))
	 (insert-chars char n)
	 hairy?)))

(define (read-quoted-char prompt-string)
  (let ((read-ascii-char
	 (lambda ()
	   (let ((input (with-editor-interrupts-disabled keyboard-read)))
	     (if (input-event? input)
		 (abort-current-command input)
		 (begin
		   (if (not (and (char? input) (char-ascii? input)))
		       (editor-error "Can't quote non-ASCII char:" input))
		   (set-command-prompt!
		    (string-append (command-prompt) (key-name input)))
		   input))))))
    (let ((read-digit
	   (lambda ()
	     (or (char->digit (read-ascii-char) 8)
		 (editor-error "Not an octal digit")))))
      (set-command-prompt! prompt-string)
      (let ((char (read-ascii-char)))
	(let ((digit (char->digit char 4)))
	  (if digit
	      (ascii->char
	       (let ((digit2 (read-digit)))
		 (let ((digit3 (read-digit)))
		   (+ (* (+ (* digit 8) digit2) 8) digit3))))
	      char))))))

(define-command quoted-insert
  "Reads a character and inserts it."
  "p"
  (lambda (argument)
    (insert-chars (read-quoted-char "Quote Character: ")
		  argument)))

(define-command open-line
  "Insert a newline after point.
Differs from ordinary insertion in that point remains
before the inserted characters.
With an argument, inserts several newlines."
  "P"
  (lambda (argument)
    (let ((m* (mark-right-inserting (current-point))))
      (insert-newlines (command-argument-numeric-value argument))
      (set-current-point! m*))))

(define-command narrow-to-region
  "Restrict editing in current buffer to text between point and mark.
Use \\[widen] to undo the effects of this command."
  "r"
  region-clip!)

(define-command widen
  "Remove restrictions from current buffer.
Allows full text to be seen and edited."
  ()
  (lambda ()
    (buffer-widen! (current-buffer))))

(define-command set-key
  "Define a key binding from the keyboard.
Prompts for a command and a key, and sets the key's binding.
The key is bound in fundamental mode."
  (lambda ()
    (let ((command (prompt-for-command "Command" 'HISTORY 'SET-KEY)))
      (list command
	    (prompt-for-key (string-append "Put \""
					   (command-name-string command)
					   "\" on key")
			    (mode-comtabs (ref-mode-object fundamental))))))
  (lambda (command key)
    (if (prompt-for-confirmation? "Go ahead")
	(define-key 'fundamental key (command-name command)))))

(define-variable buffer-reallocation-factor
  "Determines how much a buffer grows by when it needs to expand.
This is a real number greater than one.  When a buffer is expanded,
its size is multiplied by this factor until it is big enough.
Similarly, when a buffer shrinks, its size is divided by this factor.

Increasing this factor reduces the average time for insertion and
deletion, but increases the average space used by the buffer.

The minimum ratio between the number of characters in a buffer and the
amount of space allocated to hold them is
    (/ 1 (expt buffer-reallocation-factor 2))"
  5/4
  (lambda (object) (and (real? object) (> object 1))))

;;;; Prefixes

(define-command control-prefix
  "Sets Control-bit of following character.
This command followed by an = is equivalent to a Control-=."
  ()
  (lambda ()
    (read-extension-key
     (lambda (char)
       (merge-bucky-bits char char-bit:control)))))

(define-command meta-prefix
  "Sets Meta-bit of following character. 
Turns a following A into a Meta-A.
If the Metizer character is Altmode, it turns ^A
into Control-Meta-A.  Otherwise, it turns ^A into plain Meta-A."
  ()
  (lambda ()
    (read-extension-key
     (lambda (char)
       (merge-bucky-bits (if (eqv? (current-command-key) #\altmode)
			     char
			     (char-base char))
			 char-bit:meta)))))

(define-command control-meta-prefix
  "Sets Control- and Meta-bits of following character.
Turns a following A (or C-A) into a Control-Meta-A."
  ()
  (lambda ()
    (read-extension-key
     (lambda (char)
       (merge-bucky-bits char (fix:or char-bit:control char-bit:meta))))))

(define execute-extended-keys?
  #t)

(define extension-commands
  (list (name->command 'control-prefix)
	(name->command 'meta-prefix)
	(name->command 'control-meta-prefix)))

(define (read-extension-key modifier)
  (if execute-extended-keys?
      (set-command-prompt-prefix!))
  (let ((key (modifier (with-editor-interrupts-disabled keyboard-read))))
    (if execute-extended-keys?
	(dispatch-on-key (current-comtabs) key)
	key)))

(define-command prefix-key
  "This is a prefix for more commands.
It reads another character (a subcommand) and dispatches on it."
  ()
  (lambda ()
    (set-command-prompt-prefix!)
    (let loop ()
      (let ((input (with-editor-interrupts-disabled keyboard-read)))
	(if (input-event? input)
	    (begin
	      (apply-input-event input)
	      (loop))
	    (dispatch-on-key (current-comtabs)
			     (let ((prefix-key (current-command-key)))
			       ((if (pair? prefix-key) append cons)
				prefix-key
				(list input)))))))))

(define (set-command-prompt-prefix!)
  (set-command-prompt!
   (string-append-separated
    (command-argument-prompt)
    (string-append (xkey->name (current-command-key)) " -"))))

(define-command execute-extended-command
  "Read an extended command from the terminal with completion.
This command reads the name of a command, with completion.  Then the
command is invoked.  Completion is done as the command name is typed
For more information type the HELP key while entering the name."
  ()
  (lambda ()
    (dispatch-on-command
     (prompt-for-command
      ;; Prompt with the name of the command char.
      (list (string-append (xkey->name (current-command-key)) " "))
      'HISTORY 'EXECUTE-EXTENDED-COMMAND)
     #t)))

;;;; Errors

(define-command keyboard-quit
  "Signals a quit condition."
  ()
  (lambda ()
    (editor-beep)
    (temporary-message "Quit")
    (^G-signal)))

(define-command undefined
  "This command is used to capture undefined keys."
  ()
  (lambda ()
    (editor-error "Undefined command: " (xkey->name (current-command-key)))))

(define (barf-if-read-only)
  (editor-error "Trying to modify read only text."))

(define (check-first-group-modification group)
  (let ((buffer (group-buffer group)))
    (if (and buffer
	     (buffer-truename buffer)
	     (buffer-modification-time buffer)
	     (not (verify-visited-file-modification-time? buffer)))
	(ask-user-about-supersession-threat buffer))))

(define (ask-user-about-supersession-threat buffer)
  (if (not
       (with-selected-buffer buffer
	 (lambda ()
	   (prompt-for-confirmation?
	    "File has changed on disk; really want to edit the buffer"))))
      (editor-error "File changed on disk: "
		    (->namestring (buffer-pathname buffer))))
  (message
   "File on disk now will become a backup file if you save these changes.")
  (set-buffer-backed-up?! buffer #f))

(define (editor-failure . strings)
  (cond ((not (null? strings)) (apply message strings))
	(*defining-keyboard-macro?* (clear-message)))
  (editor-beep)
  (keyboard-macro-disable))

(define-variable beeping-allowed?
  "False if Edwin must never beep."
  #t)

(define (editor-beep)
  (if (ref-variable beeping-allowed?)
      (screen-beep (selected-screen))))

;;;; Level Control

(define-command exit-recursive-edit
  "Exit normally from a subsystem of a level of editing."
  ()
  (lambda ()
    (exit-recursive-edit 'EXIT)))

(define-command abort-recursive-edit
  "Abnormal exit from recursive editing command.
The recursive edit is exited and the command that invoked it is aborted.
For a normal exit, you should use \\[exit-recursive-edit], NOT this command."
  ()
  (lambda ()
    (exit-recursive-edit 'ABORT)))

;;;; Leaving Edwin

;; Set this to #F to indicate that returning from the editor has the
;; same effect as calling %EXIT, or to prevent the editor from
;; returning to scheme.
(define editor-can-exit? #t)

;; Set this to #F to indicate that calling QUIT has the same effect
;; as calling %EXIT, or to prevent the editor from suspending to the OS.
(define scheme-can-quit?
  #t)

;; Set this to #T to force the exit commands to always prompt for
;; confirmation before killing Edwin.
(define paranoid-exit? #f)

(define-command suspend-scheme
  "Go back to Scheme's superior job.
With argument, saves visited file first."
  "P"
  (lambda (argument)
    (if argument (save-buffer (current-buffer) #f))
    (if (and scheme-can-quit? (os/scheme-can-quit?))
	(quit-scheme)
	(editor-error "Scheme cannot be suspended"))))

(define-command suspend-edwin
  "Stop Edwin and return to Scheme."
  ()
  (lambda ()
    (if (not editor-can-exit?)
	(editor-error "Edwin cannot be suspended"))
    (quit-editor)))

(define (save-buffers-and-exit no-confirmation? noun exit)
  (save-some-buffers no-confirmation? #t)
  (if (and (or (not (there-exists? (buffer-list)
		      (lambda (buffer)
			(and (buffer-modified? buffer)
			     (buffer-pathname buffer)))))
	       (prompt-for-yes-or-no? "Modified buffers exist; exit anyway"))
	   (if (there-exists? (process-list)
		 (lambda (process)
		   (and (not (process-kill-without-query process))
			(process-runnable? process))))
	       (and (prompt-for-yes-or-no?
		     "Active processes exist; kill them and exit anyway")
		    (begin
		      (for-each delete-process (process-list))
		      #t))
	       (or (not paranoid-exit?)
		   (prompt-for-yes-or-no? (string-append "Kill " noun)))))
      (exit)))

(define-command save-buffers-kill-scheme
  "Offer to save each buffer, then kill Scheme.
With prefix arg, silently save all file-visiting buffers, then kill."
  "P"
  (lambda (no-confirmation?)
    (save-buffers-and-exit no-confirmation? "Scheme" exit-scheme)))

(define (save-buffers-kill-edwin #!optional no-confirmation?)
  (let ((no-confirmation?
	 (and (not (default-object? no-confirmation?)) no-confirmation?)))
    (if editor-can-exit?
	(save-buffers-and-exit no-confirmation? "Edwin" exit-editor)
	(save-buffers-and-exit no-confirmation? "Scheme" exit-scheme))))

(define-command save-buffers-kill-edwin
  "Offer to save each buffer, then kill Edwin, returning to Scheme.
With prefix arg, silently save all file-visiting buffers, then kill."
  "P"
  save-buffers-kill-edwin)

;;;; Comment Commands

(define-variable-per-buffer comment-column
  "Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer."
  32
  exact-nonnegative-integer?)

(define-variable comment-locator-hook
  "Procedure to find a comment, or #f if no comment syntax defined.
The procedure is passed a mark, and should return #f if it cannot
find a comment, or a pair of marks.  The car should be the start of
the comment, and the cdr should be the end of the comment's starter."
  #f)

(define-variable comment-indent-hook
  "Procedure to compute desired indentation for a comment.
The procedure is passed the start mark of the comment
and should return the column to indent the comment to."
  #f)

(define-variable comment-start
  "String to insert to start a new comment, or #f if no comment syntax defined."
  ""
  string-or-false?)

(define-variable comment-end
  "String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line."
  ""
  string?)

(define-command set-comment-column
  "Set the comment column based on point.
With no arg, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
Otherwise, set the comment column to the argument."
  "P"
  (lambda (argument)
    (if (command-argument-negative-only? argument)
	((ref-command kill-comment))
	(let ((column
	       (or (command-argument-value argument)
		   (current-column))))
	  (set-variable! comment-column column)
	  (message "comment-column set to " column)))))

(define-command indent-for-comment
  "Indent this line's comment to comment column, or insert an empty comment."
  ()
  (lambda ()
    (if (not (ref-variable comment-locator-hook))
	(editor-error "No comment syntax defined"))
    (let ((start (line-start (current-point) 0))
	  (end (line-end (current-point) 0)))
      (let ((com ((ref-variable comment-locator-hook) start)))
	(set-current-point! (if com (car com) end))
	(let ((comment-end (and com (mark-permanent! (cdr com)))))
	  (let ((indent
		 ((ref-variable comment-indent-hook) (current-point))))
	    (maybe-change-column indent)
	    (if comment-end
		(set-current-point! comment-end)
		(begin
		  (insert-string (ref-variable comment-start))
		  (insert-comment-end (current-point))))))))))

(define-variable comment-multi-line
  "True means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter."
  #f
  boolean?)

(define-command indent-new-comment-line
  "Break line at point and indent, continuing comment if presently within one."
  ()
  (lambda ()
    (indent-new-comment-line (current-point) (ref-variable fill-prefix))))

(define (indent-new-comment-line mark fill-prefix)
  (let ((mark (mark-left-inserting-copy mark)))
    (delete-horizontal-space mark)
    (insert-newlines 1 mark)
    (let ((if-not-in-comment
	   (lambda ()
	     (if fill-prefix
		 (insert-string fill-prefix mark)
		 (with-selected-buffer (mark-buffer mark)
		   (lambda ()
		     (with-current-point mark
		       (ref-command indent-according-to-mode))))))))
      (if (ref-variable comment-locator-hook mark)
	  (let ((com ((ref-variable comment-locator-hook mark)
		      (line-start mark -1))))
	    (if com
		(let ((start-column (mark-column (car com)))
		      (end-column (mark-column (cdr com)))
		      (comment-start (extract-string (car com) (cdr com))))
		  (if (ref-variable comment-multi-line mark)
		      (maybe-change-column end-column mark)
		      (begin
			(insert-string (ref-variable comment-end mark)
				       (line-end mark -1))
			(maybe-change-column start-column mark)
			(insert-string comment-start mark)))
		  (if (line-end? mark)
		      (insert-comment-end mark)))
		(if-not-in-comment)))
	  (if-not-in-comment)))
    (mark-temporary! mark)))

(define (insert-comment-end mark)
  (let ((mark (mark-right-inserting-copy mark)))
    (insert-string (ref-variable comment-end mark) mark)
    (set-buffer-point! (mark-buffer mark) mark)
    (mark-temporary! mark)))

(define-command kill-comment
  "Kill the comment on this line, if any."
  ()
  (lambda ()
    (if (not (ref-variable comment-locator-hook))
	(editor-error "No comment syntax defined")
	(let ((start (line-start (current-point) 0))
	      (end (line-end (current-point) 0)))
	  (let ((com ((ref-variable comment-locator-hook) start)))
	    (if com
		(kill-string (horizontal-space-start (car com)) end)))))))

;;;; Useful Documentation

(define-command define-command
  "Scheme special form used to define commands:

  (define-command NAME DOCUMENTATION INTERACTIVE-SPEC PROCEDURE)

where:
  NAME is a symbol;
  DOCUMENTATION is a string;
  INTERACTIVE-SPEC describes how to call PROCEDURE when the command is
    invoked interactively (see below); and
  PROCEDURE is a Scheme procedure that is called to perform the
    command's actions.

INTERACTIVE-SPEC and PROCEDURE are evaluated, the others aren't.

INTERACTIVE-SPEC specifies a way of parsing arguments for interactive
use of a command.  For example, write
  (define-command foo \"Doc string\" \"p\" (lambda (arg) ...use arg...))
to make arg be the prefix numeric argument when foo is invoked.

INTERACTIVE-SPEC is usually a string containing a code letter
 followed by a prompt.  (Some code letters do not use I/O to get
 the argument and do not need prompts.)  To prompt for multiple arguments,
 give a code letter, its prompt, a newline, and another code letter, etc.
If INTERACTIVE-SPEC is not a string, it is either a procedure or ().
 If it's a procedure, then the procedure is invoked with no arguments,
 and should return a list of arguments for the command.
 Otherwise, if it's the empty list, the command gets no arguments.

Code letters available are:
b -- Name of existing buffer (string).
B -- Name of buffer, possibly nonexistent (string).
c -- Character.
C -- Command name (symbol).
d -- Value of point (editor-mark object).  Does not do I/O.
D -- Directory name (string).
f -- Existing file name (string).
F -- Possibly nonexistent file name (string).
k -- Key sequence (list of chars).
m -- Value of mark (editor-mark object).  Does not do I/O.
n -- Number read using minibuffer.
N -- Prefix arg converted to number, or if none, do like code `n'.
p -- Prefix arg converted to number, or 1 if no prefix.  Does not do I/O.
P -- Prefix arg converted to number, or #F if no prefix.  Does not do I/O.
r -- Region: current region (editor-region object).  Does no I/O.
s -- Any string.
v -- Variable name (symbol).
x -- Scheme expression read but not evaluated.
X -- Scheme expression read and evaluated.
In addition, if the first character of the string is '*' then an error is
 signaled if the buffer is read-only.
 This happens before reading any arguments."
  ()
  (lambda () (editor-error "DEFINE-COMMAND shouldn't be invoked")))
