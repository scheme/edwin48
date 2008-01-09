#| -*-Scheme-*-

$Id: motcom.scm,v 1.54 2007/01/05 21:19:23 cph Exp $

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

;;;; Motion Commands

(declare (usual-integrations))

(define-command beginning-of-line
  "Move point to beginning of line."
  "p"
  (lambda (argument)
    (move-thing line-start (- argument 1) 'FAILURE)))

(define-command backward-char
  "Move back one character.
With argument, move that many characters backward.
Negative arguments move forward."
  "p"
  (lambda (argument)
    (move-thing mark- argument 'FAILURE)))

(define-command end-of-line
  "Move point to end of line."
  "p"
  (lambda (argument)
    (move-thing line-end (- argument 1) 'FAILURE)))

(define-command forward-char
  "Move forward one character.
With argument, move that many characters forward.
Negative args move backward."
  "p"
  (lambda (argument)
    (move-thing mark+ argument 'FAILURE)))

(define-command beginning-of-buffer
  "Go to beginning of buffer (leaving mark behind).
With arg from 0 to 10, goes that many tenths of the file
down from the beginning.  Just \\[universal-argument] as arg means go to end."
  "P"
  (lambda (argument)
    (push-current-mark! (current-point))
    (cond ((not argument)
	   (set-current-point! (buffer-start (current-buffer))))
	  ((command-argument-multiplier-only? argument)
	   (set-current-point! (buffer-end (current-buffer))))
	  ((and (number? argument) (<= 0 argument 10))
	   (set-current-point!
	    (region-10ths (buffer-region (current-buffer)) argument))))))

(define-command end-of-buffer
  "Go to end of buffer (leaving mark behind).
With arg from 0 to 10, goes up that many tenths of the file from the end."
  "P"
  (lambda (argument)
    (push-current-mark! (current-point))
    (cond ((not argument)
	   (set-current-point! (buffer-end (current-buffer))))
	  ((and (number? argument) (<= 0 argument 10))
	   (set-current-point!
	    (region-10ths (buffer-region (current-buffer))
			  (- 10 argument)))))))

(define (region-10ths region n)
  (mark+ (region-start region)
	 (quotient (* n (region-count-chars region)) 10)))

(define-command goto-char
  "Goto char ARG, counting from char 1 at beginning of buffer."
  "NGoto char"
  (lambda (n)
    (let ((group (buffer-group (current-buffer))))
      (let ((index (- n 1)))
	(if (or (< index (group-start-index group))
		(> index (group-end-index group)))
	    (editor-error))
	(set-current-point! (make-mark group index))))))

(define-variable goal-column
  "Semipermanent goal column for vertical motion,
as set by \\[set-goal-column], or false, indicating no goal column."
  false)

(define temporary-goal-column-tag
  "Temporary Goal Column")

(define-command set-goal-column
  "Set (or flush) a permanent goal for vertical motion.
With no argument, makes the current column the goal for vertical
motion commands.  They will always try to go to that column.
With argument, clears out any previously set goal.
Only \\[previous-line] and \\[next-line] are affected."
  "P"
  (lambda (argument)
    (set-variable! goal-column (and (not argument) (current-column)))))

(define (current-goal-column)
  (or (ref-variable goal-column)
      (command-message-receive temporary-goal-column-tag
	identity-procedure
	current-column)))

(define-variable next-line-add-newlines
  "Controls whether `next-line' may add newlines at buffer end.
If true, when `next-line' is run at the end of a buffer, it will
  insert a new line to travel onto.
If false, when `next-line' is run at the end of a buffer, it will
  beep and do nothing."
  #t
  boolean?)

(define-command next-line
  "Move down vertically to next real line.
Continuation lines are skipped.
If `next-line-add-newlines' is true and this command is given after
  the last newline in the buffer, make a new one at the end."
  "P"
  (lambda (argument)
    (let ((argument (command-argument-value argument))
	  (column (current-goal-column)))
      (cond ((not argument)
	     (let ((mark (line-start (current-point) 1 false)))
	       (if mark
		   (set-current-point! (move-to-column mark column))
		   (begin
		     (set-current-point! (group-end (current-point)))
		     (if (ref-variable next-line-add-newlines)
                         (insert-newline)
                         (editor-failure))))))
	    ((not (zero? argument))
	     (set-current-point!
	      (move-to-column (line-start (current-point) argument 'FAILURE)
			      column))))
      (set-command-message! temporary-goal-column-tag column))))

(define-command previous-line
  "Move up vertically to next real line.
Continuation lines are skipped."
  "p"
  (lambda (argument)
    (let ((column (current-goal-column)))
      (if (not (zero? argument))
	  (set-current-point!
	   (move-to-column (line-start (current-point) (- argument) 'FAILURE)
			   column)))
      (set-command-message! temporary-goal-column-tag column))))

;;;; Jokes

(define-command hyper-space
  "Engage warp drive."
  ()
  (lambda ()
    (message "Sorry, but superluminal travel is not available now.")))

(define-command super-man
  "Deliver super man joke."
  ()
  (lambda ()
    (message "It's a bird... It's a plane... No, it's #\S-man!")))

(define-command super-menu
  "Deliver super menu joke."
  ()
  (lambda ()
    (message "Big Mac, Medium Fries, Small Coke")))