#| -*-Scheme-*-

$Id: kilcom.scm,v 1.78 2008/01/30 20:02:03 cph Exp $

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

;;;; Kill Commands


;;;; Deletion

(define (delete-region mark)
  (if (not mark)
      (editor-error "Delete exceeds buffer bounds"))
  (delete-string mark (current-point)))

(define (kill-region mark)
  (if (not mark)
      (editor-error "Kill exceeds buffer bounds"))
  (kill-string mark (current-point)))

(define-command delete-region
  "Delete the text between point and mark."
  "*r"
  (lambda (region)
    (region-delete! region)))

(define-command delete-backward-char
  "Delete character before point.
With argument, kills several characters (saving them).
Negative args kill characters forward."
  "P"
  (lambda (argument)
    (if (not argument)
	(delete-region (mark-1+ (current-point)))
	(kill-region
	 (mark- (current-point) (command-argument-value argument))))))

(define-command delete-char
  "Delete character after point.
With argument, kill than many characters (saving them).
Negative args kill characters backward."
  "P"
  (lambda (argument)
    (if (not argument)
	(delete-region (mark1+ (current-point)))
	(kill-region
	 (mark+ (current-point) (command-argument-value argument))))))

(define-command kill-line
  "Kill to end of line, or kill an end of line.
At the end of a line (only blanks following) kill through the newline.
Otherwise, kill the rest of the line but not the newline.  
With argument (positive or negative), kill specified number of lines.
An argument of zero means kill to beginning of line, nothing if at beginning.
Killed text is pushed onto the kill ring for retrieval."
  "P"
  (lambda (argument)
    (let ((argument (command-argument-value argument))
	  (point (current-point)))
      (kill-region
       (cond ((not argument)
	      (let ((end (line-end point 0)))
		(if (and (region-blank? (make-region point end))
			 (not (group-end? end)))
		    (mark1+ end)
		    end)))
	     ((positive? argument)
	      (and (not (group-end? point))
		   (line-start point argument 'LIMIT)))
	     ((zero? argument)
	      (line-start point 0))
	     (else
	      (and (not (group-start? point))
		   (line-start point
			       (if (line-start? point)
				   argument
				   (1+ argument))
			       'LIMIT))))))))

(define-command backward-delete-char-untabify
  "Delete character before point, turning tabs into spaces.
Rather than deleting a whole tab, the tab is converted into the
appropriate number of spaces and then one space is deleted."
  "P"
  (lambda (argument)
    (define (back n)
      (let ((point (current-point)))
	(let ((m1 (mark- point n 'LIMIT)))
	  (let ((tab (char-search-backward #\tab point m1)))
	    (if (not tab)
		m1
		(begin
		  (convert-tab-to-spaces! tab)
		  (back n)))))))
    (define (forth n)
      (let ((point (current-point)))
	(let ((m1 (mark+ point n 'LIMIT)))
	  (let ((tab (char-search-forward #\tab point m1)))
	    (if (not tab)
		m1
		(begin
		  (convert-tab-to-spaces! (mark-1+ tab))
		  (forth n)))))))
    (let ((argument (command-argument-value argument)))
      (cond ((not argument)
	     (let ((point (current-point)))
	       (if (char-match-backward #\Tab point)
		   (convert-tab-to-spaces! (mark-1+ point))))
	     (delete-region (mark-1+ (current-point))))
	    ((positive? argument)
	     (kill-region (back argument)))
	    ((negative? argument)
	     (kill-region (forth (- argument))))))))

(define (convert-tab-to-spaces! m1)
  (let ((at-point? (mark= m1 (current-point)))
	(m2 (mark-left-inserting (mark1+ m1))))
    (define (perform-replacement)
      (let ((n (- (mark-column m2) (mark-column m1))))
	(delete-string m1 m2)
	(insert-string (make-string n #\Space) m2)))
    (if at-point?
	(let ((start (mark-right-inserting m1)))
	  (perform-replacement)
	  (set-current-point! start))
	(perform-replacement))))

;;;; Killing

(define-variable kill-ring-max
  "Maximum length of kill ring before oldest elements are thrown away."
  30
  exact-nonnegative-integer?)

(define-variable kill-ring
  "List of killed text sequences."
  '())

(define-variable kill-ring-yank-pointer
  "The tail of the kill ring whose car is the last thing yanked."
  '())

(define-command kill-region
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)"
  "*m\nd"
  (lambda (mark point)
    (kill-string mark point)))

(define-command copy-region-as-kill
  "Save the region as if killed, but don't kill it."
  "m\nd"
  (lambda (mark point)
    (copy-string mark point)
    (temporary-message "Region saved")))

(define-command append-next-kill
  "Cause following command, if kill, to append to previous kill."
  ()
  (lambda ()
    (set-command-message! append-next-kill-tag)))

(define (kill-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (kill-ring-save (extract-string mark point) (mark<= point mark) point)
    (delete-string mark point)))

(define (copy-string mark #!optional point)
  (let ((point (if (default-object? point) (current-point) point)))
    (kill-ring-save (extract-string mark point) (mark<= point mark) point)))

(define (kill-ring-save string forward? context)
  (command-message-receive append-next-kill-tag
    (lambda ()
      (let ((kill-ring (ref-variable kill-ring)))
	(if (null? kill-ring)
	    (editor-error "No previous kill"))
	(let ((string
	       (if forward?
		   (string-append (car kill-ring) string)
		   (string-append string (car kill-ring)))))
	  (set-car! kill-ring string)
	  (set-variable! kill-ring-yank-pointer kill-ring context)
	  (os/interprogram-cut string context))))
    (lambda ()
      (kill-ring-save-1 string context)
      (os/interprogram-cut string context)))
  (set-command-message! append-next-kill-tag))

(define (kill-ring-save-1 string context)
  (let ((strings
	 (let ((kill-ring (ref-variable kill-ring context))
	       (kill-ring-max (ref-variable kill-ring-max context)))
	   (if (zero? kill-ring-max)
	       '()
	       (let ((strings (cons string kill-ring)))
		 (if (> (length strings) kill-ring-max)
		     (set-cdr! (drop strings (- kill-ring-max 1)) '()))
		 strings)))))
    (set-variable! kill-ring strings context)
    (set-variable! kill-ring-yank-pointer strings context)))

(define append-next-kill-tag
  (list 'APPEND-NEXT-KILL))

;;;; Yanking

(define-command yank
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.
With just \\[universal-argument] as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  "*P"
  (lambda (argument)
    (yank (if (command-argument-multiplier-only? argument)
	      0
	      (- (command-argument-numeric-value argument) 1))
	  (command-argument-multiplier-only? argument)
	  push-current-mark!)))

(define-command yank-pop
  "Replace just-yanked stretch of killed-text with a different stretch.
This command is allowed only immediately after a \\[yank] or a \\[yank-pop].
At such a time, the region contains a stretch of reinserted
previously-killed text.  \\[yank-pop] deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument n, the n'th previous kill is inserted.
If n is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  "*p"
  (lambda (argument)
    (command-message-receive un-kill-tag
      (lambda () unspecific)
      (lambda () (editor-error "Previous command was not a yank")))
    (yank argument
	  (let ((point (current-point))
		(mark (current-mark)))
	    (let ((before? (mark< point mark)))
	      (delete-string point mark)
	      before?))
	  set-current-mark!)))

(define (yank offset before? set-current-mark!)
  (let* ((point (current-point))
	 (start (mark-right-inserting-copy point))
	 (end (mark-left-inserting-copy start)))
    (insert-string (let ((paste
			  (and (= offset 0) (os/interprogram-paste point))))
		     (if (and paste
			      (not (string-null? paste))
			      (let ((kill-ring (ref-variable kill-ring point)))
				(or (null? kill-ring)
				    (not (string=? paste (car kill-ring))))))
			 (begin
			   (kill-ring-save-1 paste point)
			   paste)
			 (begin
			   ((ref-command rotate-yank-pointer) offset)
			   (car (ref-variable kill-ring-yank-pointer point)))))
		   start)
    (mark-temporary! end)
    (mark-temporary! start)
    (if before?
	(begin (set-current-mark! end) (set-current-point! start))
	(begin (set-current-mark! start) (set-current-point! end))))
  (set-command-message! un-kill-tag))

(define un-kill-tag
  "Un-kill")

(define-command rotate-yank-pointer
  "Rotate the yanking point in the kill ring."
  "p"
  (lambda (argument)
    (let ((kill-ring (ref-variable kill-ring)))
      (if (null? kill-ring)
	  (editor-error "Kill ring is empty"))
      (set-variable!
       kill-ring-yank-pointer
       (drop kill-ring
	     (modulo (+ argument
			(let ((kill-ring-yank-pointer
			       (ref-variable kill-ring-yank-pointer)))
			  (let loop ((l kill-ring) (n 0))
			    (cond ((null? l) 0)
				  ((eq? l kill-ring-yank-pointer) n)
				  (else (loop (cdr l) (+ n 1)))))))
		     (length kill-ring)))))))

;;;; Marks

(define-variable mark-ring-maximum
  "The maximum number of marks that are saved on the mark ring.
This variable is only noticed when a buffer is created, so changing
it later will not affect existing buffers."
  16)

(define-command set-mark-command
  "Sets or pops the mark.
With no \\[universal-argument]'s, pushes point as the mark.
With one \\[universal-argument], pops the mark into point.
With two \\[universal-argument]'s, pops the mark and throws it away."
  "P"
  (lambda (argument)
    (case (and (command-argument-multiplier-only? argument)
	       (command-argument-value argument))
      ((4) (set-current-point! (pop-current-mark!)))
      ((16) (pop-current-mark!))
      (else (push-current-mark! (current-point))))))

(define-command mark-beginning-of-buffer
  "Set mark at beginning of buffer."
  ()
  (lambda ()
    (push-current-mark! (buffer-start (current-buffer)))))

(define-command mark-end-of-buffer
  "Set mark at end of buffer."
  ()
  (lambda ()
    (push-current-mark! (buffer-end (current-buffer)))))

(define-command mark-whole-buffer
  "Set point at beginning and mark at end of buffer.
Pushes the old point on the mark first, so two pops restore it.
With argument, puts point at end and mark at beginning."
  "P"
  (lambda (argument)
    (push-current-mark! (current-point))
    ((if (not argument) set-current-region! set-current-region-reversed!)
     (buffer-region (current-buffer)))))

(define-command exchange-point-and-mark
  "Exchange positions of point and mark."
  ()
  (lambda ()
    (let ((point (current-point))
	  (mark (current-mark)))
      (if (not mark) (editor-error "No mark to exchange"))
      (set-current-point! mark)
      (set-current-mark! point))))

;;;; Transposition

(define-command transpose-chars
  "Transpose the characters before and after the cursor.
With a positive argument it transposes the characters before and after
the cursor, moves right, and repeats the specified number of times,
dragging the character to the left of the cursor right.

With a negative argument, it transposes the two characters to the left
of the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.

With a zero argument, it transposes the characters at point and mark.

At the end of a line, with no argument, the preceding two characters
are transposed."
  "p"
  (lambda (argument)
    (cond ((and (= argument 1) (line-end? (current-point)))
	   (twiddle-characters (mark-1+ (current-point) 'ERROR)
			       (current-point)))
	  ((positive? argument)
	   (twiddle-characters (current-point)
			       (mark+ (current-point) argument 'ERROR)))
	  ((negative? argument)
	   (twiddle-characters
	    (current-point)
	    (mark- (current-point) (1+ (- argument)) 'ERROR)))
	  (else
	   (let ((m1 (mark-right-inserting (current-point)))
		 (m2 (mark-right-inserting (current-mark))))
	     (if (not (mark= m1 m2))
		 (begin
		   (let ((c1 (extract-right-char m1))
			 (c2 (extract-right-char m2)))
		     (delete-right-char m1)
		     (delete-right-char m2)
		     (insert-char c2 m1)
		     (insert-char c1 m2))
		   (set-current-point! m1)
		   (set-current-mark! m2))))))))

(define (twiddle-characters m1 m2)
  (let ((m* (mark-left-inserting m2)))
    (let ((char (extract-left-char m1)))
      (delete-left-char m1)
      (insert-char char m*))
    (set-current-point! m*)))