#| -*-Scheme-*-

$Id: bufmnu.scm,v 1.136 2007/01/05 21:19:23 cph Exp $

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

;;;; Buffer Menu


(define-variable buffer-menu-kill-on-quit
  "If not false, kill the *Buffer-List* buffer when leaving it."
  #f
  boolean?)

(define-command list-buffers
  "Display a list of names of existing buffers.
Inserts it in buffer *Buffer-List* and displays that.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY? means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  "P"
  (lambda (files-only?)
    (pop-up-buffer (update-buffer-list files-only?) #f)))

(define-command buffer-menu
  "Make a menu of buffers so you can save, delete or select them.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q immediately to make the buffer menu go away."
  "P"
  (lambda (files-only?)
    (pop-up-buffer (update-buffer-list files-only?) #t)
    (message "Commands: d, s, x; 1, 2, m, u, q; rubout; ? for help.")))

(define (update-buffer-list files-only?)
  (let ((buffer (temporary-buffer "*Buffer-List*")))
    (set-buffer-major-mode! buffer (ref-mode-object buffer-menu))
    (buffer-put! buffer 'REVERT-BUFFER-FILES-ONLY? files-only?)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD revert-buffer-menu)
    (fill-buffer-menu! buffer files-only?)
    buffer))

(define (revert-buffer-menu buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save? dont-confirm?	;ignore
  (set-buffer-writeable! buffer)
  (region-delete! (buffer-region buffer))
  (fill-buffer-menu! buffer (buffer-get buffer 'REVERT-BUFFER-FILES-ONLY?))
  buffer)

(define (fill-buffer-menu! buffer files-only?)
  (let ((buffers (buffer-list))
	(hide-buffer?
	 (lambda (buffer)
	   (or (minibuffer? buffer)
	       (and files-only?
		    (not (buffer-pathname buffer)))))))
    (let ((wn 8)
	  (ws 5)
	  (wm 8))
      (for-each
       (lambda (buffer)
	 (if (not (hide-buffer? buffer))
	     (begin
	       (let ((w (string-length (buffer-name buffer))))
		 (if (> w wn)
		     (set! wn w)))
	       (let ((w
		      (string-length
		       (number->string
			(group-length (buffer-group buffer))))))
		 (if (> w ws)
		     (set! ws w)))
	       (let ((w
		      (string-length
		       (mode-display-name (buffer-major-mode buffer)))))
		 (if (> w wm)
		     (set! wm w))))))
       buffers)
      (call-with-output-mark (buffer-point buffer)
	(lambda (port)
	  (let ((write-line
		 (lambda (k m r buffer size mode file)
		   (write-string k port)
		   (write-string m port)
		   (write-string r port)
		   (write-string " " port)
		   (write-string (string-pad-right buffer wn) port)
		   (write-string "  " port)
		   (write-string (string-pad-left size ws) port)
		   (write-string " " port)
		   (write-string (string-pad-right mode wm) port)
		   (write-string "  " port)
		   (write-string file port)
		   (newline port))))
	    (write-line " " "M" "R" "Buffer" "Size" "Mode" "File")
	    (write-line " " "-" "-" "------" "----" "----" "----")
	    (let ((current (current-buffer)))
	      (for-each
	       (lambda (buffer)
		 (if (not (hide-buffer? buffer))
		     (write-line
		      (if (eq? buffer current) "." " ")
		      (if (buffer-modified? buffer) "*" " ")
		      (if (buffer-writeable? buffer) " " "%")
		      (buffer-name buffer)
		      (number->string (group-length (buffer-group buffer)))
		      (mode-display-name (buffer-major-mode buffer))
		      (let ((truename (buffer-truename buffer)))
			(if truename
			    (->namestring truename)
			    "")))))
	       buffers)))))))
  (set-buffer-point! buffer (line-start (buffer-start buffer) 2))
  (set-buffer-read-only! buffer))

(define-major-mode buffer-menu read-only-noarg "Buffer Menu"
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
m -- mark buffer to be displayed.
q -- select buffer of line point is on.
  Also show buffers marked with m in other windows.
1 -- select that buffer in full-screen window.
2 -- select that buffer in one window,
  together with buffer selected before this one in another window.
f -- select that buffer in place of the buffer menu buffer.
o -- select that buffer in another window,
  so the buffer menu buffer remains visible in its window.
~ -- clear modified-flag on that buffer.
s -- mark that buffer to be saved, and move down.
d or k -- mark that buffer to be deleted, and move down.
C-d -- mark that buffer to be deleted, and move up.
x -- delete or save marked buffers.
u -- remove all kinds of marks from current line.
Delete -- back up a line and remove marks."
  (lambda (buffer)
    (local-set-variable! truncate-lines #t buffer)
    (event-distributor/invoke! (ref-variable buffer-menu-mode-hook buffer)
			       buffer)))

(define-variable buffer-menu-mode-hook
  "An event distributor that is invoked when entering Buffer-menu mode."
  (make-event-distributor))

(define-key 'buffer-menu #\m 'buffer-menu-mark)
(define-key 'buffer-menu #\q 'buffer-menu-quit)
(define-key 'buffer-menu #\1 'buffer-menu-1-window)
(define-key 'buffer-menu #\2 'buffer-menu-2-window)
(define-key 'buffer-menu #\f 'buffer-menu-this-window)
(define-key 'buffer-menu #\o 'buffer-menu-other-window)
(define-key 'buffer-menu #\~ 'buffer-menu-not-modified)
(define-key 'buffer-menu #\s 'buffer-menu-save)
(define-key 'buffer-menu #\d 'buffer-menu-delete)
(define-key 'buffer-menu #\k 'buffer-menu-delete)
(define-key 'buffer-menu #\c-d 'buffer-menu-delete-backwards)
(define-key 'buffer-menu #\c-k 'buffer-menu-delete)
(define-key 'buffer-menu #\x 'buffer-menu-execute)
(define-key 'buffer-menu #\u 'buffer-menu-unmark)
(define-key 'buffer-menu #\rubout 'buffer-menu-backup-unmark)
(define-key 'buffer-menu #\space 'buffer-menu-next-line)
(define-key 'buffer-menu #\c-\] 'buffer-menu-abort)
(define-key 'buffer-menu #\? 'describe-mode)

(define-command buffer-menu-mark
  "Mark buffer on this line for being displayed by \\[buffer-menu-quit] command."
  "p"
  (lambda (argument)
    (set-multiple-marks! 0 #\> argument)))

(define-command buffer-menu-quit
  "Select this line's buffer; also display buffers marked with >.
You can mark buffers with the \\[buffer-menu-mark] command."
  ()
  (lambda ()
    (let ((lstart (current-lstart))
	  (window (current-window)))
      (let ((menu (window-buffer window))
	    (buffer (buffer-menu-buffer lstart))
	    (others (map buffer-menu-buffer (find-buffers-marked 0 #\>))))
	(if (and (ref-variable preserve-window-arrangement)
		 (null? others))
	    (buffer-menu-select menu buffer #f)
	    (begin
	      (delete-other-windows window)
	      (buffer-menu-select menu buffer (memq menu others))
	      (let ((height (max (quotient (1+ (window-y-size window))
					   (1+ (length others)))
				 (1+ (ref-variable window-min-height)))))
		(define (loop window buffers)
		  (let ((new (window-split-vertically! window height)))
		    (if new
			(begin
			  (select-buffer (car buffers) new)
			  (loop new (cdr buffers))))))
		(loop window others))))))
    (clear-message)))

(define-command buffer-menu-1-window
  "Select this line's buffer, alone, in full screen."
  ()
  (lambda ()
    (let ((window (current-window)))
      (delete-other-windows window)
      (buffer-menu-select (window-buffer window)
			  (buffer-menu-buffer (current-lstart))
			  #f))
    (clear-message)))

(define-command buffer-menu-2-window
  "Select this line's buffer, with previous buffer in second window."
  ()
  (lambda ()
    (buffer-menu-select (window-buffer (current-window))
			(buffer-menu-buffer (current-lstart))
			#f)
    (with-variable-value! (ref-variable-object pop-up-windows) #t
      (lambda ()
	(pop-up-buffer (previous-buffer) #f)))
    (clear-message)))

(define-command buffer-menu-this-window
  "Select this line's buffer."
  ()
  (lambda ()
    (buffer-menu-find select-buffer)))

(define-command buffer-menu-other-window
  "Select this line's buffer in another window."
  ()
  (lambda ()
    (buffer-menu-find select-buffer-other-window)))

(define (buffer-menu-find select-buffer)
  (let ((buffer (buffer-menu-buffer (current-lstart))))
    (if (not (eq? (current-buffer) buffer))
	(select-buffer buffer)))
  (clear-message))

(define-command buffer-menu-not-modified
  "Mark buffer on this line as unmodified (no changes to save)."
  ()
  (lambda ()
    (buffer-not-modified! (buffer-menu-buffer (current-lstart)))
    (let ((lstart (current-lstart)))
      (if (char=? #\* (buffer-menu-mark lstart 1))
	  (set-buffer-menu-mark! lstart 1 #\Space)))))

(define-command buffer-menu-save
  "Mark buffer on this line to be saved by \\[buffer-menu-execute] command."
  "p"
  (lambda (argument)
    (set-multiple-marks! 1 #\S argument)))

(define-command buffer-menu-delete
  "Mark buffer on this line to be killed by \\[buffer-menu-execute] command."
  "p"
  (lambda (argument)
    (set-multiple-marks! 0 #\D argument)))

(define-command buffer-menu-delete-backwards
  "Mark buffer on this line to be killed by \\[buffer-menu-execute] command
and then move up one line."
  "p"
  (lambda (argument)
    (do ((i 0 (+ i 1)))
	((>= i argument))
      (set-buffer-menu-mark! (current-lstart) 0 #\D)
      (set-current-point! (previous-lstart)))))

(define-command buffer-menu-execute
  "Save and/or Kill buffers marked with \\[buffer-menu-save] or \\[buffer-menu-delete]."
  ()
  (lambda ()
    (buffer-menu-save-and-kill!)))

(define-command buffer-menu-unmark
  "Remove all marks from this line."
  ()
  (lambda ()
    (let ((lstart (mark-right-inserting (current-lstart))))
      (let ((buffer (buffer-menu-buffer lstart)))
	(set-buffer-menu-mark! lstart 0 #\Space)
	(set-buffer-menu-mark! lstart 1
			       (if (buffer-modified? buffer) #\* #\Space))))
    (set-current-point! (next-lstart))))

(define-command buffer-menu-backup-unmark
  "Remove all marks from the previous line."
  ()
  (lambda ()
    (set-current-point! (previous-lstart))
    ((ref-command buffer-menu-unmark))
    (set-current-point! (previous-lstart))))

(define-command buffer-menu-next-line
  "Move down to the next line."
  "p"
  (lambda (argument)
    (set-current-point! (line-start (current-point) argument 'BEEP))))

(define-command buffer-menu-abort
  "Abort buffer menu edit."
  ()
  (lambda ()
    (kill-buffer-interactive (current-buffer))
    (clear-message)))

(define (buffer-menu-select menu buffer needed?)
  (select-buffer buffer)
  (if (not (or (eq? menu buffer) needed?))
      (if (ref-variable buffer-menu-kill-on-quit)
	  (kill-buffer-interactive menu)
	  (bury-buffer menu))))

(define (buffer-menu-save-and-kill!)
  (for-each buffer-menu-save! (find-buffers-marked 1 #\S))
  (for-each buffer-menu-kill! (find-buffers-marked 0 #\D)))

(define (buffer-menu-save! lstart)
  (save-buffer (buffer-menu-buffer lstart) #f)
  (set-buffer-menu-mark! lstart 1 #\space))

(define (buffer-menu-kill! lstart)
  (define (erase-line)
    (with-read-only-defeated lstart
      (lambda ()
	(delete-string lstart (line-start lstart 1)))))
  (let ((buffer (find-buffer (buffer-menu-buffer-name lstart))))
    (cond ((not buffer) (erase-line))
	  ((not (eq? buffer (current-buffer)))
	   (kill-buffer-interactive buffer)
	   (erase-line)))))

(define (buffer-menu-buffer lstart)
  (let ((name (buffer-menu-buffer-name lstart)))
    (or (find-buffer name)
	(editor-error "No buffer named '" name "'"))))

(define (buffer-menu-buffer-name lstart)
  (guarantee-buffer-line lstart)
  (buffer-line-name lstart))

(define (current-lstart)
  (line-start (current-point) 0))

(define (next-lstart)
  (line-start (current-point) 1 'ERROR))

(define (previous-lstart)
  (line-start (current-point) -1 'ERROR))

(define (set-multiple-marks! column char n)
  (dotimes n
    (lambda (i)
      i					;ignore
      (set-buffer-menu-mark! (current-lstart) column char)
      (set-current-point! (next-lstart)))))

(define (guarantee-buffer-line lstart)
  (if (not (buffer-line? lstart))
      (editor-error "No buffer on this line")))

(define (buffer-line? lstart)
  (and (mark>= lstart (line-start (group-start lstart) 2))
       (not (mark= lstart (line-end lstart 0)))))

(define (buffer-line-name lstart)
  (let ((start (mark+ lstart 4)))
    (extract-string
     start
     (skip-chars-backward
      " \t"
      (let ((end (line-end start 0)))
	(or (re-search-forward "\t\\|  " start end)
	    end))))))

(define (buffer-menu-mark lstart column)
  (guarantee-buffer-line lstart)
  (mark-right-char (mark+ lstart column)))

(define (set-buffer-menu-mark! lstart column char)
  (guarantee-buffer-line lstart)
  (let ((m (mark+ lstart column)))
    (with-read-only-defeated m
      (lambda ()
	(delete-right-char m)
	(region-insert-char! m char)))))

(define (list-buffers-format k m r buffer size mode file)
  (let* ((result
	  (string-append k m r " " (pad-on-right-to buffer 13) "  " size))
	 (result (string-append (pad-on-right-to result 24) " " mode)))
    (string-append (pad-on-right-to result 39) " " file)))

(define list-buffers-header
  (string-append
   (list-buffers-format " " "M" "R" "Buffer" "Size" "Mode" "File")
   "\n"
   (list-buffers-format " " "-" "-" "------" "----" "----" "----")
   "\n"))

(define (find-buffers-marked column char)
  (define (loop lstart)
    (let ((next (line-start lstart 1)))
      (cond ((not next) '())
	    ((char=? (mark-right-char (mark+ lstart column)) char)
	     (cons (mark-permanent! lstart) (loop next)))
	    (else (loop next)))))
  (loop (line-start (buffer-start (current-buffer)) 2)))