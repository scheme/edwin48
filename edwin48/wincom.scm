#| -*-Scheme-*-

$Id: wincom.scm,v 1.139 2008/01/30 20:02:07 cph Exp $

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

;;;; Window Commands


(define-variable window-min-width
  "Delete any window less than this wide.
Do not set this variable below 2."
  2
  (lambda (object) (and (exact-integer? object) (>= object 2))))

(define-variable window-min-height
  "Delete any window less than this high.
The modeline is not included in this figure.
Do not set this variable below 1."
  1
  (lambda (object) (and (exact-integer? object) (>= object 1))))

(define-variable next-screen-context-lines
  "Number of lines of continuity when scrolling by screenfuls."
  2
  exact-nonnegative-integer?)

(define-variable use-multiple-frames
  "If true, commands try to use multiple frames rather than multiple windows.
Has no effect unless multiple-frame support is available."
  #f
  boolean?)
(define edwin-variable$use-multiple-screens edwin-variable$use-multiple-frames)

(define-variable pop-up-windows
  "True enables the use of pop-up windows."
  #t
  boolean?)

(define-variable preserve-window-arrangement
  "True means commands that normally change the window arrangement do not."
  #f
  boolean?)

(define-variable split-height-threshold
  "Pop-up windows would prefer to split the largest window if this large.
If there is only one window, it is split regardless of this value."
  500
  exact-nonnegative-integer?)

(define-command redraw-display
  "Redraws the entire display from scratch."
  ()
  (lambda () (update-screens! #t)))

(define-command recenter
  "Choose new window putting point at center, top or bottom.
With no argument, chooses a window to put point at the center
\(cursor-centering-point says where).
An argument gives the line to put point on;
negative args count from the bottom."
  "P"
  (lambda (argument)
    (let ((window (selected-window)))
      (if (not argument)
	  (begin
	    (window-scroll-y-absolute! window (window-y-center window))
	    (window-redraw! window)
	    (update-selected-screen! #t))
	  (window-scroll-y-absolute!
	   window
	   (modulo (if (command-argument-multiplier-only? argument)
		       (window-y-center window)
		       (command-argument-value argument))
		   (window-y-size window)))))))

(define-command move-to-window-line
  "Position point relative to window.
With no argument, position at text at center of window.
An argument specifies screen line; zero means top of window,
negative means relative to bottom of window."
  "P"
  (lambda (argument)
    (let ((window (selected-window)))
      (let ((mark
	     (or (window-coordinates->mark
		  window 0
		  (if (not argument)
		      (window-y-center window)
		      (modulo (command-argument-value argument)
			      (window-y-size window))))
		 (window-coordinates->mark
		  window 0
		  (window-mark->y window
				  (buffer-end (window-buffer window)))))))
	(set-current-point! (if (group-start? mark)
				(group-start mark)
				mark))))))

(define-command home-cursor
  "Position point at upper-left corner of window."
  ()
  (lambda ()
    (let ((mark (window-coordinates->mark (selected-window) 0 0)))
      (set-current-point! (if (group-start? mark)
			      (group-start mark)
			      mark)))))

(define-command scroll-up
  "Move down to display next screenful of text.
With argument, moves window down that many lines (negative moves up).
Just minus as an argument moves up a full screen."
  "P"
  (lambda (argument)
    (let ((window (selected-window)))
      (scroll-window window
		     (standard-scroll-window-argument window argument 1)))))

(define-command scroll-down
  "Move up to display previous screenful of text.
With argument, moves window up that many lines (negative moves down).
Just minus as an argument moves down a full screen."
  "P"
  (lambda (argument)
    (let ((window (selected-window)))
      (scroll-window window
		     (standard-scroll-window-argument window argument -1)))))

(define-command scroll-up-several-screens
  "Move down to display next screenful of text.
With argument, move window down that many screenfuls (negative moves up).
Just minus as an argument moves up a full screen."
  "P"
  (lambda (argument)
    (let ((window (selected-window)))
      (scroll-window window
		     (multi-scroll-window-argument window argument 1)))))

(define-command scroll-down-several-screens
  "Move up to display previous screenful of text.
With argument, move window down that many screenfuls (negative moves down).
Just minus as an argument moves down full screen."
  "P"
  (lambda (argument)
    (let ((window (selected-window)))
      (scroll-window window
		     (multi-scroll-window-argument window argument -1)))))

(define-command scroll-other-window
  "Scroll text of next window up ARG lines, or near full screen if no arg."
  "P"
  (lambda (argument)
    (let ((window
	   (or (and (typein-window? (selected-window))
		    (weak-car *minibuffer-scroll-window*))
	       (other-window-interactive 1))))
      (scroll-window window
		     (standard-scroll-window-argument window argument 1)))))

(define-command scroll-other-window-down
  "Scroll text of next window down ARG lines, or near full screen if no arg."
  "P"
  (lambda (argument)
    (let ((window
	   (or (and (typein-window? (selected-window))
		    (weak-car *minibuffer-scroll-window*))
	       (other-window-interactive 1))))
      (scroll-window window
		     (standard-scroll-window-argument window argument -1)))))

(define-command scroll-other-window-several-screens
  "Scroll other window up several screenfuls.
Specify the number as a numeric argument, negative for down.
The default is one screenful up.  Just minus as an argument
means scroll one screenful down."
  "P"
  (lambda (argument)
    (let ((window
	   (or (and (typein-window? (selected-window))
		    (weak-car *minibuffer-scroll-window*))
	       (other-window-interactive 1))))
      (scroll-window window
		     (multi-scroll-window-argument window argument 1)))))

(define* (scroll-window window n (limit editor-error))
  (if (window-mark-visible?
       window
       ((if (negative? n) buffer-start buffer-end) (window-buffer window)))
      (limit)
      (window-scroll-y-relative! window n)))

(define (standard-scroll-window-argument window argument factor)
  (* factor
     (let ((quantum
	    (- (window-y-size window)
	       (ref-variable next-screen-context-lines))))
       (cond ((not argument) quantum)
	     ((command-argument-negative-only? argument) (- quantum))
	     (else (command-argument-value argument))))))

(define (multi-scroll-window-argument window argument factor)
  (* factor
     (let ((quantum
	    (- (window-y-size window)
	       (ref-variable next-screen-context-lines))))
       (cond ((not argument) quantum)
	     ((command-argument-negative-only? argument) (- quantum))
	     (else (* (command-argument-value argument) quantum))))))

(define-command what-cursor-position
  "Print info on cursor position (on screen and within buffer)."
  ()
  (lambda ()
    (let ((buffer (selected-buffer))
	  (point (current-point)))
      (let ((position (mark-index point))
	    (total (group-length (buffer-group buffer))))
	(message (if (group-end? point)
		     ""
		     (let ((char (mark-right-char point)))
		       (let ((n (char->ascii char)))
			 (string-append "Char: " (key-name char)
					" ("
					(if (zero? n) "" "0")
					(number->string n 8)
					") "))))
		 "point=" (+ position 1)
		 " of " total
		 "("
		 (if (zero? total)
		     0
		     (integer-round (* 100 position) total))
		 "%) "
		 (let ((group (mark-group point)))
		   (let ((start (group-start-index group))
			 (end (group-end-index group)))
		     (if (and (zero? start) (= end total))
			 ""
			 (string-append "<" (number->string start)
					" - " (number->string end)
					"> "))))
		 "x=" (mark-column point))))))

;;;; Multiple Windows

(define-command split-window-vertically
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally."
  "P"
  (lambda (argument)
    (disallow-typein)
    (window-split-vertically! (selected-window)
			      (command-argument-value argument))))

(define-command split-window-horizontally
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets
ARG lines.  No arg means split equally."
  "P"
  (lambda (argument)
    (disallow-typein)
    (window-split-horizontally! (selected-window)
				(command-argument-value argument))))

(define-command enlarge-window
  "Makes current window ARG lines bigger."
  "p"
  (lambda (argument)
    (disallow-typein)
    (window-grow-vertically! (selected-window) argument)))

(define-command shrink-window
  "Makes current window ARG lines smaller."
  "p"
  (lambda (argument)
    (disallow-typein)
    (window-grow-vertically! (selected-window) (- argument))))

(define-command shrink-window-if-larger-than-buffer
  "Shrink the WINDOW to be as small as possible to display its contents.
Do nothing if the buffer contains more lines than the present window height,
or if some of the window's contents are scrolled out of view,
or if the window is the only window of its frame."
  ()
  (lambda () (shrink-window-if-larger-than-buffer (selected-window))))

(define-command enlarge-window-horizontally
  "Makes current window ARG columns wider."
  "p"
  (lambda (argument)
    (disallow-typein)
    (window-grow-horizontally! (selected-window) argument)))

(define-command shrink-window-horizontally
  "Makes current window ARG columns narrower."
  "p"
  (lambda (argument)
    (disallow-typein)
    (window-grow-horizontally! (selected-window) (- argument))))

(define-command delete-window
  "Delete the current window from the screen."
  ()
  (lambda ()
    (let ((window (selected-window)))
      (if (and (window-has-no-neighbors? window)
	       (use-multiple-screens?)
	       (other-screen (selected-screen) 1 #t))
	  (delete-screen! (selected-screen))
	  (window-delete! window)))))

(define-command delete-other-windows
  "Make the current window fill the screen."
  ()
  (lambda () (delete-other-windows (selected-window))))

(define-command kill-buffer-and-window
  "Kill the current buffer and delete the window it was in."
  ()
  (lambda ()
    (kill-buffer-interactive (current-buffer))
    (window-delete! (selected-window))))

(define-command other-window
  "Select the ARG'th different window."
  "p"
  (lambda (argument) (select-window (other-window-interactive argument))))

(define (other-window-interactive n)
  (let ((window
	 (let ((window (other-window n)))
	   (if (selected-window? window)
	       (and (use-multiple-screens?)
		    (let ((screen (other-screen (selected-screen))))
		      (and screen
			   (screen-selected-window screen))))
	       window))))
    (if (not window)
	(editor-error "No other window"))
    window))

(define (disallow-typein)
  (if (typein-window? (selected-window))
      (editor-error "Not implemented for typein window")))

(define (use-multiple-screens?)
  (and (ref-variable use-multiple-frames)
       (multiple-screens?)))

(define (select-buffer-other-window buffer)
  (let ((window (selected-window))
	(use-window
	 (lambda (window)
	   (select-buffer buffer window)
	   (select-window window))))
    (let loop ((windows (buffer-windows buffer)))
      (cond ((null? windows)
	     (let ((window* (next-visible-window window #f)))
	       (cond (window*
		      (use-window window*))
		     ((use-multiple-screens?)
		      (select-buffer-other-screen buffer))
		     (else
		      (use-window (window-split-vertically! window #f))))))
	    ((and (not (eq? (car windows) window))
		  (window-visible? (car windows)))
	     (select-window (car windows)))
	    (else
	     (loop (cdr windows)))))))

(define* (select-buffer-other-screen buffer (screen #f))
  (if (multiple-screens?)
      (let ((screen
	     (other-screen (if (not screen)
			       (selected-screen)
			       screen))))
	(if screen
	    (let ((window (screen-selected-window screen)))
	      (select-window window)
	      (select-buffer buffer window))
	    (make-screen buffer)))
      (editor-error "Display doesn't support multiple screens")))

(define (shrink-window-if-larger-than-buffer window)
  (if (not (window-has-no-neighbors? window))
      (let ((buffer (window-buffer window)))
	(let ((current-height (window-y-size window))
	      (min-height
	       (+ (- (window-mark->y window (buffer-end buffer))
		     (window-mark->y window (buffer-start buffer)))
		  1))
	      (max-height
	       (and (eq? window (weak-car *previous-popped-up-window*))
		    (weak-cdr *previous-popped-up-window*))))
	  (cond ((< 0 min-height current-height)
		 (adjust-window-height! window min-height))
		((and max-height
		      (> min-height current-height)
		      (< current-height max-height))
		 (adjust-window-height! window
					(min min-height max-height))))))))

(define (adjust-window-height! window new-height)
  (with-variable-value! (ref-variable-object window-min-height)
    1
    (lambda ()
      (window-grow-vertically! window
			       (- new-height (window-y-size window))))))

;;;; Pop-up Buffers

(define-command kill-pop-up-buffer
  "Kills the most recently popped up buffer, if one exists.
Also kills any pop up window it may have created."
  ()
  (lambda () (kill-pop-up-buffer #t)))

(define (cleanup-pop-up-buffers thunk)
  (fluid-let ((*previous-popped-up-window* (weak-cons #f #f))
	      (*previous-popped-up-buffer* (weak-cons #f #f))
	      (*minibuffer-scroll-window* (weak-cons #f #f))
	      (*pop-up-buffer-window-alist*
	       (map (lambda (window)
		      (weak-cons window (window-buffer window)))
		    (window-list))))
    (dynamic-wind
     (lambda () unspecific)
     thunk
     (lambda () (kill-pop-up-buffer #f)))))

(define (kill-pop-up-buffer error-if-none?)
  (let ((window (weak-car *previous-popped-up-window*)))
    (if window
	(begin
	  (weak-set-car! *previous-popped-up-window* #f)
	  (if (and (window-live? window)
		   (not (window-has-no-neighbors? window)))
	      (window-delete! window)))))
  (let ((buffer (weak-car *previous-popped-up-buffer*)))
    (cond ((and buffer (buffer-alive? buffer))
	   (for-each
	    (lambda (window)
	      (let ((entry (weak-assq window *pop-up-buffer-window-alist*)))
		(if entry
		    (select-buffer-no-record (weak-cdr entry) window))))
	    (buffer-windows buffer))
	   (weak-set-car! *previous-popped-up-buffer* #f)
	   (kill-buffer-interactive buffer))
	  (error-if-none?
	   (editor-error "No previous pop up buffer.")))))

(define (maybe-kill-pop-up-buffer buffer)
  (if (and buffer (eq? buffer (popped-up-buffer)))
      (kill-pop-up-buffer #f)))

(define (popped-up-buffer)
  (weak-car *previous-popped-up-buffer*))

(define (keep-pop-up-buffer buffer)
  (if (or (not buffer)
	  (eq? buffer (weak-car *previous-popped-up-buffer*)))
      (begin
	(weak-set-car! *previous-popped-up-window* #f)
	(weak-set-car! *previous-popped-up-buffer* #f))))

(define (pop-up-buffer-option options name default)
  (let loop ((options options))
    (if (pair? options)
	(let ((option (car options)))
	  (cond ((eq? name option)
		 #t)
		((and (pair? option)
		      (eq? name (car option))
		      (pair? (cdr option))
		      (null? (cddr option)))
		 (cadr option))
		(else
		 (loop (cdr options)))))
	default)))

(define *previous-popped-up-window* (weak-cons #f #f))
(define *previous-popped-up-buffer* (weak-cons #f #f))
(define *minibuffer-scroll-window* (weak-cons #f #f))
(define *pop-up-buffer-window-alist* '())

(define* (pop-up-buffer buffer (select? #f) (options '()))
  ;; If some new window is created by this procedure, it is returned
  ;; as the value.  Otherwise the value is false.
  (let* ((screen (pop-up-buffer-option options 'SCREEN (selected-screen)))
	 (selected (screen-selected-window screen)))

    (define (pop-up-window window)
      (let ((window
	     (window-split-vertically!
	      window
	      (pop-up-buffer-option options 'HEIGHT #f))))
	(weak-set-car! *previous-popped-up-window* window)
	(weak-set-cdr! *previous-popped-up-window* (window-y-size window))
	(pop-into-window window)
	window))

    (define (pop-into-window window)
      (select-buffer buffer window)
      (maybe-record-window window))

    (define (maybe-record-window window)
      (weak-set-car! *minibuffer-scroll-window* window)
      (if select? (select-window window))
      (and (eq? window (weak-car *previous-popped-up-window*))
	   window))

    (define (find-visible-window buffer)
      (let loop ((windows (buffer-windows buffer)))
	(and (pair? windows)
	     (let ((window (car windows)))
	       (if (and (window-visible? window)
			(eq? (window-screen window) screen)
			(not (and (pop-up-buffer-option options
							'NOT-CURRENT-WINDOW
							#f)
				  (eq? window selected))))
		   window
		   (loop (cdr windows)))))))

    (if (< (ref-variable window-min-height) 2)
	(set-variable! window-min-height 2))
    (weak-set-car! *previous-popped-up-buffer* buffer)
    (let ((window (find-visible-window buffer)))
      (if window
	  (begin
	    (set-window-point! window (buffer-point buffer))
	    (maybe-record-window window))
	  (let ((limit (* 2 (ref-variable window-min-height))))
	    (if (< (ref-variable split-height-threshold) limit)
		(set-variable! split-height-threshold limit))
	    (cond ((and (use-multiple-screens?)
			(other-screen screen))
		   => (lambda (screen)
			(pop-into-window (screen-selected-window screen))))
		  ((ref-variable preserve-window-arrangement)
		   (pop-into-window (largest-window screen)))
		  ((not (ref-variable pop-up-windows))
		   (pop-into-window (lru-window screen)))
		  ((use-multiple-screens?)
		   (maybe-record-window
		    (screen-selected-window (make-screen buffer))))
		  (else
		   (let ((window (largest-window screen)))
		     (if (and (>= (window-y-size window)
				  (ref-variable split-height-threshold))
			      (not (window-has-horizontal-neighbor? window)))
			 (pop-up-window window)
			 (let ((window (lru-window screen)))
			   (if (and (or (eq? window selected)
					(and (typein-window? selected)
					     (eq? window (window1+ window))))
				    (>= (window-y-size window) limit))
			       (pop-up-window window)
			       (pop-into-window window))))))))))))

(define (get-buffer-window buffer)
  (let loop ((windows (buffer-windows buffer)))
    (and (not (null? windows))
	 (if (window-visible? (car windows))
	     (car windows)
	     (loop (cdr windows))))))

(define (largest-window screen)
  (let ((start (screen-window0 screen)))
    (let loop
	((window (window1+ start))
	 (largest start)
	 (largest-area (* (window-x-size start) (window-y-size start))))
      (if (eq? window start)
	  largest
	  (let ((area (* (window-x-size window) (window-y-size window))))
	    (if (> area largest-area)
		(loop (window1+ window) window area)
		(loop (window1+ window) largest largest-area)))))))

(define (lru-window screen)
  (let ((start (screen-window0 screen)))
    (define (search-full-width window smallest smallest-time)
      (let ((next (window1+ window))
	    (time (window-select-time window)))
	(let ((consider-window?
	       (and (not (window-has-horizontal-neighbor? window))
		    (or (not smallest)
			(< time smallest-time)))))
	  (if (eq? window start)
	      (if consider-window?
		  window
		  (or smallest
		      (search-all next
				  start
				  (window-select-time start))))
	      (if consider-window?
		  (search-full-width next window time)
		  (search-full-width next smallest smallest-time))))))

    (define (search-all window smallest smallest-time)
      (if (eq? window start)
	  smallest
	  (let ((time (window-select-time window)))
	    (if (< time smallest-time)
		(search-all (window1+ window) window time)
		(search-all (window1+ window) smallest smallest-time)))))

    (search-full-width (window1+ start) #f #f)))

(define (delete-other-windows start)
  (let loop ((window (window1+ start)))
    (if (not (eq? window start))
	(begin
	  (window-delete! window)
	  (loop (window1+ window))))))

(define-command toggle-screen-width
  "Restrict the editor's width on the screen.
With no argument, restricts the width to 80 columns,
 unless it is already restricted, in which case it undoes the restriction.
With \\[universal-argument] only, undoes all restrictions.
Otherwise, the argument is the number of columns desired."
  "P"
  (lambda (argument)
    (let ((screen (selected-screen)))
      (let ((window (screen-root-window screen)))
	(send window ':set-size!
	      (let ((x-size (screen-x-size screen)))
		(cond ((command-argument-multiplier-only? argument)
		       x-size)
		      ((not argument)
		       (let ((x-size* (window-x-size window)))
			 (if (< x-size* x-size)
			     x-size
			     (min 80 x-size))))
		      (else
		       (let ((argument (command-argument-value argument)))
			 (if (< argument 10)
			     (editor-error "restriction too small: " argument))
			 (min x-size argument)))))
	      (screen-y-size screen)))
      (update-screen! screen #t))))

(define-command compare-windows
  "Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match."
  ()
  (lambda ()
    (let ((w1 (selected-window))
	  (w2 (other-window-interactive 1)))
      (let ((p1 (window-point w1)))
	(let loop ((s1 p1) (s2 (window-point w2)) (length 1024))
	  (if (> length 0)
	      (let ((e1 (mark+ s1 length #f))
		    (e2 (mark+ s2 length #f)))
		(if (and e1
			 e2
			 (if (= length 1)
			     (char=? (extract-right-char s1)
				     (extract-right-char s2))
			     (string=? (extract-string s1 e1)
				       (extract-string s2 e2))))
		    (loop e1 e2 length)
		    (loop s1 s2 (quotient length 2))))
	      (begin
		(set-window-point! w1 s1)
		(set-window-point! w2 s2)
		(if (mark= s1 p1)
		    (editor-beep)))))))))