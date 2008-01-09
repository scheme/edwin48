#| -*-Scheme-*-

$Id: buffrm.scm,v 1.65 2007/01/05 21:19:23 cph Exp $

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

;;;; Buffer Frames

(declare (usual-integrations))

(define-class buffer-frame combination-leaf-window
  (
   ;; The inferior (of type BUFFER-WINDOW) that displays the buffer's
   ;; text.
   text-inferior

   ;; The inferior (of type MODELINE-WINDOW) that displays the
   ;; modeline.  May be #F if this window has no modeline (e.g. a
   ;; typein window).
   modeline-inferior

   ;; The inferior (of type VERTICAL-BORDER-WINDOW) that draws a
   ;; vertical border on the right-hand side of the window when this
   ;; window has a neighbor to its right.
   border-inferior

   ;; A nonnegative integer that is updated when this window is
   ;; selected.  This updating is performed by the editor frame that
   ;; this window is a part of.
   last-select-time
   ))

(define-method buffer-frame (:make-leaf frame)
  (let ((frame* (==> superior :make-inferior buffer-frame)))
    (set-buffer-frame-size! frame* (window-x-size frame) (window-y-size frame))
    (set-window-buffer! frame* (window-buffer frame))
    (initial-modeline! frame* modeline-inferior)
    frame*))

(define-method buffer-frame (:initialize! frame window*)
  (usual==> frame :initialize! window*)
  (set! text-inferior (make-inferior frame buffer-window))
  (set! border-inferior (make-inferior frame vertical-border-window))
  (set! last-select-time 0))

(define-method buffer-frame (:kill! window)
  (remove-buffer-window! (window-buffer window) window)
  (usual==> window :kill!))

(define-method buffer-frame (:update-display! window screen x-start y-start
					      xl xu yl yu display-style)
  ;; Assumes that interrupts are disabled.
  (update-inferior! text-inferior screen x-start y-start
		    xl xu yl yu display-style
		    buffer-window:update-display!)
  (if modeline-inferior
      (update-inferior! modeline-inferior screen x-start y-start
			xl xu yl yu display-style
			modeline-window:update-display!))
  (update-inferior! border-inferior screen x-start y-start
		    xl xu yl yu display-style
		    vertical-border-window:update-display!)
  #t)

(define (initial-modeline! frame modeline?)
  ;; **** Kludge: The text-inferior will generate modeline events, so
  ;; if the modeline gets redisplayed first it will be left with its
  ;; redisplay-flag set but its superior's redisplay-flag cleared.
  (with-instance-variables buffer-frame frame (modeline?)
    (if modeline?
	(begin
	  (set! modeline-inferior (make-inferior frame modeline-window))
	  (set! inferiors
		(append! (delq! modeline-inferior inferiors)
			 (list modeline-inferior))))
	(set! modeline-inferior #f))))

(define-integrable (frame-text-inferior frame)
  (with-instance-variables buffer-frame frame ()
    (inferior-window text-inferior)))

(define-integrable (frame-modeline-inferior frame)
  (with-instance-variables buffer-frame frame ()
    (and modeline-inferior
	 (inferior-window modeline-inferior))))

(define-method buffer-frame (:set-size! window x y)
  (set-buffer-frame-size! window x y))

(define-method buffer-frame (:set-x-size! window x)
  (set-buffer-frame-size! window x y-size))

(define-method buffer-frame (:set-y-size! window y)
  (set-buffer-frame-size! window x-size y))

(define (set-buffer-frame-size! window x y)
  (with-instance-variables buffer-frame window (x y)
    (usual==> window :set-size! x y)
    (if modeline-inferior
	(begin
	  (set! y (- y (inferior-y-size modeline-inferior)))
	  (set-inferior-start! modeline-inferior 0 y)
	  (set-inferior-x-size! modeline-inferior x)))
    (if (window-has-right-neighbor? window)
	(begin
	  (set! x (- x (inferior-x-size border-inferior)))
	  (set-inferior-start! border-inferior x 0)
	  (set-inferior-y-size! border-inferior y))
	(set-inferior-start! border-inferior #f #f))
    (set-inferior-start! text-inferior 0 0)
    (set-inferior-size! text-inferior x y)))

(define-method buffer-frame (:minimum-x-size window)
  (if (window-has-right-neighbor? window)
      (+ (ref-variable window-min-width)
	 (inferior-x-size border-inferior))
      (ref-variable window-min-width)))

(define-method buffer-frame (:minimum-y-size window)
  (if modeline-inferior
      (+ (ref-variable window-min-height)
	 (inferior-y-size modeline-inferior))
      (ref-variable window-min-height)))

;;;; External Entries

(define-integrable (buffer-frame? object)
  (object-of-class? buffer-frame object))

(define (make-buffer-frame superior new-buffer modeline?)
  (let ((frame (==> superior :make-inferior buffer-frame)))
    (set-window-buffer! frame new-buffer)
    (initial-modeline! frame modeline?)
    frame))

(define-integrable (buffer-frame-x-size frame)
  (window-x-size (frame-text-inferior frame)))

(define-integrable (buffer-frame-y-size frame)
  (window-y-size (frame-text-inferior frame)))

(define-integrable (buffer-frame-needs-redisplay? frame)
  (buffer-window/needs-redisplay? (frame-text-inferior frame)))

(define-integrable (window-cursor-enable! frame)
  (buffer-window/cursor-enable! (frame-text-inferior frame)))

(define-integrable (window-cursor-disable! frame)
  (buffer-window/cursor-disable! (frame-text-inferior frame)))

(define-integrable (window-select-time frame)
  (with-instance-variables buffer-frame frame ()
    last-select-time))

(define-integrable (set-window-select-time! frame time)
  (with-instance-variables buffer-frame frame (time)
    (set! last-select-time time)))

(define-integrable (window-buffer frame)
  (buffer-window/buffer (frame-text-inferior frame)))

(define (set-window-buffer! frame buffer)
  ;; BUFFER-WINDOW/SET-BUFFER! expects to have interrupts locked here.
  (without-interrupts
   (lambda ()
     ;; Someday this will bite someone...
     (if (and (string-ci=? (buffer-name buffer) "bluffer")
	      (null? (buffer-windows buffer)))
	 (buffer-reset! buffer))
     (if (window-buffer frame)
	 (remove-buffer-window! (window-buffer frame) frame))
     (buffer-window/set-buffer! (frame-text-inferior frame) buffer)
     (add-buffer-window! buffer frame))))

(define-integrable (window-point frame)
  (buffer-window/point (frame-text-inferior frame)))

(define-integrable (set-window-point! frame mark)
  (buffer-window/set-point! (frame-text-inferior frame) mark))

(define-integrable (window-redraw! frame)
  (buffer-window/redraw! (frame-text-inferior frame)))

(define (window-modeline-event! frame type)
  (let ((window (frame-modeline-inferior frame)))
    (if window
	(modeline-window:event! window type)))
  (screen-modeline-event! (window-screen frame) frame type))

(define (notice-window-changes! frame)
  (let ((window (frame-modeline-inferior frame)))
    (if window
	(modeline-window:notice-changes! window)))
  (%notice-window-changes! (frame-text-inferior frame)))

(define-integrable (window-override-message window)
  (buffer-window/override-message (frame-text-inferior window)))

(define-integrable (window-set-override-message! window message)
  (buffer-window/set-override-message! (frame-text-inferior window) message))

(define-integrable (window-clear-override-message! window)
  (buffer-window/clear-override-message! (frame-text-inferior window)))

(define-integrable (window-direct-update! frame display-style)
  (buffer-window/direct-update! (frame-text-inferior frame) display-style))

(define-integrable (window-home-cursor! window)
  (buffer-window/home-cursor! (frame-text-inferior window)))

(define-integrable (window-char->image frame char)
  (%window-char->image (frame-text-inferior frame) char))

(define-integrable (window-direct-output-forward-char! frame)
  (buffer-window/direct-output-forward-char! (frame-text-inferior frame)))

(define-integrable (window-direct-output-backward-char! frame)
  (buffer-window/direct-output-backward-char! (frame-text-inferior frame)))

(define-integrable (window-direct-output-insert-char! frame char)
  (buffer-window/direct-output-insert-char! (frame-text-inferior frame) char))

(define-integrable (window-direct-output-insert-newline! frame)
  (buffer-window/direct-output-insert-newline! (frame-text-inferior frame)))

(define-integrable (window-direct-output-insert-substring! frame
							   string start end)
  (buffer-window/direct-output-insert-substring! (frame-text-inferior frame)
						 string start end))

(define-integrable (window-scroll-y-absolute! frame y-point)
  (buffer-window/scroll-y-absolute! (frame-text-inferior frame) y-point))

(define-integrable (window-scroll-y-relative! frame delta)
  (buffer-window/scroll-y-relative! (frame-text-inferior frame) delta))

(define-integrable (set-window-start-mark! frame mark force?)
  (buffer-window/set-start-mark! (frame-text-inferior frame) mark force?))

(define-integrable (window-y-center frame)
  (buffer-window/y-center (frame-text-inferior frame)))

(define-integrable (window-start-mark frame)
  (buffer-window/start-mark (frame-text-inferior frame)))

(define-integrable (window-mark-visible? frame mark)
  (buffer-window/mark-visible? (frame-text-inferior frame) mark))

(define-integrable (window-mark->x frame mark)
  (buffer-window/mark->x (frame-text-inferior frame) mark))

(define-integrable (window-mark->y frame mark)
  (buffer-window/mark->y (frame-text-inferior frame) mark))

(define-integrable (window-mark->coordinates frame mark)
  (buffer-window/mark->coordinates (frame-text-inferior frame) mark))

(define-integrable (window-point-x frame)
  (buffer-window/point-x (frame-text-inferior frame)))

(define-integrable (window-point-y frame)
  (buffer-window/point-y (frame-text-inferior frame)))

(define-integrable (window-point-coordinates frame)
  (buffer-window/point-coordinates (frame-text-inferior frame)))

(define-integrable (window-coordinates->mark frame x y)
  (buffer-window/coordinates->mark (frame-text-inferior frame) x y))

(define-integrable (set-window-debug-trace! frame debug-trace)
  (%set-window-debug-trace! (frame-text-inferior frame) debug-trace))

(define-variable-per-buffer truncate-lines
  "True means do not display continuation lines;
give each line of text one screen line.
Automatically becomes local when set in any fashion.

Note that this is overridden by the variable
truncate-partial-width-windows if that variable is true
and this buffer is not full-screen width."
  #f
  boolean?)

(define-variable truncate-partial-width-windows
  "True means truncate lines in all windows less than full screen wide."
  #t
  boolean?)

(define-variable-per-buffer tab-width
  "Distance between tab stops (for display of tab characters), in columns.
Automatically becomes local when set in any fashion."
  8
  exact-nonnegative-integer?)

(define-variable-per-buffer char-image-strings
  "A vector of 256 strings mapping ascii bytes to image strings.
Each image is a short string of at least one character.
Index 0 might contain \"^@\" so ascii NUL appears as ^@.
The indices for normal printing characters usually contain a
string containing just that character, e.g. index 65 usually contains \"A\".
Automatically becomes local when set in any fashion."
  default-char-image-strings
  (lambda (object)
    (and (vector? object)
	 (= (vector-length object) 256)
	 (let loop ((i 0))
	   (if (= i 256)
	       #T
	       (and (string? (vector-ref object i))
		    (<= 1 (string-length (vector-ref object i)) 255)
		    (loop (+ i 1))))))))
	 
(let ((setup-truncate-lines!
       (lambda (buffer variable)
	 variable			;ignore
	 (for-each window-redraw!	;recaches these variables
	   (if buffer
	       (buffer-windows buffer)
	       (window-list))))))
  (add-variable-assignment-daemon!
   (ref-variable-object truncate-lines)
   setup-truncate-lines!)
  (add-variable-assignment-daemon!
   (ref-variable-object truncate-partial-width-windows)
   setup-truncate-lines!)
  (add-variable-assignment-daemon!
   (ref-variable-object tab-width)
   setup-truncate-lines!)
  (add-variable-assignment-daemon!
   (ref-variable-object char-image-strings)
   setup-truncate-lines!))

;;;; Window Configurations

(define-structure (window-configuration (conc-name window-configuration/))
  (screen-x-size #f read-only #t)
  (screen-y-size #f read-only #t)
  (root-window #f read-only #t)
  (root-x-size #f read-only #t)
  (root-y-size #f read-only #t)
  (selected-window #f read-only #t)
  (cursor-window #f read-only #t)
  (minibuffer-scroll-window #f read-only #t))

(define-structure (saved-combination (conc-name saved-combination/))
  (vertical? #f read-only #t)
  (children #f read-only #t))

(define-structure (saved-window (conc-name saved-window/))
  (buffer #f read-only #t)
  (point #f read-only #t)
  (mark #f read-only #t)
  (start-mark #f read-only #t))

(define (guarantee-window-configuration object procedure)
  (if (not (window-configuration? object))
      (error:wrong-type-argument object "window configuration" procedure)))

(define (screen-window-configuration screen)
  (guarantee-screen screen 'SCREEN-WINDOW-CONFIGURATION)
  (let ((frame (screen-root-window screen))
	(converted-windows '()))
    (let ((root-window
	   (let convert-window ((window (editor-frame-root-window frame)))
	     (if (combination? window)
		 (let ((vertical? (combination-vertical? window)))
		   (make-saved-combination
		    vertical?
		    (let loop ((window (combination-child window)))
		      (cons (cons (if vertical?
				      (window-y-size window)
				      (window-x-size window))
				  (convert-window window))
			    (let ((next (window-next window)))
			      (if next
				  (loop next)
				  '()))))))
		 (let ((saved-window
			(let ((buffer (window-buffer window)))
			  (make-saved-window
			   buffer
			   (mark-right-inserting-copy (window-point window))
			   (let ((ring (buffer-mark-ring buffer)))
			     (if (ring-empty? ring)
				 #f
				 (mark-right-inserting-copy
				  (ring-ref ring 0))))
			   (mark-right-inserting-copy
			    (window-start-mark window))))))
		   (set! converted-windows
			 (cons (cons window saved-window) converted-windows))
		   saved-window))))
	  (converted-window
	   (lambda (window)
	     (let ((association (assq window converted-windows)))
	       (and association
		    (cdr association)))))
	  (selected-window (editor-frame-selected-window frame)))
      (make-window-configuration
       (screen-x-size screen)
       (screen-y-size screen)
       root-window
       (window-x-size frame)
       (window-y-size frame)
       (converted-window selected-window)
       (let ((window (editor-frame-cursor-window frame)))
	 (and (not (eq? window selected-window))
	      (converted-window window)))
       (let ((window (weak-car *minibuffer-scroll-window*)))
	 (and window
	      (converted-window window)))))))

(define (set-screen-window-configuration! screen configuration)
  (guarantee-screen screen 'SET-SCREEN-WINDOW-CONFIGURATION!)
  (guarantee-window-configuration configuration
				  'SET-SCREEN-WINDOW-CONFIGURATION!)
  (if (and (= (screen-x-size screen)
	      (window-configuration/screen-x-size configuration))
	   (= (screen-y-size screen)
	      (window-configuration/screen-y-size configuration)))
      (begin
	(delete-other-windows (screen-window0 screen))
	(let ((x-size (window-configuration/root-x-size configuration))
	      (y-size (window-configuration/root-y-size configuration))
	      (frame (screen-root-window screen)))
	  (if (not (and (= x-size (window-x-size frame))
			(= y-size (window-y-size frame))))
	      (set-editor-frame-size! frame x-size y-size)))
	(let ((converted-windows '())
	      (need-buffers '()))
	  (let loop
	      ((window (screen-window0 screen))
	       (saved-window (window-configuration/root-window configuration)))
	    (if (saved-combination? saved-window)
		(let ((vertical? (saved-combination/vertical? saved-window)))
		  (let child-loop
		      ((window window)
		       (children (saved-combination/children saved-window)))
		    (let ((new
			   ((if vertical?
				window-split-vertically!
				window-split-horizontally!)
			    window
			    (caar children))))
		      (loop window (cdar children))
		      (if (null? (cddr children))
			  (loop new (cdadr children))
			  (child-loop new (cdr children))))))
		(let ((buffer (saved-window/buffer saved-window)))
		  (if (buffer-alive? buffer)
		      (begin
			(%set-buffer-point! buffer
					    (saved-window/point saved-window))
			(select-buffer-no-record buffer window)
			(let ((mark (saved-window/mark saved-window)))
			  (if mark (push-buffer-mark! buffer mark)))
			(set-window-start-mark!
			 window
			 (saved-window/start-mark saved-window)
			 #t))
		      (set! need-buffers (cons window need-buffers)))
		  (set! converted-windows
			(cons (cons saved-window window) converted-windows)))))
	  (for-each (lambda (window)
		      (let ((buffer (other-buffer #f)))
			(if buffer
			    (select-buffer-no-record buffer window))))
		    need-buffers)
	  (let ((convert-window
		 (lambda (saved-window)
		   (let ((association (assq saved-window converted-windows)))
		     (and association (cdr association))))))
	    (let ((window
		   (window-configuration/selected-window configuration)))
	      (if window
		  (let ((window (convert-window window)))
		    (without-interrupts
		     (lambda () (screen-select-window! screen window))))))
	    (let ((window (window-configuration/cursor-window configuration)))
	      (if window
		  (let ((window (convert-window window)))
		    (without-interrupts
		     (lambda () (screen-select-cursor! screen window))))))
	    (let ((window
		   (window-configuration/minibuffer-scroll-window
		    configuration)))
	      (if window
		  (weak-set-car! *minibuffer-scroll-window*
				 (convert-window window)))))))))

(define (with-saved-configuration thunk)
  (let ((screen (selected-screen)))
    (let ((configuration (screen-window-configuration screen)))
      (fluid-let ((restore-saved-continuation? #t))
	(dynamic-wind
	 (lambda () unspecific)
	 thunk
	 (lambda ()
	   (if restore-saved-continuation?
	       (set-screen-window-configuration! screen configuration))))))))

(define (dont-restore-saved-configuration)
  ;; This conditional will signal an error if this procedure is called
  ;; outside the dynamic context of WITH-SAVED-CONFIGURATION.
  (if restore-saved-continuation?
      (set! restore-saved-continuation? #f))
  unspecific)

(define restore-saved-continuation?)