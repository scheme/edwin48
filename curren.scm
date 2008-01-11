#| -*-Scheme-*-

$Id: curren.scm,v 1.150 2007/01/05 21:19:23 cph Exp $

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

;;;; Current State


;;;; Screens

(define (screen-list)
  (editor-screens current-editor))

(define (selected-screen)
  (editor-selected-screen current-editor))

(define (selected-screen? screen)
  (eq? screen (selected-screen)))

(define (multiple-screens?)
  (display-type/multiple-screens? (current-display-type)))

(define (make-screen buffer . make-screen-args)
  (let ((display-type (current-display-type)))
    (if (not (display-type/multiple-screens? display-type))
	(error "display doesn't support multiple screens" display-type))
    (let ((screen
	   (without-interrupts
	    (lambda ()
	      (let ((screen
		     (display-type/make-screen display-type make-screen-args)))
		(initialize-screen-root-window!
		 screen
		 (editor-bufferset current-editor)
		 buffer)
		(set-editor-screens! current-editor
				     (append! (editor-screens current-editor)
					      (list screen)))
		(event-distributor/invoke!
		 (ref-variable frame-creation-hook #f)
		 screen)
		(update-screen! screen #f)
		screen)))))
      (maybe-select-buffer-layout (screen-window0 screen) buffer)
      screen)))

(define-variable frame-creation-hook
  "An event distributor that is invoked when a frame is created.
The new frame passed as its argument.
The frame is guaranteed to be deselected at that time."
  (make-event-distributor))
(define edwin-variable$screen-creation-hook edwin-variable$frame-creation-hook)

(define (delete-screen! screen #!optional allow-kill-scheme?)
  (without-interrupts
   (lambda ()
     (if (not (screen-deleted? screen))
	 (let ((other (other-screen screen 1 #t)))
	   (if other
	       (begin
		 (if (selected-screen? screen)
		     (select-screen (or (other-screen screen) other)))
		 (screen-discard! screen)
		 (set-editor-screens! current-editor
				      (delq! screen
					     (editor-screens current-editor)))
		 #t)
	       (if (or (default-object? allow-kill-scheme?) allow-kill-scheme?)
		   ((ref-command save-buffers-kill-scheme) #t)
		   #f)))))))

(define (select-screen screen)
  (without-interrupts
   (lambda ()
     (if (not (screen-deleted? screen))
	 (let ((screen* (selected-screen)))
	   (if (not (eq? screen screen*))
	       (let ((message (current-message)))
		 (clear-current-message!)
		 (screen-exit! screen*)
		 (undo-leave-window! (screen-selected-window screen*))
		 (let ((window (screen-selected-window screen)))
		   (change-selected-buffer (window-buffer window) window #t
		     (lambda ()
		       (set-editor-selected-screen! current-editor screen))))
		 (set-current-message! message)
		 (screen-enter! screen)
		 (update-screen! screen #f))))))))

(define (update-screens! display-style)
  (let loop ((screens (screen-list)))
    (if (null? screens)
	(begin
	  ;; All the buffer changes have been successfully written to
	  ;; the screens, so erase the change records.
	  (do ((buffers (buffer-list) (cdr buffers)))
	      ((null? buffers))
	    (set-group-start-changes-index! (buffer-group (car buffers)) #f))
	  #t)
	(and (update-screen! (car screens) display-style)
	     (loop (cdr screens))))))

(define (update-selected-screen! display-style)
  (update-screen! (selected-screen) display-style))

(define (screen0)
  (car (screen-list)))

(define (screen1+ screen)
  (let ((screens (screen-list)))
    (let ((s (memq screen screens)))
      (if (not s)
	  (error "not a member of screen-list" screen))
      (if (null? (cdr s))
	  (car screens)
	  (cadr s)))))

(define (screen-1+ screen)
  (let ((screens (screen-list)))
    (if (eq? screen (car screens))
	(car (last-pair screens))
	(let loop ((previous screens) (screens (cdr screens)))
	  (if (null? screens)
	      (error "not a member of screen-list" screen))
	  (if (eq? screen (car screens))
	      (car previous)
	      (loop screens (cdr screens)))))))

(define (screen+ screen n)
  (cond ((positive? n)
	 (let loop ((n n) (screen screen))
	   (if (= n 1)
	       (screen1+ screen)
	       (loop (-1+ n) (screen1+ screen)))))
	((negative? n)
	 (let loop ((n n) (screen screen))
	   (if (= n -1)
	       (screen-1+ screen)
	       (loop (1+ n) (screen-1+ screen)))))
	(else
	 screen)))

(define (other-screen screen #!optional n invisible-ok?)
  (let ((n (if (default-object? n) 1 n))
	(invisible-ok? (if (default-object? invisible-ok?) #f invisible-ok?)))
    (let ((next-screen (if (> n 0) screen1+ screen-1+)))
      (let loop ((screen* screen) (n (abs n)))
	(if (= n 0)
	    screen*
	    (let ((screen* (next-screen screen*)))
	      (and (not (eq? screen* screen))
		   (loop screen*
			 (if (or invisible-ok? (screen-visible? screen*))
			     (- n 1)
			     n)))))))))

;;;; Windows

(define (window-list)
  (append-map screen-window-list (screen-list)))

(define (selected-window)
  (screen-selected-window (selected-screen)))

(define (selected-window? window)
  (eq? window (selected-window)))

(define current-window selected-window)
(define current-window? selected-window?)

(define (window0)
  (screen-window0 (selected-screen)))

(define (select-window window)
  (without-interrupts
   (lambda ()
     (let* ((screen (window-screen window))
	    (window* (screen-selected-window screen)))
       (if (eq? window window*)
	   (if (not (selected-screen? screen))
	       (select-screen screen))
	   (begin
	     (undo-leave-window! window*)
	     (if (selected-screen? screen)
		 (change-selected-buffer (window-buffer window) window #t
		   (lambda ()
		     (screen-select-window! screen window)))
		 (begin
		   (screen-select-window! screen window)
		   (select-screen screen)))))))))

(define (select-cursor window)
  (screen-select-cursor! (window-screen window) window))

(define (window-visible? window)
  (and (window-live? window)
       (screen-visible? (window-screen window))))

(define (window-live? window)
  (let ((screen (window-screen window)))
    (or (eq? window (screen-typein-window screen))
	(let ((window0 (screen-window0 screen)))
	  (let loop ((window* (window1+ window0)))
	    (or (eq? window window*)
		(and (not (eq? window* window0))
		     (loop (window1+ window*)))))))))

(define (global-window-modeline-event! #!optional predicate)
  (let ((predicate
	 (if (or (default-object? predicate) (not predicate))
	     (lambda (window) window 'GLOBAL-MODELINE)
	     predicate)))
    (for-each
     (lambda (screen)
       (let ((window0 (screen-window0 screen)))
	 (let loop ((window (window1+ window0)))
	   (let ((type (predicate window)))
	     (if type
		 (window-modeline-event! window type)))
	   (if (not (eq? window window0))
	       (loop (window1+ window))))))
     (screen-list))))

(define (other-window #!optional n other-screens?)
  (let ((n (if (or (default-object? n) (not n)) 1 n))
	(other-screens?
	 (if (default-object? other-screens?) #f other-screens?))
	(selected-window (selected-window))
	(typein-ok? (within-typein-edit?)))
    (cond ((positive? n)
	   (let loop ((n n) (window selected-window))
	     (if (zero? n)
		 window
		 (let ((window
			(next-visible-window window
					     typein-ok?
					     other-screens?)))
		   (if window
		       (loop (-1+ n) window)
		       selected-window)))))
	  ((negative? n)
	   (let loop ((n n) (window selected-window))
	     (if (zero? n)
		 window
		 (let ((window
			(previous-visible-window window
						 typein-ok?
						 other-screens?)))
		   (if window
		       (loop (1+ n) window)
		       selected-window)))))
	  (else
	   selected-window))))

(define (next-visible-window first-window typein-ok? #!optional other-screens?)
  (let ((other-screens?
	 (if (default-object? other-screens?) #f other-screens?))
	(first-screen (window-screen first-window)))
    (letrec
	((next-screen
	  (lambda (screen)
	    (let ((screen (if other-screens? (screen1+ screen) screen)))
	      (let ((window (screen-window0 screen)))
		(if (screen-visible? screen)
		    (and (not (and (eq? screen first-screen)
				   (eq? window first-window)))
			 window)
		    (and (not (eq? screen first-screen))
			 (next-screen screen))))))))
      (if (or (not (screen-visible? first-screen))
	      (eq? first-window (screen-typein-window first-screen)))
	  (next-screen first-screen)
	  (let ((window (window1+ first-window)))
	    (if (eq? window (screen-window0 first-screen))
		(or (and typein-ok? (screen-typein-window first-screen))
		    (next-screen first-screen))
		window))))))

(define (previous-visible-window first-window typein-ok?
				 #!optional other-screens?)
  (let ((other-screens?
	 (if (default-object? other-screens?) #f other-screens?))
	(first-screen (window-screen first-window)))
    (letrec
	((previous-screen
	  (lambda (screen)
	    (let ((screen (if other-screens? (screen-1+ screen) screen)))
	      (let ((window
		     (or (and typein-ok? (screen-typein-window screen))
			 (window-1+ (screen-window0 screen)))))
		(if (screen-visible? screen)
		    (and (not (and (eq? screen first-screen)
				   (eq? window first-window)))
			 window)
		    (and (not (eq? screen first-screen))
			 (previous-screen screen))))))))
      (if (or (not (screen-visible? first-screen))
	      (eq? first-window (screen-window0 first-screen)))
	  (previous-screen first-screen)
	  (window-1+ first-window)))))

(define (typein-window)
  (screen-typein-window (selected-screen)))

(define (typein-window? window)
  (eq? window (screen-typein-window (window-screen window))))

(define (current-message)
  (window-override-message (typein-window)))

(define (set-current-message! message)
  (let ((window (typein-window)))
    (if (and message (not *suppress-messages?*))
	(window-set-override-message! window message)
	(window-clear-override-message! window))
    (if (not *executing-keyboard-macro?*)
	(window-direct-update! window #t))))

(define (clear-current-message!)
  (let ((window (typein-window)))
    (window-clear-override-message! window)
    (if (not *executing-keyboard-macro?*)
	(window-direct-update! window #t))))

(define (with-messages-suppressed thunk)
  (fluid-let ((*suppress-messages?* #t))
    (clear-current-message!)
    (thunk)))

(define *suppress-messages?* #f)

;;;; Buffers

(define (buffer-list)
  (bufferset-buffer-list (current-bufferset)))

(define (buffer-alive? buffer)
  (memq buffer (buffer-list)))

(define (buffer-names)
  (bufferset-names (current-bufferset)))

(define (selected-buffer)
  (window-buffer (selected-window)))

(define (selected-buffer? buffer)
  (eq? buffer (selected-buffer)))

(define current-buffer selected-buffer)
(define current-buffer? selected-buffer?)

(define (previous-buffer)
  (other-buffer (selected-buffer)))

(define (other-buffer buffer)
  (let loop ((less-preferred #f) (buffers (buffer-list)))
    (cond ((null? buffers)
	   less-preferred)
	  ((or (eq? buffer (car buffers))
	       (minibuffer? (car buffers)))
	   (loop less-preferred (cdr buffers)))
	  ((buffer-visible? (car buffers))
	   (loop (or less-preferred (car buffers)) (cdr buffers)))
	  (else
	   (car buffers)))))

(define (bury-buffer buffer)
  (bufferset-bury-buffer! (current-bufferset) buffer))

(define (find-buffer name #!optional error?)
  (let ((buffer (bufferset-find-buffer (current-bufferset) name)))
    (if (and (not buffer)
	     (not (default-object? error?))
	     error?)
	(editor-error "No buffer named " name))
    buffer))

(define (create-buffer name)
  (bufferset-create-buffer (current-bufferset) name))

(define (find-or-create-buffer name)
  (bufferset-find-or-create-buffer (current-bufferset) name))

(define (rename-buffer buffer new-name)
  (without-interrupts
   (lambda ()
     (run-buffer-hooks 'RENAME-BUFFER-HOOKS buffer new-name)
     (bufferset-rename-buffer (current-bufferset) buffer new-name))))

(define (add-rename-buffer-hook buffer hook)
  (add-buffer-hook buffer 'RENAME-BUFFER-HOOKS hook))

(define (remove-rename-buffer-hook buffer hook)
  (remove-buffer-hook buffer 'RENAME-BUFFER-HOOKS hook))

(define (kill-buffer buffer)
  (without-interrupts
   (lambda ()
     (if (not (make-buffer-invisible buffer))
	 (error "Buffer to be killed has no replacement" buffer))
     (for-each (lambda (process)
		 (hangup-process process #t)
		 (set-process-buffer! process #f))
	       (buffer-processes buffer))
     (run-buffer-hooks 'KILL-BUFFER-HOOKS buffer)
     (delete-buffer-layout buffer)
     (bufferset-kill-buffer! (current-bufferset) buffer))))

(define (make-buffer-invisible buffer)
  (let loop ((windows (buffer-windows buffer)) (last-buffer #f))
    (or (not (pair? windows))
	(let ((new-buffer (or (other-buffer buffer) last-buffer)))
	  (and new-buffer
	       (begin
		 (select-buffer-no-record new-buffer (car windows))
		 (loop (cdr windows) new-buffer)))))))

(define (add-kill-buffer-hook buffer hook)
  (add-buffer-hook buffer 'KILL-BUFFER-HOOKS hook))

(define (remove-kill-buffer-hook buffer hook)
  (remove-buffer-hook buffer 'KILL-BUFFER-HOOKS hook))

(define (add-buffer-hook buffer key hook)
  (let ((hooks (buffer-get buffer key '())))
    (cond ((null? hooks)
	   (buffer-put! buffer key (list hook)))
	  ((not (memq hook hooks))
	   (set-cdr! (last-pair hooks) (list hook))))))

(define (remove-buffer-hook buffer key hook)
  (buffer-put! buffer key (delq! hook (buffer-get buffer key '()))))

(define (run-buffer-hooks key buffer . arguments)
  (for-each (lambda (hook) (apply hook buffer arguments))
	    (list-copy (buffer-get buffer key '()))))

(define (select-buffer buffer #!optional window)
  (select-buffer-in-window buffer
			   (if (or (default-object? window) (not window))
			       (selected-window)
			       window)
			   #t))

(define (select-buffer-no-record buffer #!optional window)
  (select-buffer-in-window buffer
			   (if (or (default-object? window) (not window))
			       (selected-window)
			       window)
			   #f))

(define (select-buffer-in-window buffer window record?)
  (if (without-interrupts
       (lambda ()
	 (and (not (eq? buffer (window-buffer window)))
	      (begin
		(undo-leave-window! window)
		(if (selected-window? window)
		    (change-selected-buffer buffer window record?
		      (lambda ()
			(set-window-buffer! window buffer)))
		    (set-window-buffer! window buffer))
		#t))))
      (maybe-select-buffer-layout window buffer)))

(define (change-selected-buffer buffer window record? selection-thunk)
  (change-local-bindings! (selected-buffer) buffer selection-thunk)
  (set-buffer-point! buffer (window-point window))
  (if record? (bufferset-select-buffer! (current-bufferset) buffer))
  (run-buffer-hooks 'SELECT-BUFFER-HOOKS buffer window)
  (if (not (minibuffer? buffer))
      (event-distributor/invoke! (ref-variable select-buffer-hook #f)
				 buffer
				 window)))

(define (add-select-buffer-hook buffer hook)
  (add-buffer-hook buffer 'SELECT-BUFFER-HOOKS hook))

(define (remove-select-buffer-hook buffer hook)
  (remove-buffer-hook buffer 'SELECT-BUFFER-HOOKS hook))

(define-variable select-buffer-hook
  "An event distributor that is invoked when a buffer is selected.
The new buffer and the window in which it is selected are passed as arguments.
The buffer is guaranteed to be selected at that time."
  (make-event-distributor))

(define (with-selected-buffer buffer thunk)
  (let ((old-buffer))
    (dynamic-wind (lambda ()
		    (let ((window (selected-window)))
		      (set! old-buffer (window-buffer window))
		      (if (buffer-alive? buffer)
			  (select-buffer buffer window)))
		    (set! buffer)
		    unspecific)
		  thunk
		  (lambda ()
		    (let ((window (selected-window)))
		      (set! buffer (window-buffer window))
		      (if (buffer-alive? old-buffer)
			  (select-buffer old-buffer window)))
		    (set! old-buffer)
		    unspecific))))

(define (current-process)
  (let ((process (get-buffer-process (selected-buffer))))
    (if (not process)
	(editor-error "Selected buffer has no process"))
    process))

;;;; Buffer Layouts

(define (create-buffer-layout selector buffers)
  (let ((layout (cons selector (list->weak-list buffers))))
    (without-interrupts
     (lambda ()
       (for-each (lambda (buffer)
		   (if (buffer-get buffer buffer-layout-key #f)
		       (error "Can't add buffer to multiple layouts:" buffer)))
		 buffers)
       (for-each (lambda (buffer)
		   (buffer-put! buffer buffer-layout-key layout))
		 buffers)))))

(define (maybe-select-buffer-layout window buffer)
  (if (not (or setting-up-buffer-layout? (typein-window? window)))
      (let ((layout
	     (without-interrupts
	      (lambda ()
		(maybe-select-buffer-layout-1 window buffer)))))
	(if layout
	    (fluid-let ((setting-up-buffer-layout? #t))
	      ((car layout) window (weak-list->list (cdr layout))))))))

(define (maybe-select-buffer-layout-1 window buffer)
  (let ((screen (window-screen window)))
    (let ((l1 (hash-table/get screen-buffer-layouts screen #f))
	  (l2 (buffer-get buffer buffer-layout-key #f)))
      (and (or (not (eq? l1 l2))
	       (and l1 (buffer-layout-visible? l1 screen)))
	   (begin
	     (if l1
		 (begin
		   (hash-table/remove! screen-buffer-layouts screen)
		   (delete-other-windows window)))
	     (and l2
		  (if (let loop ((buffers (cdr l2)))
			(or (not (weak-pair? buffers))
			    (and (let ((buffer (weak-car buffers)))
				   (and buffer
					(buffer-alive? buffer)))
				 (loop (weak-cdr buffers)))))
		      (begin
			(delete-other-windows window)
			(hash-table/put! screen-buffer-layouts screen l2)
			l2)
		      (begin
			(delete-buffer-layout-1 l2)
			#f))))))))

(define (maybe-deselect-buffer-layout screen)
  (without-interrupts
   (lambda ()
     (if (hash-table/get screen-buffer-layouts screen #f)
	 (begin
	   (hash-table/remove! screen-buffer-layouts screen)
	   (delete-other-windows (screen-selected-window screen)))))))

(define (delete-buffer-layout buffer)
  ;; Caller disables interrupts.
  (let ((layout (buffer-get buffer buffer-layout-key #f)))
    (if layout
	(delete-buffer-layout-1 layout))))

(define (delete-buffer-layout-1 layout)
  (hash-table/for-each screen-buffer-layouts
    (lambda (screen layout*)
      (if (eq? layout layout*)
	  (hash-table/remove! screen-buffer-layouts screen))))
  (do ((buffers (cdr layout) (weak-cdr buffers)))
      ((not (weak-pair? buffers)))
    (let ((buffer (weak-car buffers)))
      (if buffer
	  (buffer-remove! buffer buffer-layout-key)))))

(define (buffer-layout-visible? layout screen)
  (let loop ((buffers (cdr layout)))
    (and (weak-pair? buffers)
	 (or (not (let ((buffer (weak-car buffers)))
		    (and buffer
			 (there-exists? (buffer-windows buffer)
			   (lambda (window)
			     (eq? (window-screen window) screen))))))
	     (weak-cdr buffers)))))

(define setting-up-buffer-layout? #f)
(define buffer-layout-key (list 'BUFFER-LAYOUT))
(define screen-buffer-layouts)

(add-event-receiver! editor-initializations
  (lambda ()
    (set! screen-buffer-layouts (make-eq-hash-table))
    unspecific))

;;;; Point

(define (current-point)
  (window-point (selected-window)))

(define (set-current-point! mark)
  (set-window-point! (selected-window) mark))

(define (set-buffer-point! buffer mark)
  (let ((window (selected-window)))
    (if (eq? buffer (window-buffer window))
	(set-window-point! window mark)
	(let ((windows (buffer-windows buffer)))
	  (if (pair? windows)
	      (set-window-point! (car windows) mark)
	      (%set-buffer-point! buffer mark))))))

(define (with-current-point point thunk)
  (let ((old-point))
    (dynamic-wind (lambda ()
		    (let ((window (selected-window)))
		      (set! old-point (window-point window))
		      (set-window-point! window point))
		    (set! point)
		    unspecific)
		  thunk
		  (lambda ()
		    (let ((window (selected-window)))
		      (set! point (window-point window))
		      (set-window-point! window old-point))
		    (set! old-point)
		    unspecific))))

(define (current-column)
  (mark-column (current-point)))

(define (save-excursion thunk)
  (let ((point (mark-right-inserting-copy (current-point)))
	(mark (mark-right-inserting-copy (current-mark))))
    (thunk)
    (let ((buffer (mark-buffer point)))
      (if (buffer-alive? buffer)
	  (begin
	    (select-buffer buffer)
	    (set-current-point! point)
	    (set-current-mark! mark))))))

;;;; Mark and Region

(define (current-mark)
  (buffer-mark (selected-buffer)))

(define (buffer-mark buffer)
  (let ((ring (buffer-mark-ring buffer)))
    (if (ring-empty? ring)
	(editor-error)
	(ring-ref ring 0))))

(define (set-current-mark! mark)
  (set-buffer-mark! (selected-buffer) (guarantee-mark mark)))

(define (set-buffer-mark! buffer mark)
  (ring-set! (buffer-mark-ring buffer) 0 (mark-right-inserting-copy mark)))

(define-variable auto-push-point-notification
  "Message to display when point is pushed on the mark ring.
If false, don't display any message."
  "Mark set"
  string-or-false?)

(define (push-current-mark! mark)
  (push-buffer-mark! (selected-buffer) (guarantee-mark mark))
  (let ((notification (ref-variable auto-push-point-notification)))
    (if (and notification
	     (not *executing-keyboard-macro?*)
	     (not (typein-window? (selected-window))))
	(temporary-message notification))))

(define (push-buffer-mark! buffer mark)
  (ring-push! (buffer-mark-ring buffer) (mark-right-inserting-copy mark)))

(define (pop-current-mark!)
  (pop-buffer-mark! (selected-buffer)))

(define (pop-buffer-mark! buffer)
  (ring-pop! (buffer-mark-ring buffer)))

(define (current-region)
  (make-region (current-point) (current-mark)))

(define (set-current-region! region)
  (set-current-point! (region-start region))
  (push-current-mark! (region-end region)))

(define (set-current-region-reversed! region)
  (push-current-mark! (region-start region))
  (set-current-point! (region-end region)))

;;;; Modes and Comtabs

(define (current-major-mode)
  (buffer-major-mode (selected-buffer)))

(define (current-minor-modes)
  (buffer-minor-modes (selected-buffer)))

(define (current-comtabs)
  (buffer-comtabs (selected-buffer)))

(define (set-current-major-mode! mode)
  (set-buffer-major-mode! (selected-buffer) mode))

(define (current-minor-mode? mode)
  (buffer-minor-mode? (selected-buffer) mode))

(define (enable-current-minor-mode! mode)
  (enable-buffer-minor-mode! (selected-buffer) mode))

(define (disable-current-minor-mode! mode)
  (disable-buffer-minor-mode! (selected-buffer) mode))