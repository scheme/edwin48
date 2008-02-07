#| -*-Scheme-*-

$Id: xterm.scm,v 1.82 2008/01/30 20:02:08 cph Exp $

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

;;;; X Terminal
;;; Package: (edwin x-screen)


(define-primitives
  (x-change-property 7)
  (x-close-all-displays 0)
  (x-close-display 1)
  (x-close-window 1)
  (x-convert-selection 6)
  (x-delete-property 3)
  (x-display-descriptor 1)
  (x-display-flush 1)
  (x-display-get-default 3)
  (x-display-get-size 2)
  (x-display-process-events 2)
  (x-display-sync 2)
  (x-get-atom-name 2)
  (x-get-selection-owner 2)
  (x-get-window-property 7)
  (x-intern-atom 3)
  (x-max-request-size 1)
  (x-open-display 1)
  (x-select-input 3)
  (x-send-selection-notify 6)
  (x-set-selection-owner 4)
  (x-window-andc-event-mask 2)
  (x-window-beep 1)
  (x-window-display 1)
  (x-window-flush 1)
  (x-window-id 1)
  (x-window-map 1)
  (x-window-or-event-mask 2)
  (x-window-raise 1)
  (x-window-set-event-mask 2)
  (x-window-set-icon-name 2)
  (x-window-set-input-focus 2)
  (x-window-set-name 2)
  (xterm-clear-rectangle! 6)
  (xterm-draw-cursor 1)
  (xterm-dump-rectangle 5)
  (xterm-enable-cursor 2)
  (xterm-erase-cursor 1)
  (xterm-map-x-coordinate 2)
  (xterm-map-x-size 2)
  (xterm-map-y-coordinate 2)
  (xterm-map-y-size 2)
  (xterm-open-window 3)
  (xterm-reconfigure 3)
  (xterm-restore-contents 6)
  (xterm-save-contents 5)
  (xterm-scroll-lines-down 6)
  (xterm-scroll-lines-up 6)
  (xterm-set-size 3)
  (xterm-write-char! 5)
  (xterm-write-cursor! 3)
  (xterm-write-substring! 7)
  (xterm-x-size 1)
  (xterm-y-size 1))

;; These constants must match "microcode/x11base.c"
(define event:process-output -2)
(define event:process-status -3)
(define event:inferior-thread-output -4)
(define event-type:button-down 0)
(define event-type:button-up 1)
(define event-type:configure 2)
(define event-type:enter 3)
(define event-type:focus-in 4)
(define event-type:focus-out 5)
(define event-type:key-press 6)
(define event-type:leave 7)
(define event-type:motion 8)
(define event-type:expose 9)
(define event-type:delete-window 10)
(define event-type:map 11)
(define event-type:unmap 12)
(define event-type:take-focus 13)
(define event-type:visibility 14)
(define event-type:selection-clear 15)
(define event-type:selection-notify 16)
(define event-type:selection-request 17)
(define event-type:property-notify 18)
(define number-of-event-types 19)

;; This mask contains button-down, button-up, configure, focus-in,
;; key-press, expose, destroy, map, unmap, visibility,
;; selection-clear, selection-notify, selection-request, and
;; property-notify.
(define event-mask #x7de57)

(define-structure (xterm-screen-state
		   (constructor make-xterm-screen-state (xterm display))
		   (conc-name xterm-screen-state/))
  (xterm #f read-only #t)
  (display #f read-only #t)
  (redisplay-flag #t)
  (selected? #t)
  (name #f)
  (icon-name #f)
  (x-visibility 'VISIBLE)
  (mapped? #f)
  (unexposed? #t))

(define screen-list)

(define (make-xterm-screen #!optional geometry)
  ;; Don't map the window until all of the data structures are in
  ;; place.  This guarantees that no events will be missed.
  (let ((xterm
	 (open-window (null? screen-list)
		      (if (default-object? geometry) #f geometry))))
    (x-window-set-event-mask xterm event-mask)
    (let ((screen
	   (make-screen (make-xterm-screen-state xterm
						 (x-window-display xterm))
			xterm-screen/beep
			xterm-screen/clear-line!
			xterm-screen/clear-rectangle!
			xterm-screen/clear-screen!
			xterm-screen/discard!
			xterm-screen/enter!
			xterm-screen/exit!
			xterm-screen/flush!
			xterm-screen/modeline-event!
			#f
			xterm-screen/scroll-lines-down!
			xterm-screen/scroll-lines-up!
			xterm-screen/wrap-update!
			xterm-screen/write-char!
			xterm-screen/write-cursor!
			xterm-screen/write-substring!
			8
			(xterm-x-size xterm)
			(xterm-y-size xterm))))
      (set! screen-list (cons screen screen-list))
      (update-visibility! screen)
      (x-window-map xterm)
      (x-window-flush xterm)
      screen)))

(define (open-window primary? geometry)
  (let ((display (or (get-x-display) (error "Unable to open display.")))
	(instance (if primary? "edwin" "edwinSecondary"))
	(class "Emacs"))
    (xterm-open-window display
		       (or geometry
			   (get-geometry display primary? instance class))
		       (vector #f instance class))))

(define (get-geometry display primary? instance class)
  (or (x-display-get-geometry display instance)
      (let ((geometry (x-display-get-geometry display class)))
	(and geometry
	     (if primary? geometry (strip-position-from-geometry geometry))))
      "80x40"))

(define (x-display-get-geometry display key)
  (or (x-display-get-default display key "geometry")
      (x-display-get-default display key "Geometry")))

(define (strip-position-from-geometry geometry)
  (let ((sign
	 (or (string-index geometry #\+)
	     (string-index geometry #\-))))
    (if sign
	(string-head geometry sign)
	geometry)))

(define (x-root-window-size)
  (x-display-get-size (or (get-x-display) (error "Unable to open display."))
		      0))

;;; According to the Xlib manual, we're not allowed to draw anything
;;; on the window until the first Expose event arrives.  The manual
;;; says nothing about the relationship between this event and the
;;; MapNotify event associated with that mapping.  We use the fields
;;; UNEXPOSED? and MAPPED? to track the arrival of those events.
;;; The screen's visibility remains 'UNMAPPED until both have arrived.
;;; Meanwhile, X-VISIBILITY tracks Visibility events.  When the window
;;; is both exposed and mapped, VISIBILITY reflects X-VISIBILITY.

(define (screen-x-visibility screen)
  (xterm-screen-state/x-visibility (screen-state screen)))

(define (set-screen-x-visibility! screen flag)
  (set-xterm-screen-state/x-visibility! (screen-state screen) flag)
  (update-visibility! screen))

(define (screen-mapped? screen)
  (xterm-screen-state/mapped? (screen-state screen)))

(define (set-screen-mapped?! screen flag)
  (set-xterm-screen-state/mapped?! (screen-state screen) flag)
  (update-visibility! screen))

(define (screen-unexposed? screen)
  (xterm-screen-state/unexposed? (screen-state screen)))

(define (set-screen-unexposed?! screen value)
  (set-xterm-screen-state/unexposed?! (screen-state screen) value))

(define (screen-exposed? screen)
  (not (screen-unexposed? screen)))

(define (note-xterm-exposed xterm)
  (let ((screen (xterm->screen xterm)))
    (if screen
	(let ((unexposed? (screen-unexposed? screen)))
	  (if unexposed?
	      (begin
		(set-screen-unexposed?! screen #f)
		(update-visibility! screen)
		(if (eq? 'ENTERED unexposed?)
		    (xterm-screen/enter! screen))))))))

(define (update-visibility! screen)
  (if (not (screen-deleted? screen))
      (set-screen-visibility! screen
			      (if (and (screen-mapped? screen)
				       (screen-exposed? screen))
				  (screen-x-visibility screen)
				  'UNMAPPED))))

(define (screen-xterm screen)
  (xterm-screen-state/xterm (screen-state screen)))

(define (xterm->screen xterm)
  (let loop ((screens screen-list))
    (and (not (null? screens))
	 (if (eq? xterm (screen-xterm (car screens)))
	     (car screens)
	     (loop (cdr screens))))))

(define (screen-display screen)
  (xterm-screen-state/display (screen-state screen)))

(define (screen-redisplay-flag screen)
  (xterm-screen-state/redisplay-flag (screen-state screen)))

(define (set-screen-redisplay-flag! screen flag)
  (set-xterm-screen-state/redisplay-flag! (screen-state screen) flag))

(define (screen-selected? screen)
  (xterm-screen-state/selected? (screen-state screen)))

(define (set-screen-selected?! screen selected?)
  (set-xterm-screen-state/selected?! (screen-state screen) selected?))

(define (screen-name screen)
  (xterm-screen-state/name (screen-state screen)))

(define (set-screen-name! screen name)
  (set-xterm-screen-state/name! (screen-state screen) name))

(define (xterm-screen/set-name screen name)
  (let ((name* (screen-name screen)))
    (if (or (not name*) (not (string=? name name*)))
	(begin
	  (set-screen-name! screen name)
	  (x-window-set-name (screen-xterm screen) name)))))

(define (screen-icon-name screen)
  (xterm-screen-state/icon-name (screen-state screen)))

(define (set-screen-icon-name! screen name)
  (set-xterm-screen-state/icon-name! (screen-state screen) name))

(define (xterm-screen/set-icon-name screen name)
  (let ((name* (screen-icon-name screen)))
    (if (or (not name*) (not (string=? name name*)))
	(begin
	  (set-screen-icon-name! screen name)
	  (x-window-set-icon-name (screen-xterm screen) name)))))

(define (xterm-screen/wrap-update! screen thunk)
  (let ((finished? #f))
    (dynamic-wind
     (lambda ()
       (xterm-enable-cursor (screen-xterm screen) #f))
     (lambda ()
       (let ((result (thunk)))
	 (set! finished? result)
	 result))
     (lambda ()
       (if (screen-selected? screen)
	   (let ((xterm (screen-xterm screen)))
	     (xterm-enable-cursor xterm #t)
	     (xterm-draw-cursor xterm)))
       (if (and finished? (screen-redisplay-flag screen))
	   (begin
	     (update-xterm-screen-names! screen)
	     (set-screen-redisplay-flag! screen #f)))
       (xterm-screen/flush! screen)))))

(define (xterm-screen/discard! screen)
  (set! screen-list (delq! screen screen-list))
  (x-close-window (screen-xterm screen)))

(define (xterm-screen/modeline-event! screen window type)
  window type				; ignored
  (set-screen-redisplay-flag! screen #t))

(define (xterm-screen/enter! screen)
  (if (screen-unexposed? screen)
      (set-screen-unexposed?! screen 'ENTERED)
      (begin
	(set-screen-selected?! screen #t)
	(let ((xterm (screen-xterm screen)))
	  (xterm-enable-cursor xterm #t)
	  (xterm-draw-cursor xterm))
	(xterm-screen/grab-focus! screen)
	(xterm-screen/flush! screen))))

(define (xterm-screen/grab-focus! screen)
  (and last-focus-time
       (not (screen-deleted? screen))
       (screen-mapped? screen)
       (begin
	 (x-window-set-input-focus (screen-xterm screen) last-focus-time)
	 #t)))

(define (xterm-screen/exit! screen)
  (set-screen-selected?! screen #f)
  (let ((xterm (screen-xterm screen)))
    (xterm-enable-cursor xterm #f)
    (xterm-erase-cursor xterm))
  (xterm-screen/flush! screen))

(define (xterm-screen/scroll-lines-down! screen xl xu yl yu amount)
  (xterm-scroll-lines-down (screen-xterm screen) xl xu yl yu amount)
  'UNCHANGED)

(define (xterm-screen/scroll-lines-up! screen xl xu yl yu amount)
  (xterm-scroll-lines-up (screen-xterm screen) xl xu yl yu amount)
  'UNCHANGED)

(define (xterm-screen/beep screen)
  (x-window-beep (screen-xterm screen))
  (xterm-screen/flush! screen))

(define (xterm-screen/flush! screen)
  (x-display-flush (screen-display screen)))

(define (xterm-screen/write-char! screen x y char highlight)
  (xterm-write-char! (screen-xterm screen) x y char (if highlight 1 0)))

(define (xterm-screen/write-cursor! screen x y)
  (xterm-write-cursor! (screen-xterm screen) x y))

(define (xterm-screen/write-substring! screen x y string start end highlight)
  (xterm-write-substring! (screen-xterm screen) x y string start end
			  (if highlight 1 0)))

(define (xterm-screen/clear-line! screen x y first-unused-x)
  (xterm-clear-rectangle! (screen-xterm screen)
			  x first-unused-x y (fix:1+ y) 0))

(define (xterm-screen/clear-rectangle! screen xl xu yl yu highlight)
  (xterm-clear-rectangle! (screen-xterm screen)
			  xl xu yl yu (if highlight 1 0)))

(define (xterm-screen/clear-screen! screen)
  (xterm-clear-rectangle! (screen-xterm screen)
			  0 (screen-x-size screen) 0 (screen-y-size screen) 0))

;;;; Event Handling

(define (get-xterm-input-operations)
  (let ((display x-display-data)
	(queue x-display-events)
	(pending-result #f)
	(string #f)
	(start 0)
	(end 0))
    (let ((process-key-press-event
	   (lambda (event)
	     (set! last-focus-time (vector-ref event 5))
	     (set! string (vector-ref event 2))
	     (set! end (string-length string))
	     (set! start end)
	     (cond ((fix:= end 0)
		    (x-make-special-key (vector-ref event 4)
					(vector-ref event 3)))
		   ((fix:= end 1)
		    (let ((char
			   (merge-bucky-bits (string-ref string 0)
					     (vector-ref event 3))))
		      (if (and signal-interrupts? (char=? char #\BEL))
			  (begin
			    (signal-interrupt!)
			    #f)
			  char)))
		   (else
		    (let ((i
			   (and signal-interrupts?
				(string-index-right string #\BEL))))
		      (if i
			  (begin
			    (set! start (fix:+ i 1))
			    (signal-interrupt!)
			    (and (fix:< start end)
				 (let ((result (string-ref string start)))
				   (set! start (fix:+ start 1))
				   result)))
			  (begin
			    (set! start 1)
			    (string-ref string 0)))))))))
      (let ((process-event
	     (lambda (event)
	       (if (fix:= event-type:key-press (vector-ref event 0))
		   (process-key-press-event event)
		   (process-special-event event))))
	    (pce-event
	     (lambda (flag)
	       (make-input-event (if (eq? flag 'FORCE-RETURN) 'RETURN 'UPDATE)
				 update-screens!
				 #f))))
	(let ((get-next-event
	       (lambda (block?)
		 (let loop ()
		   (let ((event (read-event queue display block?)))
		     (cond ((or (not event) (input-event? event))
			    event)
			   ((not (vector? event))
			    (let ((flag (process-change-event event)))
			      (if flag
				  (pce-event flag)
				  (loop))))
			   (else
			    (or (process-event event)
				(loop)))))))))
	  (let ((probe
		 (lambda (block?)
		   (let ((result (get-next-event block?)))
		     (if result
			 (set! pending-result result))
		     result)))
		(guarantee-result
		 (lambda ()
		   (or (get-next-event #t)
		       (error "#F returned from blocking read")))))
	    (values
	     (lambda ()			;halt-update?
	       (or pending-result
		   (fix:< start end)
		   (probe 'IN-UPDATE)))
	     (lambda ()			;peek-no-hang
	       (or pending-result
		   (fix:< start end)
		   (probe #f)))
	     (lambda ()			;peek
	       (or pending-result
		   (if (fix:< start end)
		       (string-ref string start)
		       (let ((result (guarantee-result)))
			 (set! pending-result result)
			 result))))
	     (lambda ()			;read
	       (cond (pending-result
		      => (lambda (result)
			   (set! pending-result #f)
			   result))
		     ((fix:< start end)
		      (let ((char (string-ref string start)))
			(set! start (fix:+ start 1))
			char))
		     (else
		      (guarantee-result)))))))))))

(define (read-event queue display block?)
  (let loop ()
    (set! reading-event? #t)
    (let ((event
	   (if (queue-empty? queue)
	       (if (eq? 'IN-UPDATE block?)
		   (x-display-process-events display 2)
		   (read-event-1 display block?))
	       (dequeue!/unsafe queue))))
      (set! reading-event? #f)
      (if (and (vector? event)
	       (fix:= (vector-ref event 0) event-type:expose))
	  (begin
	    (process-expose-event event)
	    (loop))
	  (begin
	    (if (and event trace-port)
		(write-line event trace-port))
	    event)))))

(define trace-port #f)

(define (start-trace filename)
  (stop-trace)
  (set! trace-port (open-output-file filename))
  unspecific)

(define (stop-trace)
  (let ((port trace-port))
    (set! trace-port #f)
    (if port (close-port port))))

(define (process-expose-event event)
  (let ((xterm (vector-ref event 1)))
    ;; If this is the first Expose event for this window, it
    ;; requires special treatment.  Element 6 of the event
    ;; is 0 for Expose events and 1 for GraphicsExpose
    ;; events.
    (if (eq? 0 (vector-ref event 6))
	(note-xterm-exposed xterm))
    (xterm-dump-rectangle xterm
			  (vector-ref event 2)
			  (vector-ref event 3)
			  (vector-ref event 4)
			  (vector-ref event 5))))

(define (read-event-1 display block?)
  (or (x-display-process-events display 2)
      (let loop ()
	(without-interrupts
	 (lambda ()
	   (cond (inferior-thread-changes?
		  event:inferior-thread-output)
		 ((process-output-available?)
		  event:process-output)
		 ((process-status-changes?)
		  event:process-status)
		 (else
		  (let ((flag
			 (test-for-io-on-descriptor
			  (x-display-descriptor display)
			  block?
			  'READ)))
		    (case flag
		      ((#F) #f)
		      ((PROCESS-STATUS-CHANGE) event:process-status)
		      ((INTERRUPT) (loop))
		      (else (read-event-1 display block?)))))))))))

(define (preview-event-stream)
  (set! previewer-registration
	(permanently-register-io-thread-event
	 (x-display-descriptor x-display-data)
	 'READ
	 (current-thread)
	 (lambda (mode)
	   mode
	   (if (not reading-event?)
	       (let ((event (x-display-process-events x-display-data 2)))
		 (if event
		     (preview-event event)))))))
  unspecific)

(define (wait-for-event interval predicate process-event)
  (let ((timeout (+ (real-time-clock) interval)))
    (fluid-let ((reading-event? #t))
      (let loop ()
	(let ((event (x-display-process-events x-display-data 2)))
	  (if event
	      (if (and (vector? event) (predicate event))
		  (or (process-event event) (loop))
		  (begin (preview-event event) (loop)))
	      (and (< (real-time-clock) timeout)
		   (loop))))))))

(define (preview-event event)
  (cond ((and signal-interrupts?
	      (vector? event)
	      (fix:= event-type:key-press (vector-ref event 0))
              (let ((string (vector-ref event 2)))
                (if (fix:= 1 (string-length string))
		   (char=? #\BEL
			   (merge-bucky-bits (string-ref string 0)
					     (vector-ref event 3)))
		   (string-find-next-char string #\BEL))))
	 (clean-event-queue x-display-events)
	 (signal-interrupt!))
	((and (vector? event)
	      (fix:= event-type:expose (vector-ref event 0)))
	 (process-expose-event event))
	((and (vector? event)
	      (or (fix:= event-type:map (vector-ref event 0))
		  (fix:= event-type:unmap (vector-ref event 0))
		  (fix:= event-type:visibility (vector-ref event 0))))
	 (let ((result (process-special-event event)))
	   (if result
	       (enqueue!/unsafe x-display-events result))))
	(else
	 (enqueue!/unsafe x-display-events event))))

(define (clean-event-queue queue)
  ;; Flush keyboard and mouse events from the input queue.  Other
  ;; events are harmless and must be processed regardless.
  (do ((events (let loop ()
		 (if (queue-empty? queue)
		     '()
		     (let ((event (dequeue!/unsafe queue)))
		       (if (and (vector? event)
				(let ((type (vector-ref event 0)))
				  (or (fix:= type event-type:button-down)
				      (fix:= type event-type:button-up)
				      (fix:= type event-type:key-press)
				      (fix:= type event-type:motion))))
			   (loop)
			   (cons event (loop))))))
	       (cdr events)))
      ((null? events))
    (enqueue!/unsafe queue (car events))))

(define (process-change-event event)
  (cond ((fix:= event event:process-output) (accept-process-output))
	((fix:= event event:process-status) (handle-process-status-changes))
	((fix:= event event:inferior-thread-output) (accept-thread-output))
	(else (error "Illegal change event:" event))))

(define (process-special-event event)
  (let ((handler (vector-ref event-handlers (vector-ref event 0))))
    (and handler
	 (if (vector-ref event 1)
	     (let ((screen (xterm->screen (vector-ref event 1))))
	       (and screen
		    (handler screen event)))
	     (handler #f event)))))

(define event-handlers
  (make-vector number-of-event-types #f))

(define (define-event-handler event-type handler)
  (vector-set! event-handlers event-type handler))

(define-event-handler event-type:button-down
  (lambda (screen event)
    (set! last-focus-time (vector-ref event 5))
    (if (eq? ignore-button-state 'IGNORE-BUTTON-DOWN)
	(begin
	  (set! ignore-button-state 'IGNORE-BUTTON-UP)
	  #f)
	(let ((xterm (screen-xterm screen)))
	  (make-input-event
	   'BUTTON
	   execute-button-command
	   screen
	   (let ((n (vector-ref event 4)))
	     (make-down-button (fix:and n #x0FF)
			       (fix:lsh (fix:and n #xF00) -8)))
	   (xterm-map-x-coordinate xterm (vector-ref event 2))
	   (xterm-map-y-coordinate xterm (vector-ref event 3)))))))

(define-event-handler event-type:button-up
  (lambda (screen event)
    (set! last-focus-time (vector-ref event 5))
    (if (eq? ignore-button-state 'IGNORE-BUTTON-UP)
	(begin
	  (set! ignore-button-state #f)
	  #f)
	(let ((xterm (screen-xterm screen)))
	  (make-input-event
	   'BUTTON
	   execute-button-command
	   screen
	   (let ((n (vector-ref event 4)))
	     (make-up-button (fix:and n #x0FF)
			     (fix:lsh (fix:and n #xF00) -8)))
	   (xterm-map-x-coordinate xterm (vector-ref event 2))
	   (xterm-map-y-coordinate xterm (vector-ref event 3)))))))

(define-event-handler event-type:configure
  (lambda (screen event)
    (make-input-event 'SET-SCREEN-SIZE
		      (lambda (screen event)
			(let ((xterm (screen-xterm screen))
			      (x-size (vector-ref event 2))
			      (y-size (vector-ref event 3)))
			  (let ((x-size (xterm-map-x-size xterm x-size))
				(y-size (xterm-map-y-size xterm y-size)))
			    (xterm-reconfigure xterm x-size y-size)
			    (if (not (and (= x-size (screen-x-size screen))
					  (= y-size (screen-y-size screen))))
				(begin
				  (set-screen-size! screen x-size y-size)
				  (update-screen! screen #t))))))
		      screen event)))

(define x-screen-ignore-focus-button? #f)

(define-event-handler event-type:focus-in
  (lambda (screen event)
    event
    (if x-screen-ignore-focus-button?
	(set! ignore-button-state 'IGNORE-BUTTON-DOWN))
    (and (not (selected-screen? screen))
	 (make-input-event 'SELECT-SCREEN
			   (lambda (screen)
			     (fluid-let ((last-focus-time #f))
			       (select-screen screen)))
			   screen))))

(define-event-handler event-type:delete-window
  (lambda (screen event)
    event
    (and (not (screen-deleted? screen))
	 (make-input-event 'DELETE-SCREEN delete-screen! screen))))

(define-event-handler event-type:map
  (lambda (screen event)
    event
    (and (not (screen-deleted? screen))
	 (begin
	   (set-screen-mapped?! screen #t)
	   (screen-force-update screen)
	   (make-input-event 'UPDATE update-screen! screen #f)))))

(define-event-handler event-type:unmap
  (lambda (screen event)
    event
    (if (not (screen-deleted? screen))
	(set-screen-mapped?! screen #f))
    #f))

(define-event-handler event-type:visibility
  (lambda (screen event)
    (and (not (screen-deleted? screen))
	 (let ((old-visibility (screen-x-visibility screen)))
	   (case (vector-ref event 2)
	     ((0) (set-screen-x-visibility! screen 'VISIBLE))
	     ((1) (set-screen-x-visibility! screen 'PARTIALLY-OBSCURED))
	     ((2) (set-screen-x-visibility! screen 'OBSCURED)))
	   (and (eq? old-visibility 'OBSCURED)
		(begin
		  (screen-force-update screen)
		  (make-input-event 'UPDATE update-screen! screen #f)))))))

(define-event-handler event-type:take-focus
  (lambda (screen event)
    (set! last-focus-time (vector-ref event 2))
    (make-input-event 'SELECT-SCREEN select-screen screen)))

;;;; Atoms

(define built-in-atoms
  '#(#F
     PRIMARY
     SECONDARY
     ARC
     ATOM
     BITMAP
     CARDINAL
     COLORMAP
     CURSOR
     CUT_BUFFER0
     CUT_BUFFER1
     CUT_BUFFER2
     CUT_BUFFER3
     CUT_BUFFER4
     CUT_BUFFER5
     CUT_BUFFER6
     CUT_BUFFER7
     DRAWABLE
     FONT
     INTEGER
     PIXMAP
     POINT
     RECTANGLE
     RESOURCE_MANAGER
     RGB_COLOR_MAP
     RGB_BEST_MAP
     RGB_BLUE_MAP
     RGB_DEFAULT_MAP
     RGB_GRAY_MAP
     RGB_GREEN_MAP
     RGB_RED_MAP
     STRING
     VISUALID
     WINDOW
     WM_COMMAND
     WM_HINTS
     WM_CLIENT_MACHINE
     WM_ICON_NAME
     WM_ICON_SIZE
     WM_NAME
     WM_NORMAL_HINTS
     WM_SIZE_HINTS
     WM_ZOOM_HINTS
     MIN_SPACE
     NORM_SPACE
     MAX_SPACE
     END_SPACE
     SUPERSCRIPT_X
     SUPERSCRIPT_Y
     SUBSCRIPT_X
     SUBSCRIPT_Y
     UNDERLINE_POSITION
     UNDERLINE_THICKNESS
     STRIKEOUT_ASCENT
     STRIKEOUT_DESCENT
     ITALIC_ANGLE
     X_HEIGHT
     QUAD_WIDTH
     WEIGHT
     POINT_SIZE
     RESOLUTION
     COPYRIGHT
     NOTICE
     FONT_NAME
     FAMILY_NAME
     FULL_NAME
     CAP_HEIGHT
     WM_CLASS
     WM_TRANSIENT_FOR))

(define (symbol->x-atom display name soft?)
  (or (hash-table-ref/default built-in-atoms-table name #f)
      (let ((table (car (display/cached-atoms-tables display))))
	(or (hash-table-ref/default table name #f)
	    (let ((atom
		   (x-intern-atom display
				  (string-upcase (symbol-name name))
				  soft?)))
	      (if (not (= atom 0))
		  (hash-table-set! table name atom))
	      atom)))))

(define (x-atom->symbol display atom)
  (if (< atom (vector-length built-in-atoms))
      (vector-ref built-in-atoms atom)
      (let ((table (cdr (display/cached-atoms-tables display))))
	(or (hash-table-ref/default table atom #f)
	    (let ((symbol
		   (let ((string (x-get-atom-name display atom)))
		     (if (not (string? string))
			 (error "X error (XGetAtomName):" string atom))
		     (intern string))))
	      (hash-table-set! table atom symbol)
	      symbol)))))

(define built-in-atoms-table
  (let ((n (vector-length built-in-atoms)))
    (let ((table (make-hash-table eq?)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(hash-table-set! table (vector-ref built-in-atoms i) i))
      table)))

(define display/cached-atoms-tables
  (let ((table (make-hash-table eq?)))
    (lambda (display)
      (or (hash-table-ref/default table display #f)
	  (let ((result (cons (make-hash-table eq?) (make-hash-table eqv?))))
	    (hash-table-set! table display result)
	    result)))))

;;;; Properties

(define (get-xterm-property xterm property type delete?)
  (get-window-property (x-window-display xterm)
		       (x-window-id xterm)
		       property
		       type
		       delete?))

(define (get-window-property display window property type delete?)
  (let ((property (symbol->x-atom display property #f))
	(type-atom (symbol->x-atom display type #f)))
    (let ((v (x-get-window-property display window property 0 0 #f type-atom)))
      (and v
	   (vector-ref v 3)
	   (let ((data
		  (get-window-property-1 display window property delete?
					 (vector-ref v 0)
					 (vector-ref v 1)
					 (vector-ref v 2))))
	     (if type
		 data
		 (cons (x-atom->symbol display (vector-ref v 0))
		       data)))))))

(define (get-window-property-1 display window property delete?
			       type format bytes)
  (let ((read-once
	 (lambda (offset bytes n delete?)
	   (let ((v
		  (x-get-window-property display window property
					 (quotient offset 4)
					 (integer-ceiling n 4)
					 delete? type)))
	     (if (not (and v
			   (= type (vector-ref v 0))
			   (= format (vector-ref v 1))
			   (= (- bytes n) (vector-ref v 2))
			   (vector-ref v 3)
			   (= n
			      (if (= format 8)
				  (string-length (vector-ref v 3))
				  (* (vector-length (vector-ref v 3))
				     (quotient format 8))))))
		 (error "Window property changed:" v))
	     (vector-ref v 3))))
	(qb (* (property-quantum display) 4)))
    (if (<= bytes qb)
	(read-once 0 bytes bytes delete?)
	(let ((b/w (quotient format 8)))
	  (let ((result
		 (if (= b/w 1)
		     (make-string bytes)
		     (make-vector (quotient bytes b/w))))
		(move!
		 (if (= b/w 1)
		     string-copy!
		     vector-copy!)))
	    (let loop ((offset 0) (bytes bytes))
	      (if (<= bytes qb)
		  (move! (read-once offset bytes bytes delete?)
			 0 (quotient bytes b/w)
			 result (quotient offset b/w))
		  (begin
		    (move! (read-once offset bytes qb #f) 0 (quotient qb b/w)
			   result (quotient offset b/w))
		    (loop (+ offset qb) (- bytes qb)))))
	    result)))))

(define (put-window-property display window property type format data)
  (let ((put-1
	 (let ((property (symbol->x-atom display property #f))
	       (type (symbol->x-atom display type #f)))
	   (lambda (mode data)
	     (let ((status
		    (x-change-property display window property type format
				       mode data)))
	       (cond ((= status x-status:success)
		      #t)
		     ((= status x-status:bad-alloc)
		      (x-delete-property display window property)
		      #f)
		     (else
		      (error "X error (XChangeProperty):" status)))))))
	(qw (property-quantum display))
	(i/w (quotient 32 format))
	(subpart (if (= format 8) substring subvector))
	(end (if (= format 8) (string-length data) (vector-length data)))
	(mode:replace 0)
	(mode:append 2))
    (let loop ((start 0) (nw (integer-ceiling end i/w)) (mode mode:replace))
      (if (<= nw qw)
	  (put-1 mode (if (= start 0) data (subpart data start end)))
	  (let ((end (+ start (* qw i/w))))
	    (and (put-1 mode (subpart data start end))
		 (loop end (- nw qw) mode:append)))))))

(define (property-quantum display)
  ;; The limit on the size of a property quantum is the maximum
  ;; request size less the size of the largest header needed.  The
  ;; relevant packets are the GetProperty reply packet (header size 8)
  ;; and the ChangeProperty request packet (header size 6).  The magic
  ;; number 8 is the larger of these two header sizes.
  (fix:- (x-max-request-size display) 8))

(define (delete-xterm-property xterm property)
  (delete-window-property (x-window-display xterm)
			  (x-window-id xterm)
			  property))

(define (delete-window-property display window property)
  (x-delete-property display window (symbol->x-atom display property #f)))

(define x-status:success		0)
(define x-status:bad-request		1)
(define x-status:bad-value		2)
(define x-status:bad-window		3)
(define x-status:bad-pixmap		4)
(define x-status:bad-atom		5)
(define x-status:bad-cursor		6)
(define x-status:bad-font		7)
(define x-status:bad-match		8)
(define x-status:bad-drawable	9)
(define x-status:bad-access		10)
(define x-status:bad-alloc		11)
(define x-status:bad-color		12)
(define x-status:bad-gc		13)
(define x-status:bad-id-choice	14)
(define x-status:bad-name		15)
(define x-status:bad-length		16)
(define x-status:bad-implementation	17)

;;;; Selection Source

(define-variable x-cut-to-clipboard
  "If true, cutting text copies to the clipboard.
In either case, it is copied to the primary selection."
  #t
  boolean?)

(define (os/interprogram-cut string context)
  (if (eq? x-display-type (current-display-type))
      (let ((xterm (screen-xterm (selected-screen))))
	(let ((own-selection
	       (lambda (selection)
		 (own-selection (x-window-display xterm)
				selection
				(x-window-id xterm)
				last-focus-time
				string))))
	  (own-selection 'PRIMARY)
	  (if (ref-variable x-cut-to-clipboard context)
	      (own-selection 'CLIPBOARD))))))

(define (own-selection display selection window time value)
  (and (eqv? window
	     (let ((selection (symbol->x-atom display selection #f)))
	       (x-set-selection-owner display selection window time)
	       (x-get-selection-owner display selection)))
       (begin
	 (hash-table-set! (display/selection-records display)
			  selection
			  (make-selection-record window time value))
	 #t)))

(define display/selection-records
  (let ((table (make-hash-table eq?)))
    (lambda (display)
      (or (hash-table-ref/default table display #f)
	  (let ((result (make-hash-table eq?)))
	    (hash-table-set! table display result)
	    result)))))

;;; In the next two procedures, we must allow TIME to be 0, even
;;; though the ICCCM forbids this, because existing clients use that
;;; value.  An example of a broken client is GTK+ version 1.2.6.

(define (display/selection-record display name time)
  (let ((record (hash-table-ref/default (display/selection-records display) name #f)))
    (and record
	 (or (= 0 time) (<= (selection-record/time record) time))
	 record)))

(define (display/delete-selection-record! display name time)
  (let ((records (display/selection-records display)))
    (if (let ((record (hash-table-ref/default records name #f)))
	  (and record
	       (or (= 0 time) (<= (selection-record/time record) time))))
	(hash-table-delete! records name))))

(define-structure (selection-record (conc-name selection-record/))
  (window #f read-only #t)
  (time #f read-only #t)
  (value #f read-only #t))

(define-event-handler event-type:selection-request
  (lambda (screen event)
    screen
    (let ((display x-display-data))
      (let ((requestor (selection-request/requestor event))
	    (selection
	     (x-atom->symbol display (selection-request/selection event)))
	    (target
	     (x-atom->symbol display (selection-request/target event)))
	    (property
	     (x-atom->symbol display (selection-request/property event)))
	    (time (selection-request/time event)))
	(let ((reply
	       (lambda (property)
		 (x-send-selection-notify display
					  requestor
					  (selection-request/selection event)
					  (selection-request/target event)
					  (symbol->x-atom display property #f)
					  time)
		 (x-display-flush display))))
	  (if (let ((record (display/selection-record display selection time)))
		(and record
		     property
		     (process-selection-request display requestor property
						target time record #f)))
	      (reply property)
	      (reply #f)))))
    #f))

(define-structure (selection-request (type vector)
				     (initial-offset 2)
				     (conc-name selection-request/))
  (requestor #f read-only #t)
  (selection #f read-only #t)
  (target #f read-only #t)
  (property #f read-only #t)
  (time #f read-only #t))

(define-event-handler event-type:selection-clear
  (lambda (screen event)
    screen
    (let ((display x-display-data))
      (display/delete-selection-record!
       display
       (x-atom->symbol display (selection-clear/selection event))
       (selection-clear/time event)))
    #f))

(define-structure (selection-clear (type vector)
				   (initial-offset 2)
				   (conc-name selection-clear/))
  (selection #f read-only #t)
  (time #f read-only #t))

(define (process-selection-request display requestor property target time
				   record multiple?)
  (let ((win
	 (lambda (format data)
	   (and (put-window-property display requestor property target format
				     data)
		target))))
    (case target
      ((STRING)
       (win 8 (selection-record/value record)))
      ((TARGETS)
       (win 32 (atoms->property-data '(STRING TIMESTAMP) display)))
      ((TIMESTAMP)
       (win 32 (timestamp->property-data (selection-record/time record))))
      ((MULTIPLE)
       (and multiple?
	    (let ((alist
		   (property-data->atom-alist
		    (or (get-window-property display requestor property
					     'MULTIPLE #f)
			(error "Missing MULTIPLE property:" property))
		    display)))
	      (for-each (lambda (entry)
			  (set-car! entry
				    (process-selection-request display
							       requestor
							       (cdr entry)
							       (car entry)
							       time
							       record
							       #t)))
			alist)
	      (win 32 (atom-alist->property-data alist display)))))
      (else #f))))

(define (atoms->property-data names display)
  (list->vector (map (lambda (name) (symbol->x-atom display name #f)) names)))

(define (timestamp->property-data time)
  (vector time))

(define (property-data->atom-alist data display)
  (if (not (even? (vector-length data)))
      (error:bad-range-argument data 'PROPERTY-DATA->ATOM-ALIST))
  (let loop ((atoms
	      (map (lambda (atom) (x-atom->symbol display atom))
		   (vector->list data))))
    (if (null? atoms)
	'()
	(cons (cons (car atoms) (cadr atoms))
	      (loop (cddr atoms))))))

(define (atom-alist->property-data alist display)
  (atoms->property-data (let loop ((alist alist))
			  (if (null? alist)
			      '()
			      (cons (caar alist)
				    (cons (cdar alist)
					  (loop (cdr alist))))))
			display))

;;;; Selection Sink

(define-variable x-paste-from-clipboard
  "If true, pasting text copies from the clipboard.
Otherwise, it is copied from the primary selection."
  #t
  boolean?)

(define (os/interprogram-paste context)
  (and (eq? x-display-type (current-display-type))
       (xterm/interprogram-paste (screen-xterm (selected-screen)) context)))

(define (xterm/interprogram-paste xterm context)
  (or (and (ref-variable x-paste-from-clipboard context)
	   (xterm/interprogram-paste-1 xterm 'CLIPBOARD))
      (xterm/interprogram-paste-1 xterm 'PRIMARY)))

(define (xterm/interprogram-paste-1 xterm selection)
  (with-thread-events-blocked
   (lambda ()
     (let ((property '_EDWIN_TMP_)
	   (time last-focus-time))
       (cond ((display/selection-record (x-window-display xterm)
					selection time)
	      => selection-record/value)
	     ((request-selection xterm selection 'STRING property time)
	      (receive-selection xterm property 'STRING time))
	     ((request-selection xterm selection 'C_STRING property time)
	      (receive-selection xterm property 'C_STRING time))
	     (else #f))))))

(define (request-selection xterm selection target property time)
  (let ((display (x-window-display xterm))
	(window (x-window-id xterm)))
    (let ((selection (symbol->x-atom display selection #f))
	  (target (symbol->x-atom display target #f))
	  (property (symbol->x-atom display property #f)))
      (x-delete-property display window property)
      (x-convert-selection display selection target property window time)
      (x-display-flush display)
      (eq? 'REQUEST-GRANTED
	   (wait-for-event x-selection-timeout
	     (lambda (event)
	       (fix:= event-type:selection-notify (vector-ref event 0)))
	     (lambda (event)
	       (and (= window (selection-notify/requestor event))
		    (= selection (selection-notify/selection event))
		    (= target (selection-notify/target event))
		    (= time (selection-notify/time event))
		    (if (= property (selection-notify/property event))
			'REQUEST-GRANTED
			'REQUEST-DENIED))))))))

(define-structure (selection-notify (type vector)
				    (initial-offset 2)
				    (conc-name selection-notify/))
  (requestor #f read-only #t)
  (selection #f read-only #t)
  (target #f read-only #t)
  (property #f read-only #t)
  (time #f read-only #t))

(define (receive-selection xterm property target time)
  (let ((value (get-xterm-property xterm property #f #t)))
    (if (not value)
	(error "Missing selection value."))
    (if (eq? 'INCR (car value))
	(receive-incremental-selection xterm property target time)
	(and (eq? target (car value))
	     (cdr value)))))

(define (receive-incremental-selection xterm property target time)
  ;; I have been unable to get this to work, after a day of hacking,
  ;; and I don't have any idea why it won't work.  Given that this
  ;; will only be used for selections of size exceeding ~230kb, I'm
  ;; going to leave it broken.  -- cph
  (x-window-flush xterm)
  (let loop ((time time) (accum '()))
    (let ((time
	   (wait-for-window-property-change xterm property time
					    x-property-state:new-value)))
      (if (not time)
	  (error "Timeout waiting for PROPERTY-NOTIFY event."))
      (let ((value (get-xterm-property xterm property target #t)))
	(if (not value)
	    (error "Missing property after PROPERTY-NOTIFY event."))
	(if (string-null? value)
	    (apply string-append (reverse! accum))
	    (loop time (cons value accum)))))))

(define (wait-for-window-property-change xterm property time state)
  (wait-for-event x-selection-timeout
    (lambda (event)
      (fix:= event-type:property-notify (vector-ref event 0)))
    (let ((property (symbol->x-atom (x-window-display xterm) property #f))
	  (window (x-window-id xterm)))
      (lambda (event)
	(and (= window (property-notify/window event))
	     (= property (property-notify/property event))
	     (< time (property-notify/time event))
	     (= state (property-notify/state event))
	     (property-notify/time event))))))

(define-structure (property-notify (type vector)
				   (initial-offset 2)
				   (conc-name property-notify/))
  (window #f read-only #t)
  (property #f read-only #t)
  (time #f read-only #t)
  (state #f read-only #t))

(define x-property-state:new-value 0)
(define x-property-state:delete 1)

(define x-selection-timeout 5000)

;;;; Initialization

(define reading-event?)
(define signal-interrupts?)
(define last-focus-time)
(define previewer-registration)
(define ignore-button-state)

(define (with-editor-interrupts-from-x receiver)
  (fluid-let ((reading-event? #f)
	      (signal-interrupts? #t)
	      (last-focus-time #f)
	      (previewer-registration)
	      (ignore-button-state #f))
    (dynamic-wind
     preview-event-stream
     (lambda () (receiver (lambda (thunk) (thunk)) '()))
     (lambda ()
       (deregister-io-thread-event previewer-registration)))))

(define (with-x-interrupts-enabled thunk)
  (with-signal-interrupts #t thunk))

(define (with-x-interrupts-disabled thunk)
  (with-signal-interrupts #f thunk))

(define (with-signal-interrupts enabled? thunk)
  (let ((old))
    (dynamic-wind (lambda ()
		    (set! old signal-interrupts?)
		    (set! signal-interrupts? enabled?)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! enabled? signal-interrupts?)
		    (set! signal-interrupts? old)
		    unspecific))))

(define (signal-interrupt!)
  (editor-beep)
  (temporary-message "Quit")
  (^G-signal))

(define x-display-type)
(define x-display-data)
(define x-display-events)
(define x-display-name #f)

(define (get-x-display)
  ;; X-OPEN-DISPLAY hangs, uninterruptibly, when the X server is
  ;; running the login loop of xdm.  Can this be fixed?
  (or x-display-data
      (and (implemented-primitive-procedure? x-open-display)
	   (or x-display-name (get-environment-variable "DISPLAY"))
	   (let ((display (x-open-display x-display-name)))
	     (set! x-display-data display)
	     (set! x-display-events (make-queue))
	     display))))

(define (initialize-package!)
  (set! screen-list '())
  (set! x-display-type
	(make-display-type 'X
			   #t
			   get-x-display
			   make-xterm-screen
			   (lambda (screen)
			     screen	;ignore
			     (get-xterm-input-operations))
			   with-editor-interrupts-from-x
			   with-x-interrupts-enabled
			   with-x-interrupts-disabled))
  (set! x-display-data #f)
  (set! x-display-events)
  unspecific)