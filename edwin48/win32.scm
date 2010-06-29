#| -*-Scheme-*-

$Id: win32.scm,v 1.22 2008/01/30 20:02:07 cph Exp $

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

;;;; Win32 Terminal
;;; package: (edwin screen win32)


(define-primitives
  (win32-read-event 0)
  (win32-screen-char-dimensions 1)
  (win32-screen-clear-rectangle! 6)
  (win32-screen-create! 2)
  (win32-screen-invalidate-rect! 5)
  (win32-screen-move-cursor! 3)
  (win32-screen-set-background-color! 2)
  (win32-screen-set-default-font! 1)
  (win32-screen-set-font! 2)
  (win32-screen-set-foreground-color! 2)
  (win32-screen-set-icon! 2)
  (win32-screen-show-cursor! 2)
  (win32-screen-size 1)
  (win32-screen-vertical-scroll! 6)
  (win32-screen-write-char! 5)
  (win32-screen-write-substring! 7))

;;(define (debug . details)
;;  (pp details console-output-port))

(define-structure (win32-screen-state
		   (constructor make-win32-screen-state (handle))
		   (conc-name state/))
  (handle #f read-only #t)
  (cursor-x -1) ; cached position, -1 if we dont know
  (cursor-y -1) ; ditto
  ;; This rect is the bounding box of a sequence of updates.  RECT-TOP is #F
  ;; if no box has been established, which implies that the screen needs no
  ;; update.
  (rect-top #f)
  (rect-bottom 0)
  (rect-right 0)
  (rect-left 0)
  (redisplay-title? #f)
  (name #f))

(define (screen-redisplay-title? screen)
  (state/redisplay-title? (screen-state screen)))

(define (set-screen-redisplay-title?! screen flag)
  (set-state/redisplay-title?! (screen-state screen) flag))

(define (make-win32-screen)
  (let* ((window (win32-screen-create! 0 win32-screen-features-mask))
	 (icon   (load-icon (get-handle 0) "EDWIN_ICON"))
	 (width.height (win32-screen-size window)))
    (set-window-text window "Edwin")
    (win32-screen-set-icon! window icon)
    ;; The first time (re)entering edwin we make the master tty window iconic:
    (if (null? win32-screens)
	(show-window (get-handle 1) SW_SHOWMINNOACTIVE))
    (let ((screen
	   (make-screen (make-win32-screen-state window)
			win32-screen/beep
			win32-screen/clear-line!
			win32-screen/clear-rectangle!
			win32-screen/clear-screen!
			win32-screen/discard!
			win32-screen/enter!
			win32-screen/exit!
			win32-screen/flush!
			win32-screen/modeline-event!
			#f
			win32-screen/scroll-lines-down!
			win32-screen/scroll-lines-up!
			win32-screen/wrap-update!
			win32-screen/write-char!
			win32-screen/write-cursor!
			win32-screen/write-substring!
			8
			(car width.height)
			(cdr width.height))))
      (set! win32-screens (cons screen win32-screens))
      ;;(debug 'CREATE screen)
      screen)))

(define (win32-screen/beep screen)
  screen
  (message-beep MB_OK))

(define (expand-rect screen top bottom left right)
  (define (min u v)  (if (fix:< u v) u v))
  (define (max u v)  (if (fix:> u v) u v))
  (define (set-rect! state top bottom left right)
    (set-state/rect-top!    state top)
    (set-state/rect-bottom! state bottom)
    (set-state/rect-left!   state left)
    (set-state/rect-right!  state right))

  (let ((state  (screen-state screen)))
    (if (state/rect-top state)
	(set-rect! state
		   (min top    (state/rect-top state))
		   (max bottom (state/rect-bottom state))
		   (min left   (state/rect-left state))
		   (max right  (state/rect-right state)))
	(set-rect! state top bottom left right))))

(define (invalidate-invalid-region! screen)
  (let ((state  (screen-state screen)))
    (if (state/rect-top state)
	(begin
	  (win32-screen-invalidate-rect!
	   (screen->handle screen)
	   (state/rect-top state)
	   (fix:+ (state/rect-bottom state) 1)
	   (state/rect-left state)
	   (fix:+ (state/rect-right state) 1))))))

(define (set-screen-cursor-position! screen x y)
  (set-state/cursor-x! (screen-state screen) x)
  (set-state/cursor-y! (screen-state screen) y))

(define (win32-screen/clear-line! screen x y first-unused-x)
  (win32-screen-clear-rectangle! (screen->handle screen)
				 x first-unused-x y (fix:1+ y)
				 0))

(define (win32-screen/clear-rectangle! screen xl xu yl yu highlight)
  (win32-screen-clear-rectangle! (screen->handle screen)
				 xl xu yl yu
				 (if highlight 1 0)))

(define (win32-screen/clear-screen! screen)
  (let* ((handle  (screen->handle screen))
	 (w.h     (win32-screen-size handle)))
    (win32-screen-clear-rectangle! handle 0 (car w.h) 0 (cdr w.h)  0)))

(define (win32-screen/discard! screen)
  ;;(debug 'DISCARD screen)
  (destroy-window (screen->handle screen))
  (set! win32-screens (delete screen win32-screens eq?)))

(define (win32-screen/enter! screen)
  (set-screen-cursor-position! screen -1 -1)
  (set-active-window (screen->handle screen))
  (win32-screen-show-cursor! (screen->handle screen) #T))

(define (win32-screen/exit! screen)
  (win32-screen-show-cursor! (screen->handle screen) #F))

(define (win32-screen/modeline-event! screen window type)
  window type				; ignored
  (set-screen-redisplay-title?! screen #t))

(define (win32-screen/scroll-lines-down! screen xl xu yl yu amount)
  (and #f
       (win32-screen-vertical-scroll! (screen->handle screen)
				      xl xu yl yu (fix:+ yl amount))))

(define (win32-screen/scroll-lines-up! screen xl xu yl yu amount)
  (and #f
       (win32-screen-vertical-scroll! (screen->handle screen)
				      xl xu amount yu 0)
       (win32-screen-vertical-scroll! (screen->handle screen)
				      xl xu yl yu (fix:- yl amount))))

(define (win32-screen/flush! screen)
  ;; Win32 API call causes any pending painting to be done
  (update-window (screen->handle screen))
  #F)

(define (win32-screen/wrap-update! screen thunk)
  (let ((finished? #f))
    (dynamic-wind
     (lambda ()
       (set-state/rect-top! (screen-state screen) #F))
     (lambda ()
       (let ((result (thunk)))
	 (set! finished? result)
	 result))
     (lambda ()
       ;; invalidate the region that this update affected, and then flush
       (invalidate-invalid-region! screen)
       (if (and finished? (screen-redisplay-title? screen))
	   (begin
	     (update-win32-screen-name! screen)
	     (set-screen-redisplay-title?! screen #f)))
       (win32-screen/flush! screen)))))

(define (win32-screen/write-char! screen x y char highlight)
  (win32-screen-write-char! (screen->handle screen) x y
			    (char->ascii char)
			    (if highlight 1 0))
  (if (char-graphic? char)
      (set-screen-cursor-position! screen (fix:+ x 1) y)
      (set-screen-cursor-position! screen -1 -1)))

(define (win32-screen/write-substring! screen x y string start end highlight)
  ;;(debug 'substring x y string start end)
  (win32-screen-write-substring!
   (screen->handle screen) x y string start end
   (if highlight 1 0))
  (expand-rect screen x (fix:+ x (fix:- end start)) y y))

(define (win32-screen/write-cursor! screen x y)
  (let ((state  (screen-state screen)))
    (if (or (not (fix:= (state/cursor-x state) x))
	    (not (fix:= (state/cursor-y state) y)))
	(let ((handle  (screen->handle screen)))
	  (win32-screen-move-cursor! handle x y)
	  (set-screen-cursor-position! screen x y)))))

(define (screen->handle screen)
  (if (memq screen win32-screens)
      (state/handle (screen-state screen))
      (error "Screen has unexpectedly vanished" screen)))

(define (handle->win32-screen handle)
  (find (lambda (screen) (eqv? handle (state/handle (screen-state screen))))
	win32-screens))

(define (screen-name screen)
  (state/name (screen-state screen)))

(define (set-screen-name! screen name)
  (set-state/name! (screen-state screen) name))

(define (win32-screen/set-name! screen name)
  (let ((name* (screen-name screen)))
    (if (or (not name*) (not (string=? name name*)))
	(begin
	  (set-screen-name! screen name)
	  (set-window-text (screen->handle screen) name)))))

(define (win32-screen/set-font! screen font)
  (let ((x-size (screen-x-size screen))
	(y-size (screen-y-size screen)))
    (win32-screen-set-font! (screen->handle screen) font)
    (win32-screen/set-size! screen x-size y-size)))

(define (win32-screen/set-icon! screen icon)
  (win32-screen-set-icon! (screen->handle screen) icon))

(define (win32-screen/set-foreground-color! screen color)
  (win32-screen-set-foreground-color! (screen->handle screen) color))

(define (win32-screen/set-background-color! screen color)
  (win32-screen-set-background-color! (screen->handle screen) color))

(define (win32-screen/set-size! screen width height)
  (let ((handle (screen->handle screen)))
    (let ((rect
	   (let ((x.y (win32-screen-char-dimensions handle)))
	     (make-rect 0 0 (* width (car x.y)) (* height (cdr x.y))))))
      (adjust-window-rect rect
			  WS_OVERLAPPEDWINDOW
			  (not (= 0 (get-menu handle))))
      (set-window-pos handle 0 0 0
		      (- (rect/right rect) (rect/left rect))
		      (- (rect/bottom rect) (rect/top rect))
		      (+ SWP_NOMOVE SWP_NOZORDER)))))

(define (win32-screen/set-position! screen x y)
  (set-window-pos (screen->handle screen) 0 x y 0 0
		  (+ SWP_NOSIZE SWP_NOZORDER)))

(define (win32-screen/get-position screen)
  (let ((rect (make-rect 0 0 0 0)))
    (get-window-rect (screen->handle screen) rect)
    (values (rect/left rect) (rect/top rect)
	    (rect/right rect) (rect/bottom rect))))

(define (win32-screen/get-client-size screen)
  (let ((rect (make-rect 0 0 0 0)))
    (get-client-rect (screen->handle screen) rect)
    (values (rect/right rect) (rect/bottom rect))))

(define win32-screens)
(define win32-display-type)
(define win32-event-queue)
(define signal-interrupts?)
(define reading-event?)
(define previewer-registration)

(define (win32-screen-available?)
  (implemented-primitive-procedure? win32-screen-create!))

(define (initialize-package!)
  (set! win32-screens '())
  (set! win32-display-type
	(make-display-type 'win32
			   #t		; multiple screens?
			   win32-screen-available?
			   (lambda geometry
			     geometry
			     (if (not win32-event-queue)
				 (set! win32-event-queue (make-queue)))
			     (make-win32-screen))
			   get-win32-input-operations
			   with-editor-interrupts-from-win32
			   with-win32-interrupts-enabled
			   with-win32-interrupts-disabled))
  (set! win32-event-queue #f)
  (add-event-receiver! event:before-exit
		       (lambda () (for-each screen-discard! win32-screens))))

(define (with-editor-interrupts-from-win32 receiver)
  (fluid-let ((reading-event? #f)
	      (signal-interrupts? #t)
	      (previewer-registration))
    (dynamic-wind
     (lambda () (preview-event-stream))
     (lambda () (receiver (lambda (thunk) (thunk)) '()))
     (lambda () (deregister-io-thread-event previewer-registration)))))

(define (with-win32-interrupts-enabled thunk)
  (with-signal-interrupts #t thunk))

(define (with-win32-interrupts-disabled thunk)
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

(define (get-win32-input-operations screen)
  screen ; ignored
  (let ((get-next-event
	 (lambda (block?)
	   (let loop ()
	     (let ((event (read-event block?)))
	       (and event
		    (or (process-event event)
			(loop)))))))
	(pending-result #f))
    (let ((probe
	   (lambda (block?)
	     (let ((result (get-next-event block?)))
	       (if result
		   (set! pending-result result))
	       result))))
      (values (lambda ()		;halt-update?
		(or pending-result
		    (probe 'IN-UPDATE)))
	      (lambda ()		;peek-no-hang
		(or pending-result
		    (probe #f)))
	      (lambda ()		;peek
		(or pending-result
		    (let ((result (get-next-event #t)))
		      (set! pending-result result)
		      result)))
	      (lambda ()		;read
		(cond (pending-result
		       => (lambda (result)
			    (set! pending-result #f)
			    result))
		      (else
		       (get-next-event #t))))))))

(define (process-event event)
  (cond ((fix:fixnum? event)
	 (let ((flag (process-change-event event)))
	   (and flag
		(make-input-event (if (eq? flag 'FORCE-RETURN) 'RETURN 'UPDATE)
				  update-screens! #f))))
	((vector? event) (process-special-event event))
	((input-event? event) event)
	(else #f)))

(define (process-change-event event)
  (cond ((fix:= event:process-output event) (accept-process-output))
	((fix:= event:process-status event) (handle-process-status-changes))
	((fix:= event:inferior-thread-output event) (accept-thread-output))
	(else (error "Illegal change event:" event))))

(define (process-special-event event)
  (let ((handler (hash-table-ref/default event-handlers (event-type event) #f))
	(screen (handle->win32-screen (event-handle event))))
    (and handler
	 screen
	 (handler screen event))))

(define event-handlers
  (make-hash-table eq?))

(define (define-event-handler event-type handler)
  (hash-table-set! event-handlers event-type handler))

;;;; Events

(define (read-event block?)
  (let ((queue win32-event-queue))
    (let loop ()
      (set! reading-event? #t)
      (let ((event
	     (if (queue-empty? queue)
		 (if (eq? 'IN-UPDATE block?)
		     (read-event-2)
		     (read-event-1 block?))
		 (dequeue!/unsafe queue))))
	(set! reading-event? #f)
	event))))

(define (read-event-1 block?)
  (or (read-event-2)
      (let loop ()
	(let ((mask (set-interrupt-enables! interrupt-mask/gc+win32)))
	  (cond (inferior-thread-changes?
		 (set-interrupt-enables! mask)
		 event:inferior-thread-output)
		((process-output-available?)
		 (set-interrupt-enables! mask)
		 event:process-output)
		((process-status-changes?)
		 (set-interrupt-enables! mask)
		 event:process-status)
		(else
		 (let ((flag
			(test-for-io-on-descriptor
			 ;; console-channel-descriptor here
			 ;; means "input from message queue".
			 console-channel-descriptor block? 'READ)))
		   (set-interrupt-enables! mask)
		   (case flag
		     ((#F) #f)
		     ((PROCESS-STATUS-CHANGE) event:process-status)
		     ((INTERRUPT) (loop))
		     (else (read-event-1 block?))))))))))

(define (read-event-2)
  (let ((mask (set-interrupt-enables! interrupt-mask/gc+win32)))
    (let ((result (win32-read-event)))
      (set-interrupt-enables! mask)
      result)))

(define interrupt-mask/gc+win32
  ;; Include INTERRUPT-BIT/GLOBAL-1 so that messages are dispatched to
  ;; the screen by the interrupt-handler.
  ;;(fix:or interrupt-mask/gc-ok interrupt-bit/global-1)
  15)

(define (preview-event-stream)
  (set! previewer-registration
	(permanently-register-io-thread-event
	 console-channel-descriptor
	 'READ
	 (current-thread)
	 (lambda (mode)
	   mode
	   (if (not reading-event?)
	       (let ((event (read-event-2)))
		 (if event
		     (preview-event event)))))))
  unspecific)

(define (preview-event event)
  (cond ((and signal-interrupts?
	      (vector? event)
	      (fix:= event-type:key (event-type event))
	      (eqv? #\C-g (decode-key-event event)))
	 (clean-event-queue win32-event-queue)
	 (signal-interrupt!))
	(else
	 (enqueue!/unsafe win32-event-queue event))))

(define (clean-event-queue queue)
  ;; Flush keyboard and mouse events from the input queue.  Other
  ;; events are harmless and must be processed regardless.
  (do ((events (let loop ()
		 (if (queue-empty? queue)
		     '()
		     (let ((event (dequeue!/unsafe queue)))
		       (if (and (vector? event)
				(let ((type (event-type event)))
				  (or (fix:= event-type:mouse type)
				      (fix:= event-type:key type))))
			   (loop)
			   (cons event (loop))))))
	       (cdr events)))
      ((null? events))
    (enqueue!/unsafe queue (car events))))

;; Mask bits:  VK coded special keys, Edwin mode,
;;    mouse, key, resize, close, focus, and visibility events
(define win32-screen-features-mask #x14003F)

(define event:process-output 16)
(define event:process-status 32)
(define event:inferior-thread-output 64)

(define event-type:resize		#x001)
(define event-type:key		#x002)
(define event-type:mouse		#x004)
(define event-type:close		#x008)
(define event-type:focus		#x010)
(define event-type:visibility	#x020)

(define control-key:alt-pressed	#x001)
(define control-key:control-pressed	#x002)
(define control-key:shift-pressed	#x004)

(define button-state:left-pressed	#x001)
(define button-state:right-pressed	#x002)
(define button-state:middle-pressed	#x004)

(define (some-bits? mask item)
  (not (fix:= 0 (fix:and mask item))))

(define (event-type event) (vector-ref event 0))
(define (event-handle event) (vector-ref event 1))

(define-structure (resize-event (type vector)
				(initial-offset 2)
				(conc-name resize-event/))
  (rows #f read-only #t)
  (columns #f read-only #t))

(define-structure (key-event (type vector)
			     (initial-offset 2)
			     (conc-name key-event/))
  (repeat-count #f read-only #t)
  (virtual-keycode #f read-only #t)
  (virtual-scancode #f read-only #t)
  (control-key-state #f read-only #t)
  (character #f read-only #t)
  (key-down? #f read-only #t))

(define-structure (mouse-event (type vector)
			       (initial-offset 2)
			       (conc-name mouse-event/))
  (row #f read-only #t)
  (column #f read-only #t)
  (control-key-state #f read-only #t)
  (button-state #f read-only #t)
  (up? #f read-only #t)
  (mouse-moved? #f read-only #t)
  (double-click? #f read-only #t))

(define-structure (focus-event (type vector)
			       (initial-offset 2)
			       (conc-name focus-event/))
  (gained? #f read-only #t))

(define-structure (visibility-event (type vector)
				    (initial-offset 2)
				    (conc-name visibility-event/))
  (show? #f read-only #t))

(define-event-handler event-type:resize
  (lambda (screen event)
    event
    (make-input-event 'SET-SCREEN-SIZE
		      (lambda (screen)
			(let ((w.h
			       (win32-screen-size (screen->handle screen))))
			  (if (not (and (= (car w.h) (screen-x-size screen))
					(= (cdr w.h) (screen-y-size screen))))
			      (begin
				(set-screen-size! screen (car w.h) (cdr w.h))
				(update-screen! screen #t)))))
		      screen)))

(define-event-handler event-type:key
  (lambda (screen event)
    screen
    (let ((key (decode-key-event event)))
      (if (and signal-interrupts? (eqv? key #\C-g))
	  (begin
	    (signal-interrupt!)
	    #f)
	  key))))

(define (decode-key-event event)
  (let ((key (key-event/character event))
	(state (key-event/control-key-state event)))
    (let ((bits (control-keys->bits state)))
      (if (fix:= key -1)
	  (win32-make-special-key
	   (key-event/virtual-keycode event)
	   (fix:or (if (some-bits? control-key:shift-pressed state) #x4 #x0)
		   bits))
	  (merge-bucky-bits (ascii->char key) bits)))))

(define (control-keys->bits state)
  (fix:or (if (some-bits? control-key:alt-pressed state)     #x1 #x0)
	  (if (some-bits? control-key:control-pressed state) #x2 #x0)))

(define-event-handler event-type:mouse
  (lambda (screen event)
    (make-input-event
     'BUTTON
     execute-button-command
     screen
     ((if (mouse-event/up? event)
	  make-up-button
	  make-down-button)
      (let ((state (mouse-event/button-state event)))
	(cond ((some-bits? button-state:left-pressed state) 0)
	      ((some-bits? button-state:right-pressed state) 2)
	      ((some-bits? button-state:middle-pressed state) 1)
	      (else 0)))
      (control-keys->bits (mouse-event/control-key-state event)))
     (mouse-event/column event)
     (mouse-event/row event))))

(define-event-handler event-type:close
  (lambda (screen event)
    event
    (cond ((screen-deleted? screen)
	   #f)
	  ((= (length win32-screens) 1)
	   (make-input-event 'EXIT
			     save-buffers-and-exit #f "Scheme" exit-scheme))
	  (else
	   (make-input-event 'DELETE-SCREEN delete-screen! screen)))))

(define-event-handler event-type:focus
  (lambda (screen event)
    (and (focus-event/gained? event)
	 (not (selected-screen? screen))
	 (make-input-event 'SELECT-SCREEN select-screen screen))))

(define-event-handler event-type:visibility
  (lambda (screen event)
    (and (not (screen-deleted? screen))
	 (if (visibility-event/show? event)
	     (begin
	       (screen-force-update screen)
	       (make-input-event 'UPDATE update-screen! screen #f))
	     (and (selected-screen? screen)
		  (let ((screen (other-screen screen)))
		    (and screen
			 (make-input-event 'SELECT-SCREEN
					   select-screen
					   screen))))))))