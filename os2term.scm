#| -*-Scheme-*-

$Id: os2term.scm,v 1.29 2007/01/05 21:19:24 cph Exp $

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

;;;; OS/2 Presentation Manager Interface
;;; Package: (edwin screen os2-screen)


(define os2-display-type)
(define screen-list)
(define event-queue)
(define event-descriptor)
(define virtual-key-table)
(define signal-interrupts?)
(define previewer-registration)
(define reading-event?)
(define desktop-width)
(define desktop-height)
(define hide-console?)
(define edwin-screen-icon)

(define (initialize-package!)
  (set! os2-display-type
	(make-display-type 'PM
			   #t
			   (lambda () (initialize-pm-state) #t)
			   make-os2-screen
			   get-os2-input-operations
			   with-editor-interrupts-from-os2
			   with-os2-interrupts-enabled
			   with-os2-interrupts-disabled))
  (set! virtual-key-table (make-virtual-key-table))
  (set! event-descriptor #f)
  (add-event-receiver! event:before-exit finalize-pm-state))

(define (initialize-pm-state)
  (if (not event-descriptor)
      (begin
	(set! screen-list '())
	(set! event-queue (make-queue))
	(set! event-descriptor (os2win-open-event-qid))
	(set! desktop-width (os2win-desktop-width))
	(set! desktop-height (os2win-desktop-height))
	(set! hide-console? #t)
	(set! edwin-screen-icon
	      (os2win-load-pointer HWND_DESKTOP NULLHANDLE IDI_EDWIN))
	unspecific)))

(define (finalize-pm-state)
  (if event-descriptor
      (begin
	(os2win-destroy-pointer edwin-screen-icon)
	(set! edwin-screen-icon)
	(do () ((null? screen-list)) (os2-screen/discard! (car screen-list)))
	(set! event-queue)
	(os2win-close-event-qid event-descriptor)
	(set! event-descriptor #f)
	unspecific)))

(define (with-editor-interrupts-from-os2 receiver)
  (fluid-let ((reading-event? #f)
	      (signal-interrupts? #t)
	      (previewer-registration))
    (dynamic-wind (lambda ()
		    (preview-event-stream)
		    (if hide-console?
			(begin
			  (set! hide-console? #f)
			  (os2win-set-state (os2win-console-wid)
					    window-state:hide))))
		  (lambda ()
		    (receiver (lambda (thunk) (thunk)) '()))
		  (lambda ()
		    (deregister-io-thread-event previewer-registration)))))

(define (with-os2-interrupts-enabled thunk)
  (with-signal-interrupts #t thunk))

(define (with-os2-interrupts-disabled thunk)
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

(define (make-os2-screen)
  (call-with-values open-window
    (lambda (state x-size y-size)
      (let ((screen
	     (make-screen state
			  os2-screen/beep
			  os2-screen/clear-line!
			  os2-screen/clear-rectangle!
			  os2-screen/clear-screen!
			  os2-screen/discard!
			  os2-screen/enter!
			  os2-screen/exit!
			  os2-screen/flush!
			  os2-screen/modeline-event!
			  #f
			  os2-screen/scroll-lines-down!
			  os2-screen/scroll-lines-up!
			  os2-screen/wrap-update!
			  os2-screen/write-char!
			  os2-screen/write-cursor!
			  os2-screen/write-substring!
			  8
			  x-size
			  y-size)))
	(set! screen-list (cons screen screen-list))
	screen))))

(define (open-window)
  (let ((wid (os2win-open event-descriptor "Edwin")))
    (os2win-set-icon wid edwin-screen-icon)
    (let ((metrics
	   (if current-font
	       (let ((metrics (set-normal-font! wid current-font)))
		 (if (not metrics)
		     (error "Unknown font name:" current-font))
		 metrics)
	       (let loop ((fonts initial-font-list))
		 (if (null? fonts)
		     (error "Unable to find usable font:" initial-font-list))
		 (let ((metrics (set-normal-font! wid (car fonts))))
		   (if metrics
		       (begin
			 (set! current-font (car fonts))
			 metrics)
		       (loop (cdr fonts))))))))
      (os2ps-set-colors (os2win-ps wid)
			(face-foreground-color normal-face)
			(face-background-color normal-face))
      (os2win-show-cursor wid #t)
      (os2win-show wid #t)
      (os2win-activate wid)
      (let ((w.h (os2win-get-size wid)))
	(let ((x-size (fix:quotient (car w.h) (font-metrics/width metrics)))
	      (y-size (fix:quotient (cdr w.h) (font-metrics/height metrics))))
	  (let ((size (fix:* x-size y-size)))
	    (values (make-screen-state wid
				       metrics
				       (car w.h)
				       (cdr w.h)
				       (make-string size #\space)
				       (make-vector size normal-face))
		    x-size
		    y-size)))))))

(define (os2-screen/beep screen)
  screen
  (os2win-alarm WA_ERROR))

(define (os2-screen/clear-line! screen x y first-unused-x)
  (let ((start (screen-char-index screen x y))
	(end (screen-char-index screen first-unused-x y))
	(face (screen-normal-face screen)))
    (substring-fill! (screen-char-map screen) start end #\space)
    (subvector-fill! (screen-face-map screen) start end face)
    (set-screen-face! screen face))
  (os2ps-clear (screen-psid screen)
	       (cxl->xl screen x)
	       (cxh->xh screen first-unused-x)
	       (cyh->yl screen (fix:+ y 1))
	       (cyl->yh screen y)))

(define (os2-screen/clear-rectangle! screen xl xu yl yu highlight)
  (if (fix:< xl xu)
      (let ((char-map (screen-char-map screen))
	    (face-map (screen-face-map screen))
	    (face (screen-face screen highlight))
	    (x-size (screen-x-size screen))
	    (width (fix:- xu xl)))
	(do ((y yl (fix:+ y 1))
	     (start (screen-char-index screen xl yl) (fix:+ start x-size)))
	    ((fix:= y yu))
	  (let ((end (fix:+ start width)))
	    (substring-fill! char-map start end #\space)
	    (subvector-fill! face-map start end face)))
	(set-screen-face! screen face)
	(os2ps-clear (screen-psid screen)
		     (cxl->xl screen xl) (cxh->xh screen xu)
		     (cyh->yl screen yu) (cyl->yh screen yl)))))

(define (os2-screen/clear-screen! screen)
  (let ((face (screen-normal-face screen)))
    (string-fill! (screen-char-map screen) #\space)
    (vector-fill! (screen-face-map screen) face)
    (set-screen-face! screen face))
  (os2ps-clear (screen-psid screen)
	       0 (screen-pel-width screen)
	       0 (screen-pel-height screen)))

(define (os2-screen/discard! screen)
  (set! screen-list (delq! screen screen-list))
  (os2win-close (screen-wid screen)))

(define (os2-screen/enter! screen)
  (os2win-activate (screen-wid screen)))

(define (os2-screen/exit! screen)
  screen
  unspecific)

(define (os2-screen/flush! screen)
  screen
  unspecific)

(define (os2-screen/modeline-event! screen window type)
  screen window type
  unspecific)

(define (os2-screen/wrap-update! screen thunk)
  (let ((finished? #f))
    (dynamic-wind (lambda () unspecific)
		  (lambda ()
		    (let ((result (thunk)))
		      (set! finished? result)
		      result))
		  (lambda ()
		    (set-screen-face! screen (screen-normal-face screen))
		    (if finished?
			(update-os2-screen-names! screen))))))

(define (os2-screen/write-cursor! screen x y)
  (os2win-move-cursor (screen-wid screen) (cx->x screen x) (cy->y screen y)))

(define (os2-screen/write-char! screen x y char highlight)
  (let ((char-map (screen-char-map screen))
	(index (screen-char-index screen x y))
	(face (screen-face screen highlight)))
    (string-set! char-map index char)
    (vector-set! (screen-face-map screen) index face)
    (set-screen-face! screen face)
    (os2ps-write (screen-psid screen)
		 (cx->x screen x)
		 (fix:+ (cy->y screen y) (screen-char-descender screen))
		 char-map
		 index
		 (fix:+ index 1))))

(define (os2-screen/write-substring! screen x y string start end highlight)
  (let ((start* (screen-char-index screen x y))
	(face (screen-face screen highlight)))
    (%substring-move! string start end (screen-char-map screen) start*)
    (subvector-fill! (screen-face-map screen)
		     start*
		     (fix:+ start* (fix:- end start))
		     face)
    (set-screen-face! screen face)
    (os2ps-write (screen-psid screen)
		 (cx->x screen x)
		 (fix:+ (cy->y screen y) (screen-char-descender screen))
		 string start end)))

(define use-scrolling? #t)

(define (os2-screen/scroll-lines-down! screen xl xu yl yu amount)
  (and use-scrolling?
       (begin
	 (let ((char-map (screen-char-map screen))
	       (face-map (screen-face-map screen))
	       (x-size (screen-x-size screen))
	       (width (fix:- xu xl))
	       (y-from (fix:- yu amount)))
	   (if (fix:= x-size width)
	       (let ((start (fix:* x-size yl))
		     (end (fix:* x-size y-from))
		     (start* (fix:* x-size (fix:+ yl amount))))
		 (%substring-move! char-map start end char-map start*)
		 (subvector-move-right! face-map start end face-map start*))
	       (let ((delta (fix:* x-size amount))
		     (end (screen-char-index screen xl (fix:- yl 1))))
		 (do ((from (screen-char-index screen xl (fix:- y-from 1))
			    (fix:- from x-size)))
		     ((fix:= from end))
		   (let ((from-end (fix:+ from width))
			 (to (fix:+ from delta)))
		     (%substring-move! char-map from from-end char-map to)
		     (subvector-move-right! face-map from from-end
					    face-map to))))))
	 (os2win-scroll (screen-wid screen)
			(cxl->xl screen xl)
			(cxh->xh screen xu)
			(cyh->yl screen (fix:- yu amount))
			(cyl->yh screen yl)
			0
			(fix:- 0 (fix:* amount (screen-char-height screen))))
	 'CLOBBERED-CURSOR)))

(define (os2-screen/scroll-lines-up! screen xl xu yl yu amount)
  (and use-scrolling?
       (begin
	 (let ((char-map (screen-char-map screen))
	       (face-map (screen-face-map screen))
	       (x-size (screen-x-size screen))
	       (width (fix:- xu xl))
	       (y-from (fix:+ yl amount)))
	   (if (fix:= x-size width)
	       (let ((start (fix:* x-size y-from))
		     (end (fix:* x-size yu))
		     (start* (fix:* x-size yl)))
		 (%substring-move! char-map start end char-map start*)
		 (subvector-move-left! face-map start end face-map start*))
	       (let ((delta (fix:* x-size amount))
		     (end (screen-char-index screen xl yu)))
		 (do ((from (screen-char-index screen xl y-from)
			    (fix:+ from x-size)))
		     ((fix:= from end))
		   (let ((from-end (fix:+ from width))
			 (to (fix:- from delta)))
		     (%substring-move! char-map from from-end char-map to)
		     (subvector-move-left! face-map from from-end
					   face-map to))))))
	 (os2win-scroll (screen-wid screen)
			(cxl->xl screen xl)
			(cxh->xh screen xu)
			(cyh->yl screen yu)
			(cyl->yh screen (fix:+ yl amount))
			0
			(fix:* amount (screen-char-height screen)))
	 'CLOBBERED-CURSOR)))

(define (screen-face screen highlight)
  (if highlight
      (screen-highlight-face screen)
      (screen-normal-face screen)))

(define (set-screen-face! screen face)
  (if (not (eq? face (screen-current-face screen)))
      (begin
	(os2ps-set-colors (screen-psid screen)
			  (face-foreground-color face)
			  (face-background-color face))
	(set-screen-current-face! screen face))))

(define-structure face
  (foreground-color #f read-only #t)
  (background-color #f read-only #t))

(define current-font #f)
(define initial-font-list
  '("4.System VIO" "8.Courier" "10.Courier" "12.Courier"
		   "10.System Monospaced"))
(define normal-face (make-face #x000000 #xFFFFFF))
(define highlight-face (make-face #xFFFFFF #x000000))

(define (screen-normal-face screen) screen normal-face)
(define (screen-highlight-face screen) screen highlight-face)

(define (os2-screen/set-foreground-color! screen color)
  screen
  (set! normal-face
	(make-face color (face-background-color normal-face)))
  (set! highlight-face
	(make-face (face-foreground-color highlight-face) color))
  unspecific)

(define (os2-screen/set-background-color! screen color)
  screen
  (set! normal-face
	(make-face (face-foreground-color normal-face) color))
  (set! highlight-face
	(make-face color (face-background-color highlight-face)))
  unspecific)

(define (os2-screen/set-font! screen font)
  (let ((metrics (set-normal-font! (screen-wid screen) font)))
    (if (not metrics)
	(error "Unknown font name:" font))
    (set-screen-font-metrics! screen metrics))
  (set! current-font font)
  (let ((resize (screen-resize-thunk screen)))
    (if resize
	(resize))))

(define (set-normal-font! wid font)
  (let ((metrics (os2ps-set-font (os2win-ps wid) 1 font)))
    (if metrics
	(let ((width (font-metrics/width metrics))
	      (height (font-metrics/height metrics)))
	  (os2win-set-grid wid width height)
	  (os2win-shape-cursor wid width height
			       (fix:or CURSOR_SOLID CURSOR_FLASH))))
    metrics))

(define (os2-screen/set-size! screen x-size y-size)
  (os2win-set-size (screen-wid screen)
		   (fix:* x-size (screen-char-width screen))
		   (fix:* y-size (screen-char-height screen))))

(define (os2-screen/get-frame-size screen)
  (let ((w.h (os2win-get-frame-size (screen-wid screen))))
    (values (car w.h)
	    (cdr w.h))))

(define (os2-screen/get-position screen)
  (let ((x.y (os2win-get-pos (screen-wid screen))))
    (values (car x.y)
	    (cdr x.y))))

(define (os2-screen/set-position! screen x y)
  (os2win-set-pos (screen-wid screen) x y))

(define (os2-screen/set-title! screen title)
  (let ((title* (screen-current-title screen)))
    (if (not (and title* (string=? title title*)))
	(begin
	  (set-screen-current-title! screen #f)
	  (os2win-set-title (screen-wid screen) title)
	  (set-screen-current-title! screen title)))))

(define (os2-screen/raise! screen)
  (os2win-set-state (screen-wid screen) window-state:top))

(define (os2-screen/lower! screen)
  (os2win-set-state (screen-wid screen) window-state:bottom))

(define (os2-screen/show! screen)
  (os2win-set-state (screen-wid screen) window-state:show))

(define (os2-screen/hide! screen)
  (os2win-set-state (screen-wid screen) window-state:hide))

(define (os2-screen/activate! screen)
  (os2win-set-state (screen-wid screen) window-state:activate))

(define (os2-screen/deactivate! screen)
  (os2win-set-state (screen-wid screen) window-state:deactivate))

(define (os2-screen/minimize! screen)
  (os2win-set-state (screen-wid screen) window-state:minimize))

(define (os2-screen/maximize! screen)
  (os2win-set-state (screen-wid screen) window-state:maximize))

(define (os2-screen/restore! screen)
  (os2win-set-state (screen-wid screen) window-state:restore))

(define (os2/desktop-width)
  desktop-width)

(define (os2/desktop-height)
  desktop-height)

(define (cx->x screen cx)
  ;; Returns leftmost pel of cell.
  (fix:* cx (screen-char-width screen)))

(define (cy->y screen cy)
  ;; Returns bottommost pel of cell.
  (cyl->yh screen (fix:+ cy 1)))

(define (cyl->yh screen cy)
  ;; Returns bottommost pel of cell above.
  (fix:* (fix:- (screen-y-size screen) cy) (screen-char-height screen)))

(define cxl->xl cx->x)
(define cxh->xh cx->x)
(define cyh->yl cyl->yh)

(define (x->cx screen x)
  (let ((cx (fix:quotient x (screen-char-width screen)))
	(xs (screen-x-size screen)))
    (if (fix:> cx xs)
	xs
	cx)))

(define (y->cy screen y)
  (let ((cy
	 (fix:- (fix:- (screen-y-size screen) 1)
		(fix:quotient y (screen-char-height screen)))))
    (if (fix:< cy 0)
	0
	cy)))

(define (xl->cxl screen xl)
  (let ((cx (fix:quotient xl (screen-char-width screen)))
	(xs (screen-x-size screen)))
    (if (fix:> cx xs)
	xs
	cx)))

(define (xh->cxh screen xh)
  (let ((cx
	 (let ((cw (screen-char-width screen)))
	   (let ((cx (fix:quotient xh cw)))
	     (if (fix:= 0 (fix:remainder xh cw))
		 cx
		 (fix:+ cx 1)))))
	(xs (screen-x-size screen)))
    (if (fix:> cx xs)
	xs
	cx)))

(define (yl->cyh screen yl)
  (let ((cy
	 (fix:- (screen-y-size screen)
		(fix:quotient yl (screen-char-height screen)))))
    (if (fix:< cy 0)
	0
	cy)))

(define (yh->cyl screen yh)
  (let ((cy
	 (let ((ch (screen-char-height screen)))
	   (let ((cy (fix:- (screen-y-size screen) (fix:quotient yh ch))))
	     (if (fix:= 0 (fix:remainder yh ch))
		 cy
		 (fix:- cy 1))))))
    (if (fix:< cy 0)
	0
	cy)))

(define (width->x-size screen width)
  (fix:quotient width (screen-char-width screen)))

(define (height->y-size screen height)
  (fix:quotient height (screen-char-height screen)))

(define-structure (os2-screen-state
		   (constructor
		    make-screen-state
		    (wid font-metrics pel-width pel-height char-map face-map))
		   (predicate screen-state?)
		   (conc-name screen-state/))
  (wid #f read-only #t)
  font-metrics
  (pel-width 0)
  (pel-height 0)
  (char-map "")
  (face-map '#())
  (current-face normal-face)
  (current-title #f))

(define (screen-wid screen)
  (screen-state/wid (screen-state screen)))

(define (screen-font-metrics screen)
  (screen-state/font-metrics (screen-state screen)))

(define (set-screen-font-metrics! screen metrics)
  (set-screen-state/font-metrics! (screen-state screen) metrics))

(define (screen-pel-width screen)
  (screen-state/pel-width (screen-state screen)))

(define (set-screen-pel-width! screen width)
  (set-screen-state/pel-width! (screen-state screen) width))

(define (screen-pel-height screen)
  (screen-state/pel-height (screen-state screen)))

(define (set-screen-pel-height! screen height)
  (set-screen-state/pel-height! (screen-state screen) height))

(define (screen-char-map screen)
  (screen-state/char-map (screen-state screen)))

(define (set-screen-char-map! screen char-map)
  (set-screen-state/char-map! (screen-state screen) char-map))

(define (screen-face-map screen)
  (screen-state/face-map (screen-state screen)))

(define (set-screen-face-map! screen face-map)
  (set-screen-state/face-map! (screen-state screen) face-map))

(define (screen-current-face screen)
  (screen-state/current-face (screen-state screen)))

(define (set-screen-current-face! screen face)
  (set-screen-state/current-face! (screen-state screen) face))

(define (screen-current-title screen)
  (screen-state/current-title (screen-state screen)))

(define (set-screen-current-title! screen title)
  (set-screen-state/current-title! (screen-state screen) title))

(define (screen-psid screen)
  (os2win-ps (screen-wid screen)))

(define (screen-char-width screen)
  (font-metrics/width (screen-font-metrics screen)))

(define (screen-char-height screen)
  (font-metrics/height (screen-font-metrics screen)))

(define (screen-char-descender screen)
  (font-metrics/descender (screen-font-metrics screen)))

(define (screen-char-index screen x y)
  (fix:+ (fix:* y (screen-x-size screen)) x))

(define (wid->screen wid)
  (let loop ((screens screen-list))
    (and (not (null? screens))
	 (if (fix:= wid (screen-wid (car screens)))
	     (car screens)
	     (loop (cdr screens))))))

(define (get-os2-input-operations screen)
  screen
  (let ((pending #f)
	(repeat 0))

    (define (halt-update?)
      (setup-pending 'IN-UPDATE)
      pending)

    (define (peek-no-hang)
      (setup-pending #f)
      pending)

    (define (peek)
      (setup-pending #t)
      pending)

    (define (read)
      (setup-pending #t)
      (let ((result pending))
	(if (fix:> repeat 1)
	    (set! repeat (fix:- repeat 1))
	    (set! pending #f))
	result))

    (define (setup-pending block?)
      (if (not pending)
	  (let loop ()
	    (let ((event (read-event block?)))
	      (cond ((not event)
		     (set! pending #f))
		    ((input-event? event)
		     (set! pending event)
		     (set! repeat 1))
		    ((not (vector? event))
		     (let ((flag (process-change-event event)))
		       (if flag
			   (begin
			     (set! pending
				   (make-input-event
				    (if (eq? flag 'FORCE-RETURN)
					'RETURN
					'UPDATE)
				    update-screens!
				    #f))
			     (set! repeat 1))
			   (loop))))
		    ((fix:= event-type:key (event-type event))
		     (set! pending (translate-key-event event))
		     (set! repeat (key-event/repeat event))
		     (cond ((fix:= 0 repeat)
			    (set! pending #f))
			   ((and (char? pending)
				 (or (char=? pending #\BEL)
				     (char=? pending #\C-g))
				 signal-interrupts?)
			    (set! pending #f)
			    (signal-interrupt!)))
		     (if (not pending)
			 (loop)))
		    (else
		     (set! pending (process-special-event event))
		     (if pending
			 (set! repeat 1)
			 (loop))))))))

    (values halt-update? peek-no-hang peek read)))

(define (read-event block?)
  (let loop ()
    (set! reading-event? #t)
    (let ((event
	   (if (queue-empty? event-queue)
	       (if (eq? 'IN-UPDATE block?)
		   (os2win-get-event event-descriptor #f)
		   (read-event-1 block?))
	       (dequeue!/unsafe event-queue))))
      (set! reading-event? #f)
      (if (and (vector? event) (fix:= (event-type event) event-type:paint))
	  (begin
	    (process-paint-event event)
	    (loop))
	  event))))

(define (read-event-1 block?)
  (or (os2win-get-event event-descriptor #f)
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
			 (test-for-io-on-descriptor event-descriptor
						    block?
						    'READ)))
		    (case flag
		      ((#F) #f)
		      ((PROCESS-STATUS-CHANGE) event:process-status)
		      ((INTERRUPT) (loop))
		      (else (read-event-1 block?)))))))))))

(define event:process-output -2)
(define event:process-status -3)
(define event:inferior-thread-output -4)

(define (preview-event-stream)
  (set! previewer-registration
	(permanently-register-io-thread-event
	 event-descriptor
	 'READ
	 (current-thread)
	 (lambda (mode)
	   mode
	   (if (not reading-event?)
	       (let ((event (os2win-get-event event-descriptor #f)))
		 (if event
		     (preview-event event)))))))
  unspecific)

(define (preview-event event)
  (cond ((not (vector? event))
	 (enqueue!/unsafe event-queue event))
	((and signal-interrupts?
	      (fix:= event-type:key (event-type event))
	      ;; This tests for CTRL on, ALT off, and
	      ;; not a virtual key:
	      (fix:= #x10 (fix:and #x32 (key-event/flags event)))
	      (let ((code (key-event/code event)))
		(or (fix:= code (char->integer #\G))
		    (fix:= code (char->integer #\g)))))
	 (clean-event-queue event-queue)
	 (signal-interrupt!))
	((fix:= (event-type event) event-type:visibility)
	 (let ((result (process-special-event event)))
	   (if result
	       (enqueue!/unsafe event-queue result))))
	((fix:= (event-type event) event-type:paint)
	 (process-paint-event event))
	(else
	 (enqueue!/unsafe event-queue event))))

(define (clean-event-queue queue)
  ;; Flush keyboard and mouse events from the input queue.  Other
  ;; events are harmless and must be processed regardless.
  (do ((events (let loop ()
		 (if (queue-empty? queue)
		     '()
		     (let ((event (dequeue!/unsafe queue)))
		       (if (and (vector? event)
				(let ((type (event-type event)))
				  (or (fix:= type event-type:button)
				      (fix:= type event-type:key))))
			   (loop)
			   (cons event (loop))))))
	       (cdr events)))
      ((null? events))
    (enqueue!/unsafe queue (car events))))

(define (signal-interrupt!)
  (editor-beep)
  (temporary-message "Quit")
  (^G-signal))

(define (translate-key-event event)
  (let ((code (key-event/code event))
	(flags (key-event/flags event)))
    (let ((bits (flags->bucky-bits flags)))
      (let ((process-code
	     (lambda (code)
	       (if (and (fix:<= #x40 code) (fix:< code #x60)
			(fix:= (fix:and bits char-bit:control)
			       char-bit:control))
		   (make-char (fix:and code #x1F)
			      (fix:andc bits char-bit:control))
		   (make-char code bits)))))
	(if (fix:= 0 (fix:and flags KC_VIRTUALKEY))
	    (and (fix:< code #x80)
		 (process-code code))
	    (let ((key
		   (and (fix:< code (vector-length virtual-key-table))
			(vector-ref virtual-key-table code))))
	      (and key
		   (if (fix:fixnum? key)
		       (process-code key)
		       (make-special-key key bits)))))))))

(define (process-change-event event)
  (cond ((fix:= event event:process-output) (accept-process-output))
	((fix:= event event:process-status) (handle-process-status-changes))
	((fix:= event event:inferior-thread-output) (accept-thread-output))
	(else (error "Illegal change event:" event))))

(define (process-paint-event event)
  (let ((wid (event-wid event))
	(xl (paint-event/xl event))
	(xh (paint-event/xh event))
	(yl (paint-event/yl event))
	(yh (paint-event/yh event)))
    (os2ps-clear (os2win-ps wid) xl xh yl yh)
    (let ((screen (wid->screen wid)))
      (if screen
	  (let ((cxl (xl->cxl screen xl))
		(cxh (xh->cxh screen xh))
		(cyl (yh->cyl screen yh))
		(cyh (yl->cyh screen yl))
		(char-map (screen-char-map screen))
		(face-map (screen-face-map screen))
		(x-size (screen-x-size screen))
		(char-height (screen-char-height screen)))
	    (if (fix:< cxl cxh)
		(let ((size (fix:- cxh cxl)))
		  (do ((cy cyl (fix:+ cy 1))
		       (y (fix:+ (cy->y screen cyl)
				 (screen-char-descender screen))
			  (fix:- y char-height))
		       (start (screen-char-index screen cxl cyl)
			      (fix:+ start x-size)))
		      ((fix:= cy cyh))
		    (let ((end (fix:+ start size)))
		      (let outer ((start start) (cxl cxl))
			(let ((face (vector-ref face-map start)))
			  (let inner ((index (fix:+ start 1)))
			    (if (or (fix:= index end)
				    (not (eq? face
					      (vector-ref face-map index))))
				(begin
				  (set-screen-face! screen face)
				  (os2ps-write (os2win-ps wid)
					       (cx->x screen cxl) y
					       char-map start end)
				  (if (not (fix:= index end))
				      (outer index
					     (fix:+ cxl (fix:- index start)))))
				(inner (fix:+ index 1)))))))))))))))

(define (process-special-event event)
  (let ((handler
	 (let ((type (event-type event)))
	   (and (fix:fixnum? type)
		(fix:>= type 0)
		(fix:< type (vector-length event-handlers))
		(vector-ref event-handlers type))))
	(screen (wid->screen (event-wid event))))
    (and handler
	 screen
	 (handler screen event))))

(define event-handlers
  (make-vector number-of-event-types #f))

(define (define-event-handler event-type handler)
  (vector-set! event-handlers event-type handler))

(define-event-handler event-type:button
  (lambda (screen event)
    (and (eq? button-event-type:down (button-event/type event))
	 (if (os2win-focus? (screen-wid screen))
	     (make-input-event
	      'BUTTON
	      execute-button-command
	      screen
	      (make-down-button (button-event/number event)
				(flags->bucky-bits (button-event/flags event)))
	      (x->cx screen (button-event/x event))
	      (y->cy screen (button-event/y event)))
	     (begin
	       (os2win-activate (screen-wid screen))
	       #f)))))

(define-event-handler event-type:close
  (lambda (screen event)
    event
    (and (not (screen-deleted? screen))
	 (make-input-event 'DELETE-SCREEN delete-screen! screen))))

(define-event-handler event-type:focus
  (lambda (screen event)
    (and (focus-event/gained? event)
	 (not (selected-screen? screen))
	 (make-input-event 'SELECT-SCREEN select-screen screen))))

(define-event-handler event-type:resize
  (lambda (screen event)
    (set-screen-pel-width! screen (resize-event/width event))
    (set-screen-pel-height! screen (resize-event/height event))
    (let ((thunk (screen-resize-thunk screen)))
      (and thunk
	   (make-input-event 'SET-SCREEN-SIZE
			     (lambda (screen)
			       (thunk)
			       (update-screen! screen #t))
			     screen)))))

(define (screen-resize-thunk screen)
  (let ((width (screen-pel-width screen))
	(height (screen-pel-height screen)))
    (let ((x-size (width->x-size screen width))
	  (y-size (height->y-size screen height)))
      (and (not (and (= x-size (screen-x-size screen))
		     (= y-size (screen-y-size screen))))
	   (lambda ()
	     (let ((size (fix:* x-size y-size)))
	       (let ((char-map (make-string size #\space))
		     (face-map
		      (make-vector size (screen-current-face screen))))
		 (without-interrupts
		  (lambda ()
		    (set-screen-char-map! screen char-map)
		    (set-screen-face-map! screen face-map)
		    (set-screen-size! screen x-size y-size))))))))))

(define-event-handler event-type:visibility
  (lambda (screen event)
    (and (not (screen-deleted? screen))
	 (if (visibility-event/shown? event)
	     (begin
	       (set-screen-visibility! screen 'VISIBLE)	;don't really know
	       (screen-force-update screen)
	       (make-input-event 'UPDATE update-screen! screen #f))
	     (begin
	       (set-screen-visibility! screen 'UNMAPPED)
	       (and (selected-screen? screen)
		    (let ((screen (other-screen screen)))
		      (and screen
			   (make-input-event 'SELECT-SCREEN
					     select-screen
					     screen)))))))))

(define (make-virtual-key-table)
  ;; Shift keys are commented out, causing them to be ignored.
  (let ((table (make-vector virtual-key-supremum #f)))
    (vector-set! table VK_BUTTON1	'BUTTON1)
    (vector-set! table VK_BUTTON2	'BUTTON2)
    (vector-set! table VK_BUTTON3	'BUTTON3)
    (vector-set! table VK_BREAK		'BREAK)
    (vector-set! table VK_BACKSPACE	(char-code #\rubout))
    (vector-set! table VK_TAB		(char-code #\tab))
    (vector-set! table VK_BACKTAB	'BACKTAB)
    (vector-set! table VK_NEWLINE	(char-code #\return))
    ;;(vector-set! table VK_SHIFT		'SHIFT)
    ;;(vector-set! table VK_CTRL		'CTRL)
    ;;(vector-set! table VK_ALT		'ALT)
    ;;(vector-set! table VK_ALTGRAF	'ALTGRAF)
    (vector-set! table VK_PAUSE		'PAUSE)
    ;;(vector-set! table VK_CAPSLOCK	'CAPS-LOCK)
    (vector-set! table VK_ESC		(char-code #\escape))
    (vector-set! table VK_SPACE		(char-code #\space))
    (vector-set! table VK_PAGEUP	'PAGE-UP)
    (vector-set! table VK_PAGEDOWN	'PAGE-DOWN)
    (vector-set! table VK_END		'END)
    (vector-set! table VK_HOME		'HOME)
    (vector-set! table VK_LEFT		'LEFT)
    (vector-set! table VK_UP		'UP)
    (vector-set! table VK_RIGHT		'RIGHT)
    (vector-set! table VK_DOWN		'DOWN)
    (vector-set! table VK_PRINTSCRN	'PRINT-SCREEN)
    (vector-set! table VK_INSERT	'INSERT)
    (vector-set! table VK_DELETE	'DELETE)
    ;;(vector-set! table VK_SCRLLOCK	'SCRL-LOCK)
    ;;(vector-set! table VK_NUMLOCK	'NUM-LOCK)
    (vector-set! table VK_ENTER		(char-code #\return))
    (vector-set! table VK_SYSRQ		'SYSRQ)
    (vector-set! table VK_F1		'F1)
    (vector-set! table VK_F2		'F2)
    (vector-set! table VK_F3		'F3)
    (vector-set! table VK_F4		'F4)
    (vector-set! table VK_F5		'F5)
    (vector-set! table VK_F6		'F6)
    (vector-set! table VK_F7		'F7)
    (vector-set! table VK_F8		'F8)
    (vector-set! table VK_F9		'F9)
    (vector-set! table VK_F10		'F10)
    (vector-set! table VK_F11		'F11)
    (vector-set! table VK_F12		'F12)
    (vector-set! table VK_F13		'F13)
    (vector-set! table VK_F14		'F14)
    (vector-set! table VK_F15		'F15)
    (vector-set! table VK_F16		'F16)
    (vector-set! table VK_F17		'F17)
    (vector-set! table VK_F18		'F18)
    (vector-set! table VK_F19		'F19)
    (vector-set! table VK_F20		'F20)
    (vector-set! table VK_F21		'F21)
    (vector-set! table VK_F22		'F22)
    (vector-set! table VK_F23		'F23)
    (vector-set! table VK_F24		'F24)
    (vector-set! table VK_ENDDRAG	'END-DRAG)
    (vector-set! table VK_CLEAR		'CLEAR)
    (vector-set! table VK_EREOF		'EREOF)
    (vector-set! table VK_PA1		'PA1)
    table))

(define (flags->bucky-bits flags)
  (fix:or (if (fix:= 0 (fix:and flags KC_CTRL)) #x2 #x0)
	  (if (fix:= 0 (fix:and flags KC_ALT))  #x1 #x0)))