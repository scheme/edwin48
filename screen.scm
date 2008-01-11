#| -*-Scheme-*-

$Id: screen.scm,v 1.124 2007/01/05 21:19:24 cph Exp $

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

;;;; Screen Abstraction


(define-structure (screen
		   (constructor make-screen
				(state
				 operation/beep
				 operation/clear-line!
				 operation/clear-rectangle!
				 operation/clear-screen!
				 operation/discard!
				 operation/enter!
				 operation/exit!
				 operation/flush!
				 operation/modeline-event!
				 operation/discretionary-flush
				 operation/scroll-lines-down!
				 operation/scroll-lines-up!
				 operation/wrap-update!
				 operation/write-char!
				 operation/write-cursor!
				 operation/write-substring!
				 preemption-modulus
				 x-size
				 y-size)))
  (state false read-only true)
  (operation/beep false read-only true)
  (operation/clear-line! false read-only true)
  (operation/clear-rectangle! false read-only true)
  (operation/clear-screen! false read-only true)
  (operation/discard! false read-only true)
  (operation/enter! false read-only true)
  (operation/exit! false read-only true)
  (operation/flush! false read-only true)
  (operation/modeline-event! false read-only true)
  (operation/discretionary-flush false read-only true)
  (operation/scroll-lines-down! false read-only true)
  (operation/scroll-lines-up! false read-only true)
  (operation/wrap-update! false read-only true)
  (operation/write-char! false read-only true)
  (operation/write-cursor! false read-only true)
  (operation/write-substring! false read-only true)
  (preemption-modulus false read-only true)
  (root-window false)
  ;; Visibility is one of the following:
  ;; VISIBLE PARTIALLY-OBSCURED OBSCURED UNMAPPED DELETED
  (visibility 'VISIBLE)
  (needs-update? false)
  (in-update? false)
  (x-size false)
  (y-size false)

  ;; Description of actual screen contents.
  current-matrix

  ;; Description of desired screen contents.
  new-matrix

  ;; Set this variable in the debugger to trace interesting events.
  (debug-trace false))

(define (guarantee-screen object procedure)
  (if (not (screen? object))
      (error:wrong-type-argument object "screen" procedure)))

(define (initialize-screen-root-window! screen bufferset buffer)
  (set-screen-root-window!
   screen
   (make-editor-frame
    screen
    buffer
    (bufferset-find-or-create-buffer bufferset (make-typein-buffer-name -1))))
  (set-screen-current-matrix! screen (make-matrix screen))
  (set-screen-new-matrix! screen (make-matrix screen)))

(define (screen-beep screen)
  ((screen-operation/beep screen) screen))

(define (screen-enter! screen)
  ((screen-operation/enter! screen) screen)
  (screen-modeline-event! screen
			  (screen-selected-window screen)
			  'SELECT-SCREEN))

(define (screen-exit! screen)
  ((screen-operation/exit! screen) screen)
  (screen-modeline-event! screen
			  (screen-selected-window screen)
			  'DESELECT-SCREEN))

(define (screen-discard! screen)
  (if (not (screen-deleted? screen))
      (begin
	(set-screen-visibility! screen 'DELETED)
	(for-each (lambda (window) (send window ':kill!))
		  (screen-window-list screen))
	((screen-operation/discard! screen) screen))))

(define (screen-modeline-event! screen window type)
  ((screen-operation/modeline-event! screen) screen window type))

(define (screen-selected-window screen)
  (editor-frame-selected-window (screen-root-window screen)))

(define (screen-select-window! screen window)
  (editor-frame-select-window! (screen-root-window screen) window)
  (screen-modeline-event! screen window 'SELECT-WINDOW))

(define (screen-select-cursor! screen window)
  (editor-frame-select-cursor! (screen-root-window screen) window))

(define (screen-window-list screen)
  (editor-frame-windows (screen-root-window screen)))

(define (screen-window0 screen)
  (editor-frame-window0 (screen-root-window screen)))

(define (screen-typein-window screen)
  (editor-frame-typein-window (screen-root-window screen)))

(define (window-screen window)
  (editor-frame-screen (window-root-window window)))

(define (screen-visible? screen)
  (not (or (screen-deleted? screen)
	   (eq? 'UNMAPPED (screen-visibility screen)))))

(define (screen-deleted? screen)
  (eq? 'DELETED (screen-visibility screen)))

(define (update-screen! screen display-style)
  (if (display-style/discard-screen-contents? display-style)
      (screen-force-update screen))
  (let ((finished?
	 (with-screen-in-update screen display-style
	   (lambda ()
	     (editor-frame-update-display! (screen-root-window screen)
					   display-style)))))
    (if (eq? finished? #t)
	(set-screen-needs-update?! screen #f))
    finished?))

;;; Interface from update optimizer to terminal:

(define (terminal-scroll-lines-down screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'scroll-lines-down
				   xl xu yl yu amount))
  ((screen-operation/scroll-lines-down! screen) screen xl xu yl yu amount))

(define (terminal-scroll-lines-up screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'scroll-lines-up
				   xl xu yl yu amount))
  ((screen-operation/scroll-lines-up! screen) screen xl xu yl yu amount))

(define (terminal-flush screen)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'flush))
  ((screen-operation/flush! screen) screen))

(define (terminal-move-cursor screen x y)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'move-cursor x y))
  ((screen-operation/write-cursor! screen) screen x y))

(define (terminal-clear-screen screen)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'clear-screen))
  ((screen-operation/clear-screen! screen) screen))

(define (terminal-clear-line screen x y first-unused-x)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'clear-line
				   x y first-unused-x))
  ((screen-operation/clear-line! screen) screen x y first-unused-x))

(define (terminal-output-char screen x y char face)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'output-char
				   x y char face))
  ((screen-operation/write-char! screen) screen x y char face))

(define (terminal-output-substring screen x y string start end face)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'output-substring
				   x y (string-copy string) start end face))
  ((screen-operation/write-substring! screen) screen x y string start end
					      face))

;;;; Update Optimization

(define-structure (matrix (constructor %make-matrix ()))
  ;; Vector of line contents.
  ;; (string-ref (vector-ref (matrix-contents m) y) x) is the
  ;; character at position X, Y.
  contents

  ;; Vector of line highlights.
  ;; (vector-ref (vector-ref (matrix-highlight m) y) x) is the
  ;; highlight at position X, Y.
  highlight

  ;; Boolean-vector indicating, for each line, whether its contents
  ;; mean anything.
  enable

  ;; Boolean-vector indicating, for each line, whether there is any
  ;; highlighting on the line.
  highlight-enable

  ;; Cursor position.
  (cursor-x #f)
  (cursor-y #f))

(define (make-matrix screen)
  (let ((matrix (%make-matrix))
	(x-size (screen-x-size screen))
	(y-size (screen-y-size screen)))
    (let ((contents (make-vector y-size))
	  (highlight (make-vector y-size))
	  (enable (make-boolean-vector y-size))
	  (highlight-enable (make-boolean-vector y-size)))
      (do ((i 0 (fix:1+ i)))
	  ((fix:= i y-size))
	(vector-set! contents i (make-string x-size))
	(vector-set! highlight i (make-vector x-size)))
      (boolean-vector-fill! enable false)
      (set-matrix-contents! matrix contents)
      (set-matrix-highlight! matrix highlight)
      (set-matrix-enable! matrix enable)
      (set-matrix-highlight-enable! matrix highlight-enable))
    matrix))

(define (highlight-ref matrix y x)
  (vector-ref (vector-ref (matrix-highlight matrix) y) x))

(define (highlight-set! matrix y x face)
  (vector-set! (vector-ref (matrix-highlight matrix) y) x face))

(define (set-line-highlights! matrix y face)
  (vector-fill! (vector-ref (matrix-highlight matrix) y) face))

(define (set-subline-highlights! matrix y xl xu face)
  (subvector-fill! (vector-ref (matrix-highlight matrix) y) xl xu face))

(define (clear-line-highlights! matrix y)
  (set-line-highlights! matrix y (default-face)))

(define (clear-subline-highlights! matrix y xl xu)
  (set-subline-highlights! matrix y xl xu (default-face)))

(define (copy-line-highlights! m1 y1 m2 y2)
  (vector-move! (vector-ref (matrix-highlight m1) y1)
		(vector-ref (matrix-highlight m2) y2)))

(define (copy-subline-highlights! m1 y1 xl1 xu1 m2 y2 xl2)
  (subvector-move-left! (vector-ref (matrix-highlight m1) y1) xl1 xu1
			(vector-ref (matrix-highlight m2) y2) xl2))

(define (line-highlights-cleared? matrix y)
  (vector-filled? (vector-ref (matrix-highlight matrix) y) (default-face)))

(define (swap-line-highlights! m1 y1 m2 y2)
  (let ((h (vector-ref (matrix-highlight m1) y1)))
    (vector-set! (matrix-highlight m1) y1
		 (vector-ref (matrix-highlight m2) y2))
    (vector-set! (matrix-highlight m2) y2 h)))

(define (subline-highlights-uniform? matrix y xl xu)
  (subvector-uniform? (vector-ref (matrix-highlight matrix) y) xl xu))

(define (find-subline-highlight-change matrix y xl xu face)
  (subvector-find-next-element-not (vector-ref (matrix-highlight matrix) y)
				   xl xu face))

(define (default-face? face)
  (not face))

(define (default-face)
  #f)

(define (highlight-face)
  #t)

(define (line-contents-enabled? matrix y)
  (boolean-vector-ref (matrix-enable matrix) y))

(define (enable-line-contents! matrix y)
  (boolean-vector-set! (matrix-enable matrix) y #t))

(define (disable-line-contents! matrix y)
  (boolean-vector-set! (matrix-enable matrix) y #f))

(define (multiple-line-contents-enabled? matrix yl yu)
  (boolean-subvector-all-elements? (matrix-enable matrix) yl yu #t))

(define (line-highlights-enabled? matrix y)
  (boolean-vector-ref (matrix-highlight-enable matrix) y))

(define (enable-line-highlights! matrix y)
  (boolean-vector-set! (matrix-highlight-enable matrix) y #t))

(define (disable-line-highlights! matrix y)
  (boolean-vector-set! (matrix-highlight-enable matrix) y #f))

(define (set-screen-size! screen x-size y-size)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'set-size! x-size y-size))
  (without-interrupts
   (lambda ()
     (set-screen-x-size! screen x-size)
     (set-screen-y-size! screen y-size)
     (set-screen-current-matrix! screen (make-matrix screen))
     (set-screen-new-matrix! screen (make-matrix screen))
     (send (screen-root-window screen) ':set-size! x-size y-size))))

(define (screen-move-cursor screen x y)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'move-cursor x y))
  (let ((new-matrix (screen-new-matrix screen)))
    (set-matrix-cursor-x! new-matrix x)
    (set-matrix-cursor-y! new-matrix y))
  ;; Kludge: forget current position of cursor in order to force it to
  ;; move.  Works around side-effects in terminal that move cursor.
  (let ((current-matrix (screen-current-matrix screen)))
    (set-matrix-cursor-x! current-matrix #f)
    (set-matrix-cursor-y! current-matrix #f)))

(define (screen-direct-output-move-cursor screen x y)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'direct-output-move-cursor
				   x y))
  (terminal-move-cursor screen x y)
  (terminal-flush screen)
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (set-matrix-cursor-x! current-matrix x)
    (set-matrix-cursor-y! current-matrix y)
    (set-matrix-cursor-x! new-matrix x)
    (set-matrix-cursor-y! new-matrix y)))

(define (screen-output-char screen x y char face)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'output-char x y char face))
  (let ((new-matrix (screen-new-matrix screen)))
    (cond ((not (line-contents-enabled? new-matrix y))
	   (enable-line-contents! new-matrix y)
	   (set-screen-needs-update?! screen true)
	   (initialize-new-line-contents screen y)
	   (if (not (default-face? face))
	       (begin
		 (enable-line-highlights! new-matrix y)
		 (initialize-new-line-highlight screen y)
		 (highlight-set! new-matrix y x face))))
	  ((line-highlights-enabled? new-matrix y)
	   (highlight-set! new-matrix y x face))
	  ((not (default-face? face))
	   (enable-line-highlights! new-matrix y)
	   (clear-line-highlights! new-matrix y)
	   (highlight-set! new-matrix y x face)))
    (string-set! (vector-ref (matrix-contents new-matrix) y) x char)))

(define (screen-get-output-line screen y xl xu face)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'output-line y xl xu face))
  (let ((new-matrix (screen-new-matrix screen)))
    (let ((full-line? (and (fix:= xl 0) (fix:= xu (screen-x-size screen)))))
      (cond ((not (line-contents-enabled? new-matrix y))
	     (enable-line-contents! new-matrix y)
	     (set-screen-needs-update?! screen true)
	     (if (not full-line?) (initialize-new-line-contents screen y))
	     (if (not (default-face? face))
		 (begin
		   (enable-line-highlights! new-matrix y)
		   (if (not full-line?)
		       (initialize-new-line-highlight screen y))
		   (set-subline-highlights! new-matrix y xl xu face))))
	    ((line-highlights-enabled? new-matrix y)
	     (if (and full-line? (not face))
		 (disable-line-highlights! new-matrix y)
		 (set-subline-highlights! new-matrix y xl xu face)))
	    ((not (default-face? face))
	     (enable-line-highlights! new-matrix y)
	     (if (not full-line?)
		 (set-line-highlights! new-matrix y (default-face)))
	     (set-subline-highlights! new-matrix y xl xu face))))
    (vector-ref (matrix-contents new-matrix) y)))

(define (screen-output-substring screen x y string start end face)
  (substring-move-left! string start end
			(screen-get-output-line screen y x
						(fix:+ x (fix:- end start))
						face)
			x))

(define (initialize-new-line-contents screen y)
  (if (line-contents-enabled? (screen-current-matrix screen) y)
      (string-move!
       (vector-ref (matrix-contents (screen-current-matrix screen)) y)
       (vector-ref (matrix-contents (screen-new-matrix screen)) y))
      (string-fill!
       (vector-ref (matrix-contents (screen-new-matrix screen)) y)
       #\space)))

(define (initialize-new-line-highlight screen y)
  (if (line-highlights-enabled? (screen-current-matrix screen) y)
      (copy-line-highlights! (screen-current-matrix screen) y
			     (screen-new-matrix screen) y)
      (clear-line-highlights! (screen-new-matrix screen) y)))

(define (screen-clear-rectangle screen xl xu yl yu face)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'clear-rectangle
				   xl xu yl yu face))
  (let ((new-matrix (screen-new-matrix screen)))
    (let ((new-contents (matrix-contents new-matrix)))
      (cond ((not (and (fix:= xl 0) (fix:= xu (screen-x-size screen))))
	     (let ((current-matrix (screen-current-matrix screen)))
	       (let ((current-contents (matrix-contents current-matrix)))
		 (do ((y yl (fix:1+ y)))
		     ((fix:= y yu))
		   (if (not (line-contents-enabled? new-matrix y))
		       (begin
			 (enable-line-contents! new-matrix y)
			 (if (line-contents-enabled? current-matrix y)
			     (begin
			       (string-move! (vector-ref current-contents y)
					     (vector-ref new-contents y))
			       (substring-fill! (vector-ref new-contents y)
						xl xu #\space))
			     (string-fill! (vector-ref new-contents y)
					   #\space)))
		       (substring-fill! (vector-ref new-contents y)
					xl xu #\space))
		   (cond ((line-highlights-enabled? new-matrix y)
			  (set-subline-highlights! new-matrix y xl xu face))
			 ((not (default-face? face))
			  (enable-line-highlights! new-matrix y)
			  (if (line-highlights-enabled? current-matrix y)
			      (copy-line-highlights! current-matrix y
						     new-matrix y)
			      (clear-line-highlights! new-matrix y))
			  (set-subline-highlights! new-matrix y xl xu face))
			 ((line-highlights-enabled? current-matrix y)
			  (copy-line-highlights! current-matrix y new-matrix y)
			  (clear-subline-highlights! new-matrix y xl xu)
			  (if (not (line-highlights-cleared? new-matrix y))
			      (enable-line-highlights! new-matrix y))))))))
	    ((not (default-face? face))
	     (do ((y yl (fix:1+ y)))
		 ((fix:= y yu))
	       (string-fill! (vector-ref new-contents y) #\space)
	       (enable-line-contents! new-matrix y)
	       (set-line-highlights! new-matrix y face)
	       (enable-line-highlights! new-matrix y)))
	    (else
	     (do ((y yl (fix:1+ y)))
		 ((fix:= y yu))
	       (string-fill! (vector-ref new-contents y) #\space)
	       (enable-line-contents! new-matrix y)
	       (disable-line-highlights! new-matrix y))))))
  (set-screen-needs-update?! screen true))

(define (screen-direct-output-char screen x y char face)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'direct-output-char
				   x y char face))
  (let ((cursor-x (fix:1+ x))
	(current-matrix (screen-current-matrix screen)))
    (terminal-output-char screen x y char face)
    (terminal-move-cursor screen cursor-x y)
    (terminal-flush screen)
    (string-set! (vector-ref (matrix-contents current-matrix) y) x char)
    (cond ((line-highlights-enabled? current-matrix y)
	   (highlight-set! current-matrix y x face))
	  ((not (default-face? face))
	   (enable-line-highlights! current-matrix y)
	   (highlight-set! current-matrix y x face)))
    (set-matrix-cursor-x! current-matrix cursor-x)
    (set-matrix-cursor-x! (screen-new-matrix screen) cursor-x)))

(define (screen-direct-output-substring screen x y string start end face)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'direct-output-substring
				   x y (string-copy string) start end face))
  (let ((cursor-x (fix:+ x (fix:- end start)))
	(current-matrix (screen-current-matrix screen)))
    (terminal-output-substring screen x y string start end face)
    (terminal-move-cursor screen cursor-x y)
    (terminal-flush screen)
    (substring-move-left! string start end
			  (vector-ref (matrix-contents current-matrix) y) x)
    (cond ((line-highlights-enabled? current-matrix y)
	   (set-subline-highlights! current-matrix y x cursor-x face))
	  ((not (default-face? face))
	   (enable-line-highlights! current-matrix y)
	   (set-subline-highlights! current-matrix y x cursor-x face)))
    (set-matrix-cursor-x! current-matrix cursor-x)
    (set-matrix-cursor-x! (screen-new-matrix screen) cursor-x)))

(define (screen-force-update screen)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'force-update))
  (let ((y-size (screen-y-size screen))
	(current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (terminal-clear-screen screen)
    (let ((current-contents (matrix-contents current-matrix))
	  (new-contents (matrix-contents new-matrix)))
      (do ((y 0 (fix:1+ y)))
	  ((fix:= y y-size))
	(if (not (line-contents-enabled? new-matrix y))
	    (begin
	      (let ((c (vector-ref new-contents y)))
		(vector-set! new-contents y (vector-ref current-contents y))
		(vector-set! current-contents y c))
	      (enable-line-contents! new-matrix y)
	      (if (line-highlights-enabled? current-matrix y)
		  (begin
		    (swap-line-highlights! new-matrix y current-matrix y)
		    (enable-line-highlights! new-matrix y)))))
	(string-fill! (vector-ref current-contents y) #\space)
	(enable-line-contents! current-matrix y)
	(disable-line-highlights! current-matrix y))))
  (invalidate-cursor screen)
  (set-screen-needs-update?! screen true))

(define (invalidate-cursor screen)
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (if (or (matrix-cursor-x current-matrix)
	    (matrix-cursor-y current-matrix))
	(begin
	  (set-matrix-cursor-x! new-matrix (matrix-cursor-x current-matrix))
	  (set-matrix-cursor-y! new-matrix (matrix-cursor-y current-matrix))
	  (set-matrix-cursor-x! current-matrix #f)
	  (set-matrix-cursor-y! current-matrix #f)))))

(define (screen-scroll-lines-down screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'scroll-lines-down
				   xl xu yl yu amount))
  (let ((current-matrix (screen-current-matrix screen)))
    (and (multiple-line-contents-enabled? current-matrix yl yu)
	 (not (screen-needs-update? screen))
	 (let ((scrolled?
		(terminal-scroll-lines-down screen xl xu yl yu amount)))
	   (and scrolled?
		(begin
		  (let ((contents (matrix-contents current-matrix)))
		    (do ((y (fix:-1+ (fix:- yu amount)) (fix:-1+ y))
			 (y* (fix:-1+ yu) (fix:-1+ y*)))
			((fix:< y yl))
		      (substring-move-left! (vector-ref contents y) xl xu
					    (vector-ref contents y*) xl)
		      (cond ((line-highlights-enabled? current-matrix y)
			     (enable-line-highlights! current-matrix y*)
			     (copy-subline-highlights! current-matrix y xl xu
						       current-matrix y* xl))
			    ((line-highlights-enabled? current-matrix y*)
			     (clear-subline-highlights! current-matrix y*
							xl xu))))
		    (case scrolled?
		      ((CLEARED)
		       (let ((yu (fix:+ yl amount)))
			 (if (and (fix:= xl 0)
				  (fix:= xu (screen-x-size screen)))
			     (do ((y yl (fix:1+ y)))
				 ((fix:= y yu))
			       (substring-fill! (vector-ref contents y) xl xu
						#\space)
			       (disable-line-highlights! current-matrix y))
			     (do ((y yl (fix:1+ y)))
				 ((fix:= y yu))
			       (substring-fill! (vector-ref contents y) xl xu
						#\space)
			       (if (line-highlights-enabled? current-matrix y)
				   (clear-subline-highlights! current-matrix y
							      xl xu))))))
		      ((CLOBBERED-CURSOR)
		       (invalidate-cursor screen))))
		  scrolled?))))))

(define (screen-scroll-lines-up screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'scroll-lines-up
				   xl xu yl yu amount))
  (let ((current-matrix (screen-current-matrix screen)))
    (and (multiple-line-contents-enabled? current-matrix yl yu)
	 (not (screen-needs-update? screen))
	 (let ((scrolled?
		(terminal-scroll-lines-up screen xl xu yl yu amount)))
	   (and scrolled?
		(begin
		  (let ((contents (matrix-contents current-matrix)))
		    (do ((y yl (fix:1+ y))
			 (y* (fix:+ yl amount) (fix:1+ y*)))
			((fix:= y* yu))
		      (substring-move-left! (vector-ref contents y*) xl xu
					    (vector-ref contents y) xl)
		      (cond ((line-highlights-enabled? current-matrix y*)
			     (enable-line-highlights! current-matrix y)
			     (copy-subline-highlights! current-matrix y* xl xu
						       current-matrix y xl))
			    ((line-highlights-enabled? current-matrix y)
			     (clear-subline-highlights! current-matrix y
							xl xu))))
		    (case scrolled?
		      ((CLEARED)
		       (if (and (fix:= xl 0)
				(fix:= xu (screen-x-size screen)))
			   (do ((y (fix:- yu amount) (fix:1+ y)))
			       ((fix:= y yu))
			     (substring-fill! (vector-ref contents y) xl xu
					      #\space)
			     (disable-line-highlights! current-matrix y))
			   (do ((y (fix:- yu amount) (fix:1+ y)))
			       ((fix:= y yu))
			     (substring-fill! (vector-ref contents y) xl xu
					      #\space)
			     (if (line-highlights-enabled? current-matrix y)
				 (clear-subline-highlights! current-matrix y
							    xl xu)))))
		      ((CLOBBERED-CURSOR)
		       (invalidate-cursor screen))))
		  scrolled?))))))

(define (with-screen-in-update screen display-style thunk)
  (without-interrupts
   (lambda ()
     (let ((old-flag (screen-in-update? screen)))
       (set-screen-in-update?! screen true)
       (let ((finished?
	      ((screen-operation/wrap-update! screen)
	       screen
	       (lambda ()
		 (and (thunk)
		      (if (memq (screen-visibility screen)
				'(VISIBLE PARTIALLY-OBSCURED))
			  (and (or (not (screen-needs-update? screen))
				   (and (not (display-style/no-screen-output?
					      display-style))
					(screen-update screen display-style)))
			       (begin
				 (screen-update-cursor screen)
				 #t))
			  'INVISIBLE))))))
	 (set-screen-in-update?! screen old-flag)
	 finished?)))))

(define (screen-update-cursor screen)
  (let ((x (matrix-cursor-x (screen-new-matrix screen)))
	(y (matrix-cursor-y (screen-new-matrix screen))))
    (if (not (and (eqv? x (matrix-cursor-x (screen-current-matrix screen)))
		  (eqv? y (matrix-cursor-y (screen-current-matrix screen)))))
	(terminal-move-cursor screen x y))
    (set-matrix-cursor-x! (screen-current-matrix screen) x)
    (set-matrix-cursor-y! (screen-current-matrix screen) y)))

(define (screen-update screen force?)
  ;; Update the actual terminal screen based on the data in `new-matrix'.
  ;; Value is #F if redisplay stopped due to pending input.
  ;; FORCE? true means do not stop for pending input.
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'update force?))
  (let ((new-matrix (screen-new-matrix screen))
	(y-size (screen-y-size screen))
	(preemption-modulus (screen-preemption-modulus screen))
	(discretionary-flush (screen-operation/discretionary-flush screen))
	(halt-update? (editor-halt-update? current-editor)))
    (let loop ((y 0) (m 0))
      (cond ((fix:= y y-size)
	     true)
	    ((not (line-contents-enabled? new-matrix y))
	     (loop (fix:+ y 1) m))
	    ((not (fix:= 0 m))
	     (update-line screen y)
	     (loop (fix:+ y 1) (fix:- m 1)))
	    ((begin
	       (if discretionary-flush (discretionary-flush screen))
	       (and (not force?) (halt-update?)))
	     (if (screen-debug-trace screen)
		 ((screen-debug-trace screen) 'screen screen
					      'update-preemption y))
	     false)
	    (else
	     (update-line screen y)
	     (loop (fix:+ y 1) preemption-modulus))))))

(define (update-line screen y)
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen))
	(x-size (screen-x-size screen)))
    (let ((current-contents (matrix-contents current-matrix))
	  (new-contents (matrix-contents new-matrix)))
      (let ((ccy (vector-ref current-contents y))
	    (ncy (vector-ref new-contents y))
	    (nhey (line-highlights-enabled? new-matrix y)))
	(cond ((or (not (line-contents-enabled? current-matrix y))
		   (if (line-highlights-enabled? current-matrix y)
		       (not nhey)
		       nhey))
	       (if nhey
		   (update-line-ignore-current screen y ncy new-matrix x-size)
		   (update-line-trivial screen y ncy x-size)))
	      (nhey
	       (update-line-highlight screen y
				      ccy current-matrix
				      ncy new-matrix
				      x-size))
	      (else
	       (update-line-no-highlight screen y ccy ncy x-size)))
	(vector-set! current-contents y ncy)
	(enable-line-contents! current-matrix y)
	(vector-set! new-contents y ccy)
	(disable-line-contents! new-matrix y)
	(if nhey
	    (begin
	      (swap-line-highlights! current-matrix y new-matrix y)
	      (enable-line-highlights! current-matrix y)
	      (disable-line-highlights! new-matrix y))
	    (disable-line-highlights! current-matrix y))))))

(define (update-line-ignore-current screen y nline matrix x-size)
  (cond ((not (subline-highlights-uniform? matrix y 0 x-size))
	 (let loop ((x 0))
	   (let ((face (highlight-ref matrix y x)))
	     (let ((x*
		    (find-subline-highlight-change matrix y (fix:1+ x) x-size
						   face)))
	       (if x*
		   (begin
		     (terminal-output-substring screen x y nline x x* face)
		     (loop x*))
		   (terminal-output-substring screen x y nline x x-size
					      face))))))
	((not (default-face? (highlight-ref matrix y 0)))
	 (terminal-output-substring screen 0 y nline 0 x-size
				    (highlight-ref matrix y 0)))
	(else
	 (update-line-trivial screen y nline x-size))))

(define (update-line-trivial screen y nline x-size)
  (let ((xe (substring-non-space-end nline 0 x-size)))
    (if (fix:< 0 xe)
	(terminal-output-substring screen 0 y nline 0 xe false))
    (if (fix:< xe x-size)
	(terminal-clear-line screen xe y x-size))))

(define (update-line-no-highlight screen y oline nline x-size)
  (let ((olen (substring-non-space-end oline 0 x-size))
	(nlen (substring-non-space-end nline 0 x-size)))
    (cond ((fix:= 0 olen)
	   (let ((nstart (substring-non-space-start nline 0 nlen)))
	     (if (fix:< nstart nlen)
		 (terminal-output-substring screen nstart y
					    nline nstart nlen false))))
	  ((fix:= 0 nlen)
	   (terminal-clear-line screen nlen y olen))
	  (else
	   (let ((len (fix:min olen nlen)))
	     (let find-mismatch ((x 0))
	       (cond ((fix:= x len)
		      (if (fix:< x nlen)
			  (terminal-output-substring screen x y
						     nline x nlen false)))
		     ((fix:= (vector-8b-ref oline x)
			     (vector-8b-ref nline x))
		      (find-mismatch (fix:+ x 1)))
		     (else
		      (let find-match ((x* (fix:+ x 1)))
			(cond ((fix:= x* len)
			       (terminal-output-substring
				screen x y nline x nlen false))
			      ((not (fix:= (vector-8b-ref oline x*)
					   (vector-8b-ref nline x*)))
			       (find-match (fix:+ x* 1)))
			      (else
			       ;; Ignore matches of 4 characters or less.
			       ;; The overhead of moving the cursor and
			       ;; drawing the characters is too much except
			       ;; for very slow terminals.
			       (let find-end-match ((x** (fix:+ x* 1)))
				 (cond ((fix:= x** len)
					(if (fix:< (fix:- x** x*) 5)
					    (terminal-output-substring
					     screen x y nline x nlen false)
					    (begin
					      (terminal-output-substring
					       screen x y nline x x* false)
					      (if (fix:< x** nlen)
						  (terminal-output-substring
						   screen x** y
						   nline x** nlen false)))))
				       ((fix:= (vector-8b-ref oline x**)
					       (vector-8b-ref nline x**))
					(find-end-match (fix:+ x** 1)))
				       ((fix:< (fix:- x** x*) 5)
					(find-match x**))
				       (else
					(terminal-output-substring
					 screen x y nline x x* false)
					(find-mismatch x**)))))))))))
	   (if (fix:< nlen olen)
	       (terminal-clear-line screen nlen y olen))))))

(define (screen-line-draw-cost screen y)
  (let ((line (vector-ref (matrix-contents (screen-current-matrix screen)) y)))
    (let ((end (substring-non-space-end line 0 (string-length line))))
      (if (fix:= 0 end)
	  0
	  (fix:- end (substring-non-space-start line 0 end))))))

(define (update-line-highlight screen y oline om nline nm x-size)
  (let find-mismatch ((x 0))
    (if (not (fix:= x x-size))
	(if (and (fix:= (vector-8b-ref oline x) (vector-8b-ref nline x))
		 (eqv? (highlight-ref om y x) (highlight-ref nm y x)))
	    (find-mismatch (fix:+ x 1))
	    (let ((face (highlight-ref nm y x)))
	      (let find-match ((x* (fix:+ x 1)))
		(cond ((fix:= x* x-size)
		       (terminal-output-substring screen x y nline x x* face))
		      ((not (eqv? face (highlight-ref nm y x*)))
		       (terminal-output-substring screen x y nline x x* face)
		       (find-mismatch x*))
		      ((not (and (eqv? face (highlight-ref om y x*))
				 (fix:= (vector-8b-ref oline x*)
					(vector-8b-ref nline x*))))
		       (find-match (fix:+ x* 1)))
		      (else
		       (let find-end-match ((x** (fix:+ x* 1)))
			 (cond ((fix:= x** x-size)
				(terminal-output-substring
				 screen x y nline x x* face))
			       ((and (eqv? face (highlight-ref om y x**))
				     (fix:= (vector-8b-ref oline x**)
					    (vector-8b-ref nline x**)))
				(find-end-match (fix:+ x** 1)))
			       ((fix:< (fix:- x** x*) 5)
				;; Ignore matches of 4 chars or less.
				(find-match x**))
			       (else
				(terminal-output-substring
				 screen x y nline x x* face)
				(find-mismatch x**))))))))))))

(define (substring-non-space-start string start end)
  (do ((index start (fix:+ index 1)))
      ((or (fix:= end index)
	   (not (fix:= (vector-8b-ref string index)
		       (char->integer #\space))))
       index)))

(define (substring-non-space-end string start end)
  (do ((index end (fix:- index 1)))
      ((or (fix:= start index)
	   (not (fix:= (vector-8b-ref string (fix:- index 1))
		       (char->integer #\space))))
       index)))

(define (string-move! x y)
  (substring-move-left! x 0 (string-length x) y 0))

(define (boolean-vector-ref vector index)
  (fix:= (char->integer #\t) (vector-8b-ref vector index)))

(define (boolean-vector-set! vector index value)
  (vector-8b-set! vector index (boolean->ascii value)))

(define (boolean-vector-all-elements? vector value)
  (boolean-subvector-all-elements? vector 0 (boolean-vector-length vector)
				   value))

(define (boolean-subvector-all-elements? vector start end value)
  (if (vector-8b-find-next-char vector start end (boolean->ascii (not value)))
      false
      true))

(define (boolean-subvector-uniform? vector start end)
  (if (and (fix:< start end)
	   (vector-8b-find-next-char
	    vector start end
	    (boolean->ascii (not (boolean-vector-ref vector start)))))
      false
      true))

(define (boolean-subvector-find-next vector start end value)
  (vector-8b-find-next-char vector start end (boolean->ascii value)))

(define make-boolean-vector make-string)
(define boolean-vector-length string-length)
(define boolean-vector=? string=?)
(define boolean-subvector-move-right! substring-move-right!)
(define boolean-subvector-move-left! substring-move-left!)
(define boolean-vector-move! string-move!)
(define boolean-vector-copy string-copy)

(define (boolean-subvector-fill! vector start end value)
  (vector-8b-fill! vector start end (boolean->ascii value)))

(define (boolean-vector-fill! vector value)
  (boolean-subvector-fill! vector 0 (boolean-vector-length vector) value))

(define (boolean->ascii boolean)
  (if boolean (char->integer #\t) (char->integer #\f)))