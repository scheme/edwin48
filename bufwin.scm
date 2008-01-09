;;; -*-Scheme-*-
;;;
;;; $Id: bufwin.scm,v 1.319 2007/04/01 17:33:07 riastradh Exp $
;;;
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;;     2006, 2007 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;;; USA.
;;;
;;;

;;;; Buffer Windows: Base


;;; The following instance variables contain permanent marks, which
;;; must be copied if they are passed to someone outside the buffer
;;; window abstraction, because they are modified by side-effect.
;;;
;;; current-start-mark
;;; current-end-mark
;;; start-mark
;;; start-line-mark
;;; start-clip-mark
;;; end-clip-mark

(define-class buffer-window vanilla-window
  (
   ;; The buffer being displayed in this window.
   buffer

   ;; Caches for the values of buffer-local variables that are needed
   ;; for redisplay.
   truncate-lines?
   tab-width
   char-image-strings

   ;; The point marker in this window.
   point

   ;; This is the inferior window (of class CURSOR-WINDOW) that
   ;; displays the cursor for this window.
   cursor-inferior

   ;; This is the inferior window (of class BLANK-WINDOW) that keeps
   ;; the bottom of the window clear when there is no text in it.
   ;; This is only used when the end of the buffer is visible in the
   ;; window.  When not in use, it is moved offscreen so the window
   ;; clipping will prevent it from being updated.
   blank-inferior

   ;; The topmost and bottommost OUTLINE structures for this window,
   ;; respectively.  If only one line is shown, these are EQ?.
   start-outline
   end-outline

   ;; A previously allocated OUTLINE structure that is available for
   ;; reallocation.  Any other free OUTLINE structures are chained to
   ;; this one through its NEXT field.
   free-outline

   ;; A permanent right-inserting mark at the beginning of the text
   ;; line modelled by START-OUTLINE.
   current-start-mark

   ;; A permanent left-inserting mark at the end of the text line
   ;; modelled by END-OUTLINE.
   current-end-mark

   ;; The number of characters on the first line that are invisible,
   ;; i.e. off the top of the window.
   current-start-delta

   ;; The value of START-PARTIAL for the current window contents.
   current-start-partial

   ;; The Y position, relative to the window, of the top edge of
   ;; START-OUTLINE.  A non-positive number.
   current-start-y

   ;; The Y position, relative to the window, of the bottom edge of
   ;; END-OUTLINE.  A positive number.
   current-end-y

   ;; A previously allocated O3 structure that is available for
   ;; reallocation.  Any other free O3 structures are chained to this
   ;; one through its OUTLINE field.
   free-o3

   ;; This is normally #F.  However, when the normal display of the
   ;; buffer is overridden by a one-line message, as is commonly done
   ;; for the typein window, this variable contains the message
   ;; string.
   override-string

   ;; This permanent mark is the smallest that is visible in the
   ;; window.  If the window's start is not known, this is #F.
   start-mark

   ;; This permanent mark is at the beginning of the line containing
   ;; START-MARK.  It is #F if START-MARK is.  Note that this is the
   ;; same as CURRENT-START-MARK at the end of a display update, and
   ;; is changed due to point motion and scrolling.
   start-line-mark

   ;; This is the Y coordinate of START-LINE-MARK.  It is undefined if
   ;; START-LINE-MARK is #F, otherwise it is guaranteed to be
   ;; non-positive.
   start-line-y

   ;; This is the number of columns between START-LINE-MARK
   ;; (inclusive) and START-MARK (exclusive).  In other words, it is
   ;; the starting column of START-MARK.  This is undefined if
   ;; START-MARK is #F.
   start-column

   ;; If the character to the right of START-MARK is completely
   ;; visible, this is zero.  Otherwise, this is the number of columns
   ;; of that character that are visible.  This is undefined if
   ;; START-MARK is #F.
   start-partial

   ;; This contains the buffer's MODIFIED-TICK from the last time that
   ;; redisplay completed for this window.
   modified-tick

   ;; This contains the daemon that is invoked when the buffer's
   ;; display clipping is changed.
   clip-daemon

   ;; These variables delimit the region of the buffer that has been
   ;; unaffected by clipping since the last display update.  If the
   ;; clipping has not changed since then, they are #F.
   start-clip-mark
   end-clip-mark

   ;; This flag is set to #F at the end of a display update, and
   ;; subsequently set to a true value if the point has moved, or if
   ;; it was inside a changed region, or if it was outside a clipping
   ;; region, or any of several other conditions that could possibly
   ;; affect the validity of our idea about where point is.  However,
   ;; there are two possible true values: #T means that the START-MARK
   ;; for the window has been recomputed and is known to be correct.
   ;; 'SINCE-START-MARK means the new START-MARK has not yet been
   ;; computed.
   point-moved?

   ;; If true, this flag indicates that the window should be entirely
   ;; redrawn at the next display update.
   force-redraw?

   ;; These variables record where the last display update drew its
   ;; output.  SAVED-SCREEN is the screen on which it occurred.
   ;; SAVED-X-START and SAVED-Y-START is the position, in the screen's
   ;; coordinates, at which the window was located.  SAVED-XL,
   ;; SAVED-XU, SAVED-YL, and SAVED-YU (window's coordinates) delimit
   ;; the rectangular portion of the window that was drawn.
   saved-screen
   saved-x-start
   saved-y-start
   saved-xl
   saved-xu
   saved-yl
   saved-yu

   ;; This variable, if not #F, is a procedure that is called at
   ;; interesting times to generate a debugging trace.
   debug-trace))

;;;; Instance Variable Accessors

(define-integrable (%window-buffer window)
  (with-instance-variables buffer-window window () buffer))

(define-integrable (%window-group window)
  (buffer-group (%window-buffer window)))

(define-integrable (%set-window-buffer! window buffer*)
  (with-instance-variables buffer-window window (buffer*)
    (set! buffer buffer*)))

(define-integrable (%window-truncate-lines? window)
  (with-instance-variables buffer-window window () truncate-lines?))

(define-integrable (%set-window-truncate-lines?! window truncate-lines?*)
  (with-instance-variables buffer-window window (truncate-lines?*)
    (set! truncate-lines? truncate-lines?*)))

(define-integrable (%window-tab-width window)
  (with-instance-variables buffer-window window () tab-width))

(define-integrable (%set-window-tab-width! window tab-width*)
  (with-instance-variables buffer-window window (tab-width*)
    (set! tab-width tab-width*)))

(define-integrable (%window-char-image-strings window)
  (with-instance-variables buffer-window window () char-image-strings))

(define-integrable (%set-window-char-image-strings! window char-image-strings*)
  (with-instance-variables buffer-window window (char-image-strings*)
    (set! char-image-strings char-image-strings*)))

(define-integrable (%window-char->image window char)
  (vector-ref (%window-char-image-strings window)
	      (char->ascii char)))

(define-integrable (%window-point window)
  (with-instance-variables buffer-window window () point))

(define-integrable (%window-point-index window)
  (mark-index (%window-point window)))

(define-integrable (%set-window-point! window point*)
  (with-instance-variables buffer-window window (point*)
    (set! point point*)))

(define (%set-window-point-index! window index)
  #| Optimization causes lossage.  -- cph
  (let ((point (%window-point window)))
    (if point
	(set-mark-index! point index)
	(%set-window-point! window
			    (make-permanent-mark (%window-group window)
						 index
						 #t))))
  |#
  (%set-window-point! window
		      (make-permanent-mark (%window-group window)
					   index
					   #t)))

(define (set-window-point-index! window index)
  (%set-window-point-index! window index)
  (%set-buffer-point-index! (%window-buffer window) index))

(define-integrable (%window-cursor-inferior window)
  (with-instance-variables buffer-window window () cursor-inferior))

(define-integrable (%set-window-cursor-inferior! window inferior)
  (with-instance-variables buffer-window window (inferior)
    (set! cursor-inferior inferior)))

(define-integrable (%window-blank-inferior window)
  (with-instance-variables buffer-window window () blank-inferior))

(define-integrable (%set-window-blank-inferior! window inferior)
  (with-instance-variables buffer-window window (inferior)
    (set! blank-inferior inferior)))

(define-integrable (%window-start-outline window)
  (with-instance-variables buffer-window window () start-outline))

(define-integrable (%set-window-start-outline! window outline)
  (with-instance-variables buffer-window window (outline)
    (set! start-outline outline)))

(define-integrable (%window-end-outline window)
  (with-instance-variables buffer-window window () end-outline))

(define-integrable (%set-window-end-outline! window outline)
  (with-instance-variables buffer-window window (outline)
    (set! end-outline outline)))

(define-integrable (%window-free-outline window)
  (with-instance-variables buffer-window window () free-outline))

(define-integrable (%set-window-free-outline! window outline)
  (with-instance-variables buffer-window window (outline)
    (set! free-outline outline)))

(define-integrable (%window-current-start-mark window)
  (with-instance-variables buffer-window window () current-start-mark))

(define-integrable (%window-current-start-index window)
  (mark-index (%window-current-start-mark window)))

(define-integrable (%set-window-current-start-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! current-start-mark mark)))

(define-integrable (%window-current-end-mark window)
  (with-instance-variables buffer-window window () current-end-mark))

(define-integrable (%window-current-end-index window)
  (mark-index (%window-current-end-mark window)))

(define-integrable (%set-window-current-end-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! current-end-mark mark)))

(define-integrable (%window-current-start-delta window)
  (with-instance-variables buffer-window window () current-start-delta))

(define-integrable (%set-window-current-start-delta! window delta)
  (with-instance-variables buffer-window window (delta)
    (set! current-start-delta delta)))

(define-integrable (%window-current-start-partial window)
  (with-instance-variables buffer-window window () current-start-partial))

(define-integrable (%set-window-current-start-partial! window partial)
  (with-instance-variables buffer-window window (partial)
    (set! current-start-partial partial)))

(define-integrable (%window-current-start-y window)
  (with-instance-variables buffer-window window () current-start-y))

(define-integrable (%set-window-current-start-y! window y)
  (with-instance-variables buffer-window window (y)
    (set! current-start-y y)))

(define-integrable (%window-current-end-y window)
  (with-instance-variables buffer-window window () current-end-y))

(define-integrable (%set-window-current-end-y! window y)
  (with-instance-variables buffer-window window (y)
    (set! current-end-y y)))

(define-integrable (%window-free-o3 window)
  (with-instance-variables buffer-window window () free-o3))

(define-integrable (%set-window-free-o3! window o3)
  (with-instance-variables buffer-window window (o3)
    (set! free-o3 o3)))

(define-integrable (%window-override-string window)
  (with-instance-variables buffer-window window () override-string))

(define-integrable (%set-window-override-string! window string)
  (with-instance-variables buffer-window window (string)
    (set! override-string string)))

(define-integrable (%window-start-mark window)
  (with-instance-variables buffer-window window () start-mark))

(define-integrable (%window-start-index window)
  (mark-index (%window-start-mark window)))

(define-integrable (%set-window-start-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! start-mark mark)))

(define-integrable (%window-start-line-mark window)
  (with-instance-variables buffer-window window () start-line-mark))

(define-integrable (%window-start-line-index window)
  (mark-index (%window-start-line-mark window)))

(define-integrable (%set-window-start-line-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! start-line-mark mark)))

(define-integrable (%window-start-line-y window)
  (with-instance-variables buffer-window window () start-line-y))

(define-integrable (%set-window-start-line-y! window y)
  (with-instance-variables buffer-window window (y)
    (set! start-line-y y)))

(define-integrable (%window-start-column window)
  (with-instance-variables buffer-window window () start-column))

(define-integrable (%set-window-start-column! window column)
  (with-instance-variables buffer-window window (column)
    (set! start-column column)))

(define-integrable (%window-start-partial window)
  (with-instance-variables buffer-window window () start-partial))

(define-integrable (%set-window-start-partial! window partial)
  (with-instance-variables buffer-window window (partial)
    (set! start-partial partial)))

(define-integrable (%window-modified-tick window)
  (with-instance-variables buffer-window window () modified-tick))

(define-integrable (%set-window-modified-tick! window tick)
  (with-instance-variables buffer-window window (tick)
    (set! modified-tick tick)))

(define-integrable (%window-start-changes-index window)
  (group-start-changes-index (%window-group window)))

(define-integrable (%window-end-changes-index window)
  (group-end-changes-index (%window-group window)))

(define-integrable (%window-clip-daemon window)
  (with-instance-variables buffer-window window () clip-daemon))

(define-integrable (%set-window-clip-daemon! window daemon)
  (with-instance-variables buffer-window window (daemon)
    (set! clip-daemon daemon)))

(define-integrable (%window-start-clip-mark window)
  (with-instance-variables buffer-window window () start-clip-mark))

(define-integrable (%window-start-clip-index window)
  (mark-index (%window-start-clip-mark window)))

(define-integrable (%set-window-start-clip-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! start-clip-mark mark)))

(define-integrable (%window-end-clip-mark window)
  (with-instance-variables buffer-window window () end-clip-mark))

(define-integrable (%window-end-clip-index window)
  (mark-index (%window-end-clip-mark window)))

(define-integrable (%set-window-end-clip-mark! window mark)
  (with-instance-variables buffer-window window (mark)
    (set! end-clip-mark mark)))

(define-integrable (%window-point-moved? window)
  (with-instance-variables buffer-window window () point-moved?))

(define-integrable (%set-window-point-moved?! window point-moved?*)
  (with-instance-variables buffer-window window (point-moved?*)
    (set! point-moved? point-moved?*)))

(define-integrable (%window-force-redraw? window)
  (with-instance-variables buffer-window window () force-redraw?))

(define-integrable (%set-window-force-redraw?! window force-redraw?*)
  (with-instance-variables buffer-window window (force-redraw?*)
    (set! force-redraw? force-redraw?*)))

(define-integrable (%window-saved-screen window)
  (with-instance-variables buffer-window window () saved-screen))

(define-integrable (%set-window-saved-screen! window screen)
  (with-instance-variables buffer-window window (screen)
    (set! saved-screen screen)))

(define-integrable (%window-saved-x-start window)
  (with-instance-variables buffer-window window () saved-x-start))

(define-integrable (%set-window-saved-x-start! window x-start)
  (with-instance-variables buffer-window window (x-start)
    (set! saved-x-start x-start)))

(define-integrable (%window-saved-y-start window)
  (with-instance-variables buffer-window window () saved-y-start))

(define-integrable (%set-window-saved-y-start! window y-start)
  (with-instance-variables buffer-window window (y-start)
    (set! saved-y-start y-start)))

(define-integrable (%window-saved-xl window)
  (with-instance-variables buffer-window window () saved-xl))

(define-integrable (%set-window-saved-xl! window xl)
  (with-instance-variables buffer-window window (xl)
    (set! saved-xl xl)))

(define-integrable (%window-saved-xu window)
  (with-instance-variables buffer-window window () saved-xu))

(define-integrable (%set-window-saved-xu! window xu)
  (with-instance-variables buffer-window window (xu)
    (set! saved-xu xu)))

(define-integrable (%window-saved-yl window)
  (with-instance-variables buffer-window window () saved-yl))

(define-integrable (%set-window-saved-yl! window yl)
  (with-instance-variables buffer-window window (yl)
    (set! saved-yl yl)))

(define-integrable (%window-saved-yu window)
  (with-instance-variables buffer-window window () saved-yu))

(define-integrable (%set-window-saved-yu! window yu)
  (with-instance-variables buffer-window window (yu)
    (set! saved-yu yu)))

(define-integrable (%window-debug-trace window)
  (with-instance-variables buffer-window window () debug-trace))

(define-integrable (%set-window-debug-trace! window procedure)
  (with-instance-variables buffer-window window (procedure)
    (set! debug-trace procedure)))

;;;; Outlines

(define-structure (outline
		   (constructor %make-outline)
		   (print-procedure
		    (unparser/standard-method 'OUTLINE
		      (lambda (state outline)
			(unparse-string state "index: ")
			(unparse-object state (outline-index-length outline))
			(unparse-string state " y: ")
			(unparse-object state (outline-y-size outline))))))
  ;; The number of characters in the text line.  This is exclusive of
  ;; the newlines at the line's beginning and end, if any.
  index-length

  ;; The number of screen lines that are occupied by this text line.
  y-size

  ;; A pointer to the previous outline structure, the one representing
  ;; the text line that appears directly above this line.
  previous

  ;; A pointer to the next outline structure, the one representing the
  ;; text line that appears directly below this line.
  next)

(define (make-outline window index-length y-size previous next)
  (let ((outline
	 (let ((outline (%window-free-outline window)))
	   (if (%window-free-outline window)
	       (begin
		 (let ((free (outline-next outline)))
		   (if free (set-outline-previous! free #f))
		   (%set-window-free-outline! window free))
		 (set-outline-index-length! outline index-length)
		 (set-outline-y-size! outline y-size)
		 (set-outline-previous! outline previous)
		 (set-outline-next! outline next)
		 outline)
	       (%make-outline index-length y-size previous next)))))
    (if previous (set-outline-next! previous outline))
    (if next (set-outline-previous! next outline))
    outline))

(define (deallocate-outlines! window start-outline end-outline)
  (let ((free-outline (%window-free-outline window)))
    (if (outline-next end-outline)
	(set-outline-previous! (outline-next end-outline) #f))
    (set-outline-next! end-outline free-outline)
    (if free-outline
	(set-outline-previous! free-outline end-outline)))
  (if (outline-previous start-outline)
      (set-outline-next! (outline-previous start-outline) #f))
  (set-outline-previous! start-outline #f)
  (%set-window-free-outline! window start-outline))

(define-integrable (outline-last outline)
  (do ((outline outline (outline-next outline)))
      ((not (outline-next outline)) outline)))

(define-integrable (outline-end-y outline start-y)
  (do ((outline outline (outline-next outline))
       (y start-y (fix:+ y (outline-y-size outline))))
      ((not outline) y)))

(define-integrable (outline-start-y outline end-y)
  (do ((outline outline (outline-previous outline))
       (y end-y (fix:- y (outline-y-size outline))))
      ((not outline) y)))

(define-structure (o3
		   (constructor %make-o3)
		   (print-procedure
		    (unparser/standard-method 'O3
		      (lambda (state o3)
			(unparse-string state "index: ")
			(unparse-object state (o3-index o3))
			(unparse-string state " y: ")
			(unparse-object state (o3-y o3))
			(if (outline? (o3-outline o3))
			    (begin
			      (unparse-string state " ")
			      (unparse-object state (o3-outline o3))))))))
  outline
  index
  y)

(define (make-o3 window outline index y)
  (let ((o3 (%window-free-o3 window)))
    (if o3
	(begin
	  (%set-window-free-o3! window (o3-outline o3))
	  (set-o3-outline! o3 outline)
	  (set-o3-index! o3 index)
	  (set-o3-y! o3 y)
	  o3)
	(%make-o3 outline index y))))

(define (deallocate-o3! window o3)
  (set-o3-outline! o3 (%window-free-o3 window))
  (%set-window-free-o3! window o3))

;;;; Narrowing

(define-integrable (%window-group-start-mark window)
  (group-display-start (%window-group window)))

(define-integrable (%window-group-end-mark window)
  (group-display-end (%window-group window)))

(define-integrable (%window-group-start-index window)
  (mark-index (%window-group-start-mark window)))

(define-integrable (%window-group-end-index window)
  (mark-index (%window-group-end-mark window)))

(define-integrable (%window-group-start-index? window index)
  (fix:<= index (%window-group-start-index window)))

(define-integrable (%window-group-end-index? window index)
  (fix:>= index (%window-group-end-index window)))

(define-integrable (%window-line-start-index window index)
  (let ((start (%window-group-start-index window)))
    (or (%find-previous-newline (%window-group window) index start)
	start)))

(define-integrable (%window-line-end-index window index)
  (let ((end (%window-group-end-index window)))
    (or (%find-next-newline (%window-group window) index end)
	end)))

(define (%window-line-start-index? window index)
  (or (%window-group-start-index? window index)
      (char=? (xstring-ref (group-text (%window-group window))
			   (fix:- (group-index->position-integrable
				   (%window-group window)
				   index
				   #f)
				  1))
	      #\newline)))

(define (%window-line-end-index? window index)
  (or (%window-group-end-index? window index)
      (char=? (xstring-ref (group-text (%window-group window))
			   (group-index->position-integrable
			    (%window-group window)
			    index
			    #t))
	      #\newline)))

(define (clip-mark-to-display window mark)
  (if (not (mark? mark))
      (error:wrong-type-argument mark "mark" 'CLIP-MARK-TO-DISPLAY))
  (if (and (%window-point window)
	   (not (mark~ (%window-point window) mark)))
      (error:bad-range-argument mark 'CLIP-MARK-TO-DISPLAY))
  (cond ((group-display-start-index? (mark-group mark) (mark-index mark))
	 (group-display-start (mark-group mark)))
	((group-display-end-index? (mark-group mark) (mark-index mark))
	 (group-display-end (mark-group mark)))
	(else
	 mark)))

;;;; Utilities

(define-integrable (%window-extract-string window start end)
  (group-extract-string (%window-group window) start end))

(define-integrable (%window-modeline-event! window type)
  (window-modeline-event! (window-superior window) type))

;;;; Standard Methods

(define-method buffer-window (:initialize! window window*)
  (usual==> window :initialize! window*)
  (%reset-window-structures! window)
  (%clear-window-buffer-state! window))

(define-method buffer-window (:kill! window)
  (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (%unset-window-buffer! window)
    (set-interrupt-enables! mask))
  (usual==> window :kill!))

(define-method buffer-window (:salvage! window)
  (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (%set-window-point-index! window (%window-group-start-index window))
    (%set-window-point-moved?! window 'SINCE-START-SET)
    (%reset-window-structures! window)
    (buffer-window/redraw! window)
    (set-interrupt-enables! mask)
    unspecific))

(define-method buffer-window (:set-size! window x y)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-size! x y))
  (buffer-window/redraw! window)
  (%release-window-outlines! window)
  (set-window-size! window x y)
  (%set-window-point-moved?! window 'SINCE-START-SET))

(define-method buffer-window (:set-x-size! window x)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-x-size! x))
  (buffer-window/redraw! window)
  (set-window-x-size! window x)
  (%set-window-point-moved?! window 'SINCE-START-SET))

(define-method buffer-window (:set-y-size! window y)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-y-size! y))
  (buffer-window/redraw! window)
  (%release-window-outlines! window)
  (set-window-y-size! window y)
  (%set-window-point-moved?! window 'SINCE-START-SET))

(define (buffer-window/cursor-enable! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'cursor-enable!))
  (==> (inferior-window (%window-cursor-inferior window)) :enable!))

(define (buffer-window/cursor-disable! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'cursor-disable!))
  (==> (inferior-window (%window-cursor-inferior window)) :disable!))

;;;; Update

(define (buffer-window:update-display! window screen x-start y-start
				       xl xu yl yu display-style)
  ;; Assumes that interrupts are disabled.
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window ':update-display!
				    screen x-start y-start xl xu yl yu
				    display-style))
  (%set-window-saved-screen! window screen)
  (%set-window-saved-x-start! window x-start)
  (%set-window-saved-y-start! window y-start)
  (%set-window-saved-xl! window xl)
  (%set-window-saved-xu! window xu)
  (%set-window-saved-yl! window yl)
  (%set-window-saved-yu! window yu)
  (update-buffer-window! window screen x-start y-start xl xu yl yu
			 display-style))

(define-method buffer-window :update-display!
  buffer-window:update-display!)

(define (buffer-window/direct-update! window display-style)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'direct-update!
				    display-style))
  (and (%window-saved-screen window)
       (begin
	 (%notice-window-changes! window)
	 (with-screen-in-update (%window-saved-screen window) display-style
	   (lambda ()
	     (let ((finished?
		    (update-buffer-window! window
					   (%window-saved-screen window)
					   (%window-saved-x-start window)
					   (%window-saved-y-start window)
					   (%window-saved-xl window)
					   (%window-saved-xu window)
					   (%window-saved-yl window)
					   (%window-saved-yu window)
					   display-style)))
	       (if finished?
		   (set-car! (window-redisplay-flags window) #f))
	       finished?))))))

(define (update-buffer-window! window screen x-start y-start xl xu yl yu
			       display-style)
  (if (%window-override-string window)
      (update-override-string! window screen x-start y-start xl xu yl yu)
      (update-outlines! window))
  (update-inferior! (%window-blank-inferior window) screen x-start y-start
		    xl xu yl yu display-style
		    blank-window:update-display!)
  (update-inferior! (%window-cursor-inferior window) screen x-start y-start
		    xl xu yl yu display-style
		    cursor-window:update-display!)
  #t)

(define (buffer-window/redraw! window)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'force-redraw!))
  (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (%set-window-force-redraw?! window #t)
    (%recache-window-buffer-local-variables! window)
    (%clear-window-incremental-redisplay-state! window)
    (window-needs-redisplay! window)
    (set-interrupt-enables! mask)
    unspecific))

;;;; Window State

(define (%reset-window-structures! window)
  (set-window-inferiors! window '())
  (%set-window-cursor-inferior! window (make-inferior window cursor-window))
  (%set-window-blank-inferior! window (make-inferior window blank-window))
  (%release-window-outlines! window)
  (%set-window-free-o3! window #f)
  (%set-window-override-string! window #f)
  (%set-window-clip-daemon! window (make-clip-daemon window))
  (%set-window-debug-trace! window #f)
  (%set-window-saved-screen! window #f)
  (%set-window-force-redraw?! window #f))

(define (%release-window-outlines! window)
  (%set-window-start-outline! window #f)
  (%set-window-end-outline! window #f)
  (%set-window-free-outline! window #f))

(define (%clear-window-buffer-state! window)
  (%set-window-buffer! window #f)
  (%set-window-point! window #f)
  (clear-window-start! window)
  (%clear-window-incremental-redisplay-state! window))

(define (%clear-window-incremental-redisplay-state! window)
  (if (%window-start-outline window)
      (begin
	(deallocate-outlines! window
			      (%window-start-outline window)
			      (%window-end-outline window))
	(%set-window-start-outline! window #f)
	(%set-window-end-outline! window #f)))
  (if (%window-current-start-mark window)
      (begin
	(mark-temporary! (%window-current-start-mark window))
	(%set-window-current-start-mark! window #f)
	(mark-temporary! (%window-current-end-mark window))
	(%set-window-current-end-mark! window #f)
	(%set-window-current-start-delta! window #f)
	(%set-window-current-start-partial! window #f)))
  (%clear-window-outstanding-changes! window))

(define-integrable (%clear-window-outstanding-changes! window)
  (if (%window-buffer window)
      (update-modified-tick! window))
  (if (%window-start-clip-mark window)
      (begin
	(mark-temporary! (%window-start-clip-mark window))
	(%set-window-start-clip-mark! window #f)
	(mark-temporary! (%window-end-clip-mark window))
	(%set-window-end-clip-mark! window #f)))
  (%set-window-point-moved?! window #f))

(define-integrable (update-modified-tick! window)
  (%set-window-modified-tick! window
			      (group-modified-tick (%window-group window))))

(define (%recache-window-buffer-local-variables! window)
  (let ((maybe-recache
	 (lambda (read write new-value)
	   (let ((old-value (read window)))
	     (if (not (eqv? new-value old-value))
		 (begin
		   (%set-window-force-redraw?! window #t)
		   (clear-window-start! window)
		   (write window new-value))))))
	(buffer (%window-buffer window)))
    (maybe-recache
     %window-truncate-lines?
     %set-window-truncate-lines?!
     (or (variable-local-value buffer (ref-variable-object truncate-lines))
	 (and (variable-local-value
	       buffer
	       (ref-variable-object truncate-partial-width-windows))
	      (window-has-horizontal-neighbor? (window-superior window)))))
    (maybe-recache
     %window-tab-width
     %set-window-tab-width!
     (variable-local-value buffer (ref-variable-object tab-width)))
    (maybe-recache
     %window-char-image-strings
     %set-window-char-image-strings!
     (variable-local-value buffer (ref-variable-object char-image-strings)))))

;;;; Buffer and Point

(define-integrable (buffer-window/buffer window)
  (%window-buffer window))

(define (buffer-window/set-buffer! window new-buffer)
  ;; Interrupts must be disabled when this is called.
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-buffer! new-buffer))
  (if (not (buffer? new-buffer))
      (error:wrong-type-argument new-buffer "buffer" 'SET-WINDOW-BUFFER!))
  (if (%window-buffer window)
      (%unset-window-buffer! window))
  (%set-window-buffer! window new-buffer)
  (%recache-window-buffer-local-variables! window)
  (let ((group (%window-group window)))
    (add-group-clip-daemon! group (%window-clip-daemon window))
    (%set-window-point-index! window (mark-index (group-point group))))
  (if (and (buffer-display-start new-buffer)
	   (window-x-size window))
      (set-new-coordinates! window
			    (mark-index (buffer-display-start new-buffer))
			    0
			    #f))
  (buffer-window/redraw! window))

(define (%unset-window-buffer! window)
  ;; Interrupts must be disabled when this is called.
  (let ((buffer (%window-buffer window)))
    (if (%window-debug-trace window)
	((%window-debug-trace window) 'window window 'unset-buffer! buffer))
    (set-buffer-display-start!
     buffer
     (mark-permanent! (buffer-window/start-mark window)))
    (%set-buffer-point! buffer (buffer-window/point window)))
  (remove-group-clip-daemon! (%window-group window)
			     (%window-clip-daemon window))
  (%clear-window-buffer-state! window))

(define-integrable (buffer-window/point window)
  (%window-point window))

(define (buffer-window/set-point! window mark)
  (let ((mark (clip-mark-to-display window mark)))
    (if (%window-debug-trace window)
	((%window-debug-trace window) 'window window 'set-point! mark))
    (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
      (set-window-point-index! window (mark-index mark))
      (%set-window-point-moved?! window 'SINCE-START-SET)
      (window-needs-redisplay! window)
      (set-interrupt-enables! mask)
      unspecific)))

;;;; Start Mark

(define (buffer-window/start-mark window)
  (guarantee-start-mark! window)
  (mark-temporary-copy (%window-start-mark window)))

(define (buffer-window/set-start-mark! window mark force?)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-start-mark! mark))
  (set-new-coordinates! window
			(mark-index (clip-mark-to-display window mark))
			0
			(and force? (buffer-window/y-center window))))

(define (buffer-window/scroll-y-relative! window y-delta)
  (if (not (fix:= y-delta 0))
      (begin
	(if (%window-debug-trace window)
	    ((%window-debug-trace window) 'window window 'scroll-y-relative!
					  y-delta))
	(guarantee-start-mark! window)
	(set-new-coordinates! window
			      (%window-start-line-index window)
			      (fix:- (%window-start-line-y window) y-delta)
			      (if (fix:> y-delta 0)
				  0
				  (fix:- (window-y-size window) 1))))))

(define (buffer-window/scroll-y-absolute! window y-point)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'scroll-y-absolute!
				    y-point))
  (if (not (and (fix:<= 0 y-point)
		(fix:< y-point (window-y-size window))))
      (error:bad-range-argument y-point 'WINDOW-SCROLL-Y-ABSOLUTE!))
  (let ((cws
	 (compute-window-start window (%window-point-index window) y-point)))
    (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
      (set-window-start! window cws)
      (set-interrupt-enables! mask)
      unspecific)))

(define (buffer-window/y-center window)
  (let ((y-size (window-y-size window)))
    (let ((result
	   (round->exact
	    (* y-size (/ (ref-variable cursor-centering-point) 100)))))
      (if (< result y-size)
	  result
	  (- y-size 1)))))

(define-variable cursor-centering-point
  "The distance from the top of the window at which to center the point.
This number is a percentage, where 0 is the window's top and 100 the bottom."
  50
  (lambda (cursor-centering-point)
    (and (real? cursor-centering-point)
	 (<= 0 cursor-centering-point 100))))

(define (set-new-coordinates! window index y point-y)
  (let ((cws (compute-window-start window index y)))
    (let ((start (vector-ref cws 0))
	  (y-start (vector-ref cws 1)))
      (cond ((predict-index-visible? window start y-start
				     (%window-point-index window))
	     (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	       (set-window-start! window cws)
	       (set-interrupt-enables! mask)
	       unspecific))
	    (point-y
	     (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	       (set-window-point-index!
		window
		(or (predict-index window start y-start
				   (vector-ref cws 4) point-y)
		    (%window-group-end-index window)))
	       (set-window-start! window cws)
	       (set-interrupt-enables! mask)
	       unspecific))))))

(define (set-window-start! window cws)
  (let ((start-line (vector-ref cws 0))
	(start (vector-ref cws 2)))
    (if (fix:= start-line start)
	(if (%window-start-line-mark window)
	    (begin
	      (set-mark-index! (%window-start-line-mark window) start-line)
	      (if (not (eq? (%window-start-line-mark window)
			    (%window-start-mark window)))
		  (begin
		    (mark-temporary! (%window-start-mark window))
		    (%set-window-start-mark!
		     window
		     (%window-start-line-mark window)))))
	    (let ((mark
		   (make-permanent-mark (%window-group window) start-line #f)))
	      (%set-window-start-line-mark! window mark)
	      (%set-window-start-mark! window mark)))
	(if (%window-start-line-mark window)
	    (begin
	      (set-mark-index! (%window-start-line-mark window) start-line)
	      (if (eq? (%window-start-line-mark window)
		       (%window-start-mark window))
		  (%set-window-start-mark!
		   window
		   (make-permanent-mark (%window-group window) start #f))
		  (set-mark-index! (%window-start-mark window) start)))
	    (let ((group (%window-group window)))
	      (%set-window-start-line-mark!
	       window
	       (make-permanent-mark group start-line #f))
	      (%set-window-start-mark!
	       window
	       (make-permanent-mark group start #f))))))
  (%set-window-start-line-y! window (vector-ref cws 1))
  (%set-window-start-column! window (vector-ref cws 3))
  (%set-window-start-partial! window (vector-ref cws 4))
  (if (eq? (%window-point-moved? window) 'SINCE-START-SET)
      (%set-window-point-moved?! window #t))
  (window-needs-redisplay! window))

(define (clear-window-start! window)
  (if (%window-start-line-mark window)
      (begin
	(mark-temporary! (%window-start-line-mark window))
	(%set-window-start-line-mark! window #f)))
  (if (%window-start-mark window)
      (begin
	(mark-temporary! (%window-start-mark window))
	(%set-window-start-mark! window #f)))
  (%set-window-start-line-y! window 0)
  (%set-window-start-column! window 0)
  (%set-window-start-partial! window 0))

(define (guarantee-start-mark! window)
  (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (%guarantee-start-mark! window)
    (set-interrupt-enables! mask)
    unspecific))

(define (%guarantee-start-mark! window)
  (let ((index-at!
	 (lambda (index y)
	   (set-window-start! window (compute-window-start window index y)))))
    (if (not (%window-start-line-mark window))
	(index-at! (%window-point-index window)
		   (buffer-window/y-center window))
	(let ((start-line (%window-start-line-index window)))
	  (cond ((not (%window-line-start-index? window start-line))
		 (index-at! (%window-start-index window) 0))
		((eq? (%window-point-moved? window) 'SINCE-START-SET)
		 (let ((point (%window-point-index window)))
		   (if (or (%window-start-clip-mark window)
			   (fix:> (group-modified-tick (%window-group window))
				  (%window-modified-tick window))
			   (not (%window-current-start-mark window))
			   (fix:<
			    point
			    (if (fix:< (%window-current-start-y window) 0)
				(fix:+ (%window-current-start-index window)
				       (outline-index-length
					(%window-start-outline window)))
				(%window-current-start-index window)))
			   (fix:> point
				  (if (fix:> (%window-current-end-y window)
					     (window-y-size window))
				      (fix:- (%window-current-end-index window)
					     (outline-index-length
					      (%window-end-outline window)))
				      (%window-current-end-index window))))
		       (let ((start-y (%window-start-line-y window))
			     (y-size (window-y-size window))
			     (scroll-step
			      (ref-variable scroll-step
					    (%window-buffer window))))
			 (if (fix:= 0 scroll-step)
			     (if (predict-y-limited window start-line
						    start-y point
						    0 y-size)
				 (%set-window-point-moved?! window #t)
				 (index-at! point
					    (buffer-window/y-center window)))
			     (let ((y
				    (predict-y-limited window start-line
						       start-y point
						       (fix:- 0 scroll-step)
						       (fix:+ y-size
							      scroll-step))))
			       (cond ((not y)
				      (index-at!
				       point
				       (buffer-window/y-center window)))
				     ((fix:< y 0)
				      (index-at! point (fix:+ y scroll-step)))
				     ((fix:>= y y-size)
				      (index-at! point
						 (fix:- y scroll-step)))))))
		       (%set-window-point-moved?! window #t)))))))))

(define-variable scroll-step
  "The number of lines to try scrolling a window by when point moves out.
If that fails to bring point back on screen, point is centered instead.
If this is zero, point is always centered after it moves off screen."
  0
  (lambda (scroll-step)
    (and (fix:fixnum? scroll-step)
	 (fix:>= scroll-step 0))))

;;;; Override Message

(define (buffer-window/override-message window)
  (%window-override-string window))

(define (buffer-window/set-override-message! window message)
  (if (%window-debug-trace window)
      ((%window-debug-trace window) 'window window 'set-override-message!
				    message))
  (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (%set-window-override-string! window message)
    (window-needs-redisplay! window)
    (set-interrupt-enables! mask)
    unspecific))

(define (buffer-window/clear-override-message! window)
  (if (%window-override-string window)
      (begin
	(if (%window-debug-trace window)
	    ((%window-debug-trace window) 'window window
					  'clear-override-message!))
	(let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	  (%set-window-override-string! window #f)
	  (buffer-window/redraw! window)
	  (set-interrupt-enables! mask)
	  unspecific))))

(define (update-override-string! window screen x-start y-start xl xu yl yu)
  ;; This should probably update like any other string, paying
  ;; attention to TRUNCATE-LINES? and going to multiple lines if
  ;; necessary.  For now we'll force it to be truncated to a single
  ;; line, which is fine as long as the minibuffer is only one line.
  (if (and (fix:= yl 0) (not (fix:= yu 0)))
      (let ((string (%window-override-string window))
	    (xl (fix:+ x-start xl))
	    (xu (fix:+ x-start xu))
	    (results substring-image-results))
	(let ((end (string-length string))
	      (line
	       (screen-get-output-line screen (fix:+ y-start yl) xl xu #f)))
	  (substring-image! string 0 end
			    line xl (fix:- xu 1)
			    #f 0 results
			    (%window-char-image-strings window))
	  (if (fix:= (vector-ref results 0) end)
	      (do ((x (vector-ref results 1) (fix:+ x 1)))
		  ((fix:= x xu))
		(string-set! line x #\space))
	      (string-set! line (fix:- xu 1) #\$))
	  (set-inferior-start! (%window-cursor-inferior window)
			       (vector-ref results 1)
			       0))))
  (%update-blank-inferior! window 1 #t)
  (update-modified-tick! window))

;;;; Update Finalization

(define (set-outlines! window start end)
  (%set-window-start-outline! window (o3-outline start))
  (%set-window-end-outline! window (o3-outline end))
  (if (%window-current-start-mark window)
      (begin
	(set-mark-index! (%window-current-start-mark window) (o3-index start))
	(set-mark-index! (%window-current-end-mark window) (o3-index end)))
      (begin
	(%set-window-current-start-mark!
	 window
	 (make-permanent-mark (%window-group window) (o3-index start) #f))
	(%set-window-current-end-mark!
	 window
	 (make-permanent-mark (%window-group window) (o3-index end) #t))))
  (%set-window-current-start-delta! window
				    (fix:- (%window-start-index window)
					   (%window-start-line-index window)))
  (%set-window-current-start-partial! window (%window-start-partial window))
  (%set-window-current-start-y! window (o3-y start))
  (%set-window-current-end-y! window (o3-y end))
  (deallocate-o3! window start)
  (deallocate-o3! window end)
  (update-blank-inferior! window #t)
  (update-cursor! window)
  (%window-modeline-event! window 'SET-OUTLINES))

(define (update-blank-inferior! window signal?)
  (%update-blank-inferior! window (%window-current-end-y window) signal?))

(define (%update-blank-inferior! window end-y signal?)
  (let ((inferior (%window-blank-inferior window)))
    (if (fix:< end-y (window-y-size window))
	(begin
	  (%set-window-x-size! (inferior-window inferior)
			       (window-x-size window))
	  (%set-window-y-size! (inferior-window inferior)
			       (fix:- (window-y-size window) end-y))
	  (%set-inferior-x-start! inferior 0)
	  (%set-inferior-y-start! inferior end-y)
	  (if signal?
	      (setup-redisplay-flags! (inferior-redisplay-flags inferior))))
	(begin
	  (%set-inferior-x-start! inferior #f)
	  (%set-inferior-y-start! inferior #f)))))

(define (update-cursor! window)
  (let ((xy (buffer-window/point-coordinates window)))
    (if (and (fix:<= 0 (car xy))
	     (fix:< (car xy) (window-x-size window))
	     (fix:<= 0 (cdr xy))
	     (fix:< (cdr xy) (window-y-size window)))
	(set-inferior-position! (%window-cursor-inferior window) xy)
	(begin
	  (if point-not-visible-error?
	      (error "point not visible at end of redisplay"))
	  (set-window-point-index!
	   window
	   (or (predict-index window
			      (%window-start-line-index window)
			      (%window-start-line-y window)
			      (%window-start-partial window)
			      0)
	       (%window-group-end-index window)))
	  (update-cursor! window)))))

(define point-not-visible-error?
  #f)
