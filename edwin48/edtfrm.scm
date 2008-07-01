#| -*-Scheme-*-

$Id: edtfrm.scm,v 1.98 2008/01/30 20:02:00 cph Exp $

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

;;;; Editor Frame


;;; Editor Frame

(define-class editor-frame vanilla-window
  (screen
   root-inferior
   typein-inferior
   selected-window
   cursor-window
   properties))

(define (make-editor-frame root-screen main-buffer typein-buffer)
  (let ((window (make-object editor-frame)))
    (with-instance-variables editor-frame
			     window
			     (root-screen main-buffer typein-buffer)
      (set! superior #f)
      (set! x-size (screen-x-size root-screen))
      (set! y-size (screen-y-size root-screen))
      (set! redisplay-flags (list #f))
      (set! inferiors '())
      (set! properties (make-1d-table))
      (let ((main-window (make-buffer-frame window main-buffer #t))
	    (typein-window (make-buffer-frame window typein-buffer #f)))
	(set! screen root-screen)
	(set! root-inferior (find-inferior inferiors main-window))
	(set! typein-inferior (find-inferior inferiors typein-window))
	(set! selected-window main-window)
	(set! cursor-window main-window)
	(window-cursor-enable! main-window))
      (set-editor-frame-size! window x-size y-size))
    window))

(define (editor-frame-update-display! window display-style)
  ;; Returns #t if update is successfully completed (or unnecessary).
  ;; Assumes that interrupts are disabled.
  (notice-window-changes! (editor-frame-typein-window window))
  (let ((start (editor-frame-window0 window)))
    (notice-window-changes! start)
    (do ((window (window1+ start) (window1+ window)))
	((eq? window start))
      (notice-window-changes! window)))
  (with-instance-variables editor-frame window (display-style)
    (or (not (or (display-style/ignore-redisplay-flags? display-style)
		 (car redisplay-flags)))
	(let ((finished?
	       (window-update-display! window screen 0 0 0 x-size 0 y-size
				       display-style)))
	  (if finished?
	      (set-car! redisplay-flags #f))
	  finished?))))

(define (set-editor-frame-size! window x y)
  (with-instance-variables editor-frame window (x y)
    (usual==> window :set-size! x y)
    (set-inferior-start! root-inferior 0 0)
    (let ((y* (- y typein-y-size)))
      (set-inferior-start! typein-inferior 0 y*)
      (set-inferior-size! root-inferior x y*))
    (set-inferior-size! typein-inferior x-size typein-y-size)
    (if (< x (screen-x-size screen))
	(screen-clear-rectangle screen
				x (screen-x-size screen)
				0 (screen-y-size screen)
				#f))
    (if (< y (screen-y-size screen))
	(screen-clear-rectangle screen
				0 (screen-x-size screen)
				y (screen-y-size screen)
				#f))))

(define-method editor-frame :set-size!
  set-editor-frame-size!)

(define typein-y-size 1)

(define-method editor-frame (:new-root-window! window window*)
  (set! root-inferior (find-inferior inferiors window*))
  unspecific)

(define (editor-frame-window0 window)
  (with-instance-variables editor-frame window ()
    (window0 (inferior-window root-inferior))))

(define (editor-frame-typein-window window)
  (with-instance-variables editor-frame window ()
    (inferior-window typein-inferior)))

(define (editor-frame-selected-window window)
  (with-instance-variables editor-frame window ()
    selected-window))

(define (editor-frame-cursor-window window)
  (with-instance-variables editor-frame window ()
    cursor-window))

(define (editor-frame-root-window window)
  (with-instance-variables editor-frame window ()
    (inferior-window root-inferior)))

(define (editor-frame-screen window)
  (with-instance-variables editor-frame window ()
    screen))

(define (editor-frame-properties window)
  (with-instance-variables editor-frame window ()
    properties))

(define (editor-frame-windows window)
  (cons (editor-frame-typein-window window)
	(let ((start (editor-frame-window0 window)))
	  (let loop ((window start))
	    (cons window
		  (let ((window (window1+ window)))
		    (if (eq? window start)
			'()
			(loop window))))))))

(define (editor-frame-select-window! window window*)
  (with-instance-variables editor-frame window (window*)
    (if (not (buffer-frame? window*))
	(error "Attempt to select non-window" window*))
    (window-cursor-disable! cursor-window)
    (set! selected-window window*)
    (set-window-select-time! window* (increment-select-time!))
    (set! cursor-window window*)
    (window-cursor-enable! window*)))

(define (editor-frame-select-cursor! window window*)
  (with-instance-variables editor-frame window (window*)
    (if (not (buffer-frame? window*))
	(error "Attempt to select non-window" window*))
    (window-cursor-disable! cursor-window)
    (set! cursor-window window*)
    (window-cursor-enable! cursor-window)))

(define-method editor-frame (:button-event! editor-frame button x y)
  (call-with-values
      (lambda ()
	(inferior-containing-coordinates editor-frame x y buffer-frame?))
    (lambda (frame relative-x relative-y)
      (if frame
	  ;; Make sure the event is inside the text portion of the
	  ;; buffer, not in the modeline or other decoration.
	  (cond ((and (or (not (eq? frame
				    (editor-frame-typein-window editor-frame)))
			  (within-typein-edit?))
		      (< -1 relative-x (buffer-frame-x-size frame))
		      (< -1 relative-y (buffer-frame-y-size frame))
		      (local-comtab-entry
		       (buffer-comtabs (window-buffer frame))
		       button
		       (window-coordinates->mark frame
						 relative-x
						 relative-y)))
		 => (lambda (command)
		      (with-current-button-event
			  (make-button-event frame relative-x relative-y)
			(lambda () (execute-command command)))))
		((button-down? button)
		 (editor-beep)))))))

;;
;; Local Variables:
;; eval: (put 'with-instance-variables 'scheme-indent-function 3)
;; End:
;;