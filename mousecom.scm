;;; -*-Scheme-*-
;;;
;;; $Id: mousecom.scm,v 1.10 2007/01/05 21:19:23 cph Exp $
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

;;;; Mouse Commands


(define-command mouse-select
  "Select window the mouse is on."
  ()
  (lambda ()
    (select-window (button-event/window (current-button-event)))))

(define-command mouse-keep-one-window
  "Select window mouse is on, then kill all other windows."
  ()
  (lambda ()
    ((ref-command mouse-select))
    ((ref-command delete-other-windows))))

(define-command mouse-select-and-split
  "Select window mouse is on, then split it vertically in half."
  ()
  (lambda ()
    ((ref-command mouse-select))
    ((ref-command split-window-vertically) #f)))

(define-command mouse-set-point
  "Select window mouse is on, and move point to mouse position."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(select-window window)
	(set-current-point!
	 (or (window-coordinates->mark window
				       (button-event/x button-event)
				       (button-event/y button-event))
	     (buffer-end (window-buffer window))))))))

(define-command mouse-set-mark
  "Select window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (let ((window (button-event/window button-event)))
	(select-window window)
	(let ((mark
	       (or (window-coordinates->mark window
					     (button-event/x button-event)
					     (button-event/y button-event))
		   (buffer-end (window-buffer window)))))
	  (push-current-mark! mark)
	  (mark-flash mark))))))

(define-command mouse-show-event
  "Show the mouse position in the minibuffer."
  ()
  (lambda ()
    (let ((button-event (current-button-event)))
      (message "window: " (button-event/window button-event)
	       " x: " (button-event/x button-event)
	       " y: " (button-event/y button-event)))))

(define-command mouse-scroll-up
  "Scroll up mouse-scroll-increment lines."
  ()
  (lambda ()
    (scroll-window (selected-window)
		   (ref-variable mouse-scroll-increment)
		   (lambda () unspecific))))

(define-command mouse-scroll-down
  "Scroll down mouse-scroll-increment lines."
  ()
  (lambda ()
    (scroll-window (selected-window)
		   (- (ref-variable mouse-scroll-increment))
		   (lambda () unspecific))))

(define-variable mouse-scroll-increment
  "Number of lines by which a mouse-scroll event moves."
  5
  exact-nonnegative-integer?)

(define-command mouse-ignore
  "Don't do anything."
  ()
  (lambda () unspecific))

(define button1-down (make-down-button 0))
(define button2-down (make-down-button 1))
(define button3-down (make-down-button 2))
(define button4-down (make-down-button 3))
(define button5-down (make-down-button 4))
(define button1-up (make-up-button 0))
(define button2-up (make-up-button 1))
(define button3-up (make-up-button 2))
(define button4-up (make-up-button 3))
(define button5-up (make-up-button 4))
