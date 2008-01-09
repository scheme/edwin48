#| -*-Scheme-*-

$Id: os2com.scm,v 1.10 2007/01/05 21:19:24 cph Exp $

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

;;;; OS/2 Presentation Manager Commands


(define-command set-foreground-color
  "Set foreground (text) color to COLOR."
  "sSet foreground color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (os2-screen/set-foreground-color! screen (os2/find-color name))
      (update-screen! screen #t))))

(define-command set-background-color
  "Set background (text) color to COLOR."
  "sSet background color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (os2-screen/set-background-color! screen (os2/find-color name))
      (update-screen! screen #t))))

(define-command define-color-name
  "Globally define COLOR-NAME to be COLOR.
This does not affect any colors on the screen,
but changes the meaning of COLOR-NAME when it is used in the future."
  "sDefine color\nsDefine color to"
  (lambda (name color)
    (os2/define-color name color)))

(define-command set-font
  "Set font to be used for drawing text."
  "sSet font"
  (lambda (font)
    (let ((screen (selected-screen)))
      (os2-screen/set-font! screen font)
      (update-screen! screen #t))))

(define-command set-frame-size
  "Set size of editor frame to WIDTH x HEIGHT."
  "nFrame width (chars)\nnFrame height (chars)"
  (lambda (width height)
    (os2-screen/set-size! (selected-screen) (max 2 width) (max 2 height))))

(define-command set-frame-position
  "Set position of editor frame to (X,Y)."
  "nFrame X position (pels)\nnFrame Y position (pels)"
  (lambda (x y)
    (os2-screen/set-position! (selected-screen) x y)))

(define-command show-frame-size
  "Show size of editor frame."
  ()
  (lambda ()
    (let ((screen (selected-screen)))
      (message "Frame is "
	       (screen-x-size screen)
	       " chars wide and "
	       (screen-y-size screen)
	       " chars high ("
	       (screen-pel-width screen)
	       "x"
	       (screen-pel-height screen)
	       " pels)"))))

(define-command show-frame-position
  "Show position of editor frame.
This is the position of the lower left-hand corner of the frame border
surrounding the frame, relative to the lower left-hand corner of the
desktop."
  ()
  (lambda ()
    (call-with-values (lambda () (os2-screen/get-position (selected-screen)))
      (lambda (x y)
	(message "Frame's lower left-hand corner is at (" x "," y ")")))))

;; For upwards compatibility
(define edwin-command$set-screen-size edwin-command$set-frame-size)
(define edwin-command$set-screen-position edwin-command$set-frame-position)
(define edwin-command$show-screen-size edwin-command$show-frame-size)
(define edwin-command$show-screen-position edwin-command$show-frame-position)

(define-command set-frame-name
  "Set name of selected frame to NAME.
Useful only if `frame-name-format' is false."
  "sSet frame name"
  (lambda (name) (os2-screen/set-title! (selected-screen) name)))

(define (update-os2-screen-names! screen)
  (let ((window
	 (if (and (selected-screen? screen) (within-typein-edit?))
	     (typein-edit-other-window)
	     (screen-selected-window screen))))
    (let ((buffer (window-buffer window)))
      (let ((format (ref-variable frame-name-format buffer))
	    (length (ref-variable frame-name-length buffer)))
	(if format
	    (os2-screen/set-title!
	     screen
	     (string-trim-right
	      (format-modeline-string window format length))))))))