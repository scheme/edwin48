#| -*-Scheme-*-

$Id: win32com.scm,v 1.13 2007/01/05 21:19:24 cph Exp $

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

;;;; Win32 Commands
;;; package (edwin win-commands)

(declare (usual-integrations))

(define-command set-icon
  "Set the current window's icon to ICON.
ICON must be the (string) name of one of the known icons.
When called interactively, completion is available on the input."
  (lambda ()
    (list (prompt-for-alist-value "Set Icon"
				  (map (lambda (x) (cons x x))
				       (vector->list icon-names)))))
  (lambda (icon-name)
    (let  ((icon  (load-icon (get-handle 0) icon-name)))
      (if (zero? icon)
	  (error "Unknown icon name" icon-name)
	  (win32-screen/set-icon! (selected-screen) icon)))))

(define icon-names
  '#("shield3_icon"
     "shield4_icon"
     "shield2_icon"
     "shield1_icon"
     "lambda_icon"
     "lambda2_icon"
     "edwin_icon"
     "liar1_icon"
     "liar2_icon"
     "liar3_icon"
     "graphics_icon"
     "coffee_icon"
     "conses_icon"
     "environment_icon"
     "mincer_icon"
     "bch_ico"))

(define-command set-foreground-color
  "Set foreground (text) color to COLOR."
  "sSet foreground color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (win32-screen/set-foreground-color! screen (win32/find-color name))
      (update-screen! screen #t))))

(define-command set-background-color
  "Set background (text) color to COLOR."
  "sSet background color"
  (lambda (name)
    (let ((screen (selected-screen)))
      (win32-screen/set-background-color! screen (win32/find-color name))
      (update-screen! screen #t))))

(define-command set-font
  "Set font to be used for drawing text."
  "sSet font"
  (lambda (font)
    (let ((screen (selected-screen)))
      (win32-screen/set-font! screen font)
      (update-screen! screen #t))))

(define-command set-default-font
  "Set font to be used for drawing text in new frames."
  "sSet default font"
  (lambda (font)
    ((ucode-primitive win32-screen-set-default-font! 1) font)))

(define-command set-frame-size
  "Set size of editor frame to WIDTH x HEIGHT."
  "nFrame width (chars)\nnFrame height (chars)"
  (lambda (width height)
    (win32-screen/set-size! (selected-screen) (max 2 width) (max 2 height))))

(define-command set-frame-position
  "Set position of editor frame to (X,Y)."
  "nFrame X position (pels)\nnFrame Y position (pels)"
  (lambda (x y)
    (win32-screen/set-position! (selected-screen) x y)))

(define-command show-frame-size
  "Show size of current frame."
  ()
  (lambda ()
    (let ((screen (selected-screen)))
      (call-with-values (lambda () (win32-screen/get-client-size screen))
	(lambda (width height)
	  (message "Frame is "
		   (screen-x-size screen)
		   " chars wide and "
		   (screen-y-size screen)
		   " chars high ("
		   width "x" height
		   " pels)"))))))

(define-command show-frame-position
  "Show position of current frame.
This is the position of the upper left-hand corner of the frame border
surrounding the frame, relative to the upper left-hand corner of the
desktop."
  ()
  (lambda ()
    (call-with-values (lambda () (win32-screen/get-position (selected-screen)))
      (lambda (x y r b)
	r b				; ignored
	(message "Frame's upper left-hand corner is at (" x "," y ")")))))

(define (update-win32-screen-name! screen)
  (let ((window
	 (if (and (selected-screen? screen) (within-typein-edit?))
	     (typein-edit-other-window)
	     (screen-selected-window screen))))
    (let ((buffer (window-buffer window))
	  (update-name
	   (lambda (set-name format length)
	     (if format
		 (set-name
		  screen
		  (string-trim-right
		   (format-modeline-string window format length)))))))
      (update-name win32-screen/set-name!
		   (ref-variable frame-name-format buffer)
		   (ref-variable frame-name-length buffer)))))