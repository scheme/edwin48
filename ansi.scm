#| -*-Scheme-*-

$Id: ansi.scm,v 1.12 2007/01/05 21:19:23 cph Exp $

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

;;;; Hard-coded ANSI terminal type for lack of termcap on DOS/NT


(define (make-ansi-terminal-description columns lines)
  (define (get-numstring base-name)
    (or (get-environment-variable (string-append "EDWIN_" base-name))
	(get-environment-variable base-name)))

  (define (valid-mode? color-string)
    (and (string? color-string)
	 (= (string-length color-string) 2)
	 (member (string-ref color-string 0)
		 '(#\3 #\4))
	 (member (string-ref color-string 1)
		 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))))

  (define (invert-mode color-string)
    (make-mode (string (if (eqv? (string-ref color-string 0) #\3)
			   #\4
			   #\3)
		       (string-ref color-string 1))))

  (define (make-mode color-string)
    (or (and (valid-mode? color-string)
	     (string-append ";" color-string))
	""))

  (let ((foregnd (get-numstring "FOREGROUND"))
	(backgnd (get-numstring "BACKGROUND")))
    (let ((full? (not (eq? 'DOS microcode-id/operating-system)))
	  (normal
	   (string-append "\033[0"
			  (make-mode foregnd)
			  (make-mode backgnd)
			  "m"))
	  (standout
	   (if (or (not (valid-mode? foregnd)) (not (valid-mode? backgnd)))
	       (string-append "\033[7"
			      (make-mode foregnd)
			      (make-mode backgnd)
			      "m")
	       (string-append "\033[0"
			      (invert-mode backgnd)
			      (invert-mode foregnd)
			      "m"))))

      (%make-termcap-description
       "ansi.sys"		        ; terminal-type-name
       false				; delete-is-insert-mode?
       false				; enter/exit-standout-mode-same?
       (and full? "\033[P")		; insert/delete-char-ok?
       (and full? "\033[M")		; insert/delete-line-ok?
       false				; scroll-region-ok?
       #t				; tf-automatic-wrap
       false				; tf-cursor-backwards-wrap
       false				; tf-generic
       false				; tf-hardcopy
       false				; tf-hazeltine
       false				; tf-insert-mode-motion
       false				; tf-lose-wrap
       false				; tf-magic-wrap
       false				; tf-memory-above-screen
       false				; tf-memory-below-screen
       false				; tf-meta-key
       false				; tf-must-write-spaces
       false				; tf-newline-doesnt-scroll
       false				; tf-overstrike
       false				; tf-overstrike-space-erase
       false				; tf-overwrite-preserves-standout
       false				; tf-standout-mode-motion
       false				; tf-teleray
       false				; tf-underscore
       false				; tn-memory-lines
       false				; tn-minimum-padding-speed
       false				; tn-standout-marker-width
       columns				; tn-x-size
       lines				; tn-y-size
       "\a"				; ts-audible-bell
       "\033[K"				; ts-clear-line
       false				; ts-clear-multi-char
       "\033[H\033[J"			; ts-clear-screen
       "\033[J"				; ts-clear-to-bottom
       "\n"				; ts-cursor-down
       false				; ts-cursor-down-multi
       "\b"				; ts-cursor-left
       false				; ts-cursor-left-multi
       "\r"				; ts-cursor-line-start
       false				; ts-cursor-lower-left
       "\033[%i%d;%dH"			; ts-cursor-move
       false				; ts-cursor-move-x
       "\033[C"				; ts-cursor-right
       (and full? "\033[%dC")		; ts-cursor-right-multi
       "\033[A"				; ts-cursor-up
       (and full? "\033[%dA")		; ts-cursor-up-multi
       "\033[H"				; ts-cursor-upper-left
       (and full? "\033[P")		; ts-delete-char
       (and full? "\033[M")		; ts-delete-line
       (and full? "\033[%dP")		; ts-delete-multi-char
       (and full? "\033[%dM")		; ts-delete-multi-line
       false				; ts-enhance-cursor
       false				; ts-enter-delete-mode
       false ;"\033[4h"			; ts-enter-insert-mode
       standout;"\033[7m"		; ts-enter-standout-mode
       (and full? "\033[1p")		; ts-enter-termcap-mode
       false				; ts-exit-delete-mode
       false ;"\033[4l"			; ts-exit-insert-mode
       normal;"\033[0m"			; ts-exit-standout-mode
       (and full? "\033[0p")		; ts-exit-termcap-mode
       "\n"				; ts-forward-scroll
       false				; ts-forward-scroll-multi
       (and full? "\033[@")		; ts-insert-char
       (and full? "\033[L")		; ts-insert-line
       (and full? "\033[%d@")		; ts-insert-multi-char
       (and full? "\033[%dL")		; ts-insert-multi-line
       false				; ts-invisible-cursor
       false				; ts-normal-cursor
       false				; ts-pad-char
       false				; ts-pad-inserted-char
       false				; ts-reverse-scroll
       false				; ts-reverse-scroll-multi
       false				; ts-set-scroll-region
       false				; ts-set-scroll-region-1
       false				; ts-set-window
       false				; ts-visible-bell
       '()                              ; termcap-description-keys
       ))))