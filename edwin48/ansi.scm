#| -*-Scheme-*-

$Id: ansi.scm,v 1.13 2008/01/30 20:01:58 cph Exp $

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
       #f				; delete-is-insert-mode?
       #f				; enter/exit-standout-mode-same?
       (and full? "\033[P")		; insert/delete-char-ok?
       (and full? "\033[M")		; insert/delete-line-ok?
       #f				; scroll-region-ok?
       #t				; tf-automatic-wrap
       #f				; tf-cursor-backwards-wrap
       #f				; tf-generic
       #f				; tf-hardcopy
       #f				; tf-hazeltine
       #f				; tf-insert-mode-motion
       #f				; tf-lose-wrap
       #f				; tf-magic-wrap
       #f				; tf-memory-above-screen
       #f				; tf-memory-below-screen
       #f				; tf-meta-key
       #f				; tf-must-write-spaces
       #f				; tf-newline-doesnt-scroll
       #f				; tf-overstrike
       #f				; tf-overstrike-space-erase
       #f				; tf-overwrite-preserves-standout
       #f				; tf-standout-mode-motion
       #f				; tf-teleray
       #f				; tf-underscore
       #f				; tn-memory-lines
       #f				; tn-minimum-padding-speed
       #f				; tn-standout-marker-width
       columns				; tn-x-size
       lines				; tn-y-size
       "\a"				; ts-audible-bell
       "\033[K"				; ts-clear-line
       #f				; ts-clear-multi-char
       "\033[H\033[J"			; ts-clear-screen
       "\033[J"				; ts-clear-to-bottom
       "\n"				; ts-cursor-down
       #f				; ts-cursor-down-multi
       "\b"				; ts-cursor-left
       #f				; ts-cursor-left-multi
       "\r"				; ts-cursor-line-start
       #f				; ts-cursor-lower-left
       "\033[%i%d;%dH"			; ts-cursor-move
       #f				; ts-cursor-move-x
       "\033[C"				; ts-cursor-right
       (and full? "\033[%dC")		; ts-cursor-right-multi
       "\033[A"				; ts-cursor-up
       (and full? "\033[%dA")		; ts-cursor-up-multi
       "\033[H"				; ts-cursor-upper-left
       (and full? "\033[P")		; ts-delete-char
       (and full? "\033[M")		; ts-delete-line
       (and full? "\033[%dP")		; ts-delete-multi-char
       (and full? "\033[%dM")		; ts-delete-multi-line
       #f				; ts-enhance-cursor
       #f				; ts-enter-delete-mode
       #f ;"\033[4h"			; ts-enter-insert-mode
       standout;"\033[7m"		; ts-enter-standout-mode
       (and full? "\033[1p")		; ts-enter-termcap-mode
       #f				; ts-exit-delete-mode
       #f ;"\033[4l"			; ts-exit-insert-mode
       normal;"\033[0m"			; ts-exit-standout-mode
       (and full? "\033[0p")		; ts-exit-termcap-mode
       "\n"				; ts-forward-scroll
       #f				; ts-forward-scroll-multi
       (and full? "\033[@")		; ts-insert-char
       (and full? "\033[L")		; ts-insert-line
       (and full? "\033[%d@")		; ts-insert-multi-char
       (and full? "\033[%dL")		; ts-insert-multi-line
       #f				; ts-invisible-cursor
       #f				; ts-normal-cursor
       #f				; ts-pad-char
       #f				; ts-pad-inserted-char
       #f				; ts-reverse-scroll
       #f				; ts-reverse-scroll-multi
       #f				; ts-set-scroll-region
       #f				; ts-set-scroll-region-1
       #f				; ts-set-window
       #f				; ts-visible-bell
       '()                              ; termcap-description-keys
       ))))