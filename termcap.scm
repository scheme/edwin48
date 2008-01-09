#| -*-Scheme-*-

$Id: termcap.scm,v 1.11 2007/01/05 21:19:24 cph Exp $

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

;;;; Termcap(3) Interface


(define-primitives
  (termcap-initialize 1)
  (termcap-get-number 1)
  (termcap-get-flag 1)
  (termcap-get-string 1)
  (termcap-param-string 5)
  (termcap-goto-string 5)
  (termcap-pad-string 4))

(define-structure (termcap-description
		   (constructor %make-termcap-description)
		   (conc-name false))
  (terminal-type-name false read-only true)

  (delete-is-insert-mode? false read-only true)
  (enter/exit-standout-mode-same? false read-only true)
  (insert/delete-char-ok? false read-only true)
  (insert/delete-line-ok? false read-only true)
  (scroll-region-ok? false read-only true)

  (tf-automatic-wrap false read-only true)
  (tf-cursor-backwards-wrap false read-only true)
  (tf-generic false read-only true)
  (tf-hardcopy false read-only true)
  (tf-hazeltine false read-only true)
  (tf-insert-mode-motion false read-only true)
  (tf-lose-wrap false read-only true)
  (tf-magic-wrap false read-only true)
  (tf-memory-above-screen false read-only true)
  (tf-memory-below-screen false read-only true)
  (tf-meta-key false read-only true)
  (tf-must-write-spaces false read-only true)
  (tf-newline-doesnt-scroll false read-only true)
  (tf-overstrike false read-only true)
  (tf-overstrike-space-erase false read-only true)
  (tf-overwrite-preserves-standout false read-only true)
  (tf-standout-mode-motion false read-only true)
  (tf-teleray false read-only true)
  (tf-underscore false read-only true)

  (tn-memory-lines false read-only true)
  (tn-minimum-padding-speed false read-only true)
  (tn-standout-marker-width false read-only true)
  (tn-x-size false read-only false)
  (tn-y-size false read-only false)

  (ts-audible-bell false read-only true)
  (ts-clear-line false read-only true)
  (ts-clear-multi-char false read-only true)
  (ts-clear-screen false read-only true)
  (ts-clear-to-bottom false read-only true)
  (ts-cursor-down false read-only true)
  (ts-cursor-down-multi false read-only true)
  (ts-cursor-left false read-only true)
  (ts-cursor-left-multi false read-only true)
  (ts-cursor-line-start false read-only true)
  (ts-cursor-lower-left false read-only true)
  (ts-cursor-move false read-only true)
  (ts-cursor-move-x false read-only true)
  (ts-cursor-right false read-only true)
  (ts-cursor-right-multi false read-only true)
  (ts-cursor-up false read-only true)
  (ts-cursor-up-multi false read-only true)
  (ts-cursor-upper-left false read-only true)
  (ts-delete-char false read-only true)
  (ts-delete-line false read-only true)
  (ts-delete-multi-char false read-only true)
  (ts-delete-multi-line false read-only true)
  (ts-enhance-cursor false read-only true)
  (ts-enter-delete-mode false read-only true)
  (ts-enter-insert-mode false read-only true)
  (ts-enter-standout-mode false read-only true)
  (ts-enter-termcap-mode false read-only true)
  (ts-exit-delete-mode false read-only true)
  (ts-exit-insert-mode false read-only true)
  (ts-exit-standout-mode false read-only true)
  (ts-exit-termcap-mode false read-only true)
  (ts-forward-scroll false read-only true)
  (ts-forward-scroll-multi false read-only true)
  (ts-insert-char false read-only true)
  (ts-insert-line false read-only true)
  (ts-insert-multi-char false read-only true)
  (ts-insert-multi-line false read-only true)
  (ts-invisible-cursor false read-only true)
  (ts-normal-cursor false read-only true)
  (ts-pad-char false read-only true)
  (ts-pad-inserted-char false read-only true)
  (ts-reverse-scroll false read-only true)
  (ts-reverse-scroll-multi false read-only true)
  (ts-set-scroll-region false read-only true)
  (ts-set-scroll-region-1 false read-only true)
  (ts-set-window false read-only true)
  (ts-visible-bell false read-only true)

  ;; keys is an alist mapping key names (symbols) to strings or #F
  (termcap-description-keys false read-only true)  )

(define (make-termcap-description terminal-type-name)
  (if (string-ci=? terminal-type-name "ansi.sys")
      (let ((x-size (output-port/x-size console-output-port))
	    (y-size (output-port/y-size console-output-port)))
	(make-ansi-terminal-description x-size y-size))
      (and (implemented-primitive-procedure? termcap-initialize)
	   (termcap-initialize terminal-type-name)
	   (let ((supdup? (string=? terminal-type-name "supdup"))
		 (tn-standout-marker-width (termcap-get-number "sg"))
		 (ts-cursor-down
		  (or (termcap-get-string "do") (termcap-get-string "nl")))
		 (ts-delete-char (termcap-get-string "dc"))
		 (ts-delete-line (termcap-get-string "dl"))
		 (ts-delete-multi-char (termcap-get-string "DC"))
		 (ts-delete-multi-line (termcap-get-string "DL"))
		 (ts-enter-delete-mode (termcap-get-string "dm"))
		 (ts-enter-insert-mode (termcap-get-string "im"))
		 (ts-enter-standout-mode (termcap-get-string "so"))
		 (ts-exit-standout-mode (termcap-get-string "se"))
		 (ts-forward-scroll (termcap-get-string "sf"))
		 (ts-forward-scroll-multi (termcap-get-string "SF"))
		 (ts-insert-char (termcap-get-string "ic"))
		 (ts-insert-line (termcap-get-string "al"))
		 (ts-insert-multi-char (termcap-get-string "IC"))
		 (ts-insert-multi-line (termcap-get-string "AL"))
		 (ts-pad-inserted-char (termcap-get-string "ip"))
		 (ts-reverse-scroll (termcap-get-string "sr"))
		 (ts-reverse-scroll-multi (termcap-get-string "SR"))
		 (ts-set-scroll-region (termcap-get-string "cs"))
		 (ts-set-scroll-region-1 (termcap-get-string "cS"))
		 (ts-set-window (termcap-get-string "wi")))
	     (if (not ts-forward-scroll)
		 (set! ts-forward-scroll ts-cursor-down))
	     (if (not ts-enter-standout-mode)
		 (begin
		   (set! tn-standout-marker-width (termcap-get-number "ug"))
		   (set! ts-enter-standout-mode (termcap-get-string "us"))
		   (set! ts-exit-standout-mode (termcap-get-string "ue"))))
	     (%make-termcap-description
	      terminal-type-name

	      ;; delete-is-insert-mode?
	      (and ts-enter-delete-mode
		   ts-enter-insert-mode
		   (string=? ts-enter-delete-mode ts-enter-insert-mode))
	      ;; enter/exit-standout-mode-same?
	      (and ts-enter-standout-mode
		   ts-exit-standout-mode
		   (string=? ts-enter-standout-mode ts-exit-standout-mode))
	      ;; insert/delete-char-ok?
	      (and (or ts-insert-char ts-insert-multi-char
		       ts-enter-insert-mode ts-pad-inserted-char)
		   (or ts-delete-char ts-delete-multi-char))
	      ;; insert/delete-line-ok?
	      (or (and (or ts-insert-line ts-insert-multi-line)
		       (or ts-delete-line ts-delete-multi-line))
		  (and (or ts-set-scroll-region
			   ts-set-scroll-region-1
			   ts-set-window)
		       (or ts-forward-scroll ts-forward-scroll-multi)
		       (or ts-reverse-scroll ts-reverse-scroll-multi)))
	      ;; scroll-region-ok?
	      (or ts-set-scroll-region ts-set-scroll-region-1 ts-set-window)

	      (termcap-get-flag "am")	;tf-automatic-wrap
	      (termcap-get-flag "bw")	;tf-cursor-backwards-wrap
	      (termcap-get-flag "gn")	;tf-generic
	      (termcap-get-flag "hc")	;tf-hardcopy
	      (termcap-get-flag "hz")	;tf-hazeltine
	      (termcap-get-flag "mi")	;tf-insert-mode-motion
	      supdup?			;tf-lose-wrap
	      (termcap-get-flag "xn")	;tf-magic-wrap
	      (termcap-get-flag "da")	;tf-memory-above-screen
	      (or (termcap-get-flag "db") ;tf-memory-below-screen
		  supdup?)
	      (or (termcap-get-flag "km") ;tf-meta-key
		  (termcap-get-flag "MT"))
	      (termcap-get-flag "in")	;tf-must-write-spaces
	      (termcap-get-flag "ns")	;tf-newline-doesnt-scroll
	      (termcap-get-flag "os")	;tf-overstrike
	      (termcap-get-flag "eo")	;tf-overstrike-space-erase
	      (termcap-get-flag "xs")	;tf-overwrite-preserves-standout
	      (termcap-get-flag "ms")	;tf-standout-mode-motion
	      (termcap-get-flag "xt")	;tf-teleray
	      (termcap-get-flag "ul")	;tf-underscore

	      (termcap-get-number "lm")	;tn-memory-lines
	      (termcap-get-number "pb")	;tn-minimum-padding-speed
	      tn-standout-marker-width
	      (termcap-get-number "co")	;tn-x-size
	      (termcap-get-number "li")	;tn-y-size

	      (or (termcap-get-string "bl") ;ts-audible-bell
		  "\007")
	      (termcap-get-string "ce")	;ts-clear-line
	      (termcap-get-string "ec")	;ts-clear-multi-char
	      (termcap-get-string "cl")	;ts-clear-screen
	      (termcap-get-string "cd")	;ts-clear-to-bottom
	      ts-cursor-down
	      (termcap-get-string "DO")	;ts-cursor-down-multi
	      (if (termcap-get-flag "bs") ;ts-cursor-left
		  "\010"
		  (or (termcap-get-string "le")
		      (termcap-get-string "bc")))
	      (termcap-get-string "LE")	;ts-cursor-left-multi
	      (termcap-get-string "cr")	;ts-cursor-line-start
	      (termcap-get-string "ll")	;ts-cursor-lower-left
	      (termcap-get-string "cm")	;ts-cursor-move
	      (termcap-get-string "ch")	;ts-cursor-move-x
	      (termcap-get-string "nd")	;ts-cursor-right
	      (termcap-get-string "RI")	;ts-cursor-right-multi
	      (termcap-get-string "up")	;ts-cursor-up
	      (termcap-get-string "UP")	;ts-cursor-up-multi
	      (termcap-get-string "ho")	;ts-cursor-upper-left
	      ts-delete-char
	      ts-delete-line
	      ts-delete-multi-char
	      ts-delete-multi-line
	      (termcap-get-string "vs")	;ts-enhance-cursor
	      ts-enter-delete-mode
	      ts-enter-insert-mode
	      ts-enter-standout-mode
	      (termcap-get-string "ti")	;ts-enter-termcap-mode
	      (termcap-get-string "ed")	;ts-exit-delete-mode
	      (termcap-get-string "ei")	;ts-exit-insert-mode
	      ts-exit-standout-mode
	      (termcap-get-string "te")	;ts-exit-termcap-mode
	      ts-forward-scroll
	      ts-forward-scroll-multi
	      ts-insert-char
	      ts-insert-line
	      ts-insert-multi-char
	      ts-insert-multi-line
	      (termcap-get-string "vi")	;ts-invisible-cursor
	      (termcap-get-string "ve")	;ts-normal-cursor
	      (termcap-get-string "pc")	;ts-pad-char
	      ts-pad-inserted-char
	      ts-reverse-scroll
	      ts-reverse-scroll-multi
	      ts-set-scroll-region
	      ts-set-scroll-region-1
	      ts-set-window
	      (termcap-get-string "vb")	;ts-visible-bell
	      
	      ; ts-keys
	      (append-map (lambda (string.name)
			    (let* ((string  (car string.name))
				   (keystr  (termcap-get-string string)))
			      (if keystr
				  (list (cons (cdr string.name) keystr))
				  '() )))
			  termcap-key-codes))))))


(define termcap-key-codes
  `(("ku" . up)				;arrow keys
    ("kd" . down)			;  "    "
    ("kl" . left)			;  "    "
    ("kr" . right)			;  "    "
    ("kb" . backspace)
    ("ka" . clear-all-tabs)		; clear-all-tabs key
    ("kC" . clear)			; clear screen or erase
    ("kt" . clear-tab)			; clear-tab
    ("kD" . delete-char)		; delete character
    ("kL" . delete-line)		; delete line
    ("kE" . clear-eol)			; clear-to-end-of-line key
    ("kS" . clear-eos)			; clear-to-end-of-screen key
    ("k0" . f0)
    ("k1" . f1)
    ("k2" . f2)
    ("k3" . f3)
    ("k4" . f4)
    ("k5" . f5)
    ("k6" . f6)
    ("k7" . f7)
    ("k8" . f8)
    ("k9" . f9)
    ("ka" . f10)
    ("F1" . f11)
    ("F2" . f12)
    ("F3" . f13)
    ("F4" . f14)
    ("F5" . f15)
    ("F6" . f16)
    ("F7" . f17)
    ("F8" . f18)
    ("F9" . f19)
    ("FA" . f20)
    ("FB" . f21)
    ("FC" . f22)
    ("FD" . f23)
    ("FE" . f24)
    ("kh" . home)			; home key
    ("kI" . insert-char)		; insert-char or enter-insert-mode
    ("kA" . insert-line)		; insert-line key
    ("kH" . home-down)
    ("kN" . next-page)			; next-page key
    ("kP" . previous-page)		; previous-page key
    ("kF" . scroll-forward)		; scroll-forward / scroll-down
    ("kR" . scroll-backward)		; scroll-backward / scroll-up
    ("kT" . set-tab) 
    ))