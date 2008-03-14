#| -*-Scheme-*-

$Id: edtstr.scm,v 1.36 2008/01/30 20:02:00 cph Exp $

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

;;;; Editor Data Abstraction


(define-record-type* editor
  (%make-editor name
                display-type
                (screens)
                (selected-screen)
                bufferset
                char-history
                halt-update?
                peek-no-hang
                peek
                read
                (button-event)
                (select-time))
  ())


(define (make-editor name display-type make-screen-args)
  (let ((initial-buffer
	 (make-buffer initial-buffer-name
		      (ref-mode-object fundamental)
		      (working-directory-pathname))))
    (let ((bufferset (make-bufferset initial-buffer))
	  (screen (display-type/make-screen display-type make-screen-args)))
      (initialize-screen-root-window! screen bufferset initial-buffer)
      (call-with-values
	  (lambda () (display-type/get-input-operations display-type screen))
	(lambda (halt-update? peek-no-hang peek read)
	  (%make-editor name
			display-type
			(list screen)
			screen
			bufferset
			(make-ring 100)
			halt-update?
			peek-no-hang
			peek
			read
			#f
			1))))))

(define (current-display-type)
  (editor-display-type current-editor))

(define (with-editor-interrupts-enabled thunk)
  (display-type/with-interrupts-enabled (current-display-type) thunk))

(define (with-editor-interrupts-disabled thunk)
  (display-type/with-interrupts-disabled (current-display-type) thunk))

(define (current-bufferset)
  (editor-bufferset current-editor))

(define (current-char-history)
  (editor-char-history current-editor))

(define (increment-select-time!)
  (let ((time (editor-select-time current-editor)))
    (set-editor-select-time! current-editor (1+ time))
    time))

;;;; Buttons

(define-record-type <button> :button
  (%%make-button number bits down? symbol)
  button?
  (number button-number)
  (bits button-bits)
  (down? button-down?)
  (symbol button-symbol))

(define* (make-down-button number (bits 0))
  (%make-button number bits #t 'MAKE-DOWN-BUTTON))

(define* (make-up-button number (bits 0))
  (%make-button number bits #f 'MAKE-UP-BUTTON))

(define (%make-button number bits down? caller)
  (guarantee-limited-index-fixnum number #x100 caller)
  (guarantee-limited-index-fixnum bits #x10 caller)
  (let ((name
         (symbol (bucky-bits->prefix bits)
                 'BUTTON-
                 number
                 (if down? '-DOWN '-UP))))
    (hash-table/intern! buttons-table name
                        (lambda ()
                          (%%make-button number bits down? name)))))

(define buttons-table
  (make-hash-table eq?))

(define (down-button? object)
  (and (button? object)
       (button-down? object)))

(define (up-button? object)
  (and (button? object)
       (not (button-down? object))))

(define (button-name button)
  (symbol-name (button-symbol button)))

(define-record-discloser :button
  (lambda (button)
    (list (button-symbol button))))

(define-record-type button-event :button-event
  (make-button-event window x y)
  button-event?
  (window button-event/window)
  (x      button-event/x)
  (y      button-event/y))

(define (current-button-event)
  (or (editor-button-event current-editor)
      ;; Create a "dummy" event at point.
      (let ((window (current-window)))
	(let ((coordinates (window-point-coordinates window)))
	  (make-button-event window
			     (car coordinates)
			     (cdr coordinates))))))

(define (with-current-button-event button-event thunk)
  (let ((old-button-event unspecific))
    (dynamic-wind
     (lambda ()
       (set! old-button-event (editor-button-event current-editor))
       (set-editor-button-event! current-editor button-event)
       (set! button-event #f)
       unspecific)
     thunk
     (lambda ()
       (set-editor-button-event! current-editor old-button-event)))))