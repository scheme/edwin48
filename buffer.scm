#| -*-Scheme-*-

$Id: buffer.scm,v 1.195 2007/01/05 21:19:23 cph Exp $

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

;;;; Buffer Abstraction


(define-structure (buffer
		   (constructor %make-buffer (%name %default-directory)))
  %name
  group
  mark-ring
  modes
  comtabs
  windows
  display-start
  %default-directory
  %pathname
  %truename
  alist
  local-bindings
  local-bindings-installed?
  auto-save-pathname
  auto-saved?
  %save-length
  backed-up?
  modification-time)

(define-syntax rename-buffer-accessor
  (sc-macro-transformer
   (lambda (form environment)
     (let ((slot-name (cadr form)))
       `(DEFINE-INTEGRABLE ,(symbol-append 'BUFFER- slot-name)
	  ,(close-syntax (symbol-append 'BUFFER-% slot-name)
			 environment))))))

(rename-buffer-accessor name)
(rename-buffer-accessor default-directory)
(rename-buffer-accessor pathname)
(rename-buffer-accessor truename)
(rename-buffer-accessor save-length)

(define-variable buffer-creation-hook
  "An event distributor that is invoked when a new buffer is created.
The new buffer is passed as its argument.
The buffer is guaranteed to be deselected at that time."
  (make-event-distributor))

(define (make-buffer name mode directory)
  (let ((buffer (%make-buffer name directory)))
    (let ((group (make-group buffer)))
      (set-buffer-group! buffer group)
      (add-group-clip-daemon! group (buffer-clip-daemon buffer))
      (%buffer-reset! buffer)
      (set-buffer-windows! buffer '())
      (set-buffer-display-start! buffer #f)
      (set-buffer-default-directory! buffer directory)
      (set-buffer-local-bindings! buffer '())
      (set-buffer-local-bindings-installed?! buffer #f)
      (%set-buffer-major-mode! buffer mode)
      (event-distributor/invoke!
       (variable-default-value (ref-variable-object buffer-creation-hook))
       buffer)
      buffer)))

(define (%buffer-reset! buffer)
  (let ((group (buffer-group buffer)))
    (disable-group-undo! group)
    (if (not (minibuffer? buffer))
	(enable-group-undo! group)))
  (set-buffer-mark-ring!
   buffer
   (make-ring
    (variable-default-value (ref-variable-object mark-ring-maximum))))
  (ring-push! (buffer-mark-ring buffer) (buffer-start buffer))
  (set-buffer-%pathname! buffer #f)
  (set-buffer-%truename! buffer #f)
  (set-buffer-auto-save-pathname! buffer #f)
  (set-buffer-auto-saved?! buffer #f)
  (set-buffer-%save-length! buffer 0)
  (set-buffer-backed-up?! buffer #f)
  (set-buffer-modification-time! buffer #f)
  (set-buffer-alist! buffer '()))

(define (buffer-modeline-event! buffer type)
  (let loop ((windows (buffer-windows buffer)))
    (if (not (null? windows))
	(begin
	  (window-modeline-event! (car windows) type)
	  (loop (cdr windows))))))

(define (without-editor-interrupts thunk)
  ;; Control interrupts whether or not in the editor.
  ;; WITH-EDITOR-INTERRUPTS-DISABLED is required in the running editor but
  ;; buffers are created at editor initialization time and variables may
  ;; be set as early as initial file load time (prior to dumping the band).
  (if within-editor?
      (with-editor-interrupts-disabled thunk)
      (without-interrupts thunk)))

(define (buffer-reset! buffer)
  (set-buffer-writeable! buffer)
  (buffer-widen! buffer)
  (region-delete! (buffer-region buffer))
  (buffer-not-modified! buffer)
  (without-editor-interrupts
   (lambda ()
     (undo-local-bindings! buffer #t)
     (%buffer-reset! buffer)
     (%set-buffer-major-mode!
      buffer
      (variable-default-value (ref-variable-object editor-default-mode)))
     (event-distributor/invoke! event:set-buffer-pathname buffer)
     (buffer-modeline-event! buffer 'BUFFER-RESET))))

(define (set-buffer-name! buffer name)
  (set-buffer-%name! buffer name)
  (buffer-modeline-event! buffer 'BUFFER-NAME))

(define (set-buffer-default-directory! buffer directory)
  (set-buffer-%default-directory! buffer (pathname-simplify directory)))

(define (set-buffer-pathname! buffer pathname)
  (set-buffer-%pathname! buffer pathname)
  (if pathname
      (set-buffer-default-directory! buffer (directory-pathname pathname)))
  (event-distributor/invoke! event:set-buffer-pathname buffer)
  (buffer-modeline-event! buffer 'BUFFER-PATHNAME))

(define event:set-buffer-pathname
  (make-event-distributor))

(define (set-buffer-truename! buffer truename)
  (set-buffer-%truename! buffer truename)
  (buffer-modeline-event! buffer 'BUFFER-TRUENAME))

(define (set-buffer-save-length! buffer)
  (set-buffer-%save-length! buffer (buffer-length buffer)))

(define (buffer-point buffer)
  (cond ((current-buffer? buffer)
	 (current-point))
	((let ((windows (buffer-windows buffer)))
	   (and (pair? windows)
		(null? (cdr windows))
		(car windows)))
	 => window-point)
	(else
	 (group-point (buffer-group buffer)))))

(define (%set-buffer-point! buffer mark)
  (set-group-point! (buffer-group buffer) mark))

(define (%set-buffer-point-index! buffer index)
  (set-group-point-index! (buffer-group buffer) index))

(define (minibuffer? buffer)
  (char=? (string-ref (buffer-name buffer) 0) #\Space))

(define (buffer-region buffer)
  (group-region (buffer-group buffer)))

(define (buffer-string buffer)
  (region->string (buffer-region buffer)))

(define (buffer-unclipped-region buffer)
  (group-unclipped-region (buffer-group buffer)))

(define (buffer-widen! buffer)
  (group-widen! (buffer-group buffer)))

(define (buffer-length buffer)
  (group-length (buffer-group buffer)))

(define (buffer-start buffer)
  (group-start-mark (buffer-group buffer)))

(define (buffer-end buffer)
  (group-end-mark (buffer-group buffer)))

(define (buffer-absolute-start buffer)
  (group-absolute-start (buffer-group buffer)))

(define (buffer-absolute-end buffer)
  (group-absolute-end (buffer-group buffer)))

(define (add-buffer-window! buffer window)
  (set-buffer-windows! buffer (cons window (buffer-windows buffer))))

(define (remove-buffer-window! buffer window)
  (set-buffer-windows! buffer (delq! window (buffer-windows buffer))))

(define (buffer-visible? buffer)
  (any window-visible? (buffer-windows buffer)))

(define (buffer-x-size buffer)
  (let ((windows (buffer-windows buffer)))
    (if (null? windows)
	(screen-x-size (selected-screen))
	(apply min (map window-x-size windows)))))

(define (mark-x-size mark)
  (let ((buffer (mark-buffer mark)))
    (if buffer
	(buffer-x-size buffer)
	(screen-x-size (selected-screen)))))

(define (buffer-get buffer key #!optional default)
  (let ((entry (assq key (buffer-alist buffer))))
    (if entry
	(cdr entry)
	(if (default-object? default) #f default))))

(define (buffer-put! buffer key value)
  (let ((entry (assq key (buffer-alist buffer))))
    (if entry
	(set-cdr! entry value)
	(set-buffer-alist! buffer
			   (cons (cons key value) (buffer-alist buffer))))))

(define (buffer-remove! buffer key)
  (set-buffer-alist! buffer (del-assq! key (buffer-alist buffer))))

(define (->buffer object)
  (or (cond ((buffer? object) object)
	    ((mark? object) (mark-buffer object))
	    ((group? object) (group-buffer object))
	    ((region? object) (mark-buffer (region-start object)))
	    ((window? object) (window-buffer object))
	    ((not object) (current-buffer))
	    (else (error:wrong-type-argument object "buffer" '->BUFFER)))
      (error:bad-range-argument object '->BUFFER)))

;;;; Modification Flags

(define (buffer-modified? buffer)
  (group-modified? (buffer-group buffer)))

(define (buffer-not-modified! buffer)
  (without-editor-interrupts
   (lambda ()
     (let ((group (buffer-group buffer)))
       (if (group-modified? group)
	   (begin
	     (set-group-modified?! group #f)
	     (buffer-modeline-event! buffer 'BUFFER-MODIFIED)
	     (set-buffer-auto-saved?! buffer #f)))))))

(define (buffer-modified! buffer)
  (without-editor-interrupts
   (lambda ()
     (let ((group (buffer-group buffer)))
       (if (not (group-modified? group))
	   (begin
	     (set-group-modified?! group #t)
	     (buffer-modeline-event! buffer 'BUFFER-MODIFIED)))))))

(define (verify-visited-file-modification-time? buffer)
  (let ((truename (buffer-truename buffer))
	(buffer-time (buffer-modification-time buffer)))
    (or (not truename)
	(not buffer-time)
	(let ((file-time (file-modification-time truename)))
	  (and file-time (< (abs (- buffer-time file-time)) 2))))))

(define (clear-visited-file-modification-time! buffer)
  (set-buffer-modification-time! buffer #f))

(define (set-buffer-auto-saved! buffer)
  (set-buffer-auto-saved?! buffer #t)
  (set-group-modified?! (buffer-group buffer) 'AUTO-SAVED))

(define (buffer-auto-save-modified? buffer)
  (eq? #t (group-modified? (buffer-group buffer))))

(define (buffer-clip-daemon buffer)
  (lambda (group start end)
    group start end			;ignore
    (buffer-modeline-event! buffer 'CLIPPING-CHANGED)))

(define (buffer-read-only? buffer)
  (group-read-only? (buffer-group buffer)))

(define (buffer-writeable? buffer)
  (not (buffer-read-only? buffer)))

(define (set-buffer-writeable! buffer)
  (set-group-writeable! (buffer-group buffer))
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (set-buffer-read-only! buffer)
  (set-group-read-only! (buffer-group buffer))
  (buffer-modeline-event! buffer 'BUFFER-MODIFIABLE))

(define (with-read-only-defeated object thunk)
  (let ((group (buffer-group (->buffer object)))
	(outside)
	(inside 'FULLY))
    (dynamic-wind (lambda ()
		    (set! outside (group-writeable? group))
		    (set-group-writeable?! group inside))
		  thunk
		  (lambda ()
		    (set! inside (group-writeable? group))
		    (set-group-writeable?! group outside)))))

;;;; Local Bindings

(define (define-variable-local-value! buffer variable value)
  (let ((buffer (->buffer buffer))
	(value (normalize-variable-value variable value)))
    (without-editor-interrupts
     (lambda ()
       (let ((binding (search-local-bindings buffer variable)))
	 (if binding
	     (set-cdr! binding value)
	     (set-buffer-local-bindings!
	      buffer
	      (cons (cons variable value)
		    (buffer-local-bindings buffer)))))
       (if (buffer-local-bindings-installed? buffer)
	   (set-variable-%value! variable value))
       (invoke-variable-assignment-daemons! buffer variable)))))

(define (undefine-variable-local-value! buffer variable)
  (let ((buffer (->buffer buffer)))
    (without-editor-interrupts
     (lambda ()
       (let ((binding (search-local-bindings buffer variable)))
	 (if binding
	     (begin
	       (set-buffer-local-bindings!
		buffer
		(delq! binding (buffer-local-bindings buffer)))
	       (if (buffer-local-bindings-installed? buffer)
		   (set-variable-%value! variable
					 (variable-default-value variable)))
	       (invoke-variable-assignment-daemons! buffer variable))))))))

(define (variable-local-value buffer variable)
  (let ((not-mark-local
	 (lambda ()
	   (let ((binding
		  (and buffer
		       (search-local-bindings (->buffer buffer) variable))))
	     (if binding
		 (cdr binding)
		 (variable-default-value variable))))))
    (if (mark? buffer)
	(let ((no-datum (list 'NO-DATUM)))
	  (let ((value (region-get buffer variable no-datum)))
	    (if (eq? value no-datum)
		(not-mark-local)
		value)))
	(not-mark-local))))

(define (variable-local-value? buffer variable)
  (or (not buffer)
      (search-local-bindings (->buffer buffer) variable)))

(define (set-variable-local-value! buffer variable value)
  (if buffer
      (let ((buffer (->buffer buffer)))
	(cond ((variable-buffer-local? variable)
	       (define-variable-local-value! buffer variable value))
	      ((search-local-bindings buffer variable)
	       =>
	       (lambda (binding)
		 (let ((value (normalize-variable-value variable value)))
		   (without-editor-interrupts
		    (lambda ()
		      (set-cdr! binding value)
		      (if (buffer-local-bindings-installed? buffer)
			  (set-variable-%value! variable value))
		      (invoke-variable-assignment-daemons! buffer
							   variable))))))
	      (else
	       (set-variable-default-value! variable value))))
      (set-variable-default-value! variable value)))

(define (set-variable-default-value! variable value)
  (if within-editor?
      (let ((value (normalize-variable-value variable value)))
	(without-editor-interrupts
	 (lambda ()
	   (set-variable-%default-value! variable value)
	   (if (not (search-local-bindings (current-buffer) variable))
	       (set-variable-%value! variable value))
	   (invoke-variable-assignment-daemons! #f variable))))
      (set-default-variable-value!/outside-editor variable value)))

(define (search-local-bindings buffer variable)
  (let loop ((bindings (buffer-local-bindings buffer)))
    (and (not (null? bindings))
	 (if (eq? (caar bindings) variable)
	     (car bindings)
	     (loop (cdr bindings))))))

(define (undo-local-bindings! buffer all?)
  ;; Caller guarantees that interrupts are disabled.
  (let ((bindings (buffer-local-bindings buffer)))
    (if (buffer-local-bindings-installed? buffer)
	(do ((bindings bindings (cdr bindings)))
	    ((null? bindings))
	  (set-variable-%value! (caar bindings)
				(variable-default-value (caar bindings)))))
    (call-with-values
	(lambda ()
	  (split-list bindings
		      (lambda (binding)
			(variable-permanent-local? (car binding)))))
      (lambda (permanent impermanent)
	(set-buffer-local-bindings! buffer (if all? '() permanent))
	(do ((bindings impermanent (cdr bindings)))
	    ((null? bindings))
	  (invoke-variable-assignment-daemons! buffer (caar bindings)))))))

(define (with-current-local-bindings! thunk)
  (dynamic-wind (lambda ()
		  (install-buffer-local-bindings! (current-buffer)))
		thunk
		(lambda ()
		  (uninstall-buffer-local-bindings! (current-buffer)))))

(define (change-local-bindings! old-buffer new-buffer select-buffer!)
  ;; Assumes that interrupts are disabled and that OLD-BUFFER is selected.
  (uninstall-buffer-local-bindings! old-buffer)
  (select-buffer!)
  (install-buffer-local-bindings! new-buffer))

(define (install-buffer-local-bindings! buffer)
  (do ((bindings (buffer-local-bindings buffer) (cdr bindings)))
      ((null? bindings))
    (set-variable-%value! (caar bindings) (cdar bindings)))
  (set-buffer-local-bindings-installed?! buffer #t))

(define (uninstall-buffer-local-bindings! buffer)
  (do ((bindings (buffer-local-bindings buffer) (cdr bindings)))
      ((null? bindings))
    (set-variable-%value! (caar bindings)
			  (variable-default-value (caar bindings))))
  (set-buffer-local-bindings-installed?! buffer #f))

(define (set-variable-value! variable value)
  (if within-editor?
      (set-variable-local-value! (current-buffer) variable value)
      (set-default-variable-value!/outside-editor variable value)))

(define (set-default-variable-value!/outside-editor variable value)
  (let ((value (normalize-variable-value variable value)))
    (without-interrupts
     ;; Not with-editor-interrupts-disabled as we are not within-editor?
     (lambda ()
       (set-variable-%default-value! variable value)
       (set-variable-%value! variable value)
       (invoke-variable-assignment-daemons! #f variable)))))

(define (with-variable-value! variable new-value thunk)
  (let ((old-value))
    (dynamic-wind (lambda ()
		    (set! old-value (variable-value variable))
		    (set-variable-value! variable new-value)
		    (set! new-value)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! new-value (variable-value variable))
		    (set-variable-value! variable old-value)
		    (set! old-value)
		    unspecific))))

;;;; Modes

(define (buffer-major-mode buffer)
  (car (buffer-modes buffer)))

(define (set-buffer-major-mode! buffer mode)
  (if (not (and (mode? mode) (mode-major? mode)))
      (error:wrong-type-argument mode "major mode" 'SET-BUFFER-MAJOR-MODE!))
  (if (buffer-get buffer 'MAJOR-MODE-LOCKED)
      (editor-error "The major mode of this buffer is locked: " buffer))
  ;; The very first buffer is created before the editor
  (without-editor-interrupts
   (lambda ()
     (undo-local-bindings! buffer #f)
     (%set-buffer-major-mode! buffer mode)
     (buffer-modeline-event! buffer 'BUFFER-MODES))))

(define (%set-buffer-major-mode! buffer mode)
  (set-buffer-modes! buffer (list mode))
  (set-buffer-comtabs! buffer (mode-comtabs mode))
  (set-variable-local-value! buffer
			     (ref-variable-object mode-name)
			     (mode-display-name mode))
  ((mode-initialization mode) buffer)
  (event-distributor/invoke! event:set-buffer-major-mode buffer))

(define event:set-buffer-major-mode
  (make-event-distributor))

(define (buffer-minor-modes buffer)
  (list-copy (cdr (buffer-modes buffer))))

(define (buffer-minor-mode? buffer mode)
  (if (not (and (mode? mode) (not (mode-major? mode))))
      (error:wrong-type-argument mode "minor mode" 'BUFFER-MINOR-MODE?))
  (memq mode (cdr (buffer-modes buffer))))

(define (enable-buffer-minor-mode! buffer mode)
  (if (not (minor-mode? mode))
      (error:wrong-type-argument mode "minor mode" 'ENABLE-BUFFER-MINOR-MODE!))
  (without-editor-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (not (memq mode (cdr modes)))
	   (begin
	     (set-cdr! modes (append! (cdr modes) (list mode)))
	     (set-buffer-comtabs! buffer
				  (cons (minor-mode-comtab mode)
					(buffer-comtabs buffer)))
	     (add-minor-mode-line-entry! buffer mode)
	     ((mode-initialization mode) buffer)
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))

(define (disable-buffer-minor-mode! buffer mode)
  (if (not (minor-mode? mode))
      (error:wrong-type-argument mode "minor mode"
				 'DISABLE-BUFFER-MINOR-MODE!))
  (without-editor-interrupts
   (lambda ()
     (let ((modes (buffer-modes buffer)))
       (if (memq mode (cdr modes))
	   (begin
	     (set-cdr! modes (delq! mode (cdr modes)))
	     (set-buffer-comtabs! buffer
				  (delq! (minor-mode-comtab mode)
					 (buffer-comtabs buffer)))
	     (remove-minor-mode-line-entry! buffer mode)
	     (buffer-modeline-event! buffer 'BUFFER-MODES)))))))