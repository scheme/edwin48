#| -*-Scheme-*-

$Id: dosshell.scm,v 1.11 2008/01/30 20:02:00 cph Exp $

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

;;;; Pseudo Shell subprocess in a buffer
;;; Inspired by "cmushell.el", by Olin Shivers.


(load-option 'DOSPROCESS)

(define-major-mode pseudo-shell fundamental "Pseudo Shell"
  "Major mode for executing DOS commands.
Return executes the current line as a DOS command.
Output is inserted into the buffer after the command.
There is currently no way to send input interactively to the command.
Use \\[shell-command-on-region] to feed input to the command.

cd, pushd, and popd commands are not executed as commands (they would have
no effect) but emulated directly by Edwin.

Customization: Entry to this mode runs the hook pseudo-shell-mode-hook."
  (lambda (buffer)
    (define-variable-local-value! buffer
      (ref-variable-object pseudo-shell-dirstack)
      '())
    (define-variable-local-value! buffer
      (ref-variable-object pseudo-shell-dirtrack?)
      #t)
    (define-variable-local-value! buffer
      (ref-variable-object comint-input-ring)
      (make-ring (ref-variable comint-input-ring-size)))
    (define-variable-local-value! buffer
      (ref-variable-object comint-last-input-match)
      #f)
    (define-variable-local-value! buffer
      (ref-variable-object pseudo-shell-active?)
      #t)
    (event-distributor/invoke! (ref-variable pseudo-shell-mode-hook)
			       buffer)))

(define-variable pseudo-shell-mode-hook
  "An event distributor that is invoked when entering Pseudo Shell mode."
  (make-event-distributor))

(define-variable pseudo-shell-active?
  "Is this shell buffer active?"
  #f
  boolean?)

(define-key 'pseudo-shell (kbd (ctrl #\a)) 'pseudo-shell-bol)
(define-key 'pseudo-shell (kbd (ctrl #\m)) 'pseudo-shell-execute-command)

(define-key 'pseudo-shell (kbd (meta p)) 'comint-previous-input)
(define-key 'pseudo-shell (kbd (meta n)) 'comint-next-input)
(define-key 'pseudo-shell (kbd (ctrl #\c) (ctrl #\r)) 'comint-history-search-backward)
(define-key 'pseudo-shell (kbd (ctrl #\c) (ctrl #\s)) 'comint-history-search-forward)
(define-key 'pseudo-shell (kbd (ctrl #\c) (ctrl #\w)) 'backward-kill-word)

;; (define-key 'pseudo-shell (kbd tab) 'comint-dynamic-complete)
;; (define-key 'pseudo-shell (kbd (meta #\?)) 'comint-dynamic-list-completions)

(define-command shell
  "Run an inferior pseudo shell, with I/O through buffer *shell*.
With prefix argument, unconditionally create a new buffer.
If buffer exists, just switch to buffer *shell*.

The buffer is put in Pseudo Shell mode, giving commands for sending input
and tracking directories."
  "P"
  (lambda (new-buffer?)
    (let ((buffer
	   (cond ((and (not new-buffer?)
		       (find-buffer "*shell*"))
		  => (lambda (buffer)
		       (let ((end (buffer-end buffer)))
			 (if (or (mark= end (line-start end 0))
				 (not (mark= end
					     (pseudo-shell-line-start end))))
			     (begin
			       (buffer-freshline buffer)
			       (insert-pseudo-shell-prompt!
				(buffer-end buffer)))))
		       buffer))
		 (else
		  (let ((buffer (create-buffer "*shell*")))
		    (insert-pseudo-shell-prompt! (buffer-start buffer))
		    (set-buffer-major-mode! buffer
					    (ref-mode-object pseudo-shell))
		    (set-buffer-default-directory!
		     buffer
		     (buffer-default-directory (current-buffer)))
		    buffer)))))

      (set-buffer-point! buffer (buffer-end buffer))
      (define-variable-local-value! buffer
	(ref-variable-object pseudo-shell-active?)
	#t)
      (select-buffer buffer))))

(define (insert-pseudo-shell-prompt! #!optional point)
  ;; This corresponds to the $p$g prompt pattern.
  (insert-string (string-append
		  (pseudo-directory-namestring 
		   (buffer-default-directory (current-buffer)))
		  ">")
		 (if (default-object? point)
		     (current-point)
		     point)))

(define (pseudo-directory-namestring dir)
  (string-upcase
   (->namestring
    (directory-pathname-as-file dir))))

(define-command pseudo-shell-bol
  "Goes to the beginning of line, then skips past the prompt, if any.
With argument, don't skip the prompt -- go straight to column 0."
  "P"
  (lambda (argument)
    (set-current-point!
     (if argument
	 (line-start (current-point) 0)
	 (pseudo-shell-line-start (current-point))))))

(define (pseudo-shell-line-start mark)
  (let ((start (line-start mark 0)))
    (let ((mark (search-forward ">" start (line-end start 0))))
      (if (and mark (mark<= mark (line-end start 0)))
	  mark
	  start))))

(define-command pseudo-shell-execute-command
  "Execute the command on the current line."
  ()
  (lambda ()
    (let ((point (current-point))
	  (buffer (current-buffer)))
      (let ((start (pseudo-shell-line-start point))
	    (end (line-end point 0)))
	(let ((command (extract-string start end)))
	  (ring-push! (ref-variable comint-input-ring)
		      command)
	  (if (not (mark= end (buffer-end buffer)))
	      (begin
		(buffer-freshline buffer)
		(insert-region (line-start start 0) end
			       (buffer-end buffer))))
	  (buffer-freshline buffer)
	  (dynamic-wind
	   (lambda ()
	     unspecific)
	   (lambda ()
	     (pseudo-execute command
			     (buffer-default-directory buffer)
			     (buffer-end buffer))
	     (insert-newline (buffer-end buffer)))
	   (lambda ()
	     (if (ref-variable pseudo-shell-active? buffer)
		 (begin
		   (buffer-freshline buffer)
		   (insert-pseudo-shell-prompt! (buffer-end buffer))
		   (set-buffer-point! buffer (buffer-end buffer)))))))))))

(define (buffer-freshline buffer)
  (let* ((end (buffer-end buffer))
	 (start (line-start end 0)))
    (if (not (mark= start end))
	(insert-newline end))))

(define (pseudo-execute command dir output-mark)
  (let* ((command (string-trim command))
	 (next (string-index command char-set:whitespace))
	 (prog (if (not next)
		   command
		   (substring command 0 next))))
    (let ((handler (assoc (string-downcase prog) pseudo-shell-builtins)))
      (if (not handler)
	  (shell-command #f output-mark dir command)
	  ((cdr handler) prog
			 (if (not next)
			     ""
			     (string-trim-left
			      (substring command (1+ next)
					 (string-length command))))
			 dir
			 output-mark)))))

(define-variable pseudo-shell-dirstack
  "List of directories saved by pushd in this buffer's shell."
  '())

(define-variable pseudo-shell-dirtrack? "" #f)

(define (pseudo-parse-directory dir prog args)
  (cond ((string-null? args)
	 #f)
	((string-index args char-set:whitespace)
	 (pseudo-error "Too many arguments" prog args))
	(else
	 (let ((dir (merge-pathnames args dir)))
	   (if (not (file-directory? dir))
	       (pseudo-error "Not a directory" prog args))
	   (pathname-simplify (pathname-as-directory dir))))))

(define (pseudo-error string . strings)
  (apply editor-error string
	 (map (lambda (string)
		(string-append " " string))
	      strings)))

(define pseudo-shell-builtins
  (let ((cd (lambda (prog args dir output-mark)
	      (if (not (ref-variable pseudo-shell-dirtrack?))
		  (editor-error "Not tracking directories"))
	      (let ((dir
		     (or (pseudo-parse-directory dir prog args)
			 (let ((home (get-environment-variable "HOME")))
			   (if (not home)
			       (pseudo-error "Unknown home:" prog)
			       (pathname-simplify
				(pathname-as-directory
				 (merge-pathnames home dir))))))))
		(set-default-directory dir)
		(insert-string
		 (string-append (pseudo-directory-namestring dir)
				"\n")
		 output-mark))))

	(show-dirs
	 (lambda (dir output-mark)
	   (with-output-to-mark output-mark
	     (lambda ()
	       (write-char #\()
	       (write-string (pseudo-directory-namestring dir))
	       (let loop ((dirs (ref-variable pseudo-shell-dirstack)))
		 (if (null? dirs)
		     (begin
		       (write-char #\))
		       (write-char #\Newline))
		     (begin
		       (write-char #\Space)
		       (write-string (pseudo-directory-namestring (car dirs)))
		       (loop (cdr dirs))))))))))

    `((""
       . ,(lambda (prog args dir output-mark)
	    prog args dir output-mark	; ignored
	    (message "Empty command line")
	    (editor-beep)))

      ("cd" . ,cd)

      ("pushd"
       . ,(lambda (prog args dir output-mark)
	    (if (not (ref-variable pseudo-shell-dirtrack?))
		(editor-error "Not tracking directories"))
	    (let ((dir* (pseudo-parse-directory dir prog args))
		  (stack (ref-variable pseudo-shell-dirstack)))
	      (cond (dir*
		     (set-variable! pseudo-shell-dirstack (cons dir stack))
		     (set-default-directory dir*)
		     (show-dirs dir* output-mark))
		    ((null? stack)
		     (pseudo-error "Empty directory stack:" prog))
		    (else
		     (let ((dir* (car stack)))
		       (set-variable! pseudo-shell-dirstack
				      (cons dir (cdr stack)))
		       (set-default-directory dir*)
		       (show-dirs dir* output-mark)))))))

      ("popd"
       . ,(lambda (prog args dir output-mark)
	    dir				; ignored
	    (if (not (ref-variable pseudo-shell-dirtrack?))
		(editor-error "Not tracking directories"))
	    (if (not (string-null? args))
		(pseudo-error "Too many arguments:" prog)
		(let ((stack (ref-variable pseudo-shell-dirstack)))
		  (if (null? stack)
		      (pseudo-error "Directory stack is empty:" prog)
		      (let ((dir (car stack)))
			(set-variable! pseudo-shell-dirstack (cdr stack))
			(set-default-directory dir)
			(insert-string
			 (string-append (pseudo-directory-namestring dir)
					"\n")
			 output-mark)))))))

      ("dirs"
       . ,(lambda (prog args dir output-mark)
	    (if (not (ref-variable pseudo-shell-dirtrack?))
		(editor-error "Not tracking directories"))
	    (if (not (string-null? args))
		(pseudo-error "Too many arguments:" prog))
	    (show-dirs dir output-mark)))

      ("cwd" . ,cd)

      ("exit"
       . ,(lambda (prog args dir output-mark)
	    prog args dir		; ignored
	    (define-variable-local-value! (mark-buffer output-mark)
	      (ref-variable-object pseudo-shell-active?)
	      #f)
	    (message "Pseudo exitted"))))))
