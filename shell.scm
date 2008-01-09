#| -*-Scheme-*-

$Id: shell.scm,v 1.28 2007/01/05 21:19:24 cph Exp $

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

;;;; Shell subprocess in a buffer
;;; Translated from "cmushell.el", by Olin Shivers.


(define-variable shell-prompt-pattern
  "Regexp to match prompts in the inferior shell."
  (os/default-shell-prompt-pattern)
  string?)

(define-variable explicit-shell-file-name
  "If not #F, file name to use for explicitly requested inferior shell."
  #f
  string-or-false?)

(define-major-mode shell comint "Shell"
  "Major mode for interacting with an inferior shell.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

cd, pushd and popd commands given to the shell are watched to keep
this buffer's default directory the same as the shell's working directory.
\\[shell-resync-dirs] queries the shell and resyncs Edwin's idea of what the
    current directory stack is.
\\[shell-dirtrack-toggle] turns directory tracking on and off.

\\{shell}
Customisation: Entry to this mode runs the hooks on comint-mode-hook and
shell-mode-hook (in that order).

Variables shell-cd-regexp, shell-pushd-regexp and shell-popd-regexp are used
to match their respective commands."
  (lambda (buffer)
    (local-set-variable! comint-prompt-regexp
			 (ref-variable shell-prompt-pattern buffer)
			 buffer)
    (local-set-variable! comint-dynamic-complete-functions
			 (list shell-dynamic-complete-command
			       comint-dynamic-complete-filename)
			 buffer)
    (local-set-variable! comint-input-sentinel shell-directory-tracker buffer)
    (local-set-variable! shell-dirstack '() buffer)
    (local-set-variable! shell-dirtrack? #t buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable shell-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable shell-mode-hook buffer) buffer)))

(define-variable shell-mode-abbrev-table
  "Mode-specific abbrev table for Shell mode.")
(define-abbrev-table 'shell-mode-abbrev-table '())

(define-variable shell-mode-hook
  "An event distributor that is invoked when entering Shell mode."
  (make-event-distributor))

(define-key 'shell #\tab 'comint-dynamic-complete)
(define-key 'shell #\M-? 'comint-dynamic-list-completions)

(define-command shell
  "Run an inferior shell, with I/O through buffer *shell*.
With prefix argument, unconditionally create a new buffer and process.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, just switch to buffer *shell*.

The shell to use comes from the first non-#f variable found from these:
explicit-shell-file-name in Edwin, ESHELL in the environment or
shell-file-name in Edwin.

The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell."
  "P"
  (lambda (new-buffer?)
    (select-buffer
     (let ((program
	    (or (ref-variable explicit-shell-file-name)
		(get-environment-variable "ESHELL")
		(ref-variable shell-file-name))))
       (apply make-comint
	      (ref-mode-object shell)
	      (if (not new-buffer?) "*shell*" (new-buffer "*shell*"))
	      program
	      (let ((variable
		     (string-table-get editor-variables
				       (string-append "explicit-"
						      (os/shell-name program)
						      "-args"))))
		(if variable
		    (variable-value variable)
		    (os/default-shell-args))))))))

;;;; Directory Tracking

(define-variable shell-popd-regexp
  "Regexp to match subshell commands equivalent to popd."
  "popd")

(define-variable shell-pushd-regexp
  "Regexp to match subshell commands equivalent to pushd."
  "pushd")

(define-variable shell-cd-regexp
  "Regexp to match subshell commands equivalent to cd."
  "cd")

(define-variable shell-dirstack-query
  "Command used by shell-resync-dirs to query shell."
  "dirs")

(define-variable shell-dirstack
  "List of directories saved by pushd in this buffer's shell."
  '())

(define-variable shell-dirtrack? "" #f)

(define (shell-directory-tracker string)
  (if (ref-variable shell-dirtrack?)
      (let ((start
	     (let ((r
		    (re-string-match "^\\s *" string #f
				     (ref-variable syntax-table))))
	       (if r
		   (re-match-end-index 0 r)
		   0)))
	    (end (string-length string)))
	(let ((try
	       (let ((match
		      (lambda (regexp start)
			(re-substring-match regexp
					    string start end
					    #f
					    (ref-variable syntax-table)))))
		 (lambda (command)
		   (let ((eoc
			  (let ((r (match command start)))
			    (and r
				 (re-match-end-index 0 r)))))
		     (cond ((not eoc) #f)
			   ((match "\\s *\\(\;\\|$\\)" eoc) "")
			   ((match "\\s +\\([^ \t\;]+\\)\\s *\\(\;\\|$\\)" eoc)
			    => (lambda (r)
				 (substring string
					    (re-match-start-index 1 r)
					    (re-match-end-index 1 r))))
			   (else #f)))))))
	  (cond ((try (ref-variable shell-cd-regexp))
		 => shell-process-cd)
		((try (ref-variable shell-pushd-regexp))
		 => shell-process-pushd)
		((try (ref-variable shell-popd-regexp))
		 => shell-process-popd))))))

(define (shell-process-pushd arg)
  (let ((default-directory
	  (->namestring (buffer-default-directory (current-buffer))))
	(dirstack (ref-variable shell-dirstack)))
    (if (string-null? arg)
	;; no arg -- swap pwd and car of shell stack
	(if (null? dirstack)
	    (message "Directory stack empty")
	    (begin
	      (set-variable! shell-dirstack
			     (cons default-directory (cdr dirstack)))
	      (shell-process-cd (car dirstack))))
	(let ((num (shell-extract-num arg)))
	  (if num			; pushd +n
	      (if (> num (length dirstack))
		  (message "Directory stack not that deep")
		  (let ((dirstack
			 (let ((dirstack (cons default-directory dirstack)))
			   (append (list-tail dirstack num)
				   (list-head dirstack
					      (- (length dirstack) num))))))
		    (set-variable! shell-dirstack (cdr dirstack))
		    (shell-process-cd (car dirstack))))
	      (begin
		(set-variable! shell-dirstack
			       (cons default-directory dirstack))
		(shell-process-cd arg)))))))

(define (shell-process-popd arg)
  (let ((dirstack (ref-variable shell-dirstack))
	(num
	 (if (string-null? arg)
	     0
	     (shell-extract-num arg))))
    (cond ((not num)
	   (message "Bad popd"))
	  ((>= num (length dirstack))
	   (message "Directory stack empty"))
	  ((= num 0)
	   (set-variable! shell-dirstack (cdr dirstack))
	   (shell-process-cd (car dirstack)))
	  (else
	   (if (= num 1)
	       (set-variable! shell-dirstack (cdr dirstack))
	       (let ((pair (list-tail dirstack (- num 1))))
		 (set-cdr! pair (cddr pair))))
	   (shell-dirstack-message)))))

(define (shell-extract-num string)
  (and (re-string-match "^\\+[1-9][0-9]*$" string)
       (string->number string)))

(define (shell-process-cd filename)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:editor-error)
	 (lambda (condition)
	   (apply message (editor-error-strings condition))
	   (continuation unspecific))
       (lambda ()
	 (set-default-directory
	  (if (string-null? filename)
	      (user-homedir-pathname)
	      filename))))))
  (shell-dirstack-message))

(define (shell-dirstack-message)
  (apply message
	 (let loop
	     ((dirs
	       (cons (buffer-default-directory (current-buffer))
		     (ref-variable shell-dirstack))))
	   (cons (os/pathname->display-string (car dirs))
		 (if (null? (cdr dirs))
		     '()
		     (cons " " (loop (cdr dirs))))))))

(define-command shell-dirtrack-toggle
  "Turn directory tracking on and off in a shell buffer."
  "P"
  (lambda (argument)
    (set-variable!
     shell-dirtrack?
     (let ((argument (command-argument-value argument)))
       (cond ((not argument) (not (ref-variable shell-dirtrack?)))
	     ((positive? argument) #t)
	     ((negative? argument) #f)
	     (else (ref-variable shell-dirtrack?)))))
    (message "Directory tracking "
	     (if (ref-variable shell-dirtrack?) "on" "off")
	     ".")))

(define-command shell-resync-dirs
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to
shell-dirstack-query (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose.  If this happens, just do the
command again."
  ()
  (lambda ()
    (let ((process (current-process)))
      (let ((mark (process-mark process)))
	(set-current-point! mark)
	(let ((pending-input
	       ;; Kill any pending input.
	       (extract-and-delete-string mark (group-end mark)))
	      (point (mark-left-inserting-copy (current-point))))
	  ;; Insert the command, then send it to the shell.
	  (let ((dirstack-query (ref-variable shell-dirstack-query)))
	    (insert-string dirstack-query point)
	    (move-mark-to! (ref-variable comint-last-input-end) point)
	    (insert-newline point)
	    (move-mark-to! mark point)
	    (process-send-string process (string-append dirstack-query "\n")))
	  ;; Wait for a line of output.
	  (let ((output-line
		 (let ((output-start (mark-right-inserting-copy point)))
		   (do ()
		       ((re-match-forward ".*\n" output-start)
			(mark-temporary! output-start)
			(extract-string (re-match-start 0)
					(mark-1+ (re-match-end 0))))
		     (accept-process-output)))))
	    ;; Restore any pending input.
	    (insert-string pending-input point)
	    (mark-temporary! point)
	    (let ((dirlist (shell-tokenize-dirlist output-line)))
	      (set-variable! shell-dirstack (cdr dirlist))
	      (shell-process-cd (car dirlist)))))))))

(define (shell-tokenize-dirlist string)
  (let ((end (string-length string)))
    (let skip-spaces ((start 0))
      (cond ((= start end)
	     '())
	    ((char=? #\space (string-ref string start))
	     (skip-spaces (+ start 1)))
	    (else
	     (let skip-nonspaces ((index (+ start 1)))
	       (cond ((= index end)
		      (list (substring string start end)))
		     ((char=? #\space (string-ref string index))
		      (cons (substring string start index)
			    (skip-spaces (+ index 1))))
		     (else
		      (skip-nonspaces (+ index 1))))))))))

;;;; Command Completion

(define-variable shell-command-regexp
  "Regexp to match a single command within a pipeline.
This is used for command completion and does not do a perfect job."
  (os/shell-command-regexp)
  string?)

(define-variable shell-completion-execonly
  "If true, use executable files only for completion candidates.
This mirrors the optional behavior of tcsh.

Detecting executability of files may slow command completion considerably."
  #t
  boolean?)

(define (shell-backward-command mark n)
  (and (> n 0)
       (let ((limit
	      (let ((limit (comint-line-start mark)))
		(if (mark> limit mark)
		    (line-start mark 0)
		    limit)))
	     (regexp
	      (string-append "["
			     (os/shell-command-separators)
			     "]+[\t ]*\\("
			     (ref-variable shell-command-regexp mark)
			     "\\)")))
	 (let loop
	     ((mark
	       (let ((m (re-search-backward "\\S " mark limit #f)))
		 (if m
		     (mark1+ m)
		     limit)))
	      (n n))
	   (let ((mark* (re-search-backward regexp mark limit #f))
		 (n (- n 1)))
	     (if mark*
		 (if (> n 0)
		     (loop mark* (- n 1))
		     (skip-chars-forward (os/shell-command-separators)
					 (re-match-start 1)))
		 limit))))))

(define (shell-dynamic-complete-command)
  "Dynamically complete the command at point.
This function is similar to `comint-dynamic-complete-filename', except that it
searches the PATH environment variable for completion candidates.
Note that this may not be the same as the shell's idea of the path.

Completion is dependent on the value of `shell-completion-execonly', plus
those that effect file completion."
  (let ((r (comint-current-filename-region)))
    (and (not (mark= (region-start r) (region-end r)))
	 (string=? "" (directory-namestring (region->string r)))
	 (let ((m (shell-backward-command (current-point) 1)))
	   (and m
		(mark= (region-start r) m)))
	 (begin
	   (message "Completing command name...")
	   (standard-completion (region->string r)
	     (lambda (filename if-unique if-not-unique if-not-found)
	       (shell-complete-command
		(parse-namestring filename)
		(ref-variable shell-completion-execonly (region-start r))
		if-unique if-not-unique if-not-found))
	     (lambda (filename)
	       (region-delete! r)
	       (insert-string filename (region-start r))))))))

(define (shell-complete-command command exec-only?
				if-unique if-not-unique if-not-found)
  (let* ((results '())
	 (maybe-add-filename!
	  (let ((add-filename!
		 (lambda (filename)
		   (let ((s (file-namestring filename)))
		     (if (not (member s results))
			 (set! results (cons s results))))
		   unspecific)))
	    (if exec-only?
		(lambda (filename)
		  (if (file-executable? filename)
		      (add-filename! filename)))
		add-filename!))))
    (for-each
     (lambda (directory)
       (filename-complete-string (merge-pathnames command directory)
	 maybe-add-filename!
	 (lambda (common get-completions)
	   (let ((directory (directory-pathname common)))
	     (for-each
	      (lambda (filename)
		(maybe-add-filename! (merge-pathnames directory filename)))
	      (get-completions))))
	 (lambda () unspecific)))
     (os/parse-path-string (get-environment-variable "PATH")))
    (cond ((null? results)
	   (if-not-found))
	  ((null? (cdr results))
	   (if-unique
	    (let ((result (car results)))
	      (if (member (pathname-type result)
			  (os/executable-pathname-types))
		  (->namestring (pathname-new-type result #f))
		  result))))
	  (else
	   (if-not-unique (compute-max-prefix results) (lambda () results))))
    (not (null? results))))

(define (compute-max-prefix strings)
  (let loop ((prefix (car strings)) (strings (cdr strings)))
    (if (null? strings)
	prefix
	(loop (let ((n (string-match-forward prefix (car strings))))
		(if (fix:< n (string-length prefix))
		    (string-head prefix n)
		    prefix))
	      (cdr strings)))))