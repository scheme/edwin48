#| -*-Scheme-*-

$Id: comint.scm,v 1.35 2008/01/30 20:01:59 cph Exp $

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

;;;; Command interpreter subprocess control
;;; Translated from "comint.el", by Olin Shivers.


(define (make-comint mode buffer program . switches)
  (let ((buffer
	 (if (buffer? buffer)
	     buffer
	     (find-or-create-buffer buffer))))
    (if (let ((process (get-buffer-process buffer)))
	  (or (not process)
	      (not (process-runnable? process))))
	(begin
	  (comint-exec buffer (buffer-name buffer) program switches)
	  (set-buffer-major-mode! buffer mode)))
    buffer))

(define (comint-exec buffer name program switches)
  ;; Get rid of any old processes.
  (for-each delete-process (buffer-processes buffer))
  (set-buffer-point! buffer (buffer-end buffer))
  (local-set-variable! comint-program-name program buffer)
  (apply start-process
	 name
	 buffer
	 (process-environment-bind scheme-subprocess-environment
				   (string-append
				    "TERMCAP=emacs:co#"
				    (number->string
				     (screen-x-size (selected-screen)))
				    ":tc=unknown")
				   "TERM=emacs"
				   "EMACS=t")
	 program
	 switches))

(define-variable-per-buffer comint-prompt-regexp
  "Regexp to recognise prompts in the inferior process.
Defaults to \"^\", the null string at BOL.

Good choices:
  Canonical Lisp: \"^[^> ]*>+:? *\" (Lucid, franz, kcl, T, cscheme, oaklisp)
  Lucid Common Lisp: \"^\\(>\\|\\(->\\)+\\) *\"
  franz: \"^\\(->\\|<[0-9]*>:\\) *\"
  kcl: \"^>+ *\"
  shell: \"^[^#$%>]*[#$%>] *\"
  T: \"^>+ *\"

This is a good thing to set in mode hooks."
  "^")

(define-variable comint-last-input-end "" #f)

(define-variable comint-program-name
  "File name of program that is running in this buffer."
  #f)

(define (comint-strip-carriage-returns buffer)
  (let ((process (get-buffer-process buffer)))
    (if process
	(add-process-filter process process-filter:strip-carriage-returns))))

(define process-filter:strip-carriage-returns
  (standard-process-filter
   (lambda (mark string start end)
     (let ((group (mark-group mark)))
       (let loop ((start start))
	 (let ((cr
		(or (string-index string #\return start end)
		    end))
	       (index (mark-index mark)))
	   (group-insert-substring! group index string start cr)
	   (set-mark-index! mark (fix:+ index (fix:- cr start)))
	   (if (not (fix:= cr end))
	       (loop (fix:+ cr 1)))))))))

(define-major-mode comint fundamental "Comint"
  "Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

This mode is typically customised to create inferior-lisp-mode,
shell-mode, etc.. This can be done by setting the hooks
comint-input-sentinel, comint-input-filter, and comint-get-old-input
to appropriate procedures, and the variable comint-prompt-regexp to
the appropriate regular expression.

An input history is maintained of size comint-input-ring-size, and
can be accessed with the commands comint-next-input [\\[comint-next-input]] and
comint-previous-input [\\[comint-previous-input]].  Commands not keybound by
default are send-invisible, comint-dynamic-complete, and
comint-list-dynamic-completions.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

Entry to this mode runs the hooks on comint-mode-hook."
  (lambda (buffer)
    (local-set-variable! mode-line-process '(": %s") buffer)
    (local-set-variable! comint-input-ring
			 (make-ring
			  (ref-variable comint-input-ring-size buffer))
			 buffer)
    (local-set-variable! comint-last-input-end
			 (mark-right-inserting-copy (buffer-end buffer))
			 buffer)
    (local-set-variable! comint-last-input-match #f buffer)
    (event-distributor/invoke! (ref-variable comint-mode-hook buffer) buffer)))

(define-variable comint-mode-hook
  "An event distributor that is invoked when entering Comint mode."
  (make-event-distributor))

(define-key 'comint (kbd (ctrl #\d)) 'comint-delchar-or-maybe-eof)
(define-key 'comint (kbd (ctrl #\m)) 'comint-send-input)

(define-key 'comint (kbd (meta #\p)) 'comint-previous-input)
(define-key 'comint (kbd (meta #\n)) 'comint-next-input)
(define-key 'comint (kbd (meta #\s)) 'comint-previous-similar-input)

(define-key 'comint (kbd (ctrl #\c) (ctrl #\a)) 'comint-bol)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\c)) 'comint-interrupt-subjob)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\f)) 'comint-continue-subjob)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\l)) 'comint-show-output)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\o)) 'comint-flush-output)
;;(define-key 'comint (kbd (ctrl #\c) (ctrl #\q)) 'comint-send-char)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\r)) 'comint-history-search-backward)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\s)) 'comint-history-search-forward)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\u)) 'comint-kill-input)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\w)) 'backward-kill-word)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\z)) 'comint-stop-subjob)
(define-key 'comint (kbd (ctrl #\c) (ctrl #\\)) 'comint-quit-subjob)

(define-command comint-send-input
  "Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls
value of variable comint-get-old-input to retrieve old input, copies
it to the end of the buffer, and sends it.  A terminal newline is also
inserted into the buffer and sent to the process.  In either case,
value of variable comint-input-sentinel is called on the input before
sending it.  The input is entered into the input history ring, if
value of variable comint-input-filter returns non-false when called on
the input."
  ()
  (lambda () (comint-send-input "\n" #f)))

(define (comint-send-input terminator delete?)
  (let ((process (current-process)))
    (let ((mark (process-mark process)))
      (let ((string
	     (let ((point (current-point)))
	       (if (mark>= point mark)
		   (let ((end (group-end point)))
		     (set-current-point! end)
		     (extract-string mark end))
		   (let ((string ((ref-variable comint-get-old-input))))
		     (delete-string mark (group-end mark))
		     (set-current-point! mark)
		     (insert-string string mark)
		     string)))))
	(let ((point (current-point)))
	  (move-mark-to! (ref-variable comint-last-input-end) point)
	  (if ((ref-variable comint-input-filter) string)
	      (comint-record-input (ref-variable comint-input-ring) string))
	  ((ref-variable comint-input-sentinel) string)
	  (if delete?
	      (delete-string mark point)
	      (insert-newline point))
	  (move-mark-to! mark point)
	  (process-send-string process (string-append string terminator)))))))

(define-variable-per-buffer comint-get-old-input
  "Procedure that submits old text in comint mode.
This procedure is called when return is typed while the point is in old text.
It returns the text to be submitted as process input.  The default is
comint-get-old-input-default, which grabs the current line and strips off
leading text matching comint-prompt-regexp."
  (lambda ()
    (let ((mark (comint-line-start (current-point))))
      (extract-string mark (line-end mark 0)))))

(define-variable-per-buffer comint-input-sentinel
  "Called on each input submitted to comint mode process by comint-send-input.
Thus it can, for instance, track cd/pushd/popd commands issued to the shell."
  (lambda (string)
    string
    unspecific))

(define-variable-per-buffer comint-input-filter
  "Predicate for filtering additions to input history.
Only inputs answering #t to this procedure are saved on the input
history list.  Default is to save anything that isn't all whitespace."
  (lambda (string)
    (not (re-string-match "\\`\\s *\\'"
			  string
			  #f
			  (ref-variable syntax-table)))))

(define-command send-invisible
  "Read a string without echoing, and send it to the process running
in the current buffer.  A new-line is additionally sent.
String is not saved on comint input history list.
Security bug: your string can still be temporarily recovered with
\\[view-lossage]."
  ()
  (lambda ()
    (call-with-pass-phrase "Non-echoed text" send-invisible)))

(define (send-invisible string)
  (process-send-string (current-process) string)
  (process-send-string (current-process) "\n"))

(define-command comint-send-char
  "Send single character to process."
  "p"
  (lambda (prefix)
    (let ((string (string (read-quoted-char "Send Character: "))))
      (do ((i 0 (+ i 1)))
	  ((= i prefix))
	(process-send-string (current-process) string)))))

(define-command comint-previous-similar-input
  "Reenter the last input that matches the string typed so far.
If repeated successively, older inputs are reentered.
With negative arg, newer inputs are reentered."
  "p"
  (lambda (argument)
    (let ((tag '(COMINT-PREVIOUS-SIMILAR-INPUT))
	  (mark (comint-process-mark))
	  (point (current-point))
	  (ring (ref-variable comint-input-ring)))
      (if (mark< point mark)
	  (editor-error "Not after process mark"))
      (let ((do-it
	     (lambda (index* prefix)
	       (let ((size (ring-size ring)))
		 (let loop ((index index*))
		   (let ((index (+ index (if (negative? argument) -1 1))))
		     (if (or (negative? index)
			     (>= index size))
			 (begin
			   (editor-failure "Not found")
			   (if (not (= index* -1))
			       (set-command-message! tag index* prefix)))
			 (let ((string (ring-ref ring index)))
			   (if (string-prefix? prefix string)
			       (begin
				 (delete-string mark point)
				 (insert-string string point)
				 (set-command-message! tag index prefix))
			       (loop index))))))))))
	(command-message-receive tag
	  do-it
	  (lambda () (do-it -1 (extract-string mark point))))))))

(define-command comint-kill-input
  "Kill all text from last stuff output by interpreter to point."
  ()
  (lambda ()
    (let ((mark (comint-process-mark))
	  (point (current-point)))
      (if (mark>= point mark)
	  (kill-string mark point)
	  (editor-error "Nothing to kill")))))

(define-command comint-flush-output
  "Kill all output from interpreter since last input."
  ()
  (lambda ()
    (let ((start
	   (mark-left-inserting-copy
	    (let ((start (ref-variable comint-last-input-end)))
	      (if (eqv? #\newline (extract-right-char start))
		  (mark1+ start)
		  start))))
	  (end (line-start (comint-process-mark) 0)))
      (if (< (mark-index start) (mark-index end))
	  (begin
	    (delete-string start end)
	    (guarantee-newline start)
	    (insert-string "*** output flushed ***\n" start)))
      (mark-temporary! start))))

(define (comint-process-mark)
  (let ((buffer (selected-buffer)))
    ((or (buffer-get buffer 'COMINT-PROCESS-MARK #f)
	 (lambda (buffer)
	   (let ((process (get-buffer-process buffer)))
	     (if (not process)
		 (editor-error "Buffer has no process:" buffer))
	     (process-mark process))))
     buffer)))

(define-command comint-show-output
  "Start display of the current window at line preceding start of last output.
\"Last output\" is considered to start at the line following the last command
entered to the process."
  ()
  (lambda ()
    (let ((mark (line-start (ref-variable comint-last-input-end) 0)))
      (set-current-point! (comint-line-start mark))
      (set-window-start-mark! (current-window) mark #t))))

(define-command comint-bol
  "Goes to the beginning of line, then skips past the prompt, if any.
With argument, don't skip the prompt -- go straight to column 0.

The prompt skip is done by skipping text matching the regular expression
comint-prompt-regexp."
  "P"
  (lambda (argument)
    (set-current-point!
     (if argument
	 (line-start (current-point) 0)
	 (comint-line-start (current-point))))))

(define (comint-line-start mark)
  (let ((start (line-start mark 0)))
    (let ((mark
	   (re-match-forward (ref-variable comint-prompt-regexp mark)
			     start
			     (line-end mark 0))))
      (if (and mark (mark<= mark (line-end start 0)))
	  mark
	  start))))

(define-command comint-delchar-or-maybe-eof
  "If at end of buffer, send EOF to the current subprocess.
If not at end of buffer, just like \\[delete-char]."
  "P"
  (lambda (argument)
    (if (group-end? (current-point))
	(process-send-eof (current-process))
	((ref-command delete-char) argument))))

(define-command comint-interrupt-subjob
  "Sent an interrupt signal to the current subprocess.
If the process-connection-type is via ptys, the signal is sent to the current
process group of the pseudoterminal which Edwin is using to communicate with
the subprocess.  If the process is a job-control shell, this means the
shell's current subjob.  If the process connection is via pipes, the signal is
sent to the immediate subprocess."
  ()
  (lambda () (interrupt-process (current-process) #t)))

(define-command comint-kill-subjob
  "Send a kill signal to the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\"."
  ()
  (lambda () (kill-process (current-process) #t)))

(define-command comint-quit-subjob
  "Send a quit signal to the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\"."
  ()
  (lambda () (quit-process (current-process) #t)))

(define-command comint-stop-subjob
  "Stop the current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\".

WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer.  If you accidentally do
this, use \\[comint-continue-subjob] to resume the process.   (This is not a
problem with most shells, since they ignore this signal.)"
  ()
  (lambda () (stop-process (current-process) #t)))

(define-command comint-continue-subjob
  "Send a continue signal to current subprocess.
See comint-interrupt-subjob for a description of \"current subprocess\".
Useful if you accidentally suspend the top-level process."
  ()
  (lambda () (continue-process (current-process) #t)))

;;;; Filename Completion

(define-command comint-replace-by-expanded-filename
  "Replace the filename at point with its expanded, canonicalised completion.
\"Expanded\" means environment variables (e.g., $HOME) and ~'s are
replaced with the corresponding directories.  \"Canonicalised\" means ..
and . are removed, and the filename is made absolute instead of relative.
See also \\[comint-dynamic-complete]."
  ()
  (lambda ()
    (let ((region (comint-current-filename-region)))
      (let ((filename (region->string region)))
	(set-current-point! (region-end region))
	(comint-filename-complete
	 (merge-pathnames filename (buffer-default-directory (current-buffer)))
	 filename
	 (lambda (filename*)
	   (region-delete! region)
	   (insert-string filename* (region-start region))))))))

(define (comint-dynamic-complete-filename)
  "Complete the filename at point.
This function is similar to \\[comint-replace-by-expanded-filename], except
that it won't change parts of the filename already entered in the buffer;
it just adds completion characters to the end of the filename."
  (let ((region (comint-current-filename-region)))
    (let ((pathname
	   (merge-pathnames (region->string region)
			    (buffer-default-directory (current-buffer)))))
      (let ((filename (->namestring pathname)))
	(set-current-point! (region-end region))
	(comint-filename-complete
	 pathname
	 filename
	 (lambda (filename*)
	   (insert-substring filename*
			     (string-length filename)
			     (string-length filename*)
			     (region-end region)))))))
  #t)

(define-command comint-dynamic-list-completions
  "List all possible completions of the filename at point."
  ()
  (lambda ()
    (pop-up-generated-completions
     (lambda ()
       (filename-completions-list
	(merge-pathnames (region->string (comint-current-filename-region))
			 (buffer-default-directory (current-buffer))))))))

(define (comint-current-filename-region)
  (let ((point (current-point)))
    (os/comint-filename-region (let ((line-start (comint-line-start point)))
				 (if (mark< point line-start)
				     point
				     line-start))
			       point
			       (line-end point 0))))

(define (comint-filename-complete pathname filename insert-completion)
  (standard-completion filename
    (lambda (filename if-unique if-not-unique if-not-found)
      filename
      (filename-complete-string pathname if-unique if-not-unique if-not-found))
    insert-completion))

(define-variable comint-dynamic-complete-functions
  "List of functions called to perform completion.
Functions should return #t if completion was performed.
See also `comint-dynamic-complete'.

This is a good thing to set in mode hooks."
  (list comint-dynamic-complete-filename)
  (lambda (object)
    (and (list? object)
	 (every (lambda (object)
		  (and (procedure? object)
		       (procedure-arity-valid? object 0)))
		object))))

(define-command comint-dynamic-complete
  "Dynamically perform completion at point.
Calls the functions in `comint-dynamic-complete-functions' to perform
completion until a function returns true, at which point completion is
assumed to have occurred."
  ()
  (lambda ()
    (let loop ((thunks (ref-variable comint-dynamic-complete-functions)))
      (if (not (null? thunks))
	  (if (not ((car thunks)))
	      (loop (cdr thunks)))))))