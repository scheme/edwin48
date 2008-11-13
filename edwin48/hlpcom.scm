#| -*-Scheme-*-

$Id: hlpcom.scm,v 1.132 2008/01/30 20:02:01 cph Exp $

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

;;;; Help Commands


(define-command help-prefix
  "This is a prefix for more commands.
It reads another character (a subcommand) and dispatches on it."
  "cA B C F I K L M T V W or C-h for more help"
  (lambda (char)
    (dispatch-on-key
     (current-comtabs)
     (list #\Backspace
	   (if (or (char=? char #\Backspace)
		   (char=? char #\?))
	       (cleanup-pop-up-buffers
		(lambda ()
		  (let ((buffer (temporary-buffer "*Help*")))
		    (insert-string the-help-text (buffer-point buffer))
		    (set-buffer-point! buffer (buffer-start buffer))
		    (buffer-not-modified! buffer)
		    (pop-up-buffer buffer #f)
		    (let ((window (get-buffer-window buffer)))
		      (let loop ()
			(let ((char
			       (prompt-for-char
				"A B C F I K L M T V W or space to scroll")))
			  (let ((test-for
				 (lambda (char*)
				   (char=? char (remap-alias-key char*)))))
			    (cond ((or (test-for #\C-h)
				       (test-for #\?))
				   (loop))
				  ((or (test-for #\space)
				       (test-for #\C-v))
				   (scroll-window
				    window
				    (standard-scroll-window-argument
				     window #f 1)
				    editor-beep)
				   (loop))
				  ((or (test-for #\rubout)
				       (test-for #\M-v))
				   (scroll-window
				    window
				    (standard-scroll-window-argument
				     window #f -1)
				    editor-beep)
				   (loop))
				  (else char)))))))))
	       char)))))

(define the-help-text
  "You have typed C-h, the help character.  Type a Help option:

A  command-apropos.  Type a substring, and see a list of commands
              that contain that substring.
B  describe-bindings.  Display table of all key bindings.
C  describe-key-briefly.  Type a key sequence;
              it prints the name of the command that sequence runs.
F  describe-function.  Type a command name and get its documentation.
I  info.  The Info documentation reader.
K  describe-key.  Type a key sequence;
              it prints the full documentation.
L  view-lossage.  Prints the last 100 characters you typed.
M  describe-mode.  Print documentation of current major mode,
              which describes the commands peculiar to it.
S  describe-syntax.  Display contents of syntax table, plus explanations.
T  help-with-tutorial.  Select the Emacs learn-by-doing tutorial.
V  describe-variable.  Type a variable name and get its documentation.
W  where-is.  Type a command name and get its key binding.")

;;;; Commands and Keys

(define-command command-apropos
  "Show all commands whose names contain a match for REGEXP."
  "sCommand apropos (regexp)"
  (lambda (regexp)
    (with-output-to-help-display
     (lambda ()
       (command-apropos regexp)))))

(define-command apropos-command
  (command-description (ref-command-object command-apropos))
  (command-interactive-specification (ref-command-object command-apropos))
  (command-procedure (ref-command-object command-apropos)))

(define (command-apropos regexp)
  (for-each (lambda (command)
	      (let ((name (command-name-string command)))
		(write-string name)
		(print-key-bindings command (string-length name)))
	      (newline)
	      (print-short-description "Command"
				       (command-description command)))
	    (string-table-apropos editor-commands regexp)))

(define-command describe-function
  "Prompts for a command, and describes it.
Prints the full documentation for the given command."
  "CDescribe command"
  (lambda (name)
    (help-describe-command (name->command name))))

(define (help-describe-command command)
  (with-output-to-help-display
   (lambda ()
     (write-string (command-name-string command))
     (write-string ":\n")
     (write-description (command-description command)))))

(define-command where-is
  "Prompts for a command, and shows what key it is bound to."
  "CWhere is command"
  (lambda (name)
    (let ((command (name->command name)))
      (let ((bindings (comtab-key-bindings (current-comtabs) command)))
	(if (pair? bindings)
	    (message (command-name-string command) " is on "
		     (key->name (car bindings)))
	    (message (command-name-string command) " is not on any keys"))))))

(define-command describe-key-briefly
  "Prompts for a key, and describes the command it is bound to.
Prints the brief documentation for that command."
  "kDescribe key briefly"
  (lambda (key)
    (let ((command (local-comtab-entry (current-comtabs) key (current-point))))
      (if (eq? command (ref-command-object undefined))
	  (help-describe-unbound-key key)
	  (message (xkey->name key)
		   " runs the command "
		   (command-name-string command))))))

(define-command describe-key
  "Prompts for a key, and describes the command it is bound to.
Prints the full documentation for that command."
  "kDescribe key"
  (lambda (key)
    (let ((command (local-comtab-entry (current-comtabs) key (current-point))))
      (if (eq? command (ref-command-object undefined))
	  (help-describe-unbound-key key)
	  (help-describe-command command)))))

(define (help-describe-unbound-key key)
  (message (xkey->name key) " is undefined"))

;;;; Variables

(define-command variable-apropos
  "Show all variables whose names contain a match for REGEXP."
  "sVariable apropos (regexp)"
  (lambda (regexp)
    (with-output-to-help-display
     (lambda ()
       (variable-apropos regexp)))))

(define-command apropos-variable
  (command-description (ref-command-object variable-apropos))
  (command-interactive-specification (ref-command-object variable-apropos))
  (command-procedure (ref-command-object variable-apropos)))

(define (variable-apropos regexp)
  (for-each (lambda (variable)
	      (write-string (variable-name-string variable))
	      (newline)
	      (print-short-description "Variable"
				       (variable-description variable)))
	    (string-table-apropos editor-variables regexp)))

(define-command describe-variable
  "Prompts for a variable, and describes it.
Prints the full documentation for the given variable."
  "vDescribe variable"
  (lambda (name)
    (let ((variable (name->variable name)))
      (with-output-to-help-display
       (lambda ()
	 (write-string (variable-name-string variable))
	 (newline)
	 (print-variable-binding variable)
	 (write-string "\nDocumentation:\n")
	 (write-description (variable-description variable)))))))

(define-command set-variable
  "Set VARIABLE to VALUE.  VALUE is a Scheme object.
When using this interactively, supply a Scheme expression for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes."
  (lambda ()
    (let ((variable (prompt-for-variable "Set variable")))
      (list (variable-name variable)
	    (prompt-for-expression-value
	     (string-append "Set " (variable-name-string variable) " to value")
	     (variable-value variable)))))
  (lambda (variable value)
    (set-variable-value! (name->variable variable) value)))

(define-command make-local-variable
  "Make a variable have a local value in the current buffer."
  (lambda ()
    (let ((variable (prompt-for-variable "Make local variable")))
      (list (variable-name variable)
	    (prompt-for-expression-value
	     (string-append "Set " (variable-name-string variable) " to value")
	     (variable-value variable)))))
  (lambda (variable value)
    (define-variable-local-value! (current-buffer) (name->variable variable)
      value)))

(define-command kill-local-variable
  "Make a variable use its global value in the current buffer."
  "vKill local variable"
  (lambda (name)
    (undefine-variable-local-value! (current-buffer) (name->variable name))))

;;;; Other Stuff

(define-command apropos
  "Show all commands, variables, and modes matching REGEXP."
  "sApropos (regexp)"
  (lambda (regexp)
    (with-output-to-help-display
     (lambda ()
       (command-apropos regexp)
       (variable-apropos regexp)
       (mode-apropos regexp)))))

(define (mode-apropos regexp)
  (for-each (lambda (mode)
	      (write-string (symbol-name (mode-name mode)))
	      (newline)
	      (print-short-description "Mode" (mode-description mode)))
	    (string-table-apropos editor-modes regexp)))

(define-command view-lossage
  "Print the keyboard history."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (for-each (lambda (key)
		   (write-string (string-append (key-name key) " ")))
		 (reverse (ring-list (current-char-history))))))))

(define-command describe-mode
  "Print the documentation for the current mode."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (write-description (mode-description (current-major-mode)))))))

(define-command help-with-tutorial
  "Visit the Edwin learn-by-doing tutorial."
  ()
  (lambda ()
    (delete-other-windows (current-window))
    (let ((pathname (merge-pathnames "TUTORIAL" (default-homedir-pathname))))
      (let ((buffer (pathname->buffer pathname)))
	(if buffer
	    (select-buffer buffer)
	    (let ((buffer (new-buffer (pathname->buffer-name pathname))))
	      (read-buffer buffer (edwin-tutorial-pathname) #t)
	      (set-buffer-pathname! buffer pathname)
	      (set-buffer-truename! buffer #f)
	      (select-buffer buffer)
	      (set-current-major-mode! (ref-mode-object fundamental))
	      (disable-buffer-auto-save! buffer)
	      (let ((mark
		     (line-start (search-forward "\n<<"
						 (buffer-start buffer)
						 (buffer-end buffer))
				 0)))
		(delete-string (line-end mark -1) (line-end mark 0))
		(let ((wanted-newlines
		       (- (window-y-size (current-window))
			  ;; Add four to account for the length of the
			  ;; message about using C-v.
			  (+ 4
			     (region-count-lines
			      (make-region (buffer-start buffer) mark))))))
		  (if (> wanted-newlines 0)
		      (insert-newlines wanted-newlines mark)
		      (begin
			;; Add a single newline anyway for aesthetics.
			(insert-newline mark)
			(message "Tutorial does not fit in window;"
				 " type C-v to scroll down.")))))
	      (set-buffer-point! buffer (buffer-start buffer))
	      (buffer-not-modified! buffer)))))))

(define (with-output-to-help-display thunk)
  (string->temporary-buffer (with-output-to-string thunk)
			    "*Help*"
			    '(READ-ONLY)))

(define (write-description description #!optional port)
  (write-string (substitute-command-keys description)
		(if (default-object? port) (current-output-port) port)))

(define (print-key-bindings command column #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port))
	(bindings (comtab-key-bindings (current-comtabs) command)))
    (if (pair? bindings)
	(begin
	  (write-string (if (< column 30)
			    (make-string (- 30 column) #\space)
			    " ")
			port)
	  (write-string (key-list-string bindings) port)))))

(define (key-list-string xkeys)
  (let loop ((xkeys (sort xkeys xkey<?)))
    (if (pair? (cdr xkeys))
	(string-append (xkey->name (car xkeys))
		       ", "
		       (loop (cdr xkeys)))
	(xkey->name (car xkeys)))))

(define (print-variable-binding variable #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (write-string "    which is bound to: " port)
    (write (variable-value variable) port)
    (newline port)))

(define (print-short-description prefix description #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (write-string "    " port)
    (if prefix
	(begin
	  (write-string prefix port)
	  (write-string ": " port)))
    (write-description (description-first-line description) port)
    (newline port)))

(define (substitute-command-keys description #!optional buffer)
  (let* ((string (description->string description))
	 (buffer (if (default-object? buffer) (current-buffer) buffer))
	 (end (string-length string)))

    (define (find-escape start* comtabs)
      (let loop ((start start*))
	(let ((index (string-index string #\\ start end)))
	  (if (not index)
	      (list (substring string start* end))
	      (let ((next (fix:+ index 1)))
		(cond ((fix:= next end)
		       (list (substring string start* end)))
		      ((char=? #\[ (string-ref string next))
		       (find-terminator start* index #\] subst-key comtabs))
		      ((char=? #\{ (string-ref string next))
		       (find-terminator start* index #\}
					show-bindings comtabs))
		      ((char=? #\< (string-ref string next))
		       (find-terminator start* index #\> new-mode comtabs))
		      ((char=? #\= (string-ref string next))
		       (cons (substring string start* index)
			     (quote-next (fix:+ next 1) comtabs)))
		      (else
		       (loop next))))))))

    (define (find-terminator start slash char procedure comtabs)
      (cons (substring string start slash)
	    (let ((start (fix:+ slash 2)))
	      (let ((terminator
		     (string-index string char start end)))
		(if (not terminator)
		    (error "Missing terminator character:" char))
		(procedure (intern (substring string start terminator))
			   (fix:+ terminator 1)
			   comtabs)))))

    (define (subst-key argument next comtabs)
      (cons (let ((command (name->command argument #f)))
	      (if command
		  (let ((bindings (comtab-key-bindings comtabs command)))
		    (if (pair? bindings)
			(xkey->name (car bindings))
			(string-append "M-x " (command-name-string command))))
		  (string-append "M-x " (symbol-name argument))))
	    (find-escape next comtabs)))

    (define (show-bindings argument next comtabs)
      comtabs
      (cons (call-with-output-string
	     (lambda (port)
	       (describe-bindings (mode-comtabs (name->mode argument 'ERROR))
				  #f
				  port)
	       (newline port)))
	    (find-escape next comtabs)))

    (define (new-mode argument next comtabs)
      comtabs
      (find-escape next (mode-comtabs (name->mode argument 'ERROR))))

    (define (quote-next start comtabs)
      (if (fix:= start end)
	  (finish start)
	  (let ((next (fix:+ start 1)))
	    (if (char=? #\\ (string-ref string start))
		(if (fix:= next end)
		    (finish start)
		    (continue start (fix:+ next 1) comtabs))
		(continue start next comtabs)))))

    (define (continue start end comtabs)
      (cons (substring string start end)
	    (find-escape end comtabs)))

    (define (finish start)
      (list (substring string start end)))

    (apply string-append (find-escape 0 (buffer-comtabs buffer)))))