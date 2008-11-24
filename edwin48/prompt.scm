#| -*-Scheme-*-

$Id: prompt.scm,v 1.205 2008/01/30 20:02:04 cph Exp $

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

;;;; User Prompting
;;; Package: (edwin prompt)


(define typein-edit-abort-flag "Abort")
(define typein-edit-continuation)
(define typein-edit-depth)
(define typein-saved-buffers)
(define typein-saved-windows)
(define map-name/internal->external)
(define map-name/external->internal)

(add-event-receiver! editor-initializations
  (lambda ()
    (set! typein-edit-continuation #f)
    (set! typein-edit-depth -1)
    (set! typein-saved-buffers '())
    (set! typein-saved-windows '())
    (set! map-name/internal->external identity-procedure)
    (set! map-name/external->internal identity-procedure)
    (set! prompt-histories (make-hash-table eq?))
    unspecific))

(define (make-typein-buffer-name depth)
  (string-append " *Typein-" (number->string depth) "*"))

(define (within-typein-edit thunk)
  (let ((value
	 (call-with-current-continuation
	  (lambda (continuation)
	    (fluid-let ((typein-edit-continuation continuation)
			(typein-edit-depth (1+ typein-edit-depth))
			(typein-saved-buffers
			 (cons (window-buffer (typein-window))
			       typein-saved-buffers))
			(typein-saved-windows
			 (cons (current-window)
			       typein-saved-windows)))
	      (dynamic-wind
	       (lambda () unspecific)
	       (lambda ()
		 (let ((window (typein-window)))
		   (select-window window)
		   (select-buffer
		    (find-or-create-buffer
		     (make-typein-buffer-name typein-edit-depth)))
		   (buffer-reset! (current-buffer))
		   (reset-command-prompt!)
		   (window-clear-override-message! window))
		 (thunk))
	       (lambda ()
		 (let ((window (typein-window)))
		   (select-window window)
		   (let ((buffer (car typein-saved-buffers)))
		     (bufferset-guarantee-buffer! (current-bufferset) buffer)
		     (select-buffer buffer))
		   (reset-command-prompt!)
		   (window-clear-override-message! window))
		 (if (zero? typein-edit-depth)
		     (buffer-reset! (current-buffer)))
		 (cond ((window-visible? (car typein-saved-windows))
			(select-window (car typein-saved-windows)))
		       ((zero? typein-edit-depth)
			(select-window (other-window)))))))))))
    (cond ((condition? value)
	   (signal-condition value))
	  ((and (pair? value) (eq? (car value) typein-edit-abort-flag))
	   (apply-input-event (cdr value))
	   (within-typein-edit thunk))
	  (else
	   value))))

(define (within-typein-edit?)
  (not (null? typein-saved-windows)))

(define (typein-edit-other-window)
  (let loop ((windows typein-saved-windows))
    (if (pair? windows)
	(if (and (not (typein-window? (car windows)))
		 (window-visible? (car windows)))
	    (car windows)
	    (loop (cdr windows)))
	(window0))))

(define-variable enable-recursive-minibuffers
  "True means allow minibuffers to invoke commands that use recursive minibuffers."
  #f
  boolean?)

(define-variable completion-auto-help
  "True means automatically provide help for invalid completion input."
  #t
  boolean?)

(define (prompt-for-typein prompt-string check-recursion? thunk)
  (if (and check-recursion?
	   (not (ref-variable enable-recursive-minibuffers))
	   (typein-window? (current-window)))
      (editor-error "Command attempted to use minibuffer while in minibuffer"))
  (cleanup-pop-up-buffers
   (lambda ()
     (within-typein-edit
      (lambda ()
	(insert-string prompt-string)
	(let ((mark (current-point)))
	  (with-text-clipped (mark-right-inserting mark)
			     (mark-left-inserting mark)
	    (lambda ()
	      (bind-condition-handler (list condition-type:^G)
		  (lambda (condition)
		    (cond ((not (eq? (current-window) (typein-window)))
			   (signal-condition condition))
			  (typein-edit-continuation
			   (typein-edit-continuation condition))
			  (else
			   (error "illegal ^G signaled in typein window"))))
		thunk)))))))))

(define (typein-editor-thunk mode)
  (lambda ()
    (let ((buffer (current-buffer)))
      (ring-clear! (buffer-mark-ring buffer))
      (push-current-mark! (buffer-start buffer))
      (if (major-mode? mode)
          (set-buffer-major-mode! buffer mode)
          (mode buffer))))
  (command-reader))

(define (abort-typein-edit event)
  (typein-edit-continuation (cons typein-edit-abort-flag event)))

(define (exit-typein-edit)
  (if (not typein-edit-continuation)
      (error "Not editing typein; can't exit"))
  ;; Indicate that typein has been accepted.
  (let ((window (current-window)))
    (window-home-cursor! window)
    (typein-edit-continuation (buffer-string (window-buffer window)))))

(define (typein-string)
  (map-name/external->internal (buffer-string (current-buffer))))

(define* (set-typein-string! string (update? #t))
  (let ((dont-update?
	 (or (not update?)
	     (window-needs-redisplay? (typein-window)))))
    (region-delete! (buffer-region (current-buffer)))
    (insert-string (map-name/internal->external string))
    (if (not dont-update?) (update-typein!))))

(define (update-typein!)
    (if (not *executing-keyboard-macro?*)
	(window-direct-update! (typein-window) #f)))

;;;; String Prompt

(define (prompt-for-string prompt default-string . options)
  (%prompt-for-string
   prompt
   (parse-prompt-options
    (basic-prompt-options (ref-mode-object minibuffer-local)
			  default-string)
    options)))

(define (prompt-for-completed-string prompt
				     default-string
				     complete-string
				     list-completions
				     verify-final-value
				     . options)
  (%prompt-for-string
   prompt
   (parse-prompt-options
    (completion-prompt-options (ref-mode-object minibuffer-local-completion)
			       default-string
			       complete-string
			       list-completions
			       verify-final-value)
    options)))

(define *options*)

(define (%prompt-for-string prompt options)
  (fluid-let ((*options* options))
    (let ((type (default-type))
	  (string (default-string)))
      (let ((initial-string
	     (if (and string (eq? type 'INSERTED-DEFAULT))
		 string
		 "")))
	(with-history-state initial-string
	  (lambda ()
	    (prompt-for-typein
	     (prompt-for-string/prompt prompt
				       (and string
					    (eq? 'VISIBLE-DEFAULT type)
					    (write-to-string string)))
	     #t
	     (let ((thunk (typein-editor-thunk (options/mode *options*))))
	       (lambda ()
		 (insert-string initial-string)
		 ((thunk)))))))))))

(define (default-type) (options/default-type *options*))
(define (default-string) (options/default-string *options*))

(define (case-insensitive-completion?)
  (options/case-insensitive-completion? *options*))

(define (prompt-for-string/prompt prompt default-string)
  (cond ((string? prompt)
	 (string-append
	  prompt
	  (if default-string
	      (string-append " (default is: " default-string ")")
	      "")
	  (if (string-suffix? " " prompt)
	      ""
	      ": ")))
	((and (pair? prompt)
	      (string? (car prompt))
	      (null? (cdr prompt)))
	 (car prompt))
	(else
	 (error:wrong-type-argument prompt
				    "prompt string"
				    'PROMPT-FOR-STRING/PROMPT))))

(define (prompt-for-number prompt default . options)
  (let ((string
	 (apply prompt-for-string
		prompt
		(and default (number->string default))
		options)))
    (or (string->number string)
	(editor-error "Input not a number: " string))))

(define (prompt-for-string-table-name prompt default-string string-table
				      . options)
  (apply prompt-for-completed-string
	 prompt
	 default-string
	 (lambda (string if-unique if-not-unique if-not-found)
	   (string-table-complete string-table
				  string
				  if-unique
				  if-not-unique
				  if-not-found))
	 (lambda (string)
	   (string-table-completions string-table string))
	 (lambda (string)
	   (let ((default (list 'DEFAULT)))
	     (not (eq? (string-table-get string-table string
					 (lambda (index) index default))
		       default))))
	 'CASE-INSENSITIVE-COMPLETION? (string-table-ci? string-table)
	 options))

(define (prompt-for-string-table-value prompt default-string string-table
				       . options)
  (string-table-get string-table
		    (apply prompt-for-string-table-name
			   prompt default-string string-table
			   options)))

(define* (prompt-for-alist-value prompt alist (default #f) (ci? #t))
  (fluid-let ((map-name/external->internal identity-procedure)
	      (map-name/internal->external identity-procedure))
    (prompt-for-string-table-value prompt
                                   default
				   (alist->string-table
				    alist
                                    ci?)
				   'REQUIRE-MATCH? #t)))

(define (prompt-for-command prompt . options)
  (fluid-let ((map-name/external->internal editor-name/external->internal)
	      (map-name/internal->external editor-name/internal->external))
    (apply prompt-for-string-table-value prompt #f editor-commands
	   'DEFAULT-TYPE 'NO-DEFAULT
	   'REQUIRE-MATCH? #t
	   options)))

(define (prompt-for-variable prompt . options)
  (fluid-let ((map-name/external->internal editor-name/external->internal)
	      (map-name/internal->external editor-name/internal->external))
    (apply prompt-for-string-table-value prompt #f editor-variables
	   'DEFAULT-TYPE 'NO-DEFAULT
	   'REQUIRE-MATCH? #t
	   options)))

;;;; Prompt Options

(define-structure
  (prompt-options (conc-name options/)
		  (constructor basic-prompt-options
			       (mode default-string))
		  (constructor completion-prompt-options
			       (mode default-string
				     complete-string
				     list-completions
				     verify-final-value)))
  (seen '())
  (mode #f)
  (default-string #f)
  (complete-string #f read-only #t)
  (list-completions #f read-only #t)
  (verify-final-value #f read-only #t)
  (default-type 'VISIBLE-DEFAULT)
  (require-match? #f)
  (case-insensitive-completion? #f)
  (history #f)
  (history-index -1))

(define (parse-prompt-options option-structure options)
  (let loop ((options options))
    (cond ((and (pair? options)
		(symbol? (car options))
		(pair? (cdr options)))
	   (let ((entry (assq (car options) prompt-options-table))
		 (arg (cadr options)))
	     (if (not entry)
		 (error "Unknown prompt option:" (car options)))
	     (set-options/seen! option-structure
				(cons (car options)
				      (options/seen option-structure)))
	     (if (not (let ((predicate (cadr entry)))
			(if (pair? predicate)
			    (any (lambda (p) (p arg)) predicate)
			    (predicate arg))))
		 (error "Not a valid option argument:" arg))
	     ((cddr entry) option-structure arg)
	     (loop (cddr options))))
	  ((null? options)
	   option-structure)
	  (else
	   (error "Illegal options tail:" options)))))

(define (lookup-prompt-option options keyword default)
  ;; If there are multiple instances of KEYWORD, return the last.
  (let loop ((options options) (winner #f))
    (if (pair? options)
	(begin
	  (if (not (pair? (cdr options)))
	      (error "Options list has odd length:" options))
	  (loop (cddr options)
		(if (eq? keyword (car options)) options winner)))
	(begin
	  (if (not (null? options))
	      (error "Illegal options tail:" options))
	  (if winner
	      (cadr winner)
	      default)))))

(define prompt-options-table
  '())

(define (define-prompt-option keyword type modifier)
  (let ((entry (assq keyword prompt-options-table))
	(body (cons type modifier)))
    (if entry
	(set-cdr! entry body)
	(begin
	  (set! prompt-options-table
		(cons (cons keyword body)
		      prompt-options-table))
	  unspecific))))

(define (define-simple-option keyword type)
  (define-prompt-option keyword type
    (lambda (options value)
      ((record-modifier (record-type-descriptor options) keyword)
       options
       value))))

(define-simple-option 'MODE (list major-mode? procedure?))
(define-simple-option 'DEFAULT-STRING string-or-false?)
(define-simple-option 'CASE-INSENSITIVE-COMPLETION? boolean?)

(define-simple-option 'DEFAULT-TYPE
  (lambda (object)
    (memq object
	  '(VISIBLE-DEFAULT
	    INVISIBLE-DEFAULT
	    INSERTED-DEFAULT
	    NULL-DEFAULT
	    NO-DEFAULT))))

(define-prompt-option 'REQUIRE-MATCH?
  (lambda (object)
    (or (boolean? object)
	(eq? 'CONFIRM object)))
  (lambda (options require-match?)
    (if require-match?
	(set-options/mode! options
			   (ref-mode-object minibuffer-local-must-match)))
    (set-options/require-match?! options require-match?)))

(define (options/confirm-completion? options)
  (eq? 'CONFIRM (options/require-match? options)))

;;;; Prompt History Mechanism

(define *history-items*)
(define *history-index*)

(define (with-history-state initial-string thunk)
  (let ((history (name->history (options/history *options*))))
    (fluid-let ((*history-items*
		 (cons initial-string (list-copy (cdr history))))
		(*history-index* (+ 1 (options/history-index *options*))))
      (if (< *history-index* 0)
	  (set! *history-index* 0)
	  (let ((hl (length *history-items*)))
	    (if (>= *history-index* hl)
		(set! *history-index* (- hl 1)))))
      (let ((string (thunk)))
	(if (not (and (pair? (cdr history))
		      (string=? string (car (cdr history)))))
	    (set-cdr! history (cons string (cdr history))))
	string))))

(define prompt-histories)

(define (name->history name)
  (if (not (or (not name) (symbol? name)))
      (error:wrong-type-argument name "symbol" 'NAME->HISTORY))
  (let ((name (or name 'MINIBUFFER-DEFAULT)))
    (or (hash-table-ref/default prompt-histories name #f)
	(let ((history (list 'PROMPT-HISTORY)))
	  (hash-table-set! prompt-histories name history)
	  history))))

(define (prompt-history-strings name)
  (list-copy (cdr (name->history name))))

(define (set-prompt-history-strings! name strings)
  (if (not (list-of-strings? strings))
      (error:wrong-type-argument strings "list of strings"
				 'SET-PROMPT-HISTORY-STRINGS!))
  (set-cdr! (name->history name) (list-copy strings)))

(define-simple-option 'HISTORY symbol?)

(define-prompt-option 'HISTORY-INDEX exact-nonnegative-integer?
  (lambda (options index)
    (set-options/history-index! options index)
    (history->default-string options)))

(define (history->default-string options)
  (let ((history (name->history (options/history options)))
	(index (options/history-index options)))
    (if (and (not (options/default-string options))
	     (not (memq 'DEFAULT-STRING (options/seen options)))
	     (let ((length (length (cdr history))))
	       (and (> length 0)
		    (< index length))))
	(set-options/default-string! options (list-ref (cdr history) index)))))

(define (prompt-options-default-string options)
  (or (lookup-prompt-option options 'DEFAULT-STRING #f)
      (let ((index (lookup-prompt-option options 'HISTORY-INDEX #f)))
	(and index
	     (<= 0 index)
	     (let ((strings
		    (cdr
		     (name->history
		      (lookup-prompt-option options 'HISTORY #f)))))
	       (and (< index (length strings))
		    (list-ref strings index)))))))

;;;; String Prompt Modes

(define-major-mode minibuffer-local fundamental #f
  "Major mode for editing solicited input strings.
The following commands are special to this mode:

\\[exit-minibuffer] terminates the input.
\\[minibuffer-yank-default] yanks the default string, if there is one.
\\[next-prompt-history-item] moves to the next item in the history.
\\[previous-prompt-history-item] moves to the previous item in the history.")

(define-key 'minibuffer-local #\return 'exit-minibuffer)
(define-key 'minibuffer-local #\linefeed 'exit-minibuffer)
(define-key 'minibuffer-local #\c-m-y 'minibuffer-yank-default)
(define-key 'minibuffer-local #\M-n 'next-prompt-history-item)
(define-key 'minibuffer-local #\M-p 'previous-prompt-history-item)

(define-major-mode minibuffer-local-completion minibuffer-local #f
  (description-append (mode-description (ref-mode-object minibuffer-local))
		      "
\\[minibuffer-complete] completes as much of the input as possible.
\\[minibuffer-complete-word] completes the next word of the input.
\\[minibuffer-completion-help] displays possible completions of the input."))

(define-key 'minibuffer-local-completion #\tab 'minibuffer-complete)
(define-key 'minibuffer-local-completion #\space 'minibuffer-complete-word)
(define-key 'minibuffer-local-completion #\? 'minibuffer-completion-help)

(define-major-mode minibuffer-local-must-match minibuffer-local-completion #f
  (mode-description (ref-mode-object minibuffer-local-completion)))

(define-key 'minibuffer-local-must-match #\return
  'minibuffer-complete-and-exit)
(define-key 'minibuffer-local-must-match #\linefeed
  'minibuffer-complete-and-exit)

(define-command exit-minibuffer
  "Terminate this minibuffer argument."
  ()
  (lambda ()
    (cond ((not (string-null? (typein-string)))
	   (call-with-current-continuation
	    (lambda (k)
	      ;; Run the final value verification, just to catch any
	      ;; errors that it might generate.
	      (verify-final-value k)
	      (exit-typein-edit))))
	  ((memq (default-type) '(NULL-DEFAULT INSERTED-DEFAULT))
	   (exit-typein-edit))
	  ((or (not (default-string))
	       (eq? (default-type) 'NO-DEFAULT))
	   (editor-failure))
	  (else
	   (if (and (memq (default-type) '(INVISIBLE-DEFAULT VISIBLE-DEFAULT))
		    (default-string))
	       (set-typein-string! (default-string) #f))
	   (exit-typein-edit)))))

(define-command minibuffer-yank-default
  "Insert the default string at point."
  ()
  (lambda ()
    (if (default-string)
	(insert-string (default-string))
	(editor-failure))))

(define-command minibuffer-complete
  "Complete the minibuffer contents as far as possible."
  ()
  (lambda ()
    (case (complete-input-string (options/complete-string *options*) #t)
      ((WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION)
       (temporary-typein-message " [Sole completion]"))
      ((WAS-ALREADY-EXACT-COMPLETION)
       (temporary-typein-message " [Complete, but not unique]")))))

(define-command minibuffer-complete-word
  "Complete the minibuffer contents at most a single word."
  ()
  (lambda ()
    (case (complete-input-string completion-procedure/complete-word #t)
      ((WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION)
       (temporary-typein-message " [Sole completion]"))
      ((WAS-ALREADY-EXACT-COMPLETION)
       (temporary-typein-message " [Complete, but not unique]")))))

(define-command minibuffer-completion-help
  "Display a list of possible completions of the current minibuffer contents."
  ()
  (lambda ()
    (minibuffer-completion-help
     (lambda ()
       (with-messages-suppressed
	(lambda ()
	  ((options/list-completions *options*) (typein-string))))))))

(define (minibuffer-completion-help list-completions)
  (pop-up-generated-completions
   (lambda ()
     (map map-name/internal->external (list-completions)))))

(define-command minibuffer-complete-and-exit
  "Complete the minibuffer contents, and maybe exit.
Exit if the name is valid with no completion needed.
If name was completed to a valid match,
a repetition of this command will exit."
  ()
  (lambda ()
    (let ((string (typein-string)))
      (if (and (string-null? string)
	       (memq (default-type) '(INVISIBLE-DEFAULT VISIBLE-DEFAULT))
	       (default-string))
	  (set-typein-string! (default-string) #f)))
    (call-with-current-continuation
     (lambda (k)
       (if (verify-final-value k)
	   (exit-typein-edit)
	   (case (complete-input-string (options/complete-string *options*)
					#f)
	     ((WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION
	       WAS-ALREADY-EXACT-COMPLETION)
	      (exit-typein-edit))
	     ((COMPLETED-TO-EXACT-AND-UNIQUE-COMPLETION
	       COMPLETED-TO-EXACT-COMPLETION)
	      (if (options/confirm-completion? *options*)
		  (temporary-typein-message " [Confirm]")
		  (exit-typein-edit)))
	     (else
	      (update-typein!)
	      (editor-failure))))))))

(define (verify-final-value error-continuation)
  (let ((verifier (options/verify-final-value *options*)))
    (if verifier
	(bind-condition-handler (list condition-type:error)
	    (lambda (condition)
	      condition
	      (editor-failure)
	      (temporary-typein-message " [Error]")
	      (error-continuation unspecific))
	  (lambda () (verifier (typein-string))))
	#t)))

;;;; Completion Primitives

(define (complete-input-string complete-string update?)
  (call-with-current-continuation
   (lambda (k)
     (let ((original (typein-string))
	   (effected? #f))
       (let ((finish
	      (lambda (string not-completed completed list-completions)
		(let ((verified?
		       ((options/verify-final-value *options*) string)))
		  (set! effected? #t)
		  (if (not (string=? string original))
		      (set-typein-string! string update?))
		  (if verified?
		      (if (if (case-insensitive-completion?)
			      (string-ci=? string original)
			      (string=? string original))
			  not-completed
			  completed)
		      (if (if (case-insensitive-completion?)
			      (string-ci=? string original)
			      (string=? string original))
			  (begin
			    (if list-completions
				(if (ref-variable completion-auto-help)
				    (minibuffer-completion-help
				     list-completions)
				    (temporary-typein-message
				     " [Next char not unique]")))
			    'NO-COMPLETION-HAPPENED)
			  'SOME-COMPLETION-HAPPENED))))))
	 (bind-condition-handler (list condition-type:error)
	     (lambda (condition)
	       condition
	       (if (not effected?)
		   (begin
		     (editor-failure)
		     (temporary-typein-message " [Error]")
		     (k 'NO-MATCH))))
	   (lambda ()
	     (with-messages-suppressed
	      (lambda ()
		(complete-string original
		  (lambda (string)
		    (finish string
			    'WAS-ALREADY-EXACT-AND-UNIQUE-COMPLETION
			    'COMPLETED-TO-EXACT-AND-UNIQUE-COMPLETION
			    #f))
		  (lambda (string list-completions)
		    (finish string
			    'WAS-ALREADY-EXACT-COMPLETION
			    'COMPLETED-TO-EXACT-COMPLETION
			    list-completions))
		  (lambda ()
		    (set! effected? #t)
		    (editor-beep)
		    (temporary-typein-message " [No match]")
		    'NO-MATCH)))))))))))

(define (completion-procedure/complete-word string
					    if-unique
					    if-not-unique
					    if-not-found)
  (let ((truncate-string
	 (lambda (new-string)
	   (let ((end (string-length new-string)))
	     (let ((index
		    (and (if (case-insensitive-completion?)
			     (string-prefix-ci? string new-string)
			     (string-prefix? string new-string))
			 (substring-find-next-char-not-of-syntax
			  new-string (string-length string) end
			  (ref-variable syntax-table) #\w))))
	       (if index
		   (substring new-string 0 (1+ index))
		   new-string))))))
    (let ((if-unique
	   (lambda (new-string)
	     (if-unique (truncate-string new-string))))
	  (if-not-unique
	   (lambda (new-string list-completions)
	     (if-not-unique (truncate-string new-string) list-completions))))
      ((options/complete-string *options*) string
	if-unique
	(lambda (new-string list-completions)
	  (if (= (string-length new-string) (string-length string))
	      (let ((completions (list-completions)))
		(let ((try-suffix
		       (lambda (suffix if-not-found)
			 (let ((completions
				(filter
				 (let ((prefix (string-append string suffix)))
				   (if (case-insensitive-completion?)
				       (lambda (completion)
					 (string-prefix-ci? prefix
							    completion))
				       (lambda (completion)
					 (string-prefix? prefix
							 completion))))
				 completions)))
			   (cond ((null? completions)
				  (if-not-found))
				 ((null? (cdr completions))
				  (if-unique (car completions)))
				 (else
				  (if-not-unique
				   ((if (case-insensitive-completion?)
					string-greatest-common-prefix-ci
					string-greatest-common-prefix)
				    completions)
				   (lambda () completions))))))))
		  (try-suffix "-"
		    (lambda ()
		      (try-suffix " "
			(lambda ()
			  (if-not-unique string (lambda () completions))))))))
	      (if-not-unique new-string list-completions)))
	if-not-found))))

;;;; Support for Completion

(define (standard-completion prefix complete-string insert-completed-string)
  (let ((insert-completed-string
	 (lambda (completion)
	   (insert-completed-string completion)
	   (completion-message ""))))
    (complete-string prefix
      (lambda (completion)
	(if (not (string=? prefix completion))
	    (insert-completed-string completion)
	    (completion-message "Sole completion"))
	(flush-completions-list))
      (lambda (completion generate-completions)
	(cond ((not (string=? prefix completion))
	       (insert-completed-string completion)
	       (flush-completions-list))
	      ((ref-variable completion-auto-help)
	       (pop-up-generated-completions generate-completions))
	      (else
	       (completion-message "Next char not unique"))))
      (lambda ()
	(editor-beep)
	(completion-message "No completions")
	(flush-completions-list)))))

(define (pop-up-generated-completions generate-completions)
  (message "Making completion list...")
  (call-with-current-continuation
   (lambda (k)
     (let ((completions
	    (bind-condition-handler (list condition-type:error)
		(lambda (condition)
		  condition
		  (clear-message)
		  (editor-beep)
		  (completion-message "Error")
		  (k unspecific))
	      generate-completions)))
       (clear-message)
       (if (null? completions)
	   (begin
	     (editor-beep)
	     (completion-message "No completions"))
	   (pop-up-completions-list completions))))))

(define (pop-up-completions-list strings)
  (with-output-to-temporary-buffer " *Completions*"
    '(SHRINK-WINDOW FLUSH-ON-SPACE)
    (lambda ()
      (write-completions-list strings))))

(define (write-completions-list strings)
  (if (null? strings)
      (write-string
       "There are no possible completions of what you have typed.")
      (begin
	(write-string "Possible completions are:\n")
	(write-strings-densely strings))))

(define (flush-completions-list)
  (maybe-kill-pop-up-buffer (find-buffer " *Completions*")))

(define (completion-message string)
  (if (typein-window? (current-window))
      (if (not (string-null? string))
	  (temporary-typein-message (string-append " [" string "]")))
      (message string)))

(define (temporary-typein-message string)
  (let ((point) (start) (end))
    (dynamic-wind (lambda ()
		    (set! point (current-point))
		    (set! end (buffer-end (current-buffer)))
		    (set! start (mark-right-inserting end))
		    unspecific)
		  (lambda ()
		    (insert-string string start)
		    (set-current-point! start)
		    (sit-for 2000))
		  (lambda ()
		    (delete-string start end)
		    (set-current-point! point)))))

;;;; Character Prompts

(define (prompt-for-char prompt)
  (let ((input
	 (prompt-for-typein (string-append prompt ": ") #f
	   (lambda ()
	     (let ((input (with-editor-interrupts-disabled keyboard-read)))
	       (if (and (char? input) (char-ascii? input))
		   (set-typein-string! (key-name input) #t))
	       (if (input-event? input)
		   (abort-typein-edit input)
		   input))))))
    (if (not (and (char? input) (char-ascii? input)))
	(editor-error "Not an ASCII character:" input))
    input))

(define* (prompt-for-key prompt (comtab (current-comtabs)))
  (prompt-for-typein (string-append prompt ": ") #f
      (lambda ()
	(let outer-loop ((prefix '()))
	  (let inner-loop
	      ((char (with-editor-interrupts-disabled keyboard-read)))
	    (if (input-event? char)
		(abort-typein-edit char))
	    (let ((chars (append! prefix (list char))))
	      (set-typein-string! (xkey->name chars) #t)
	      (if (prefix-key-list? comtab chars)
		  (outer-loop chars)
		  (let ((command (comtab-entry comtab chars)))
		    (if (memq command extension-commands)
			(inner-loop
			 (fluid-let ((execute-extended-keys? #f))
			   (dispatch-on-command command)))
			chars)))))))))

;;;; Confirmation Prompts

(define (prompt-for-confirmation? prompt)
  (prompt-for-typein (if (string-suffix? " " prompt)
			 prompt
			 (string-append prompt " (y or n)? "))
		     #f
    (lambda ()
      (let loop ((lost? #f))
	(let ((char (keyboard-read)))
	  (cond ((and (char? char)
		      (or (char-ci=? char #\y)
			  (char-ci=? char #\space)))
		 (set-typein-string! "y" #t)
		 #t)
		((and (char? char)
		      (or (char-ci=? char #\n)
			  (char-ci=? char #\rubout)))
		 (set-typein-string! "n" #t)
		 #f)
		((input-event? char)
		 (abort-typein-edit char))
		(else
		 (editor-beep)
		 (if (not lost?)
		     (insert-string "Please answer y or n.  "
				    (buffer-absolute-start (current-buffer))))
		 (loop #t))))))))

(define (prompt-for-yes-or-no? prompt)
  (string-ci=?
   "Yes"
   (prompt-for-typein (string-append prompt " (yes or no)? ") #t
     (typein-editor-thunk (ref-mode-object minibuffer-local-yes-or-no)))))

(define-major-mode minibuffer-local-yes-or-no fundamental #f
  "Enter either \"yes\" or \"no\".")

(define-key 'minibuffer-local-yes-or-no #\return 'exit-minibuffer-yes-or-no)

(define-command exit-minibuffer-yes-or-no
  "Like \\[exit-minibuffer], but insists on \"yes\" or \"no\" as an answer."
  ()
  (lambda ()
    (let ((string (typein-string)))
      (if (or (string-ci=? "yes" string)
	      (string-ci=? "no" string))
	  (exit-typein-edit)
	  (begin
	    (editor-beep)
	    (message "Please answer yes or no.")
	    (sit-for 2000)
	    (clear-message)
	    (set-typein-string! "" #f))))))

;;;; Prompt History

(define-command next-prompt-history-item
  "Inserts the next item of the prompt history into the minibuffer.
The next item is the one more recent than the current item.
Has no effect if there is no history associated with this prompt.
With argument, skips forward that many items in the history."
  "p"
  (lambda (argument)
    (if (and (not (null? *history-items*))
	     (not (zero? argument)))
	(let* ((hl (length *history-items*))
	       (index
		(let ((index (- *history-index* argument)))
		  (cond ((< index 0) 0)
			((>= index hl) (- hl 1))
			(else index)))))
	  (set-car! (drop *history-items* *history-index*)
		    (typein-string))
	  (set! *history-index* index)
	  (set-typein-string! (list-ref *history-items* *history-index*) #t)
	  (set-current-point! (buffer-start (current-buffer)))))))

(define-command previous-prompt-history-item
  "Inserts the previous item of the prompt history into the minibuffer.
The previous item is the one less recent than the current item.
Has no effect if there is no history associated with this prompt.
With argument, skips backward that many items in the history."
  "p"
  (lambda (argument)
    ((ref-command next-prompt-history-item) (- argument))))

(define-command repeat-complex-command
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Scheme form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history."
  "p"
  (lambda (argument)
    ;; Kludge.
    (set-prompt-history-strings!
     'REPEAT-COMPLEX-COMMAND
     (map (lambda (command)
	    (fluid-let ((*unparse-with-maximum-readability?* #t))
	      (write-to-string command)))
	  (command-history-list)))
    (execute-command-history-entry
     (read-from-string
      (prompt-for-string "Redo" #f
			 'DEFAULT-TYPE 'INSERTED-DEFAULT
			 'HISTORY 'REPEAT-COMPLEX-COMMAND
			 'HISTORY-INDEX (- argument 1))
      (->environment '(EDWIN))))))

;;;; Pass-phrase Prompts

;;; Hair to make sure pass phrases aren't left around in memory.

(define (call-with-pass-phrase prompt receiver)
  (let ((phrase)
	(phrase*))
    (dynamic-wind
     (lambda ()
       (set! phrase "")
       (set! phrase* #f)
       unspecific)
     (lambda ()
       (prompt-for-typein (if (string-suffix? " " prompt)
			      prompt
			      (string-append prompt ": "))
			  #f
	 (lambda ()
	   (let loop ()
	     (set-typein-string! (make-string (string-length phrase) #\.) #t)
	     (let ((input (keyboard-read #t)))
	       (cond ((input-event? input)
		      (abort-typein-edit input))
		     ((eqv? input #\return)
		      (receiver phrase))
		     (else
		      (cond ((eqv? input #\rubout)
			     (let ((length (string-length phrase)))
			       (if (fix:> length 0)
				   (let ((length (fix:- length 1)))
				     (set! phrase* phrase)
				     (set! phrase (string-head phrase length))
				     (string-fill! phrase* #\NUL)
				     (set! phrase* #f)))))
			    ((and (char? input) (char-ascii? input))
			     (set! phrase* phrase)
			     (set! phrase
				   (string-append phrase (string input)))
			     (string-fill! phrase* #\NUL)
			     (set! phrase* #f)))
		      (loop))))))))
     (lambda ()
       (string-fill! phrase #\NUL)
       (set! phrase)
       (if phrase* (string-fill! phrase* #\NUL))
       (set! phrase*)
       unspecific))))

(define (call-with-confirmed-pass-phrase receiver)
  (call-with-pass-phrase "Pass phrase"
    (lambda (p1)
      (call-with-pass-phrase "Verify pass phrase"
	(lambda (p2)
	  (if (not (string=? p1 p2))
	      (editor-error "Pass phrases do not match."))))
      (receiver p1))))