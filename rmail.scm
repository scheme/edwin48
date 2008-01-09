#| -*-Scheme-*-

$Id: rmail.scm,v 1.78 2007/01/05 21:19:24 cph Exp $

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

;;;; RMAIL Mail Reader

(declare (usual-integrations))

(define rmail-spool-directory
  #f)

(define-variable rmail-file-name
  ""
  "~/RMAIL"
  string?)

(define-variable rmail-last-file
  "Last file used by \\[rmail-output]."
  "~/xmail"
  string?)

(define-variable rmail-last-rmail-file
  "Last file used by \\[rmail-output-to-rmail-file]."
  "~/XMAIL"
  string?)

(define-variable rmail-inbox-list
  ""
  '()
  list-of-strings?)

(define-variable rmail-primary-inbox-list
  "List of files which are inboxes for user's primary mail file ~/RMAIL.
Empty list means the default, which is (\"~/mbox\" \"/usr/spool/mail/$USER\")
\(the second name varies depending on the operating system)."
  '()
  list-of-strings?)

(define-variable rmail-dont-reply-to-names
  "A regular expression specifying names to prune in replying to messages.
#f means don't reply to yourself."
  false
  string-or-false?)

(define-variable rmail-default-dont-reply-to-names
  "A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customisation file."
  "info-"
  string?)

(define-variable rmail-ignored-headers
  "Gubbish header fields one would rather not see."
  "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^received:\\|^[a-z-]*message-id:\\|^summary-line:\\|^errors-to:"
  string-or-false?)

(define-variable rmail-message-filter
  "If not #f, is a filter procedure for new headers in RMAIL.
Called with the start and end marks of the header as arguments."
  false
  (lambda (object) (or (not object) (procedure? object))))

(define-variable rmail-delete-after-output
  "True means automatically delete a message that is copied to a file."
  false
  boolean?)

(define-variable rmail-reply-with-re
  "True means prepend subject with Re: in replies."
  false
  boolean?)

(define-variable rmail-mode-hook
  "An event distributor that is invoked when entering RMAIL mode."
  (make-event-distributor))

(define-variable rmail-new-mail-hook
  "An event distributor that is invoked when RMAIL incorporates new mail."
  (make-event-distributor))

(define-major-mode rmail read-only "RMAIL"
  "Rmail Mode is used by \\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

.	Move point to front of this message (same as \\[beginning-of-buffer]).
SPC	Scroll to next screen of this message.
DEL	Scroll to previous screen of this message.
n	Move to Next non-deleted message.
p	Move to Previous non-deleted message.
M-n	Move to Next message whether deleted or not.
M-p	Move to Previous message whether deleted or not.
>	Move to the last message in Rmail file.
j	Jump to message specified by numeric position in file.
M-s	Search for string and show message it is found in.
d	Delete this message, move to next nondeleted.
C-d	Delete this message, move to previous nondeleted.
u	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
e	Expunge deleted messages.
s	Expunge and save the file.
q       Quit Rmail: expunge, save, then switch to another buffer.
C-x C-s Save without expunging.
g	Move new mail from system spool directory or mbox into this file.
m	Mail a message (same as \\[mail-other-window]).
c	Continue composing outgoing message started before.
r	Reply to this message.  Like m but initializes some fields.
f	Forward this message to another user.
o       Output this message to an Rmail file (append it).
C-o	Output this message to a Unix-format mail file (append it).
i	Input Rmail file.  Run Rmail on that file.
a	Add label to message.  It will be displayed in the mode line.
k	Kill label.  Remove a label from current message.
C-M-n   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with `a'.
C-M-p   Move to Previous message with specified label
C-M-h	Show headers buffer, with a one line summary of each message.
C-M-l	Like h only just messages with particular label(s) are summarized.
C-M-r   Like h only just messages with particular recipient(s) are summarized.
t	Toggle header, show Rmail header if unformatted or vice versa.
w	Edit the current message.  C-c C-c to return to Rmail."
  (lambda (buffer)
    (guarantee-rmail-variables-initialized)
    (define-variable-local-value! buffer
	(ref-variable-object mode-line-modified)
      "--- ")
    (define-variable-local-value! buffer (ref-variable-object version-control)
      'NEVER)
    (define-variable-local-value! buffer
	(ref-variable-object file-precious-flag)
      true)
    (define-variable-local-value! buffer
	(ref-variable-object require-final-newline)
      false)
    (define-variable-local-value! buffer
      (ref-variable-object translate-file-data-on-output)
      #f)
    (define-variable-local-value! buffer (ref-variable-object rmail-last-file)
      (ref-variable rmail-last-file buffer))
    (define-variable-local-value! buffer (ref-variable-object rmail-inbox-list)
      (let ((inboxes (parse-file-inboxes buffer)))
	(if (and (null? inboxes)
		 (pathname=? (buffer-pathname buffer)
			     (ref-variable rmail-file-name buffer)))
	    (ref-variable rmail-primary-inbox-list buffer)
	    inboxes)))
    (buffer-put! buffer 'REVERT-BUFFER-METHOD rmail-revert-buffer)
    (memoize-buffer buffer)
    (set-buffer-read-only! buffer)
    (disable-group-undo! (buffer-group buffer))
    (event-distributor/invoke! (ref-variable rmail-mode-hook buffer) buffer)))

(define-major-mode rmail-edit text "RMAIL Edit"
  "Major mode for editing the contents of an RMAIL message.
The editing commands are the same as in Text mode,
together with two commands to return to regular RMAIL:
  * \\[rmail-abort-edit] cancels the changes you have made and returns to RMAIL
  * \\[rmail-cease-edit] makes them permanent."
  (lambda (buffer)
    (enable-group-undo! (buffer-group buffer))))

(define (guarantee-rmail-variables-initialized)
  (if (not rmail-spool-directory)
      (set! rmail-spool-directory (os/rmail-spool-directory)))
  (if (not (ref-variable rmail-pop-procedure))
      (set-variable! rmail-pop-procedure (os/rmail-pop-procedure)))
  (if (null? (ref-variable rmail-primary-inbox-list))
      (set-variable! rmail-primary-inbox-list
		     (os/rmail-primary-inbox-list
		      (let ((server
			     (and (ref-variable rmail-pop-procedure)
				  (ref-variable rmail-primary-pop-server))))
			(cond (server
			       (list (string-append "pop:" server)))
			      (rmail-spool-directory
			       (list (string-append rmail-spool-directory
						    (current-user-name))))
			      (else '()))))))
  (if (not (ref-variable rmail-dont-reply-to-names))
      (set-variable!
       rmail-dont-reply-to-names
       (string-append
	(let ((rmail-default-dont-reply-to-names
	       (ref-variable rmail-default-dont-reply-to-names)))
	  (if rmail-default-dont-reply-to-names
	      (string-append rmail-default-dont-reply-to-names "\\|")
	      ""))
	(re-quote-string (current-user-name))
	"\\>")))
  (if (not umail-message-end-regexp)
      (set! umail-message-end-regexp
	    (string-append "\\(^"
			   umail-message-start-regexp
			   "\\|"
			   mmdf-message-start-regexp
			   "\\|"
			   babyl-header-start-regexp
			   "\\|^[\037]?"
			   babyl-message-start-regexp
			   "\\)")))
  unspecific)

(define (parse-file-inboxes buffer)
  (let ((start (buffer-start buffer))
	(end (buffer-end buffer)))
    (if (re-match-forward babyl-header-start-regexp start end false)
	(let ((end
	       (if (re-search-forward babyl-header-end-regexp start end false)
		   (re-match-start 0)
		   end)))
	  (let ((start (search-forward "\nMail:" start end true)))
	    (if start
		(parse-comma-list start (line-end start 0))
		'())))
	'())))

(define (parse-comma-list start end)
  (let loop ((start start))
    (let ((start (skip-chars-forward " " start end)))
      (let ((m (skip-chars-forward "^," start end)))
	(cons (extract-string start (skip-chars-backward " " m start))
	      (if (mark< m end)
		  (loop (mark1+ m))
		  '()))))))

(define-key 'rmail #\.		'beginning-of-buffer)
(define-key 'rmail #\space	'scroll-up)
(define-key 'rmail #\rubout	'scroll-down)
(define-key 'rmail #\n		'rmail-next-undeleted-message)
(define-key 'rmail #\p		'rmail-previous-undeleted-message)
(define-key 'rmail #\m-n	'rmail-next-message)
(define-key 'rmail #\m-p	'rmail-previous-message)
(define-key 'rmail #\c-m-n	'rmail-next-labeled-message)
(define-key 'rmail #\c-m-p	'rmail-previous-labeled-message)
(define-key 'rmail #\a		'rmail-add-label)
(define-key 'rmail #\k		'rmail-kill-label)
(define-key 'rmail #\d		'rmail-delete-forward)
(define-key 'rmail #\u		'rmail-undelete-previous-message)
(define-key 'rmail #\e		'rmail-expunge)
(define-key 'rmail #\x		'rmail-expunge)
(define-key 'rmail #\s		'rmail-expunge-and-save)
(define-key 'rmail #\g		'rmail-get-new-mail)
(define-key 'rmail #\h		'rmail-summary)
(define-key 'rmail #\c-m-h	'rmail-summary)
(define-key 'rmail #\l		'rmail-summary-by-labels)
(define-key 'rmail #\c-m-l	'rmail-summary-by-labels)
(define-key 'rmail #\c-m-r	'rmail-summary-by-recipients)
(define-key 'rmail #\t		'rmail-toggle-header)
(define-key 'rmail #\m		'rmail-mail)
(define-key 'rmail #\r		'rmail-reply)
(define-key 'rmail #\c		'rmail-continue)
(define-key 'rmail #\f		'rmail-forward)
(define-key 'rmail #\m-s	'rmail-search)
(define-key 'rmail #\j		'rmail-show-message)
(define-key 'rmail #\o		'rmail-output-to-rmail-file)
(define-key 'rmail #\c-o	'rmail-output)
(define-key 'rmail #\i		'rmail-input)
(define-key 'rmail #\q		'rmail-quit)
(define-key 'rmail #\>		'rmail-last-message)
(define-key 'rmail #\?		'describe-mode)
(define-key 'rmail #\w		'rmail-edit-current-message)
(define-key 'rmail #\c-d	'rmail-delete-backward)

(define-key 'rmail-edit '(#\c-c #\c-c) 'rmail-cease-edit)
(define-key 'rmail-edit '(#\c-c #\c-\]) 'rmail-abort-edit)

(define-command rmail
  "Read and edit incoming mail.
Moves messages into file named by  rmail-file-name  (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with filename as argument;
then performs rmail editing on that file,
but does not copy any new mail into the file."
  (lambda ()
    (list (and (command-argument)
	       (prompt-for-existing-file "Run rmail on RMAIL file" #f))))
  (lambda (filename)
    (rmail-find-file (or filename (ref-variable rmail-file-name)))
    (let ((mode (current-major-mode)))
      (cond ((eq? mode (ref-mode-object rmail-edit))
	     (editor-error "Exit rmail-edit mode before getting new mail"))
	    ((not (eq? mode (ref-mode-object rmail)))
	     (set-current-major-mode! (ref-mode-object rmail)))))
    ;; This guarantees that a message is selected.  This is desirable
    ;; because the process of getting mail may perform prompting, and
    ;; since this buffer is selected, it will appear to the user when
    ;; the prompting occurs.  By selecting a message, the buffer at
    ;; least appears as the user expects it to.
    (let ((buffer (current-buffer)))
      (show-message buffer
		    (let ((memo (buffer-msg-memo buffer)))
		      (if (msg-memo? memo)
			  (msg-memo/number memo)
			  0))))
    (if (not filename) ((ref-command rmail-get-new-mail) #f))))

(define-command rmail-input
  "Run RMAIL on file FILENAME."
  "FRun rmail on RMAIL file"
  (ref-command rmail))

(define (rmail-find-file filename)
  (fluid-let ((after-find-file rmail-after-find-file))
    (find-file filename)))

(define (rmail-find-file-revert buffer)
  (fluid-let ((after-find-file rmail-after-find-file))
    (find-file-revert buffer)))

(define (rmail-revert-buffer buffer dont-use-auto-save? dont-confirm?)
  (let ((n
	 (let ((memo (buffer-msg-memo buffer)))
	   (and (msg-memo? memo)
		(msg-memo/number memo)))))
    (fluid-let ((after-find-file rmail-after-find-file))
      (revert-buffer-default buffer dont-use-auto-save? dont-confirm?))
    (show-message buffer
		  (and n
		       (let ((memo (buffer-msg-memo buffer)))
			 (and (msg-memo? memo)
			      (<= n (msg-memo/number (msg-memo/last memo)))
			      n)))))
  buffer)

(define (rmail-after-find-file buffer error? warn?)
  error? warn?
  (disable-buffer-auto-save! buffer)	;No need to auto save RMAIL files.
  (convert-buffer-to-babyl-format buffer)
  (set-buffer-major-mode! buffer (ref-mode-object rmail))
  buffer)

(define-command rmail-quit
  "Quit out of RMAIL."
  ()
  (lambda ()
    ((ref-command rmail-expunge-and-save))
    ((ref-command bury-buffer))))

(define-command rmail-expunge-and-save
  "Expunge and save RMAIL file."
  ()
  (lambda ()
    ((ref-command rmail-expunge))
    ((ref-command save-buffer) false)))

;;;; Mail input

(define-command rmail-get-new-mail
  "Move any new mail from this RMAIL file's inbox files.
The inbox files can be specified with the file's Mail: option.
The variable rmail-primary-inbox-list specifies the inboxes for
your primary RMAIL file if it has no Mail: option.
These are normally your ~/mbox and your /usr/spool/mail/$USER.

You can also specify the file to get new mail from.  In this
case, the file of new mail is not changed or deleted.
Noninteractively, you can pass the inbox file name as an argument.
Interactively, a prefix argument causes us to read a file name
and use that file as the inbox."
  (lambda ()
    (list (and (command-argument)
	       (prompt-for-existing-file "Get new mail from file" #f))))
  (lambda (filename)
    (let ((buffer (rmail-find-file-revert (current-buffer))))
      (let ((n-messages
	     (let ((memo (buffer-msg-memo buffer)))
	       (if (msg-memo? memo)
		   (msg-memo/number (msg-memo/last memo))
		   0))))
	(with-buffer-open buffer
	  (lambda ()
	    (with-buffer-undo-disabled buffer
	      (lambda ()
		(if filename
		    (get-new-mail buffer (list filename) false)
		    (get-new-mail buffer
				  (ref-variable rmail-inbox-list)
				  true))))))
	(show-message
	 buffer
	 (let ((memo (buffer-msg-memo buffer)))
	   (cond ((not (msg-memo? memo)) 0)
		 ((> (msg-memo/number (msg-memo/last memo)) n-messages)
		  (+ n-messages 1))
		 (else (msg-memo/number memo)))))
	(event-distributor/invoke! (ref-variable rmail-new-mail-hook))))))

(define (get-new-mail buffer inbox-list delete-inboxes?)
  (let ((start (mark-right-inserting-copy (buffer-end buffer)))
	(end (mark-left-inserting-copy (buffer-end buffer)))
	(modified? (buffer-modified? buffer)))
    (delete-string (skip-chars-backward " \t\n" end) end)
    (let ((inserted-inboxes
	   (let loop ((filenames inbox-list) (result '()))
	     (if (null? filenames)
		 result
		 (loop (cdr filenames)
		       (let ((pathname
			      (insert-inbox-text buffer
						 end
						 (car filenames)
						 delete-inboxes?)))
			 (if pathname
			     (cons pathname result)
			     result)))))))
      (let ((new-messages (convert-region-to-babyl-format start end)))
	(if (> new-messages 0)
	    (begin
	      (memoize-messages buffer start end)
	      (save-buffer buffer
			   ;; If buffer has not changed yet, and has
			   ;; not been saved yet, don't replace the
			   ;; old backup file now.
			   (if (and (ref-variable make-backup-files buffer)
				    modified?)
			       false
			       'NO-BACKUP))))
	(if delete-inboxes?
	    (for-each delete-file-no-errors inserted-inboxes))
	(cond ((> new-messages 0)
	       (message new-messages
			" new message"
			(if (= new-messages 1) "" "s")
			" read"))
	      ((not (null? inbox-list))
	       (message "(No new mail has arrived)")))
	(mark-temporary! end)
	(mark-temporary! start)
	new-messages))))

(define (insert-inbox-text buffer mark inbox-name rename?)
  (let ((insert
	 (lambda (pathname)
	   (and (file-exists? pathname)
		(let ((mark (mark-left-inserting-copy mark)))
		  (insert-file mark pathname)
		  (if (let ((char (mark-left-char mark)))
			(and char
			     (not (char=? char #\newline))
			     (not (char=? char (integer->char #o037)))))
		      (insert-newline mark))
		  (mark-temporary! mark)
		  pathname)))))
    (cond ((string-prefix? "pop:" inbox-name)
	   (get-mail-from-pop-server (string-tail inbox-name 4)
				     insert
				     buffer))
	  ((not rename?)
	   (insert inbox-name))
	  ((string=? rmail-spool-directory (directory-namestring inbox-name))
	   (rename-inbox-using-movemail inbox-name
					insert
					(buffer-default-directory buffer)))
	  (else
	   (rename-inbox-using-rename inbox-name insert)))))

(define (rename-inbox-using-rename inbox-name insert)
  (let ((target (string-append inbox-name "+")))
    (let ((msg (string-append "Getting mail from " inbox-name "...")))
      (message msg)
      (if (and (file-exists? inbox-name) (not (file-exists? target)))
	  (rename-file inbox-name target))
      (let ((value (insert target)))
	(message msg "done")
	value))))

(define (rename-inbox-using-movemail inbox-name insert directory)
  (let ((source
	 ;; On some systems, /usr/spool/mail/foo is a directory and
	 ;; the actual inbox is /usr/spool/mail/foo/foo.
	 (if (file-directory? inbox-name)
	     (merge-pathnames (pathname-name inbox-name)
			      (pathname-as-directory inbox-name))
	     inbox-name))
	(target (merge-pathnames ".newmail" directory)))
    (let ((msg
	   (string-append "Getting mail from " (->namestring source) "...")))
      (message msg)
      (if (and (file-exists? source)
	       (not (file-exists? target)))
	  (let ((error-buffer (temporary-buffer " movemail errors")))
	    (let ((start (buffer-start error-buffer))
		  (end (buffer-end error-buffer)))
	      (run-synchronous-process
	       false start false false
	       (os/find-program "movemail"
				(edwin-etc-directory)
				(ref-variable exec-path))
	       (->namestring source)
	       (->namestring target))
	      (if (mark< start end)
		  (error
		   (let ((m
			  (or (match-forward "movemail: " start end false)
			      start)))
		     (string-append
		      "movemail: "
		      (extract-string
		       m
		       (skip-chars-backward " \t" (line-end m 0) m)))))))
	    (kill-buffer error-buffer)))
      (let ((value (insert target)))
	(message msg "done")
	value))))

;;;; POP Support

(define-variable rmail-pop-procedure
  "A procedure that will get mail from a POP server.
This procedure will be called with four arguments:
  1. The server's name.
  2. The user name on that server.
  3. The password for that user.
  4. The directory in which to temporarily store the mail.
The procedure must return the name of the file in which the mail is
stored.  If there is no mail, this file must exist but be empty.

A value of #F means there is no mechanism to get POP mail."
  #f
  (lambda (object) (or (not object) (procedure? object))))

(define-variable rmail-primary-pop-server
  "The host name of a POP server to use as a default, or #F.
If not #F, this server is used to initialize rmail-primary-inbox-list.
Otherwise, rmail-primary-inbox-list is initialized to the operating
system's mail inbox.

If this variable is set, it is useful to initialize the variable
rmail-pop-accounts with the corresponding account information.

This variable is ignored if rmail-pop-procedure is #F."
  #f
  string-or-false?)

(define-variable rmail-pop-accounts
  "A list of lists, each of which specifies a POP account.
Each element of the list is a list of three items:

  1. The POP server host name, a string.
  2. The user name to use with that server, a string.
  3. The password to use for that account.

Each server host name should appear only once; only the first entry
with that name is used.

The password field can take on several values.  A string is the
password to use.  The symbol 'PROMPT-ONCE means to prompt the first
time the password is needed, saving the password and reusing it
subsequently.  The symbol 'PROMPT-ALWAYS means to prompt each time
that the password is needed.  A list (FILE <filename>) means that the
password is in the file <filename>.

This variable is ignored if rmail-pop-procedure is #F."
  '()
  (lambda (object)
    (and (list? object)
	 (for-all? object
	   (lambda (object)
	     (and (list? object)
		  (= 3 (length object))
		  (string? (car object))
		  (string? (cadr object))
		  (let ((password (caddr object)))
		    (or (string? password)
			(symbol? password)
			(and (pair? password)
			     (eq? 'FILE (car password))
			     (pair? (cdr password))
			     (or (string? (cadr password))
				 (pathname? (cadr password)))
			     (null? (cddr password)))))))))))

(define (get-mail-from-pop-server server insert buffer)
  (let ((procedure (ref-variable rmail-pop-procedure buffer)))
    (and procedure
	 (call-with-values (lambda () (get-pop-account-info server buffer))
	   (lambda (user-name password save-password?)
	     (let ((msg
		    (string-append "Getting mail from POP server "
				   server
				   "...")))
	       (message msg)
	       (let ((value
		      (insert
		       (let ((filename
			      (procedure server user-name password
					 (buffer-default-directory buffer))))
			 (if save-password?
			     ;; Password is saved only after
			     ;; successful execution of the client, to
			     ;; prevent saving an incorrect password.
			     (save-pop-server-password server
						       user-name
						       password))
			 filename))))
		 (message msg "done")
		 value)))))))

(define (get-pop-account-info server buffer)
  (let ((entry (assoc server (ref-variable rmail-pop-accounts buffer))))
    (if entry
	(let ((user-name (cadr entry))
	      (password (caddr entry)))
	  (cond ((eq? 'PROMPT-ONCE password)
		 (let ((password
			(get-saved-pop-server-password server user-name)))
		   (if password
		       (values user-name password #f)
		       (values user-name
			       (prompt-for-pop-server-password server)
			       #t))))
		((eq? 'PROMPT-ALWAYS password)
		 (values user-name (prompt-for-pop-server-password server) #f))
		((or (string? password) (symbol? password))
		 (values user-name password #f))
		((and (pair? password) (eq? 'FILE (car password)))
		 (values user-name
			 (list 'FILE
			       (->namestring
				(merge-pathnames (cadr password)
						 (user-homedir-pathname))))
			 #f))
		(else
		 (error "Illegal password value in rmail-pop-accounts entry:"
			password))))
	(let ((user-name
	       (prompt-for-string
		(string-append "User name for POP server " server)
		(current-user-name))))
	  (values user-name (prompt-for-pop-server-password server) #f)))))

(define (get-saved-pop-server-password server user-name)
  (let ((entry (assoc (cons server user-name) saved-pop-passwords)))
    (and entry
	 (cdr entry))))

(define (save-pop-server-password server user-name password)
  (set! saved-pop-passwords
	(cons (cons (cons server user-name) password)
	      saved-pop-passwords))
  unspecific)

(define (delete-saved-pop-server-password server user-name)
  (set! saved-pop-passwords
	(del-assoc! (cons server user-name) saved-pop-passwords))
  unspecific)

(define saved-pop-passwords '())

(define (prompt-for-pop-server-password server)
  (call-with-pass-phrase (string-append "Password for POP server " server)
			 string-copy))

;;;; Moving around

(define-command rmail-next-message
  "Show following message whether deleted or not.
With prefix argument N, moves forward N messages,
or backward if N is negative."
  "p"
  (lambda (n) (move-to-message n (lambda (memo) memo #t) "message")))

(define-command rmail-previous-message
  "Show previous message whether deleted or not.
With prefix argument N, moves backward N messages,
or forward if N is negative."
  "p"
  (lambda (n) ((ref-command rmail-next-message) (- n))))

(define-command rmail-next-undeleted-message
  "Show following non-deleted message.
With prefix argument N, moves forward N non-deleted messages,
or backward if N is negative."
  "p"
  (lambda (n)
    (move-to-message n
		     (lambda (memo) (not (msg-memo/deleted? memo)))
		     "undeleted message")))

(define-command rmail-previous-undeleted-message
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages,
or forward if N is negative."
  "p"
  (lambda (n) ((ref-command rmail-next-undeleted-message) (- n))))

(define (move-to-message n predicate noun)
  (if (not (= n 0))
      (call-with-values
	  (lambda ()
	    (if (< n 0)
		(values (- n) msg-memo/previous "previous")
		(values n msg-memo/next "next")))
	(lambda (n step direction)
	  (let loop ((n n) (memo (current-msg-memo)) (winner #f))
	    (let ((next
		   (let loop ((memo memo))
		     (let ((next (step memo)))
		       (if (or (not next) (predicate next))
			   next
			   (loop next))))))
	      (cond ((not next)
		     (if winner (set-current-msg-memo! winner))
		     (message "No " direction " " noun))
		    ((= n 1)
		     (set-current-msg-memo! next))
		    (else
		     (loop (- n 1) next next)))))))))

(define-command rmail-next-labeled-message
  "Show next message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves forward N messages with these labels."
  "p\nsMove to next msg with labels"
  (lambda (n labels)
    (let ((labels (check-multi-labels labels)))
      (move-to-message n
		       (multi-labels-predicate labels)
		       (string-append "message with labels " labels)))))

(define-command rmail-previous-labeled-message
  "Show previous message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves backward N messages with these labels."
  "p\nsMove to previous msg with labels"
  (lambda (n labels) ((ref-command rmail-next-labeled-message) (- n) labels)))

(define (check-multi-labels labels)
  (let ((labels (if (string-null? labels) rmail-last-multi-labels labels)))
    (if (not labels)
	(editor-error "No labels to find have been specified previously"))
    (set! rmail-last-multi-labels labels)
    labels))

(define rmail-last-multi-labels #f)

(define (multi-labels-predicate labels)
  (let ((regexp
	 (string-append " ?\\(" (multi-labels->regexp labels) "\\),")))
    (lambda (memo)
      (let ((start (msg-memo/start memo)))
	(with-group-open (mark-group start)
	  (lambda ()
	    (let ((start (attributes-start-mark start)))
	      (re-search-forward regexp start (line-end start 0) #t))))))))

(define (multi-labels->regexp labels)
  (apply string-append
	 (let ((labels (map string-trim (burst-string labels #\,))))
	   (cons (car labels)
		 (append-map (lambda (label) (list "\\|" label))
			     (cdr labels))))))

(define (burst-string string delimiter)
  (let ((end (string-length string)))
    (let loop ((start 0) (result '()))
      (let ((index (substring-find-next-char string start end delimiter)))
	(if index
	    (loop (fix:+ index 1)
		  (cons (substring string start index) result))
	    (reverse! (cons (substring string start end) result)))))))

(define-command rmail-show-message
  "Show message number N (prefix argument), counting from start of file."
  "p"
  (lambda (n)
    (show-message (current-buffer) n)))

(define-command rmail-last-message
  "Show last message in file."
  ()
  (lambda ()
    (set-current-msg-memo! (last-msg-memo))))

(define-command rmail-search
  "Show message containing next match for REGEXP.
Search in reverse (earlier messages) with 2nd arg REVERSEP true.
Interactively, empty argument means use same regexp used last time,
and reverse search is specified by a negative numeric arg."
  (lambda ()
    (let ((reverse? (< (command-argument-numeric-value (command-argument)) 0)))
      (let ((regexp
	     (prompt-for-string (string-append (if reverse? "Reverse " "")
					       "Rmail search (regexp)")
				search-last-regexp)))
	(set! search-last-regexp regexp)
	(list regexp reverse?))))
  (lambda (regexp reverse?)
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo))
	  (msg
	   (string-append (if reverse? "Reverse " "")
			  "Rmail search for "
			  regexp
			  "...")))
      (message msg)
      (with-values
	  (lambda ()
	    (without-clipping buffer
	      (lambda ()
		(if reverse?
		    (let loop ((memo memo))
		      (let ((memo (msg-memo/previous memo)))
			(cond ((not memo)
			       (values false false))
			      ((re-search-backward regexp
						   (msg-memo/end-body memo)
						   (msg-memo/start-body memo))
			       =>
			       (lambda (mark) (values memo mark)))
			      (else
			       (loop memo)))))
		    (let loop ((memo memo))
		      (let ((memo (msg-memo/next memo)))
			(cond ((not memo)
			       (values false false))
			      ((re-search-forward regexp
						  (msg-memo/start-body memo)
						  (msg-memo/end-body memo))
			       =>
			       (lambda (mark) (values memo mark)))
			      (else
			       (loop memo)))))))))
	(lambda (memo mark)
	  (if memo
	      (let ((mark (mark-left-inserting-copy mark)))
		(select-message buffer memo)
		(set-current-point! mark)
		(mark-temporary! mark)
		(message msg "done"))
	      (editor-failure "Search failed: " regexp)))))))

(define search-last-regexp
  false)

(define (show-message buffer n)
  (if (not (eq? (buffer-major-mode buffer) (ref-mode-object rmail)))
      (error "Can't change buffer message -- not in Rmail mode"))
  (let ((memo (buffer-msg-memo buffer)))
    (if (not (msg-memo? memo))
	(begin
	  (let ((start (buffer-start buffer)))
	    (let ((m
		   (re-search-backward babyl-header-end-regexp
				       (buffer-end buffer)
				       start
				       false)))
	      (if m
		  (narrow-to-region start (mark1+ m))))
	    (set-buffer-point! buffer start))
	  (if (current-buffer? buffer)
	      (begin
		(update-mode-line! buffer)
		(message "No messages"))))
	(let ((last (msg-memo/last memo)))
	  (cond ((not n)
		 (select-message buffer last))
		((<= 1 n (msg-memo/number last))
		 (select-message buffer (msg-memo/nth memo n)))
		((current-buffer? buffer)
		 (message "No such message")))))))

(define (current-msg-memo)
  (let ((memo (buffer-msg-memo (current-buffer))))
    (if (not (msg-memo? memo))
	(editor-error "No messages"))
    memo))

(define (last-msg-memo)
  (msg-memo/last (current-msg-memo)))

(define (set-current-msg-memo! memo)
  (select-message (mark-buffer (msg-memo/start memo)) memo))

(define (select-message buffer memo)
  (let ((start (msg-memo/start memo)))
    (set-buffer-msg-memo! buffer memo)
    (widen start)
    (let ((end (msg-memo/end memo)))
      (if (match-forward "\f\n0" start end false)
	  (with-read-only-defeated start
	    (lambda ()
	      (reformat-message start end))))
      (clear-attribute! memo 'UNSEEN)
      (update-mode-line! buffer)
      (let ((start (re-search-forward babyl-eooh-regexp start end false)))
	(narrow-to-region start (mark-1+ end))
	(set-buffer-point! buffer start))
	(set-buffer-mark! buffer (mark-1+ end)))))

(define (update-mode-line! buffer)
  (define-variable-local-value! buffer (ref-variable-object mode-line-process)
    (mode-line-summary-string buffer))
  (buffer-modeline-event! buffer 'PROCESS-STATUS))

(define (mode-line-summary-string buffer)
  (let ((memo (buffer-msg-memo buffer)))
    (and (msg-memo? memo)
	 (apply string-append
		" "
		(number->string (msg-memo/number memo))
		"/"
		(number->string (msg-memo/number (msg-memo/last memo)))
		(append-map!
		 (lambda (label) (list "," label))
		 (append! (map symbol-name (msg-memo/attributes memo))
			  (parse-labels (msg-memo/start memo))))))))

;;;; Message deletion

(define-command rmail-delete-message
  "Delete this message and stay on it."
  ()
  (lambda () (set-attribute! (current-msg-memo) 'DELETED)))

(define-command rmail-undelete-previous-message
  "Back up to deleted message, select it, and undelete it."
  ()
  (lambda ()
    (let ((memo (current-msg-memo)))
      (if (msg-memo/deleted? memo)
	  (clear-attribute! memo 'DELETED)
	  (let ((memo (msg-memo/previous-deleted memo)))
	    (if (not memo) (editor-error "No previous deleted message"))
	    (clear-attribute! memo 'DELETED)
	    (set-current-msg-memo! memo))))))

(define-command rmail-delete-forward
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward."
  "P"
  (lambda (backward?)
    (set-attribute! (current-msg-memo) 'DELETED)
    ((ref-command rmail-next-undeleted-message) (if backward? -1 1))))

(define-command rmail-delete-backward
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  ()
  (lambda () ((ref-command rmail-delete-forward) true)))

(define-command rmail-expunge
  "Actually erase all deleted messages in the file."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((memo (buffer-msg-memo buffer)))
	(if (msg-memo? memo)
	    (show-message
	     buffer
	     (with-buffer-open buffer (lambda () (expunge buffer memo)))))))))

(define (expunge buffer current-memo)
  (let ((new-memo
	 (if (not (msg-memo/deleted? current-memo))
	     current-memo
	     (or (msg-memo/next-undeleted current-memo)
		 (msg-memo/previous-undeleted current-memo)))))
    (let loop ((memo (msg-memo/first current-memo)) (n 1))
      (let ((next (msg-memo/next memo)))
	(cond ((not (msg-memo/deleted? memo))
	       (set-msg-memo/number! memo n)
	       (if next (loop next (+ n 1))))
	      (next
	       (let ((start (msg-memo/start memo)))
		 (delete-string start (msg-memo/start next))
		 (mark-temporary! start))
	       (let ((previous (msg-memo/previous memo)))
		 (if previous (set-msg-memo/next! previous next))
		 (set-msg-memo/previous! next previous))
	       (loop next n))
	      (else
	       (let ((start (msg-memo/start memo))
		     (end (buffer-last-msg-end buffer)))
		 (set-buffer-last-msg-end! buffer start)
		 (delete-string start end)
		 (mark-temporary! end))
	       (let ((previous (msg-memo/previous memo)))
		 (if previous (set-msg-memo/next! previous false)))))))
    (if new-memo
	(begin
	  (set-buffer-msg-memo! buffer new-memo)
	  (msg-memo/number new-memo))
	(begin
	  (set-buffer-msg-memo! buffer true)
	  false))))

;;;; Mailing commands

(define-command rmail-mail
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  ()
  (lambda ()
    (make-mail-buffer '(("To" "") ("Subject" ""))
		      (current-buffer)
		      select-buffer-other-window)))

(define-command rmail-continue
  "Continue composing outgoing message previously being composed."
  ()
  (lambda ()
    ((ref-command mail-other-window) true)))

(define-command rmail-forward
  "Forward the current message to another user."
  ()
  (lambda ()
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo)))
      (set-attribute! memo 'FORWARDED)
      (make-mail-buffer
       (without-clipping buffer
	 (lambda ()
	   (with-values (lambda () (original-header-limits memo))
	     (lambda (start end)
	       `(("To" "")
		 ("Subject"
		  ,(string-append
		    "["
		    (let ((from (fetch-first-field "from" start end)))
		      (if from
			  (rfc822:canonicalize-address-string from)
			  ""))
		    ": "
		    (or (fetch-first-field "subject" start end) "")
		    "]")))))))
       #f
       (if (window-has-no-neighbors? (current-window))
	   select-buffer
	   select-buffer-other-window))
      (insert-region (buffer-start buffer)
		     (buffer-end buffer)
		     (buffer-end (current-buffer))))))

(define-command rmail-reply
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
  "P"
  (lambda (just-sender?)
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo)))
      (set-attribute! memo 'ANSWERED)
      (make-mail-buffer (without-clipping buffer
			  (lambda ()
			    (rfc822-region-reply-headers
			     (call-with-values
				 (lambda () (original-header-limits memo))
			       make-region)
			     (not just-sender?))))
			buffer
			select-buffer-other-window))))

(define (rfc822-region-reply-headers region cc?)
  (let ((start (region-start region))
	(end (region-end region)))
    (let ((resent-reply-to (fetch-last-field "resent-reply-to" start end))
	  (from (fetch-first-field "from" start end)))
      `(("To"
	 ,(rfc822:canonicalize-address-string
	   (or resent-reply-to
	       (fetch-all-fields "reply-to" start end)
	       from)))
	("CC"
	 ,(and cc?
	       (let ((to
		      (if resent-reply-to
			  (fetch-last-field "resent-to" start end)
			  (fetch-all-fields "to" start end)))
		     (cc
		      (if resent-reply-to
			  (fetch-last-field "resent-cc" start end)
			  (fetch-all-fields "cc" start end))))
		 (let ((cc
			(if (and to cc)
			    (string-append to ", " cc)
			    (or to cc))))
		   (and cc
			(let ((addresses
			       (dont-reply-to (rfc822:string->addresses cc))))
			  (and (pair? addresses)
			       (rfc822:addresses->string addresses))))))))
	("In-reply-to"
	 ,(if resent-reply-to
	      (make-in-reply-to-field
	       from
	       (fetch-last-field "resent-date" start end)
	       (fetch-last-field "resent-message-id" start end))
	      (make-in-reply-to-field
	       from
	       (fetch-first-field "date" start end)
	       (fetch-first-field "message-id" start end))))
	("Subject"
	 ,(let ((subject
		 (or (and resent-reply-to
			  (fetch-last-field "resent-subject"
					    start end))
		     (fetch-first-field "subject" start end))))
	    (cond ((not subject) "")
		  ((ref-variable rmail-reply-with-re)
		   (if (string-prefix-ci? "re:" subject)
		       subject
		       (string-append "Re: " subject)))
		  (else
		   (do ((subject
			 subject
			 (string-trim-left (string-tail subject 3))))
		       ((not (string-prefix-ci? "re:" subject))
			subject))))))))))

(define (original-header-limits memo)
  (let ((start (msg-memo/start memo))
	(end (msg-memo/end memo)))
    (if (match-forward "\f\n0" start end false)
	(begin
	  (if (not (re-search-forward babyl-eooh-regexp start end false))
	      (editor-error))
	  (let ((hstart (re-match-end 0)))
	    (values hstart (header-end hstart end))))
	(values
	 (let ((start (line-start start 2 'ERROR)))
	   (if (match-forward "Summary-line:" start end true)
	       (line-start start 1 'ERROR)
	       start))
	 (begin
	   (if (not (re-search-forward babyl-eooh-regexp start end false))
	       (editor-error))
	   (re-match-start 0))))))

(define (fetch-first-field field start end)
  (let ((fs (re-search-forward (field-name->regexp field) start end true)))
    (and fs
	 (extract-field fs end))))

(define (fetch-last-field field start end)
  (and (re-search-backward (field-name->regexp field) end start true)
       (extract-field (re-match-end 0) end)))

(define (fetch-all-fields field start end)
  (let ((strings
	 (let ((regexp (field-name->regexp field)))
	   (let loop ((start start))
	     (let ((fs (re-search-forward regexp start end true)))
	       (if fs
		   (let ((string (extract-field fs end))
			 (strings (loop fs)))
		     (if string
			 (cons string
			       (if (null? strings)
				   '()
				   (cons ", " strings)))
			 strings))
		   '()))))))
    (and (not (null? strings))
	 (apply string-append strings))))

(define (extract-field fs end)
  (let ((fe
	 (skip-chars-backward " \t\n"
			      (if (re-search-forward "^[^ \t]" fs end false)
				  (re-match-start 0)
				  end)
			      fs)))
    (and (mark< fs fe)
	 (extract-string fs fe))))

(define (field-name->regexp field)
  (string-append "^" (re-quote-string field) "[ \t]*:[ \t]*"))

(define (header-end start end)
  (or (search-forward "\n\n" start end false) end))

(define (dont-reply-to addresses)
  (let ((pattern
	 (re-compile-pattern
	  (string-append "\\(.*!\\|\\)\\("
			 (ref-variable rmail-dont-reply-to-names)
			 "\\)")
	  true)))
    (let loop ((addresses addresses))
      (cond ((null? addresses)
	     '())
	    ((re-string-match pattern (car addresses))
	     (loop (cdr addresses)))
	    (else
	     (cons (car addresses) (loop (cdr addresses))))))))

(define (separated-append tokens separator)
  (if (null? (cdr tokens))
      (car tokens)
      (apply string-append
	     (let loop ((tokens tokens))
	       (if (null? (cdr tokens))
		   (list (car tokens))
		   (cons* (car tokens) separator (loop (cdr tokens))))))))

(define (make-in-reply-to-field from date message-id)
  (cond ((not from)
	 message-id)
	(message-id
	 ;; Append from field to message-id if needed.
	 (let ((from (rfc822:first-address from)))
	   (if (re-string-search-forward
		(let ((r (re-string-search-forward "@[^@]*\\'" from #f)))
		  (if r
		      (string-head from (re-match-start-index 0 r))
		      from))
		message-id #t)
	       message-id
	       (string-append message-id " (" from ")"))))
	(else
	 (let ((field (write-to-string (rfc822:first-address from))))
	   (if date
	       (string-append field "'s message of " date)
	       field)))))

;;;; Address Extraction

(define (strip-quoted-names-1 tokens)
  (define (parse-addr-spec tokens)
    (let ((local-part (parse-list tokens parse-word #\.)))
      (and local-part
	   (not (null? (cdr local-part)))
	   (eqv? #\@ (cadr local-part))
	   (let ((domain (parse-domain (cddr local-part))))
	     (and domain
		  (cons (string-append (separated-append (car local-part) ".")
				       "@"
				       (separated-append (car domain) "."))
			(cdr domain)))))))
  (define (parse-domain tokens)
    (parse-list tokens
		(lambda (tokens)
		  (and (not (null? tokens))
		       (string? (car tokens))
		       (not (eqv? #\" (string-ref (car tokens) 0)))
		       tokens))
		#\.))
  (define (parse-list tokens parse-element separator)
    (let ((first (parse-element tokens)))
      (and first
	   (let loop ((tokens (cdr first)) (words (list (car first))))
	     (let ((next
		    (and (not (null? tokens))
			 (eqv? separator (car tokens))
			 (parse-element (cdr tokens)))))
	       (if next
		   (loop (cdr next) (cons (car next) words))
		   (cons (reverse! words) tokens)))))))
  (define (parse-word tokens)
    (and (not (null? tokens))
	 (string? (car tokens))
	 (not (eqv? #\[ (string-ref (car tokens) 0)))
	 tokens))
  (parse-list
   tokens
   (lambda (tokens)
     (or (parse-addr-spec tokens)
	 (let ((word (parse-word tokens)))
	   (and word
		(let ((tokens
		       (let loop ((tokens (cdr word)))
			 (let ((word (parse-word tokens)))
			   (if word
			       (loop (cdr word))
			       tokens)))))
		  (and (not (null? tokens))
		       (eqv? #\< (car tokens))
		       (let ((addr-spec
			      (parse-addr-spec
			       (let ((domains
				      (parse-list
				       (cdr tokens)
				       (lambda (tokens)
					 (and (not (null? tokens))
					      (eqv? #\@ (car tokens))
					      (parse-domain (cdr tokens))))
				       #\,)))
				 (if (and domains
					  (not (null? (cdr domains)))
					  (eqv? #\: (cadr domains)))
				     (cddr domains)
				     (cdr tokens))))))
			 (and addr-spec
			      (not (null? (cdr addr-spec)))
			      (eqv? #\> (cadr addr-spec))
			      (cons (car addr-spec) (cddr addr-spec))))))))))
   #\,))

;;;; Mail output

(define-command rmail-output-to-rmail-file
  "Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the
buffer visiting that file."
  (lambda ()
    (list (prompt-for-rmail-output-filename
	   "Output message to Rmail file"
	   (ref-variable rmail-last-rmail-file))))
  (lambda (pathname)
    (set-variable! rmail-last-rmail-file (->namestring pathname))
    (let ((memo (current-msg-memo)))
      (rmail-output-to-rmail-file (make-region (msg-memo/start memo)
					       (msg-memo/end memo))
				  pathname)
      (set-attribute! memo 'FILED)
      (if (ref-variable rmail-delete-after-output)
	  ((ref-command rmail-delete-forward) #f)))))

(define (rmail-output-to-rmail-file region pathname)
  ;; REGION is assumed to be in babyl format.
  (let ((buffer (pathname->buffer pathname)))
    (if buffer
	(begin
	  (if (eq? buffer (mark-buffer (region-start region)))
	      (editor-error
	       "Can't output message to same file it's already in"))
	  (with-buffer-open buffer
	    (lambda ()
	      (let ((memo (buffer-msg-memo buffer))
		    (end (buffer-end buffer)))
		(let ((start (mark-right-inserting-copy end))
		      (end (mark-left-inserting-copy end)))
		  (if memo
		      (delete-string (skip-chars-backward " \t\n" end)
				     end))
		  (insert-region (region-start region)
				 (region-end region)
				 end)
		  (if memo
		      (begin
			(memoize-messages buffer start end)
			(select-message buffer memo)))
		  (mark-temporary! start)
		  (mark-temporary! end))))))
	(begin
	  (if (not (file-exists? pathname))
	      (begin
		(if (not (prompt-for-yes-or-no?
			  (string-append "\"" (->namestring pathname)
					 "\" does not exist, create it")))
		    (editor-error "Output file does not exist."))
		(call-with-temporary-buffer " rmail output"
		  (lambda (buffer)
		    (insert-string babyl-initial-header (buffer-start buffer))
		    (write-region (buffer-region buffer) pathname #f #f)))))
	  (append-to-file region pathname #f #f)))))

(define-command rmail-output
  "Append this message to Unix mail file named FILE-NAME."
  (lambda ()
    (list (prompt-for-rmail-output-filename "Output message to Unix mail file"
					    (ref-variable rmail-last-file))))
  (lambda (filename)
    (set-variable! rmail-last-file (->namestring filename))
    (let ((memo (current-msg-memo)))
      (rmail-output-to-unix-mail-file (buffer-region (current-buffer))
				      filename)
      (set-attribute! memo 'FILED)
      (if (ref-variable rmail-delete-after-output)
	  ((ref-command rmail-delete-forward) #f)))))

(define (rmail-output-to-unix-mail-file region pathname)
  ;; REGION is assumed to be in RFC-822 format.
  (let ((buffer (temporary-buffer " rmail output")))
    (let ((end (mark-left-inserting-copy (buffer-end buffer))))
      (insert-region (region-start region) (region-end region) end)
      (insert-newline end)
      (let loop ((start (buffer-start buffer)))
	(if (re-search-forward "^From " start end #t)
	    (loop (replace-match ">\\&"))))
      (mark-temporary! end)
      (let ((start (buffer-start buffer)))
	(insert-string
	 (string-append
	  "From "
	  (or (rfc822:first-address
	       (fetch-first-field "from" start (header-end start end)))
	      "unknown")
	  " "
	  (universal-time->local-ctime-string (get-universal-time))
	  "\n")
	 start)))
    (define-variable-local-value! buffer
      (ref-variable-object translate-file-data-on-output)
      #f)
    (append-to-file (buffer-region buffer) pathname #f #f)
    (kill-buffer buffer)))

(define (prompt-for-rmail-output-filename prompt default)
  (->namestring
   (let ((pathname
	  (prompt-for-pathname
	   (string-append prompt " (default " (file-namestring default) ")")
	   (directory-pathname default))))
     (if (file-directory? pathname)
	 (merge-pathnames (file-pathname default)
			  (pathname-as-directory pathname))
	 pathname))))

;;;; Editing

(define-command rmail-edit-current-message
  "Edit the current RMAIL message."
  '()
  (lambda ()
    (let ((buffer (current-buffer)))
      (set-buffer-major-mode! buffer (ref-mode-object rmail-edit))
      (buffer-put! buffer
		   'RMAIL-OLD-TEXT
		   (extract-string (buffer-start buffer)
				   (buffer-end buffer)))
      (set-buffer-writeable! buffer)
      (message
       (substitute-command-keys
	"Editing: Type \\[rmail-cease-edit] to return to Rmail, \\[rmail-abort-edit] to abort."
	buffer)))))

(define-command rmail-cease-edit
  "Finish editing message; switch back to Rmail proper."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (guarantee-newline (buffer-end buffer))
      (set-buffer-major-mode! buffer (ref-mode-object rmail))
      (with-buffer-open buffer
	(lambda ()
	  (memoize-buffer buffer)
	  (let ((memo (buffer-msg-memo buffer)))
	    (if (msg-memo? memo)
		(let ((first (msg-memo/first memo))
		      (point (current-point)))
		  (if (mark<= (msg-memo/start first) point)
		      (let loop ((memo first))
			(if memo
			    (if (mark< point (msg-memo/end memo))
				(begin
				  ; Need to force a recalc of the summary line
				  ; after message edit
				  (let ((rmail-summary-buffer
					 (ref-variable rmail-summary-buffer)))
				    (if rmail-summary-buffer
					(let ((rmail-summary-vector
					       (ref-variable
						rmail-summary-vector
						rmail-summary-buffer)))
					  (if rmail-summary-vector
					      (vector-set! 
					       rmail-summary-vector
					       (- (msg-memo/number memo) 1)
					       false)))))
				  (let ((point
					 (line-start (msg-memo/start memo) 2)))
				    (if (string-prefix?
					 "Summary-line: "
					 (extract-string point
							 (line-end point 0)))
					(delete-string point
						       (line-start point 1))))
				  (select-message buffer memo))
				(loop (msg-memo/next memo))))))))))))))

(define-command rmail-abort-edit
  "Abort edit of current message; restore original contents."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((text (buffer-get buffer 'RMAIL-OLD-TEXT)))
	(if text
	    (begin
	      (delete-string (buffer-start buffer)
			     (buffer-end buffer))
	      (insert-string text (buffer-start buffer)))
	    (message "Can't restore buffer contents."))))
    ((ref-command rmail-cease-edit))))

;;;; Undigestifier

(define-command undigestify-rmail-message
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  ()
  (lambda ()
    (let ((buffer (current-buffer))
	  (memo (current-msg-memo)))
      (let ((temp (temporary-buffer " rmail undigestify")))
	(let ((start (buffer-start temp))
	      (end (mark-left-inserting-copy (buffer-end temp))))
	  (insert-string babyl-initial-message-start end)
	  (insert-region (buffer-start buffer) (buffer-end buffer) end)
	  (delete-string (skip-chars-backward " \t\n" end start) end)
	  (insert-string "\n\037" end)
	  (let ((digest-name
		 (rfc822:first-address
		  (let ((hend (header-end start end)))
		    (or (fetch-first-field "Reply-To" start hend)
			(fetch-first-field "To" start hend)
			(fetch-first-field "Apparently-To" start hend)
			(fetch-first-field "From" start hend))))))
	    (if (not digest-name)
		(editor-error "Message is not a digest--bad header."))
	    (if (not (re-search-backward digest-end-regexp end start #t))
		(editor-error "Message is not a digest--no end line."))
	    (let ((start
		   (mark-left-inserting-copy (digest-summary-end start end))))
	      (if (not (fetch-first-field "To" start (header-end start end)))
		  (begin
		    (insert-string "To: " start)
		    (insert-string digest-name start)
		    (insert-newline start)))
	      (let loop ()
		(let ((m (digest-message-end start end)))
		  (if m
		      (begin
			(move-mark-to! start m)
			(if (or (match-forward "End " start end true)
				(not
				 (fetch-first-field "To"
						    start
						    (header-end start end))))
			    (begin
			      (insert-string "To: " start)
			      (insert-string digest-name start)
			      (insert-string "\n\n" start)))
			(loop)))))
	      (mark-temporary! start)))
	  (mark-temporary! end))
	(message "Message successfully undigestified")
	(with-buffer-open buffer
	  (lambda ()
	    (let* ((end (msg-memo/end memo))
		   (start (mark-right-inserting-copy end)))
	      (insert-region (buffer-start temp)
			     (buffer-end temp)
			     end)
	      (kill-buffer temp)
	      (memoize-messages-insert buffer start end memo)
	      (mark-temporary! start)))))
      (show-message buffer (msg-memo/number memo))
      ((ref-command rmail-delete-forward) false))))

(define (digest-summary-end start end)
  (if (not (re-search-forward digest-summary-separator-regexp
			      (header-end start end) end #f))
      (editor-error "Missing summary separator"))
  (replace-match digest-separator-replacement))

(define (digest-message-end start end)
  (and (re-search-forward digest-message-separator-regexp start end false)
       (replace-match digest-separator-replacement)))

;;;; Message memoization

(define (memoize-buffer buffer)
  (let ((end (buffer-end buffer)))
    (let ((m
	   (re-match-forward babyl-header-start-regexp
			     (buffer-start buffer)
			     end
			     false)))
      (if m
	  (let ((m (re-search-forward babyl-header-end-regexp m end false)))
	    (if m
		(begin
		  (set-buffer-msg-memo! buffer #f)
		  (memoize-messages buffer m end))))))))

(define (memoize-messages buffer start end)
  (let ((memo (buffer-msg-memo buffer)))
    (with-values
	(lambda ()
	  (memoize-messages* start
			     end
			     (and (msg-memo? memo) (msg-memo/last memo))))
      (lambda (start tail)
	(if (not (msg-memo? memo))
	    (set-buffer-msg-memo! buffer (or tail true)))
	(let ((old-end (buffer-last-msg-end buffer)))
	  (if old-end
	      (mark-temporary! old-end)))
	(set-buffer-last-msg-end! buffer start)))))

(define (memoize-messages-insert buffer start end memo)
  (let ((next (msg-memo/next memo)))
    (if (not next)
	(memoize-messages buffer start end)
	(with-values (lambda () (memoize-messages* start end memo))
	  (lambda (start tail)
	    (mark-temporary! start)
	    (set-msg-memo/next! tail next)
	    (set-msg-memo/previous! next tail)
	    (let loop ((memo next) (n (+ (msg-memo/number tail) 1)))
	      (set-msg-memo/number! memo n)
	      (if (msg-memo/next memo)
		  (loop (msg-memo/next memo) (+ n 1)))))))))

(define (memoize-messages* start end tail)
  (message "Counting messages...")
  (let loop ((start (mark-left-inserting-copy start)) (tail tail) (n 1))
    (let ((mend (search-forward babyl-message-end-regexp start end false)))
      (if mend
	  (let ((mend (mark-left-inserting-copy mend)))
	    (canonicalize-message-attributes start)
	    (let ((memo
		   (make-msg-memo tail
				  false
				  start
				  (if tail (+ (msg-memo/number tail) 1) 1)
				  (parse-attributes start))))
	      (if tail
		  (set-msg-memo/next! tail memo))
	      (if (zero? (remainder n 20))
		  (message "Counting messages..." n))
	      (loop mend memo (+ n 1))))
	  (begin
	    (message "Counting messages...done")
	    (values start tail))))))

(define-integrable (buffer-msg-memo buffer)
  (buffer-get buffer 'RMAIL-MSG-MEMO))

(define-integrable (set-buffer-msg-memo! buffer memo)
  (buffer-put! buffer 'RMAIL-MSG-MEMO memo))

(define-integrable (buffer-last-msg-end buffer)
  (buffer-get buffer 'RMAIL-LAST-MSG-END))

(define-integrable (set-buffer-last-msg-end! buffer memo)
  (buffer-put! buffer 'RMAIL-LAST-MSG-END memo))

(define-structure (msg-memo (conc-name msg-memo/))
  previous
  next
  (start false read-only true)
  number
  attributes)

(define (msg-memo/end memo)
  (let ((next (msg-memo/next memo)))
    (if next
	(msg-memo/start next)
	(buffer-last-msg-end (mark-buffer (msg-memo/start memo))))))

(define (msg-memo/start-body memo)
  (let ((start (msg-memo/start memo)))
    (or (re-search-forward babyl-eooh-regexp start (msg-memo/end memo) false)
	start)))

(define (msg-memo/end-body memo)
  (mark-1+ (msg-memo/end memo)))

(define (msg-memo/first memo)
  (let loop ((memo memo))
    (let ((previous (msg-memo/previous memo)))
      (if previous
	  (loop previous)
	  memo))))

(define (msg-memo/last memo)
  (let loop ((memo memo))
    (let ((next (msg-memo/next memo)))
      (if next
	  (loop next)
	  memo))))

(define (msg-memo/nth memo n)
  (if (= n (msg-memo/number memo))
      memo
      (let ((msg-memo/next
	     (if (< n (msg-memo/number memo))
		 msg-memo/previous
		 msg-memo/next)))
	(let loop ((memo memo))
	  (let ((next (msg-memo/next memo)))
	    (cond ((not next) memo)
		  ((= n (msg-memo/number next)) next)
		  (else (loop next))))))))

(define-integrable (msg-memo/deleted? memo)
  (memq 'DELETED (msg-memo/attributes memo)))

(define (msg-memo/next-undeleted memo)
  (let ((next (msg-memo/next memo)))
    (and next
	 (if (msg-memo/deleted? next)
	     (msg-memo/next-undeleted next)
	     next))))

(define (msg-memo/previous-undeleted memo)
  (let ((previous (msg-memo/previous memo)))
    (and previous
	 (if (msg-memo/deleted? previous)
	     (msg-memo/previous-undeleted previous)
	     previous))))

(define (msg-memo/previous-deleted memo)
  (let ((previous (msg-memo/previous memo)))
    (and previous
	 (if (msg-memo/deleted? previous)
	     previous
	     (msg-memo/previous-deleted previous)))))

;;;; Attributes and Labels

(define-command rmail-add-label
  "Add LABEL to labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  (lambda () (list (rmail-read-label "Add label" #f)))
  (lambda (label)
    (let ((memo (current-msg-memo))
	  (attribute (label->attribute label)))
      (if attribute
	  (set-attribute! memo attribute)
	  (set-keyword! memo label)))))

(define-command rmail-kill-label
  "Remove LABEL from labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  (lambda () (list (rmail-read-label "Remove label" #t)))
  (lambda (label)
    (let ((memo (current-msg-memo))
	  (attribute (label->attribute label)))
      (if attribute
	  (clear-attribute! memo attribute)
	  (clear-keyword! memo label)))))

(define (rmail-read-label prompt require-match?)
  (let ((label
	 (prompt-for-string-table-name
	  prompt
	  rmail-last-label
	  (alist->string-table
	   (map list
		(append! (map symbol-name attributes)
			 (buffer-keywords (current-buffer)))))
	  'REQUIRE-MATCH? require-match?)))
    (set! rmail-last-label label)
    label))

(define rmail-last-label #f)

(define (canonicalize-message-attributes mstart)
  (let ((start (attributes-start-mark mstart)))
    (let ((end (line-end start 0)))
      (let loop ((m start) (in-labels? false))
	(cond ((re-match-forward " [^ ,]+," m end false)
	       (loop (re-match-end 0) in-labels?))
	      ((and (not in-labels?) (match-forward "," m end false))
	       => (lambda (m) (loop m true)))
	      ((and in-labels? (mark= m end))
	       unspecific)
	      ((re-match-forward " *\\([^ ,]+\\)," m end false)
	       (loop (replace-match " \\1,") in-labels?))
	      ((and (not in-labels?) (re-match-forward " +," m end false))
	       (loop (replace-match ",") true))
	      ((and in-labels? (re-match-forward " +$" m end false))
	       (delete-match))
	      (else
	       (editor-error "Malformed message attributes: "
			     (extract-string start end))))))))

(define (set-attribute! memo attribute)
  (if (not (memq attribute (msg-memo/attributes memo)))
      (begin
	(set-msg-memo/attributes! memo
				  (cons attribute
					(msg-memo/attributes memo)))
	(let ((start (msg-memo/start memo)))
	  (with-group-open (mark-group start)
	    (lambda ()
	      (insert-string (attribute->string attribute)
			     (attributes-end-mark start))
	      (update-mode-line! (mark-buffer start))))))))

(define (clear-attribute! memo attribute)
  (if (memq attribute (msg-memo/attributes memo))
      (begin
	(set-msg-memo/attributes! memo
				  (delq! attribute
					 (msg-memo/attributes memo)))
	(let ((start (msg-memo/start memo)))
	  (with-group-open (mark-group start)
	    (lambda ()
	      (if (search-forward (attribute->string attribute)
				  (attributes-start-mark start)
				  (attributes-end-mark start)
				  true)
		  (delete-match))
	      (update-mode-line! (mark-buffer start))))))))

(define (attribute->string attribute)
  (string-append " " (string-downcase (symbol-name attribute)) ","))

(define (label->attribute label)
  (let ((s (intern-soft label)))
    (and s
	 (memq s attributes)
	 s)))

(define attributes
  '(DELETED ANSWERED FILED FORWARDED UNSEEN EDITED RESENT))

(define (set-keyword! memo keyword)
  (let ((mstart (msg-memo/start memo))
	(ks (keyword->string keyword)))
    (with-group-open (mark-group mstart)
      (lambda ()
	(if (not (search-forward ks
				 (labels-start-mark mstart)
				 (labels-end-mark mstart)
				 #t))
	    (insert-string ks (labels-end-mark mstart)))
	(let ((buffer (mark-buffer mstart)))
	  (if (not (member keyword (buffer-keywords buffer)))
	      (begin
		(buffer-remove! buffer 'RMAIL-KEYWORDS)
		(insert-string
		 (string-append "," keyword)
		 (line-end (or (keywords-start-mark buffer)
			       (let ((s (line-end (buffer-start buffer) 0)))
				 (insert-string "\nLabels:" s)
				 (mark1+ s)))
			   0)))))
	(update-mode-line! (mark-buffer mstart))))))

(define (clear-keyword! memo keyword)
  (let ((mstart (msg-memo/start memo)))
    (with-group-open (mark-group mstart)
      (lambda ()
	(if (search-forward (keyword->string keyword)
			    (labels-start-mark mstart)
			    (labels-end-mark mstart)
			    #t)
	    (delete-match))
	(update-mode-line! (mark-buffer mstart))))))

(define (keyword->string keyword)
  (string-append " " (string-downcase keyword) ","))

(define (buffer-keywords buffer)
  (cdr (or (buffer-get buffer 'RMAIL-KEYWORDS #f)
	   (let ((keywords (cons 'RMAIL-KEYWORDS (parse-keywords buffer))))
	     (buffer-put! buffer 'RMAIL-KEYWORDS keywords)
	     keywords))))

(define (attributes-start-mark mstart)
  (let ((m
	 (re-match-forward babyl-message-start-regexp
			   mstart
			   (group-end mstart)
			   false)))
    (if (not m)
	(editor-error "Mark not at message start: " mstart))
    m))

(define (attributes-end-mark mstart)
  (mark-1+ (labels-start-mark mstart)))

(define (labels-start-mark mstart)
  (let ((m
	 (let ((lstart (line-start mstart 1 'ERROR)))
	   (search-forward ",," lstart (line-end lstart 0) false))))
    (if (not m)
	(editor-error "Can't find attributes/labels separator"))
    m))

(define (labels-end-mark mstart)
  (line-end mstart 1 'ERROR))

(define (parse-attributes mstart)
  (map intern
       (parse-label-list (attributes-start-mark mstart)
			 (attributes-end-mark mstart))))

(define (parse-labels mstart)
  (parse-label-list (labels-start-mark mstart)
		    (labels-end-mark mstart)))

(define (parse-keywords buffer)
  (with-buffer-open buffer
    (lambda ()
      (let ((start (keywords-start-mark buffer)))
	(if start
	    (parse-label-list start (line-end start 0))
	    '())))))

(define (keywords-start-mark buffer)
  (search-forward "\nLabels:"
		  (buffer-start buffer)
		  (msg-memo/start (msg-memo/first (buffer-msg-memo buffer)))
		  #t))

(define (parse-label-list start end)
  (let loop ((m start))
    (if (mark< m end)
	(let ((aend (char-search-forward #\, m end false)))
	  (let ((label
		 (string-downcase
		  (string-trim
		   (extract-string m (if aend (mark-1+ aend) end)))))
		(rest (if aend (loop aend) '())))
	    (if (string-null? label)
		rest
		(cons label rest))))
	'())))

(define-command rmail-toggle-header
  "Show original message header if pruned header currently shown, or vice versa."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (let ((memo (current-msg-memo)))
	(with-buffer-open buffer
	  (lambda ()
	    (let ((start (msg-memo/start memo))
		  (end (msg-memo/end memo)))
	      (cond ((match-forward "\f\n0" start end false)
		     (reformat-message start end))
		    ((match-forward "\f\n1" start end false)
		     (unformat-message start end)))))))
      (set-current-point! (buffer-start buffer)))))

(define (reformat-message start end)
  (let ((m (mark+ start 2)))
    (delete-right-char m)
    (insert-char #\1 m))
  (if (not (re-search-forward babyl-eooh-regexp start end false))
      (editor-error))
  (let ((eooh (re-match-start 0)))
    (let ((hstart (mark-right-inserting-copy (line-start eooh 1 'ERROR))))
      (let ((hend
	     (let ((m (search-forward "\n\n" hstart end false)))
	       (if m
		   (mark-left-inserting-copy m)
		   (let ((m (mark-left-inserting-copy end)))
		     (if (char-match-backward #\newline m)
			 (insert-newline m)
			 (insert-newlines 2 m))
		     m)))))
	(insert-string (extract-string hstart hend) eooh)
	(let ((regexp (ref-variable rmail-ignored-headers)))
	  (if regexp
	      (do ()
		  ((not (re-search-forward regexp hstart hend true)))
		(let ((m (line-start (re-match-start 0) 0)))
		  (delete-string
		   m
		   (mark-1+ (re-search-forward "\n[^ \t]" m hend false)))))))
	(let ((filter (ref-variable rmail-message-filter)))
	  (if filter
	      (filter hstart hend)))
	(mark-temporary! hend)
	(mark-temporary! hstart)))))

(define (unformat-message start end)
  (let ((m (mark+ start 2)))
    (delete-right-char m)
    (insert-char #\0 m))
  (let ((start
	 (let ((start (line-start start 2 'ERROR)))
	   (if (match-forward "Summary-line:" start end true)
	       (line-start start 1 'ERROR)
	       start))))
    (if (not (re-search-forward babyl-eooh-regexp start end false))
	(editor-error))
    (let ((header (extract-and-delete-string start (re-match-start 0))))
      (let ((hstart (line-start start 1)))
	(delete-string hstart (header-end hstart end))
	(insert-string header hstart)))))

;;;; Mail conversion

(define (convert-region-to-babyl-format start end)
  (define (loop point count)
    (text-clip point end)
    (cond ((mark= point end)
	   count)
	  ((re-match-forward babyl-header-start-regexp point end false)
	   (delete-string
	    point
	    (or (search-forward babyl-header-end-regexp point end false) end))
	   (loop point count))
	  ((re-match-forward babyl-message-start-regexp point end false)
	   (let ((m
		  (or (search-forward babyl-message-end-regexp point end false)
		      (missing-end end "Babyl"))))
	     (delete-string m (skip-chars-forward " \t\n" m end))
	     (loop m (+ count 1))))
	  ((re-match-forward umail-message-start-regexp point end false)
	   (let ((point (mark-right-inserting-copy point))
		 (end (mark-left-inserting-copy end)))
	     (nuke-pinhead-header point end)
	     (mark-temporary! end)
	     (mark-temporary! point)
	     (process-rfc822
	      point
	      count
	      (if (re-search-forward umail-message-end-regexp point end false)
		  (re-match-start 0)
		  end))))
	  ((re-match-forward mmdf-message-start-regexp point end true)
	   (let ((start (delete-match)))
	     (process-rfc822
	      start
	      count
	      (if (re-search-forward mmdf-message-end-regexp start end true)
		  (mark-1+ (replace-match "\037"))
		  (missing-end end "MMDF")))))
	  (else
	   (editor-error "error converting to Babyl format")
	   true)))

  (define (process-rfc822 point count mend)
    (let ((mend (mark-left-inserting-copy mend)))
      (rfc822-region->babyl (make-region point mend))
      (mark-temporary! mend)
      (loop mend (+ count 1))))

  (define (missing-end end type)
    (message "Invalid " type " format in inbox!")
    (sit-for 1)
    end)

  (with-text-clipped start end
    (lambda ()
      (loop (skip-chars-forward "\n" start end) 0))))

(define (rfc822-region->babyl region)
  (let ((start (mark-left-inserting-copy (region-start region))))
    (insert-string babyl-initial-message-start start)
    (mark-temporary! start)
    (let ((end (mark-left-inserting-copy (region-end region))))
      ;; Eliminate babyl message-separation pair from message body.
      (do ((m start (replace-match "\n^_")))
	  ((not (search-forward "\n\037" m end #f))))
      (guarantee-newline end)
      (if (not (eqv? (integer->char #o37) (extract-right-char end)))
	  (insert-string "\037" end))
      (mark-temporary! end))))

(define (convert-buffer-to-babyl-format buffer)
  (with-buffer-open buffer
    (lambda ()
      (let ((start (buffer-start buffer))
	    (end (buffer-end buffer)))
	(if (not (re-match-forward babyl-header-start-regexp start end false))
	    (insert-string babyl-initial-header start))
	(search-backward "\n\037" end start false)
	(let ((start (re-match-end 0)))
	  (let ((m (skip-chars-forward "\n" start end)))
	    (cond ((and (mark= m end)
			(mark< start m))
		   (delete-string start m))
		  ((re-match-forward umail-message-start-regexp m end false)
		   (delete-string start m)
		   (message "Converting to Babyl format...")
		   (convert-region-to-babyl-format start end)
		   (message "Converting to Babyl format...done")))))))))

(define (nuke-pinhead-header start end)
  (let ((hend
	 (or (search-forward "\n\n" start end false)
	     (begin
	       (insert-string "\n\n" end)
	       end))))
    (let ((has-from (search-forward "\nFrom:" start hend true))
	  (has-date (search-forward "\nDate:" start hend true)))
      (if (and has-from has-date)
	  (delete-string start (line-start start 1))
	  (begin
	    (re-match-forward umail-message-start-regexp start hend false)
	    (replace-match
	     (let ((from "From: \\1")
		   (date
		    (if (mark< (re-match-start 7) (re-match-end 7))
			"Date: \\3, \\5 \\4 \\8 \\6\\7"
			"Date: \\3, \\5 \\4 \\8 \\6 EST")))
	       (cond (has-from date)
		     (has-date from)
		     (else (string-append date "\n" from))))))))))

;;;; Utilities

(define (without-clipping buffer thunk)
  (let ((group (buffer-group buffer)))
    (with-group-text-clipped! group 0 (group-length group) thunk)))

(define-integrable (with-buffer-open buffer thunk)
  (with-group-open (buffer-group buffer) thunk))

(define-integrable (with-buffer-undo-disabled buffer thunk)
  (with-group-undo-disabled (buffer-group buffer) thunk))

(define (with-group-open group thunk)
  (let ((outside-writeable)
	(inside-writeable 'FULLY)
	(outside-start)
	(outside-end)
	(inside-start (mark-permanent! (group-absolute-start group)))
	(inside-end (mark-permanent! (group-absolute-end group))))
    (dynamic-wind (lambda ()
		    (set! outside-writeable (group-writeable? group))
		    (set! outside-start (group-start-mark group))
		    (set! outside-end (group-end-mark group))
		    (set-group-writeable?! group inside-writeable)
		    (set-group-start-mark! group inside-start)
		    (set-group-end-mark! group inside-end))
		  thunk
		  (lambda ()
		    (set! inside-writeable (group-writeable? group))
		    (set! inside-start (group-start-mark group))
		    (set! inside-end (group-end-mark group))
		    (set-group-writeable?! group outside-writeable)
		    (set-group-start-mark! group outside-start)
		    (set-group-end-mark! group outside-end)))))

;;;; Constants

(define umail-message-start-regexp
  "From \\([^ \n]*\\(\\|\".*\"[^ \n]*\\)\\)  ?\\([^ \n]*\\) \\([^ \n]*\\) *\\([0-9]*\\) \\([0-9:]*\\)\\( ?[A-Z]?[A-Z][A-Z]T\\| ?[-+]?[0-9][0-9][0-9][0-9]\\|\\) \\([1-9][0-9][0-9][0-9]\\) *\\(remote from .*\\)?$")

(define umail-message-end-regexp
  false)

(define mmdf-message-start-regexp
  "^\001\001\001\001\n")

(define mmdf-message-end-regexp
  "^\001\001\001\001\n")

(define babyl-header-start-regexp
  "^BABYL OPTIONS:")

(define babyl-header-end-regexp
  "\n\037")

(define babyl-initial-header
  "BABYL OPTIONS:
Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.\n\037")

(define babyl-message-start-regexp
  "\f\n[01],")

(define babyl-message-end-regexp
  "\n\037")

(define babyl-eooh-string
  "*** EOOH ***\n")

(define babyl-eooh-regexp
  (string-append "^" (re-quote-string babyl-eooh-string)))

(define babyl-initial-message-start
  (string-append "\f\n0, unseen,,\n" babyl-eooh-string))

(define-integrable digest-end-regexp
  "^End of.*Digest.*\n\\*\\*\\*\\*\\*\\*\\*\\*\\**\\(\n------*\\)*")

(define-integrable digest-summary-separator-regexp
  "\n*\n------------------------------*\n*")

(define-integrable digest-message-separator-regexp
  "\n*\n\n----------------------------*\n*")

(define digest-separator-replacement
  (string-append "\n\037" babyl-initial-message-start))