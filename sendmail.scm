#| -*-Scheme-*-

$Id: sendmail.scm,v 1.92 2007/09/09 16:36:50 riastradh Exp $

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

;;;; Mail Sending


(define-variable user-mail-address
  "Full mailing address of this user.
This is initialized based on `mail-host-address',
after your init file is read, in case it sets `mail-host-address'."
  #f
  string-or-false?)

(define-variable mail-host-address
  "Name of this machine, for purposes of naming users."
  #f
  string-or-false?)

(define-variable mail-full-name
  "Your full name.
Appears in the From: field of mail and news messages, following the address.
If set to the null string, From: field contains only the email address."
  #f
  string-or-false?)

(define-variable mail-from-style
  "Specifies how \"From:\" fields look.
One of the following values:
'PARENS	king@grassland.com (Elvis Parsley)
'ANGLES	Elvis Parsley <king@grassland.com>
#F	king@grassland.com"
  'ANGLES
  (lambda (object) (memq object '(PARENS ANGLES #F))))

(define-variable mail-organization
  "The name of your organization.
Appears in the Organization: field of mail and news messages.
If set to the null string, no Organization: field is generated."
  ""
  string?)

(define-variable mail-identify-reader
  "Switch controlling generation of User-Agent headers in messages."
  #t
  boolean?)

(define-variable mail-default-reply-to
  "Address to insert as default Reply-to field of outgoing messages."
  #f
  (lambda (object)
    (or (not object)
	(string? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 0)))))

(define-variable mail-self-blind
  "True means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default."
  #f
  boolean?)

(define-variable mail-archive-file-name
  "Name of file to write all outgoing messages in, or #f for none."
  #f
  string-or-false?)

(define-variable mail-relay-host
  "Name of host to which all outgoing mail should be sent.
Can be a host name (a string) or #F.
If #F, mail is passed to sendmail for handling.
Otherwise, mail is sent directly to the named host using SMTP."
  #f
  string-or-false?)

(define-variable mail-relay-service
  "Service to connect to on the mail relay host.
Can be a service name (a string), a service number, or #F.
If #F, service \"smtp\" is used.
This is used only of `mail-relay-host' is set."
  #f
  (lambda (service)
    (or (not service)
        (exact-positive-integer? service)
        (string? service))))

(define-variable smtp-trace
  "If true, direct SMTP transmissions are traced in a buffer."
  #f
  boolean?)

(define-variable smtp-require-valid-recipients
  "If true, all SMTP recipients must be valid before a message is sent.
Otherwise, only one valid recipient is required."
  #t
  boolean?)

(define-variable sendmail-program
  "Filename of sendmail program."
  #f
  string-or-false?)

(define-variable send-mail-procedure
  "Procedure to call to send the current buffer as mail.
The headers are delimited by a string found in mail-header-separator."
  (lambda () (sendmail-send-it))
  (lambda (object)
    (and (procedure? object)
	 (procedure-arity-valid? object 0))))
(variable-permanent-local! (ref-variable-object send-mail-procedure))

(define-variable mail-yank-ignored-headers
  "Delete these headers from old message when it's inserted in a reply."
  (regexp-group "^via:"
		"^mail-from:"
		"^origin:"
		"^status:"
		"^remailed"
		"^received:"
		"^[a-z-]*message-id:"
		"^summary-line:"
		"^to:"
		"^cc:"
		"^subject:"
		"^in-reply-to:"
		"^return-path:")
  string?)

(define-variable mail-interactive
  "True means when sending a message wait for and display errors.
#F means let mailer mail back a message to report errors."
  #f
  boolean?)

(define-variable mail-header-separator
  "Line used to separate headers from text in messages being composed."
  "--text follows this line--"
  string?)

(define-variable mail-header-function
  "A function of one argument, POINT (the current point), which inserts
additional header lines into a mail message.  The function is called
after all other headers are inserted.  If this variable is #f, it
is ignored."
  #f
  (lambda (object)
    (or (false? object)
	(and (procedure? object)
	     (procedure-arity-valid? object 1)))))

(define-variable mail-reply-buffer
  ""
  #f
  (lambda (object) (or (false? object) (buffer? object))))
(variable-permanent-local! (ref-variable-object mail-reply-buffer))

(define-variable mail-abbreviate-mime
  "If true, sent mail doesn't contain some unnecessary MIME headers.
Specifically, Content-Type and Content-Transfer-Encoding headers
  in subparts of a multipart message are omitted if they specify the default.
If false, sent mail contains full MIME headers."
  #t
  boolean?)

(define-command mail
  "Edit a message to be sent.  Argument means resume editing (don't erase).
While editing message, type C-c C-c to send the message and exit.

Separate names of recipients with commas.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields.

If mail-self-blind is true, a BCC: to yourself is inserted when the
message is initialized.

If mail-default-reply-to is a string, a Reply-to: field containing
that string is inserted.

If mail-archive-file-name is true, an FCC: field with that file name
is inserted."
  "P"
  (lambda (no-erase?) (mail-command no-erase? select-buffer)))

(define-command mail-other-window
  "Like \\[mail], but display mail buffer in another window."
  "P"
  (lambda (no-erase?) (mail-command no-erase? select-buffer-other-window)))

(define-command mail-other-frame
  "Like \\[mail], but display mail buffer in another frame."
  "P"
  (lambda (no-erase?) (mail-command no-erase? select-buffer-other-screen)))

(define (mail-command no-erase? select-buffer)
  (make-mail-buffer '(("To" "") ("Subject" "")) #f select-buffer
		    (if no-erase?
			'KEEP-PREVIOUS-MAIL
			'QUERY-DISCARD-PREVIOUS-MAIL)))

(define (make-mail-buffer headers reply-buffer #!optional
			  selector handle-previous buffer-name mode)
  (let ((selector (if (default-object? selector) #f selector))
	(handle-previous
	 (if (default-object? handle-previous)
	     'QUERY-DISCARD-PREVIOUS-MAIL
	     handle-previous))
	(buffer-name
	 (if (or (default-object? buffer-name) (not buffer-name))
	     "*mail*"
	     buffer-name))
	(mode (if (default-object? mode) #f mode)))
    (let ((buffer (find-buffer buffer-name))
	  (continue
	   (lambda (select?)
	     (let ((buffer (find-or-create-buffer buffer-name)))
	       (buffer-reset! buffer)
	       (set-buffer-default-directory! buffer
					      (default-homedir-pathname))
	       (setup-buffer-auto-save! buffer)
	       (mail-setup buffer headers reply-buffer mode)
	       (if (and select? selector) (selector buffer))
	       buffer))))
      (cond ((not buffer)
	     (continue #t))
	    ((eq? handle-previous 'KEEP-PREVIOUS-MAIL)
	     (if selector (selector buffer))
	     #f)
	    ((or (not (buffer-modified? buffer))
		 (eq? handle-previous 'DISCARD-PREVIOUS-MAIL))
	     (continue #t))
	    ((eq? handle-previous 'QUERY-DISCARD-PREVIOUS-MAIL)
	     (if selector (selector buffer))
	     (if (cleanup-pop-up-buffers
		  (lambda ()
		    (if (not selector) (pop-up-buffer buffer #f))
		    (prompt-for-confirmation?
		     "Unsent message being composed; erase it")))
		 (continue #f)
		 #f))
	    (else
	     (error:bad-range-argument handle-previous 'MAKE-MAIL-BUFFER))))))

(define (mail-setup buffer headers reply-buffer #!optional mode)
  (guarantee-mail-aliases)
  (set-buffer-major-mode! buffer
			  (or (and (not (default-object? mode)) mode)
			      (ref-mode-object mail)))
  (local-set-variable! mail-reply-buffer reply-buffer buffer)
  (let ((headers (add-standard-headers headers buffer))
	(point (mark-left-inserting-copy (buffer-start buffer)))
	(fill
	 (lambda (start end)
	   (fill-region-as-paragraph start end
				     "\t" (ref-variable fill-column buffer)
				     #f))))
    (let ((start (mark-right-inserting-copy point)))
      (for-each
       (lambda (header)
	 (let ((key (car header))
	       (value (cadr header)))
	   (if value
	       (begin
		 (move-mark-to! start point)
		 (insert-string key point)
		 (insert-string ": " point)
		 (let ((end (string-length value)))
		   (let loop ((start 0))
		     (let ((index
			    (substring-find-next-char value start end
						      #\newline)))
		       (if index
			   (let ((index (fix:+ index 1)))
			     (insert-substring value start index point)
			     (if (and (fix:< index end)
				      (not
				       (let ((char (string-ref value index)))
					 (or (char=? char #\space)
					     (char=? char #\tab)))))
				 (insert-char #\tab point))
			     (loop index))
			   (insert-substring value start end point)))))
		 (if (and (not (string-null? value))
			  (if (null? (cddr header))
			      (or (string-ci=? key "to")
				  (string-ci=? key "cc"))
			      (caddr header)))
		     (fill start point))
		 (insert-newline point)))))
       headers)
      (mark-temporary! start))
    (let ((mail-header-function (ref-variable mail-header-function buffer)))
      (if mail-header-function
	  (mail-header-function point)))
    (insert-string (ref-variable mail-header-separator buffer) point)
    (insert-newline point)
    (mark-temporary! point)
    (let ((given-header?
	   (lambda (name null-true?)
	     (let ((header
		    (find-matching-item headers
		      (lambda (header)
			(string-ci=? (car header) name)))))
	       (and header
		    (cadr header)
		    (if null-true?
			(string-null? (cadr header))
			(not (string-null? (cadr header)))))))))
      (set-buffer-point! buffer
			 (if (given-header? "To" #t)
			     (mail-position-on-field buffer "To")
			     (buffer-end buffer)))
      (if (not (or (given-header? "To" #f)
		   (given-header? "Subject" #f)
		   (given-header? "In-reply-to" #f)))
	  (buffer-not-modified! buffer))))
  (event-distributor/invoke! (ref-variable mail-setup-hook buffer) buffer))

(define (add-standard-headers headers buffer)
  (let ((add
	 (lambda (key value)
	   (if (string? value)
	       (list (list key value #f))
	       '()))))
    (append headers
	    (add "Reply-to"
		 (let ((mail-default-reply-to
			(ref-variable mail-default-reply-to buffer)))
		   (if (procedure? mail-default-reply-to)
		       (mail-default-reply-to)
		       mail-default-reply-to)))
	    (add "BCC"
		 (and (ref-variable mail-self-blind buffer)
		      (mail-from-string buffer)))
	    (add "FCC" (ref-variable mail-archive-file-name buffer)))))

(define (mail-from-string lookup-context)
  (let ((address (user-mail-address lookup-context))
	(full-name (mail-full-name lookup-context)))
    (if (string-null? full-name)
	address
	(case (ref-variable mail-from-style lookup-context)
	  ((PARENS)
	   (string-append address " (" full-name ")"))
	  ((ANGLES)
	   (string-append (rfc822:quote-string full-name) " <" address ">"))
	  (else address)))))

(define (user-mail-address lookup-context)
  (or (ref-variable user-mail-address lookup-context)
      (string-append (current-user-name)
		     "@"
		     (or (ref-variable mail-host-address lookup-context)
			 (os/hostname)))))

(define (mail-full-name lookup-context)
  (or (ref-variable mail-full-name lookup-context)
      ""))

(define-variable mail-setup-hook
  "An event distributor invoked immediately after a mail buffer is initialized.
The mail buffer is passed as an argument; it is not necessarily selected."
  (make-event-distributor))

(define-major-mode mail text "Mail"
  "Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
\\[mail-send]  mail-send (send the message)    \\[mail-send-and-exit]  mail-send-and-exit
Here are commands that move to a header field (and create it if there isn't):
	 \\[mail-to]  move to To:	\\[mail-subject]  move to Subject:
	 \\[mail-cc]  move to CC:	\\[mail-bcc]  move to BCC:
\\[mail-signature]  mail-signature (insert ~/.signature file).
\\[mail-yank-original]  mail-yank-original (insert current message, in Rmail).
\\[mail-fill-yanked-message]  mail-fill-yanked-message (fill what was yanked).
\\[mail-browse-attachments]  view, add, or delete MIME attachments."
  (lambda (buffer)
    (add-kill-buffer-hook buffer mail-kill-buffer)
    (local-set-variable!
     paragraph-start
     (string-append "^"
		    (re-quote-string (ref-variable mail-header-separator))
		    "$\\|^[ \t]*[-_][-_][-_]+$\\|"
		    (ref-variable paragraph-start buffer))
     buffer)
    (local-set-variable!
     paragraph-separate
     (string-append "^"
		    (re-quote-string (ref-variable mail-header-separator))
		    "$\\|^[ \t]*[-_][-_][-_]+$\\|"
		    (ref-variable paragraph-separate buffer))
     buffer)
    (event-distributor/invoke! (ref-variable mail-mode-hook buffer) buffer)))

(define-variable mail-mode-hook
  "An event distributor that is invoked when entering Mail mode."
  (make-event-distributor))

(define (mail-kill-buffer buffer)
  (let ((attachments-buffer (buffer-get buffer 'MIME-ATTACHMENTS-BROWSER #f)))
    (if attachments-buffer
	(kill-buffer attachments-buffer))))

(define-key 'mail '(#\C-c #\?) 'describe-mode)
(define-key 'mail '(#\C-c #\C-f #\C-t) 'mail-to)
(define-key 'mail '(#\C-c #\C-f #\C-b) 'mail-bcc)
(define-key 'mail '(#\C-c #\C-f #\C-c) 'mail-cc)
(define-key 'mail '(#\C-c #\C-f #\C-s) 'mail-subject)
(define-key 'mail '(#\C-c #\C-a) 'mail-browse-attachments)
(define-key 'mail '(#\C-c #\C-w) 'mail-signature)
(define-key 'mail '(#\C-c #\C-y) 'mail-yank-original)
(define-key 'mail '(#\C-c #\C-q) 'mail-fill-yanked-message)
(define-key 'mail '(#\C-c #\C-c) 'mail-send-and-exit)
(define-key 'mail '(#\C-c #\C-s) 'mail-send)

(define-command mail-signature
  "Sign letter with contents of ~/.signature file."
  ()
  (lambda ()
    (insert-file (buffer-end (selected-buffer)) "~/.signature")))

(define ((field-mover field))
  (set-current-point! (mail-position-on-field (selected-buffer) field)))

(define ((cc-field-mover field))
  (set-current-point! (mail-position-on-cc-field (selected-buffer) field)))

(define-command mail-to
  "Move point to end of To field."
  ()
  (field-mover "To"))

(define-command mail-subject
  "Move point to end of Subject field."
  ()
  (field-mover "Subject"))

(define-command mail-cc
  "Move point to end of CC field."
  ()
  (cc-field-mover "CC"))

(define-command mail-bcc
  "Move point to end of BCC field."
  ()
  (cc-field-mover "BCC"))

(define (mail-position-on-field buffer field)
  (mail-field-end! (buffer-start buffer)
		   (mail-match-header-separator buffer)
		   field))

(define (mail-position-on-cc-field buffer field)
  (let ((start (buffer-start buffer))
	(end (mail-match-header-separator buffer)))
    (or (mail-field-end start end field)
	(mail-insert-field (or (mail-field-end start end "CC")
			       (mail-field-end start end "To")
			       (mail-insert-field end "To"))
			   field))))

(define (mail-match-header-separator buffer)
  (if (not (re-search-forward
	    (string-append
	     "^"
	     (re-quote-string (ref-variable mail-header-separator buffer))
	     "$")
	    (buffer-start buffer)
	    (buffer-end buffer)
	    #f))
      (editor-error "Can't find mail-header-separator."))
  (re-match-start 0))

(define (mail-header-end start #!optional end error?)
  (let ((mark
	 (search-forward "\n\n"
			 start
			 (if (or (default-object? end) (not end))
			     (group-end start)
			     end)
			 #f)))
    (if (and (not mark) (or (default-object? error?) error?))
	(error "Unable to locate mail header end:" start))
    (and mark
	 (mark-1+ mark))))

(define (mail-field-start header-start header-end field)
  (re-search-forward (string-append "^" field ":[ \t]*")
		     header-start
		     header-end
		     #t))

(define (mail-field-end header-start header-end field)
  (let ((field-start (mail-field-start header-start header-end field)))
    (and field-start
	 (%mail-field-end field-start header-end))))

(define (mail-field-region header-start header-end field)
  (let ((field-start (mail-field-start header-start header-end field)))
    (and field-start
	 (make-region field-start (%mail-field-end field-start header-end)))))

(define (%mail-field-end field-start header-end)
  (if (re-search-forward "^[^ \t]" field-start header-end #f)
      (mark-1+ (re-match-start 0))
      header-end))

(define (mail-insert-field mark field)
  (let ((mark (mark-left-inserting-copy mark)))
    (if (not (line-start? mark))
	(let ((ls (line-start mark 1 #f)))
	  (if ls
	      (move-mark-to! mark ls)
	      (begin
		(move-mark-to! mark (line-end mark 0))
		(insert-newline mark)))))
    (insert-string field mark)
    (insert-string ": " mark)
    (insert-newline mark)
    (mark-temporary! mark)
    (mark-1+ mark)))

(define (mail-field-end! header-start header-end field)
  (or (mail-field-end header-start header-end field)
      (mail-insert-field header-end field)))

(define (mail-new-field! header-start header-end field)
  (let ((region (mail-field-region header-start header-end field)))
    (if region
	(begin
	  (region-delete! region)
	  (region-start region))
	(mail-insert-field header-end field))))

(define (mail-insert-field-value header-end field value)
  (insert-string value (mail-insert-field header-end field)))

(define (mail-insert-field-value! header-start header-end field value)
  (insert-string value (mail-new-field! header-start header-end field)))

(define-command mail-yank-original
  "Insert the message being replied to, if any (in rmail).
Puts point after the text and mark before.
Indents each nonblank line ARG spaces (default 3).
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  "P"
  (lambda (argument)
    (let ((mail-reply-buffer (ref-variable mail-reply-buffer))
	  (left-margin
	   (if (command-argument-multiplier-only? argument)
	       0
	       (or (command-argument-value argument) 3))))
      (if mail-reply-buffer
	  (begin
	    (for-each (lambda (window)
			(if (not (window-has-no-neighbors? window))
			    (window-delete! window)))
		      (buffer-windows mail-reply-buffer))
	    (let ((end (mark-left-inserting-copy (current-point))))
	      (let ((start (mark-right-inserting-copy end)))
		(let ((method
		       (buffer-get mail-reply-buffer
				   'MAIL-YANK-ORIGINAL-METHOD
				   #f)))
		  (if method
		      (method mail-reply-buffer left-margin end)
		      (insert-region (buffer-start mail-reply-buffer)
				     (buffer-end mail-reply-buffer)
				     start)))
		(if (not (line-end? end))
		    (insert-newline end))
		(if (not (command-argument-multiplier-only? argument))
		    (begin
		      (mail-yank-clear-headers start end)
		      (indent-rigidly start end left-margin)))
		(mark-temporary! start)
		(mark-temporary! end)
		(push-current-mark! start)
		(set-current-point! end))))))))

(define (mail-yank-clear-headers start end)
  (let ((start (mark-left-inserting-copy start))
	(end
	 (mark-left-inserting-copy
	  (if (re-search-forward "\n\n" start end #f)
	      (mark1+ (re-match-start 0))
	      end)))
	(mail-yank-ignored-headers (ref-variable mail-yank-ignored-headers)))
    (with-text-clipped start end
      (lambda ()
	(do ()
	    ((not
	      (re-search-forward mail-yank-ignored-headers start end #t)))
	  (move-mark-to! start (re-match-start 0))
	  (delete-string
	   start
	   (if (re-search-forward "^[^ \t]" (line-end start 0) end #f)
	       (re-match-start 0)
	       end)))))
    (mark-temporary! start)
    (mark-temporary! end)))

(define-command mail-fill-yanked-message
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well."
  "P"
  (lambda (justify?)
    (let ((buffer (selected-buffer)))
      (mail-match-header-separator buffer)
      (fill-individual-paragraphs (re-match-end 0)
				  (buffer-end buffer)
				  (ref-variable fill-column)
				  justify?
				  #t))))

(define-command mail-send-and-exit
  "Send message like mail-send, then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window."
  "P"
  (lambda (argument)
    ((ref-command mail-send))
    (bury-buffer (selected-buffer))
    (if (and (not argument)
	     (not (window-has-no-neighbors? (selected-window)))
	     (eq? (ref-mode-object rmail)
		  (buffer-major-mode (window-buffer (other-window)))))
	(window-delete! (selected-window))
	(select-buffer (previous-buffer)))))

(define-command mail-send
  "Send the message in the current buffer.
If `mail-interactive' is true, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (if (if (buffer-pathname buffer)
	      (prompt-for-confirmation? "Send buffer contents as mail message")
	      (or (buffer-modified? buffer)
		  (prompt-for-confirmation? "Message already sent; resend")))
	  (begin
	    ((ref-variable send-mail-procedure))
	    (buffer-not-modified! buffer)
	    (delete-auto-save-file! buffer))))))

(define (sendmail-send-it)
  (let ((mail-buffer (selected-buffer)))
    (prepare-mail-buffer-for-sending mail-buffer
      (lambda (h-start h-end b-start b-end)
	(finish-preparing-mail-buffer h-start h-end b-start b-end mail-buffer
	  (lambda (send-mail message-pathname)
	    message-pathname
	    (send-mail)))))))

(define (prepare-mail-buffer-for-sending mail-buffer receiver)
  (guarantee-newline (buffer-end mail-buffer))
  (call-with-temporary-buffer " sendmail header"
    (lambda (h-buffer)
      (let ((m (mail-match-header-separator mail-buffer)))
	(let ((b-start
	       (mark-right-inserting-copy
		(line-start (re-match-end 0) 1 'LIMIT)))
	      (b-end (mark-left-inserting-copy (buffer-end mail-buffer)))
	      (h-start (mark-right-inserting-copy (buffer-start h-buffer)))
	      (h-end (mark-left-inserting-copy (buffer-start h-buffer))))
	  (delete-string h-start h-end)
	  (insert-region (buffer-start mail-buffer) m h-end)
	  (guarantee-newline h-end)
	  ;; Delete any blank lines in the header.
	  (do ((h-start h-start (replace-match "\n")))
	      ((not (re-search-forward "\n\n+" h-start h-end #f))))
	  ;; Delete a blank subject line.
	  (if (re-search-forward "^Subject:[ \t]*\n" h-start h-end #t)
	      (delete-match))
	  (expand-mail-aliases h-start h-end)
	  (let ((add-field
		 (lambda (name value)
		   (if (and value (not (mail-field-start h-start h-end name)))
		       (mail-insert-field-value h-end name value)))))
	    (add-field "Date" (universal-time->string (get-universal-time)))
	    ;; If there is a From and no Sender, put in a Sender.
	    (add-field (if (mail-field-start h-start h-end "From")
			   "Sender"
			   "From")
		       (mail-from-string mail-buffer))
	    (add-field "Organization" (mail-organization-string mail-buffer))
	    (add-field "User-Agent" (mailer-version-string mail-buffer)))
	  (let ((v (receiver h-start h-end b-start b-end)))
	    (mark-temporary! h-start)
	    (mark-temporary! h-end)
	    (mark-temporary! b-start)
	    (mark-temporary! b-end)
	    v))))))

(define (mail-organization-string buffer)
  (let ((organization (ref-variable mail-organization buffer)))
    (and (not (string-null? organization))
	 organization)))

(define (mailer-version-string buffer)
  (and (ref-variable mail-identify-reader buffer)
       (let ((generic
	      (string-append "Edwin/"
			     (get-subsystem-version-string "edwin")
			     "; MIT-Scheme/"
			     (get-subsystem-version-string "release")))
	     (method
	      (or (buffer-get buffer 'MAILER-VERSION-STRING #f)
		  global-mailer-version-string)))
	 (if method
	     (method generic)
	     generic))))

(define global-mailer-version-string #f)

(define (finish-preparing-mail-buffer h-start h-end b-start b-end
				      lookup-context receiver)
  (if (buffer-mime-processing-enabled? (mark-buffer b-start))
      (begin
	(guarantee-mime-compliant-headers h-start h-end)
	(delete-mime-headers! h-start h-end)))
  (let ((fcc-pathnames
	 (if (mail-field-start h-start h-end "FCC")
	     (compute-fcc-pathnames h-start h-end)
	     '())))
    (call-with-temporary-file-pathname
     (lambda (message-pathname)
       (receiver
	(if (ref-variable mail-relay-host lookup-context)
	    (let ((recipients (compute-message-recipients h-start h-end)))
	      (write-message-file h-start h-end b-start b-end message-pathname)
	      (write-fcc-messages fcc-pathnames
				  message-pathname
				  lookup-context)
	      (lambda ()
		(send-mail-using-smtp message-pathname
				      recipients
				      lookup-context)))
	    (begin
	      (write-message-file h-start h-end b-start b-end message-pathname)
	      (write-fcc-messages fcc-pathnames
				  message-pathname
				  lookup-context)
	      (lambda ()
		(send-mail-using-sendmail message-pathname lookup-context))))
	message-pathname)))))

(define (write-message-file h-start h-end b-start b-end message-pathname)
  (call-with-output-file message-pathname
    (lambda (port)
      (write-region-to-port h-start h-end port)
      (if (buffer-mime-processing-enabled? (mark-buffer b-start))
	  (write-mime-message-body b-start b-end port)
	  (begin
	    (newline port)
	    (write-region-to-port b-start b-end port)
	    (fresh-line port))))))

(define (write-region-to-port start end port)
  (group-write-to-port (mark-group start)
		       (mark-index start)
		       (mark-index end)
		       port))

(define (compute-fcc-pathnames h-start h-end)
  (let ((m (mark-right-inserting-copy h-start)))
    (let loop ((pathnames '()))
      (if (re-search-forward "^FCC:[ \t]*\\([^ \t\n]+\\)" m h-end #t)
	  (let ((filename
		 (extract-string (re-match-start 1) (re-match-end 1))))
	    (move-mark-to! m (line-start (re-match-start 0) 0))
	    (delete-string m (line-start m 1))
	    (loop
	     (cons (merge-pathnames filename (user-homedir-pathname))
		   pathnames)))
	  (begin
	    (mark-temporary! m)
	    pathnames)))))

(define (write-fcc-messages pathnames message-pathname lookup-context)
  (for-each
   (let ((append-message
	  (let ((header-line
		 (string-append
		  "From "
		  (user-mail-address lookup-context)
		  " "
		  (universal-time->local-ctime-string (get-universal-time)))))
	    (lambda (length port)
	      (if (> length 0)
		  (newline port))
	      (write-string header-line port)
	      (newline port)
	      (call-with-input-file message-pathname
		(lambda (input)
		  (let loop ()
		    (let ((line (read-line input)))
		      (if (not (eof-object? line))
			  (begin
			    ;; ``Quote'' "^From " as ">From "
			    ;; (note that this isn't really quoting,
			    ;; as there is no requirement that
			    ;; "^[>]+From " be quoted in the same
			    ;; transparent way.)
			    (if (string-prefix-ci? "from " line)
				(write-char #\> port))
			    (write-string line port)
			    (newline port)
			    (loop)))))))
	      (newline port)))))
     (lambda (pathname)
       (let ((buffer (pathname->buffer pathname)))
	 (if buffer
	     (call-with-output-mark (buffer-end buffer)
	       (lambda (port)
		 (append-message (buffer-length buffer) port)))
	     (call-with-append-file pathname
	       (lambda (port)
		 (append-message ((port/operation port 'LENGTH) port)
				 port)))))))
   pathnames))

(define (compute-message-recipients h-start h-end)
  (receive (regexp prefix)
      (if (mail-field-start h-start h-end "resent-to")
	  (values "^\\(resent-to:\\|resent-cc:\\|resent-bcc:\\)[ \t]*"
		  "resent-bcc:")
	  (values "^\\(to:\\|cc:\\|bcc:\\)[ \t]*" "bcc:"))
    (let loop ((start h-start) (addresses '()))
      (let ((f-start (re-search-forward regexp start h-end #t)))
	(if f-start
	    (let* ((f-end (%mail-field-end f-start h-end))
		   (addresses
		    (append (rfc822:string->addresses
			     (extract-string f-start f-end))
			    addresses))
		   (ls (line-start f-start 0)))
	      (if (match-forward prefix ls h-end #t)
		  (begin
		    (delete-string ls (mark1+ f-end 'LIMIT))
		    (loop ls addresses))
		  (loop f-end addresses)))
	    addresses)))))

;;;; Sendmail transmission

(define (send-mail-using-sendmail message-pathname lookup-context)
  (message "Sending...")
  (let ((program
	 (or (ref-variable sendmail-program lookup-context)
	     (os/sendmail-program))))
    (if (ref-variable mail-interactive lookup-context)
	(call-with-temporary-buffer " sendmail errors"
	  (lambda (error-buffer)
	    (let ((error-port (mark->output-port (buffer-end error-buffer))))
	      (run-synchronous-process-1 error-port
		(lambda ()
		  (run-shell-command
		   (string-append program
				  " -oi -t"
				  ;; Always specify who from, since
				  ;; some systems have broken
				  ;; sendmails.
				  " -f" (current-user-name)
				  " < " (->namestring message-pathname))
		   'OUTPUT error-port)))
	      (close-port error-port))
	    (let ((end (buffer-end error-buffer)))
	      (do ((start (buffer-start error-buffer) (replace-match "; ")))
		  ((not (re-search-forward "\n+ *" start end #f)))))
	    (let ((errors (buffer-string error-buffer)))
	      (if (not (string-null? errors))
		  (editor-error "Sending...failed to " errors)))))
	;; If we aren't going to look at the errors, run the program
	;; in the background so control returns to the user as soon as
	;; possible.
	(run-shell-command
	 (string-append program
			" -oi -t"
			;; Always specify who from, since some systems
			;; have broken sendmails.
			" -f" (current-user-name)
			;; These mean "report errors by mail" and
			;; "deliver in background".
			" -oem -odb"
			" < " (->namestring message-pathname))
	 'OUTPUT #f)))
  (message "Sending...done"))

;;;; Direct SMTP transmission

(define (send-mail-using-smtp message-pathname recipients lookup-context)
  (message "Sending...")
  (let ((from
	 (rfc822:canonicalize-address-string
	  (mail-from-string lookup-context)))
	(trace-buffer
	 (and (ref-variable smtp-trace lookup-context)
	      (temporary-buffer "*SMTP-trace*")))
	(require-valid?
	 (ref-variable smtp-require-valid-recipients lookup-context))
	(valid-response?
	 (lambda (response) (= 250 (smtp-response-number response)))))
    (if (null? recipients)
	(editor-error "No recipients specified for mail."))
    (let ((responses
	   (call-with-smtp-socket (ref-variable mail-relay-host lookup-context)
                                  (ref-variable mail-relay-service
                                                lookup-context)
				  trace-buffer
	     (lambda (port banner)
	       banner
	       (smtp-command/helo port)
	       (smtp-command/mail port from)
	       (let ((responses
		      (map (lambda (recipient)
			     (smtp-command/rcpt port recipient))
			   recipients)))
		 (if (if require-valid?
			 (for-all? responses valid-response?)
			 (there-exists? responses valid-response?))
		     (smtp-command/data port message-pathname)
		     (smtp-command/rset port))
		 (smtp-command/quit port)
		 responses)))))
      (cond ((not (for-all? responses valid-response?))
	     (pop-up-temporary-buffer "*SMTP-invalid*"
				      '(READ-ONLY FLUSH-ON-SPACE)
	       (lambda (buffer window)
		 window
		 (let ((m (mark-left-inserting-copy (buffer-start buffer))))
		   (for-each (lambda (recipient response)
			       (if (not (valid-response? response))
				   (begin
				     (insert-string recipient m)
				     (insert-char #\tab m)
				     (insert-string response m)
				     (insert-newline m))))
			     recipients responses)
		   (mark-temporary! m)))))
	    (trace-buffer
	     (set-buffer-point! trace-buffer (buffer-start trace-buffer))
	     (buffer-not-modified! trace-buffer)
	     (pop-up-buffer trace-buffer #f)))
      (message "Sending..."
	       (if (if require-valid?
		       (for-all? responses valid-response?)
		       (there-exists? responses valid-response?))
		   "done"
		   "aborted")))))

(define (call-with-smtp-socket host-name service trace-buffer receiver)
  (let ((port #f))
    (dynamic-wind
     (lambda ()
       (set! port
	     (make-smtp-port (open-tcp-stream-socket host-name
                                                     (or service "smtp"))
			     trace-buffer))
       unspecific)
     (lambda ()
       (receiver port (smtp-read-response port 220)))
     (lambda ()
       (if port
	   (begin
	     (close-port (smtp-port-port port))
	     (set! port #f)
	     unspecific))))))

(define-structure smtp-port
  (port #f read-only #t)
  (trace-buffer #f read-only #t))

(define (smtp-read-line port)
  (let ((line (read-line (smtp-port-port port))))
    (smtp-trace-write-string line port)
    (smtp-trace-newline port)
    line))

(define (smtp-write-line port . strings)
  (for-each (lambda (string)
	      (smtp-trace-write-string string port)
	      (write-string string (smtp-port-port port)))
	    strings)
  (smtp-trace-newline port)
  (newline (smtp-port-port port)))

(define (smtp-drain-output port)
  (flush-output (smtp-port-port port)))

(define (smtp-trace-write-string string port)
  (let ((trace-buffer (smtp-port-trace-buffer port)))
    (if trace-buffer
	(insert-string string (buffer-end trace-buffer)))))

(define (smtp-trace-newline port)
  (let ((trace-buffer (smtp-port-trace-buffer port)))
    (if trace-buffer
	(insert-newline (buffer-end trace-buffer)))))

(define (smtp-command/helo port)
  (smtp-write-line port "HELO " (os/hostname))
  (smtp-read-response port 250))

(define (smtp-command/mail port from)
  (smtp-write-line port "MAIL FROM:<" from ">")
  (smtp-read-response port 250))

(define (smtp-command/rcpt port recipient)
  (smtp-write-line port "RCPT TO:<" recipient ">")
  (smtp-read-response port 250 550))

(define (smtp-command/data port message-pathname)
  (smtp-write-line port "DATA")
  (smtp-read-response port 354)
  (call-with-input-file message-pathname
    (lambda (input)
      (let loop ()
	(let ((line (read-line input)))
	  (if (not (eof-object? line))
	      (begin
		(if (and (fix:> 0 (string-length line))
			 (char=? #\. (string-ref line 0)))
		    (smtp-write-line port "." line)
		    (smtp-write-line port line))
		(loop)))))))
  (smtp-write-line port ".")
  (smtp-read-response port 250))

(define (smtp-command/rset port)
  (smtp-write-line port "RSET")
  (smtp-read-response port 250))

(define (smtp-command/quit port)
  (smtp-write-line port "QUIT")
  (smtp-read-response port 221))

(define (smtp-read-response port . numbers)
  (smtp-drain-output port)
  (let ((response (smtp-read-line port)))
    (let ((n (smtp-response-number response)))
      (if (not (there-exists? numbers (lambda (n*) (= n n*))))
	  (editor-error response))
      (if (smtp-response-continued? response)
	  (let loop ((responses (list response)))
	    (let ((response (smtp-read-line port)))
	      (if (not (= n (smtp-response-number response)))
		  (error "Mismatched codes in multiline response:" n response))
	      (let ((responses (cons response responses)))
		(if (smtp-response-continued? response)
		    (loop responses)
		    (convert-smtp-multiline-response (reverse! responses))))))
	  response))))

(define (smtp-response-number line)
  (or (and (fix:>= (string-length line) 3)
	   (substring->nonnegative-integer line 0 3))
      (error "Malformed SMTP response:" line)))

(define (smtp-response-continued? line)
  (and (fix:>= (string-length line) 4)
       (char=? #\- (string-ref line 3))))

(define (convert-smtp-multiline-response responses)
  (apply string-append
	 (cons* (string-head (car responses) 3)
		" "
		(let ((lines
		       (map (lambda (response) (string-tail response 4))
			    responses)))
		  (cons (car lines)
			(append-map (lambda (line) (list "\n" line))
				    lines))))))

;;;; MIME

(define (write-mime-message-body b-start b-end port)
  (write-message-header-field "MIME-Version" "1.0" port)
  (let ((attachments (buffer-mime-attachments (mark-buffer b-start))))
    (if (null? attachments)
	(write-mime-message-body-1 b-start b-end #f port)
	(write-mime-message-body-with-attachments b-start b-end attachments
						  port))))

(define (write-message-header-field name value port)
  (write-string name port)
  (write-string ": " port)
  (write-string value port)
  (newline port))

(define (write-mime-message-body-1 b-start b-end subpart? port)
  (if (not (and subpart? (ref-variable mail-abbreviate-mime b-start)))
      (write-message-header-field "Content-Type"
				  "text/plain; charset=us-ascii"
				  port))
  (if (or (any-non-us-ascii-chars? b-start b-end)
	  (any-lines-too-long? b-start b-end 76))
      (begin
	(write-message-header-field "Content-Transfer-Encoding"
				    "quoted-printable"
				    port)
	(newline port)
	(let ((context (encode-quoted-printable:initialize port #t)))
	  (%group-write (mark-group b-start)
			(mark-index b-start)
			(mark-index b-end)
	    (lambda (string start end)
	      (encode-quoted-printable:update
               context
               (xsubstring string 0 (xstring-length string))
               start
               end)))
	  (encode-quoted-printable:finalize context)))
      (begin
	(if (not (and subpart? (ref-variable mail-abbreviate-mime b-start)))
	    (write-message-header-field "Content-Transfer-Encoding"
					"7bit"
					port))
	(newline port)
	(write-region-to-port b-start b-end port))))

(define (any-non-us-ascii-chars? start end)
  (group-find-next-char-in-set (mark-group start)
			       (mark-index start)
			       (mark-index end)
			       char-set:non-us-ascii))

(define (any-lines-too-long? start end n)
  (let loop ((ls (line-start start 0)))
    (let ((le (line-end ls 0)))
      (or (> (- (mark-index le) (mark-index ls)) n)
	  (let ((ls (line-start le 1 #f)))
	    (and ls
		 (mark< ls end)
		 (loop ls)))))))

(define char-set:us-ascii
  (char-set-union char-set:graphic (char-set #\tab #\page #\linefeed)))

(define char-set:non-us-ascii
  (char-set-invert char-set:us-ascii))

(define regexp:non-us-ascii
  (char-set->regexp char-set:non-us-ascii))

(define (write-mime-message-body-with-attachments b-start b-end attachments
						  port)
  (let ((boundary (random-mime-boundary-string 32)))
    (write-message-header-field "Content-Type"
				(string-append "multipart/mixed; boundary=\""
					       boundary
					       "\"")
				port)
    (write-message-header-field "Content-Transfer-Encoding" "7bit" port)
    (newline port)
    (write-string "This is a multi-part message in MIME format." port)
    (write-mime-boundary boundary #f port)
    (write-mime-message-body-1 b-start b-end #t port)
    (for-each (lambda (attachment)
		(write-mime-boundary boundary #f port)
		(write-mime-attachment attachment b-start port))
	      attachments)
    (write-mime-boundary boundary #t port)))

(define (write-mime-boundary boundary final? port)
  (newline port)
  (write-string "--" port)
  (write-string boundary port)
  (if final? (write-string "--" port))
  (newline port))

(define (write-mime-attachment attachment lookup-context port)
  (let ((type (mime-attachment-type attachment))
	(subtype (mime-attachment-subtype attachment)))
    (write-message-header-field
     "Content-Type"
     (string-append (symbol-name type)
		    "/"
		    (symbol-name subtype)
		    (mime-parameters->string
		     (mime-attachment-parameters attachment)))
     port)
    (if (and (eq? type 'MESSAGE) (eq? subtype 'RFC822))
	(if (not (ref-variable mail-abbreviate-mime lookup-context))
	    (write-message-header-field "Content-Transfer-Encoding"
					"7bit"
					port))
	(write-message-header-field "Content-Transfer-Encoding"
				    (if (eq? type 'TEXT)
					"quoted-printable"
					"base64")
				    port))
    (let ((disposition (mime-attachment-disposition attachment)))
      (if disposition
	  (write-message-header-field "Content-Disposition"
				      (mime-disposition->string disposition)
				      port)))
    (newline port)
    (if (and (eq? type 'MESSAGE) (eq? subtype 'RFC822))
	(begin
	  (for-each (lambda (nv)
		      (write-message-header-field (car nv) (cadr nv) port))
		    (mime-attachment-message-headers attachment))
	  (newline port)
	  ((mime-attachment-message-body-generator attachment) port))
	(receive (initialize update finalize text?)
	    (if (eq? type 'TEXT)
		(values encode-quoted-printable:initialize
			encode-quoted-printable:update
			encode-quoted-printable:finalize
			#t)
		(values encode-base64:initialize
			encode-base64:update
			encode-base64:finalize
			#f))
	  (let ((context (initialize port text?)))
	    ((if (eq? type 'TEXT)
		 call-with-input-file
		 call-with-binary-input-file)
	     (mime-attachment-pathname attachment)
	     (lambda (input-port)
	       (let ((buffer (make-string 4096)))
		 (let loop ()
		   (let ((n-read (read-string! buffer input-port)))
		     (if (> n-read 0)
			 (begin
			   (update context buffer 0 n-read)
			   (loop))))))))
	    (finalize context))))))

(define (enable-buffer-mime-processing! buffer)
  (buffer-remove! buffer 'MAIL-DISABLE-MIME-PROCESSING))

(define (disable-buffer-mime-processing! buffer)
  (buffer-put! buffer 'MAIL-DISABLE-MIME-PROCESSING #t))

(define (buffer-mime-processing-enabled? buffer)
  (not (buffer-get buffer 'MAIL-DISABLE-MIME-PROCESSING #f)))

(define (add-buffer-mime-attachment! buffer mime-type parameters disposition
				     . rest)
  (let ((attachment
	 (list->vector
	  (cons* (mime-type/top-level mime-type)
		 (mime-type/subtype mime-type)
		 parameters
		 disposition
		 rest))))
    (set-buffer-mime-attachments! buffer
				  (append (buffer-mime-attachments buffer)
					  (list attachment)))
    attachment))

(define (delete-buffer-mime-attachment! buffer attachment)
  (set-buffer-mime-attachments! buffer
				(delq! attachment
				       (buffer-mime-attachments buffer))))

(define (buffer-mime-attachments buffer)
  (buffer-get buffer 'MAIL-MIME-ATTACHMENTS '()))

(define (set-buffer-mime-attachments! buffer attachments)
  (buffer-put! buffer 'MAIL-MIME-ATTACHMENTS attachments)
  (local-set-variable! mode-line-process
		       (let ((n (length attachments)))
			 (and (> n 0)
			     (string-append
			      " ("
			      (number->string n)
			      " attachment"
			      (if (> n 1) "s" "")
			      ")")))
		       buffer)
  (buffer-modeline-event! buffer 'PROCESS-STATUS))

(define-integrable (mime-attachment-type attachment)
  (vector-ref attachment 0))

(define-integrable (mime-attachment-subtype attachment)
  (vector-ref attachment 1))

(define-integrable (mime-attachment-parameters attachment)
  (vector-ref attachment 2))

(define-integrable (mime-attachment-disposition attachment)
  (vector-ref attachment 3))

(define-integrable (mime-attachment-message-headers attachment)
  (vector-ref attachment 4))

(define-integrable (mime-attachment-message-body-generator attachment)
  (vector-ref attachment 5))

(define-integrable (mime-attachment-pathname attachment)
  (vector-ref attachment 4))

(define (mime-parameters->string parameters)
  (decorated-string-append
   "; " "" ""
   (map (lambda (parameter)
	  (string-append (symbol-name (car parameter))
			 "=\""
			 (cadr parameter)
			 "\""))
	parameters)))

(define (mime-disposition->string disposition)
  (string-append (symbol-name (car disposition))
		 (mime-parameters->string (cdr disposition))))

(define (guarantee-mime-compliant-headers h-start h-end)
  (if (any-non-us-ascii-chars? h-start h-end)
      (begin
	(pop-up-occur-buffer h-start h-end regexp:non-us-ascii #f)
	(editor-error "Message contains illegal characters in header.")))
  (if (any-lines-too-long? h-start h-end 998)
      (editor-error "Message contains over-long line in header.")))

(define (delete-mime-headers! h-start h-end)
  (let loop ((f-start h-start))
    (if (mark< f-start h-end)
	(let ((colon (search-forward ":" f-start (line-end f-start 0))))
	  (if (not colon)
	      (error "Not a header-field line:" f-start))
	  (let ((name (string-trim (extract-string f-start (mark-1+ colon))))
		(f-start*
		 (if (re-search-forward "^[^ \t]" colon h-end #f)
		     (re-match-start 0)
		     h-end)))
	    (if (or (string-ci=? "mime-version" name)
		    (string-prefix-ci? "content-" name))
		(begin
		  (delete-string f-start f-start*)
		  (loop f-start))
		(loop f-start*)))))))

(define (random-mime-boundary-string length)
  (if (not (exact-nonnegative-integer? length))
      (error:wrong-type-argument length "exact nonnegative integer"
				 'RANDOM-MIME-BOUNDARY-STRING))
  (let* ((prefix "=_")
	 (plen (string-length prefix)))
    (if (not (<= 1 length (- 70 plen)))
	(error:bad-range-argument length 'RANDOM-MIME-BOUNDARY-STRING))
    (let ((s
	   (call-with-output-string
	    (lambda (port)
	      (write-string prefix port)
	      (let ((context (encode-base64:initialize port #f)))
		(let ((n (* (integer-ceiling (- length 2) 4) 3)))
		  (encode-base64:update context (random-byte-vector n) 0 n))
		(encode-base64:finalize context)))))
	  (n (+ plen length)))
      (if (fix:> (string-length s) n)
	  (set-string-maximum-length! s n))
      s)))

;;;; Attachment browser

(define-command mail-browse-attachments
  "Visit a buffer showing a list of the MIME attachments for this message.
You can add and delete attachments from that buffer."
  ()
  (lambda ()
    (select-buffer (mail-mime-attachments-browser (selected-buffer)))))

(define (mail-mime-attachments-browser mail-buffer)
  (let ((buffer (get-mime-attachments-buffer mail-buffer #t)))
    (rebuild-mime-attachments-buffer buffer)
    buffer))

(define (get-mime-attachments-buffer mail-buffer intern?)
  (or (let ((buffer
	     (buffer-get mail-buffer 'MIME-ATTACHMENTS-BROWSER #f)))
	(and buffer
	     (if (buffer-alive? buffer)
		 buffer
		 (begin
		   (buffer-remove! mail-buffer 'MIME-ATTACHMENTS-BROWSER)
		   #f))))
      (and intern?
	   (let ((buffer
		  (new-buffer
		   (string-append (buffer-name mail-buffer)
				  "-attachments"))))
	     (buffer-put! mail-buffer 'MIME-ATTACHMENTS-BROWSER buffer)
	     (buffer-put! buffer 'MAIL-BUFFER mail-buffer)
	     buffer))))

(define (rebuild-mime-attachments-buffer buffer)
  (buffer-widen! buffer)
  (with-read-only-defeated (buffer-start buffer)
    (lambda ()
      (fill-mime-attachments-buffer buffer)))
  (set-buffer-major-mode! buffer (ref-mode-object mime-attachments))
  (buffer-not-modified! buffer)
  (set-buffer-point! buffer (line-start (buffer-start buffer) 2 'ERROR)))

(define (fill-mime-attachments-buffer buffer)
  (let ((mail-buffer (buffer-get buffer 'MAIL-BUFFER #f)))
    (if (not (and mail-buffer (buffer-alive? mail-buffer)))
	(error "Missing mail buffer:" buffer))
    (region-delete! (buffer-region buffer))
    (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
      (insert-string-pad-right "Type" 30 #\space mark)
      (insert-char #\space mark)
      (insert-string "Filename" mark)
      (insert-newline mark)
      (insert-chars #\- 30 mark)
      (insert-char #\space mark)
      (insert-chars #\-
		    (max 8 (- (mark-x-size mark) (+ (mark-column mark) 1)))
		    mark)
      (insert-newline mark)
      (for-each (lambda (attachment)
		  (write-mime-attachment-line attachment mark))
		(buffer-mime-attachments mail-buffer))
      (mark-temporary! mark))))

(define (write-mime-attachment-line attachment mark)
  (let ((start (mark-right-inserting-copy mark))
	(type (mime-attachment-type attachment))
	(subtype (mime-attachment-subtype attachment)))
    (insert-string-pad-right (string-append (symbol-name type)
					    "/"
					    (symbol-name subtype))
			     30 #\space mark)
    (if (not (and (eq? type 'MESSAGE) (eq? subtype 'RFC822)))
	(begin
	  (insert-char #\space mark)
	  (insert-string
	   (->namestring (mime-attachment-pathname attachment))
	   mark)))
    (insert-newline mark)
    (region-put! start mark 'MIME-ATTACHMENT attachment)
    (mark-temporary! start)))

(define-major-mode mime-attachments read-only "MIME Attachments"
  "Major mode for browsing MIME mail attachments.
Commands available in this mode:

\\{mime-attachments}"
  (lambda (buffer)
    (buffer-put! buffer 'REVERT-BUFFER-METHOD mime-attachments-revert-buffer)
    (add-kill-buffer-hook buffer mime-attachments-kill-buffer)
    (local-set-variable! truncate-lines #t buffer)
    (local-set-variable! mode-line-modified "--- " buffer)
    (set-buffer-read-only! buffer)
    (disable-group-undo! (buffer-group buffer))
    (event-distributor/invoke! (ref-variable mime-attachments-mode-hook buffer)
			       buffer)))

(define-variable mime-attachments-mode-hook
  "An event distributor that is invoked when entering MIME Attachments mode."
  (make-event-distributor))

(define-key 'mime-attachments #\a 'add-mime-file-attachment)
(define-key 'mime-attachments #\d 'kill-mime-attachment)
(define-key 'mime-attachments #\k 'kill-mime-attachment)
(define-key 'mime-attachments #\? 'describe-mode)
(define-key 'mime-attachments #\q 'mime-attachments-quit)
(define-key 'mime-attachments '(#\c-c #\c-c) 'mime-attachments-quit)

(define (mime-attachments-revert-buffer buffer
					dont-use-auto-save? dont-confirm?)
  dont-use-auto-save?
  (if (or dont-confirm? (prompt-for-yes-or-no? "Revert attachments buffer"))
      (rebuild-mime-attachments-buffer buffer)))

(define (mime-attachments-kill-buffer buffer)
  (let ((mail-buffer (buffer-get buffer 'MAIL-BUFFER #f)))
    (if mail-buffer
	(buffer-remove! mail-buffer 'MIME-ATTACHMENTS-BROWSER))))

(define (selected-mail-buffer)
  (let ((buffer (selected-buffer)))
    (or (buffer-get buffer 'MAIL-BUFFER #f)
	buffer)))

(define-command add-mime-file-attachment
  "Add a file as a MIME attachment to the current mail message.
With prefix argument, allows you to specify the MIME type of the file.
Otherwise, the MIME type is determined from the file's suffix;
 if the suffix is unknown, you may choose a generic text or binary type."
  "FFile to attach\nP"
  (lambda (pathname argument)
    (let ((mail-buffer (selected-mail-buffer)))
      (let ((attachment
	     (receive (mime-type parameters)
		 (pathname->mime-type pathname mail-buffer argument)
	       (add-buffer-mime-attachment!
		mail-buffer
		mime-type
		`(,@parameters
		  (NAME ,(pathname-name pathname)))
		`(ATTACHMENT (FILENAME ,(file-namestring pathname)))
		pathname))))
	(let ((buffer (get-mime-attachments-buffer mail-buffer #f)))
	  (if buffer
	      (let ((mark (mark-left-inserting-copy (buffer-end buffer))))
		(with-read-only-defeated mark
		  (lambda ()
		    (write-mime-attachment-line attachment mark)))
		(mark-temporary! mark))))))))

(define-command kill-mime-attachment
  "Delete the MIME attachment that point is on."
  ()
  (lambda ()
    (let ((point (current-point)))
      (let ((attachment (region-get point 'MIME-ATTACHMENT #f)))
	(if (not attachment)
	    (editor-error "No attachment on current line."))
	(if (prompt-for-yes-or-no? "Delete attachment")
	    (begin
	      (delete-buffer-mime-attachment! (selected-mail-buffer)
					      attachment)
	      (with-read-only-defeated point
		(lambda ()
		  (delete-string (line-start point 0)
				 (line-start point 1 'ERROR))))))))))

(define-command mime-attachments-quit
  "Delete the MIME attachments buffer, returning to the message buffer."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (let ((mail-buffer (buffer-get buffer 'MAIL-BUFFER #f)))
	(if (not mail-buffer)
	    (editor-error "No mail buffer found!"))
	(select-buffer mail-buffer))
      (kill-buffer-interactive buffer))))

(define (pathname->mime-type pathname buffer prompt?)
  (let ((mime-type
	 (let ((type (pathname-type pathname)))
	   (let ((do-mime
		  (lambda ()
		    (let ((type
			   (prompt-for-alist-value "MIME type"
						   mime-top-level-types
						   #f
						   #t)))
		      (make-mime-type
		       type
		       (string->symbol
			(prompt-for-string "MIME subtype" #f)))))))
	     (if prompt?
		 (do-mime)
		 (let ((entry
			(find-matching-item
			    (ref-variable file-type-to-mime-type buffer)
			  (lambda (entry)
			    (cond ((string? type)
				   (string-ci=? (car entry) type))
				  ((not type)
				   (not (car entry)))
				  (else
				   (eq? type 'WILD)))))))
		   (cond (entry (make-mime-type (cadr entry) (caddr entry)))
			 ((pathname-mime-type pathname))
			 (else
			  (let loop ()
			    (case (prompt-for-char
				   "File type (T=text, B=binary, M=MIME)")
			      ((#\t #\T) (make-mime-type 'TEXT 'PLAIN))
			      ((#\b #\B) (make-mime-type 'APPLICATION
							 'OCTET-STREAM))
			      ((#\m #\M) (do-mime))
			      (else (editor-beep) (loop))))))))))))
    (values mime-type
	    (if (eq? (mime-type/top-level mime-type) 'TEXT)
		'((CHARSET "iso-8859-1"))
		'()))))

(define-variable file-type-to-mime-type
  "Specifies the MIME type/subtype for files with a given type.
This is a list, each element of which is a list of three items:
1. The file type as a string, e.g. \"jpg\".
   This can also be #F for files with no type.
2. The MIME type, one of the following symbols:
      TEXT IMAGE AUDIO VIDEO APPLICATION
3. The MIME subtype, also specified as a symbol."
  '(("scm" TEXT X-SCHEME)
    ("text" TEXT PLAIN)
    ("txi" TEXT X-TEXINFO))
  (lambda (x)
    (list-of-type? x
      (lambda (x)
	(and (list? x)
	     (= (length x) 3)
	     (or (not (car x)) (string? (car x)))
	     (there-exists? mime-top-level-types
	       (lambda (e)
		 (eq? (cdr e) (cadr x))))
	     (symbol? (caddr x)))))))

(define mime-top-level-types
  (map (lambda (s) (cons (symbol->string s) s))
       '(TEXT IMAGE AUDIO VIDEO APPLICATION)))