#| -*-Scheme-*-

$Id: filcom.scm,v 1.228 2007/01/05 21:19:23 cph Exp $

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

;;;; File Commands


(define (find-file filename)
  (select-buffer (find-file-noselect filename #t)))

(define-command find-file
  "Visit a file in its own buffer.
If the file is already in some buffer, select that buffer.
Otherwise, visit the file in a buffer named after the file."
  "FFind file"
  find-file)

(define (find-file-other-window filename)
  (select-buffer-other-window (find-file-noselect filename #t)))

(define-command find-file-other-window
  "Visit a file in another window.
May create a window, or reuse one."
  "FFind file in other window"
  find-file-other-window)

(define (find-file-other-screen filename)
  (select-buffer-other-screen (find-file-noselect filename #t)))

(define-command find-file-other-frame
  "Visit a file in another frame."
  "FFind file in other frame"
  find-file-other-screen)
(define edwin-command$find-file-other-screen
  edwin-command$find-file-other-frame)

(define-command find-alternate-file
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want."
  (lambda ()
    (list
     (prompt-for-file
      "Find alternate file"
      (let ((pathname (buffer-pathname (selected-buffer))))
	(and pathname
	     (list pathname))))))
  (lambda (filename)
    (let ((buffer (selected-buffer)))
      (let ((do-it
	     (lambda ()
	       (kill-buffer-interactive buffer)
	       (find-file filename))))
	(if (other-buffer buffer)
	    (do-it)
	    (let ((buffer* (new-buffer "*dummy*")))
	      (do-it)
	      (kill-buffer buffer*)))))))

(define-variable find-file-run-dired
  "True says run dired if find-file is given the name of a directory."
  #t
  boolean?)

(define-variable find-file-not-found-hooks
  "List of procedures to be called for find-file on nonexistent file.
These functions are called as soon as the error is detected.
The functions are called in the order given,
until one of them returns non-false."
  '()
  (lambda (object)
    (list-of-type? object
      (lambda (object)
	(and (procedure? object)
	     (procedure-arity-valid? object 1))))))

(define-variable find-file-hooks
  "List of procedures to be called after a buffer is loaded from a file.
The buffer's local variables (if any) will have been processed before the
procedures are called."
  '()
  (lambda (object)
    (list-of-type? object
      (lambda (object)
	(and (procedure? object)
	     (procedure-arity-valid? object 1))))))

(define (find-file-noselect filename warn?)
  (let ((pathname (pathname-simplify (merge-pathnames filename))))
    (if (file-test-no-errors file-directory? pathname)
	(if (ref-variable find-file-run-dired)
	    (make-dired-buffer (pathname-as-directory pathname))
	    (editor-error (->namestring pathname) " is a directory."))
	(let ((buffer (pathname->buffer pathname)))
	  (if buffer
	      (if warn?
		  (find-file-revert buffer)
		  buffer)
	      (let ((buffer (new-buffer (pathname->buffer-name pathname))))
		(let ((error?
		       (not
			(catch-file-errors
			 (lambda (condition) condition #f)
			 (lambda () (read-buffer buffer pathname #t))))))
		  (if error?
		      (do ((hooks
			    (ref-variable find-file-not-found-hooks buffer)
			    (cdr hooks)))
			  ((or (null? hooks)
			       ((car hooks) buffer))))
		      (maybe-change-buffer-name! buffer pathname))
		  (after-find-file buffer error? warn?))))))))

(define (maybe-change-buffer-name! buffer pathname)
  (let ((name (pathname->buffer-name pathname))
	(name* (pathname->buffer-name (buffer-pathname buffer))))
    (if (not (string=? name name*))
	(rename-buffer buffer (new-buffer-name name*)))))

(define (after-find-file buffer error? warn?)
  (let ((pathname (or (buffer-truename buffer) (buffer-pathname buffer))))
    (let ((buffer-read-only?
	   (not (file-test-no-errors file-writeable? pathname))))
      (if buffer-read-only?
	  (set-buffer-read-only! buffer)
	  (set-buffer-writeable! buffer))
      (setup-buffer-auto-save! buffer)
      (let ((serious-message
	     (lambda (msg)
	       (message msg)
	       (sit-for 1))))
	(cond ((not buffer-read-only?)
	       (cond ((and warn?
			   (let ((asp (buffer-auto-save-pathname buffer)))
			     (and asp
				  (file-newer-than-file? asp pathname))))
		      (serious-message
		       "Auto save file is newer; consider M-x recover-file"))
		     (error?
		      (message "(New file)"))))
	      ((not error?)
	       (message "File is write protected"))
	      (else
	       (serious-message
		(if (file-test-no-errors
		     (lambda (pathname) (file-access pathname 0))
		     pathname)
		    "File exists, but is read-protected."
		    (string-append
		     "File not found and directory "
		     (let ((directory
			    (directory-pathname-as-file
			     (directory-pathname
			      (buffer-pathname buffer)))))
		       (if (file-test-no-errors file-exists? directory)
			   "write-protected"
			   "doesn't exist")))))))))
    (normal-mode buffer #t)
    (load-find-file-initialization buffer pathname)
    (let loop ((hooks (ref-variable find-file-hooks buffer)) (buffer buffer))
      (if (pair? hooks)
	  (loop (cdr hooks) ((car hooks) buffer))
	  buffer))))

(define (file-test-no-errors test . args)
  (catch-file-errors (lambda (condition) condition #f)
		     (lambda () (apply test args))))

(define (file-newer-than-file? a b)
  (let ((a (file-modification-time-indirect a)))
    (and a
	 (let ((b (file-modification-time-indirect b)))
	   (or (not b)
	       (> a b))))))

(let ((procedure
       (lambda (buffer)
	 (let ((pathname (buffer-pathname buffer)))
	   (if pathname
	       (load-find-file-initialization buffer pathname))))))
  (add-event-receiver! event:set-buffer-pathname procedure)
  (add-event-receiver! event:set-buffer-major-mode procedure))

(define (load-find-file-initialization buffer pathname)
  (let ((pathname
	 (catch-file-errors
	  (lambda (condition) condition #f)
	  (lambda () (os/find-file-initialization-filename pathname)))))
    (if pathname
	(let ((database
	       (with-output-to-transcript-buffer
		(lambda ()
		  (bind-condition-handler (list condition-type:error)
		      evaluation-error-handler
		    (lambda ()
		      (catch-file-errors (lambda (condition) condition #f)
			(lambda ()
			  (fluid-let ((load/suppress-loading-message? #t))
			    (load pathname '(EDWIN)))))))))))
	  (if (and (procedure? database)
		   (procedure-arity-valid? database 1))
	      (database buffer)
	      (message
	       "Ill-formed find-file initialization file: "
	       (os/pathname->display-string pathname)))))))

(define (standard-scheme-find-file-initialization database)
  ;; DATABASE -must- be a vector whose elements are all three element
  ;; lists.  The car of each element must be a string.
  (sort! database (lambda (x y) (string<? (car x) (car y))))
  (lambda (buffer)
    (let ((entry
	   (let ((pathname (buffer-pathname buffer)))
	     (and pathname
		  (equal? "scm" (pathname-type pathname))
		  (let ((name (pathname-name pathname)))
		    (and name
			 (vector-binary-search database
					       string<?
					       car
					       name)))))))
      (if entry
	  (begin
	    (local-set-variable! scheme-environment (cadr entry) buffer)
	    (if (and (eq? 'DEFAULT (ref-variable scheme-environment buffer))
		     (not (eq? 'DEFAULT (cadr entry))))
		(message "Ignoring bad evaluation environment: "
			 (cadr entry))))))))

(define (find-file-revert buffer)
  (if (verify-visited-file-modification-time? buffer)
      buffer
      (let ((pathname (buffer-pathname buffer)))
	(cond ((not (file-exists? pathname))
	       (editor-error "File "
			     (->namestring pathname)
			     " no longer exists!"))
	      ((prompt-for-yes-or-no?
		(string-append
		 "File has changed since last visited or saved.  "
		 (if (buffer-modified? buffer)
		     "Flush your changes"
		     "Read from disk")))
	       (revert-buffer buffer #t #t))
	      (else buffer)))))

(define-command revert-buffer
  "Replace the buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
If latest auto-save file is more recent than the visited file,
asks user whether to use that instead.
Argument means don't offer to use auto-save file."
  "P"
  (lambda (argument)
    (revert-buffer (selected-buffer) argument #f)))

(define (revert-buffer buffer dont-use-auto-save? dont-confirm?)
  ((or (buffer-get buffer 'REVERT-BUFFER-METHOD) revert-buffer-default)
   buffer dont-use-auto-save? dont-confirm?))

(define (revert-buffer-default buffer dont-use-auto-save? dont-confirm?)
  (let ((auto-save?
	 (and (not dont-use-auto-save?)
	      (buffer-auto-saved? buffer)
	      (buffer-auto-save-pathname buffer)
	      (file-readable? (buffer-auto-save-pathname buffer))
	      (prompt-for-confirmation?
"Buffer has been auto-saved recently.  Revert from auto-save file"))))
    (let ((pathname
	   (if auto-save?
	       (buffer-auto-save-pathname buffer)
	       (buffer-pathname buffer))))
      (if (not pathname)
	  (editor-error "Buffer does not seem to be associated with any file"))
      (if (not (file-readable? pathname))
	  (editor-error "File "
			(->namestring pathname)
			" no longer "
			(if (file-exists? pathname) "exists" "readable")
			"!"))
      (if (or dont-confirm?
	      (prompt-for-yes-or-no?
	       (string-append "Revert buffer from file "
			      (->namestring pathname))))
	  (begin
	    ;; If file was backed up but has changed since, we
	    ;; should make another backup.
	    (if (and (not auto-save?)
		     (not (verify-visited-file-modification-time? buffer)))
		(set-buffer-backed-up?! buffer #f))
	    (let ((where (mark-index (buffer-point buffer)))
		  (group (buffer-group buffer))
		  (do-it
		   (lambda ()
		     (read-buffer buffer pathname (not auto-save?)))))
	      (if (group-undo-data group)
		  (begin
		    ;; Throw away existing undo data.
		    (disable-group-undo! group)
		    (do-it)
		    (enable-group-undo! group))
		  (do-it))
	      (set-buffer-point!
	       buffer
	       (make-mark group (min where (buffer-length buffer))))
	      (after-find-file buffer #f #f)))
	  buffer))))

(define-command recover-file
  "Visit file FILE, but get contents from its last auto-save file."
  "FRecover file"
  (lambda (filename)
    (let ((pathname (pathname-simplify (merge-pathnames filename))))
      (let ((filename (->namestring pathname)))
	(if (os/auto-save-filename? filename)
	    (editor-error filename " is an auto-save file")))
      (let ((auto-save-pathname (os/auto-save-pathname pathname #f)))
	(let ((auto-save-filename (->namestring auto-save-pathname)))
	  (if (not (file-newer-than-file? auto-save-pathname pathname))
	      (editor-error "Auto-save file "
			    auto-save-filename
			    " not current"))
	  (if (not (call-with-temporary-buffer "*Directory*"
		     (lambda (buffer)
		       (insert-dired-entry! pathname (buffer-end buffer))
		       (insert-dired-entry! auto-save-pathname
					    (buffer-end buffer))
		       (set-buffer-point! buffer (buffer-start buffer))
		       (buffer-not-modified! buffer)
		       (pop-up-buffer buffer #f)
		       (prompt-for-yes-or-no?
			(string-append "Recover auto save file "
				       auto-save-filename)))))
	      (editor-error "Recover-file cancelled."))
	  (let ((buffer (find-file-noselect pathname #f)))
	    (read-buffer buffer auto-save-pathname #f)
	    (let ((buffer (after-find-file buffer #f #f)))
	      (disable-buffer-auto-save! buffer)
	      (message
	       "Auto-save off in this buffer till you do M-x auto-save-mode.")
	      (select-buffer buffer))))))))

(define-command insert-filename
  "Interactively read a file name and insert it at point.
The file name is normally inserted using Scheme syntax,
but see the variable insert-filename-format."
  "FInsert filename"
  (lambda (filename)
    (insert-string ((ref-variable insert-filename-format) filename)
		   (current-point))))

(define-variable insert-filename-format
  "Defines the format used by \[insert-filename].
The value of this variable must be a procedure of one argument.
The procedure is called with the filename as an argument,
and returns the string that is inserted into the buffer."
  write-to-string
  (lambda (object)
    (and (procedure? object)
	 (procedure-arity-valid? object 1))))

(define-command save-buffer
  "Save current buffer in visited file if modified.  Versions described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
With 1 or 3 \\[universal-argument]'s, marks this version
 to become a backup when the next save is done.
With 2 or 3 \\[universal-argument]'s,
 unconditionally makes the previous version into a backup file.
With argument of 0, never makes the previous version into a backup file.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `version-control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `version-control' is
 not #F.
We don't want excessive versions piling up, so there are variables
 `kept-old-versions', which tells Edwin how many oldest versions to keep,
 and `kept-new-versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
If `trim-versions-without-asking' is false, system will query user
 before trimming versions.  Otherwise it does it silently."
  "p"
  (lambda (argument)
    (save-buffer (selected-buffer)
		 (case argument
		   ((0) 'NO-BACKUP)
		   ((4) 'BACKUP-NEXT)
		   ((16) 'BACKUP-PREVIOUS)
		   ((64) 'BACKUP-BOTH)
		   (else #f)))))

(define (save-buffer buffer backup-mode)
  (if (buffer-modified? buffer)
      (begin
	(if (not (buffer-pathname buffer))
	    (set-visited-pathname
	     buffer
	     (prompt-for-pathname
	      (string-append "Write buffer " (buffer-name buffer) " to file")
	      #f)))
	(if (and (ref-variable enable-emacs-write-file-message)
		 (> (buffer-length buffer) 50000))
	    (message "Saving file "
		     (->namestring (buffer-pathname buffer))
		     "..."))
	(write-buffer-interactive buffer backup-mode))
      (message "(No changes need to be written)")))

(define-command save-some-buffers
  "Saves some modified file-visiting buffers.  Asks user about each one.
With argument, saves all with no questions."
  "P"
  (lambda (no-confirmation?)
    (save-some-buffers no-confirmation? #f)))

(define (save-some-buffers no-confirmation? exiting?)
  (let ((buffers
	 (let ((exiting? (and (not (default-object? exiting?)) exiting?)))
	   (list-transform-positive (buffer-list)
	     (lambda (buffer)
	       (and (buffer-modified? buffer)
		    (or (buffer-pathname buffer)
			(and exiting?
			     (ref-variable buffer-offer-save buffer)
			     (> (buffer-length buffer) 0)))))))))
    (for-each (if (and (not (default-object? no-confirmation?))
		       no-confirmation?)
		  (lambda (buffer)
		    (write-buffer-interactive buffer #f))
		  (lambda (buffer)
		    (if (prompt-for-confirmation?
			 (let ((pathname (buffer-pathname buffer)))
			   (if pathname
			       (string-append "Save file "
					      (->namestring pathname))
			       (string-append "Save buffer "
					      (buffer-name buffer)))))
			(write-buffer-interactive buffer #f))))
	      buffers)
    (let ((abbrevs-saved? (maybe-save-abbrevs no-confirmation?)))
      (if (and (null? buffers) (not abbrevs-saved?))
	  (message "(No files need saving)")))))

(define-variable-per-buffer buffer-offer-save
  "True in a buffer means offer to save the buffer on exit
even if the buffer is not visiting a file.  Automatically local in
all buffers."
  #f
  boolean?)

(define (pathname->buffer-name pathname)
  (let ((pathname (directory-pathname-as-file (->pathname pathname))))
    (if (directory-pathname? pathname)
	(->namestring pathname)
	(file-namestring pathname))))

(define (pathname->buffer pathname)
  (let ((pathname (->pathname pathname)))
    (list-search-positive (buffer-list)
      (lambda (buffer)
	(equal? pathname (buffer-pathname buffer))))))

(define-command set-visited-file-name
  "Change name of file visited in current buffer.
The next time the buffer is saved it will go in the newly specified file.
Delete the initial contents of the minibuffer
if you wish to make buffer not be visiting any file."
  "FSet visited file name"
  (lambda (filename)
    (set-visited-pathname
     (selected-buffer)
     (let ((pathname (->pathname filename)))
       (and (not (directory-pathname? pathname))
	    pathname)))))

(define (set-visited-pathname buffer pathname)
  (if (and pathname (directory-pathname? pathname))
      (editor-error "File name cannot be a directory: "
		    (->namestring pathname)))
  (set-buffer-pathname! buffer pathname)
  (set-buffer-truename! buffer #f)
  (if pathname
      (let ((name (pathname->buffer-name pathname)))
	(if (not (find-buffer name))
	    (rename-buffer buffer name))))
  (set-buffer-backed-up?! buffer #f)
  (clear-visited-file-modification-time! buffer)
  (cond ((buffer-auto-save-pathname buffer)
	 (rename-auto-save-file! buffer))
	(pathname
	 (setup-buffer-auto-save! buffer)))
  (if pathname
      (buffer-modified! buffer)))

(define-command write-file
  "Write current buffer into file FILENAME.
Makes buffer visit that file, and marks it not modified."
  "FWrite file"
  (lambda (filename)
    (write-file (selected-buffer) filename)))

(define (write-file buffer filename)
  (if (and filename
	   (not (string-null? filename)))
      (set-visited-pathname buffer (->pathname filename)))
  (buffer-modified! buffer)
  (save-buffer buffer #f))

(define-command write-region
  "Write current region into specified file."
  "r\nFWrite region to file"
  (lambda (region filename)
    (write-region region filename #t #t)))

(define-command append-to-file
  "Write current region into specified file."
  "r\nFAppend to file"
  (lambda (region filename)
    (append-to-file region filename #t #t)))

(define-command insert-file
  "Insert contents of file into existing text.
Leaves point at the beginning, mark at the end."
  "FInsert file"
  (lambda (filename)
    (let ((point (mark-right-inserting (current-point))))
      (let ((mark (mark-left-inserting point)))
	(insert-file point filename)
	(set-current-point! point)
	(push-current-mark! mark)))))

(define-command copy-file
  "Copy a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (lambda ()
    (let ((old (prompt-for-existing-file "Copy file" #f)))
      (list old (prompt-for-file "Copy to" old))))
  (lambda (old new)
    (if (or (not (file-exists? new))
	    (prompt-for-yes-or-no?
	     (string-append "File "
			    (->namestring new)
			    " already exists; copy anyway")))
	(begin
	  (copy-file old new)
	  (message "Copied " (->namestring old) " => " (->namestring new))))))

(define-command rename-file
  "Rename a file; the old and new names are read in the typein window.
If a file with the new name already exists, confirmation is requested first."
  (lambda ()
    (let ((old (prompt-for-existing-file "Rename file" #f)))
      (list old (prompt-for-file "Rename to" old))))
  (lambda (old new)
    (let ((do-it
	   (lambda ()
	     (rename-file old new)
	     (message "Renamed " (->namestring old)
		      " => " (->namestring new)))))
      (if (file-exists? new)
	  (if (prompt-for-yes-or-no?
	       (string-append "File "
			      (->namestring new)
			      " already exists; rename anyway"))
	      (begin (delete-file new) (do-it)))
	  (do-it)))))

(define-command delete-file
  "Delete a file; the name is read in the typein window."
  "fDelete File"
  delete-file)

(define-command pwd
  "Show the current default directory."
  ()
  (lambda ()
    (message "Directory "
	     (->namestring (buffer-default-directory (selected-buffer))))))

(define-command cd
  "Make DIR become the current buffer's default directory."
  "DChange default directory"
  (lambda (directory)
    (set-default-directory directory)
    ((ref-command pwd))))

(define (set-default-directory directory)
  (let ((buffer (selected-buffer)))
    (let ((directory
	   (pathname-as-directory
	    (merge-pathnames directory (buffer-default-directory buffer)))))
      (if (not (file-directory? directory))
	  (editor-error (->namestring directory) " is not a directory"))
      (if (not (file-access directory 1))
	  (editor-error "Cannot cd to "
			(->namestring directory)
			": Permission denied"))
      (set-buffer-default-directory! buffer directory))))

;;;; Encryption

(define-command encrypt-file
  "Encrypt a file with the blowfish encryption algorithm.
Prompts for the plaintext and ciphertext filenames.
Prefix arg means treat the plaintext file as binary data.
Deletes the plaintext file after encryption."
  (lambda ()
    (guarantee-blowfish-available)
    (let ((from (prompt-for-existing-file "Encrypt file (plaintext)" #f)))
      (let ((to
	     (prompt-for-file
	      "Encrypt file to (ciphertext)"
	      (list (string-append (->namestring from) ".bf")))))
	(list from to (command-argument)))))
  (lambda (from to binary-plaintext?)
    (blowfish-encrypt-file from to binary-plaintext? #t)))

(define-command decrypt-file
  "Decrypt a file with the blowfish encryption algorithm.
Prompts for the ciphertext and plaintext filenames.
Prefix arg means treat the plaintext file as binary data."
  (lambda ()
    (guarantee-blowfish-available)
    (let ((from (prompt-for-existing-file "Decrypt file (ciphertext)" #f)))
      (let ((to
	     (prompt-for-file
	      "Decrypt file to (plaintext)"
	      (and (pathname-type from)
		   (list (pathname-new-type from #f))))))
	(list from to (command-argument)))))
  (lambda (from to binary-plaintext?)
    (blowfish-decrypt-file from to binary-plaintext? #f)))

(define (guarantee-blowfish-available)
  (if (not (blowfish-available?))
      (editor-error "Blowfish encryption not supported on this system.")))

(define (blowfish-encrypt-file from to binary-plaintext? delete-plaintext?)
  (guarantee-blowfish-available)
  (and (or (not (file-exists? to))
	   (prompt-for-yes-or-no?
	    (string-append "File "
			   (->namestring to)
			   " already exists; overwrite")))
       (begin
	 ((if binary-plaintext?
	      call-with-binary-input-file
	      call-with-input-file)
	  from
	  (lambda (input)
	    (%blowfish-encrypt-file to input)))
	 (let ((t (file-modification-time-indirect from)))
	   (set-file-times! to t t))
	 (set-file-modes! to (file-modes from))
	 (if delete-plaintext? (delete-file from))
	 #t)))

(define (blowfish-decrypt-file from to binary-plaintext? delete-ciphertext?)
  (guarantee-blowfish-available)
  (and (or (not (file-exists? to))
	   (prompt-for-yes-or-no?
	    (string-append "File "
			   (->namestring to)
			   " already exists; overwrite")))
       (begin
	 ((if binary-plaintext?
	      call-with-binary-output-file
	      call-with-output-file)
	  to
	  (lambda (output)
	    (%blowfish-decrypt-file from output)))
	 (let ((t (file-modification-time-indirect from)))
	   (set-file-times! to t t))
	 (set-file-modes! to (file-modes from))
	 (if delete-ciphertext? (delete-file from))
	 #t)))

(define (%blowfish-encrypt-file pathname input)
  (call-with-binary-output-file pathname
    (lambda (output)
      (call-with-sensitive-string (call-with-confirmed-pass-phrase md5-string)
	(lambda (key-string)
	  (blowfish-encrypt-port input output key-string
				 (write-blowfish-file-header output)
				 #t))))))

(define (%blowfish-decrypt-file pathname output)
  (call-with-binary-input-file pathname
    (lambda (input)
      (call-with-sensitive-string
       (call-with-pass-phrase "Pass phrase" md5-string)
       (lambda (key-string)
	 (blowfish-encrypt-port input output key-string
				(read-blowfish-file-header input)
				#f))))))

(define (call-with-sensitive-string string receiver)
  (dynamic-wind (lambda ()
		  unspecific)
		(lambda ()
		  (receiver string))
		(lambda ()
		  (string-fill! string #\NUL)
		  (set! string)
		  unspecific)))

;;;; Prompting

(define (prompt-for-file prompt default . options)
  (->namestring
   (prompt-for-pathname* prompt default file-non-directory? options)))

(define (prompt-for-existing-file prompt default . options)
  (->namestring
   (prompt-for-pathname* prompt default file-non-directory?
			 (cons* 'REQUIRE-MATCH? #t options))))

(define (file-non-directory? file)
  (and (file-exists? file)
       (not (file-directory? file))))

(define (prompt-for-directory prompt default . options)
  (->namestring
   (let ((directory
	  (prompt-for-pathname* prompt default file-directory-not-wild?
				options)))
     ;; Don't convert the result to directory form unless it is known
     ;; to be a directory.  If REQUIRE-MATCH? is false, it is allowed
     ;; to specify a file part.
     (if (file-test-no-errors file-directory-not-wild? directory)
	 (pathname-as-directory directory)
	 directory))))

(define (prompt-for-existing-directory prompt default . options)
  (->namestring
   (pathname-as-directory
    (prompt-for-pathname* prompt default file-directory-not-wild?
			  (cons* 'REQUIRE-MATCH? #t options)))))

(define (file-directory-not-wild? pathname)
  (and (not (pathname-wild? pathname))
       (file-directory? pathname)))

(define (prompt-for-pathname prompt default . options)
  (prompt-for-pathname* prompt default file-exists? options))

(define (prompt-for-pathname* prompt default verify-final-value options)
  (let ((directory
	 (if default
	     (directory-pathname (if (pair? default) (car default) default))
	     (buffer-default-directory (selected-buffer))))
	(options
	 (cons* 'DEFAULT-TYPE 'INSERTED-DEFAULT
		'HISTORY 'PROMPT-FOR-PATHNAME
		options)))
    (let ((insertion
	   (or (prompt-options-default-string options)
	       (os/pathname->display-string
		(if (pair? default)
		    (car default)
		    directory)))))
      (prompt-string->pathname
       (apply prompt-for-completed-string
	      prompt
	      insertion
	      (lambda (string if-unique if-not-unique if-not-found)
		(filename-complete-string
		 (prompt-string->pathname string insertion directory)
		 (lambda (filename)
		   (if-unique (os/pathname->display-string filename)))
		 (lambda (prefix get-completions)
		   (if-not-unique (os/pathname->display-string prefix)
				  get-completions))
		 if-not-found))
	      (lambda (string)
		(filename-completions-list
		 (prompt-string->pathname string insertion directory)))
	      (lambda (string)
		(file-test-no-errors
		 verify-final-value
		 (prompt-string->pathname string insertion directory)))
	      options)
       insertion
       directory))))

;;;; Filename Completion

(define (filename-complete-string pathname
				  if-unique if-not-unique if-not-found)
  (let ((directory (directory-namestring pathname))
	(if-directory
	 (lambda (directory)
	   (if-not-unique directory
			  (lambda ()
			    (canonicalize-filename-completions
			     directory
			     (os/directory-list directory)))))))
    (cond ((not (file-test-no-errors file-directory? directory))
	   (if-not-found))
	  ((directory-pathname? pathname)
	   (if-directory directory))
	  (else
	   (let ((filenames
		  (os/directory-list-completions directory
						 (file-namestring pathname)))
		 (unique-case
		  (lambda (filename)
		    (let ((pathname (merge-pathnames filename directory)))
		      (if (file-test-no-errors file-directory? pathname)
			  (if-directory
			   (->namestring (pathname-as-directory pathname)))
			  (if-unique (->namestring pathname))))))
		 (non-unique-case
		  (lambda (filenames filtered-filenames)
		    (let ((string
			   (string-greatest-common-prefix filtered-filenames)))
		      (if-not-unique
		       (->namestring (merge-pathnames string directory))
		       (lambda ()
			 (canonicalize-filename-completions
			  directory
			  (list-transform-positive filenames
			    (lambda (filename)
			      (string-prefix? string filename))))))))))
	     (cond ((null? filenames)
		    (if-not-found))
		   ((null? (cdr filenames))
		    (unique-case (car filenames)))
		   (else
		    (let ((filtered-filenames
			   (list-transform-negative filenames
			     (lambda (filename)
			       (completion-ignore-filename?
				(merge-pathnames filename directory))))))
		      (cond ((null? filtered-filenames)
			     (non-unique-case filenames filenames))
			    ((null? (cdr filtered-filenames))
			     (unique-case (car filtered-filenames)))
			    (else
			     (non-unique-case filenames
					      filtered-filenames)))))))))))

(define (filename-completions-list pathname)
  (let ((directory (directory-namestring pathname)))
    (canonicalize-filename-completions
     directory
     (os/directory-list-completions directory
				    (file-namestring pathname)))))

(define (prompt-string->pathname string insertion directory)
  (merge-pathnames (let ((pathname (os/trim-pathname-string string insertion)))
		     (if (memq (pathname-device pathname) '(#F UNSPECIFIC))
			 pathname
			 (pathname-default-directory pathname '(ABSOLUTE))))
		   directory))

(define (canonicalize-filename-completions directory filenames)
  (do ((filenames filenames (cdr filenames)))
      ((null? filenames))
    (if (file-test-no-errors file-directory?
			     (merge-pathnames (car filenames) directory))
	(set-car! filenames
		  (->namestring (pathname-as-directory (car filenames))))))
  (sort filenames string<?))

(define (completion-ignore-filename? filename)
  (os/completion-ignore-filename? (->namestring filename)))