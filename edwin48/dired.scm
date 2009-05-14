#| -*-Scheme-*-

$Id: dired.scm,v 1.203 2008/01/30 20:02:00 cph Exp $

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

;;;; Directory Editor
;; package: (edwin dired)


(define-variable dired-trivial-filenames
  "Regexp of files to skip when finding first file of a directory.
A value of #f means move to the subdir line.
A value of #t means move to first file."
  "^\\.\\.?$\\|^#"
  (lambda (object) (or (string? object) (boolean? object))))

(define-variable dired-mode-hook
  "An event distributor that is invoked when entering Dired mode."
  (make-event-distributor))

(define-variable dired-kept-versions
  "When cleaning directory, number of versions to keep."
  2
  exact-nonnegative-integer?)

(define-variable dired-backup-overwrite
  "True if Dired should ask about making backups before overwriting files.
Special value `always' suppresses confirmation."
  #f
  boolean?)

(define-major-mode dired read-only "Dired"
  "Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory.
You can move using the usual cursor motion commands.
Letters no longer insert themselves.  Digits are prefix arguments.
Instead, type \\[dired-flag-file-deletion] to flag a file for Deletion.
Type \\[dired-mark] to Mark a file for later commands.
  Most commands operate on the marked files and use the current file
  if no files are marked.  Use a numeric prefix argument to operate on
  the next ARG (or previous -ARG if ARG<0) files, or just `1'
  to operate on the current file only.  Prefix arguments override marks.
Type \\[dired-unmark] to Unmark a file.
Type \\[dired-backup-unmark] to back up one line and unmark.
Type \\[dired-do-deletions] to eXecute the deletions requested.
Type f to Find the current line's file
  (or dired it in another buffer, if it is a directory).
Type \\[dired-find-file-other-window] to find file or dired directory in Other window.
Type \\[dired-flag-auto-save-files] to flag temporary files (names beginning with #) for Deletion.
Type \\[dired-flag-backup-files] to flag backup files (names ending with ~) for Deletion.
Type \\[dired-clean-directory] to flag numerical backups for Deletion.
  (Spares dired-kept-versions or its numeric argument.)
Type \\[dired-do-rename] to rename a file or move the marked files to another directory.
Type \\[dired-do-copy] to copy files.
Type \\[dired-sort-toggle-or-edit] to toggle sorting by name/date or change the listing switches.
Type \\[dired-revert] to read the directory again.  This discards all deletion-flags.
Space and Rubout can be used to move down and up by lines.

\\{dired}"
  (lambda (buffer)
    (event-distributor/invoke! (ref-variable dired-mode-hook buffer) buffer)))

(define-key 'dired (kbd #\#) 'dired-flag-auto-save-files)
(define-key 'dired (kbd #\+) 'dired-create-directory)
(define-key 'dired (kbd #\.) 'dired-clean-directory)
(define-key 'dired (kbd #\?) 'dired-summary)
(define-key 'dired (kbd #\d) 'dired-flag-file-deletion)
(define-key 'dired (kbd #\e) 'dired-find-file)
(define-key 'dired (kbd #\f) 'dired-find-file)
(define-key 'dired (kbd return) 'dired-find-file)
(define-key 'dired (kbd #\g) 'dired-revert)
(define-key 'dired (kbd #\h) 'describe-mode)
(define-key 'dired (kbd #\m) 'dired-mark)
(define-key 'dired (kbd #\n) 'dired-next-line)
(define-key 'dired (kbd #\o) 'dired-find-file-other-window)
(define-key 'dired (kbd #\p) 'dired-previous-line)
(define-key 'dired (kbd #\q) 'dired-quit)
(define-key 'dired (kbd #\s) 'dired-sort-toggle-or-edit)
(define-key 'dired (kbd #\u) 'dired-unmark)
(define-key 'dired (kbd #\v) 'dired-view-file)
(define-key 'dired (kbd #\x) 'dired-do-deletions)
(define-key 'dired (kbd #\~) 'dired-flag-backup-files)

(define-key 'dired (kbd #\C) 'dired-do-copy)
(define-key 'dired (kbd #\K) 'dired-encrypt/decrypt-file)
(define-key 'dired (kbd #\R) 'dired-do-rename)

(define-key 'dired (kbd (ctrl d)) 'dired-flag-file-deletion)
(define-key 'dired (kbd (ctrl n)) 'dired-next-line)
(define-key 'dired (kbd (ctrl p)) 'dired-previous-line)
(define-key 'dired (kbd (ctrl #\])) 'dired-abort)

(define-key 'dired (kbd rubout) 'dired-backup-unmark)
(define-key 'dired (kbd (meta rubout)) 'dired-unmark-all-files)
(define-key 'dired (kbd #\space) 'dired-next-line)

(define-key 'dired (kbd down) 'dired-next-line)
(define-key 'dired (kbd up) 'dired-previous-line)

(define-command dired
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Dired displays a list of files in DIRNAME.
You can move around in it with the usual commands.
You can flag files for deletion with C-d
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  "DDired (directory)"
  (lambda (directory)
    (select-buffer (make-dired-buffer directory))))

(define-command dired-other-window
  "\"Edit\" directory DIRNAME.  Like \\[dired] but selects in another window."
  "DDired in other window (directory)"
  (lambda (directory)
    (select-buffer-other-window (make-dired-buffer directory))))

(define-command dired-other-frame
  "\"Edit\" directory DIRNAME.  Like \\[dired] but selects in another frame."
  "DDired in other frame (directory)"
  (lambda (directory)
    (select-buffer-other-screen (make-dired-buffer directory))))

(define (make-dired-buffer directory #!optional file-list)
  (let ((directory (pathname-simplify directory))
	(file-list (if (default-object? file-list) 'ALL file-list)))
    (let ((directory-spec (cons directory file-list)))
      (or (find-dired-buffer directory-spec)
	  (let ((buffer (new-buffer (pathname->buffer-name directory))))
	    (set-buffer-major-mode! buffer (ref-mode-object dired))
	    (set-buffer-default-directory! buffer
					   (directory-pathname directory))
	    (buffer-put! buffer 'DIRED-DIRECTORY-SPEC directory-spec)
	    (buffer-put! buffer 'REVERT-BUFFER-METHOD revert-dired-buffer)
	    (fill-dired-buffer! buffer directory-spec)
	    (dired-initial-position! buffer)
	    buffer)))))

(define (find-dired-buffer directory-spec)
  (find (lambda (buffer)
	  (equal? directory-spec (buffer-get buffer 'DIRED-DIRECTORY-SPEC)))
	(buffer-list)))

(define (dired-buffer-directory-spec buffer)
  (or (buffer-get buffer 'DIRED-DIRECTORY-SPEC)
      (let ((directory-spec (cons (buffer-default-directory buffer) 'ALL)))
	(buffer-put! buffer 'DIRED-DIRECTORY-SPEC directory-spec)
	directory-spec)))

(define (dired-buffer-directory buffer)
  (car (dired-buffer-directory-spec buffer)))

(define (revert-dired-buffer buffer dont-use-auto-save? dont-confirm?)
  dont-use-auto-save? dont-confirm?	;ignore
  (let ((lstart
	 (line-start (if (current-buffer? buffer)
			 (current-point)
			 (buffer-point buffer))
		     0)))
    (let ((filename (dired-filename-string lstart)))
      (fill-dired-buffer! buffer (dired-buffer-directory-spec buffer))
      (set-dired-point!
       (or (and filename
		 (let loop ((lstart (buffer-start buffer)))
		   (if (eqv? filename (dired-filename-string lstart))
		       lstart
		       (let ((lstart (line-start lstart 1 #f)))
			 (and lstart
			      (loop lstart))))))
	   (line-start
	    (if (mark< lstart (buffer-end buffer))
		lstart
		(buffer-end buffer))
	    0)))))
  buffer)

(define (fill-dired-buffer! buffer directory-spec)
  (let ((pathname (car directory-spec))
	(file-list (cdr directory-spec)))
    (set-buffer-writeable! buffer)
    (region-delete! (buffer-region buffer))
    ((message-wrapper #t "Reading directory " (->namestring pathname))
     (lambda ()
       (read-directory pathname
		       file-list
		       (ref-variable dired-listing-switches buffer)
		       (buffer-point buffer))))
    (let ((point (mark-left-inserting-copy (buffer-point buffer)))
	  (group (buffer-group buffer)))
      (let ((index (mark-index (buffer-start buffer))))
	(if (not (group-end-index? group index))
	    (let loop ((index index))
	      (set-mark-index! point index)
	      (group-insert-string! group index "  ")
	      (let ((index (1+ (line-end-index group (mark-index point)))))
		(if (not (group-end-index? group index))
		    (loop index))))))
      (mark-temporary! point))
    (set-buffer-point! buffer (buffer-start buffer))
    (buffer-not-modified! buffer)
    (set-buffer-read-only! buffer)))

(define (read-directory pathname file-list switches mark)
  (if (eq? 'ALL file-list)
      (insert-directory! (let ((dir (pathname-as-directory pathname)))
			   (if (and (not (dired-pathname-wild? pathname))
				    (not (pathname=? pathname dir))
				    (file-directory? pathname))
			       dir
			       pathname))
			 switches mark
			 (if (dired-pathname-wild? pathname)
			     'WILDCARD
			     'DIRECTORY))
      (let ((mark (mark-left-inserting-copy mark)))
	(for-each (lambda (file)
		    (insert-directory! (merge-pathnames file pathname)
				       switches
				       mark
				       'FILE))
		  file-list)
	(mark-temporary! mark))))

(define (insert-dired-entry! pathname mark)
  (let ((mark (mark-left-inserting-copy mark)))
    (insert-string "  " mark)
    (insert-directory! pathname
		       (ref-variable dired-listing-switches mark)
		       mark
		       'FILE)
    (mark-temporary! mark)))

(define (dired-initial-position! buffer)
  (let ((lstart (buffer-start buffer)))
    (if (ref-variable dired-trivial-filenames lstart)
	(let ((lstart (next-nontrivial-file-line lstart)))
	  (if lstart
	      (set-buffer-point! buffer (dired-filename-start lstart)))))))

(define (next-nontrivial-file-line lstart)
  (let ((dired-trivial-filenames
	 (ref-variable dired-trivial-filenames lstart))
	(syntax-table (group-syntax-table (mark-group lstart))))
    (let loop ((lstart lstart))
      (let ((filename (dired-filename-string lstart)))
	(if (and filename
		 (or (not (string? dired-trivial-filenames))
		     (not (re-string-match dired-trivial-filenames
					   filename #f syntax-table))))
	    lstart
	    (let ((lstart (line-start lstart 1 #f)))
	      (and lstart
		   (loop lstart))))))))

(define-command dired-find-file
  "Read the current file into a buffer."
  ()
  (lambda ()
    (find-file (dired-current-pathname))))

(define-command dired-find-file-other-window
  "Read the current file into a buffer in another window."
  ()
  (lambda ()
    (find-file-other-window (dired-current-pathname))))

(define-command dired-revert
  "Read the current buffer."
  ()
  (lambda ()
    (revert-buffer (current-buffer) #t #t)))

(define-command dired-flag-file-deletion
  "Mark the current file to be killed."
  "p"
  (lambda (argument)
    (dired-mark dired-flag-delete-char argument)))

(define-command dired-mark
  "Mark the current (or next ARG) files."
  "p"
  (lambda (argument)
    (dired-mark dired-marker-char argument)))

(define-command dired-unmark
  "Unmark the current (or next ARG) files."
  "p"
  (lambda (argument)
    (dired-mark #\space argument)))

(define-command dired-backup-unmark
  "Move up one line and remove deletion flag there.
Optional prefix ARG says how many lines to unflag; default is one line."
  "p"
  (lambda (argument)
    (dired-mark-backward #\space argument)))

(define-command dired-next-line
  "Move down to the next line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) argument 'BEEP))))

(define-command dired-previous-line
  "Move up to the previous line."
  "p"
  (lambda (argument)
    (set-dired-point! (line-start (current-point) (- argument) 'BEEP))))

(define-command dired-do-deletions
  "Kill all marked files."
  ()
  (lambda ()
    (dired-kill-files)))

(define-command dired-quit
  "Exit Dired, offering to kill any files first."
  ()
  (lambda ()
    (dired-kill-files)
    (kill-buffer-interactive (current-buffer))))

(define-command dired-abort
  "Exit Dired."
  ()
  (lambda ()
    (kill-buffer-interactive (current-buffer))))

(define-command dired-summary
  "Summarize the Dired commands in the typein window."
  ()
  (lambda ()
    (message "d-elete, u-ndelete, x-ecute, q-uit, f-ind, o-ther window")))

(define-command dired-flag-auto-save-files
  "Flag for deletion files whose names suggest they are auto save files."
  ()
  (lambda ()
    (for-each-file-line (current-buffer)
      (lambda (lstart)
	(if (os/auto-save-filename? (dired-filename-string lstart))
	    (dired-mark-1 lstart dired-flag-delete-char))))))

(define-command dired-flag-backup-files
  "Flag all backup files for deletion."
  ()
  (lambda ()
    (for-each-file-line (current-buffer)
      (lambda (lstart)
	(if (os/backup-filename? (dired-filename-string lstart))
	    (dired-mark-1 lstart dired-flag-delete-char))))))

(define-command dired-clean-directory
  "Flag numerical backups for deletion.
Spares dired-kept-versions latest versions, and kept-old-versions oldest.
Positive numeric arg overrides dired-kept-versions;
negative numeric arg overrides kept-old-versions with minus the arg."
  "P"
  (lambda (argument)
    (let ((argument (command-argument-value argument))
	  (old (ref-variable kept-old-versions))
	  (new (ref-variable dired-kept-versions))
	  (do-it
	   (lambda (old new)
	     (let ((total (+ old new)))
	       (for-each
		(lambda (file)
		  (let ((nv (length (cdr file))))
		    (if (> nv total)
			(let ()
			  (let ((end (- nv total)))
			    (do ((versions
				  (drop
				   (sort (cdr file)
					 (lambda (x y)
					   (< (car x) (car y))))
				   old)
				  (cdr versions))
				 (index 0 (fix:+ index 1)))
				((fix:= index end))
			      (dired-mark-1 (cdar versions)
					    dired-flag-delete-char)))))))
		(dired-numeric-backup-files))))))
      (cond ((and argument (> argument 0)) (do-it old argument))
	    ((and argument (< argument 0)) (do-it (- argument) new))
	    (else (do-it old new))))))

(define (dired-numeric-backup-files)
  (let ((result '()))
    (let loop ((start (line-start (buffer-start (current-buffer)) 0)))
      (let ((next (line-start start 1 #f)))
	(if next
	    (begin
	      (let ((filename (dired-filename-string start)))
		(if filename
		    (let ((root.version
			   (os/numeric-backup-filename? filename)))
		      (if root.version
			  (let ((root (car root.version))
				(version.index
				 (cons (cdr root.version) start)))
			    (let ((entry (assoc root result)))
			      (if entry
				  (set-cdr! entry
					    (cons version.index (cdr entry)))
				  (set! result
					(cons (list root version.index)
					      result)))))))))
	      (loop next)))))
    result))

(define-command dired-unmark-all-files
  "Remove a specific mark (or any mark) from every file.
After this command, type the mark character to remove, 
or type RET to remove all marks.
With prefix arg, query for each marked file.
Type \\[help-command] at that time for help."
  "cRemove marks (RET means all)\nP"
  (lambda (mark arg)
    (for-each (if arg
		  (let ((query-state (list #f)))
		    (lambda (pair)
		      (let ((pathname (car pair))
			    (lstart (cdr pair)))
			(if (with-current-point (dired-filename-start lstart)
			      (lambda ()
				(dired-query
				 query-state
				 (string-append "Unmark file `"
						(file-namestring pathname)
						"'"))))
			    (dired-mark-1 lstart #\space)))))
		  (lambda (pair)
		    (dired-mark-1 (cdr pair) #\space)))
	      (dired-marked-files #f (if (eqv? #\return mark) #t mark)))))

(define (dired-query state prompt . args)
  (case (car state)
    ((YES) #t)
    ((NO) #f)
    (else
     (let ((result
	    (let ((prompt (string-append prompt " [Type y, n, q or !]")))
	      (let loop ()
		(apply message prompt args)
		(let ((char (keyboard-read-char)))
		  (cond ((or (char-ci=? #\y char)
			     (char=? #\space char))
			 #t)
			((or (char-ci=? #\n char)
			     (char=? #\rubout char))
			 #f)
			((char-ci=? #\q char)
			 (set-car! state 'NO)
			 #f)
			((char=? #\! char)
			 (set-car! state 'YES)
			 #t)
			(else
			 (editor-failure "Please answer y, n, q or !.")
			 (sit-for 1000)
			 (loop))))))))
       (clear-message)
       result))))

(define-command dired-sort-toggle-or-edit
  "Toggle between sort by date/name and refresh the dired buffer.
With a prefix argument you can edit the current listing switches instead."
  "P"
  (lambda (argument)
    (if argument
	(dired-change-listing-switches
	 (lambda (switches)
	   (prompt-for-string "Listing switches (must contain -l)"
			      switches
			      'DEFAULT-TYPE 'INSERTED-DEFAULT)))
	(dired-toggle-switch #\t))))

(define (dired-toggle-switch switch)
  (dired-change-listing-switches
   (lambda (switches)
     (let loop ((switches (burst-string switches char-set:whitespace #t)))
       (if (pair? switches)
	   (let ((index
		  (and (string-prefix? "-" (car switches))
		       (not (string-prefix? "--" (car switches)))
		       (string-index (car switches) switch))))
	     (if index
		 (let ((s1
			(string-append (string-head (car switches) index)
				       (string-tail (car switches)
						    (fix:+ index 1))))
		       (s2
			(reduce-right string-append-separated
				      ""
				      (cdr switches))))
		   (if (string=? "-" s1)
		       s2
		       (string-append-separated s1 s2)))
		 (string-append-separated (car switches)
					  (loop (cdr switches)))))
	   (string #\- switch))))))

(define (dired-change-listing-switches procedure)
  (local-set-variable! dired-listing-switches
		       (procedure (ref-variable dired-listing-switches)))
  ((ref-command dired-revert)))

;;;; File Operation Commands

(define-command dired-create-directory
  "Create a directory named DIRECTORY."
  "DCreate directory"
  (lambda (directory)
    (let ((directory (directory-pathname-as-file directory)))
      (make-directory directory)
      (let ((lstart
	     (mark-right-inserting-copy (line-start (current-point) 0))))
	(with-read-only-defeated lstart
	  (lambda ()
	    (insert-dired-entry! directory lstart)))
	(set-dired-point! lstart)
	(mark-temporary! lstart)))))

(define-command dired-do-copy
  "Copy all marked (or next ARG) files, or copy the current file.
This preserves the last-modified date when copying.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory,
and new copies of these files are made in that directory
with the same names that the files currently have."
  "P"
  (lambda (argument)
    (dired-create-files argument "copy" "copies"
			(dired-create-file-operation copy-file))))

(define-command dired-do-rename
  "Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory."
  "P"
  (lambda (argument)
    (dired-create-files
     argument "rename" "renames"
     (let ((rename (dired-create-file-operation rename-file)))
       (lambda (lstart from to)
	 (let ((condition (rename lstart from to)))
	   (if (not condition)
	       (dired-redisplay to lstart))
	   condition))))))

(define (dired-create-file-operation operation)
  (lambda (lstart from to)
    lstart
    (catch-file-errors (lambda (condition) condition)
      (lambda ()
	(dired-handle-overwrite to)
	(operation from to)
	#f))))

(define (dired-handle-overwrite to)
  (if (and (file-exists? to)
	   (ref-variable dired-backup-overwrite)
	   (or (eq? 'ALWAYS (ref-variable dired-backup-overwrite))
	       (prompt-for-confirmation?
		(string-append "Make backup for existing file `"
			       (->namestring to)
			       "'"))))
      (call-with-values (lambda () (os/buffer-backup-pathname to #f))
	(lambda (backup-pathname targets)
	  targets
	  (rename-file to backup-pathname)))))

(define (dired-create-files argument singular-verb plural-verb operation)
  (let ((filenames
	 (if argument
	     (dired-next-files (command-argument-value argument))
	     (let ((files (dired-marked-files)))
	       (if (null? files)
		   (dired-next-files 1)
		   files)))))
    (cond ((null? filenames)
	   (message "No files to " (string-downcase singular-verb) "."))
	  ((null? (cdr filenames))
	   (dired-create-one-file (cdar filenames) (caar filenames)
				  singular-verb operation))
	  (else
	   (dired-create-many-files filenames
				    singular-verb plural-verb operation)))))

(define (dired-create-one-file lstart from singular-verb operation)
  (let ((to
	 (prompt-for-pathname (string-append (string-titlecase singular-verb)
					     " "
					     (file-namestring from)
					     " to")
			      from)))
    (let ((condition
	   (operation lstart from
		      (if (file-directory? to)
			  (merge-pathnames (file-pathname from)
					   (pathname-as-directory to))
			  to))))
      (if condition
	  (editor-error (string-titlecase singular-verb)
			" failed: "
			(condition/report-string condition))))))

(define (dired-create-many-files filenames singular-verb plural-verb operation)
  (let ((destination
	 (directory-pathname
	  (cleanup-pop-up-buffers
	   (lambda ()
	     (dired-pop-up-files-window filenames)
	     (prompt-for-existing-directory
	      (string-append (string-titlecase singular-verb)
			     " these files to directory")
	      #f))))))
    (for-each (lambda (filename)
		(set-cdr! filename (mark-right-inserting-copy (cdr filename))))
	      filenames)
    (let loop ((filenames filenames) (failures '()))
      (cond ((not (null? filenames))
	     (loop (cdr filenames)
		   (if (operation (cdar filenames)
				  (caar filenames)
				  (merge-pathnames
				   (file-pathname (caar filenames))
				   destination))
		       (cons (file-namestring (caar filenames)) failures)
		       failures)))
	    ((not (null? failures))
	     (message (string-titlecase plural-verb)
		      " failed: "
		      (reverse! failures)))))
    (for-each (lambda (filename)
		(mark-temporary! (cdr filename)))
	      filenames)))

;;;; Encrypt/Decrypt File

(define-command dired-encrypt/decrypt-file
  "Encrypt/Decrypt a file.
Decrypts if the file's suffix is recognized as a known encryption type.
Otherwise encrypts using Blowfish.
Prefix arg means treat the plaintext file as binary data (ignored on unix)."
  "P"
  (lambda (binary-plaintext?)
    (let ((pathname (dired-current-pathname)))
      (let ((type (pathname-type pathname)))
	(cond ((equal? "bf" type)
	       (dired-blowfish-decrypt-file pathname binary-plaintext?))
	      (else
	       (dired-blowfish-encrypt-file pathname binary-plaintext?)))))))

(define (dired-blowfish-encrypt-file from binary-plaintext?)
  (let ((to (string-append (->namestring from) ".bf")))
    (blowfish-encrypt-file from to binary-plaintext? #t)
    (dired-redisplay to)))

(define (dired-blowfish-decrypt-file from binary-plaintext?)
  (let ((to (pathname-new-type from #f)))
    (blowfish-decrypt-file from to binary-plaintext? #t)
    (dired-redisplay to)))

;;;; List Directory

(define-command list-directory
  "Display a list of files in or matching DIRNAME.
Prefix arg (second arg if noninteractive) means display a verbose listing.
Actions controlled by variables list-directory-brief-switches
 and list-directory-verbose-switches."
  (lambda ()
    (let ((argument (command-argument)))
      (list (prompt-for-directory (if argument
				      "List directory (verbose)"
				      "List directory (brief)")
				  #f)
	    argument)))
  (lambda (directory argument)
    (let ((directory (->pathname directory))
	  (buffer (temporary-buffer "*Directory*")))
      (disable-group-undo! (buffer-group buffer))
      (let ((point (buffer-end buffer)))
	(insert-string "Directory " point)
	(insert-string (->namestring directory) point)
	(insert-newline point)
	(read-directory directory
			'ALL
			(if argument
			    (ref-variable list-directory-verbose-switches)
			    (ref-variable list-directory-brief-switches))
			point))
      (set-buffer-point! buffer (buffer-start buffer))
      (buffer-not-modified! buffer)
      (pop-up-buffer buffer #f))))

;;;; Utilities

(define (dired-filename-start lstart)
  (re-match-forward directory-listing-before-filename-regexp
		    lstart
		    (line-end lstart 0)
		    #f))

(define (dired-filename-string lstart)
  (let ((start (dired-filename-start lstart)))
    (and start
	 (extract-string start
			 (let ((end (line-end start 0)))
			   (if (search-forward " -> " start end)
			       (re-match-start 0)
			       end))))))

(define (set-dired-point! mark)
  (set-current-point!
   (let ((lstart (line-start mark 0)))
     (or (dired-filename-start lstart)
	 lstart))))

(define (dired-current-pathname)
  (let ((lstart (line-start (current-point) 0)))
    (guarantee-dired-filename-line lstart)
    (dired-pathname lstart)))

(define (guarantee-dired-filename-line lstart)
  (if (not (dired-filename-start lstart))
      (editor-error "No file on this line")))

(define (dired-pathname lstart)
  (let ((filename (dired-filename-string lstart)))
    (and filename
	 (merge-pathnames
	  (directory-pathname (dired-buffer-directory (mark-buffer lstart)))
	  filename))))

(define (dired-mark char n)
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i n) unspecific)
    (let ((lstart (line-start (current-point) 0)))
      (guarantee-dired-filename-line lstart)
      (dired-mark-1 lstart char)
      (set-dired-point! (line-start lstart 1)))))

(define (dired-mark-backward char n)
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i n) unspecific)
    (let ((lstart (line-start (current-point) -1 'ERROR)))
      (set-dired-point! lstart)
      (guarantee-dired-filename-line lstart)
      (dired-mark-1 lstart char))))

(define (dired-mark-1 lstart char)
  (with-read-only-defeated lstart
    (lambda ()
      (delete-right-char lstart)
      (insert-chars char 1 lstart))))

(define (dired-file-line? lstart)
  (and (dired-filename-start lstart)
       (not (re-match-forward ". d" lstart (mark+ lstart 3)))))

(define (for-each-file-line buffer procedure)
  (let ((point (mark-right-inserting-copy (buffer-start buffer))))
    (do () ((group-end? point))
      (if (dired-file-line? point)
	  (procedure point))
      (move-mark-to! point (line-start point 1)))
    (mark-temporary! point)))

;;; Regular expression to match up to the file name in a directory
;;; listing.  The default value is designed to recognize dates and
;;; times regardless of the language.

;;; Stolen from Emacs 22 and translated to REXP format.

(define directory-listing-before-filename-regexp
  (let* ((l
	  (char-set-union char-set:alphabetic
			  (ascii-range->char-set #x80 #x100)))
	 (l? (rexp-optional l))
	 (l-or-quote (char-set-union l (char-set #\')))
	 (digit (string->char-set "0123456789"))
	 ;; In some locales, month abbreviations are as short as 2 letters,
	 ;; and they can be followed by ".".
	 ;; In Breton, a month name  can include a quote character.
	 (month
	  (rexp-sequence (rexp-n* 2 l-or-quote)
			 (rexp-optional ".")))
	 (s " ")
	 (s+ (rexp+ s))
	 (yyyy (rexp-n*n 4 char-set:numeric))
	 (dd (rexp-sequence (string->char-set " 0123") digit))
	 (HH:MM (rexp-sequence (string->char-set " 012")
			       digit
			       (string->char-set ":.")
			       (string->char-set "012345")
			       digit))
	 (seconds (rexp-sequence (string->char-set "0123456")
				 digit
				 (rexp-optional (string->char-set ".,")
						(rexp+ digit))))
	 (zone (rexp-sequence (string->char-set "-+")
			      (string->char-set "012")
			      digit
			      (string->char-set "012345")
			      digit))
	 (iso-mm-dd (rexp-sequence (string->char-set "01")
				   digit
				   "-"
				   (string->char-set "0123")
				   digit))
	 (iso-time
	  (rexp-sequence HH:MM
			 (rexp-optional ":"
					seconds
					(rexp-optional (rexp-optional s)
						       zone))))
	 (iso
	  (rexp-alternatives (rexp-sequence (rexp-optional yyyy "-")
					    iso-mm-dd
					    (string->char-set " T")
					    iso-time)
			     (rexp-sequence yyyy
					    "-"
					    iso-mm-dd)))
	 (western
	  (rexp-sequence
	   (rexp-alternatives (rexp-sequence month s+ dd)
			      (rexp-sequence dd (rexp-optional ".") s month))
	   s+
	   (rexp-alternatives HH:MM yyyy)))
	 (western-comma
	  (rexp-sequence month s+ dd "," s+ yyyy))
	 ;; Japanese MS-Windows ls-lisp has one-digit months, and
	 ;; omits the Kanji characters after month and day-of-month.
	 ;; On Mac OS X 10.3, the date format in East Asian locales is
	 ;; day-of-month digits followed by month digits.
	 (mm (rexp-sequence (rexp-optional (string->char-set " 01"))
			    digit))
	 (east-asian
	  (rexp-sequence
	   (rexp-alternatives (rexp-sequence mm l? s dd l? s+)
			      (rexp-sequence dd s mm s+))
	   (rexp-alternatives HH:MM
			      (rexp-sequence yyyy l?)))))
    ;; The "[0-9]" below requires the previous column to end in a digit.
    ;; This avoids recognizing `1 may 1997' as a date in the line:
    ;; -r--r--r--   1 may      1997        1168 Oct 19 16:49 README

    ;; The "[BkKMGTPEZY]?" below supports "ls -alh" output.
    ;; The ".*" below finds the last match if there are multiple matches.
    ;; This avoids recognizing `jservice  10  1024' as a date in the line:
    ;; drwxr-xr-x  3 jservice  10  1024 Jul  2  1997 esg-host

    ;; vc dired listings provide the state or blanks between file
    ;; permissions and date.  The state is always surrounded by
    ;; parentheses:
    ;; -rw-r--r-- (modified) 2005-10-22 21:25 files.el
    ;; Support for this was added to Edwin by cph.
    (rexp-compile
     (rexp-sequence (rexp* (rexp-any-char))
		    (rexp-alternatives
		     (rexp-sequence digit
				    (rexp-optional
				     (string->char-set "BkKMGTPEZY")))
		     ")")
		    s+
		    (rexp-alternatives western
				       western-comma
				       east-asian
				       iso)
		    s+))))

(define (dired-redisplay pathname #!optional mark)
  (let ((lstart
	 (mark-right-inserting-copy
	  (line-start (if (or (default-object? mark) (not mark))
			  (current-point)
			  mark)
		      0))))
    (let ((point-on-line? (mark= lstart (line-start (current-point) 0))))
      (with-read-only-defeated lstart
	(lambda ()
	  (let ((marker-char (mark-right-char lstart))
		(lstart* (mark-left-inserting-copy lstart)))
	    (if (pathname=? (buffer-default-directory (mark-buffer lstart))
			    (directory-pathname pathname))
		(begin
		  (insert-dired-entry! pathname lstart)
		  (delete-right-char lstart)
		  (insert-chars marker-char 1 lstart)))
	    (delete-string lstart* (line-start lstart* 1))
	    (mark-temporary! lstart*))))
      (if point-on-line?
	  (set-dired-point! lstart)))
    (mark-temporary! lstart)))

(define (dired-kill-files)
  (let ((filenames (dired-marked-files #f dired-flag-delete-char)))
    (if (and (not (null? filenames))
	     (cleanup-pop-up-buffers
	      (lambda ()
		(dired-pop-up-files-window filenames)
		(prompt-for-yes-or-no? "Delete these files"))))
	;; Must delete the files in reverse order so that the
	;; non-permanent marks remain valid as lines are deleted.
	(let loop ((filenames (reverse! filenames)) (failures '()))
	  (cond ((not (null? filenames))
		 (loop (cdr filenames)
		       (if (dired-kill-file! (caar filenames) (cdar filenames))
			   failures
			   (cons (file-namestring (caar filenames))
				 failures))))
		((not (null? failures))
		 (message "Deletions failed: " failures)))))))

(define (dired-pop-up-files-window filenames)
  (pop-up-temporary-buffer " *dired-files*" '(READ-ONLY SHRINK-WINDOW)
    (lambda (buffer window)
      (local-set-variable! truncate-partial-width-windows #f buffer)
      (write-strings-densely (map (lambda (filename)
				    (file-namestring (car filename)))
				  filenames)
			     (mark->output-port (buffer-point buffer))
			     (window-x-size
			      (or window (car (buffer-windows buffer))))))))

(define (dired-kill-file! filename lstart)
  (let ((deleted?
	 (if (let ((attributes (file-attributes-direct filename)))
	       (and attributes
		    (eq? #t (file-attributes/type attributes))))
	     (delete-directory-no-errors filename)
	     (delete-file-no-errors filename))))
    (if deleted?
	(with-read-only-defeated lstart
	  (lambda ()
	    (delete-string lstart (line-start lstart 1)))))
    deleted?))

(define dired-flag-delete-char #\D)
(define dired-marker-char #\*)

(define (dired-mark-files! buffer predicate)
  (for-each-file-line buffer
    (lambda (lstart)
      (if (predicate (dired-pathname lstart))
	  (dired-mark-1 lstart dired-marker-char)))))

(define (dired-marked-files #!optional mark marker-char)
  (let ((mark
	 (cond ((or (default-object? mark) (not mark))
		(buffer-start (current-buffer)))
	       ((buffer? mark)
		(buffer-start mark))
	       (else
		mark)))
	(marker-char
	 (if (or (default-object? marker-char) (not marker-char))
	     dired-marker-char
	     marker-char)))
    (let loop ((start (line-start mark 0)))
      (let ((continue
	     (lambda ()
	       (let ((next (line-start start 1 #f)))
		 (if next
		     (loop next)
		     '())))))
	(if (and (let ((char (mark-right-char start)))
		   (and char
			(if (eq? #t marker-char)
			    (not (char=? #\space char))
			    (char-ci=? marker-char char))))
		 (dired-filename-start start))
	    (cons (cons (dired-pathname start) start)
		  (continue))
	    (continue))))))

(define (dired-next-files n #!optional mark)
  (let ((mark
	 (cond ((or (default-object? mark) (not mark))
		(current-point))
	       ((buffer? mark)
		(if (selected-buffer? mark)
		    (current-point)
		    (buffer-point mark)))
	       (else mark))))
    (let loop ((start (line-start mark 0)) (n n))
      (if (<= n 0)
	  '()
	  (let ((continue
		 (lambda ()
		   (let ((next (line-start start 1 #f)))
		     (if next
			 (loop next (- n 1))
			 '())))))
	    (if (dired-filename-start start)
		(cons (cons (dired-pathname start) start)
		      (continue))
		(continue)))))))

(define (dired-this-file #!optional mark error?)
  (let ((mark
	 (cond ((or (default-object? mark) (not mark))
		(current-point))
	       ((buffer? mark)
		(if (selected-buffer? mark)
		    (current-point)
		    (buffer-point mark)))
	       (else mark))))
    (let ((start (line-start mark 0)))
      (if (dired-filename-start start)
	  (dired-pathname start)
	  (and error? (editor-error "No file on this line"))))))

(define (for-each-dired-mark buffer procedure)
  (for-each (lambda (file)
	      (procedure (car file))
	      (dired-mark-1 (cdr file) #\space)
	      (update-screens! #f))
	    (dired-marked-files buffer)))

(define (dired-change-files verb argument procedure)
  (let ((filenames
	 (if argument
	     (dired-next-files (command-argument-value argument))
	     (let ((files (dired-marked-files)))
	       (if (null? files)
		   (dired-next-files 1)
		   files)))))
    (if (null? filenames)
	(message "No files to " verb ".")
	(begin
	  (for-each (lambda (filename)
		      (set-cdr! filename
				(mark-right-inserting-copy (cdr filename))))
		    filenames)
	  (for-each (lambda (filename)
		      (procedure (car filename) (cdr filename))
		      (mark-temporary! (cdr filename)))
		    filenames)))
    (length filenames)))