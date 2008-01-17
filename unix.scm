#| -*-Scheme-*-

$Id: unix.scm,v 1.123 2007/01/05 21:19:24 cph Exp $

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

;;;; Unix Customizations for Edwin


(define-variable backup-by-copying-when-symlink
  "#T means use copying to create backups for a symbolic name.
This causes the actual names to refer to the latest version as edited.
'QUERY means ask whether to backup by copying and write through, or rename.
This variable is relevant only if  backup-by-copying  is #F."
  #f
  (lambda (object) (memq object '(#F #T QUERY))))

(define-variable backup-by-copying-when-linked
  "#T means use copying to create backups for files with multiple names.
This causes the alternate names to refer to the latest version as edited.
This variable is relevant only if  backup-by-copying  is false."
  #f
  boolean?)

(define-variable backup-by-copying-when-mismatch
  "#T means create backups by copying if this preserves owner or group.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner or group of the file;
that is, for files which are owned by you and whose group matches
the default for a new file created there by you.
This variable is relevant only if  Backup By Copying  is false."
  #f
  boolean?)

(define-variable version-control
  "Control use of version numbers for backup files.
#T means make numeric backup versions unconditionally.
#F means make them for files that have some already.
'NEVER means do not make them."
  #f
  (lambda (object) (memq object '(#F #T NEVER))))

(define-variable kept-old-versions
  "Number of oldest versions to keep when a new numbered backup is made."
  2
  exact-nonnegative-integer?)

(define-variable kept-new-versions
  "Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0."
  2
  (lambda (n) (and (exact-integer? n) (> n 0))))

(define (os/trim-pathname-string string prefix)
  (let ((index (string-match-forward prefix string)))
    (if (and index
	     (or (fix:= index (string-length prefix))
		 (and (fix:> index 0)
		      (char=? (string-ref prefix (fix:- index 1)) #\/)))
	     (re-substring-match "[/$~]" string index (string-length string)))
	(string-tail string index)
	string)))

(define (os/pathname->display-string pathname)
  ;; This code is more thorough than ENOUGH-PATHNAME, in that it
  ;; checks to see if one of the ancestor directories of PATHNAME is
  ;; an alias for the homedir.  ENOUGH-PATHNAME only does syntactic
  ;; comparison, and will not notice aliases.
  (let ((homedir (user-homedir-pathname)))
    (let loop ((directory (directory-pathname pathname)))
      (cond ((file-test-no-errors file-eq? directory homedir)
	     (string-append
	      "~/"
	      (->namestring (enough-pathname pathname directory))))
	    ((equal? (pathname-directory directory) '(ABSOLUTE))
	     (->namestring pathname))
	    (else
	     (loop
	      (directory-pathname (directory-pathname-as-file directory))))))))

(define (os/auto-save-pathname pathname buffer)
  (let ((wrap
	 (lambda (name directory)
	   (merge-pathnames (string-append "#" name "#") directory))))
    (if (not pathname)
	(wrap (string-append "%" (buffer-name buffer))
	      (buffer-default-directory buffer))
	(wrap (file-namestring pathname)
	      (directory-pathname pathname)))))

(define (os/precious-backup-pathname pathname)
  (let ((directory (directory-pathname pathname)))
    (let loop ((i 0))
      (let ((pathname
	     (merge-pathnames (string-append "#tmp#" (number->string i))
			      directory)))
	(if (allocate-temporary-file pathname)
	    pathname
	    (loop (+ i 1)))))))

(define (os/backup-buffer? truename)
  (memv (string-ref (file-attributes/mode-string (file-attributes truename)) 0)
	'(#\- #\l)))

(define (os/default-backup-filename)
  "~/%backup%~")

(define (os/truncate-filename-for-modeline filename width)
  (let ((length (string-length filename)))
    (if (< 0 width length)
	(let ((result
	       (substring
		filename
		(let ((index (- length width)))
		  (or (and (not (char=? #\/ (string-ref filename index)))
			   (string-index filename #\/ index length))
		      (1+ index)))
		length)))
	  (string-set! result 0 #\$)
	  result)
	filename)))

(define (os/backup-by-copying? truename buffer)
  (let ((attributes (file-attributes truename)))
    (or (and (ref-variable backup-by-copying-when-linked buffer)
	     (> (file-attributes/n-links attributes) 1))
	(let ((flag (ref-variable backup-by-copying-when-symlink buffer)))
	  (and flag
	       (string? (file-attributes/type attributes))
	       (or (not (eq? flag 'QUERY))
		   (prompt-for-confirmation?
		    (string-append "Write through symlink to "
				   (->namestring
				    (enough-pathname
				     (pathname-simplify
				      (merge-pathnames
				       (file-attributes/type attributes)
				       (buffer-pathname buffer)))
				     (buffer-default-directory buffer))))))))
	(and (ref-variable backup-by-copying-when-mismatch buffer)
	     (not (and (= (file-attributes/uid attributes)
			  (unix/current-uid))
		       (= (file-attributes/gid attributes)
			  (unix/current-gid))))))))

(define (os/newest-numeric-backup pathname)
  (define (first-loop filenames)
    (if (null? filenames)
        #f
        (cond ((os/numeric-backup-filename? (car filenames))
               => (lambda (root.version)
                    (max-loop (cdr filenames)
                              (car filenames)
                              (cdr root.version))))
              (else (first-loop (cdr filenames))))))
  (define (max-loop filenames filename version)
    (if (null? filenames)
        filename
        (cond ((os/numeric-backup-filename? (car filenames))
               => (lambda (root.version)
                    (if (> (cdr root.version) version)
                        (max-loop (cdr filenames)
                                  (car filenames)
                                  (cdr root.version))
                        (max-loop (cdr filenames) filename version))))
              (else (max-loop (cdr filenames) filename version)))))
  (first-loop (os/directory-list-completions
               (directory-namestring pathname)
               (string-append (file-namestring pathname) ".~"))))

(define (os/newest-backup pathname)
  (or (os/newest-numeric-backup pathname)
      (find os/backup-filename?
	    (os/directory-list-completions
	     (directory-namestring pathname)
	     (string-append (file-namestring pathname) "~")))))

(define (os/buffer-backup-pathname truename buffer)
  (call-with-values
      (lambda ()
	(let ((type (pathname-type truename)))
	  (if (member type os/encoding-pathname-types)
	      (values (pathname-new-type truename #f)
		      (string-append "~." type))
	      (values truename "~"))))
    (lambda (truename suffix)
      (if (eq? 'NEVER (ref-variable version-control buffer))
	  (values (unix/make-backup-pathname truename #f suffix) '())
	  (let ((prefix (string-append (file-namestring truename) ".~")))
	    (let ((backups
		   (let loop
		       ((filenames
			 (os/directory-list-completions
			  (directory-namestring truename)
			  prefix))
			(backups '()))
		     (if (null? filenames)
			 (sort backups (lambda (x y) (< (cdr x) (cdr y))))
			 (loop (cdr filenames)
			       (let ((root.version
				      (os/numeric-backup-filename?
				       (car filenames))))
				 (if root.version
				     (cons (cons (car filenames)
						 (cdr root.version))
					   backups)
				     backups)))))))
	      (if (null? backups)
		  (values (unix/make-backup-pathname
			   truename
			   (and (ref-variable version-control buffer) 1)
			   suffix)
			  '())
		  (values (unix/make-backup-pathname
			   truename
			   (+ (apply max (map cdr backups)) 1)
			   suffix)
			  (let ((start (ref-variable kept-old-versions buffer))
				(end
				 (- (length backups)
				    (- (ref-variable kept-new-versions buffer)
				       1))))
			    (if (< start end)
				(map (let ((dir (directory-pathname truename)))
				       (lambda (entry)
					 (merge-pathnames (car entry) dir)))
				     (sublist backups start end))
				'()))))))))))

(define (unix/make-backup-pathname pathname version suffix)
  (string-append (->namestring pathname)
		 (if version
		     (string-append ".~" (number->string version) suffix)
		     suffix)))

(define (os/directory-list directory)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (os/directory-list-completions directory prefix)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read-matching channel prefix)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define os/encoding-pathname-types
  '("Z" "gz" "bz2" "bf"))

(define unix/backup-suffixes
  (cons "~"
	(map (lambda (type) (string-append "~." type))
	     os/encoding-pathname-types)))

(define (os/backup-filename? filename)
  (let ((end (string-length filename)))
    (let loop ((suffixes unix/backup-suffixes))
      (and (not (null? suffixes))
	   (or (let ((suffix (car suffixes)))
		 (let ((start (fix:- end (string-length suffix))))
		   (and (fix:> start 0)
			(let loop ((suffix-index 0) (index start))
			  (if (fix:= index end)
			      start
			      (and (char=? (string-ref suffix suffix-index)
					   (string-ref filename index))
				   (loop (fix:+ suffix-index 1)
					 (fix:+ index 1))))))))
	       (loop (cdr suffixes)))))))

(define (os/numeric-backup-filename? filename)
  (let ((suffix (os/backup-filename? filename)))
    (and suffix
	 (fix:>= suffix 4)
	 (let loop ((index (fix:- suffix 2)))
	   (and (fix:>= index 2)
		(if (char-numeric? (string-ref filename index))
		    (loop (fix:- index 1))
		    (and (char=? (string-ref filename index) #\~)
			 (char=? (string-ref filename (fix:- index 1)) #\.)
			 (cons (string-head filename (fix:- index 1))
			       (substring->number filename
						  (fix:+ index 1)
						  suffix)))))))))

(define (os/completion-ignore-filename? filename)
  (and (not (file-test-no-errors file-directory? filename))
       (any (lambda (extension)
	      (string-suffix? extension filename))
	    (ref-variable completion-ignored-extensions))))

(define (os/completion-ignored-extensions)
  (append (list ".bin" ".com" ".ext"
		".inf" ".bif" ".bsm" ".bci" ".bcs"
		".psb" ".moc" ".fni"
		".bco" ".bld" ".bad" ".glo" ".fre"
		".o" ".elc" ".bin" ".lbin" ".fasl"
		".dvi" ".toc" ".log" ".aux"
		".lof" ".blg" ".bbl" ".glo" ".idx" ".lot")
	  (list-copy unix/backup-suffixes)))

(define-variable completion-ignored-extensions
  "Completion ignores filenames ending in any string in this list."
  (os/completion-ignored-extensions)
  (lambda (extensions)
    (and (list? extensions)
	 (every (lambda (extension)
		  (and (string? extension)
		       (not (string-null? extension))))
		extensions))))

(define (os/init-file-name) "~/.edwin")
(define (os/abbrev-file-name) "~/.abbrev_defs")

(define (os/find-file-initialization-filename pathname)
  (or (and (equal? "scm" (pathname-type pathname))
	   (let ((pathname (pathname-new-type pathname "ffi")))
	     (and (file-exists? pathname)
		  pathname)))
      (let ((pathname
	     (merge-pathnames ".edwin-ffi" (directory-pathname pathname))))
	(and (file-exists? pathname)
	     pathname))))

(define (os/auto-save-filename? filename)
  ;; This could be more sophisticated, but is what the edwin
  ;; code was originally doing.
  (and (string? filename)
       (string-index filename #\#)))

(define (os/read-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (pathname mark visit?)
	  visit?
	  (let ((type (pathname-type pathname)))
	    (cond ((equal? "gz" type)
		   (read-compressed-file "gzip -d" pathname mark))
		  ((equal? "bz2" type)
		   (read-compressed-file "bzip2 -d" pathname mark))
		  ((equal? "Z" type)
		   (read-compressed-file "uncompress" pathname mark))))))
    ,@(os-independent/read-file-methods)))

(define (os/write-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (region pathname visit?)
	  visit?
	  (let ((type (pathname-type pathname)))
	    (cond ((equal? "gz" type)
		   (write-compressed-file "gzip" region pathname))
		  ((equal? "bz2" type)
		   (write-compressed-file "bzip2" region pathname))
		  ((equal? "Z" type)
		   (write-compressed-file "compress" region pathname))))))
    ,@(os-independent/write-file-methods)))

(define (os/alternate-pathnames group pathname)
  (if (ref-variable enable-compressed-files group)
      (append (map (let ((filename (->namestring pathname)))
		     (lambda (suffix)
		       (string-append filename "." suffix)))
		   unix/compressed-file-suffixes)
	      (os-independent/alternate-pathnames group pathname))
      '()))

;;;; Compressed Files

(define-variable enable-compressed-files
  "If #T, compressed files are automatically uncompressed when read,
and recompressed when written.  A compressed file is identified by one
of the filename suffixes \".gz\", \".bz2\", or \".Z\"."
  #t
  boolean?)

(define (read/write-compressed-file? group pathname)
  (and (ref-variable enable-compressed-files group)
       (member (pathname-type pathname) unix/compressed-file-suffixes)))

(define unix/compressed-file-suffixes
  '("gz" "bz2" "Z"))

(define (read-compressed-file program pathname mark)
  ((message-wrapper #f "Uncompressing file " (->namestring pathname))
   (lambda ()
     (call-with-temporary-file-pathname
      (lambda (temporary)
	(if (not (equal? '(EXITED . 0)
			 (shell-command #f #f
					(directory-pathname pathname)
					#f
					(string-append
					 program
					 " < "
					 (file-namestring pathname)
					 " > "
					 (->namestring temporary)))))
	    (error:file-operation pathname
				  program
				  "file"
				  "[unknown]"
				  read-compressed-file
				  (list pathname mark)))
	(group-insert-file! (mark-group mark)
			    (mark-index mark)
			    temporary))))))

(define (write-compressed-file program region pathname)
  ((message-wrapper #f "Compressing file " (->namestring pathname))
   (lambda ()
     (if (not (equal?
	       '(EXITED . 0)
	       (shell-command region
			      #f
			      (directory-pathname pathname)
			      #f
			      (string-append program
					     " > "
					     (file-namestring pathname)))))
	 (error:file-operation pathname
			       program
			       "file"
			       "[unknown]"
			       write-compressed-file
			       (list region pathname))))))

;;;; Dired customization

(define-variable dired-listing-switches
  "Switches passed to ls for dired.  MUST contain the 'l' option.
CANNOT contain the 'F' option."
  "-al"
  string?)

(define-variable list-directory-brief-switches
  "Switches for list-directory to pass to `ls' for brief listing,"
  "-CF"
  string?)

(define-variable list-directory-verbose-switches
  "Switches for list-directory to pass to `ls' for verbose listing,"
  "-l"
  string?)

(define-variable insert-directory-program
  "Absolute or relative name of the `ls' program used by `insert-directory'."
  "ls"
  string?)

(define (insert-directory! file switches mark type)
  ;; Insert directory listing for FILE, formatted according to SWITCHES.
  ;; The listing is inserted at MARK.
  ;; TYPE can have one of three values:
  ;;   'WILDCARD means treat FILE as shell wildcard.
  ;;   'DIRECTORY means FILE is a directory and a full listing is expected.
  ;;   'FILE means FILE itself should be listed, and not its contents.
  ;; SWITCHES must not contain "-d".
  (let ((directory (directory-pathname (merge-pathnames file)))
	(program (ref-variable insert-directory-program mark))
	(switches
	 (if (eq? 'DIRECTORY type)
	     switches
	     (string-append-separated "-d" switches))))
    (if (eq? 'WILDCARD type)
	(shell-command #f mark directory #f
		       (string-append program
				      " "
				      switches
				      " "
				      (file-namestring file)))
	(apply run-synchronous-process
	       #f mark directory #f
	       (os/find-program program #f (ref-variable exec-path))
	       (append
		(split-unix-switch-string switches)
		(list
		 (if (eq? 'DIRECTORY type)
		     ;; If FILE is a symbolic link, this reads the
		     ;; directory that it points to.
		     (->namestring
		      (pathname-new-directory file
					      (append (pathname-directory file)
						      (list "."))))
		     (file-namestring file))))))))

(define (split-unix-switch-string switches)
  (let ((end (string-length switches)))
    (let loop ((start 0))
      (if (fix:< start end)
	  (let ((space (string-index switches #\space start end)))
	    (if space
		(cons (substring switches start space)
		      (loop (fix:+ space 1)))
		(list (substring switches start end))))
	  '()))))

(define (dired-pathname-wild? pathname)
  (let ((namestring (file-namestring pathname)))
    (or (string-index index namestring #\*)
	(string-index index namestring #\?)
	(string-index index namestring #\[))))

;;;; Subprocess/Shell Support

(define (os/shell-name pathname)
  (file-namestring pathname))

(define (os/default-shell-args)
  '("-i"))

(define-variable explicit-csh-args
  "Args passed to inferior shell by M-x shell, if the shell is csh.
Value is a list of strings."
  (if (string=? microcode-id/operating-system-variant "HP-UX")
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
      '("-i")))

(define (os/default-shell-prompt-pattern)
  "^[^#$>]*[#$>] *")

(define (os/comint-filename-region start point end)
  (let ((chars "~/A-Za-z0-9---_.$#,"))
    (let ((start (skip-chars-backward chars point start)))
      (make-region start (skip-chars-forward chars start end)))))

(define (os/shell-command-separators)
  ";&|")

(define (os/shell-command-regexp)
  (string-append "[^" (os/shell-command-separators) "\n]+"))

;;;; POP Mail

(define-variable rmail-pop-delete
  "If #T, messages are deleted from the POP server after being retrieved.
Otherwise, messages remain on the server and will be re-fetched later."
  #t
  boolean?)

(define-variable rmail-popclient-is-debian
  "If #T, the popclient running on this machine is Debian popclient.
Otherwise, it is the standard popclient.  Debian popclient differs from
standard popclient in that it does not accept the -p <password>
option, instead taking -P <filename>."
  #f
  boolean?)

(define (os/rmail-pop-procedure)
  (and (os/find-program "popclient" #f (ref-variable exec-path) #f)
       (lambda (server user-name password directory)
	 (unix/pop-client server user-name password directory))))

(define (unix/pop-client server user-name password directory)
  (let ((target (->namestring (merge-pathnames ".popmail" directory))))
    (let ((buffer (temporary-buffer "*popclient*")))
      (cleanup-pop-up-buffers
       (lambda ()
	 (pop-up-buffer buffer #f)
	 (let ((status.reason
		(unix/call-with-pop-client-password-options password
		  (lambda (password-options)
		    (let ((args
			   (append (list "-u" user-name)
				   password-options
				   (list "-o" target server))))
		      (apply run-synchronous-process
			     #f (cons (buffer-end buffer) #t) #f #f
			     "popclient"
			     "-3"
			     (if (ref-variable rmail-pop-delete)
				 args
				 (cons "-k" args))))))))
	   (if (and (eq? 'EXITED (car status.reason))
		    (memv (cdr status.reason) '(0 1)))
	       (kill-pop-up-buffer buffer)
	       (begin
		 (keep-pop-up-buffer buffer)
		 (editor-error "Error getting mail from POP server.")))))))
    target))

(define (unix/call-with-pop-client-password-options password receiver)
  (cond ((eq? password 'KERBEROS-V4)
	 (receiver (list "-K")))
	((ref-variable rmail-popclient-is-debian)
	 (cond ((string? password)
		(call-with-temporary-filename
		 (lambda (temporary-file)
		   (set-file-modes! temporary-file #o600)
		   (call-with-output-file temporary-file
		     (lambda (port)
		       (write-string password port)
		       (newline port)))
		   (receiver (list "-P" temporary-file)))))
	       ((and (pair? password) (eq? 'FILE (car password)))
		(receiver (list "-P" (cadr password))))
	       (else
		(error "Illegal password:" password))))
	(else
	 (cond ((string? password)
		(receiver (list "-p" password)))
	       ((and (pair? password) (eq? 'FILE (car password)))
		(receiver
		 (list "-p"
		       (call-with-input-file (cadr password)
			 (lambda (port)
			   (read-string (char-set #\newline) port))))))
	       (else
		(error "Illegal password:" password))))))

;;;; Miscellaneous

(define (os/scheme-can-quit?)
  (subprocess-job-control-available?))

(define (os/quit dir)
  dir					; ignored
  (%quit))

(define (os/set-file-modes-writeable! pathname)
  (set-file-modes! pathname #o777))

(define os/restore-modes-to-updated-file!
  set-file-modes!)

(define (os/rmail-spool-directory)
  (or (find file-directory?
	    '("/var/spool/mail/" "/var/mail/" "/usr/spool/mail/" "/usr/mail/"))
      "/usr/spool/mail/"))

(define (os/rmail-primary-inbox-list system-mailboxes)
  (cons "~/mbox" system-mailboxes))

(define (os/sendmail-program)
  (or (os/find-program "sendmail" #f (ref-variable exec-path) #f)
      (find file-executable?
	    '("/usr/sbin/sendmail" "/usr/lib/sendmail" "/usr/ucblib/sendmail"))
      "fakemail"))

(define (os/newsrc-file-name server)
  (let ((homedir (user-homedir-pathname)))
    (let ((specific
	   (merge-pathnames (string-append ".newsrc-" server) homedir)))
      (if (file-exists? specific)
	  specific
	  (merge-pathnames ".newsrc" homedir)))))

(define (os/info-default-directory-list)
  (list "/usr/local/lib/info"
	"/usr/local/info"
	"/usr/share/info"
	"/usr/info"))