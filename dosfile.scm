#| -*-Scheme-*-

$Id: dosfile.scm,v 1.47 2007/01/05 21:19:23 cph Exp $

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

;;;; DOS-Syntax File Customizations


(define-variable version-control
  "Control use of version numbers for backup files.
#T means make numeric backup versions unconditionally.
#F means make them for files that have some already.
'NEVER means do not make them."
  #f
  (lambda (object) (or (eq? object 'NEVER) (boolean? object))))

(define-variable kept-old-versions
  "Number of oldest versions to keep when a new numbered backup is made."
  2
  exact-nonnegative-integer?)

(define-variable kept-new-versions
  "Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0."
  2
  (lambda (n) (and (exact-integer? n) (> n 0))))

(define os/encoding-pathname-types
  '("gz" "bf" "ky"))

(define dos/backup-suffixes
  (cons "~"
	(map (lambda (type) (string-append "~." type))
	     os/encoding-pathname-types)))

(define-variable completion-ignored-extensions
  "Completion ignores filenames ending in any string in this list."
  (append (list ".bin" ".com" ".ext"
		".inf" ".bif" ".bsm" ".bci" ".bcs"
		".psb" ".moc" ".fni"
		".bco" ".bld" ".bad" ".glo" ".fre"
		".obj" ".exe" ".pif" ".grp"
		".dvi" ".toc" ".log" ".aux")
	  (list-copy dos/backup-suffixes))
  (lambda (extensions)
    (and (list? extensions)
	 (for-all? extensions
	   (lambda (extension)
	     (and (string? extension)
		  (not (string-null? extension))))))))

;;;; Filename I/O

(define (os/trim-pathname-string string prefix)
  (let ((index (string-match-forward prefix string)))
    (if (and index
	     (or (fix:= index (string-length prefix))
		 (and (fix:> index 0)
		      (or (char=? (string-ref prefix (fix:- index 1)) #\/)
			  (char=? (string-ref prefix (fix:- index 1)) #\\))))
	     (re-substring-match "[\\/$~]\\|[a-zA-Z]:"
				 string index (string-length string)))
	(string-tail string index)
	string)))

(define (os/pathname->display-string pathname)
  (or (let ((relative (enough-pathname pathname (user-homedir-pathname))))
	(and (not (pathname-device relative))
	     (not (pathname-absolute? relative))
	     (string-append "~\\" (->namestring relative))))
      (->namestring pathname)))

(define (os/truncate-filename-for-modeline filename width)
  (let ((length (string-length filename)))
    (if (< 0 width length)
	(let ((result
	       (substring
		filename
		(let ((index (- length width)))
		  (if (char=? #\\ (string-ref filename index))
		      index
		      (or (substring-find-next-char filename index length #\\)
			  (fix:+ index 1))))
		length)))
	  (string-set! result 0 #\$)
	  result)
	filename)))

(define (os/directory-list directory)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons (begin (string-downcase! name) name) result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (os/directory-list-completions directory prefix)
  (let ((channel (directory-channel-open directory)))
    (let loop ((result '()))
      (let ((name (directory-channel-read-matching channel prefix)))
	(if name
	    (loop (cons (begin (string-downcase! name) name) result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

;;;; Dired customization

(define-variable dired-listing-switches
  "Dired listing format.
Recognized switches are:
    -a	show all files including system and hidden files
    -t	sort files according to modification time
    -l	ignored (but allowed for unix compatibility)
Switches may be concatenated, e.g. `-lt' is equivalent to `-l -t'."
  "-l"
  string?)

(define-variable list-directory-brief-switches
  "list-directory brief listing format.
Recognized switches are:
    -a	show all files including system and hidden files
    -t	sort files according to modification time
    -l	ignored (but allowed for unix compatibility)
Switches may be concatenated, e.g. `-lt' is equivalent to `-l -t'."
  ""
  string?)

(define-variable list-directory-verbose-switches
  "list-directory verbose listing format.
Recognized switches are:
    -a	show all files including system and hidden files
    -t	sort files according to modification time
    -l	ignored (but allowed for unix compatibility)
Switches may be concatenated, e.g. `-lt' is equivalent to `-l -t'."
  "-l"
  string?)

(define (insert-directory! file switches mark type)
  ;; Insert directory listing for FILE at MARK.
  ;; SWITCHES are examined for the presence of "a" and "t".
  ;; TYPE can have one of three values:
  ;;   'WILDCARD means treat FILE as shell wildcard.
  ;;   'DIRECTORY means FILE is a directory and a full listing is expected.
  ;;   'FILE means FILE itself should be listed, and not its contents.
  (let ((mark (mark-left-inserting-copy mark))
	(now (get-universal-time)))
    (catch-file-errors (lambda (c)
			 (insert-string (condition/report-string c) mark)
			 (insert-newline mark))
      (lambda ()
	(for-each
	 (lambda (entry)
	   (insert-string (dos/dired-line-string (car entry) (cdr entry) now)
			  mark)
	   (insert-newline mark))
	 (if (eq? 'FILE type)
	     (let ((attributes (file-attributes file)))
	       (if attributes
		   (list (cons (file-namestring file) attributes))
		   '()))
	     (sort (dos/read-dired-files file
					 (string-find-next-char switches #\a))
		   (if (string-find-next-char switches #\t)
		       (lambda (x y)
			 (> (file-attributes/modification-time (cdr x))
			    (file-attributes/modification-time (cdr y))))
		       (lambda (x y)
			 (string-ci<? (car x) (car y)))))))))
    (mark-temporary! mark)))

(define (dos/dired-line-string name attr now)
  (string-append
   (file-attributes/mode-string attr)
   " "
   (string-pad-left (number->string (file-attributes/length attr)) 10 #\space)
   " "
   (file-time->ls-string (file-attributes/modification-time attr) now)
   " "
   name))

(define dired-pathname-wild?
  pathname-wild?)

;;;; Backup and Auto-Save Filenames

(define (os/buffer-backup-pathname truename buffer)
  (call-with-values
      (lambda ()
	(if (dos/fs-long-filenames? truename)
	    (let ((type (pathname-type truename)))
	      (if (member type os/encoding-pathname-types)
		  (values (pathname-new-type truename #f)
			  (string-append "~." type))
		  (values truename "~")))
	    (values truename "")))
    (lambda (truename suffix)
      (if (eq? 'NEVER (ref-variable version-control buffer))
	  (values (dos/make-backup-pathname truename #f suffix) '())
	  (let ((prefix
		 (if (dos/fs-long-filenames? truename)
		     (string-append (file-namestring truename) ".~")
		     (string-append (pathname-name truename) "."))))
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
		  (values (dos/make-backup-pathname
			   truename
			   (and (ref-variable version-control buffer) 1)
			   suffix)
			  '())
		  (values (dos/make-backup-pathname
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

(define (dos/make-backup-pathname pathname version suffix)
  (if (dos/fs-long-filenames? pathname)
      (string-append (->namestring pathname)
		     (if version
			 (string-append ".~" (number->string version) suffix)
			 suffix))
      (pathname-new-type pathname
			 (if (and version (< version 1000))
			     (let ((type (pathname-type pathname))
				   (vs (number->string version)))
			       (if (and (< version 100)
					(string? type)
					(not (string-null? type)))
				   (string-append (substring type 0 1)
						  (string-pad-left vs 2 #\0))
				   (string-pad-left vs 3 #\0)))
			     "bak"))))

(define (os/default-backup-filename)
  "$TMP\\edwin.bak")

(define (os/backup-filename? filename)
  (or (there-exists? dos/backup-suffixes
	(lambda (suffix)
	  (string-suffix? suffix filename)))
      (let ((type (pathname-type filename)))
	(and (string? type)
	     (or (string-ci=? "bak" type)
		 (re-string-match ".[0-9][0-9]" type))))))

(define (os/numeric-backup-filename? filename)
  (let ((r
	 (let ((try
		(lambda (pattern)
		  (re-string-search-forward pattern filename))))
	   (or (try "^\\([^.]+\\)\\.\\([0-9][0-9][0-9]\\)$")
	       (try "^\\([^.]+\\.[^.]\\)\\([0-9][0-9]\\)$")
	       (let loop ((suffixes dos/backup-suffixes))
		 (and (pair? suffixes)
		      (or (try (string-append "^\\(.+\\)\\.~\\([0-9]+\\)"
					      (re-quote-string (car suffixes))
					      "$"))
			  (loop (cdr suffixes)))))))))
    (and r
	 (let ((root-start (re-match-start-index 1 r))
	       (root-end (re-match-end-index 1 r))
	       (version-start (re-match-start-index 2 r))
	       (version-end (re-match-end-index 2 r)))
	   (let ((version
		  (substring->number filename version-start version-end)))
	     (and (> version 0)
		  (cons (substring filename root-start root-end)
			version)))))))

(define (os/auto-save-filename? filename)
  (if (dos/fs-long-filenames? filename)
      (re-string-match "^#.+#$" (file-namestring filename))
      (let ((type (pathname-type filename)))
	(and (string? type)
	     (string-ci=? "sav" type)))))

(define (os/precious-backup-pathname pathname)
  (let ((directory (directory-pathname pathname)))
    (let loop ((i 0))
      (let ((pathname
	     (merge-pathnames (string-append "#tmp#" (number->string i))
			      directory)))
	(if (allocate-temporary-file pathname)
	    (begin
	      (deallocate-temporary-file pathname)
	      pathname)
	    (loop (+ i 1)))))))

(define (os/auto-save-pathname pathname buffer)
  (let ((pathname
	 (or pathname
	     (merge-pathnames (dos/buffer-auto-save-name buffer)
			      (buffer-default-directory buffer)))))
    (if (dos/fs-long-filenames? pathname)
	(merge-pathnames (string-append "#" (file-namestring pathname) "#")
			 (directory-pathname pathname))
	(pathname-new-type pathname "sav"))))

(define (dos/buffer-auto-save-name buffer)
  (string-append
   "%"
   (let ((directory (buffer-default-directory buffer)))
     (cond ((not (dos/fs-long-filenames? directory))
	    (let ((name (dos/buffer-short-name buffer char-set:valid-fat)))
	      (if (string-null? name)
		  "buffer%"
		  name)))
	   ((string-ci=? "hpfs" (car (dos/fs-drive-type directory)))
	    (dos/buffer-long-name buffer char-set:valid-hpfs))
	   (else
	    (dos/buffer-long-name buffer char-set:valid-windows-long))))))

(define (dos/buffer-long-name buffer valid-chars)
  (let ((name (buffer-name buffer)))
    (let ((length (string-length name)))
      (let ((copy (make-string length)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i length))
	  (string-set!
	   copy i
	   (let ((char (string-ref name i)))
	     (if (char-set-member? valid-chars char)
		 char
		 #\_))))
	copy))))

(define (dos/buffer-short-name buffer valid-chars)
  (let ((name
	 (list->string
	  (let loop ((chars (string->list (buffer-name buffer))))
	    (cond ((null? chars)
		   '())
		  ((char-set-member? valid-chars (car chars))
		   (cons (car chars) (loop (cdr chars))))
		  (else
		   (loop (cdr chars))))))))
    (let ((n (string-length name)))
      (if (fix:<= n 7)
	  name
	  (string-head name 7)))))

(define char-set:valid-hpfs)
(define char-set:valid-windows-long)
(let ((reserved-chars
       (char-set-union (string->char-set "\"/:<>\\|")
		       (string->char-set "*?"))))
  (set! char-set:valid-hpfs
	(char-set-difference (ascii-range->char-set #x21 #x7F)
			     reserved-chars))
  (set! char-set:valid-windows-long
	(char-set-difference (ascii-range->char-set #x20 #x100)
			     reserved-chars)))

(define char-set:valid-fat
  (char-set-union char-set:alphanumeric
		  (string->char-set "!#$%'()-@^_`{}~")))

;;;; Miscellaneous

(define (os/backup-buffer? truename)
  (let ((attrs (file-attributes truename)))
    (and attrs
	 (eq? #f (file-attributes/type attrs)))))

(define (os/backup-by-copying? truename buffer)
  truename buffer
  #f)

(define (os/completion-ignore-filename? filename)
  (or (os/backup-filename? filename)
      (os/auto-save-filename? filename)
      (and (not (file-directory? filename))
	   (there-exists? (ref-variable completion-ignored-extensions)
   	     (lambda (extension)
	       (string-suffix? extension filename))))))

(define (os/init-file-name) "~/edwin.ini")
(define (os/abbrev-file-name) "~/abbrevs.scm")

(define (os/find-file-initialization-filename pathname)
  (or (and (equal? "scm" (pathname-type pathname))
	   (let ((pathname (pathname-new-type pathname "ffi")))
	     (and (file-exists? pathname)
		  pathname)))
      (let ((pathname
	     (merge-pathnames "edwin.ffi" (directory-pathname pathname))))
	(and (file-exists? pathname)
	     pathname))))

(define (os/newsrc-file-name server)
  (let ((homedir (user-homedir-pathname)))
    (if (dos/fs-long-filenames? homedir)
	(let ((specific
	       (merge-pathnames (string-append ".newsrc-" server) homedir)))
	  (if (file-exists? specific)
	      specific
	      (merge-pathnames ".newsrc" homedir)))
	(merge-pathnames "newsrc.ini" homedir))))

(define (os/info-default-directory-list)
  '())

;;;; Subprocess/Shell Support

(define (os/shell-name pathname)
  (if (member (pathname-type pathname) (os/executable-pathname-types))
      (pathname-name pathname)
      (file-namestring pathname)))

(define (os/default-shell-args)
  '())

(define (os/default-shell-prompt-pattern)
  "^[^]$]*[]>] *")

(define (os/comint-filename-region start point end)
  (let ((chars "]\\\\A-Za-z0-9!#$%&'()+,.:;=@[^_`{}~---"))
    (let ((start (skip-chars-backward chars point start)))
      (make-region start (skip-chars-forward chars start end)))))

(define (os/shell-command-separators)
  "&|")

(define (os/shell-command-regexp)
  (string-append "[^" (os/shell-command-separators) "\n]+"))

;;;; File-Encoding Methods

(define (os/read-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (pathname mark visit?)
	  visit?
	  (read-compressed-file "gzip" '("-d") pathname mark)))
    ,@(os-independent/read-file-methods)))

(define (os/write-file-methods)
  `((,read/write-compressed-file?
     . ,(lambda (region pathname visit?)
	  visit?
	  (write-compressed-file "gzip" '() region pathname)))
    ,@(os-independent/write-file-methods)))

(define (os/alternate-pathnames group pathname)
  (if (dos/fs-long-filenames? pathname)
      (append (if (and (ref-variable enable-compressed-files group)
		       (not (equal? "gz" (pathname-type pathname))))
		  (list (string-append (->namestring pathname) ".gz"))
		  '())
	      (os-independent/alternate-pathnames group pathname))
      '()))

;;;; Compressed Files

(define-variable enable-compressed-files
  "If true, compressed files are automatically uncompressed when read,
and recompressed when written.  A compressed file is identified by the
filename suffix \".gz\"."
  #t
  boolean?)

(define (read/write-compressed-file? group pathname)
  (and (ref-variable enable-compressed-files group)
       (equal? "gz" (pathname-type pathname))))

(define (read-compressed-file program arguments pathname mark)
  ((message-wrapper #f "Uncompressing file " (->namestring pathname))
   (lambda ()
     (call-with-temporary-file-pathname
      (lambda (temporary)
	(if (not (equal? '(EXITED . 0)
			 (shell-command #f #f
					(directory-pathname pathname)
					#f
					(string-append
					 (quote-program program arguments)
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

(define (write-compressed-file program arguments region pathname)
  ((message-wrapper #f "Compressing file " (->namestring pathname))
   (lambda ()
     (if (not (equal?
	       '(EXITED . 0)
	       (shell-command region
			      #f
			      (directory-pathname pathname)
			      #f
			      (string-append (quote-program program arguments)
					     " > "
					     (file-namestring pathname)))))
	 (error:file-operation pathname
			       program
			       "file"
			       "[unknown]"
			       write-compressed-file
			       (list region pathname))))))

(define (quote-program program arguments)
  (string-append (if (eq? 'NT microcode-id/operating-system)
		     (string-append "\"" (os/find-program program #f) "\"")
		     program)
		 (apply string-append
			(map (lambda (argument)
			       (string-append " " argument))
			     arguments))))