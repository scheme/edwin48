#| -*-Scheme-*-

$Id: abbrev.scm,v 1.13 2008/01/30 20:01:58 cph Exp $

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

;;;; Abbrev Mode


;;;; Low-level data structures

(define make-abbrev-table (lambda () make-hash-table string=? string-hash))
(define abbrev-table? hash-table?)

(define-structure (abbrev-entry (type-descriptor <abbrev-entry>))
  (expansion #f read-only #t)
  (hook #f read-only #t)
  (count 0))

(define (guarantee-abbrev-table object caller)
  (if (not (abbrev-table? object))
      (error:wrong-type-argument object "abbrev table" caller)))

(define (clear-abbrev-table table)
  (set! abbrevs-changed? #t)
  (hash-table/clear! table))

(define (define-abbrev table abbrev expansion #!optional hook count)
  (let ((hook (if (default-object? hook) #f hook))
	(count (if (default-object? count) 0 count)))
    (guarantee-abbrev-table table 'DEFINE-ABBREV)
    (guarantee-string abbrev 'DEFINE-ABBREV)
    (guarantee-string expansion 'DEFINE-ABBREV)
    (if hook (guarantee-symbol hook 'DEFINE-ABBREV))
    (guarantee-exact-nonnegative-integer count 'DEFINE-ABBREV)
    (set! abbrevs-changed? #t)
    (hash-table-set! table
		     (string-downcase abbrev)
		     (make-abbrev-entry expansion hook count))))

(define (define-global-abbrev abbrev expansion)
  (define-abbrev (ref-variable global-abbrev-table #f) abbrev expansion))

(define (define-mode-abbrev abbrev expansion)
  (let ((table (ref-variable local-abbrev-table)))
    (if (not table)
	(error "Major mode has no abbrev table."))
    (define-abbrev table abbrev expansion)))

(define (undefine-abbrev table abbrev)
  (guarantee-abbrev-table table 'UNDEFINE-ABBREV)
  (guarantee-string abbrev 'UNDEFINE-ABBREV)
  (set! abbrevs-changed? #t)
  (hash-table-delete! table (string-downcase abbrev)))

(define (abbrev-entry abbrev where)
  (let ((abbrev
	 (string-downcase
	  (cond ((string? abbrev) abbrev)
		((symbol? abbrev) (symbol-name abbrev))
		(else
		 (error:wrong-type-argument abbrev "string"
					    'ABBREV-EXPANSION))))))
    (if (abbrev-table? where)
	(hash-table-ref/default where abbrev #f)
	(let ((buffer (if (not where) (selected-buffer) where)))
	  (or (let ((table (ref-variable local-abbrev-table buffer)))
		(and table
		     (hash-table-ref/default table abbrev #f)))
	      (hash-table-ref/default (ref-variable global-abbrev-table #f)
			      abbrev
			      #f))))))

(define (abbrev-expansion abbrev where)
  (let ((entry (abbrev-entry abbrev where)))
    (and entry
	 (abbrev-entry-expansion entry))))

(define (define-abbrev-table name definitions)
  (let ((table (make-abbrev-table)))
    (for-each
     (lambda (definition)
       (if (not (and (list? definition)
		     (= 4 (length definition))
		     (string? (car definition))
		     (string? (cadr definition))
		     (symbol? (caddr definition))
		     (exact-nonnegative-integer? (cadddr definition))))
	   (error "Malformed abbrev definition:" definition))
       (define-abbrev table
	 (car definition)
	 (cadr definition)
	 (if (eq? 'NIL (caddr definition)) #f (caddr definition))
	 (cadddr definition)))
     definitions)
    (set-variable-default-value! (name->variable name 'INTERN) table)
    (let ((names (ref-variable abbrev-table-name-list #f)))
      (if (not (memq name names))
	  (set-variable! abbrev-table-name-list (cons name names) #f)))))

(define (get-named-abbrev-table name)
  (let ((table (variable-default-value (name->variable name 'ERROR))))
    (if (not (abbrev-table? table))
	(error:bad-range-argument name 'GET-NAMED-ABBREV-TABLE))
    table))

;;;; Variables

(define-variable abbrev-table-name-list
  "List of symbols whose values are abbrev tables."
  (list 'FUNDAMENTAL-MODE-ABBREV-TABLE 'GLOBAL-ABBREV-TABLE)
  (lambda (object) (list-of-type? object symbol?)))

(define-variable global-abbrev-table
  "The abbrev table whose abbrevs affect all buffers.
Each buffer may also have a local abbrev table.
If it does, the local table overrides the global one
for any particular abbrev defined in both."
  (make-abbrev-table)
  abbrev-table?)

(define-variable fundamental-mode-abbrev-table
  "The abbrev table of mode-specific abbrevs for Fundamental Mode."
  (make-abbrev-table)
  abbrev-table?)

(define-variable-per-buffer local-abbrev-table
  "Local (mode-specific) abbrev table of current buffer."
  #f
  (lambda (object)
    (or (not object)
	(abbrev-table? object))))

(define-variable abbrev-all-caps
  "Set true means expand multi-word abbrevs all caps if abbrev was so."
  #f
  boolean?)

(define-variable save-abbrevs
  "True means save word abbrevs too when files are saved.
Loading an abbrev file sets this to #t."
  #f
  boolean?)

(define-variable only-global-abbrevs
  "True means user plans to use global abbrevs only.
This makes the commands that normally define mode-specific abbrevs
define global abbrevs instead."
  #f
  boolean?)

(define-variable abbrev-file-name
  "Default name of file to read abbrevs from."
  (os/abbrev-file-name)
  string?)

(define-variable pre-abbrev-expand-hook
  "An event distributor that is invoked prior to expanding an abbrev.
The events are called with a single argument: the current point marker."
  (make-event-distributor))

(define abbrevs-changed? #f)

;;;; Abbrev definition

(define-command add-mode-abbrev
  "Define mode-specific abbrev for last word(s) before point.
Argument is how many words before point form the expansion;
or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
Reads the abbreviation in the minibuffer.

Don't use this procedure in a Scheme program; use `define-abbrev' instead."
  "p"
  (lambda (n)
    (add-abbrev (if (ref-variable only-global-abbrevs)
		    (ref-variable global-abbrev-table #f)
		    (or (ref-variable local-abbrev-table)
			(editor-error "No per-mode abbrev table.")))
		"Mode"
		n)))

(define-command add-global-abbrev
  "Define global (all modes) abbrev for last word(s) before point.
The prefix argument specifies the number of words before point that form the
expansion; or zero means the region is the expansion.
A negative argument means to undefine the specified abbrev.
This command uses the minibuffer to read the abbreviation.

Don't use this procedure in a Scheme program; use `define-abbrev' instead."
  "p"
  (lambda (n)
    (add-abbrev (ref-variable global-abbrev-table #f) "Global" n)))

(define-command inverse-add-mode-abbrev
  "Define last word before point as a mode-specific abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  "p"
  (lambda (n)
    (inverse-add-abbrev (if (ref-variable only-global-abbrevs)
			    (ref-variable global-abbrev-table #f)
			    (or (ref-variable local-abbrev-table)
				(editor-error "No per-mode abbrev table.")))
			"Mode"
			n)))

(define-command inverse-add-global-abbrev
  "Define last word before point as a global (mode-independent) abbrev.
With prefix argument N, defines the Nth word before point.
This command uses the minibuffer to read the expansion.
Expands the abbreviation after defining it."
  "p"
  (lambda (n)
    (inverse-add-abbrev (ref-variable global-abbrev-table #f)
			"Global"
			n)))

(define (add-abbrev table type n)
  (let ((expansion
	 (and (>= n 0)
	      (extract-string
	       (if (= n 0)
		   (current-mark)
		   (backward-word (current-point) n 'LIMIT))))))
    (let ((name
	   (prompt-for-string
	    (if expansion
		(string-append type " abbrev for \"" expansion "\"")
		(string-append "Undefine " type " abbrev"))
	    #f)))
      (if expansion
	  (conditionally-define-abbrev table name expansion)
	  (undefine-abbrev table name)))))

(define (inverse-add-abbrev table type n)
  (let* ((m (backward-word (current-point) n 'LIMIT))
	 (location (forward-word m 1 'LIMIT))
	 (name (extract-string m location))
	 (expansion
	  (prompt-for-string
	   (string-append type " expansion for \"" name "\"")
	   #f)))
    (if (conditionally-define-abbrev table name expansion)
	((ref-command expand-abbrev) location))))

(define (conditionally-define-abbrev table name expansion)
  (let ((do-it?
	 (let ((expansion (abbrev-expansion name table)))
	   (or (not expansion)
	       (prompt-for-confirmation?
		(string-append name
			       " expands to \""
			       expansion
			       "\"; redefine"))))))
    (if do-it?
	(define-abbrev table name expansion))
    do-it?))

;;;; Abbrev expansion

(define-command expand-abbrev
  "Expand the abbrev before point, if there is an abbrev there.
Effective when explicitly called even when `abbrev-mode' is nil.
Returns the abbrev symbol, if expansion took place."
  "d"
  (lambda (mark)
    (event-distributor/invoke! (ref-variable pre-abbrev-expand-hook) mark)
    (let* ((start
	    (let ((buffer (selected-buffer)))
	      (let ((start (buffer-get buffer 'ABBREV-START-LOCATION #f)))
		(if start
		    (begin
		      (buffer-remove! buffer 'ABBREV-START-LOCATION)
		      (if (eqv? #\- (extract-right-char start))
			  (delete-right-char start))
		      (mark-temporary! start)
		      start)
		    (backward-word mark 1 #f)))))
	   (end
	    (and start
		 (let ((end (forward-word start 1 'LIMIT)))
		   (if (mark> end mark)
		       mark
		       end)))))
      (and start
	   (mark< start end)
	   (let* ((abbrev (extract-string start end))
		  (entry (abbrev-entry abbrev #f)))
	     (and entry
		  (begin
		    ;; Add an undo boundary, in case we are doing
		    ;; this for a self-inserting command which has
		    ;; avoided making one so far.
		    (undo-boundary! end)
		    (buffer-put! (selected-buffer)
				 'LAST-ABBREV
				 (let ((expansion
					(abbrev-entry-expansion entry)))
				   (and expansion
					(vector abbrev expansion start))))
		    (set-abbrev-entry-count!
		     entry
		     (+ (abbrev-entry-count entry) 1))
		    (let ((expansion (abbrev-entry-expansion entry)))
		      (if (string? expansion)
			  (let ((start (mark-right-inserting-copy start))
				(end (mark-left-inserting-copy end)))
			    (let ((r (make-region start end)))
			      (region-delete! r)
			      (insert-string expansion start)
			      (cond ((string-upper-case? abbrev)
				     (if (or (ref-variable abbrev-all-caps)
					     (= (count-words-region r) 1))
					 (upcase-region r)
					 (capitalize-region r)))
				    ((and (not (string-lower-case? abbrev))
					  (let ((m (forward-to-word start #f)))
					    (and m
						 (mark< m end)
						 m)))
				     => (lambda (m)
					  (upcase-region
					   (make-region m (mark1+ m)))))))
			    (mark-temporary! end)
			    (mark-temporary! start))))
		    (let ((hook (abbrev-entry-hook entry)))
		      (cond ((symbol? hook)
			     ((eval hook (->environment '(EDWIN)))))
			    (hook
			     (error "Abbrev hook must be symbol:" hook))))
		    abbrev)))))))

(define-command unexpand-abbrev
  "Undo the expansion of the last abbrev that expanded.
This differs from ordinary undo in that other editing done since then
is not undone."
  ()
  (lambda ()
    (let ((last (buffer-get (selected-buffer) 'LAST-ABBREV #f)))
      (if last
	  (let ((abbrev (vector-ref last 0))
		(expansion (vector-ref last 1))
		(start (vector-ref last 2)))
	    (let ((end (mark+ start (string-length expansion))))
	      (if (not (string-ci=? (extract-string start end) expansion))
		  (editor-error "Can't expand abbrev; contents changed."))
	      (delete-string start end)
	      (insert-string abbrev start))
	    (buffer-remove! (selected-buffer) 'LAST-ABBREV))))))

(define-command abbrev-prefix-mark
  "Mark current point as the beginning of an abbrev.
Abbrev to be expanded starts here rather than at beginning of word.
This way, you can expand an abbrev with a prefix: insert the prefix,
use this command, then insert the abbrev."
  "P"
  (lambda (argument)
    (if (not argument)
	((ref-command expand-abbrev) (current-point)))
    (buffer-put! (selected-buffer)
		 'ABBREV-START-LOCATION
		 (mark-right-inserting-copy (current-point)))
    (insert-string "-")))

(define-command expand-region-abbrevs
  "For abbrev occurrence in the region, offer to expand it.
The user is asked to type y or n for each occurrence.
A prefix argument means don't query; expand all abbrevs."
  "r\nP"
  (lambda (region no-query?)
    (let ((end (region-end region)))
      (let loop ((start (region-start region)))
	(let ((ws (forward-to-word start #f)))
	  (if (and ws (mark< ws end))
	      (let ((we (forward-word ws 1 'LIMIT)))
		(if (and (mark<= we end)
			 (let ((word (extract-string ws we)))
			   (and (abbrev-expansion word #f)
				(or no-query?
				    (prompt-for-confirmation?
				     (string-append "Expand \"" word "\""))))))
		    (let ((we (mark-left-inserting-copy we)))
		      ((ref-command expand-abbrev) we)
		      (mark-temporary! we)
		      (loop we))
		    (loop we)))))))))

(define-minor-mode abbrev "Abbrev"
  "Minor mode in which abbrevs are expanded.")

(define-command abbrev-mode
  "Toggle abbrev mode.
With argument ARG, turn abbrev mode on iff ARG is positive.
In abbrev mode, inserting an abbreviation causes it to expand
and be replaced by its expansion."
  "P"
  (lambda (argument)
    (let ((mode (ref-mode-object abbrev)))
      (if (if argument
	      (> (command-argument-numeric-value argument) 0)
	      (not (current-minor-mode? mode)))
	  (enable-current-minor-mode! mode)
	  (disable-current-minor-mode! mode)))))

;;;; Editing abbrevs

(define-command edit-abbrevs
  "Alter abbrev definitions by editing a list of them.
Selects a buffer containing a list of abbrev definitions.
You can edit them and type \\<edit-abbrevs>\\[edit-abbrevs-redefine] to redefine abbrevs
according to your editing.
Buffer contains a header line for each abbrev table,
 which is the abbrev table name in parentheses.
This is followed by one line per abbrev in that table:
NAME   USECOUNT   EXPANSION   HOOK
where NAME and EXPANSION are strings with quotes,
USECOUNT is an integer, and HOOK is any valid function
or may be omitted (it is usually omitted)."
  ()
  (lambda () (select-buffer (prepare-abbrev-list-buffer))))

(define-command list-abbrevs
  "Display a list of all defined abbrevs."
  ()
  (lambda () (pop-up-buffer (prepare-abbrev-list-buffer) #f)))

(define (prepare-abbrev-list-buffer)
  (let ((buffer (find-or-create-buffer "*Abbrevs*")))
    (buffer-reset! buffer)
    (insert-abbrev-table-descriptions (buffer-start buffer))
    (buffer-not-modified! buffer)
    (set-buffer-point! buffer (buffer-start buffer))
    (set-buffer-major-mode! buffer (ref-mode-object edit-abbrevs))
    buffer))

(define-command insert-abbrevs
  "Insert after point a description of all defined abbrevs.
Mark is set after the inserted text."
  ()
  (lambda ()
    (let ((point (mark-right-inserting-copy (current-point))))
      (insert-abbrev-table-descriptions (current-point))
      (set-current-mark! (current-point))
      (set-current-point! point))))

(define (insert-abbrev-table-descriptions mark)
  (let ((mark (mark-left-inserting-copy mark)))
    (for-each
     (lambda (name)
       (let ((table (get-named-abbrev-table name)))
	 (insert-string "(" mark)
	 (insert-string (symbol-name name) mark)
	 (insert-string ")\n\n" mark)
	 (hash-table-walk table
	   (lambda (abbrev entry)
	     (if (abbrev-entry-expansion entry)
		 (begin
		   (insert abbrev mark)
		   (indent-to 15 1 mark)
		   (insert (abbrev-entry-count entry) mark)
		   (indent-to 20 1 mark)
		   (insert (abbrev-entry-expansion entry) mark)
		   (if (abbrev-entry-hook entry)
		       (begin
			 (indent-to 45 1 mark)
			 (insert (abbrev-entry-hook entry) mark)))
		   (insert-newline mark)))))
	 (insert-string "\n\n" mark)))
     (ref-variable abbrev-table-name-list #f))
    (mark-temporary! mark)))

(define-major-mode edit-abbrevs fundamental "Edit-Abbrevs"
  "Major mode for editing the list of abbrev definitions.

\\{edit-abbrevs}"
  (lambda (buffer)
    buffer
    unspecific))

(define-key 'edit-abbrevs (kbd (ctrl x) (ctrl s)) 'edit-abbrevs-redefine)
(define-key 'edit-abbrevs (kbd (ctrl c) (ctrl c)) 'edit-abbrevs-redefine)

(define-command edit-abbrevs-redefine
  "Redefine abbrevs according to current buffer contents."
  ()
  (lambda ()
    ((ref-command define-abbrevs) #t)
    (buffer-not-modified! (selected-buffer))))

(define-command define-abbrevs
  "Define abbrevs according to current visible buffer contents.
See documentation of \\[edit-abbrevs] for info on the format of the
text you must have in the buffer.
With argument, eliminate all abbrev definitions except
the ones defined from the buffer now."
  "P"
  (lambda (argument)
    (if argument ((ref-command kill-all-abbrevs)))
    (let ((buffer (selected-buffer)))
      (let read-tables ((start (buffer-start buffer)))
	(let ((m (re-search-forward "^(" start)))
	  (define (read-expr)
	    (with-input-from-mark m read
	      (lambda (expr m*)
		(set! m m*)
		expr)))
	  (if m
	      (let ((name (read-expr)))
		(set! m (forward-line m 1 'LIMIT))
		(define-abbrev-table name
		  (let loop ()
		    (set! m (forward-line m 1 'LIMIT))
		    (if (line-end? m)
			'()
			(let* ((abbrev (read-expr))
			       (count (read-expr))
			       (expansion (read-expr)))
			  (set! m (skip-chars-backward " \t\n\f" m))
			  (let ((hook
				 (if (line-end? m)
				     'NIL
				     (read-expr))))
			    (set! m (skip-chars-backward " \t\n\f" m))
			    (cons (list abbrev expansion hook count)
				  (loop)))))))
		(read-tables m))))))))

(define-command kill-all-abbrevs
  "Undefine all defined abbrevs."
  ()
  (lambda ()
    (for-each (lambda (name)
		(clear-abbrev-table (get-named-abbrev-table name)))
	      (ref-variable abbrev-table-name-list #f))
    (set-variable! abbrev-table-name-list '() #f)))

;;;; Abbrev-file I/O

(define-command read-abbrev-file
  "Read abbrev definitions from file written with `write-abbrev-file'.
Argument FILENAME is the name of the file to read;
it defaults to the value of `abbrev-file-name'."
  (lambda ()
    (list
     (prompt-for-existing-file "Read abbrev file"
			       (list (ref-variable abbrev-file-name #f)))))
  (lambda (filename)
    (let ((filename (abbrev-file/filename filename)))
      ((message-wrapper #f "Loading " filename)
       (lambda ()
	 (quietly-read-abbrev-file filename))))))

(define (quietly-read-abbrev-file #!optional filename)
  (let ((filename
	 (abbrev-file/filename
	  (if (default-object? filename) #f filename))))
    (load-edwin-file filename '(EDWIN) #f)
    (set-variable! save-abbrevs #t #f)
    (set! abbrevs-changed? #f)
    unspecific))

(define-command write-abbrev-file
  "Write all abbrev definitions to a file of Lisp code.
The file written can be loaded in another session to define the same abbrevs.
The argument FILENAME is the file name to write."
  (lambda ()
    (list
     (prompt-for-file "Write abbrev file"
		      (merge-pathnames (ref-variable abbrev-file-name #f)))))
  (lambda (filename)
    (let ((filename (abbrev-file/filename filename)))
      (call-with-output-file filename
	(lambda (port)
	  (for-each
	   (lambda (name)
	     (let ((table (get-named-abbrev-table name)))
	       (write-string "(define-abbrev-table '" port)
	       (write name port)
	       (write-string " '(" port)
	       (newline port)
	       (hash-table-walk table
		 (lambda (abbrev entry)
		   (if (abbrev-entry-expansion entry)
		       (begin
			 (write-string "    " port)
			 (write (list abbrev
				      (abbrev-entry-expansion entry)
				      (or (abbrev-entry-hook entry) 'NIL)
				      (abbrev-entry-count entry))
				port)
			 (newline port)))))
	       (write-string "    ))" port)
	       (newline port)
	       (newline port)))
	   (ref-variable abbrev-table-name-list #f)))))))

(define (abbrev-file/filename filename)
  (or filename (ref-variable abbrev-file-name #f)))

(define (maybe-save-abbrevs no-confirmation?)
  (and (ref-variable save-abbrevs #f)
       abbrevs-changed?
       (begin
	 (if (or no-confirmation?
		 (prompt-for-confirmation?
		  (string-append "Save abbrevs in "
				 (ref-variable abbrev-file-name #f))))
	     ((ref-command write-abbrev-file) #f))
	 ;; Don't keep bothering user if he says no.
	 (set! abbrevs-changed? #f)
	 #t)))