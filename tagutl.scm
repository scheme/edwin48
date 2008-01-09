;;; -*-Scheme-*-
;;;
;;; $Id: tagutl.scm,v 1.65 2007/01/05 21:19:24 cph Exp $
;;;
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;;     2006, 2007 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;;; USA.
;;;
;;;

;;;; Tags Facility
;;;  From GNU Emacs (thank you RMS)


(define-variable tags-table-pathnames
  "List of pathnames of all of the active tags tables.

See documentation for visit-tags-table and visit-additional-tags-table."
  '()
  (lambda (object)
    (list-of-type? object
      (lambda (object)
	(or (string? object)
	    (pathname? object))))))

(define-command visit-tags-table
  "Tell tags commands to use only the tag table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory.
To use more than one tag table file at a time,
see \\[visit-additional-tags-table]."
  "FVisit tags table (default TAGS)"
  (lambda (filename)
    (set-variable! tags-table-pathnames
		   (list (pathname-default-name filename "TAGS")))))

(define-command visit-additional-tags-table
  "Adds another tags table file to the current list of active tags tables."
  "FVisit additional tags table (default TAGS)"
  (lambda (filename)
    (set-variable! tags-table-pathnames
		   (append (ref-variable tags-table-pathnames)
			   (list (pathname-default-name filename "TAGS"))))))

(define-command find-tag
  "Find tag (in current list of tag tables) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-false (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-table-pathnames."
  (lambda () (find-tag-arguments "Find tag"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file)))

(define-command find-tag-other-window
  "Like \\[find-tag], but select buffer in another window."
  (lambda () (find-tag-arguments "Find tag in other window"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file-other-window)))

(define-command find-tag-other-frame
  "Like \\[find-tag], but select buffer in another frame."
  (lambda () (find-tag-arguments "Find tag in other frame"))
  (lambda (string previous-tag?)
    (&find-tag-command string previous-tag? find-file-other-screen)))

;;;; Find Tag

(define find-tag-pathnames-list
  #f)

(define (handle-includes! included-pathnames)
  (if included-pathnames
      (set! find-tag-pathnames-list
	    (append (list (car find-tag-pathnames-list))
		    (if included-pathnames
			included-pathnames
			'())			
		    (cdr find-tag-pathnames-list)))))

(define (first-tags-table-buffer)
  (if (not (pair? (ref-variable tags-table-pathnames)))
      (dispatch-on-command (ref-command-object visit-tags-table)))
  (set! find-tag-pathnames-list (ref-variable tags-table-pathnames))
  (let* ((pathname (car find-tag-pathnames-list))
	 (buffer (get-tags-table pathname))
	 (included-pathnames (get-included-pathnames buffer)))
    (handle-includes! included-pathnames)
    buffer))

(define (current-tags-table-buffer)
  (if find-tag-pathnames-list
      (find-file-noselect (car find-tag-pathnames-list) #f)
      #f))
  
(define (next-tags-table-buffer)
  (if (and (pair? find-tag-pathnames-list)
	   (pair? (cdr find-tag-pathnames-list)))
      (let ((pathname (second find-tag-pathnames-list)))
	(set! find-tag-pathnames-list
	      (cdr find-tag-pathnames-list))
	(let* ((buffer (get-tags-table pathname))
	       (included-pathnames (get-included-pathnames buffer)))
	  (handle-includes! included-pathnames)
	  buffer))
      #f))

(define (find-tag-arguments prompt)
  (let ((previous-tag? (command-argument)))
    (list (and (not previous-tag?)
	       (prompt-for-string prompt (find-tag-default)))
	  previous-tag?)))

(define (&find-tag-command string previous-tag? find-file)
  (if previous-tag?
      (let ((buffer (current-tags-table-buffer)))
	(find-tag previous-find-tag-string
		  buffer
		  (buffer-point buffer)
		  find-file))
	(begin
	  (set! previous-find-tag-string string)
	  (let ((buffer (first-tags-table-buffer)))
	    (find-tag string buffer (buffer-start buffer) find-file))))
  (set! tags-loop-continuation
	(lambda ()
	  (&find-tag-command #f #t find-file)))
  unspecific)

(define previous-find-tag-string
  #f)

(define (find-tag-default)
  (let ((end
	 (let ((point (current-point)))
	   (or (re-match-forward "\\(\\sw\\|\\s_\\)+"
				 point
				 (group-end point)
				 #f)
	       (let ((mark
		      (re-search-backward "\\sw\\|\\s_"
					  point
					  (group-start point)
					  #f)))
		 (and mark
		      (mark1+ mark)))))))
    (and end
	 (extract-string (forward-prefix-chars (backward-sexp end 1 'LIMIT)
					       end)
			 end))))

(define (find-tag string buffer start find-file)
  (let ((end (group-end start)))
    (let ((tag
	   (let loop ((mark start))
	     (let ((mark (search-forward string mark end)))
	       (and mark
		    (or (re-match-forward find-tag-match-regexp mark)
			(loop mark)))))))
      (if (not tag)
	  (let ((next-buffer (next-tags-table-buffer)))
	    (if (not next-buffer)
		(editor-failure "No "
				(if (group-start? start) "" "more ")
				"entries containing "
				string)
		(find-tag string next-buffer (buffer-start next-buffer)
			  find-file)))
	  (let ((pathname
		 (merge-pathnames
		  (tag->pathname tag)
		  (directory-pathname (buffer-pathname buffer))))
		(regexp
		 (string-append
		  "^"
		  (re-quote-string (extract-string (mark-1+ tag)
						   (line-start tag 0)))))
		(start
		 (-1+
		  (string->number
		   (let ((mark (search-forward "," tag end)))
		     (extract-string mark (line-end mark 0)))))))
	    (set-buffer-point! buffer (line-end tag 0))
	    (find-file pathname)
	    (let* ((buffer (current-buffer))
		   (group (buffer-group buffer)))
	      (buffer-widen! buffer)
	      (push-current-mark! (current-point))
	      (let ((mark
		     (let loop ((offset 1000))
		       (let ((index (- start offset))
			     (end (group-end-index group)))
			 (if (and (fix:> index 0)
				  (fix:< index end))
			     (or (re-search-forward
				  regexp
				  (make-mark group index)
				  (make-mark group (min (+ start offset) end)))
				 (loop (* 3 offset)))
			     (re-search-forward regexp
						(make-mark group 0)
						(group-end-mark group)))))))
		(if (not mark)
		    (editor-failure regexp
				    " not found in "
				    (file-namestring pathname))
		    (set-current-point! (line-start mark 0))))))))))

(define find-tag-match-regexp
  "[^\n\177]*\177")

;;;; Tags Search

(define-command tags-search
  "For every tag table in the current list, search through all files
specified in it for match for REGEXP.  Stops when a match is found.
To continue searching for next match, use command
\\[tags-loop-continue].

See documentation of variable tags-table-pathnames."
  (re-search-prompt "Tags search")
  (lambda (arg regexp)
    ;; arg controls case-sensitivity, just ignore for right now
    arg
    (set! tags-loop-continuation
	  (lambda ()
	    (let ((mark
		   (let ((point (current-point)))
		     (re-search-forward regexp point (group-end point)))))
	      (if mark
		  (begin
		    (set-current-point! mark)
		    (clear-message))
		  (begin
		    (smart-buffer-kill)
		    (tags-loop-start))))))
    (set! tags-loop-pathnames
	  (get-all-pathnames (initial-tags-table-buffers)))
    (tags-loop-start)))

(define-command tags-query-replace
  "Query-replace-regexp FROM with TO through all files listed in all of
the tag tables.  Third arg DELIMITED (prefix arg) means replace only
word-delimited matches.  If you exit (C-g or ESC), you can resume the
query-replace with the command \\[tags-loop-continue].

See documentation of variable tags-file-pathnames."
  (lambda ()
    (let ((source (prompt-for-string "Tags query replace (regexp)" #f)))
      (list source
	    (prompt-for-string
	     (string-append "Tags query replace " source " with")
	     #f
	     'DEFAULT-TYPE 'NULL-DEFAULT)
	    (command-argument))))
  (lambda (source target delimited)
    (set! tags-loop-continuation
	  (lambda ()
	    (if (not (replace-string source target delimited #t #t))
		(begin
		  (smart-buffer-kill)
		  (tags-loop-start)))))
    (set! tags-loop-pathnames
	  (get-all-pathnames (initial-tags-table-buffers)))
    (tags-loop-start)))

(define-command tags-loop-continue
  "Continue last \\[find-tag], \\[tags-search] or \\[tags-query-replace]
command."
  ()
  (lambda ()
    (if (not tags-loop-continuation)
	(editor-error "No tags loop in progress"))
    (tags-loop-continuation)))

(define tags-loop-continuation #f)
(define tags-loop-pathnames)
(define tags-loop-current-buffer #f)

(define (tags-loop-start)
  (let ((pathnames tags-loop-pathnames))
    (if (not (pair? pathnames))
	(begin
	  (set! tags-loop-continuation #f)
	  (editor-error "All files processed.")))
    (set! tags-loop-pathnames (cdr pathnames))
    (let ((buffer
	   (let ((buffer (pathname->buffer (car pathnames))))
	     (if buffer
		 (begin
		   (select-buffer buffer)
		   (buffer-remove! buffer 'TAGS-LOOP-MODIFIED-TICK)
		   buffer)
		 (let ((buffer (find-file-noselect (car pathnames) #t)))
		   (buffer-put! buffer
				'TAGS-LOOP-MODIFIED-TICK
				(buffer-modified-tick buffer))
		   buffer)))))
      (message "Scanning file " (->namestring (buffer-pathname buffer)) "...")
      (select-buffer buffer)
      (set-current-point! (buffer-start buffer))
      (set! tags-loop-current-buffer buffer))
    (tags-loop-continuation)))

(define (smart-buffer-kill)
  (let ((buffer tags-loop-current-buffer))
    (if buffer
	(begin
	  (if (and (ref-variable new-tags-behavior? buffer)
		   (let ((tick (buffer-get buffer 'TAGS-LOOP-MODIFIED-TICK)))
		     (and tick
			  (fix:= tick (buffer-modified-tick buffer)))))
	      (kill-buffer buffer)
	      (buffer-remove! buffer 'TAGS-LOOP-MODIFIED-TICK))
	  (set! tags-loop-current-buffer #f)
	  unspecific))))

(define (buffer-modified-tick buffer)
  (group-modified-tick (buffer-group buffer)))

(define-variable new-tags-behavior?
  "This variable controls the behavior of tags-search and
tags-query-replace.  The new behavior cause any new buffers to be
killed if they are not modified."
  #t
  boolean?)

;;;; Tags Tables

(define (tag->pathname tag)
  (define (loop mark)
    (let ((file-mark (skip-chars-backward "^,\n" (line-end mark 1))))
      (let ((mark (mark+ (line-start file-mark 1)
			 (with-input-from-mark file-mark read))))
	(if (mark> mark tag)
	    (->pathname (extract-string (line-start file-mark 0)
					(mark-1+ file-mark)))
	    (loop mark)))))
  (loop (group-start tag)))

(define (get-tags-table pathname)
  (let ((buffer
	 (let ((buffer (find-file-noselect pathname #f)))
	   (if (and (not (verify-visited-file-modification-time? buffer))
		    (prompt-for-yes-or-no?
		     "Tags file has changed; read new contents"))
	       (revert-buffer buffer #t #t)
	       buffer))))
    (if (not (eqv? (extract-right-char (buffer-start buffer)) #\Page))
	(editor-error "File "
		      (->namestring pathname)
		      " not a valid tag table"))
    buffer))

(define (pathnames->tags-table-buffers pathnames)
  (map get-tags-table pathnames))

(define (initial-tags-table-buffers)
  ;; first make sure there is at least one tags table
  (if (not (ref-variable tags-table-pathnames))
      (dispatch-on-command (ref-command-object visit-tags-table)))
  (pathnames->tags-table-buffers (ref-variable tags-table-pathnames)))

(define (tags-table-pathnames buffers)
  (append-map 
   (lambda (buffer)
     (or (buffer-get buffer 'TAGS-TABLE-PATHNAMES)
	 (let ((directory
		(directory-pathname (buffer-truename buffer)))
	       (finish (lambda (pathnames included-pathnames)
			 (buffer-put! buffer 'TAGS-TABLE-PATHNAMES pathnames)
			 (buffer-put! buffer
				      'TAGS-TABLE-INCLUDED-PATHNAMES
				      included-pathnames)
			 pathnames)))
	   (let loop ((mark (buffer-start buffer))
		      (pathnames '())
		      (included-tables '()))
	     (let ((file-mark
		    (skip-chars-backward "^,\n" (line-end mark 1))))
	       (let ((word (with-input-from-mark file-mark read))
		     (name
		      (merge-pathnames
		       (extract-string (line-start file-mark 0)
				       (mark-1+ file-mark))
		       directory)))
		 (if (number? word)
		     (let ((mark
			    (mark+ (line-start file-mark 1) word)))
		       (if (group-end? mark)
			   (finish (reverse (cons name pathnames))
				   (reverse included-tables))
			   (loop mark
				 (cons name pathnames)
				 included-tables)))
		     ;; if it is not a number than it must be an include
		     (if (group-end? (line-end file-mark 1))
			 (finish (reverse pathnames)
				 (reverse (cons name included-tables)))
			 (loop (line-start mark 2)
			       pathnames
			       (cons name included-tables))))))))))
   buffers))

(define (get-included-pathnames buffer)
  (tags-table-pathnames (list buffer))
  (buffer-get buffer 'TAGS-TABLE-INCLUDED-PATHNAMES))

(define (get-all-pathnames buffers)
  (let ((pathnames (tags-table-pathnames buffers))
	(includes (append-map get-included-pathnames buffers)))
    (if (pair? includes)
	(append pathnames
		(get-all-pathnames
		 (pathnames->tags-table-buffers includes)))
	pathnames)))
