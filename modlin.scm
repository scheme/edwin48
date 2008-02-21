#| -*-Scheme-*-

$Id: modlin.scm,v 1.29 2008/01/30 20:02:03 cph Exp $

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

;;;; Modeline Format
;;; package: (edwin mode-line-format)


(define-variable-per-buffer mode-line-format
  "Template for displaying mode line for current buffer.
Each buffer has its own value of this variable.
Value may be a string, a symbol, a list, or a pair.
For a symbol, its value is used (but it is ignored if #t or #f).
 A string appearing directly as the value of a symbol is processed verbatim
 in that the %-constructs below are not recognized.
For a list whose car is a symbol, the symbol's value is taken,
 and if that is true, the cadr of the list is processed recursively.
 Otherwise, the caddr of the list (if there is one) is processed.
For a list whose car is a string or list, each element is processed
 recursively and the results are effectively concatenated.
For a list whose car is an integer, the cdr of the list is processed
  and padded (if the number is positive) or truncated (if negative)
  to the width specified by that number.
A string is printed verbatim in the mode line except for %-constructs:
  (%-constructs are allowed when the string is the entire mode-line-format
   or when it is found in a cons-cell or a list)
  %b -- print buffer name.      %f -- print visited file name.
  %* -- print *, % or hyphen.
  %s -- print process status.
  %p -- print percent of buffer above top of window, or top, bot or all.
  %n -- print Narrow if appropriate.
  %[ -- print one [ for each recursive editing level.  %] similar.
  %% -- print %.   %- -- print infinitely many dashes.
Decimal digits after the % specify field width to which to pad."
  '("" mode-line-modified
       mode-line-buffer-identification
       "   "
       global-mode-string
       "   %[("
       mode-name
       minor-mode-alist
       "%n"
       mode-line-process
       ")%]----"
       (-3 . "%p")
       "-%-"))

(define-variable-per-buffer mode-line-modified
  "Mode-line control for displaying whether current buffer is modified."
  '("--%1*%1*-"))

(define-variable-per-buffer mode-line-buffer-identification
  "Mode-line control for identifying the buffer being displayed.
Its default value is \"Edwin: %17b\".  Major modes that edit things
other than ordinary files may change this (e.g. Info, Dired,...)"
  '("Edwin: %17b"))

(define-variable global-mode-string
  "Extra stuff appearing after buffer-name in standard mode-line-format."
  #f)

(define-variable-per-buffer mode-name
  "Pretty name of current buffer's major mode (a string)."
  ""
  string?)

(define-variable-per-buffer minor-mode-alist
  "Alist saying how to show minor modes in the mode line.
Each element looks like (VARIABLE STRING);
STRING is included in the mode line iff VARIABLE's value is true.

Actually, STRING need not be a string; any possible mode-line element
is okay.  See `mode-line-format'."
  `((,(lambda (window) window *defining-keyboard-macro?*) " Def"))
  alist?)

(define-variable-per-buffer mode-line-process
  "Mode-line control for displaying info on process status."
  #f)

(define-variable-per-buffer mode-line-procedure
  "Procedure used to generate the mode-line.
Must accept four arguments: WINDOW STRING START END.
Must generate a modeline string for WINDOW in the given substring.
If #F, the normal method is used."
  #f)

(define (modeline-string! window line start end)
  (let ((procedure
	 (variable-local-value (window-buffer window)
			       (ref-variable-object mode-line-procedure))))
    (if procedure
	(procedure window line start end)
	(let ((last
	       (display-mode-element
		(variable-local-value (window-buffer window)
				      (ref-variable-object mode-line-format))
		window line start end end)))
	  (if (fix:< last end)
	      (do ((x last (fix:+ x 1)))
		  ((fix:= x end))
		(string-set! line x #\space)))))))

(define (format-modeline-string window format size)
  (let ((line (make-string size)))
    (display-mode-element format window line 0 size size)
    line))

(define (display-mode-element element window line column min-end max-end)
  (cond ((pair? element)
	 (display-mode-pair element window line column min-end max-end))
	((string? element)
	 (display-mode-string element window line column min-end max-end))
	((or (symbol? element) (variable? element))
	 (let ((value
		(if (symbol? element)
		    (window-symbol-value window element)
		    (variable-local-value (window-buffer window) element))))
	   (cond ((string? value)
		  (display-string value line column min-end max-end))
		 ((boolean? value)
		  (display-pad line column min-end))
		 (else
		  (display-mode-element
		   value window line column min-end max-end)))))
	((procedure? element)
	 (display-mode-element (element window)
			       window line column min-end max-end))
	(else
	 (display-string "*invalid*" line column min-end max-end))))

(define (display-mode-pair element window line column min-end max-end)
  (let ((invalid
	 (lambda () (display-string "*invalid*" line column min-end max-end)))
	(finish (lambda (column) (display-pad line column min-end)))
	(key (car element))
	(rest (cdr element)))
    (let ((do-boolean
	   (lambda (value)
	     (cond ((not (pair? rest))
		    (invalid))
		   (value
		    (display-mode-element (car rest)
					  window line column min-end max-end))
		   ((null? (cdr rest))
		    (finish column))
		   ((pair? (cdr rest))
		    (display-mode-element (cadr rest)
					  window line column min-end max-end))
		   (else
		    (invalid))))))
      (cond ((boolean? key)
	     (do-boolean key))
	    ((symbol? key)
	     (do-boolean (window-symbol-value window key)))
	    ((variable? key)
	     (do-boolean (variable-local-value (window-buffer window) key)))
	    ((minor-mode? key)
	     (do-boolean (buffer-minor-mode? (window-buffer window) key)))
	    ((integer? key)
	     (let ((values
		    (lambda (min-end max-end)
		      (display-mode-element rest window line column
					    min-end
					    max-end))))
	       (cond ((negative? key)
		      (values min-end (min max-end (- column key))))
		     ((positive? key)
		      (values (max min-end (min max-end (+ column key)))
			      max-end))
		     (else
		      (values min-end max-end)))))
	    ((or (string? key) (pair? key))
	     (let loop ((element element) (column column))
	       (if (and (pair? element)
			(< column max-end))
		   (loop (cdr element)
			 (display-mode-element
			  (car element)
			  window line column column max-end))
		   (finish column))))
	    ((procedure? key)
	     (display-mode-pair (cons (key window) rest)
				window line column min-end max-end))
	    (else
	     (finish column))))))

(define (display-mode-string element window line column min-end max-end)
  (let ((end (string-length element)))
    (let loop ((start 0) (column column))
      (if (and (< start end)
	       (< column max-end))
	  (let ((percent (string-index element #\% start end)))
	    (if (not percent)
		(display-substring element start end
				   line column min-end max-end)
		(let* ((column
			(if (< start percent)
			    (display-substring
			     element start percent line column min-end max-end)
			    column))
		       (values
			(lambda (index width)
			  (if (< index end)
			      (loop (+ index 1)
				    (display-mode-spec
				     (string-ref element index)
				     window
				     line
				     column
				     (min max-end (+ width column))
				     max-end))
			      (loop index column)))))
		  (let loop ((index (+ percent 1)) (width 0))
		    (if (< index end)
			(let* ((char (string-ref element index))
			       (digit (char->digit char)))
			  (if digit
			      (loop (+ index 1) (+ (* 10 width) digit))
			      (values index width)))
			(values index width))))))
	  (display-pad line column min-end)))))

(define (display-mode-spec char window line column min-end max-end)
  (let ((max-width (- max-end column))
	(buffer (window-buffer window)))
    (if (char=? char #\m)
	(display-mode-element (ref-variable minor-mode-alist buffer)
			      window line column min-end max-end)
	(display-string
	 (case char
	   ((#\b)
	    (let ((name (buffer-name buffer)))
	      (if (< 2 max-width (string-length name))
		  (let ((result (substring name 0 max-width)))
		    (string-set! result (- max-width 1) #\\)
		    result)
		  name)))
	   ((#\f)
	    (let ((pathname (buffer-pathname buffer)))
	      (if (pathname? pathname)
		  (os/truncate-filename-for-modeline (->namestring pathname)
						     max-width)
		  "")))
	   ((#\M)
	    (ref-variable mode-name buffer))
	   ((#\n)
	    (if (group-clipped? (buffer-group buffer)) " Narrow" ""))
	   ((#\*)
	    (cond ((not (buffer-writeable? buffer)) "%")
		  ((buffer-modified? buffer) "*")
		  (else "-")))
	   ((#\s)
	    (let ((process (get-buffer-process buffer)))
	      (if process
		  (symbol-name (process-status process))
		  "no process")))
	   ((#\p)
	    (let ((group (buffer-group buffer)))
	      (let ((start (group-display-start group)))
		(if (let ((end (group-display-end group)))
		      (or (window-mark-visible? window end)
			  (and (mark< start end)
			       (line-start? end)
			       (window-mark-visible? window (mark-1+ end)))))
		    (if (window-mark-visible? window start)
			"All"
			"Bottom")
		    (if (window-mark-visible? window start)
			"Top"
			(string-append
			 (string-pad
			  (number->string
			   (min
			    (let ((start (group-display-start-index group)))
			      (integer-round
			       (* 100
				  (- (mark-index (window-start-mark window))
				     start))
			       (- (group-display-end-index group) start)))
			    99))
			  2)
			 "%"))))))
	   ((#\[ #\])
	    (cond ((<= recursive-edit-level 10)
		   (make-string recursive-edit-level char))
		  ((char=? #\[ char)
		   "[[[... ")
		  (else
		   " ...]]]")))
	   ((#\%) "%")
	   ((#\-) (make-string max-width #\-))
	   (else ""))
	 line column min-end max-end))))

(define (display-string string line column min-end max-end)
  (display-substring string 0 (string-length string)
		     line column min-end max-end))

(define (display-substring string start end line column min-end max-end)
  (let ((results substring-image-results))
    (substring-image! string start end
		      line column max-end
		      #f 0 results
		      (variable-default-value
		       (ref-variable-object char-image-strings)))
    (if (fix:< (vector-ref results 1) min-end)
	(begin
	  (do ((x (vector-ref results 1) (fix:+ x 1)))
	      ((fix:= x min-end))
	    (string-set! line x #\space))
	  min-end)
	(vector-ref results 1))))

(define (display-pad line column min-end)
  (if (< column min-end)
      (begin
	(string-fill! line #\space column min-end)
	min-end)
      column))

(define (window-symbol-value window symbol)
  (variable-local-value (window-buffer window) (name->variable symbol)))

(define* (add-minor-mode-line-entry! buffer predicate (consequent #f))
  (let ((consequent
	 (if (not consequent)
	     (cond ((minor-mode? predicate)
		    (string-append " " (mode-display-name predicate)))
		   ((or (symbol? predicate) (variable? predicate))
		    predicate)
		   (else ""))
	     consequent))
	(minor-mode-alist (ref-variable-object minor-mode-alist)))
    (let ((alist (variable-local-value buffer minor-mode-alist)))
      (if (not (assq predicate alist))
	  (set-variable-local-value! buffer
				     minor-mode-alist
				     (cons (list predicate consequent)
					   alist))))))

(define (remove-minor-mode-line-entry! buffer predicate)
  (let ((minor-mode-alist (ref-variable-object minor-mode-alist)))
    (set-variable-local-value!
     buffer
     minor-mode-alist
     (alist-delete predicate (variable-local-value buffer minor-mode-alist) eq?))))