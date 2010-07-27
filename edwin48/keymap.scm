#| -*-Scheme-*-

$Id: keymap.scm,v 1.23 2008/01/30 20:02:02 cph Exp $

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

;;;; Command Summary


(define-command describe-bindings
  "Show a list of all defined keys, and their definitions.
The list is put in a buffer, which is displayed."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (describe-bindings (current-comtabs) #t (current-output-port))))))

(define (describe-bindings comtabs global? port)
  (let ((alists (map sort-by-prefix (comtabs->alists comtabs global?))))
    (if (pair? alists)
	(let ((write-element
	       (lambda (element port)
		 (write-string (car element) port)
		 (write-string (let ((n (string-length (car element))))
				 (cond ((fix:< n 8) "\t\t")
				       ((fix:< n 16) "\t")
				       (else " ")))
			       port)
		 (write-string (cdr element) port)
		 (newline port))))
	  (let ((write-groups
		 (lambda (groups port)
		   (write-element '("key" . "binding") port)
		   (write-element '("---" . "-------") port)
		   (for-each (lambda (elements)
			       (newline port)
			       (for-each (lambda (element)
					   (write-element element port))
					 elements))
			     groups))))
	    (write-groups (car alists) port)
	    (for-each (lambda (groups)
			(newline port)
			(write-groups groups port))
		      (cdr alists)))))))

(define-command make-command-summary
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first."
  ()
  (lambda ()
    (with-output-to-help-display
     (lambda ()
       (make-command-summary (current-comtabs) (current-output-port))))))

(define (make-command-summary comtabs port)
  (let ((alists (comtabs->alists comtabs #t)))
    (if (pair? alists)
	(begin
	  (write-summary-keymap (car alists) port)
	  (for-each (lambda (alist)
		      (write-string separator port)
		      (write-summary-keymap alist port))
		    (cdr alists))))))

(define separator
  (string-append "\n" (make-string 79 #\=) "\n\n"))

(define (write-summary-keymap alist port)
  (let ((element-lists (sort-by-prefix alist)))
    (if (not (null? element-lists))
	(let loop
	    ((entry (car element-lists))
	     (element-lists (cdr element-lists)))
	  (write-summary-style-elements entry port)
	  (if (not (null? element-lists))
	      (begin
		(newline port)
		(loop (car element-lists) (cdr element-lists))))))))

(define (write-summary-style-elements elements port)
  (let loop ((elements (reorder-list elements)))
    (if (not (null? elements))
	(let ((element->string
	       (lambda (element)
		 (string-append
		  (let ((string (car element)))
		    (if (< (string-length string) 9)
			(pad-on-right-to string 9)
			(let loop ((n 16))
			  (if (< (string-length string) n)
			      (pad-on-right-to string n)
			      (loop (+ n 8))))))
		  (cdr element)))))
	  (let ((string (element->string (car elements))))
	    (if (null? (cdr elements))
		(begin
		  (write-string string port)
		  (newline port))
		(begin
		  (write-string (pad-on-right-to string 39) port)
		  (write-char #\space port)
		  (write-string (element->string (cadr elements)) port)
		  (newline port)
		  (loop (cddr elements)))))))))

(define (reorder-list items)
  (let ((tail (drop items (integer-ceiling (length items) 2))))
    (let loop ((items items) (items* tail))
      (cond ((eq? items tail) '())
	    ((null? items*) (list (car items)))
	    (else
	     (cons* (car items)
		    (car items*)
		    (loop (cdr items) (cdr items*))))))))

(define (comtabs->alists comtabs global?)
  (let loop ((comtabs comtabs))
    (cons (sort-and-simplify (comtab->alist (car comtabs)))
	  (if (and (pair? (cdr comtabs))
		   (comtab? (cadr comtabs))
		   (or global?
		       (not (any (lambda (mode)
				   (eq? (cdr comtabs) (mode-comtabs mode)))
				 global-modes))))
	      (loop (cdr comtabs))
	      '()))))

(define (sort-and-simplify elements)
  (map (lambda (element)
	 (cons (xkey->name (car element))
	       (command-name-string (cdr element))))
       (list-sort (lambda (a b) (xkey<? (car a) (car b)))
		  (remove (lambda (element) (button? (car element)))
			  elements))))

(define (sort-by-prefix elements)
  (let ((prefix-alist '()))
    (let ((make-entry
	   (lambda (prefix element)
	     (let ((entry
		    (find (lambda (entry)
			    (string=? (car entry) prefix))
			  prefix-alist)))
	       (if entry
		   (set-cdr! entry (cons element (cdr entry)))
		   (set! prefix-alist
			 (cons (list prefix element) prefix-alist)))
	       unspecific))))
      (for-each (lambda (element)
		  (let ((string (car element)))
		    (let ((has-prefix
			   (lambda (index)
			     (make-entry (string-head string index) element)))
			  (index (string-index-right string #\space)))
		      (if index
			  (has-prefix (1+ index))
			  (let ((end (string-length string)))
			    (let loop ((index 0))
			      (let ((index+1 (+ index 1)))
				(if (and (< index+1 end)
					 (char=? #\-
						 (string-ref string index+1))
					 (memv (string-ref string index)
					       '(#\C #\M #\H #\S #\T)))
				    (loop (+ index+1 1))
				    (has-prefix index)))))))))
		elements))
    (map (lambda (entry)
	   (group-elements (reverse! (cdr entry))))
	 (list-sort (lambda (x y) (string<? (car x) (car y))) prefix-alist))))

(define (group-elements elements)
  (if (or (null? elements)
	  (null? (cdr elements)))
      elements
      (let ((command-name (cdar elements)))
	(if (string=? command-name (cdadr elements))
	    (let ((last
		   (let loop ((elements (cdr elements)))
		     (if (or (null? (cdr elements))
			     (not (string=? command-name (cdadr elements))))
			 elements
			 (loop (cdr elements))))))
	      (cons (cons (string-append (caar elements)
					 " .. "
					 (caar last))
			  command-name)
		    (group-elements (cdr last))))
	    (cons (car elements) (group-elements (cdr elements)))))))