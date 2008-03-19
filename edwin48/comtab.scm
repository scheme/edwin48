#| -*-Scheme-*-

$Id: comtab.scm,v 1.78 2008/01/30 20:01:59 cph Exp $

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

;;;; Command Tables


(define-record-type* comtab
  (make-comtab)
  (vector
   alist))

(define (comtab-get comtab key)
  (let ((vector (comtab-vector comtab)))
    (let ((try
	   (lambda (key)
	     (if (and (vector? vector)
		      (char? key)
		      (< (char->integer key) (vector-length vector)))
		 (vector-ref vector (char->integer key))
		 (let ((entry (assq key (comtab-alist comtab))))
		   (and entry
			(cdr entry)))))))
      (if (and (char? key) (char-upper-case? (char-base key)))
	  (or (try key) (try (char-downcase key)))
	  (try key)))))

(define (comtab-put! comtab key datum)
  (cond ((not datum)
	 (comtab-remove! comtab key))
	((and (char? key) (< (char->integer key) 256))
	 (let ((vector (comtab-vector comtab)))
	   (if (vector? vector)
	       (vector-set! vector (char->integer key) datum)
	       (let ((alist (comtab-alist comtab)))
		 (let ((entry (assq key alist)))
		   (if entry
		       (set-cdr! entry datum)
		       (let ((vector (+ vector 1))
			     (alist (cons (cons key datum) alist)))
			 (if (< vector 64)
			     (without-interrupts
			      (lambda ()
				(set-comtab-vector! comtab vector)
				(set-comtab-alist! comtab alist)))
			     (let* ((vector (make-vector 256 #f))
				    (alist
				     (remove (lambda (entry)
					       (let ((key (car entry)))
						 (and (char? key)
						      (< (char->integer key) 256)
						      (begin
							(vector-set!
							 vector
							 (char->integer key)
							 (cdr entry))
							#t))))
					     alist)))
			       (without-interrupts
				(lambda ()
				  (set-comtab-vector! comtab vector)
				  (set-comtab-alist! comtab alist))))))))))))
	(else
	 (let ((alist (comtab-alist comtab)))
	   (let ((entry (assq key alist)))
	     (if entry
		 (set-cdr! entry datum)
		 (set-comtab-alist! comtab
				    (cons (cons key datum) alist))))))))

(define (comtab-remove! comtab key)
  (if (and (char? key) (< (char->integer key) 256))
      (let ((vector (comtab-vector comtab)))
	(if (vector? vector)
	    (vector-set! vector (char->integer key) #f)
	    (let ((alist (comtab-alist comtab)))
	      (let ((entry (assq key alist)))
		(if entry
		    (let ((vector (- vector 1))
			  (alist (delete entry alist eq?)))
		      (without-interrupts
		       (lambda ()
			 (set-comtab-vector! comtab vector)
			 (set-comtab-alist! comtab alist)))))))))
      (set-comtab-alist! comtab (alist-delete key (comtab-alist comtab) eq?))))

(define (valid-comtabs? object)
  (or (mode? object)
      (symbol? object)
      (comtab? object)
      (list-of-comtabs? object)))

(define (guarantee-comtabs object procedure)
  (cond ((mode? object)
	 (mode-comtabs object))
	((symbol? object)
	 (mode-comtabs (->mode object)))
	((comtab? object)
	 (list object))
	((list-of-comtabs? object)
	 object)
	(else
	 (error:wrong-type-argument object "list of comtabs" procedure))))

(define (mode-name? object)
  (and (symbol? object)
       (string-table-get editor-modes (symbol-name object))))

(define (list-of-comtabs? object)
  (and (not (null? object))
       (list? object)
       (every comtab? object)))

(define (comtab-key? object)
  (or (key? object)
      (prefixed-key? object)
      (button? object)))

(define (prefixed-key? object)
  (let loop ((object object))
    (and (pair? object)
	 (key? (car object))
	 (or (null? (cdr object))
	     (loop (cdr object))))))

(define (valid-datum? object)
  (or (not object)
      (command? object)
      (comtab? object)
      (command&comtab? object)
      (comtab-alias? object)))

(define (command&comtab? object)
  (and (pair? object)
       (command? (car object))
       (comtab? (cdr object))))

(define (comtab-alias? object)
  (and (pair? object)
       (valid-comtabs? (car object))
       (comtab-key? (cdr object))))

(define (comtab-alias/dereference datum)
  (lookup-key (car datum) (cdr datum)))

(define (lookup-key comtabs key)
  (let ((comtabs (guarantee-comtabs comtabs 'LOOKUP-KEY)))
    (let ((simple-lookup
	   (lambda (key)
	     (let loop ((comtabs* comtabs))
	       (cond ((comtab-get (car comtabs*) key)
		      => handle-datum)
		     ((not (null? (cdr comtabs*)))
		      (loop (cdr comtabs*)))
		     (else
		      #f))))))
      (cond ((key? key)
	     (simple-lookup (remap-alias-key key)))
	    ((button? key)
	     (simple-lookup key))
	    ((prefixed-key? key)
	     (let ((prefix (drop-right key 1))
		   (key (remap-alias-key (car (take-right key 1)))))
	       (if (null? prefix)
		   (simple-lookup key)
		   (let loop ((comtabs* comtabs))
		     (let ((comtab
			    (lookup-prefix (car comtabs*) prefix #f)))
		       (cond ((and comtab (comtab-get comtab key))
			      => handle-datum)
			     ((not (null? (cdr comtabs*)))
			      (loop (cdr comtabs*)))
			     (else
			      #f)))))))
	    (else
	     (error:wrong-type-argument key "comtab key" 'LOOKUP-KEY))))))

(define (handle-datum datum)
  (cond ((or (command? datum)
	     (comtab? datum)
	     (command&comtab? datum))
	 datum)
	((comtab-alias? datum)
	 (comtab-alias/dereference datum))
	(else
	 (error "Illegal comtab datum:" datum))))

(define (lookup-prefix comtab prefix intern?)
  (let loop ((comtab comtab) (prefix* prefix))
    (if (null? prefix*)
	comtab
	(let ((key (remap-alias-key (car prefix*)))
	      (prefix* (cdr prefix*)))
	  (let datum-loop ((datum (comtab-get comtab key)))
	    (cond ((not datum)
		   (and intern?
			(let ((datum (make-comtab)))
			  ;; Note that this will clobber a comtab-alias
			  ;; that points to an undefined entry.
			  (comtab-put! comtab key datum)
			  (loop datum prefix*))))
		  ((comtab? datum)
		   (loop datum prefix*))
		  ((command&comtab? datum)
		   (loop (cdr datum) prefix*))
		  ((comtab-alias? datum)
		   (datum-loop (comtab-alias/dereference datum)))
		  ((command? datum)
		   (error "Key sequence too long:"
			  prefix
			  (- (length prefix) (length prefix*))))
		  (else
		   (error "Illegal comtab datum:" datum))))))))

(define (comtab-entry comtabs key)
  (or (%comtab-entry comtabs key)
      (and (not (button? key))
	   (ref-command-object undefined))))

(define (local-comtab-entry comtabs key mark)
  (or (and mark
	   (let ((local-comtabs (local-comtabs mark)))
	     (and local-comtabs
		  (%comtab-entry local-comtabs key))))
      (comtab-entry comtabs key)))

(define (%comtab-entry comtabs key)
  (let ((object (lookup-key comtabs key)))
    (cond ((not object)
	   #f)
	  ((command? object)
	   object)
	  ((command&comtab? object)
	   (car object))
	  ((comtab? object)
	   (ref-command-object prefix-key))
	  (else
	   (error "Illegal result from lookup-key:" object)))))

(define (prefix-key-list? comtabs key)
  (let ((object (lookup-key comtabs key)))
    (or (comtab? object)
	(command&comtab? object))))

(define (define-key mode key datum)
  (%define-key (guarantee-comtabs mode 'DEFINE-KEY)
	       key
	       (if (valid-datum? datum) datum (->command datum))
	       'DEFINE-KEY))

(define* (define-prefix-key mode key (command #f))
  (%define-key (guarantee-comtabs mode 'DEFINE-PREFIX-KEY)
	       (begin
		 (if (button? key)
		     (error:wrong-type-argument key
						"comtab prefix key"
						'DEFINE-PREFIX-KEY))
		 key)
	       (let ((comtab (make-comtab)))
		 (if (not command)
		     comtab
		     (let ((command (->command command)))
		       (if (eq? command (ref-command-object prefix-key))
			   comtab
			   (cons command comtab)))))
	       'DEFINE-PREFIX-KEY))

(define (%define-key comtabs key datum procedure)
  (let* ((comtab (car comtabs))
	 (put!
	  (lambda (key)
	    (comtab-put! comtab (remap-alias-key key) datum))))
    (cond ((or (key? key) (button? key))
	   (put! key))
	  ((char-set? key)
	   (for-each put! (char-set->list key)))
	  ((prefixed-key? key)
	   (let ((prefix (drop-right key 1)))
	     (comtab-put! (if (null? prefix)
			      comtab
			      (lookup-prefix comtab prefix #t))
			  (remap-alias-key (car (take-right key 1)))
			  datum)))
	  (else
	   (error:wrong-type-argument key "comtab key" procedure))))
  key)

(define (comtab-alist* comtab)
  (let ((vector (comtab-vector comtab))
	(alist (comtab-alist comtab)))
    (if (vector? vector)
	(let ((end (vector-length vector)))
	  (let loop ((index 0) (alist alist))
	    (if (fix:< index end)
		(loop (fix:+ index 1)
		      (let ((datum (vector-ref vector index)))
			(if datum
			    (cons (cons (integer->char index) datum)
				  alist)
			    alist)))
		alist)))
	alist)))

(define (comtab->alist comtab)
  (let loop ((prefix '()) (comtab comtab))
    (append-map!
     (lambda (entry)
       (if (and (button? (car entry))
		(not (null? prefix)))
	   '()
	   (let ((prefix (append prefix (list (car entry)))))
	     (let ((key (if (null? (cdr prefix)) (car prefix) prefix)))
	       (let datum-loop ((datum (cdr entry)))
		 (cond ((not datum)
			'())
		       ((command? datum)
			(list (cons key datum)))
		       ((comtab? datum)
			(loop prefix datum))
		       ((command&comtab? datum)
			(cons (cons key (car datum))
			      (loop prefix (cdr datum))))
		       ((comtab-alias? datum)
			(datum-loop (comtab-alias/dereference datum)))
		       (else
			(error "Illegal comtab datum:" datum))))))))
     (comtab-alist* comtab))))

(define (comtab-key-bindings comtabs command)
  (let ((comtabs (guarantee-comtabs comtabs 'COMTAB-KEY-BINDINGS))
	(command (->command command)))
    ;; In addition to having a binding of COMMAND, every key in the
    ;; result satisfies VALID-KEY?.  This eliminates bindings that are
    ;; shadowed by other bindings.
    (let ((valid-key?
	   (lambda (key)
	     (let ((datum (lookup-key comtabs key)))
	       (cond ((command? datum)
		      (eq? command datum))
		     ((comtab? datum)
		      (eq? command (ref-command-object prefix-key)))
		     ((command&comtab? datum)
		      (eq? command (car datum)))
		     (else
		      #f))))))
      (sort (let loop ((comtabs comtabs))
	      (if (null? comtabs)
		  '()
		  (%comtab-bindings (car comtabs)
				    (loop (cdr comtabs))
				    command
				    valid-key?)))
	(let ((v
	       (lambda (k)
		 (cond ((char? k) 0)
		       ((list-of-type? k char?) 1)
		       ((special-key? k) 2)
		       ((button? k) 3)
		       (else 4)))))
	  (lambda (k1 k2)
	    (< (v k1) (v k2))))))))

(define (%comtab-bindings comtab keys command valid-key?)
  (let comtab-loop ((comtab comtab) (keys keys) (prefix '()))
    (let alist-loop ((entries (comtab-alist* comtab)))
      (if (null? entries)
	  keys
	  (let ((key
		 (if (pair? prefix)
		     (append prefix (list (caar entries)))
		     (caar entries))))
	    (let datum-loop
		((datum (cdar entries))
		 (keys (alist-loop (cdr entries))))
	      (cond ((not datum)
		     keys)
		    ((command? datum)
		     (if (and (eq? datum command)
			      (valid-key? key))
			 (cons key keys)
			 keys))
		    ((comtab? datum)
		     (let ((keys
			    (comtab-loop datum keys
					 (if (pair? prefix)
					     key
					     (list key)))))
		       (if (and (eq? command (ref-command-object prefix-key))
				(valid-key? key))
			   (cons key keys)
			   keys)))
		    ((command&comtab? datum)
		     (datum-loop (car datum)
				 (datum-loop (cdr datum) keys)))
		    ((comtab-alias? datum)
		     (datum-loop (comtab-alias/dereference datum) keys))
		    (else
		     (error "Illegal comtab datum:" datum)))))))))