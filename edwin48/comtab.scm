;;; -*- Mode: Scheme; scheme48-package: edwin:command-table -*-

#|
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

;;; Example of a command:
;;; (define fundamental (make-comtab))
;;; (define-command do-something "Does something" () (lambda () (display "Did something\n")))
;;; (define-key fundamental (kbd s) 'do-something)
;;; (dispatch-on-key fundamental (kbd s))

(define-record-type* comtab
  (really-make-comtab table)
  ())

(define (make-comtab)
  (really-make-comtab (make-hash-table key=? key-hash)))

;;; Given a key, evaluate the procedure if
;;; there is a command with that keystroke
(define (dispatch-on-key comtab keystroke)
  (let* ((command   (comtab-entry comtab keystroke))
         (procedure (command-procedure command)))
    (procedure)))

;;; Returns a command for the given key in a comtab
;;; this walks the tree until it finds something.
;;; Meaning it follows prefixed keys.
(define (comtab-get comtab key)
  (cond ((prefixed-key? key)
         (let ((prekey (car key)))
           (comtab-get (comtab-get comtab prekey)
                       (cdr key))))
        ((key? key)
         (hash-table-ref/default (comtab-table comtab) key #f))
        (else
         comtab)))

;;; Returns the value of a key in a comtab or list of comtabs
(define (lookup-key comtabs keystroke)
  (let ((comtabs (guarantee-comtabs comtabs 'LOOKUP-KEY)))
    (let ((simple-lookup
	   (lambda (key)
	     (let loop ((comtabs* comtabs))
	       (cond ((comtab-get (car comtabs*) key)
		      => handle-datum)
		     ((not (null? (cdr comtabs*)))
		      (loop (cdr comtabs*)))
		     (else #f))))))
      (simple-lookup keystroke))))


;;; Checks to make sure that the object passed
;;; in has an equivelent comtab represenation in a list
(define (guarantee-comtabs object procedure)
  (cond ((mode? object)
        (mode-comtabs object))           ; Get the mode's comtabs
        ((symbol? object)
        (mode-comtabs (->mode object)))  ; Find mode for the name then comtabs
        ((comtab? object) (list object)) ; If just a comtab, put it in a list
        ((list-of-comtabs? object) object)
   (else
    (error:wrong-type-argument object "list of comtabs" procedure))))

;;; Get the Command for the given key in
;;; the given command table
(define (comtab-entry comtab keystroke)
  (hash-table-ref/default (comtab-table comtab)
                          keystroke
                          (ref-command-object undefined)))

;;; Get the Command for a keystroke in a specific mark
(define (local-comtab-entry comtabs keystroke mark)
  (or (and mark
	   (let ((local-comtabs (local-comtabs mark)))
	     (and local-comtabs
		  (%comtab-entry local-comtabs keystroke))))
      (comtab-entry comtabs keystroke)))

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

;;; Add a command to the given command table
(define (comtab-put! comtab keystroke datum)
  (if (not datum)
      (comtab-remove! comtab keystroke)
      (hash-table-set! (comtab-table comtab) keystroke datum)))

;;; Remove a command from the given command table
(define (comtab-remove! comtab keystroke)
  (hash-table-delete! (comtab-table comtab) keystroke))

(define (comtab-key? object)
  (key? object))

(define (valid-comtabs? object)
  (or (symbol? object)
      (comtab? object)
      (mode? object)
      (list-of-comtabs? object)))

(define (list-of-comtabs? object)
  (and (not (null? object))
       (list? object)
       (every comtab? object)))

(define (comtab-alias? object)
  (and (pair? object)
       (valid-comtabs? (car object))
       (comtab-key? (cdr object))))

(define (command&comtab? object)
  (and (pair? object)
       (command? (car object))
       (comtab? (cdr object))))

(define (comtab-alias/dereference datum)
  (lookup-key (car datum) (cdr datum)))

(define (prefixed-key? object)
  (let loop ((object object))
    (and (pair? object)
	 (key? (car object))
	 (or (null? (cdr object))
	     (loop (cdr object))))))

(define (prefix-key-list? comtabs key)
  (let ((object (lookup-key comtabs key)))
    (or (comtab? object)
	(command&comtab? object))))

(define (valid-datum? object)
  (or (not object)
      (command? object)
      (comtab? object)))

;;; Evaluate the datum
(define (handle-datum datum)
  (cond ((or (command? datum)
	     (comtab? datum)
	     (command&comtab? datum))
	 datum)
	((comtab-alias? datum)
	 (comtab-alias/dereference datum))
	(else
	 (error "Illegal comtab datum:" datum))))

;;; Adds a keystroke with a corresponding command in the mode comtab
;;; mode: A mode
;;; keystoke: A kbd i.e. (kbd #\a) or (kbd (ctrl #\x) (ctrl #\f))
;;; datum: A command or another command table
(define (define-key mode keystroke datum)
  (%define-key (guarantee-comtabs mode 'DEFINE-KEY)
	       keystroke
	       (if (valid-datum? datum) datum (->command datum))
	       'DEFINE-KEY))

;;; Really define the key. Uses 'procedure' for error
;;; messages, but we are not supporting it at the moment.
(define (%define-key comtabs keystroke datum procedure)
  (let* ((comtab (car comtabs))
         (put!
          (lambda (keystroke)
            (comtab-put! comtab keystroke datum))))
    (cond ((key? keystroke)
           (put! keystroke))
          ((prefixed-key? keystroke)
           ;; Find the comtab for the prefix key
           ;; then add the following key to the datum
           ;; in the prefix-key's hash table
           (let ((prefixes (drop-right keystroke 1))
                 (last-key (last keystroke)))
             (comtab-put! (if (null? prefixes)
                              comtab
                              (comtab-get comtab prefixes))
                          last-key
                          datum)))
          (else
           (error:wrong-type-argument keystroke "comtab key" procedure)))))

;;; A prefix key is a keystoke with a comtab and not a command
(define* (define-prefix-key mode key (command #f))
  (%define-key (guarantee-comtabs mode 'DEFINE-PREFIX-KEY)
               key
	       (let ((comtab (make-comtab)))
		 (if (not command)
		     comtab
		     (let ((command (->command command)))
		       (if (eq? command (ref-command-object prefix-key))
			   comtab
			   (cons command comtab)))))
	       'DEFINE-PREFIX-KEY))

;;; Used in keymap
(define (comtab->alist comtab)
  (let ((table (comtab-table comtab)))
    (hash-table->alist table)))

;;; Returns a list of keys depending on the order of comtabs that
;;; are passed to it.
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
      (list-sort (let ((v (lambda (k)
                            (cond ((simple-key? k) 0)
                                  ((special-key? k) 1)
                                  (else 2)))))
                   (lambda (k1 k2)
                     (< (v k1) (v k2))))

                 (let loop ((comtabs comtabs))
                   (if (null? comtabs)
                       '()
                       (%comtab-bindings (car comtabs)
                                         (loop (cdr comtabs))
                                         command
                                         valid-key?)))))))

(define (%comtab-bindings comtab keys command valid-key?)
  (let comtab-loop ((comtab comtab) (keys keys) (prefix '()))
    (let alist-loop ((entries (hash-table->alist (comtab-table comtab))))
      (if (null? entries)
	  keys
	  (let ((key
		 (if (pair? prefix)
		     (append prefix (list (caar entries)))
		     (caar entries))))
	    (let datum-loop
		((datum (cadar entries))
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

