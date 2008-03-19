#| -*-Scheme-*-

$Id: calias.scm,v 1.36 2008/01/30 20:01:59 cph Exp $

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

;;;; Alias Keys


(define alias-keys '())

(define (define-alias-key key alias)
  (let ((entry (assq key alias-keys)))
    (if entry
	(set-cdr! entry alias)
	(set! alias-keys (cons (cons key alias) alias-keys))))
  unspecific)

(define (undefine-alias-key key)
  (set! alias-keys (alist-delete! key alias-keys eq?))
  unspecific)

(define (remap-alias-key key)
  (let ((entry (assq key alias-keys)))
    (cond (entry
	   (remap-alias-key (cdr entry)))
	  ((and (char? key)
		(char-bits-set? char-bit:control key))
	   (let ((code (char-code key))
		 (remap
		  (lambda (code)
		    (make-char code
			       (fix:andc (char-bits key) char-bit:control)))))
	     (cond ((<= #x40 code #x5F) (remap (- code #x40)))
		   ((<= #x61 code #x7A) (remap (- code #x60)))
		   (else key))))
	  (else key))))

(define (unmap-alias-key key)
  (if (and (char? key)
	   (ascii-controlified? key)
	   (let ((code (char-code key)))
	     (not (or (= code #x09)	;tab
		      (= code #x0A)	;linefeed
		      (= code #x0C)	;page
		      (= code #x0D)	;return
		      (= code #x1B)	;altmode
		      )))
	   (char-bits-clear? char-bit:control key))
      (unmap-alias-key
       (make-char (let ((code (char-code key)))
		    (+ code (if (<= #x01 code #x1A) #x60 #x40)))
		  (fix:or (char-bits key) char-bit:control)))
      (let ((entry
	     (find (lambda (entry) (eqv? (cdr entry) key))
		   alias-keys)))
	(if entry
	    (unmap-alias-key (car entry))
	    key))))

(define-variable enable-emacs-key-names
  "True means keys are shown using Emacs-style names."
  #t
  boolean?)

(define (key-name key)
  (cond ((ref-variable enable-emacs-key-names) (emacs-key-name key #t))
	((char? key) (char->name (unmap-alias-key key)))
	((special-key? key) (special-key/name key))
	((button? key) (button-name key))
        (else (error:wrong-type-argument key "key" 'KEY-NAME))))

(define (xkey->name xkey)
  (let ((keys (xkey->list xkey)))
    (string-append-separated
     (key-name (car keys))
     (let ((key-name
	    (if (ref-variable enable-emacs-key-names)
		(lambda (key)
		  (emacs-key-name key #f))
		(lambda (key)
		  (key-name (unmap-alias-key key))))))
       (let loop ((keys (cdr keys)))
	 (if (pair? keys)
	     (string-append-separated (key-name (car keys))
				      (loop (cdr keys)))
	     ""))))))

(define (emacs-key-name key handle-prefixes?)
  (cond ((char? key)
         (let ((code (char-code key))
               (bits (char-bits key)))
	   (define (prefix bits suffix)
	     (if (zero? bits)
		 suffix
		 (string-append "M-" suffix)))
	   (define (process-code bits)
	     (if (<= code #x20)
		 (cond ((= code #x09) (prefix bits "TAB"))
		       ((= code #x0A) (prefix bits "LFD"))
		       ((= code #x0D) (prefix bits "RET"))
		       ((= code #x1B) (prefix bits "ESC"))
		       ((= code #x20) (prefix bits "SPC"))
		       (else
			(string-append (if (zero? bits) "C-" "C-M-")
				       (string
					(integer->char
					 (+ (if (<= #x01 code #x1A) #x60 #x40)
					    code))))))
		 (prefix bits
			 (if (= code #x7F)
			     "DEL"
			     (vector-ref (ref-variable char-image-strings #f)
					 code)))))
	   (cond ((or (fix:= bits 0)
		      (fix:= bits char-bit:meta))
		  (process-code bits))
		 ((and handle-prefixes?
		       (not (fix:= 0 (fix:and bits
					      (fix:or char-bit:control
						      char-bit:meta)))))
		  (string-append (if (fix:= bits char-bit:control)
				     "C-^ "
				     "C-z ")
				 (process-code 0)))
		 (else
		  (char->name (unmap-alias-key key))))))
	((special-key? key) (special-key/name key))
	((button? key) (button-name key))
        (else (error:wrong-type-argument key "key" 'EMACS-KEY-NAME))))

(define (key? object)
  (or (char? object)
      (special-key? object)
      (button? object)))

(define (key-bucky-bits key)
  (cond ((char? key) (char-bits key))
	((special-key? key) (special-key/bucky-bits key))
	((button? key) (button-bits key))
        (else (error:wrong-type-argument key "key" 'KEY-BUCKY-BITS))))

(define (key<? key1 key2)
  (or (< (key-bucky-bits key1) (key-bucky-bits key2))
      (and (= (key-bucky-bits key1) (key-bucky-bits key2))
	   (cond ((char? key1)
		  (or (not (char? key2))
		      (char<? key1 key2)))
		 ((special-key? key1)
		  (if (special-key? key2)
		      (string<? (special-key/name key1)
				(special-key/name key2))
		      (button? key2)))
		 ((button? key1)
		  (and (button? key2)
		       (string<? (button-name key1) (button-name key2))))
		 (else
		  (error:wrong-type-argument key1 "key" 'KEY<?))))))

(define (key=? key1 key2)
  (and (= (key-bucky-bits key1) (key-bucky-bits key2))
       (cond ((char? key1)
	      (and (char? key2)
		   (char=? key1 key2)))
	     ((special-key? key1)
	      (and (special-key? key2)
		   (string=? (special-key/name key1) (special-key/name key2))))
	     ((button? key1)
	      (eq? key1 key2))
	     (else
	      (error:wrong-type-argument key1 "key" 'KEY=?)))))

(define (xkey<? x y)
  (let loop ((x (xkey->list x)) (y (xkey->list y)))
    (or (key<? (car x) (car y))
	(and (key=? (car x) (car y))
	     (pair? (cdr y))
	     (or (not (pair? (cdr x)))
		 (loop (cdr x) (cdr y)))))))

(define (xkey->list xkey)
  (cond ((or (key? xkey) (button? xkey))
	 (list xkey))
	((and (pair? xkey)
	      (list-of-type? xkey key?))
	 xkey)
	((and (string? xkey)
	      (not (string-null? xkey)))
	 (string->list xkey))
	(else
	 (error "Not a key or list of keys" xkey))))

;;;; Special Keys (system-dependent)

(define-structure (special-key (constructor %make-special-key)
			       (conc-name special-key/)
			       (print-procedure
				(standard-unparser-method 'SPECIAL-KEY
				  (lambda (key port)
				    (write-char #\space port)
				    (write-string (special-key/name key)
						  port)))))
  (symbol #f read-only #t)
  (bucky-bits #f read-only #t))

(define (intern-special-key name bucky-bits)
  (let ((name-entry (assq name (cdr hashed-keys))))
    (if name-entry
	(let ((bits-entry (assq bucky-bits (cdr name-entry))))
	  (if bits-entry
	      (cdr bits-entry)
	      (let ((new-key (%make-special-key name bucky-bits)))
		(set-cdr! name-entry
			  (cons (cons bucky-bits new-key)
				(cdr name-entry)))
		new-key)))
	(let ((new-key (%make-special-key name bucky-bits)))
	  (set-cdr! hashed-keys
		    (cons (cons name (list (cons bucky-bits new-key)))
			  (cdr hashed-keys)))
	  new-key))))

(define (special-key/name special-key)
  (string-append (bucky-bits->prefix (special-key/bucky-bits special-key))
		 (symbol-name (special-key/symbol special-key))))

(define (make-special-key name bits)
  (hook/make-special-key name bits))

(define hashed-keys (list 'HASHED-KEYS))
(define hook/make-special-key intern-special-key)

;; Predefined special keys
(define-syntax define-special-key
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(DEFINE ,(cadr form)
	(INTERN-SPECIAL-KEY ',(cadr form) 0)))))

(define-special-key backspace)
(define-special-key stop)
(define-special-key f1)
(define-special-key f2)
(define-special-key f3)
(define-special-key f4)
(define-special-key menu)
(define-special-key system)
(define-special-key user)
(define-special-key f5)
(define-special-key f6)
(define-special-key f7)
(define-special-key f8)
(define-special-key f9)
(define-special-key f10)
(define-special-key f11)
(define-special-key f12)
(define-special-key insertline)
(define-special-key deleteline)
(define-special-key insertchar)
(define-special-key deletechar)
(define-special-key home)
(define-special-key prior)
(define-special-key next)
(define-special-key up)
(define-special-key down)
(define-special-key left)
(define-special-key right)
(define-special-key select)
(define-special-key print)