#| -*-Scheme-*-

$Id: bufset.scm,v 1.17 2007/01/05 21:19:23 cph Exp $

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

;;;; Buffer Set Abstraction

(declare (usual-integrations))

(define-structure (bufferset (constructor %make-bufferset))
  buffer-list
  (names #f read-only #t))

(define (make-bufferset initial-buffer)
  (%make-bufferset (list initial-buffer)
		   (let ((names (make-string-table 16 false)))
		     (string-table-put! names
					(buffer-name initial-buffer)
					initial-buffer)
		     names)))

(define (bufferset-select-buffer! bufferset buffer)
  (if (memq buffer (bufferset-buffer-list bufferset))
      (set-bufferset-buffer-list!
       bufferset
       (cons buffer (delq! buffer (bufferset-buffer-list bufferset)))))
  unspecific)

(define (bufferset-bury-buffer! bufferset buffer)
  (if (memq buffer (bufferset-buffer-list bufferset))
      (set-bufferset-buffer-list!
       bufferset
       (append! (delq! buffer (bufferset-buffer-list bufferset))
		(list buffer))))
  unspecific)

(define (bufferset-guarantee-buffer! bufferset buffer)
  (if (not (memq buffer (bufferset-buffer-list bufferset)))
      (begin
	(string-table-put! (bufferset-names bufferset)
			   (buffer-name buffer)
			   buffer)
	(set-bufferset-buffer-list! bufferset
				    (append! (bufferset-buffer-list bufferset)
					     (list buffer)))))
  unspecific)

(define (bufferset-find-buffer bufferset name)
  (string-table-get (bufferset-names bufferset) name))

(define (bufferset-create-buffer bufferset name)
  (if (bufferset-find-buffer bufferset name)
      (error "Attempt to re-create buffer" name))
  (let ((buffer
	 (make-buffer name
		      (ref-variable editor-default-mode)
		      (if within-editor?
			  (buffer-default-directory (current-buffer))
			  (working-directory-pathname)))))
    (string-table-put! (bufferset-names bufferset) name buffer)
    (set-bufferset-buffer-list!
     bufferset
     (append! (bufferset-buffer-list bufferset) (list buffer)))
    buffer))

(define (bufferset-find-or-create-buffer bufferset name)
  (or (bufferset-find-buffer bufferset name)
      (bufferset-create-buffer bufferset name)))

(define (bufferset-kill-buffer! bufferset buffer)
  (if (not (memq buffer (bufferset-buffer-list bufferset)))
      (error "Attempt to kill unknown buffer" buffer))
  (set-bufferset-buffer-list! bufferset
			      (delq! buffer (bufferset-buffer-list bufferset)))
  (string-table-remove! (bufferset-names bufferset) (buffer-name buffer)))

(define (bufferset-rename-buffer bufferset buffer new-name)
  (if (not (memq buffer (bufferset-buffer-list bufferset)))
      (error "Attempt to rename unknown buffer" buffer))
  (if (bufferset-find-buffer bufferset new-name)
      (error "Attempt to rename buffer to existing buffer name" new-name))
  (let ((names (bufferset-names bufferset)))
    (string-table-remove! names (buffer-name buffer))
    (set-buffer-name! buffer new-name)
    (string-table-put! names new-name buffer)))