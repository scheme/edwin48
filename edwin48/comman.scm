#| -*-Scheme-*-

$Id: comman.scm,v 1.92 2008/01/30 20:01:59 cph Exp $

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

;;;; Commands and Variables


(define-record-type* command
  (%make-command)
  (name
   %description
   interactive-specification
   procedure))

(define (command-description command)
  (let ((desc (command-%description command)))
    (if (description? desc)
	desc
	(let ((new (->doc-string (symbol-name (command-name command)) desc)))
	  (if new
	      (set-command-%description! command new))
	  new))))

(define (command-name-string command)
  (editor-name/internal->external (symbol-name (command-name command))))

(define (editor-name/internal->external string)
  string)

(define (editor-name/external->internal string)
  string)

(define (make-command name description specification procedure)
  (let* ((sname (symbol-name name))
	 (command
	  (or (string-table-get editor-commands sname)
	      (let ((command (%make-command)))
		(string-table-put! editor-commands sname command)
		command))))
    (set-command-name! command name)
    (set-command-%description! command (doc-string->posn sname description))
    (set-command-interactive-specification! command specification)
    (set-command-procedure! command procedure)
    command))

(define editor-commands
  (make-string-table 500))

(define* (name->command name (if-undefined 'INTERN))
  (or (string-table-get editor-commands (symbol-name name))
      (case if-undefined
	((#F) #f)
	((ERROR) (error "Undefined command:" name))
	((INTERN)
	 (letrec ((command
		   (make-command
		    name
		    "undefined command"
		    '()
		    (lambda () (editor-error "Undefined command:" name)))))
	   command))
	(else
	 (error:bad-range-argument if-undefined 'NAME->COMMAND)))))

(define (->command object)
  (if (command? object)
      object
      (name->command object)))

(define (copy-command new-name command)
  (make-command new-name
		(command-%description command)
		(command-interactive-specification command)
		(command-procedure command)))

