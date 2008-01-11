#| -*-Scheme-*-

$Id: comman.scm,v 1.90 2007/01/05 21:19:23 cph Exp $

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

;;;; Commands and Variables


(define-structure (command
		   (constructor %make-command ())
		   (print-procedure
		    (unparser/standard-method 'COMMAND
		      (lambda (state command)
			(unparse-object state (command-name command))))))
  name
  %description
  interactive-specification
  procedure)

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

(define (name->command name #!optional if-undefined)
  (or (string-table-get editor-commands (symbol-name name))
      (case (if (default-object? if-undefined) 'INTERN if-undefined)
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

(define-structure (variable
		   (constructor %make-variable ())
		   (print-procedure
		    (unparser/standard-method 'VARIABLE
		      (lambda (state variable)
			(unparse-object state (variable-name variable))))))
  name
  %description
  %value
  buffer-local?
  initial-value
  %default-value
  assignment-daemons
  value-validity-test
  value-normalization)

(define (variable-description variable)
  (let ((desc (variable-%description variable)))
    (if (description? desc)
	desc
	(let ((new (->doc-string (symbol-name (variable-name variable)) desc)))
	  (if new
	      (set-variable-%description! variable new))
	  new))))

(define variable-value variable-%value)
(define variable-default-value variable-%default-value)

(define (variable-name-string variable)
  (editor-name/internal->external (symbol-name (variable-name variable))))

(define (make-variable name description value buffer-local?
		       #!optional test normalization)
  (let* ((sname (symbol-name name))
	 (variable
	  (or (string-table-get editor-variables sname)
	      (let ((variable (%make-variable)))
		(string-table-put! editor-variables sname variable)
		variable))))
    (set-variable-name! variable name)
    (set-variable-%description! variable (doc-string->posn sname description))
    (set-variable-%value! variable value)
    (set-variable-buffer-local?! variable buffer-local?)
    (set-variable-initial-value! variable value)
    (set-variable-%default-value! variable value)
    (set-variable-assignment-daemons! variable '())
    ;; Next two are written strangely because DEFAULT-OBJECT?
    ;; expansion contains (THE-ENVIRONMENT), which can't be inlined.
    (if (default-object? test)
	(set-variable-value-validity-test! variable #f)
	(set-variable-value-validity-test! variable test))
    (if (default-object? normalization)
	(set-variable-value-normalization! variable #f)
	(set-variable-value-normalization! variable normalization))
    variable))

(define (make-variable-buffer-local! variable)
  (set-variable-buffer-local?! variable #t))

(define (normalize-variable-value variable value)
  (if (and (variable-value-validity-test variable)
	   (not ((variable-value-validity-test variable) value)))
      (editor-error "Invalid value for " (variable-name-string variable)
		    ": " value))
  (if (variable-value-normalization variable)
      ((variable-value-normalization variable) value)
      value))

(define (add-variable-assignment-daemon! variable daemon)
  (let ((daemons (variable-assignment-daemons variable)))
    (if (not (memq daemon daemons))
	(set-variable-assignment-daemons! variable (cons daemon daemons)))))

(define (invoke-variable-assignment-daemons! buffer variable)
  (if within-editor?
      (do ((daemons (variable-assignment-daemons variable) (cdr daemons)))
	  ((null? daemons))
	((car daemons) buffer variable))))

(define editor-variables
  (make-string-table 50))

(define (name->variable name #!optional if-undefined)
  (or (string-table-get editor-variables (symbol-name name))
      (case (if (default-object? if-undefined) 'INTERN if-undefined)
	((#F) #f)
	((ERROR) (error "Undefined variable:" name))
	((INTERN) (make-variable name "" #f #f))
	(else (error:bad-range-argument if-undefined 'NAME->VARIABLE)))))

(define (->variable object)
  (if (variable? object)
      object
      (name->variable object)))

(define (variable-permanent-local! variable)
  (hash-table/put! permanent-local-variables variable #t))

(define (variable-permanent-local? variable)
  (hash-table/get permanent-local-variables variable #f))

(define permanent-local-variables
  (make-eq-hash-table))