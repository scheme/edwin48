#| -*-Scheme-*-

$Id: tximod.scm,v 1.34 2008/01/30 20:02:06 cph Exp $

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

;;;; Texinfo Mode


(define-command texinfo-mode
  "Make the current mode be Texinfo mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object texinfo))))

(define-major-mode texinfo text "Texinfo"
  "Major mode for editing Texinfo files.

  These are files that are used as input for TeX to make printed manuals
and also to be turned into Info files by \\[texinfo-format-buffer] or
`makeinfo'.  These files must be written in a very restricted and
modified version of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Texinfo bracket groups.

  In addition, Texinfo mode provides commands that insert various
frequently used @-sign commands into the buffer.  You can use these
commands to save keystrokes.

\\{texinfo}"
  (lambda (buffer)
    (local-set-variable! syntax-table texinfo-mode:syntax-table buffer)
    (local-set-variable! fill-column 72 buffer)
    (local-set-variable! require-final-newline #t buffer)
    (local-set-variable! indent-tabs-mode #f buffer)
    (local-set-variable!
     page-delimiter
     "^@\\(chapter\\|unnumbered \\|appendix \\|majorheading\\|chapheading\\)"
     buffer)
    (local-set-variable! paragraph-start
			 (string-append "\010\\|@[a-zA-Z]*[ \n]\\|"
					(ref-variable paragraph-start buffer))
			 buffer)
    (local-set-variable! paragraph-separate
			 (string-append "\010\\|@[a-zA-Z]*[ \n]\\|"
					(ref-variable paragraph-separate
						      buffer))
			 buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable texinfo-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable texinfo-mode-hook buffer)
			       buffer)))

(define texinfo-mode:syntax-table (make-char-syntax-table))
(set-char-syntax! texinfo-mode:syntax-table #\" " ")
(set-char-syntax! texinfo-mode:syntax-table #\\ " ")
(set-char-syntax! texinfo-mode:syntax-table #\@ "\\")
(set-char-syntax! texinfo-mode:syntax-table #\DC1 "\\")
(set-char-syntax! texinfo-mode:syntax-table #\' "w")

(define-key 'texinfo (kbd (ctrl #\c) #\{) 'texinfo-insert-braces)
(define-key 'texinfo (kbd (ctrl #\c) #\}) 'up-list)

(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\c) 'texinfo-insert-@code)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\d) 'texinfo-insert-@dfn)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\e) 'texinfo-insert-@end)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\f) 'texinfo-insert-@file)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\i) 'texinfo-insert-@item)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\k) 'texinfo-insert-@kbd)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\n) 'texinfo-insert-@node)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\o) 'texinfo-insert-@noindent)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\s) 'texinfo-insert-@samp)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\t) 'texinfo-insert-@table)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\v) 'texinfo-insert-@var)
(define-key 'texinfo (kbd (ctrl #\c) (ctrl #\c) #\x) 'texinfo-insert-@example)

(define ((texinfo-insert-@-with-arg keyword) argument)
  (call-with-values
      (lambda ()
	(if argument
	    (let ((n (command-argument-value argument)))
	      (if (< n 0)
		  (let ((end
			 (skip-chars-backward " \t\n\r\f" (current-point))))
		    (values (forward-sexp end n 'ERROR) end))
		  (let ((start
			 (skip-chars-forward " \t\n\r\f" (current-point))))
		    (values start (forward-sexp start n 'ERROR)))))
	    (let ((start (current-point)))
	      (values start start))))
    (lambda (start end)
      (let ((end (mark-left-inserting-copy end)))
	(insert-string (string-append "@" keyword "{") start)
	(insert-string "}" end)
	(set-current-point! (if argument end (mark-1+ end)))
	(mark-temporary! end)))))

(define-command texinfo-insert-braces
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  ()
  (lambda ()
    (insert-string "{}")
    (set-current-point! (mark-1+ (current-point)))))

(define-command texinfo-insert-@code
  "Insert a `@code{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "code"))

(define-command texinfo-insert-@dfn
  "Insert a `@dfn{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "dfn"))

(define-command texinfo-insert-@example
  "Insert the string `@example' in a texinfo buffer."
  ()
  (lambda () (insert-string "@example\n")))

(define-command texinfo-insert-@file
  "Insert a `@file{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "file"))

(define-command texinfo-insert-@item
  "Insert the string `@item' in a texinfo buffer."
  ()
  (lambda () (insert-string "@item\n")))

(define-command texinfo-insert-@kbd
  "Insert a `@kbd{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "kbd"))

(define-command texinfo-insert-@node
  "Insert the string `@node' in a texinfo buffer.
This also inserts on the following line a comment indicating
the order of arguments to @node."
  ()
  (lambda ()
    (insert-string "@node ")
    (let ((m (mark-right-inserting-copy (current-point))))
      (insert-string "\n@comment  node-name,  next,  previous,  up")
      (set-current-point! m)
      (mark-temporary! m))))

(define-command texinfo-insert-@noindent
  "Insert the string `@noindent' in a texinfo buffer."
  ()
  (lambda () (insert-string "@noindent\n")))

(define-command texinfo-insert-@samp
  "Insert a `@samp{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "samp"))

(define-command texinfo-insert-@table
  "Insert the string `@table' in a texinfo buffer."
  ()
  (lambda () (insert-string "@table ")))

(define-command texinfo-insert-@var
  "Insert a `@var{...}' command in a texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  "P"
  (texinfo-insert-@-with-arg "var"))

(define-command texinfo-insert-@end
  "Insert the matching `@end' for the last Texinfo command that needs one."
  ()
  (lambda ()
    (let ((start (buffer-start (selected-buffer))))
      (let loop ((mark (current-point)) (depth 1))
	(let ((m
	       (re-search-backward texinfo-environment-regexp mark start #f)))
	  (if m
	      (let ((string (re-match-extract-string 1)))
		(cond ((string=? string "end")
		       (loop m (+ depth 1)))
		      ((> depth 1)
		       (loop m (- depth 1)))
		      (else
		       (insert-string "@end ")
		       (insert-string string)
		       (insert-newline))))
	      (insert-string "@end ")))))))

(define texinfo-environment-regexp
  (string-append "^@"
		 (regexp-group "cartouche"
			       "copying"
			       "defcv"
			       "deffn"
			       "defivar"
			       "defmac"
			       "defmethod"
			       "defop"
			       "defopt"
			       "defspec"
			       "deftp"
			       "deftypefn"
			       "deftypefun"
			       "deftypevar"
			       "deftypevr"
			       "defun"
			       "defvar"
			       "defvr"
			       "description"
			       "detailmenu"
			       "direntry"
			       "display"
			       "end"
			       "enumerate"
			       "example"
			       "flushleft"
			       "flushright"
			       "format"
			       "ftable"
			       "group"
			       "html"
			       "ifclear"
			       "ifhtml"
			       "ifinfo"
			       "ifnothtml"
			       "ifnotinfo"
			       "ifnottex"
			       "ifset"
			       "iftex"
			       "ignore"
			       "itemize"
			       "lisp"
			       "macro"
			       "menu"
			       "multitable"
			       "quotation"
			       "smalldisplay"
			       "smallexample"
			       "smallformat"
			       "smalllisp"
			       "table"
			       "tex"
			       "titlepage"
			       "vtable")
		 "\\>"))