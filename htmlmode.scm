#| -*-Scheme-*-

$Id: htmlmode.scm,v 1.17 2008/01/30 20:02:02 cph Exp $

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

;;;; Major Mode for XML


(define-major-mode xml text "XML"
  "Major mode for editing XML.

\\{xml}"
  (lambda (buffer)
    (local-set-variable! syntax-table xml-syntax-table buffer)
    (local-set-variable! indent-line-procedure
			 (ref-command indent-relative)
			 buffer)
    (local-set-variable! paragraph-separate xml-paragraph-separator buffer)
    (local-set-variable! paragraph-start xml-paragraph-separator buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-locator-hook xml-comment-locate buffer)
    (local-set-variable! comment-indent-hook xml-comment-indentation buffer)
    (local-set-variable! comment-start "<!-- " buffer)
    (local-set-variable! comment-end " -->" buffer)
    (local-set-variable! comment-multi-line #t buffer)
    (local-set-variable!
     sentence-end
     "[.?!][]\"')}]*\\(<[^>]*>\\)*\\($\\| $\\|\t\\|  \\)[ \t\n]*"
     buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable xml-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable xml-mode-hook buffer) buffer)))

(define xml-paragraph-separator
  (let ((lwsp (rexp* (char-set #\space #\tab #\U+A0))))
    (rexp->regexp
     (rexp-sequence
      (rexp-optional lwsp
		     "<"
		     (rexp* (char-set-difference char-set:graphic
						 (string->char-set ">")))
		     ">")
      lwsp
      (rexp-line-end)))))

(define-command xml-mode
  "Enter XML mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object xml))))

(define-variable xml-mode-abbrev-table
  "Mode-specific abbrev table for XML.")
(define-abbrev-table 'xml-mode-abbrev-table '())

(define-variable xml-mode-hook
  "An event distributor that is invoked when entering XML mode."
  (make-event-distributor))

(define xml-syntax-table
  (let ((syntax-table (make-char-syntax-table text-mode:syntax-table)))
    (set-char-syntax! syntax-table #\< "(>")
    (set-char-syntax! syntax-table #\! ". ")
    (set-char-syntax! syntax-table #\- "_ 1234")
    (set-char-syntax! syntax-table #\> ")<")
    (set-char-syntax! syntax-table #\" "\"\"")
    (set-char-syntax! syntax-table #\. "_")
    (set-char-syntax! syntax-table #\_ "_")
    (set-char-syntax! syntax-table #\: "_")
    syntax-table))

(define (xml-comment-locate mark)
  (and (re-search-forward "<!--+[ \t]*" (line-start mark 0) (line-end mark 0))
       (cons (re-match-start 0) (re-match-end 0))))

(define (xml-comment-indentation mark)
  mark
  0)

;; Backwards compatibility
(define-editor-alias mode html xml)
(define-editor-alias command html-mode xml-mode)
(define-editor-alias variable html-mode-hook xml-mode-hook)
(define-editor-alias variable html-mode-abbrev-table xml-mode-abbrev-table)
(define html-syntax-table xml-syntax-table)