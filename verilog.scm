#| -*-Scheme-*-

$Id: verilog.scm,v 1.15 2007/01/18 02:15:05 riastradh Exp $

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

;;;; Major Mode for Verilog Programs


(define-command verilog-mode
  "Enter Verilog mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object verilog))))

(define-major-mode verilog fundamental "Verilog"
  "Major mode specialized for editing Verilog code.

\\{verilog}"
  (lambda (buffer)
    (local-set-variable! syntax-table verilog-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook verilog-comment-locate buffer)
    (local-set-variable! comment-indent-hook verilog-comment-indentation
			 buffer)
    (local-set-variable! comment-start "// " buffer)
    (local-set-variable! comment-end "" buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! indent-line-procedure
			 (ref-command keyparser-indent-line)
			 buffer)
    (local-set-variable! definition-start verilog-defun-start-regexp buffer)
    (local-set-variable! require-final-newline #t buffer)
    (local-set-variable! keyparser-description verilog-description buffer)
    (local-set-variable! keyword-table verilog-keyword-table buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable verilog-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable verilog-mode-hook buffer)
			       buffer)))

(define verilog-mode:syntax-table
  (let ((syntax-table (make-char-syntax-table)))
    (for-each (lambda (char) (set-char-syntax! syntax-table char "."))
	      (string->list "+-=%<>&|"))
    (set-char-syntax! syntax-table #\' "_")
    (set-char-syntax! syntax-table #\` ". p")
    (set-char-syntax! syntax-table #\# ". p")
    (set-char-syntax! syntax-table #\@ ". p")
    (set-char-syntax! syntax-table #\/ ". 1456")
    (set-char-syntax! syntax-table #\* ". 23")
    (set-char-syntax! syntax-table #\newline ">")
    syntax-table))

(define-key 'verilog #\linefeed 'reindent-then-newline-and-indent)
(define-key 'verilog #\rubout 'backward-delete-char-untabify)
(define-key 'verilog #\tab 'keyparser-indent-line)
(define-key 'verilog #\c-m-\\ 'keyparser-indent-region)
(define-key 'verilog #\) 'lisp-insert-paren)
(define-key 'verilog #\] 'lisp-insert-paren)
(define-key 'verilog #\} 'lisp-insert-paren)
(define-key 'verilog #\m-tab 'complete-keyword)

;;;; Syntax Description

(define (verilog-comment-locate mark)
  (let ((state (parse-partial-sexp mark (line-end mark 0))))
    (and (parse-state-in-comment? state)
	 (verilog-comment-match-start (parse-state-comment-start state))
	 (cons (re-match-start 0) (re-match-end 0)))))

(define (verilog-comment-match-start mark)
  (re-match-forward "/\\(/+\\|\\*+\\)[ \t]*" mark))

(define (verilog-comment-indentation mark)
  (let ((column
	 (cond ((or (and (line-start? mark)
			 (match-forward "/*" mark))
		    (match-forward "////" mark))
		0)
	       ((match-forward "///" mark)
		(keyparser-compute-indentation mark #t))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ (mark-column (horizontal-space-start mark)) 1)
	     column))))

(define verilog-defun-start-regexp
  (string-append
   "^"
   (regexp-group "module" "macromodule" "primitive" "parameter")
   (regexp-group "\\s " "$")))

(define verilog-keyword-table
  (alist->string-table
   (map list
	'("always" "and" "assign" "begin" "buf" "bufif0" "bufif1"
		   "case" "casex" "casez" "cmos" "deassign" "default"
		   "define" "defparam" "disable" "else" "end"
		   "endcase" "endfunction" "endmodule" "endprimitive"
		   "endtable" "endtask" "event" "for" "force"
		   "forever" "fork" "function" "if" "ifdef" "include"
		   "initial" "inout" "input" "integer" "join" "macromodule"
		   "module" "nand" "negedge" "nmos" "nor" "not"
		   "notif0" "notif1" "or" "output" "parameter" "pmos"
		   "posedge" "primitive" "pulldown" "pullup" "rcmos"
		   "real" "reg" "release" "repeat" "rnmos" "rpmos"
		   "rtran" "rtranif0" "rtranif1" "scalared" "supply0"
		   "supply1" "table" "task" "time" "tran" "tranif0"
		   "tranif1" "tri" "tri0" "tri1" "triand" "trior"
		   "trireg" "udp" "vectored" "wait" "wand" "while"
		   "wire" "wor" "xnor" "xor"))
   #f))

(define (parse-forward-past-semicolon start end)
  (let loop ((start start) (state #f))
    (let ((semi (char-search-forward #\; start end #f)))
      (and semi
	   (let ((state (parse-partial-sexp start semi #f #f state)))
	     (if (in-char-syntax-structure? state)
		 (loop semi state)
		 semi))))))

(define (parse-forward-past-block-tag start end)
  (if (re-match-forward "[ \t]*:[ \t]*" start end)
      (forward-one-sexp start end)
      start))

(define (parse-forward-noop start end)
  end
  start)

(define (continued-header-indent mark)
  (+ (mark-indentation mark)
     (ref-variable verilog-continued-header-offset mark)))

(define (continued-statement-indent mark)
  (+ (mark-indentation mark)
     (ref-variable verilog-continued-statement-offset mark)))

(define verilog-description
  (make-keyparser-description
   'FIND-STATEMENT-END
   parse-forward-past-semicolon
   'INDENT-CONTINUED-STATEMENT
   continued-statement-indent
   'INDENT-CONTINUED-COMMENT
   (lambda (mark)
     (mark-column (or (verilog-comment-match-start mark) mark)))))

(define-keyparser-statement-leader #\# verilog-description
  (re-compile-char #\# #f)
  forward-one-sexp)

(define-keyparser-statement-leader #\@ verilog-description
  (re-compile-char #\@ #f)
  forward-one-sexp)

(define (define-standard-keyword keyword end parse-header)
  (define-keyparser-pattern keyword verilog-description
    (list
     (make-keyparser-fragment 'KEYWORD
			      keyword
			      'PARSE-HEADER
			      parse-header
			      'INDENT-HEADER
			      continued-header-indent
			      'PARSE-BODY
			      keyparse-forward
			      'INDENT-BODY
			      continued-statement-indent)
     (and end
	  (make-keyparser-fragment 'KEYWORD
				   end
				   'PARSE-HEADER
				   parse-forward-noop
				   'INDENT-HEADER
				   continued-header-indent
				   'PARSE-BODY
				   #f
				   'INDENT-BODY
				   #f)))))

(define-standard-keyword "always" #f
  parse-forward-noop)

(define-standard-keyword "begin" "end"
  parse-forward-past-block-tag)

(define-standard-keyword "case" "endcase"
  forward-one-sexp)

(define-standard-keyword "casex" "endcase"
  forward-one-sexp)

(define-standard-keyword "casez" "endcase"
  forward-one-sexp)

(define-standard-keyword "else" #f
  parse-forward-noop)

(define-standard-keyword "for" #f
  forward-one-sexp)

(define-standard-keyword "forever" #f
  parse-forward-noop)

(define-standard-keyword "fork" "join"
  parse-forward-past-block-tag)

(define-standard-keyword "function" "endfunction"
  parse-forward-past-semicolon)

(define-standard-keyword "if" #f
  forward-one-sexp)

(define-standard-keyword "initial" #f
  parse-forward-noop)

(define-standard-keyword "macromodule" "endmodule"
  parse-forward-past-semicolon)

(define-standard-keyword "module" "endmodule"
  parse-forward-past-semicolon)

(define-standard-keyword "primitive" "endprimitive"
  parse-forward-past-semicolon)

(define-standard-keyword "repeat" #f
  forward-one-sexp)

(define-standard-keyword "table" "endtable"
  parse-forward-noop)

(define-standard-keyword "task" "endtask"
  parse-forward-past-semicolon)

(define-standard-keyword "while" #f
  forward-one-sexp)