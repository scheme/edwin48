#| -*-Scheme-*-

$Id: vhdl.scm,v 1.14 2007/01/05 21:19:24 cph Exp $

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

;;;; Major Mode for VHDL Programs

(declare (usual-integrations))

(define-command vhdl-mode
  "Enter VHDL mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object vhdl))))

(define-major-mode vhdl fundamental "VHDL"
  "Major mode specialized for editing VHDL code.

\\{vhdl}"
  (lambda (buffer)
    (local-set-variable! syntax-table vhdl-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #f buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook vhdl-comment-locate buffer)
    (local-set-variable! comment-indent-hook vhdl-comment-indentation buffer)
    (local-set-variable! comment-start "-- " buffer)
    (local-set-variable! comment-end "" buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! indent-line-procedure
			 (ref-command keyparser-indent-line)
			 buffer)
    (local-set-variable! definition-start vhdl-defun-start-regexp buffer)
    (local-set-variable! require-final-newline #t buffer)
    (local-set-variable! keyparser-description vhdl-description buffer)
    (local-set-variable! keyword-table vhdl-keyword-table buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable vhdl-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable vhdl-mode-hook buffer)
			       buffer)))

(define vhdl-mode:syntax-table
  (let ((syntax-table (make-char-syntax-table)))
    (for-each (lambda (char) (set-char-syntax! syntax-table char "_"))
	      (string->list "_.#+"))
    (for-each (lambda (char) (set-char-syntax! syntax-table char "."))
	      (string->list "*/&|<>=$%"))
    (set-char-syntax! syntax-table #\\ "\"")
    (set-char-syntax! syntax-table #\' "\"")
    (set-char-syntax! syntax-table #\- "_ 56")
    (set-char-syntax! syntax-table #\newline ">")
    syntax-table))

(define-key 'vhdl #\linefeed 'reindent-then-newline-and-indent)
(define-key 'vhdl #\rubout 'backward-delete-char-untabify)
(define-key 'vhdl #\tab 'keyparser-indent-line)
(define-key 'vhdl #\c-m-\\ 'keyparser-indent-region)
(define-key 'vhdl #\) 'lisp-insert-paren)
(define-key 'vhdl #\] 'lisp-insert-paren)
(define-key 'vhdl #\} 'lisp-insert-paren)
(define-key 'vhdl #\m-tab 'complete-keyword)

;;;; Syntax Description

(define (vhdl-comment-locate mark)
  (let ((state (parse-partial-sexp mark (line-end mark 0))))
    (and (parse-state-in-comment? state)
	 (vhdl-comment-match-start (parse-state-comment-start state))
	 (cons (re-match-start 0) (re-match-end 0)))))

(define (vhdl-comment-match-start mark)
  (re-match-forward "--+[ \t]*" mark))

(define (vhdl-comment-indentation mark)
  (let ((column
	 (cond ((match-forward "----" mark)
		0)
	       ((match-forward "---" mark)
		(keyparser-compute-indentation mark #t))
	       ((let ((s.e
		       (let ((ls (line-start mark -1)))
			 (and ls
			      (vhdl-comment-locate ls)))))
		  (and s.e
		       (mark-column (car s.e)))))
	       (else
		(ref-variable comment-column mark)))))
    (if (within-indentation? mark)
	column
	(max (+ (mark-column (horizontal-space-start mark)) 1)
	     column))))

(define vhdl-defun-start-regexp
  (string-append
   "^"
   (regexp-group "architecture" "configuration" "entity"
		 "library" "package" "use")
   (regexp-group "[^a-zA-Z0-9_]" "$")))

(define vhdl-keyword-table
  (alist->string-table
   (map list
	'("abs" "access" "after" "alias" "all" "and" "architecture" "array"
	  "assert" "attribute" "begin" "block" "body" "buffer" "bus" "case"
	  "component" "configuration" "constant" "disconnect" "downto" "else"
	  "elsif" "end" "entity" "exit" "file" "for" "function" "generate"
	  "generic" "group" "guarded" "if" "impure" "in" "inertial" "inout"
	  "is" "label" "library" "linkage" "literal" "loop" "map" "mod" "nand"
	  "new" "next" "nor" "not" "null" "of" "on" "open" "or" "others" "out"
	  "package" "port" "postponed" "procedure" "process" "pure" "range"
	  "record" "register" "reject" "rem" "report" "return" "rol" "ror"
	  "select" "severity" "signal" "shared" "sla" "sll" "sra" "srl"
	  "subtype" "then" "to" "transport" "type" "unaffected" "units" "until"
	  "use" "variable" "wait" "when" "while" "with" "xnor" "xor"))
   #f))

(define (continued-header-indent mark)
  (+ (mark-indentation mark)
     (ref-variable vhdl-continued-header-offset mark)))

(define (continued-statement-indent mark)
  (+ (mark-indentation mark)
     (ref-variable vhdl-continued-statement-offset mark)))

(define comatch:skip-whitespace
  (comatch:general
   (lambda (start end)
     (let loop ((start start))
       (let ((start (skip-chars-forward " \t\f\n" start end)))
	 (if (match-forward "--" start end)
	     (let ((le (line-end start 0)))
	       (and (mark<= le end)
		    (loop le)))
	     start))))))

(define comatch:identifier-end
  (comatch:general
   (lambda (start end)
     (and (re-match-forward "[^a-zA-Z0-9_]\\|$" start end)
	  start))))

(define comatch:identifier
  (comatch:append comatch:skip-whitespace
		  (comatch:regexp "[a-zA-Z][a-zA-Z0-9_]*")
		  comatch:identifier-end))

(define (comatch:keyword keyword)
  (comatch:append comatch:skip-whitespace
		  (comatch:string keyword)
		  comatch:identifier-end))

(define (comatch:matched-sexp comatcher)
  (comatch:append comatch:skip-whitespace
		  (comatch:and comatcher
			       comatch:sexp)))

(define comatch:list
  (comatch:matched-sexp (comatch:char #\()))

(define comatch:name
  (let ((id-or-string
	 (comatch:or comatch:identifier
		     (comatch:matched-sexp (comatch:char #\")))))
    (comatch:append
     id-or-string
     (comatch:*
      (comatch:append
       comatch:skip-whitespace
       (comatch:or (comatch:append
		    (comatch:char #\.)
		    (comatch:or id-or-string
				(comatch:matched-sexp (comatch:char #\'))))
		   comatch:list
		   (comatch:append
		    (comatch:? (comatch:matched-sexp (comatch:char #\[)))
		    (comatch:char #\')
		    comatch:identifier)))))))

(define comatch:for-header:control
  (comatch:append comatch:identifier
		  (comatch:keyword "in")))

(define comatch:for-header:component
  (comatch:append comatch:identifier
		  (comatch:*
		   (comatch:append comatch:skip-whitespace
				   (comatch:char #\,)
				   comatch:identifier))
		  comatch:skip-whitespace
		  (comatch:char #\:)))

(define comatch:for-header:block
  (comatch:not (comatch:or comatch:for-header:control
			   comatch:for-header:component)))

(define ((parse-forward-past search) start end)
  (let loop ((start start) (state #f))
    (let ((mark (search start end)))
      (and mark
	   (let ((state (parse-partial-sexp start mark #f #f state)))
	     (if (in-char-syntax-structure? state)
		 (loop mark state)
		 mark))))))

(define (parse-forward-past-char char)
  (parse-forward-past
   (lambda (start end) (char-search-forward char start end #f))))

(define parse-forward-past-semicolon
  (parse-forward-past-char #\;))

(define (parse-forward-past-token token)
  (parse-forward-past
   (let ((regexp
	  (string-append (regexp-group "[^a-zA-Z0-9_]" "^")
			 token
			 (regexp-group "[^a-zA-Z0-9_]" "$"))))
     (lambda (start end)
       (re-search-forward regexp start end)))))

(define parse-forward-past-is
  (parse-forward-past-token "is"))

(define parse-forward-past-then
  (parse-forward-past-token "then"))

(define parse-forward-past-=>
  (parse-forward-past-token "=>"))

(define (parse-forward-noop start end)
  end
  start)

(define (parse-comatch comatcher)
  (lambda (start end)
    (comatch-apply comatcher start end)))

(define parse-forward-past-name
  (parse-comatch comatch:name))

(define (trailing-keyword-matcher keyword . keywords)
  (let ((parser
	 (parse-forward-past-token (apply regexp-group keyword keywords))))
    (lambda (mark stack)
      stack
      (let ((m (parser mark (group-end mark))))
	(and m
	     (let ((s (backward-one-sexp m)))
	       (and s
		    (let ((e (forward-one-sexp s)))
		      (and e
			   (string-ci=? keyword (extract-string s e))
			   m)))))))))

(define vhdl-description
  (make-keyparser-description
   'FIND-STATEMENT-END
   parse-forward-past-semicolon
   'INDENT-CONTINUED-STATEMENT
   continued-statement-indent
   'INDENT-CONTINUED-COMMENT
   (lambda (mark)
     (mark-column (or (vhdl-comment-match-start mark) mark)))))

(define-keyparser-statement-leader 'LABEL vhdl-description
  "[a-zA-Z][a-zA-Z0-9_]*\\s *:"
  parse-forward-noop)

(define (define-matched-keyword pkey keyword match-header parse-header end
	  . rest)
  (define-keyparser-pattern pkey vhdl-description
    (cons* (standard-keyword keyword match-header parse-header)
	   end
	   rest)))

(define (define-standard-keyword pkey keyword parse-header end . rest)
  (apply define-matched-keyword pkey keyword #f parse-header end rest))

(define (standard-keyword keyword match-header parse-header . rest)
  (apply make-keyparser-fragment
	 'KEYWORD keyword
	 'MATCH-HEADER match-header
	 'PARSE-HEADER parse-header
	 'INDENT-HEADER continued-header-indent
	 'PARSE-BODY keyparse-forward
	 'INDENT-BODY continued-statement-indent
	 rest))

(define begin-frag (standard-keyword "begin" #f parse-forward-noop))
(define end-frag (standard-keyword "end" #f parse-forward-past-semicolon))

(define-standard-keyword 'ARCHITECTURE "architecture"
  parse-forward-past-is
  end-frag
  begin-frag)

(define-standard-keyword 'BLOCK "block"
  (parse-comatch
   (comatch:append (comatch:? comatch:list)
		   (comatch:? (comatch:keyword "is"))))
  end-frag
  begin-frag)

(define-standard-keyword 'CASE "case"
  parse-forward-past-is
  end-frag)

(define-standard-keyword 'COMPONENT "component"
  (parse-comatch
   (comatch:append comatch:identifier
		   (comatch:? (comatch:keyword "is"))))
  end-frag
  begin-frag)

(define-standard-keyword 'CONFIGURATION "configuration"
  parse-forward-past-is
  end-frag)

(define-standard-keyword 'ENTITY "entity"
  parse-forward-past-is
  end-frag
  begin-frag)

(define-standard-keyword 'FUNCTION "function"
  parse-forward-past-is
  end-frag
  begin-frag)

(define-standard-keyword '(FUNCTION IMPURE) "impure"
  parse-forward-past-is
  end-frag
  begin-frag)

(define-standard-keyword '(FUNCTION PURE) "pure"
  parse-forward-past-is
  end-frag
  begin-frag)

(define-matched-keyword '(GENERATE FOR) "for"
  (let ((parser (trailing-keyword-matcher "generate" "loop")))
    (lambda (mark stack)
      (let ((mark (comatch-apply comatch:for-header:control mark)))
	(and mark
	     (parser mark stack)))))
  parse-forward-noop
  end-frag)

(define-matched-keyword '(GENERATE IF) "if"
  (trailing-keyword-matcher "generate" "then")
  parse-forward-noop
  end-frag)

(define-matched-keyword 'IF "if"
  (trailing-keyword-matcher "then" "generate")
  parse-forward-noop
  end-frag
  (standard-keyword "elsif" #f parse-forward-past-then)
  (standard-keyword "else" #f parse-forward-noop))

(define-standard-keyword 'LOOP "loop"
  parse-forward-noop
  end-frag)

(define-matched-keyword '(LOOP FOR) "for"
  (let ((parser (trailing-keyword-matcher "loop" "generate")))
    (lambda (mark stack)
      (let ((mark (comatch-apply comatch:for-header:control mark)))
	(and mark
	     (parser mark stack)))))
  parse-forward-noop
  end-frag)

(define-standard-keyword '(LOOP WHILE) "while"
  (parse-forward-past-token "loop")
  end-frag)

(define-standard-keyword 'PACKAGE "package"
  parse-forward-past-is
  end-frag)

(define-standard-keyword 'PROCEDURE "procedure"
  parse-forward-past-is
  end-frag
  begin-frag)

(define-standard-keyword 'PROCESS "process"
  (parse-comatch
   (comatch:append (comatch:? comatch:list)
		   (comatch:? (comatch:keyword "is"))))
  end-frag
  begin-frag)

(define-standard-keyword '(PROCESS POSTPONED) "postponed"
  (parse-comatch
   (comatch:append (comatch:keyword "process")
		   (comatch:? comatch:list)
		   (comatch:? (comatch:keyword "is"))))
  end-frag
  begin-frag)

(define-standard-keyword 'RECORD "record"
  parse-forward-noop
  end-frag)

(define-standard-keyword 'UNITS "range"
  (parse-forward-past-token "units")
  end-frag)

(define-standard-keyword 'WHEN "when"
  parse-forward-past-=>
  (standard-keyword "end" #f parse-forward-past-semicolon 'POP-CONTAINER 1)
  (standard-keyword "when" #f parse-forward-past-=>))

(define-standard-keyword 'WITH "with"
  (parse-forward-past-token "select")
  #f)

(define-matched-keyword 'COMPONENT-SPECIFICATION "for"
  (lambda (mark stack)
    (let ((mark (comatch-apply comatch:for-header:component mark)))
      (and mark
	   (in-configuration? stack)
	   mark)))
  parse-forward-past-name
  end-frag)

(define-matched-keyword 'CONFIGURATION-SPECIFICATION "for"
  (lambda (mark stack)
    (let ((mark (comatch-apply comatch:for-header:component mark)))
      (and mark
	   (not (in-configuration? stack))
	   mark)))
  parse-forward-past-name
  #f)

(define (in-configuration? stack)
  (there-exists? stack
    (lambda (entry)
      (equal? 'CONFIGURATION (keyparser-stack-entry/keyword entry)))))

(define-matched-keyword 'BLOCK-CONFIGURATION "for"
  (lambda (mark stack)
    stack
    (and (comatch-apply comatch:for-header:block mark)
	 mark))
  parse-forward-noop
  end-frag)