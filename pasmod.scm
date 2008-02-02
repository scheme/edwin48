#| -*-Scheme-*-

$Id: pasmod.scm,v 1.57 2008/01/30 20:02:04 cph Exp $

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

;;;; Pascal Mode


(define-command pascal-mode
  "Enter Pascal mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object pascal))))

(define-major-mode pascal fundamental "Pascal"
  "Major mode specialized for editing Pascal code.

\\{pascal}"
  (lambda (buffer)
    (local-set-variable! syntax-table pascal-mode:syntax-table buffer)
    (local-set-variable! syntax-ignore-comments-backwards #t buffer)
    (local-set-variable! indent-line-procedure
			 (ref-command pascal-indent-line)
			 buffer)
    (local-set-variable! comment-column 32 buffer)
    (local-set-variable! comment-locator-hook pascal-comment-locate buffer)
    (local-set-variable! comment-indent-hook pascal-comment-indentation buffer)
    (local-set-variable! comment-start "(* " buffer)
    (local-set-variable! comment-end " *)" buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable pascal-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable pascal-mode-hook buffer) buffer)))

(define pascal-mode:syntax-table (make-char-syntax-table))
(set-char-syntax! pascal-mode:syntax-table #\( "()1 ")
(set-char-syntax! pascal-mode:syntax-table #\) ")( 4")
(set-char-syntax! pascal-mode:syntax-table #\[ "(]  ")
(set-char-syntax! pascal-mode:syntax-table #\] ")[  ")
(set-char-syntax! pascal-mode:syntax-table #\{ "<   ")
(set-char-syntax! pascal-mode:syntax-table #\} ">   ")
(set-char-syntax! pascal-mode:syntax-table #\' "\"   ")
(set-char-syntax! pascal-mode:syntax-table #\$ "\"   ")
(set-char-syntax! pascal-mode:syntax-table #\* "_ 23")
(set-char-syntax! pascal-mode:syntax-table #\. "_   ")
(set-char-syntax! pascal-mode:syntax-table #\^ "_   ")
(set-char-syntax! pascal-mode:syntax-table #\@ "'   ")
(set-char-syntax! pascal-mode:syntax-table #\% "    ")
(set-char-syntax! pascal-mode:syntax-table #\" "    ")
(set-char-syntax! pascal-mode:syntax-table #\\ "    ")

(define (pascal-comment-locate mark)
  (if (re-search-forward "\\((\\*\\|{\\)[ \t]*" mark (line-end mark 0))
      (cons (re-match-start 0) (re-match-end 0))))

(define (pascal-comment-indentation mark)
  (let ((start (horizontal-space-start mark)))
    (if (line-start? start)
	(indentation-of-previous-non-blank-line mark)
	(max (1+ (mark-column start))
	     (ref-variable comment-column)))))

(define-key 'pascal #\c-\( 'pascal-shift-left)
(define-key 'pascal #\c-\) 'pascal-shift-right)
(define-key 'pascal #\rubout 'backward-delete-char-untabify)
(define-key 'pascal #\tab 'pascal-indent-line)

(define-command pascal-indent-line
  "Indents the current line for Pascal code."
  ()
  (lambda ()
    (let ((point (current-point)))
      (let ((indentation (calculate-pascal-indentation point)))
	(cond ((not (= indentation (current-indentation point)))
	       (change-indentation indentation point))
	      ((line-start? (horizontal-space-start point))
	       (set-current-point! (horizontal-space-end point))))))))

(define-command pascal-shift-right
  "Shift the current line right by Pascal Shift Increment.
With an argument, shifts right that many times."
  "p"
  (lambda (argument)
    (if (not (zero? argument))
	(let ((mark (line-start (current-point) 0)))
	  (change-indentation (+ (current-indentation mark)
				 (* argument
				    (ref-variable pascal-shift-increment)))
			      mark)))))

(define-command pascal-shift-left
  "Shift the current line left by Pascal Shift Increment.
With an argument, shifts left that many times."
  "p"
  (lambda (argument)
    (if (not (zero? argument))
	(let ((mark (line-start (current-point) 0)))
	  (change-indentation (- (current-indentation mark)
				 (* argument
				    (ref-variable pascal-shift-increment)))
			      mark)))))

(define (calculate-pascal-indentation mark)
  (let ((def-start
	  (let ((nb (find-previous-non-blank-line mark)))
	    (if (not nb)
		(group-start mark)
		(let ((start (backward-one-paragraph nb)))
		  (if (not start)
		      (group-start mark)
		      (line-start start 1)))))))
    (define (find-statement-start mark)
      (let ((start (find-previous-non-blank-line mark)))
	(cond ((not start) #f)
	      ((mark< start def-start) def-start)
	      (else
	       (let ((container
		      (parse-state-containing-sexp
		       (parse-partial-sexp def-start start))))
		 (if container
		     (find-statement-start start)
		     start))))))
    (let ((state (parse-partial-sexp def-start (line-start mark 0))))
      (let ((container (parse-state-containing-sexp state))
	    (last-sexp (parse-state-last-sexp state)))
	(if container
	    ;; Inside some parenthesized expression or arglist.
	    (if (mark> (line-end container 0) last-sexp)
		;; Indent first line under opening paren.
		(mark-column (horizontal-space-end (mark1+ container)))
		;; Indent subsequent line under previous line.
		(indentation-of-previous-non-blank-line mark))
	    (let ((start (find-statement-start mark)))
	      (if (not start)
		  0
		  (let ((start (horizontal-space-end start)))
		    (let ((indentation (mark-column start)))
		      (if (and (ref-variable pascal-indentation-keywords)
			       (re-match-forward
				(ref-variable pascal-indentation-keywords)
				start))
			  (+ indentation
			     (ref-variable pascal-shift-increment))
			  indentation))))))))))