#| -*-Scheme-*-

$Id: sercom.scm,v 1.70 2007/01/05 21:19:24 cph Exp $

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

;;;; Search Commands
;;; package: (edwin)

(declare (usual-integrations))

;;;; Variables

(define-variable-per-buffer case-fold-search
  "True if searches should ignore case.
Automatically becomes local when set in any fashion.
If given a numeric argument, most of the search commands will toggle
this variable temporarily."
  #t
  boolean?)

(define-variable search-last-string
  "Last string search for by a non-regexp search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted."
  ""
  string?)

(define-variable search-last-regexp
  "Last string searched for by a regexp search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted."
  ""
  string?)

(define-variable search-repeat-char
  "Character to repeat incremental search forwards."
  #\C-s
  char?)

(define-variable search-reverse-char
  "Character to repeat incremental search backwards."
  #\C-r
  char?)

(define-variable search-exit-char
  "Character to exit incremental search."
  #\return
  char?)

(define-variable search-delete-char
  "Character to delete from incremental search string."
  #\rubout
  char?)

(define-variable search-quote-char
  "Character to quote special characters for incremental search."
  #\C-q
  char?)

(define-variable search-yank-word-char
  "Character to pull next word from buffer into search string."
  #\C-w
  char?)

(define-variable search-yank-line-char
  "Character to pull rest of line from buffer into search string."
  #\C-y
  char?)

(define-variable search-exit-option
  "True means random control characters terminate incremental search."
  #t
  boolean?)

(define-variable search-slow-speed
  "Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached."
  1200
  exact-nonnegative-integer?)

(define-variable search-slow-window-lines
  "Number of lines in slow search display windows.
These are the short windows used during incremental search on slow terminals.
Negative means put the slow search window at the top (normally it's at bottom)
and the value is minus the number of lines."
  1
  exact-integer?)

;;;; String Search

;;; these should print the numeric-argument when there is one
(define (search-prompt prompt)
  (lambda ()
    (let ((string
	   (prompt-for-string prompt (ref-variable search-last-string))))
      (set-variable! search-last-string string)
      (list (command-argument) string))))

(define (re-search-prompt prompt)
  (lambda ()
    (let ((regexp
	   (prompt-for-string prompt (ref-variable search-last-regexp))))
      (set-variable! search-last-regexp regexp)
      (list (command-argument) regexp))))

(define (search-failure string)
  (editor-error "Search failed: " (write-to-string string)))

(define (opposite-case-fold toggle-case-fold? thunk)
  (if toggle-case-fold?
      (with-variable-value! (ref-variable-object case-fold-search)
			    (not (ref-variable case-fold-search))
			    thunk)
      (thunk)))

(define-command search-forward
  "Search forward from point for STRING.
Set point to the end of the occurrence found."
  (search-prompt "Search")
  (lambda (toggle-case-fold? string)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(let ((point (current-point)))
	  (let ((mark (search-forward string point (group-end point))))
	    (if (not mark) (search-failure string))
	    (set-current-point! mark)))))))

(define-command search-backward
  "Search backward from point for STRING.
Set point to the beginning of the occurrence found."
  (search-prompt "Search backward")
  (lambda (toggle-case-fold? string)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(let ((point (current-point)))
	  (let ((mark (search-backward string point (group-start point))))
	    (if (not mark) (search-failure string))
	    (set-current-point! mark)))))))

(define-command re-search-forward
  "Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found."
  (re-search-prompt "RE search")
  (lambda (toggle-case-fold? regexp)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(let ((point (current-point)))
	  (let ((mark (re-search-forward regexp point (group-end point))))
	    (if (not mark) (search-failure regexp))
	    (set-current-point! mark)))))))

(define-command re-search-backward
  "Search backward from point for regular expression REGEXP.
Set point to the beginning of the occurrence found.
The match found is the one starting last in the buffer
and yet ending before the place of the origin of the search."
  (re-search-prompt "RE search backward")
  (lambda (toggle-case-fold? regexp)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(let ((point (current-point)))
	  (let ((mark (re-search-backward regexp point (group-start point))))
	    (if (not mark) (search-failure regexp))
	    (set-current-point! mark)))))))

;;;; Word Search

(define-command word-search-forward
  "Search forward from point for STRING, ignoring differences in punctuation.
Set point to the end of the occurrence found."
  (search-prompt "Word search")
  (lambda (toggle-case-fold? string)
    ((ref-command re-search-forward)
     toggle-case-fold?
     (string->wordified-regexp string (ref-variable syntax-table)))))
  
(define-command word-search-backward
  "Search backward from point for STRING, ignoring differences in punctuation.
Set point to the beginning of the occurrence found."
  (search-prompt "Word search backward")
  (lambda (toggle-case-fold? string)
     ((ref-command re-search-backward)
      toggle-case-fold?
      (string->wordified-regexp string (ref-variable syntax-table)))))

(define (string->wordified-regexp string syntax-table)
  (apply
   string-append
   (let ((end (string-length string)))
     (let ((index
	    (substring-find-next-char-of-syntax string 0 end
						syntax-table #\w)))
       (if index
	   (cons "\\b"
		 (let loop ((start index))
		   (let ((index
			  (substring-find-next-char-not-of-syntax
			   string start end
			   syntax-table #\w)))
		     (if index
			 (cons (substring string start index)
			       (let ((index
				      (substring-find-next-char-of-syntax
				       string (+ index 1) end
				       syntax-table #\w)))
				 (if index
				     (cons "\\W+" (loop index))
				     '("\\b"))))
			 (cons (substring string start end) '("\\b"))))))
	   '())))))

;;;; Incremental Search

(define-command isearch-forward
  "Do incremental search forward.
As you type characters, they add to the search string and are found.
A numeric argument allows you to toggle case-fold-search but this
 information is lost whenever you exit search, even if you do a C-s C-s.
Type Delete to cancel characters from end of search string.
Type RET to exit, leaving point at location found.
Type C-s to search again forward, C-r to search again backward.
Type C-w to yank word from buffer onto end of search string and search for it.
Type C-y to yank rest of line onto end of search string, etc.
Type C-q to quote control character to search for it.
Other control and meta characters terminate the search
 and are then executed normally.
The above special characters are mostly controlled by parameters;
 do M-x variable-apropos on search-.*-char to find them.
C-g while searching or when search has failed
 cancels input back to what has been found successfully.
C-g when search is successful aborts and moves point to starting point."
  "P"
  (lambda (toggle-case-fold?)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(isearch #t #f)))))

(define-command isearch-forward-regexp
  "Do incremental search forward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  "P"
  (lambda (toggle-case-fold?)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(isearch #t #t)))))

(define-command isearch-backward
  "Do incremental search backward.
See \\[isearch-forward] for more information."
  "P"
  (lambda (toggle-case-fold?)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(isearch #f #f)))))

(define-command isearch-backward-regexp
  "Do incremental search backward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  "P"
  (lambda (toggle-case-fold?)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(isearch #f #t)))))

;;;; Character Search
;;;  (Courtesy of Jonathan Rees)

(define-command char-search-forward
  "Search for a single character.
Special characters:
  C-a  calls \\[search-forward].
  C-r  searches backwards for the current default.
  C-s  searches forward for the current default.
  C-q  quotes the character to be searched for;
       this allows search for special characters."
  "P"
  (lambda (toggle-case-fold?)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(character-search #t)))))

(define-command char-search-backward
  "Like \\[char-search-forward], but searches backwards."
  "P"
  (lambda (toggle-case-fold?)
    (opposite-case-fold toggle-case-fold?
      (lambda ()
	(character-search #f)))))

(define (character-search forward?)
  (let ((char (prompt-for-char "Character search")))
    (let ((test-for
	   (lambda (char*)
	     (char=? char (remap-alias-key char*)))))
      (if (test-for #\C-a)
	  (dispatch-on-command
	   (if forward?
	       (ref-command-object search-forward)
	       (ref-command-object search-backward)))
	  (let ((mark
		 (let ((m (current-point)))
		   (cond ((test-for #\C-s)
			  (search-forward (ref-variable search-last-string)
					  m
					  (group-end m)))
			 ((test-for #\C-r)
			  (search-backward (ref-variable search-last-string)
					   m
					   (group-start m)))
			 (else
			  (let ((char
				 (if (test-for #\C-q)
				     (prompt-for-char "Quote character")
				     char)))
			    (if forward?
				(char-search-forward char m (group-end m))
				(char-search-backward char m
						      (group-start m)))))))))
	    (if mark
		(set-current-point! mark)
		(editor-failure)))))))