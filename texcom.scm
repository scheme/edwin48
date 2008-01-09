;;; -*-Scheme-*-
;;;
;;; $Id: texcom.scm,v 1.51 2007/01/05 21:19:24 cph Exp $
;;;
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;;     2006, 2007 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;;; USA.
;;;
;;;

;;;; Text Commands


(define-major-mode text fundamental "Text"
  "Major mode for editing english text."
  (lambda (buffer)
    (local-set-variable! syntax-table text-mode:syntax-table buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable text-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable text-mode-hook buffer) buffer)))

(define-key 'text #\m-s 'center-line)

(define text-mode:syntax-table (make-char-syntax-table))
(set-char-syntax! text-mode:syntax-table #\" "    ")
(set-char-syntax! text-mode:syntax-table #\\ "    ")
(set-char-syntax! text-mode:syntax-table #\[ "(]  ")
(set-char-syntax! text-mode:syntax-table #\] ")[  ")
(set-char-syntax! text-mode:syntax-table #\{ "(}  ")
(set-char-syntax! text-mode:syntax-table #\} "){  ")
(set-char-syntax! text-mode:syntax-table #\' "w   ")

(define-variable text-mode-abbrev-table
  "Mode-specific abbrev table for Text mode.")
(define-abbrev-table 'text-mode-abbrev-table '())

(define-variable text-mode-hook
  "An event distributor that is invoked when entering Text mode."
  (make-event-distributor))

(define (turn-on-auto-fill buffer)
  (enable-buffer-minor-mode! buffer (ref-mode-object auto-fill)))

(define-command text-mode
  "Make the current mode be Text mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object text))))

(define-major-mode indented-text text "Indented-Text"
  "Like Text mode, but indents each line under previous non-blank line."
  (lambda (buffer)
    (define-variable-local-value! buffer
	(ref-variable-object indent-line-procedure)
      (ref-command indent-relative))))

(define-command indented-text-mode
  "Make the current mode be Indented Text mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object indented-text))))

;;;; Words

(define-command forward-word
  "Move one or more words forward."
  "p"
  (lambda (argument)
    (move-thing forward-word argument 'FAILURE)))

(define-command backward-word
  "Move one or more words backward."
  "p"
  (lambda (argument)
    (move-thing backward-word argument 'FAILURE)))

(define-command mark-word
  "Set mark one or more words from point."
  "p"
  (lambda (argument)
    (mark-thing forward-word argument 'FAILURE)))

(define-command kill-word
  "Kill one or more words forward."
  "p"
  (lambda (argument)
    (kill-thing forward-word argument 'FAILURE)))

(define-command backward-kill-word
  "Kill one or more words backward."
  "p"
  (lambda (argument)
    (kill-thing backward-word argument 'FAILURE)))

(define-command transpose-words
  "Transpose the words before and after the cursor.
With a positive argument it transposes the words before and after the
 cursor, moves right, and repeats the specified number of times,
 dragging the word to the left of the cursor right.
With a negative argument, it transposes the two words to the left of
 the cursor, moves between them, and repeats the specified number of
 times, exactly undoing the positive argument form.
With a zero argument, it transposes the words at point and mark."
  "p"
  (lambda (argument)
    (transpose-things forward-word argument)))

(define (count-words-region region)
  (let ((end (region-end region)))
    (let loop ((start (region-start region)) (count 0))
      (let ((start (forward-to-word start 'LIMIT)))
	(if (mark< start end)
	    (let ((count (+ count 1))
		  (m (forward-word start 1 #f)))
	      (if m
		  (loop m count)
		  count))
	    count)))))

;;;; Case Conversion

(define-command upcase-region
  "Convert the region to upper case."
  "r"
  (lambda (region) (upcase-region region)))

(define-command downcase-region
  "Convert the region to lower case."
  "r"
  (lambda (region) (downcase-region region)))

(define-command capitalize-region
  "Convert the region to capitalized form.
Capitalized form means each word's first character is upper case
and the rest of it is lower case."
  "r"
  (lambda (region) (capitalize-region region)))

(define-command upcase-word
  "Convert following word (or ARG words) to upper case, moving over.
With negative argument, convert previous words but do not move.
See also `capitalize-word'."
  "p"
  (lambda (argument) (case-word-command upcase-region argument)))

(define-command downcase-word
  "Convert following word (or ARG words) to lower case, moving over.
With negative argument, convert previous words but do not move."
  "p"
  (lambda (argument) (case-word-command downcase-region argument)))

(define-command capitalize-word
  "Capitalize the following word (or ARG words), moving over.
This gives the word(s) a first character in upper case
and the rest lower case.
With negative argument, capitalize previous words but do not move."
  "p"
  (lambda (argument) (case-word-command capitalize-region argument)))

(define (case-word-command procedure argument)
  (let* ((point (current-point))
	 (end (forward-word point argument 'ERROR)))
    (procedure (make-region point end))
    (if (positive? argument) (set-current-point! end))))

(define (downcase-region region)
  (region-transform! region
    (lambda (string)
      (string-downcase! string)
      string)))

(define (upcase-region region)
  (region-transform! region
    (lambda (string)
      (string-upcase! string)
      string)))

(define (capitalize-region region)
  (let ((end (region-end region)))
    (let loop ((start (region-start region)))
      (let ((start (forward-to-word start 'LIMIT)))
	(if (mark< start end)
	    (let ((m (forward-word start 1 #f)))
	      (if m
		  (begin
		    (region-transform! (make-region start m)
		      (lambda (string)
			(string-capitalize! string)
			string))
		    (loop m))
		  (region-transform! (make-region start end)
		    (lambda (string)
		      (string-capitalize! string)
		      string)))))))))

;;;; Sentences

(define-command forward-sentence
  "Move forward to next sentence-end.  With argument, repeat.
With negative argument, move backward repeatedly to sentence-beginning.
Sentence ends are identified by the value of Sentence End
treated as a regular expression.  Also, every paragraph boundary
terminates sentences as well."
  "p"
  (lambda (argument)
    (move-thing forward-sentence argument 'ERROR)))

(define-command backward-sentence
  "Move backward to start of sentence.  With arg, do it arg times.
See \\[forward-sentence] for more information."
  "p"
  (lambda (argument)
    (move-thing backward-sentence argument 'ERROR)))

(define-command kill-sentence
  "Kill from point to end of sentence.
With arg, repeat, or backward if negative arg."
  "p"
  (lambda (argument)
    (kill-thing forward-sentence argument 'ERROR)))

(define-command backward-kill-sentence
  "Kill back from point to start of sentence.
With arg, repeat, or forward if negative arg."
  "p"
  (lambda (argument)
    (kill-thing backward-sentence argument 'ERROR)))

;;;; Paragraphs

(define-command forward-paragraph
  "Move forward to end of paragraph.  With arg, do it arg times.
A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  "p"
  (lambda (argument)
    (move-thing forward-paragraph argument 'ERROR)))

(define-command backward-paragraph
  "Move backward to start of paragraph.  With arg, do it arg times.
A paragraph start is the beginning of a line which is a first-line-of-paragraph
or which is ordinary text and follows a paragraph-separating line; except:
if the first real line of a paragraph is preceded by a blank line,
the paragraph starts at that blank line.
See forward-paragraph for more information."
  "p"
  (lambda (argument)
    (move-thing backward-paragraph argument 'ERROR)))

(define-command mark-paragraph
  "Put point at beginning of this paragraph, mark at end."
  ()
  (lambda ()
    (let ((end (forward-paragraph (current-point) 1 'ERROR)))
      (set-current-region!
       (make-region (backward-paragraph end 1 'ERROR) end)))))
