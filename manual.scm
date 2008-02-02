#| -*-Scheme-*-

$Id: manual.scm,v 1.22 2008/01/30 20:02:03 cph Exp $

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

;;;; Display Manual Pages


(define-command manual-entry
  "Display the Unix manual entry for TOPIC.
TOPIC is either the title of the entry, or has the form TITLE(SECTION)
where SECTION is the desired section of the manual, as in `tty(4)'."
  "sManual entry (topic): "
  (lambda (topic #!optional section)
    (let ((r
	   (and (default-object? section)
		(re-string-match
		 "\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'"
		 topic))))
      (if r
	  (begin
	    (set! section
		  (substring topic
			     (re-match-start-index 2 r)
			     (re-match-end-index 2 r)))
	    (set! topic
		  (substring topic
			     (re-match-start-index 1 r)
			     (re-match-end-index 1 r))))
	  (set! section #f)))
    (let ((buffer-name
	   (if (ref-variable manual-entry-reuse-buffer?)
	       "*Manual-Entry*"
	       (string-append
		"*Man "
		topic
		(if section (string-append "(" section ")") "")
		"*"))))
      (let ((buffer (temporary-buffer buffer-name)))
	(disable-group-undo! (buffer-group buffer))
	(message "Invoking man "
		 (if section (string-append section " ") "")
		 topic
		 "...")
	(shell-command #f (buffer-point buffer) #f #f
		       (string-append (or (ref-variable manual-command)
					  (if (file-exists? "/usr/bin/man")
					      "/usr/bin/man"
					      "/usr/ucb/man"))
				      (if section
					  (string-append " " section)
					  "")
				      " "
				      topic))
	(message "Cleaning manual entry for " topic "...")
	(nuke-nroff-bs buffer)
	(buffer-not-modified! buffer)
	(set-buffer-read-only! buffer)
	(set-buffer-point! buffer (buffer-start buffer))
	(pop-up-buffer buffer #f)
	(message "Manual page ready")))))

(define-command clean-manual-entry
  "Clean the unix manual entry in the current buffer.
The current buffer should contain a formatted manual entry."
  ()
  (lambda () (nuke-nroff-bs (current-buffer))))

(define (nuke-nroff-bs buffer)
  (nuke-underlining buffer)
  (nuke-overstriking buffer)
  ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
  (nuke-regexp buffer
	       "^ *\\([A-Za-z][-_A-Za-z0-9]*([-0-9A-Z]+)\\).*\\1$"
	       #f)
  ;; Nuke vendor-specific footers
  (nuke-regexp buffer manual-vendor-pattern #t)
  ;; Nuke generic footers
  (nuke-regexp buffer "^[A-Za-z0-9_]*[ \t]*[0-9]+$" #f)
  (crunch-blank-lines buffer)
  ;; Nuke blanks lines at start.
  (if (re-match-forward "\\([ \t]*\n\\)+"
			(buffer-start buffer)
			(buffer-end buffer)
			#f)
      (delete-match))
  ;; Nuke "Reformatting page" message, plus trailing blank lines.
  (if (re-match-forward "Reformatting \\(page\\|entry\\).*\n\\([ \t]*\n\\)*"
			(buffer-start buffer)
			(buffer-end buffer)
			#f)
      (delete-match))
  ;; Nuke blanks lines at end.
  (let ((end (buffer-end buffer)))
    (if (line-blank? (line-start end 0))
	(delete-string (let loop ((mark (line-start end 0)))
			 (let ((m (line-start mark -1 #f)))
			   (cond ((not m) mark)
				 ((not (line-blank? m)) mark)
				 (else (loop m)))))
		       end))))

(define manual-vendor-pattern
  (string-append
   "^\\("
   "\\(Printed\\|Sun Release\\) [0-9].*[0-9]"
   "\\|"
   " *Page [0-9]*.*(printed [0-9/]*)"
   "\\|"
   "[ \t]*Hewlett-Packard\\( Company\\|\\)[ \t]*- [0-9]* -.*"
   "\\)$"))

(define (nuke-underlining buffer)
  (let ((group (buffer-group buffer)))
    (let loop
	((index
	  (let ((start (group-start-index group)))
	    (if (and (fix:< start (group-end-index group))
		     (char=? #\backspace (group-right-char group start)))
		(fix:+ start 1)
		start))))
      (let ((bs
	     (group-find-next-char group
				   index
				   (group-end-index group)
				   #\backspace)))
	(if bs
	    (if (char=? #\_ (group-left-char group bs))
		(begin
		  (group-delete! group (fix:- bs 1) (fix:+ bs 1))
		  (loop (fix:- bs 1)))
		(loop (fix:+ bs 1))))))))

(define (nuke-overstriking buffer)
  (let ((group (buffer-group buffer)))
    (let loop ((start (group-start-index group)))
      (let ((end (group-end-index group)))
	(let ((bs (group-find-next-char group start end #\backspace)))
	  (if bs
	      (if (fix:< (fix:+ bs 2) end)
		  (let find-end ((index (fix:+ bs 2)))
		    (if (and (fix:< (fix:+ index 2) end)
			     (char=? #\backspace
				     (group-right-char group index)))
			(find-end (fix:+ index 2))
			(begin
			  (group-delete! group bs index)
			  (loop bs)))))))))))

(define (nuke-regexp buffer regexp case-fold-search)
  (let ((group (buffer-group buffer))
	(pattern (re-compile-pattern regexp case-fold-search)))
    (let ((syntax-table (group-syntax-table group)))
      (let loop ((index (group-start-index group)))
	(if (re-search-buffer-forward pattern
				      syntax-table
				      group
				      index
				      (group-end-index group))
	    (let ((start (re-match-start-index 0)))
	      (group-delete! group start (re-match-end-index 0))
	      (loop start)))))))

(define (crunch-blank-lines buffer)
  (let ((group (buffer-group buffer)))
    (let loop ((start (group-start-index group)))
      (let ((end (group-end-index group)))
	(let ((nl (group-find-next-char group start end #\newline)))
	  (if nl
	      (let ((nl+2 (fix:+ nl 2)))
		(if (fix:< nl+2 end)
		    (begin
		      (if (and (char=? #\newline
				       (group-right-char group (fix:+ nl 1)))
			       (char=? #\newline
				       (group-right-char group nl+2)))
			  (let find-end ((index (fix:+ nl 3)))
			    (if (and (fix:< index end)
				     (char=? #\newline
					     (group-right-char group index)))
				(find-end (fix:+ index 1))
				(group-delete! group nl+2 index))))
		      (loop nl+2))))))))))