#| -*-Scheme-*-

$Id: debian-changelog.scm,v 1.11 2007/01/05 21:19:23 cph Exp $

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

;;;; Debian changelog mode


(define-command debian-changelog-mode
  "Enter Debian changelog mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object debian-changelog))))

(define-major-mode debian-changelog text "Debian changelog"
  "Major mode for editing Debian-style change logs.
Runs `debian-changelog-mode-hook' if it exists.

Key bindings:

\\{debian-changelog}"
  (lambda (buffer)
    (local-set-variable! fill-prefix "    " buffer)
    (local-set-variable! fill-column 74 buffer)

    ;; Let each entry behave as one paragraph:
    (local-set-variable! paragraph-start "\\*" buffer)
    (local-set-variable! paragraph-separate "\\*\\|\\s-*$|\\S-" buffer)

    ;; Let each version behave as one page.
    ;; Match null string on the heading line so that the heading line
    ;; is grouped with what follows.
    (local-set-variable! page-delimiter "^\\<" buffer)
    (local-set-variable! version-control 'NEVER buffer)

    (event-distributor/invoke! (ref-variable debian-changelog-mode-hook buffer)
			       buffer)))

(define-key 'debian-changelog '(#\C-c #\C-a) 'debian-changelog-add-entry)
(define-key 'debian-changelog '(#\C-c #\C-f)
  'debian-changelog-finalize-last-version)
(define-key 'debian-changelog '(#\C-c #\C-c)
  'debian-changelog-finalize-and-save)
(define-key 'debian-changelog '(#\C-c #\C-v) 'debian-changelog-add-version)
(define-key 'debian-changelog '(#\C-c #\C-d) 'debian-changelog-distribution)
(define-key 'debian-changelog '(#\C-c #\C-u) 'debian-changelog-urgency)
(define-key 'debian-changelog '(#\C-c #\C-e)
  'debian-changelog-unfinalize-last-version)

(define-command debian-changelog-add-version
  "Add a new version section to a debian-style changelog file."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (if (not (version-finalized? buffer))
	  (error "Previous version not yet finalized."))
      (let* ((finish
	      (lambda (package version)
		(let ((m (mark-left-inserting-copy (buffer-start buffer))))
		  (insert-string package m)
		  (insert-string " (" m)
		  (insert-string version m)
		  (insert-string ") unstable; urgency=low" m)
		  (insert-newlines 2 m)
		  (insert-string "  * " m)
		  (let ((p (mark-right-inserting-copy m)))
		    (insert-newlines 2 m)
		    (insert-string " --" m)
		    (insert-newlines 2 m)
		    (mark-temporary! m)
		    (mark-temporary! p)
		    (set-buffer-point! buffer p)))))
	     (prompt
	      (lambda ()
		(let ((package (prompt-for-string "Package name" #f)))
		  (finish package
			  (prompt-for-string
			   "New version (including any revision)"
			   #f))))))
	(if (match-title-line buffer #f)
	    (let ((package
		   (re-match-extract-string title-regexp-index:package-name))
		  (version
		   (re-match-extract-string title-regexp-index:version)))
	      (let ((regs
		     (re-string-search-forward "\\([0-9]+\\)$" version)))
		(if regs
		    (finish
		     package
		     (string-append
		      (string-head version (re-match-start-index 1 regs))
		      (number->string
		       (+ (string->number (re-match-extract version regs 1))
			  1))))
		    (prompt))))
	    (prompt))))))

(define-command debian-changelog-add-entry
  "Add a new change entry to a debian-style changelog."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (if (version-finalized? buffer)
	  (error
	   (substitute-command-keys
	    (string-append
	     "Most recent version has been finalized - use "
	     "\\[debian-changelog-unfinalize-last-version] or "
	     "\\[debian-changelog-add-version]")
	    buffer)))
      (let ((m (mark-left-inserting-copy (mark- (trailer-line buffer) 4))))
	(guarantee-newline m)
	(insert-string "  * " m)
	(insert-newline m)
	(mark-temporary! m)
	(set-current-point! (mark-1+ m))))))

(define-command debian-changelog-distribution
  "Delete the current distribution and prompt for a new one."
  ()
  (lambda ()
    (set-title-distribution (selected-buffer)
			    (prompt-for-alist-value
			     "Select distribution"
			     (map (lambda (s) (cons s s))
				  '("stable"
				    "frozen"
				    "unstable" 
				    "stable frozen unstable"
				    "stable unstable frozen"
				    "unstable stable frozen"
				    "unstable frozen stable"
				    "frozen unstable stable"
				    "frozen stable unstable"
				    "frozen unstable"
				    "unstable frozen"
				    "stable frozen"
				    "frozen stable"
				    "stable unstable"
				    "unstable stable"))))))

(define-command debian-changelog-urgency
  "Delete the current urgency and prompt for a new one."
  ()
  (lambda ()
    (set-title-urgency (selected-buffer)
		       (prompt-for-alist-value
			"Select urgency"
			(map (lambda (s) (cons s s))
			     '("low" "medium" "high" ))))))

(define-command debian-changelog-finalize-and-save
  "Finalize, if necessary, and then save a debian-style changelog file."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (if (not (version-finalized? buffer))
	  (finalize-last-version buffer))
      (save-buffer buffer #f))))

(define-command debian-changelog-finalize-last-version
  "Add the `finalization' information (maintainer's name and email
address and release date)."
  ()
  (lambda () (finalize-last-version (selected-buffer))))

(define (finalize-last-version buffer)
  (let ((m (mark-left-inserting-copy (trailer-line buffer))))
    (delete-string m (line-end m 0))
    (insert-string " " m)
    (insert-string (or (ref-variable add-log-full-name buffer)
		       (mail-full-name buffer))
		   m)
    (insert-string " <" m)
    (insert-string (or (ref-variable add-log-mailing-address buffer)
		       (user-mail-address buffer))
		   m)
    (insert-string ">  " m)
    (insert-string (universal-time->string (get-universal-time)) m)
    (mark-temporary! m)))

(define-command debian-changelog-unfinalize-last-version
  "Remove the `finalization' information (maintainer's name and email
address and release date) so that new entries can be made."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (if (not (version-finalized? buffer))
	  (error "Most recent version is not finalized."))
      (let ((m (trailer-line buffer)))
	(delete-string m (line-end m 0))))))

(define (version-finalized? buffer)
  (let ((m (trailer-line buffer)))
    (cond ((re-match-forward
	    "[ \t]+\\S [^\n\t]+\\S  <[^ \t\n<>]+>  \\S [^\t\n]+\\S [ \t]*$"
	    m)
	   #t)
	  ((re-match-forward "[ \t]*$" m) #f)
	  (else (error "Malformed finalization line.")))))

(define (trailer-line buffer)
  (let ((start (buffer-start buffer))
	(end (buffer-end buffer)))
    (let ((m
	   (if (re-search-forward "\n\\S " start end)
	       (mark1+ (re-match-start 0))
	       end)))
      (if (re-search-backward "\n --"  m)
	  (re-match-end 0)
	  (let ((m (mark-left-inserting-copy m)))
	    (insert-string " --" m)
	    (insert-newlines 2 m)
	    (mark-temporary! m)
	    (mark- m 2))))))

(define (set-title-distribution buffer distribution)
  (set-title-value buffer title-regexp-index:distribution distribution))

(define (set-title-urgency buffer urgency)
  (set-title-value buffer title-regexp-index:urgency urgency))

(define (set-title-value buffer index value)
  (match-title-line buffer #t)
  (let ((s (mark-left-inserting-copy (re-match-start index)))
	(e (re-match-end index)))
    (delete-string s e)
    (insert-string value s)
    (mark-temporary! s)))

(define (match-title-line buffer error?)
  (or (let ((start (buffer-start buffer)))
	(re-match-forward title-regexp start (line-end start 0)))
      (and error? (error "Unable to match title line."))))

(define title-regexp
  (let ((package-name "[a-zA-Z0-9+.-]+")
	(version "[a-zA-Z0-9+.:-]+"))
    (string-append "^\\("
		   package-name
		   "\\) +(\\("
		   version
		   "\\)) \\( *"
		   package-name
		   "\\( +"
		   package-name
		   "\\)* *\\);.*"
		   " urgency=\\([a-zA-Z0-9]+\\)")))
(define title-regexp-index:package-name 1)
(define title-regexp-index:version 2)
(define title-regexp-index:distribution 3)
(define title-regexp-index:urgency 5)