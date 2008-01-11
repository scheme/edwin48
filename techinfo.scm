;;; -*- Scheme -*-

;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;     2005, 2006, 2007 Massachusetts Institute of Technology
;;
;; This file is part of MIT/GNU Scheme.
;;
;; MIT/GNU Scheme is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; MIT/GNU Scheme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with MIT/GNU Scheme; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;;
;;; techinfo.scm - Edwin mode for accessing TechInfo database
;;; requires ability to "attach" Athena filesystems
;;;
;;; Author: Brian A. LaMacchia -- bal@zurich.ai.mit.edu
;;;
;;; $Id: techinfo.scm,v 1.10 2007/01/05 21:19:24 cph Exp $
;;;


(define-variable techinfo-web-buffer
  "Buffer which contains a copy of the TechInfo web file."
  #f)

(define-variable techinfo-buffer
  "Buffer used for displaying TechInfo information."
  #f)

(define-variable techinfo-current-node-list
  "Cached list of web file information for the current node."
  #f)

(define-variable techinfo-attached-filesystems
  "List of filesystems already attached by this run of TechInfo.
If a filesystem name appears in this list subsequent calls to
attach will not be made."
  '())

(define-variable techinfo-parent-list
  "List of parent nodes from this node to the root node."
  #f)

(define-variable techinfo-history-list
  "List of TechInfo nodes previously visited."
  '("0"))

(define-variable techinfo-need-to-attach-afs
  "True if AFS is not available by default, and the NFS->AFS
translator on Atalanta is needed.  At Tech Square this variable
is set to t.  At Athena it is nil."
  #t)

;;; Returns the line of the TechInfo web file which corresponds to node NUMBER.
(define (techinfo-find-line string)
  (let ((web-buffer (ref-variable techinfo-web-buffer)))
    (let ((the-mark
	   (re-search-forward
	    (string-append "^" string ":")
	    (buffer-start web-buffer)
	    (buffer-end web-buffer))))
      (extract-string (line-start the-mark 0) (line-end the-mark 0)))))

;;; Takes one arg, STRING, which is a line from the TechInfo web file.
;;; Parses the line and returns a list of the information contained.
;;; Usually the result of (techinfo-find-line n) is passed directly to
;;; this function.
(define (techinfo-parse-line string)
  (define (techinfo-internal-parse string char)
    (let loop ((start 0)
	       (end (string-length string))
	       (the-list '()))
      (let ((next-index
	     (substring-find-next-char string start end char)))
	(if next-index
	    (loop (1+ next-index)
		  end
		  (cons (substring string start next-index) the-list))
	    (cons (string-tail string start) the-list)))))
  (let ((the-list-1 (techinfo-internal-parse string #\:)))
    (reverse
     (cons
      (reverse (techinfo-internal-parse (car the-list-1) #\,))
      (cons
       (reverse (techinfo-internal-parse (cadr the-list-1) #\,))
       (cddr the-list-1))))))

(define (techinfo-line/number techinfo-line)
  (list-ref techinfo-line 0))

(define (techinfo-line/unknown2 techinfo-line)
  (list-ref techinfo-line 1))

(define (techinfo-line/unknown3 techinfo-line)
  (list-ref techinfo-line 2))

(define (techinfo-line/unknown4 techinfo-line)
  (list-ref techinfo-line 3))

(define (techinfo-line/title techinfo-line)
  (list-ref techinfo-line 4))

(define (techinfo-line/source techinfo-line)
  (list-ref techinfo-line 5))

(define (techinfo-line/filesystem techinfo-line)
  (list-ref techinfo-line 6))

(define (techinfo-line/filename techinfo-line)
  (list-ref techinfo-line 7))

(define (techinfo-line/parent-list techinfo-line)
  (list-ref techinfo-line 8))

(define (techinfo-line/children-list techinfo-line)
  (list-ref techinfo-line 9))

(define (techinfo-display-node node-number)
  (let ((node-list (techinfo-parse-line (techinfo-find-line node-number))))
    (set-variable! techinfo-history-list
		   (cons (techinfo-line/number node-list)
			 (ref-variable techinfo-history-list)))
    (if (string=? (techinfo-line/filesystem node-list) "")
	(let ((foo (member (cadr (ref-variable techinfo-history-list))
			   (techinfo-line/children-list node-list))))
	  (if foo
	      (techinfo-display-internal-node
	       node-list
	       (1+ (- (length (techinfo-line/children-list node-list))
		      (length foo))))
	      (techinfo-display-internal-node node-list)))
	(techinfo-display-leaf-node node-list))))

(define (techinfo-insert-node-header node-list)
  (let ((siblings
	 (techinfo-line/children-list
	  (techinfo-parse-line (techinfo-find-line
				(car (ref-variable techinfo-parent-list)))))))
      (let ((rest
	     (member (techinfo-line/number
		      (ref-variable techinfo-current-node-list)) siblings)))
	(if rest
	    (let* ((num (- (length siblings) (length rest)))
		   (prev (-1+ num))
		   (next (1+ num))
		   (left-string
		    (if (>= prev 0)
			(string-append
			 "Prev: "
			 (techinfo-line/title
			  (techinfo-parse-line
			   (techinfo-find-line (list-ref siblings prev)))))
			"Prev: (none)"))
		   (right-string
		    (if (< next (length siblings))
			(string-append
			 "Next: "
			 (techinfo-line/title
			  (techinfo-parse-line
			   (techinfo-find-line (list-ref siblings next)))))
			"Next: (none)")))
	      (let ((padding
		     (- (ref-variable fill-column)
			(string-length right-string))))
		(insert-string
		 (if (> padding (+ 10 (string-length left-string)))
		     (string-pad-right left-string padding)
		     (string-append left-string (make-string 10 #\space)))))
	      (insert-string right-string)
	      (insert-string "\n")))))
  (insert-string "\n")
  (insert-string (techinfo-line/title node-list))
  (center-line (current-point))
  (insert-string "\n\n"))

(define (techinfo-display-internal-node node-list #!optional placement)
  (set-variable! techinfo-current-node-list node-list)
  (set-buffer-writeable! (current-buffer))
  (set-current-point! (buffer-start (current-buffer)))
  (kill-string (buffer-start (current-buffer))
	       (buffer-end (current-buffer)))
  (techinfo-insert-node-header node-list)
  (let ((point (buffer-start (current-buffer)))
	(placement-index
	 (if (default-object? placement) 1 placement)))
    (let loop ((the-children (cdr (techinfo-line/children-list node-list)))
	       (the-index 1)
	       (this-child (car (techinfo-line/children-list node-list))))
      (if (not (string=? this-child ""))
	  (begin
	    (insert-string
	     (string-append
	      "  " (string-pad-left (number->string the-index) 5) " "
	      (techinfo-line/title
	       (techinfo-parse-line (techinfo-find-line this-child)))
	      "\n"))
	    (if (= the-index placement-index)
		(set! point (line-start (current-point) -1)))
	    (if (not (null? the-children))
		(loop (cdr the-children) (1+ the-index) (car the-children))))))
    (set-buffer-read-only! (current-buffer))
    (set-current-point! point)))

(define (techinfo-display-leaf-node node-list)
  (set-variable! techinfo-current-node-list node-list)
  (set-buffer-writeable! (current-buffer))
  (set-current-point! (buffer-start (current-buffer)))
  (kill-string (buffer-start (current-buffer))
	       (buffer-end (current-buffer)))
  (techinfo-insert-node-header node-list)
  (let ((the-filesys (techinfo-line/filesystem node-list))
	(the-filename (techinfo-line/filename node-list)))
    (if (not (member the-filesys (ref-variable techinfo-attached-filesystems)))
	(begin
	  (if (and (string=? the-filesys "afs")
		   (ref-variable techinfo-need-to-attach-afs))
	      (run-synchronous-process #f #f
				       (->pathname "/usr/local/bin") #f
				       "attach" "-n" "-m" "/afs" "-e"
				       "atalanta.mit.edu:/afs")
	      (run-synchronous-process #f #f
				       (->pathname "/usr/local/bin") #f
				       "attach" "-n" the-filesys))
	  (set-variable! techinfo-attached-filesystems
			 (cons the-filesys
			       (ref-variable techinfo-attached-filesystems)))))
    (if (file-exists? the-filename)
	(insert-file (current-point) the-filename))
    (set-current-point! (buffer-start (current-buffer)))))

(define (techinfo-space-DWIM-internal-node)
  (let ((point (current-point)))
    (let ((the-match (re-match-forward "[ ]*\\([0-9][0-9]*\\)"
				       (line-start point 0)
				       (line-end point 0))))
      (if the-match
	  (let ((num (string->number
		      (extract-string (re-match-start 1) (re-match-end 1)))))
	    ((ref-command techinfo-goto-node) num))))))

(define (techinfo-space-DWIM-leaf-node)
  ((ref-command scroll-up) #f))

(define (techinfo-delete-DWIM-internal-node)
  ((ref-command techinfo-up)))

(define (techinfo-delete-DWIM-leaf-node)
  ((ref-command scroll-down) #f))

(define (techinfo-initialize)
  (if (not (member "ti_data" (ref-variable techinfo-attached-filesystems)))
      (begin
	(run-synchronous-process #f #f (->pathname "/usr/local/bin")
				 #f "attach" "-n" "ti_data")
	(set-variable! techinfo-attached-filesystems
		       (cons "ti_data"
			     (ref-variable techinfo-attached-filesystems)))))
  (set-variable! techinfo-web-buffer (find-buffer "*techinfo-web*"))
  (if (not (ref-variable techinfo-web-buffer))
      (let ((the-buf (create-buffer "*techinfo-web*")))
	(set-variable! techinfo-web-buffer the-buf)
	(kill-string (buffer-start the-buf)
		     (buffer-end the-buf))
	(insert-file (buffer-start the-buf) "/mit/ti_data/admin/pips.web")
	(set-buffer-read-only! the-buf)
	(bury-buffer the-buf)))
  (select-buffer (find-or-create-buffer "*TechInfo*"))
  (set-buffer-read-only! (current-buffer))
  (set-current-point! (buffer-start (current-buffer)))
  (set-current-major-mode! (ref-mode-object techinfo))
  (set-variable! techinfo-parent-list '("0"))
  (techinfo-display-node "0"))

(define-major-mode techinfo read-only "TechInfo"
  "Major mode for viewing information in the TechInfo database.
In an internal node:
Space	Move to node listed on line containing point.
DEL	Move to parent of current node.
g	Move to node specified by number.

In a leaf node:
Space	Scroll forward a page.
DEL	Scroll backward a page.

In all nodes:
.	Move to beginning of buffer.
n	Move to next sibling of this node.
p	Move to previous sibling of this node.
u	Move to parent of this node.

d	Move to root node.

q	Exit TechInfo.")

(define-key 'techinfo #\. 'beginning-of-buffer)
(define-key 'techinfo #\Space 'techinfo-space)
(define-key 'techinfo #\d 'techinfo-top)
(define-key 'techinfo #\g 'techinfo-goto-node)
(define-key 'techinfo #\n 'techinfo-next)
(define-key 'techinfo #\p 'techinfo-prev)
(define-key 'techinfo #\q 'techinfo-exit)
(define-key 'techinfo #\u 'techinfo-up)
(define-key 'techinfo #\l 'techinfo-last)
(define-key 'techinfo #\Del 'techinfo-delete)

(define-command techinfo
  "Enter TechInfo mode."
  '()
  (lambda ()
    (techinfo-initialize)))

(define-command techinfo-space
  "In an internal node, move to the subnode whose title is on the
same line as point.  In a leaf node, scroll-up."
  '()
  (lambda ()
    (if (string=? (techinfo-line/filesystem
		   (ref-variable techinfo-current-node-list))
		  "")
	(techinfo-space-DWIM-internal-node)
	(techinfo-space-DWIM-leaf-node))))

(define-command techinfo-delete
  "In an internal node, move to the parent of the current node.
In a leaf node, scroll-down."
  '()
  (lambda ()
    (if (string=? (techinfo-line/filesystem
		   (ref-variable techinfo-current-node-list))
		  "")
	(techinfo-delete-DWIM-internal-node)
	(techinfo-delete-DWIM-leaf-node))))

(define-command techinfo-up
  "Move to the parent of the current node."
  '()
  (lambda ()
    (let ((parent (car (ref-variable techinfo-parent-list))))
      (if (not (null? (cdr (ref-variable techinfo-parent-list))))
	  (set-variable! techinfo-parent-list
			 (cdr (ref-variable techinfo-parent-list))))
      (techinfo-display-node parent))))

(define-command techinfo-top
  "Move to the top node in the TechInfo tree."
  '()
  (lambda ()
    (set-variable! techinfo-parent-list '("0"))
    (techinfo-display-node "0")))

(define-command techinfo-goto-node
  "Go to the NTH node listed as a child of the current node.
NTH is an integer between 1 and the number of children of the current node.
When called interactively, NTH may be provided either as a prefix arg,
or the user will be prompted for a value."
  "NItem number: "
  (lambda (num)
    (let ((new-node (list-ref (techinfo-line/children-list
			       (ref-variable techinfo-current-node-list))
			      (-1+ num))))
      (if new-node
	  (begin
	    (set-variable! techinfo-parent-list
			   (cons (techinfo-line/number
				  (ref-variable techinfo-current-node-list) )
				 (ref-variable techinfo-parent-list)))
	    (techinfo-display-node new-node))))))

(define-command techinfo-next
  "Move to the next sibling of the current node.  (i.e., the node which
is listed immediately after the current node in the current node's parent.)
Do nothing if no such sibling exists."
  '()
  (lambda ()
    (let ((siblings
	   (techinfo-line/children-list
	    (techinfo-parse-line
	     (techinfo-find-line
	      (car (ref-variable techinfo-parent-list)))))))
      (let ((rest (member (techinfo-line/number
			   (ref-variable techinfo-current-node-list))
			  siblings)))
	(if (and rest (cdr rest))
	    (techinfo-display-node (car (cdr rest))))))))

(define-command techinfo-prev
  "Move to the previous sibling of the current node.  (i.e., the node which
is listed immediately before the current node in the current node's parent.)
Do nothing if no such subling exists."
  '()
  (lambda ()
    (let ((siblings
	   (techinfo-line/children-list
	    (techinfo-parse-line
	     (techinfo-find-line
	      (car (ref-variable techinfo-parent-list)))))))
      (let ((rest (member (techinfo-line/number
			   (ref-variable techinfo-current-node-list))
			  siblings)))
	(if rest
	    (let ((num (- (length siblings) (length rest))))
	      (techinfo-display-node (list-ref siblings (-1+ num)))))))))

(define-command techinfo-exit
  "Exit TechInfo mode."
  '()
  (lambda ()
    (bury-buffer (current-buffer))
    (select-buffer (other-buffer (current-buffer)))))

(define-command techinfo-last
  "Go to the node visited immediately before this node.
WARNING: Calling this function causes the parent information to
no longer be valid."
  '()
  (lambda ()
    (if (and (not (null? (ref-variable techinfo-history-list)))
	     (not (null? (cdr (ref-variable techinfo-history-list)))))
	(let ((new-node (car (cdr (ref-variable techinfo-history-list)))))
	  (set-variable! techinfo-history-list
			 (cdr (ref-variable techinfo-history-list)))
	  (techinfo-display-node new-node)
	  (set-variable! techinfo-history-list
			 (cdr (ref-variable techinfo-history-list)))))))