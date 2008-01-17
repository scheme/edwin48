#| -*-Scheme-*-

$Id: info.scm,v 1.143 2007/01/05 21:19:23 cph Exp $

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

;;;; Info Mode
;;; Shamelessly copied from GNU Emacs.


(define-command info
  "Create a buffer for Info, the documentation browser program."
  ()
  (lambda ()
    (let ((buffer (find-buffer info-buffer-name)))
      (if buffer
	  (select-buffer buffer)
	  ((ref-command info-directory))))))

(define info-buffer-name "*info*")

(define-variable info-enable-edit
  "If true, the \\[info-edit] command in Info can edit the current node."
  #f
  boolean?)

(define-variable info-enable-active-nodes
  "If true, allows Info to execute Scheme code associated with nodes.
The Scheme code is executed when the node is selected."
  #t
  boolean?)

(define-variable info-selection-key
  "Specifies a key or button that is used to select Info nodes.
The value of this variable is either #F, a key, or a mouse button.
If it is a key or button, menu items and adjacent node names are
highlighted (but see info-selections-highlighted), and within these
regions the key/button is bound to a command that selects the
appropriate node."
  #f
  (lambda (object) (or (not object) (comtab-key? object))))

(define-variable info-selections-highlighted
  "If true, menu and node-name selection regions are highlighted.
Such selection regions are active only when info-selection-key is set."
  #t
  boolean?)

(define-variable info-directory
  "Directory to search for Info documentation files.
This variable is now obsolete; use info-directory-list instead."
  #f
  string-or-false?)

(define-variable info-directory-list
  "List of directories to search for Info documentation files.
Empty list means not yet initialized.  In this case, Info uses the environment
variable INFOPATH to initialize it, or `info-default-directory-list'
if there is no INFOPATH variable in the environment.
The last element of `info-directory-list' is the directory
where Edwin installs the Info files that come with it."
  '()
  list-of-pathnames?)

(define-variable info-default-directory-list
  "Default list of directories to search for Info documentation files.
They are searched in the order they are given in the list.
Therefore, the directory of Info files that come with Edwin
normally should come last (so that local files override standard ones).

Once Info is started, the list of directories to search
comes from the variable `info-directory-list'.
This variable `info-default-directory-list' is used as the default
for initializing `info-directory-list' when Info is started."
  (os/info-default-directory-list)
  list-of-pathnames?)

(define-variable info-suffix-list
  "List of file-name suffixes for Info documentation files."
  (list ".info" ".inf")
  list-of-strings?)

(define-variable info-previous-search
  "Default search string for Info \\[info-search] command to search for."
  #f)

(define-variable info-history
  "List of info nodes user has visited.
Each element of list is a vector #(FILENAME NODENAME BUFFERPOS)."
  '())

(define-variable info-current-file
  "Info file that Info is now looking at, or #F."
  #f)

(define-variable info-current-subfile
  "Info subfile that is actually in the *info* buffer now,
or #F if current info file is not split into subfiles."
  #f)

(define-variable info-current-node
  "Name of node that Info is now looking at, or #F."
  #f)

(define-variable info-tag-table-start
  "Mark pointing to beginning of current Info file's tag table,
or #F if file has no tag table.")

(define-variable info-tag-table-end
  "Mark pointing to end of current Info file's tag table,
or #F if file has no tag table.")

(define-major-mode info read-only-noarg "Info"
  "Info mode provides commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which
discusses one topic and contains references to other nodes
which discuss related topics.  Info has commands to follow
the references and show you other nodes.

\\[info-help]	Invoke the Info tutorial.

Selecting other nodes:
\\[info-current-menu-item]	Follow the menu item the point is on.
\\[info-next]	Move to the \"next\" node of this node.
\\[info-previous]	Move to the \"previous\" node of this node.
\\[info-up]	Move \"up\" from this node.
\\[info-menu]	Pick menu item specified by name (or abbreviation).
	Picking a menu item causes another node to be selected.
\\[info-follow-reference]	Follow a cross reference.  Reads name of reference.
\\[info-last]	Move to the last node you were at.

Moving within a node:
\\[scroll-up]	scroll forward a page.
\\[scroll-down]	scroll backward.
\\[beginning-of-buffer]	Go to beginning of node.
\\[info-find-next-reference]	Find the next menu item or cross reference in the node.
\\[info-find-previous-reference]	Find the previous menu item or cross reference in the node.

Advanced commands:
\\[info-exit]	Quit Info: reselect previously selected buffer.
\\[info-edit]	Edit contents of selected node.
1	Pick first item in node's menu.
2, 3, 4, 5   Pick second ... fifth item in node's menu.
\\[info-goto-node]	Move to node specified by name.
	You may include a filename as well, as (FILENAME)NODENAME.
\\[info-search]	Search through this Info file for specified regexp,
	and select the node in which the next occurrence is found."
  (lambda (buffer)
    (define-variable-local-value! buffer (ref-variable-object syntax-table)
      text-mode:syntax-table)
    (define-variable-local-value! buffer (ref-variable-object case-fold-search)
      #t)
    (define-variable-local-value! buffer (ref-variable-object info-history)
      (ref-variable info-history buffer))
    (define-variable-local-value! buffer
	(ref-variable-object info-current-file)
      #f)
    (define-variable-local-value! buffer
	(ref-variable-object info-current-subfile)
      #f)
    (define-variable-local-value! buffer
	(ref-variable-object info-current-node)
      #f)
    (define-variable-local-value! buffer
	(ref-variable-object info-tag-table-start)
      #f)
    (define-variable-local-value! buffer
	(ref-variable-object info-tag-table-end)
      #f)
    (info-set-mode-line! buffer)))

(define-key 'info #\space 'scroll-up)
(define-key 'info #\. 'beginning-of-buffer)
(define-key 'info #\return 'info-current-menu-item)
(define-key 'info #\tab 'info-find-next-reference)
(define-key 'info #\M-tab 'info-find-previous-reference)
(define-key 'info #\1 'info-first-menu-item)
(define-key 'info #\2 'info-second-menu-item)
(define-key 'info #\3 'info-third-menu-item)
(define-key 'info #\4 'info-fourth-menu-item)
(define-key 'info #\5 'info-fifth-menu-item)
(define-key 'info #\? 'info-summary)
(define-key 'info #\b 'beginning-of-buffer)
(define-key 'info #\d 'info-directory)
(define-key 'info #\e 'info-edit)
(define-key 'info #\f 'info-follow-reference)
(define-key 'info #\g 'info-goto-node)
(define-key 'info #\h 'info-help)
(define-key 'info #\l 'info-last)
(define-key 'info #\m 'info-menu)
(define-key 'info #\n 'info-next)
(define-key 'info #\p 'info-previous)
(define-key 'info #\q 'info-exit)
(define-key 'info #\s 'info-search)
(define-key 'info #\u 'info-up)
(define-key 'info #\rubout 'scroll-down)

;;;; Motion

(define-command info-directory
  "Go to the Info directory node."
  ()
  (lambda ()
    (find-node "dir" "Top")))

(define-command info-help
  "Enter the Info tutorial."
  ()
  (lambda ()
    (find-node "info"
	       (if (< (window-y-size (current-window)) 23)
		   "Help-Small-Screen"
		   "Help"))))

(define-command info-next
  "Go to the next node of this node."
  ()
  (lambda ()
    (follow-pointer extract-node-next "Next")))

(define-command info-previous
  "Go to the previous node of this node."
  ()
  (lambda ()
    (follow-pointer extract-node-previous "Previous")))

(define-command info-up
  "Go to the superior node of this node."
  ()
  (lambda ()
    (follow-pointer extract-node-up "Up")))

(define (follow-pointer extractor name)
  (goto-node
   (or (extractor (buffer-start (current-buffer)))
       (editor-error "Node has no " name))))

(define-command info-goto-node
  "Go to Info node of given name.  Give just NODENAME or (FILENAME)NODENAME."
  "sGoto node"
  (lambda (name)
    (goto-node name)))

(define-command info-last
  "Go back to the last node visited."
  ()
  (lambda ()
    (if (null? (ref-variable info-history))
	(editor-error "This is the first Info node you have looked at"))
    (let ((entry (car (ref-variable info-history))))
      (set-variable! info-history (cdr (ref-variable info-history)))
      (find-node (vector-ref entry 0) (vector-ref entry 1))
      (set-variable! info-history (cdr (ref-variable info-history)))
      (set-current-point!
       (mark+ (region-start (buffer-unclipped-region (current-buffer)))
	      (vector-ref entry 2))))))

(define (find-reference-command re-search default-start default-end)
  (define (search from)
    (re-search reference-item-regexp from (default-end (current-buffer)) #t))
  (lambda ()
    (set-current-point!
     (or (search (current-point))
	 (search (default-start (current-buffer)))
	 (editor-error "No references in this node!")))))

(define-command info-find-next-reference
  "Find the starting point of the next menu item or cross reference."
  ()
  (find-reference-command re-search-forward buffer-start buffer-end))

(define-command info-find-previous-reference
  "Find the starting point of the previous menu item or cross reference."
  ()
  (find-reference-command re-search-backward buffer-end buffer-start))

;;;; Miscellaneous

(define-command info-exit
  "Exit Info by selecting some other buffer."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (select-buffer (previous-buffer))
      (bury-buffer buffer))))

(define-command info-summary
  "Display a brief summary of all Info commands."
  ()
  (lambda ()
    (let ((buffer (temporary-buffer "*Help*")))
      (call-with-output-mark (buffer-point buffer)
	(lambda (port)
	  (write-string
	   (substitute-command-keys (mode-description (current-major-mode)))
	   port)))
      (set-buffer-point! buffer (buffer-start buffer))
      (buffer-not-modified! buffer)
      (with-selected-buffer buffer
	(lambda ()
	  (let loop ()
	    (update-selected-screen! #f)
	    (let ((end-visible?
		   (window-mark-visible? (current-window)
					 (buffer-end buffer))))
	      (message (if end-visible?
			   "Type Space to return to Info"
			   "Type Space to see more"))
	      (let ((key (keyboard-peek)))
		(if (and (char? key)
			 (char=? key #\Space))
		    (begin
		      (keyboard-read)
		      (if (not end-visible?)
			  (begin
			    ((ref-command scroll-up) #f)
			    (loop))))))))
	  (clear-message))))))

(define-command info-edit
  "Edit the contents of this Info node.
Allowed only if the variable Info Enable Edit is not false."
  ()
  (lambda ()
    (if (not (ref-variable info-enable-edit))
	(editor-error "Editing Info nodes is not enabled"))
    (set-buffer-writeable! (current-buffer))
    (set-current-major-mode! (ref-mode-object info-edit))
    (message "Editing: Type C-c C-c to return to Info")))

(define-major-mode info-edit text "Info-Edit"
  "Major mode for editing the contents of an Info node.
The editing commands are the same as in Text mode,
except for \\[info-cease-edit] to return to Info."
  (lambda (buffer)
    (define-variable-local-value! buffer (ref-variable-object page-delimiter)
      (string-append "^\037\f\\|" (ref-variable page-delimiter buffer)))))

(define-key 'info-edit '(#\c-c #\c-c) 'info-cease-edit)

(define-command info-cease-edit
  "Finish editing Info node; switch back to Info proper."
  ()
  (lambda ()
    (save-buffer-changes (current-buffer))
    (set-current-major-mode! (ref-mode-object info))
    (set-buffer-read-only! (current-buffer))
    (clear-message)))

;;;; Search

(define-command info-search
  "Search for regexp, starting from point, and select node it's found in."
  (lambda ()
    (let ((regexp
	   (prompt-for-string "Search (regexp)"
			      (ref-variable info-previous-search))))
      (set-variable! info-previous-search regexp)
      (list regexp)))
  (lambda (regexp)
    (let ((regexp
	   (if (string-null? regexp)
	       (ref-variable info-previous-search)
	       (begin
		 (set-variable! info-previous-search regexp)
		 regexp)))
	  (buffer (current-buffer)))
      (let ((original-file (ref-variable info-current-file))
	    (original-subfile (ref-variable info-current-subfile))
	    (original-node (ref-variable info-current-node))
	    (original-point (mark-index (current-point)))
	    (perform-search
	     (lambda (start)
	       (without-group-clipped! (buffer-group buffer)
		 (lambda ()
		   (re-search-forward regexp start (group-end start))))))
	    (win
	     (lambda (mark)
	       (buffer-widen! buffer)
	       (select-buffer buffer)
	       (select-node mark))))
	(let ((mark (perform-search (current-point))))
	  (if mark
	      (win mark)
	      (if (not original-subfile)
		  (editor-error)
		  (let loop
		      ((subfiles
			(let ((subfile (ref-variable info-current-subfile)))
			  (let loop ((subfiles (subfile-list)))
			    (if (pathname=? (subfile-pathname (car subfiles))
					    subfile)
				(cdr subfiles)
				(loop (cdr subfiles)))))))
		    (if (null? subfiles)
			(begin
			  (clear-message)
			  (set-current-subfile! original-subfile)
			  (select-node
			   (mark+ (buffer-start buffer) original-point))
			  (editor-error))
			(begin
			  (let ((pathname (subfile-pathname (car subfiles))))
			    (message "Searching subfile "
				     (file-namestring pathname)
				     "...")
			    (set-current-subfile! pathname))
			  (let ((mark (perform-search (buffer-start buffer))))
			    (if mark
				(begin
				  (clear-message)
				  (win mark))
				(loop (cdr subfiles))))))))))
	(if (and original-file
		 (not (and (pathname=? original-file
				       (ref-variable info-current-file))
			   (string-ci=? original-node
					(ref-variable info-current-node)))))
	    (record-node original-file original-node original-point))))))

;;;; Menus

(define-command info-menu
  "Go to node for menu item of given name."
  ()
  (lambda ()
    (let ((menu (find-menu)))
      (if (not menu)
	  (editor-error "No menu in this node"))
      (goto-node
       (menu-item-name
	(make-mark
	 (mark-group menu)
	 (let ((item-alist (collect-menu-items menu)))
	   (let ((current-entry
		  (let ((current-item (current-menu-item (current-point))))
		    (and current-item
			 (let ((current-index (mark-index current-item)))
			   (find (lambda (entry) (= current-index (cdr entry)))
				 item-alist))))))
	     (if current-entry
		 (prompt-for-alist-value "Menu item"
					 item-alist
					 (car current-entry))
		 (prompt-for-alist-value "Menu item"
					 item-alist))))))))))

(define-command info-current-menu-item
  "Go to the node of the menu item that point is on."
  ()
  (lambda ()
    (let ((point
	   (let ((event (current-button-event)))
	     (let ((window (button-event/window event)))
	       (or (window-coordinates->mark window
					     (button-event/x event)
					     (button-event/y event))
		   (window-point window))))))
      (let ((item (current-menu-item point)))
	(if (not item)
	    (editor-error "Point not on a menu item"))
	(goto-node (menu-item-name item))))))

(define (nth-menu-item n)
  (lambda ()
    (let loop
	((mark
	  (next-menu-item
	   (or (find-menu) (editor-error "No menu in this node"))))
	 (n n))
      (cond ((not mark) (editor-error "Too few items in menu"))
	    ((zero? n) (goto-node (menu-item-name mark)))
	    (else (loop (next-menu-item mark) (-1+ n)))))))

(define-command info-first-menu-item
  "Go to the node of the first menu item."
  ()
  (nth-menu-item 0))

(define-command info-second-menu-item
  "Go to the node of the second menu item."
  ()
  (nth-menu-item 1))

(define-command info-third-menu-item
  "Go to the node of the third menu item."
  ()
  (nth-menu-item 2))

(define-command info-fourth-menu-item
  "Go to the node of the fourth menu item."
  ()
  (nth-menu-item 3))

(define-command info-fifth-menu-item
  "Go to the node of the fifth menu item."
  ()
  (nth-menu-item 4))

(define (find-menu)
  (let ((buffer (current-buffer)))
    (re-search-forward menu-indicator-regexp
		       (buffer-start buffer)
		       (buffer-end buffer)
		       #t)))

(define rexp:menu-indicator
  (rexp-sequence (rexp-line-start)
		 "* Menu:"))

(define menu-indicator-regexp (rexp-compile rexp:menu-indicator))

(define rexp:menu-item
  (rexp-sequence
   (rexp-line-start)
   "* "
   (rexp* (char-set #\space #\tab))
   (rexp-group (rexp* (char-set-invert (char-set #\: #\tab #\newline))))
   (rexp* (char-set #\space #\tab))
   ":"))

(define menu-item-regexp (rexp-compile rexp:menu-item))

(define rexp:cross-reference-item
  (rexp-sequence "*Note" (rexp* (char-set #\space #\tab #\newline))))

(define cross-reference-item-regexp (rexp-compile rexp:cross-reference-item))

(define rexp:reference-item
  (rexp-alternatives rexp:menu-item rexp:cross-reference-item))

(define reference-item-regexp (rexp-compile rexp:reference-item))

(define (collect-menu-items mark)
  (let ((group (mark-group mark)))
    (let ((end (group-end-index group)))
      (let loop ((start (mark-index mark)))
	(if (re-search-buffer-forward menu-item-regexp #f group start end)
	    (let ((item (re-match-start-index 1)))
	      (let ((keyword
		     (group-extract-string group
					   item
					   (re-match-end-index 1))))
		(cons (cons keyword item)
		      (loop item))))
	    '())))))

(define (mark-menu-items mark marker)
  (let ((group (mark-group mark)))
    (let ((end (group-end-index group)))
      (let loop ((start (mark-index mark)))
	(if (re-search-buffer-forward menu-item-regexp #f group start end)
	    (let ((item (re-match-start-index 1)))
	      (marker group
		      item
		      (re-match-end-index 1))
	      (loop item)))))))

(define (next-menu-item mark)
  (and (re-search-forward menu-item-regexp mark (group-end mark) #f)
       (re-match-start 1)))

(define (current-menu-item mark)
  (let ((menu (find-menu))
	(start (mark-1+ (line-start mark 0) 'LIMIT)))
    (and menu
	 (mark> start menu)
	 (re-search-forward menu-item-regexp
			    (mark-1+ (line-start mark 0) 'LIMIT)
			    (line-end mark 0)
			    #f)
	 (re-match-start 1))))

(define (menu-item-name item)
  (let ((colon (char-search-forward #\: item (line-end item 0) #f)))
    (if (not colon)
	(error "Menu item missing colon."))
    (if (match-forward "::" (mark-1+ colon))
	(extract-string item (skip-chars-backward " \t" (mark-1+ colon)))
	(following-node-name colon ".,\t\n"))))

(define (following-node-name start delimiters)
  (let ((start (skip-chars-forward " \t\n" start)))
    (extract-string
     start
     (skip-chars-backward
      " "
      (let loop ((start start))
	(if (re-match-forward (string-append "[^" delimiters "]") start)
	    (loop
	     (let ((m
		    (skip-chars-forward (string-append "^" delimiters "(")
					start)))
	       (if (match-forward "(" m)
		   (skip-chars-forward "^)" m)
		   m)))
	    start))))))

;;;; Cross References

(define-command info-follow-reference
  "Follow cross reference, given name, to the node it refers to.
The name may be an abbreviation of the reference name."
  ()
  (lambda ()
    (let ((items (collect-cref-items (buffer-start (current-buffer)))))
      (if (null? items)
	  (editor-error "No cross references in this node")
	  (goto-node
	   (prompt-for-alist-value "Follow reference named" items))))))

(define (collect-cref-items mark)
  (let ((item (next-cref-item mark)))
    (if (not item)
	'()
	(cons (cons (cref-item-keyword item)
		    (cref-item-name item))
	      (collect-cref-items item)))))

(define (next-cref-item start)
  (re-search-forward cross-reference-item-regexp start (group-end start) #t))

(define (cref-item-keyword item)
  (let ((colon (char-search-forward #\: item (group-end item) #f)))
    (if (not colon)
	(error "Cross reference missing colon."))
    (%cref-item-keyword item (mark-1+ colon))))

(define (%cref-item-keyword item colon)
  (let ((string (extract-string item colon)))
    (string-replace! string #\newline #\Space)
    (string-trim string)))

(define (cref-item-name item)
  (let ((colon (char-search-forward #\: item (group-end item) #f)))
    (if (not colon)
	(error "Cross reference missing colon."))
    (if (match-forward "::" (mark-1+ colon))
	(%cref-item-keyword item (skip-chars-backward " \t" (mark-1+ colon)))
	(following-node-name colon ".,\t\n"))))

;;;; Validation

(define-command info-validate
  "Check that every node pointer points to an existing node."
  ()
  (lambda ()
    (let ((nodes (current-nodes-list))
	  (losers '()))
      (define (validate this-name type node-name)
	(and node-name
	     (parse-node-name node-name
	       (lambda (filename nodename)
		 (and (not filename)
		      (let ((entry (node-assoc nodename nodes)))
			(if (not entry)
			    (set! losers
				  (cons (vector this-name type node-name)
					losers)))
			entry))))))
      (for-each (lambda (entry)
		  (let ((name (car entry))
			(node (region-start (cadr entry))))
		    (define (validate-extract type extractor)
		      (validate name type (extractor node)))

		    (define ((validate-item prefix) item)
		      (validate name
				(string-append prefix " " (car item))
				(cdr item)))

		    (with-region-clipped! (cadr entry)
		      (lambda ()
			(let ((entry* (validate-extract "Next"
							extract-node-next)))
			  (if (and entry*
				   (or (not (caddr entry*))
				       (not
					(string-ci=? (caddr entry*) name))))
			      (set! losers
				    (cons (vector name
						  "Previous-pointer in Next"
						  (car entry*))
					  losers))))
			(validate-extract "Previous" extract-node-previous)
			(validate-extract "Up" extract-node-up)
			(let ((menu (find-menu)))
			  (if menu
			      (for-each (validate-item "Menu item")
					(collect-menu-items menu))))
			(for-each (validate-item "Reference")
				  (collect-cref-items node))))))
		nodes)
      (report-losers losers))))

(define (report-losers losers)
  (if (null? losers)
      (message "File appears valid")
      (with-output-to-temporary-buffer " *problems in info file*" '()
	(lambda ()
	  (for-each (lambda (loser)
		      (write-string
		       (string-append "In node " (vector-ref loser 0)
				      ", invalid " (vector-ref loser 1)
				      ": " (vector-ref loser 2)))
		      (newline))
		    losers)))))

(define (current-nodes-list)
  (let ((buffer (current-buffer)))
    (without-group-clipped! (buffer-group buffer)
      (lambda ()
	(collect-nodes (buffer-start buffer))))))

(define (collect-nodes mark)
  (let ((node (next-node mark (group-end mark))))
    (if (not node)
	'()
	(let ((name (extract-node-name node)))
	  (if name
	      (cons (list name (node-region node) (extract-node-previous node))
		    (collect-nodes node))
	      (collect-nodes node))))))

(define node-assoc
  (association-procedure string-ci=? car))

;;;; Node Parsing

(define (goto-node name)
  (parse-node-name name find-node))

(define (find-node filename nodename)
  (let ((buffer (find-or-create-buffer info-buffer-name)))
    (select-buffer buffer)
    (if (ref-variable info-current-file buffer)
	(record-node (ref-variable info-current-file buffer)
		     (ref-variable info-current-node buffer)
		     (mark-index (current-point))))
    (if (and filename (string-ci=? "dir" filename))
	(initialize-info-buffer buffer (find-dir-node buffer))
	(let ((pathname (and filename (find-node-1 buffer filename))))
	  ;; Switch files if necessary.
	  (if (and pathname
		   (not (equal? pathname
				(ref-variable info-current-file buffer))))
	      (begin
		(read-buffer buffer pathname #t)
		(initialize-info-buffer buffer pathname))
	      (begin
		(if (not (eq? (buffer-major-mode buffer)
			      (ref-mode-object info)))
		    (set-buffer-major-mode! buffer (ref-mode-object info)))
		(buffer-widen! buffer)))))
      (set-buffer-read-only! buffer)
      (if (string=? nodename "*")
	  (begin
	    (set-variable! info-current-subfile #f)
	    (set-variable! info-current-node nodename)
	    (info-set-mode-line! buffer))
	  (select-node (find-node-in-buffer nodename buffer)))))

(define (find-node-in-buffer nodename buffer)
  (let ((end (buffer-end buffer)))
    (let loop ((start (node-search-start nodename)))
      (let ((node (next-node start end)))
	(if (not node) (editor-error "No such node: " nodename))
	(if (let ((name (extract-node-name node)))
	      (and name
		   (string-ci=? nodename name)))
	    node
	    (loop node))))))

(define (initialize-info-buffer buffer pathname)
  (if (not (eq? (buffer-major-mode buffer) (ref-mode-object info)))
      (set-buffer-major-mode! buffer (ref-mode-object info)))
  (find-tag-table buffer)
  (set-variable! info-current-file pathname)
  (set-variable! info-current-subfile #f))

(define (find-dir-node buffer)
  (let ((pathnames (find-dir-node-files buffer)))
    (if (null? pathnames)
	(editor-error "Can't find the Info directory node."))
    (read-buffer buffer (car pathnames) #t)
    (set-variable! info-tag-table-start #f)
    (set-variable! info-tag-table-end #f)
    (let ((submenus (append-map find-dir-node-menus (cdr pathnames))))
      (find-dir-node/insert-node-names buffer submenus)
      (for-each (lambda (submenu)
		  (find-dir-node/insert-menu-items buffer submenu))
		submenus))
    "dir"))

(define (find-dir-node-files buffer)
  (let loop ((directories (buffer-directory-list buffer)) (pathnames '()))
    (if (null? directories)
	(reverse! pathnames)
	(loop (cdr directories)
	      (let ((pathname
		     (find-node-2 buffer
				  (merge-pathnames "dir" (car directories)))))
		(if pathname
		    (cons pathname pathnames)
		    pathnames))))))

(define (find-dir-node-menus pathname)
  (call-with-temporary-buffer " info dir"
    (lambda (buffer)
      (insert-file (buffer-end buffer) pathname)
      (let ((bs (buffer-start buffer))
	    (be (buffer-end buffer)))
	(let loop ((start bs) (menus '()))
	  (let ((ms
		 (let ((m (re-search-forward "^\\* menu:" start be #t)))
		   (and m
			(line-start m 1)))))
	    (if (not ms)
		(reverse! menus)
		(let ((ne (node-end ms)))
		  (loop ne
			(cons (cons (extract-node-name (node-start ms bs))
				    (extract-string ms ne))
			      menus))))))))))

(define (find-dir-node/insert-node-names buffer submenus)
  (let ((main-menu
	 (let ((m
		(re-search-forward "^\\* menu:"
				   (buffer-start buffer)
				   (buffer-end buffer)
				   #t)))
	   (if (not m)
	       (error "Unable to find Info menu in buffer:" buffer))
	   (mark-left-inserting-copy m)))
	(menu-items '("top")))
    (let ((end (node-end main-menu)))
      (for-each
       (lambda (submenu)
	 (let ((nodename (car submenu)))
	   (if (not (or (find (lambda (item) (string-ci=? item nodename))
			      menu-items)
			(re-search-forward (string-append "^\\* "
							  (re-quote-string
							   nodename)
							  "::")
					   main-menu
					   end
					   #t)))
	       (begin
		 (set! menu-items (cons nodename menu-items))
		 (insert-string (string-append "* " nodename "::\n")
				main-menu)))))
       submenus))
    (mark-temporary! main-menu)))

(define (find-dir-node/insert-menu-items buffer submenu)
  (let ((nodename (car submenu))
	(menu-entries (cdr submenu)))
    (let ((m
	   (let ((node (find-node-in-buffer nodename buffer)))
	     (if node
		 (let ((m (mark-left-inserting-copy (node-end node))))
		   (guarantee-newlines 2 m)
		   m)
		 (let ((m (mark-left-inserting-copy (buffer-end buffer))))
		   (guarantee-newline m)
		   (insert-string (string-append "\037\nFile: dir\tNode: "
						 nodename
						 "\n\n* Menu:\n\n")
				  m)
		   m)))))
      (insert-string menu-entries m)
      (guarantee-newline m)
      (mark-temporary! m))))

(define (find-node-1 buffer pathname)
  (let loop
      ((directories
	(if (let ((directory (pathname-directory pathname)))
	      (and (pair? directory)
		   (or (eq? (car directory) 'ABSOLUTE)
		       (and (eq? (car directory) 'RELATIVE)
			    (pair? (cdr directory))
			    (equal? (cadr directory) ".")))))
	    (list (buffer-default-directory buffer))
	    (buffer-directory-list buffer))))
    (if (null? directories)
	(editor-error "Can't find Info file: " (->namestring pathname)))
    (or (find-node-2 buffer (merge-pathnames pathname (car directories)))
	(loop (cdr directories)))))

(define (buffer-directory-list buffer)
  (map pathname-as-directory
       (let ((variable (ref-variable-object info-directory-list)))
	 (let ((directories (variable-local-value buffer variable)))
	   (if (null? directories)
	       (let ((directories
		      (let ((directories
			     (cond ((ref-variable info-directory buffer)
				    => list)
				   ((get-environment-variable "INFOPATH")
				    => os/parse-path-string)
				   (else
				    (ref-variable info-default-directory-list
						  buffer)))))
			(map ->namestring
			     (let ((info-dir (edwin-info-directory)))
			       (if (and info-dir
					(file-directory? info-dir)
					(not (any (lambda (dir)
						    (pathname=? info-dir dir))
						  directories)))
				   (append directories (list info-dir))
				   directories))))))
		 (set-variable-local-value! buffer variable directories)
		 directories)
	       directories)))))

(define (find-node-2 buffer pathname)
  (let ((group (buffer-group buffer)))
    (or (get-pathname-or-alternate group pathname #f)
	(let ((s (->namestring pathname)))
	  (let loop ((suffixes (ref-variable info-suffix-list buffer)))
	    (and (not (null? suffixes))
		 (or (get-pathname-or-alternate
		      group
		      (string-append s (car suffixes))
		      #f)
		     (loop (cdr suffixes)))))))))

(define (info-set-mode-line! buffer)
  (define-variable-local-value! buffer
      (ref-variable-object mode-line-buffer-identification)
    (string-append "Info:  ("
		   (let ((pathname (ref-variable info-current-file)))
		     (if pathname
			 (file-namestring pathname)
			 ""))
		   ")"
		   (or (ref-variable info-current-node) ""))))

(define (parse-node-name name receiver)
  (let ((name (string-trim name)))
    (if (char=? (string-ref name 0) #\()
	(let ((index (string-index name #\))))
	  (if index
	      (let ((filename (string-trim (substring name 1 index)))
		    (nodename (string-trim (substring name (1+ index)
						      (string-length name)))))
		(receiver filename
			  (if (string-null? nodename) "Top" nodename)))
	      (error "PARSE-NODE-NAME: Missing close paren" name)))
	(receiver #f (if (string-null? name) "Top" name)))))

(define (select-node point)
  (let ((node (node-start point (group-start point))))
    (set-variable! info-current-node (extract-node-name node))
    (info-set-mode-line! (current-buffer))
    ;; **** need to add active node hacking here ****
    (region-clip! (node-region node))
    (set-current-point! point)
    (let ((key (ref-variable info-selection-key point)))
      (if key
	  (info-enable-selections node
				  key
				  (ref-variable info-selections-highlighted
						point)))))
  (buffer-not-modified! (mark-buffer point)))

(define (info-enable-selections node key highlight?)
  (let ((comtab
	 (lambda (command)
	   (let ((comtab (make-comtab)))
	     (define-key comtab key command)
	     (list comtab)))))
    (let ((do-button
	   (lambda (locator command)
	     (let ((region (locator node)))
	       (if region
		   (begin
		     (if highlight? (highlight-region region (highlight-face)))
		     (set-region-local-comtabs! region (comtab command))))))))
      (do-button locate-node-up (ref-command-object info-up))
      (do-button locate-node-previous (ref-command-object info-previous))
      (do-button locate-node-next (ref-command-object info-next)))
    (let ((menu (find-menu)))
      (if menu
	  (mark-menu-items menu
	    (let ((comtabs
		   (comtab (ref-command-object info-current-menu-item))))
	      (lambda (group start end)
		(if highlight?
		    (highlight-subgroup group start end (highlight-face)))
		(set-subgroup-local-comtabs! group start end comtabs))))))))

(define (record-node file node point)
  (set-variable! info-history
		 (cons (vector (->namestring file) node point)
		       (ref-variable info-history))))

(define (node-start start end)
  (line-start (or (search-backward "\n\037" start end #f)
		  (editor-error))
	      2
	      'ERROR))

(define (node-region node)
  (make-region node (node-end node)))

(define (node-end node)
  (let ((end (group-end node)))
    (let loop ((start node))
      (let ((mark (re-search-forward "[\f\037]" start end #f)))
	(if (not mark)
	    end
	    (let ((m (re-match-start 0)))
	      (if (char=? (extract-left-char m) #\newline)
		  (mark-1+ m)
		  (loop mark))))))))

(define (next-node start end)
  (let ((mark (search-forward "\n\037" start end #f)))
      (and mark
	   (line-start mark 1))))

(define ((field-value-locator field) node)
  (let ((end (line-end node 0)))
    (let ((mark (re-search-forward field node end #t)))
      (and mark
	   (let ((start (skip-chars-forward " " mark end)))
	     (make-region start
			  (skip-chars-backward " "
					       (skip-chars-forward "^,\t"
								   start
								   end)
					       start)))))))

(define locate-node-name
  (field-value-locator "Node:"))

(define locate-node-up
  (field-value-locator "Up:"))

(define locate-node-previous
  (field-value-locator "Prev\\(ious\\|\\):"))

(define locate-node-next
  (field-value-locator "Next:"))

(define ((field-value-extractor locator) node)
  (let ((region (locator node)))
    (and region
	 (region->string region))))

(define extract-node-name
  (field-value-extractor locate-node-name))

(define extract-node-up
  (field-value-extractor locate-node-up))

(define extract-node-previous
  (field-value-extractor locate-node-previous))

(define extract-node-next
  (field-value-extractor locate-node-next))

;;;; Tag Tables

(define-command info-tagify
  "Create or update tag table of current info file."
  ()
  (lambda ()
    (let ((buffer (current-buffer)))
      (without-group-clipped! (buffer-group buffer)
	(lambda ()
	  (with-read-only-defeated (buffer-end buffer)
	    (lambda ()
	      ;; Flush old tag table if present.
	      (find-tag-table buffer)
	      (if (ref-variable info-tag-table-start)
		  (delete-string (mark- (ref-variable info-tag-table-start)
					(string-length tag-table-start-string))
				 (mark+ (ref-variable info-tag-table-end)
					(string-length tag-table-end-string))))
	      ;; Then write new table.
	      (let ((entries (collect-tag-entries (buffer-start buffer))))
		(call-with-output-mark (buffer-end buffer)
		  (lambda (port)
		    (write-string tag-table-start-string port)
		    (for-each (lambda (entry)
				(write-string (cdr entry) port)
				(write-char #\Rubout port)
				(write (mark-index (car entry)) port)
				(newline port))
			      entries)
		    (write-string tag-table-end-string port))))))
	  ;; Finally, reset the tag table marks.
	  (find-tag-table buffer))))))

(define (collect-tag-entries mark)
  (let ((node (next-node mark (group-end mark))))
    (if (not node)
	'()
	(let ((entry (extract-tag-entry node)))
	  (if entry
	      (cons (cons node entry)
		    (collect-tag-entries node))
	      (collect-tag-entries node))))))

(define (extract-tag-entry node)
  (let ((end (line-end node 0)))
    (let ((mark (search-forward "Node:" node end #t)))
      (and mark
	   (string-trim
	    (extract-string node
			    (skip-chars-forward "^,\t" mark end)))))))

(define tag-table-start-string
  "\nTag table:\n")

(define tag-table-end-string
  "\037\nEnd tag table\n")

(define (find-tag-table buffer)
  (let* ((end (buffer-end buffer))
	 (mark (line-start end -8))
	 (tag-table-end
	  (and mark
	       (search-forward tag-table-end-string mark end #t)
	       (re-match-start 0)))
	 (tag-table-start
	  (and tag-table-end
	       (search-backward tag-table-start-string
				tag-table-end
				(buffer-start buffer)
				#t)
	       (re-match-end 0))))
    (if (and tag-table-end (not tag-table-start))
	(begin
	  (message "Ill-formed tag table, ignoring")
	  (editor-beep)))
    ;; If this is an indirect file, read the top node into another
    ;; buffer and set the marks to point at it.
    (if (and tag-table-start
	     (match-forward "(Indirect)\n" tag-table-start))
	(let* ((buffer* (find-or-create-buffer " *info tag table*"))
	       (group (buffer-group buffer*)))
	  (insert-string (extract-string (buffer-start buffer)
					 (buffer-end buffer))
			 (buffer-start buffer*))
	  (set-variable! info-tag-table-start
			 (make-mark group (mark-index tag-table-start)))
	  (set-variable! info-tag-table-end
			 (make-mark group (mark-index tag-table-end))))
	(begin
	  (set-variable! info-tag-table-start tag-table-start)
	  (set-variable! info-tag-table-end
			 (and tag-table-start tag-table-end))))))

(define (node-search-start nodename)
  (let ((buffer (current-buffer)))
    (if (ref-variable info-tag-table-start)
	(let ((mark
	       (or (search-forward (string-append "Node: " nodename "\177")
				   (ref-variable info-tag-table-start)
				   (ref-variable info-tag-table-end)
				   #t)
		   (editor-error "No such node: " nodename))))
	  ;; Force order of events, since read-subfile has side-effect.
	  (let ((index
		 (let ((start (read-index-from-mark mark)))
		   (if (mark~ (ref-variable info-tag-table-start)
			      (buffer-start buffer))
		       start
		       (read-subfile start)))))
	    (mark+ (buffer-start buffer) (max 0 (- index 1000)))))
	(buffer-start buffer))))

(define (read-subfile index)
  (let loop
      ((subfiles
	(let ((subfiles (subfile-list)))
	  (if (> (cdar subfiles) index)
	      (editor-error "Illegal indirect index" index))
	  subfiles)))
    (if (or (null? (cdr subfiles))
	    (> (cdadr subfiles) index))
	(begin
	  (set-current-subfile! (subfile-pathname (car subfiles)))
	  (+ (- index (subfile-index (car subfiles)))
	     (mark-index
	      (let ((buffer (current-buffer)))
		(or (search-forward "\n\037"
				    (buffer-start buffer)
				    (buffer-end buffer)
				    #f)
		    (editor-error))))))
	(loop (cdr subfiles)))))

(define (set-current-subfile! pathname)
  (if (not (equal? pathname (ref-variable info-current-subfile)))
      (begin
	(read-buffer (current-buffer) pathname #t)
	(set-variable! info-current-subfile pathname))))

(define subfile-filename car)
(define subfile-index cdr)

(define (subfile-pathname subfile)
  (merge-pathnames (subfile-filename subfile)
		   (ref-variable info-current-file)))

(define (subfile-list)
  (let ((result
	 (let loop
	     ((start
	       (let ((start (ref-variable info-tag-table-start)))
		 (or (search-forward "\n\037\nIndirect:\n"
				     (group-start start)
				     start
				     #t)
		     (editor-error)))))
	   (if (match-forward "\037" start)
	       '()
	       (begin
		 (if (not (search-forward ": " start (group-end start) #f))
		     (editor-error))
		 (let* ((colon (re-match-start 0))
			(index (read-index-from-mark (re-match-end 0))))
		   (cons (cons (extract-string start colon) index)
			 (loop (line-start start 1 'ERROR)))))))))
    (if (null? result)
	(editor-error "Empty indirect file list"))
    result))

(define (read-index-from-mark mark)
  (let ((lose
	 (lambda ()
	   (editor-error "Malformed index in Info file"))))
    (bind-condition-handler (list condition-type:error)
	(lambda (condition)
	  condition
	  (lose))
      (lambda ()
	(let ((index (with-input-from-mark mark read)))
	  (if (and (integer? index)
		   (positive? index))
	      (-1+ index)
	      (lose)))))))