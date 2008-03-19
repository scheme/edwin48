#| -*-Scheme-*-

$Id: snr.scm,v 1.70 2008/01/30 20:02:05 cph Exp $

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

;;;; Scheme News Reader


(load-option 'ORDERED-VECTOR)

;;; Variables affecting the reader:

(define-variable news-server
  "Host name of the default News server.
This is the name used by \\[rnews].
If it is an empty string, \\[rnews] will prompt for a host name and
 save it back into news-server."
  ""
  string?)

(define-variable news-server-name-appearance
  "Switch controlling appearance of server name in News buffers.
This has three possible values:
  'NONE means do not include the server name.
  'FULL means include the fully qualified host name.
  'HOST-ONLY means include the host name, but not the domain."
  'NONE
  (lambda (object) (memq object '(NONE FULL HOST-ONLY))))

(define-variable news-server-proxy-alist
  "Alist mapping news servers to associated proxies.
Each entry in the list is a pair of strings:
 the car of the entry is the FQDN of a news server;
 the cdr of the entry is the FQDN of a proxy for that server."
  '()
  (lambda (object)
    (list-of-type? object
      (lambda (entry)
	(and (pair? entry)
	     (string? (car entry))
	     (string? (cdr entry)))))))

(define-variable news-server-initial-refresh
  "Switch controlling whether News groups are refreshed when reader starts.
If false (the default), groups are initially listed with the estimates
 that were current the last time the news-reader was run.
If true, the server is asked to provide current estimates for all
 subscribed groups.
Note that if this variable is true, the reader will go on-line when it
 is started."
  #f
  boolean?)

(define-variable news-server-offline-timeout
  "Number of seconds to stay online after each server transaction.
If no further transactions are performed after this long, the server
 connection is closed.
This variable can be set to #F to disable the timeout altogether.
[THIS VARIABLE CURRENTLY HAS NO EFFECT.]"
  #f
  (lambda (object) (or (not object) (exact-nonnegative-integer? object))))

(define-variable news-header-filter
  "Procedure for filtering news headers, or #F for no filter.
Every header read by the news reader is filtered through this filter.  If it
  returns false, the header is ignored."
  #f
  (lambda (filter)
    (or (not filter)
        (procedure-of-arity? filter 1))))

;;; Variables for News-group-list buffers:

(define-variable news-show-unsubscribed-groups
  "Switch controlling whether unsubscribed News groups appear in server buffers.
If false (the default), only currently subscribed groups are shown.
If true, previously subscribed groups are also shown."
  #f
  boolean?)

(define-variable news-show-nonexistent-groups
  "Switch controlling whether nonexistent News groups appear in server buffers.
If false, only News groups existing on the server are shown.  Note
 that this forces the reader to go on-line to determine which groups
 exist.
If true (the default), all subscribed groups are shown."
  #t
  boolean?)

(define-variable news-sort-groups
  "Switch controlling whether the News groups are sorted.
If true (the default), News groups in the subscribed-groups buffer are sorted.
If false, groups appear in the order they are listed in the init file."
  #t
  boolean?)

(define-variable news-hide-groups
  "List of regexps indicatings groups to be hidden.
Any News group whose name matches one of these regexps will not be shown
 in the all-groups and new-groups buffers.
Subscribing to such a group will still work, and afterwards the
 group will appear in the subscribed-groups buffer."
  '()
  list-of-strings?)

(define-variable news-refresh-group-when-selected
  "Switch controlling whether News group is refreshed when selected.
If true, selecting a group causes it to be refreshed, so that the
 headers shown are current at the time of selection.  Note that this
 forces the reader to go on-line to determine the current set of
 headers.
If false (the default), the headers shown are the ones that were
 current when the group was last selected."
  #f
  boolean?)

;;; Variables for News-group buffers:

(define-variable news-initially-collapse-threads
  "Switch controlling initial collapsing of News threads.
If true (the default), threads are initially collapsed.
If false, they are initially expanded.
A collapsed thread is automatically expanded when entered."
  #t
  boolean?)

(define-variable news-automatically-collapse-threads
  "Switch controlling automatic collapsing of News threads.
A collapsed thread is automatically expanded when entered.
This switch can take several values:
  'NEVER      Threads are never automatically collapsed.  This is the default.
  'AUTOMATIC  Any automatically expanded thread is re-collapsed when left.
  'ALWAYS     Any expanded thread is re-collapsed when left."
  'NEVER
  (lambda (object) (memq object '(NEVER AUTOMATIC ALWAYS))))

(define-variable news-split-threads-on-subject-changes
  "Switch controlling whether News threads can span subject changes.
If true (the default), a thread is broken into multiple threads when
 the Subject: header changes.  This guarantees that each thread covers
 only a single subject.
Otherwise, a thread containing subject changes remains whole."
  #t
  boolean?)

(define-variable news-join-threads-with-same-subject
  "Switch controlling whether News threads with same subject are joined.
If true (the default), two or more threads with the same Subject:
 header are joined together into a single thread.
Otherwise, threads with the same subject remain separate."
  #t
  boolean?)

(define-variable news-article-context-lines
  "The number of lines to show in a News-group context window."
  5
  (lambda (object) (and (exact-integer? object) (> object 0))))

(define-variable news-article-highlight-selected
  "Switch controlling display of selected articles in a News-group buffer.
If true (the default), selected articles are indicated by highlights.
If false, there is no indication.
This is primarily used to enhance the context window."
  #t
  boolean?)

(define-variable news-group-author-columns
  "Number of columns used to display the author in a News-article header line.
This applies only to header lines that contain subjects."
  15
  exact-nonnegative-integer?)

(define-variable news-group-show-author-name
  "Switch controlling appearance of author's name in a News-article header line.
If true (the default), the author's full name will be shown, if available.
If false, the email address of the author is shown."
  #t
  boolean?)

(define-variable news-group-ignored-subject-retention
  "How long to retain ignored-subject data, in days.
If an ignored subject is not seen for this many days, the subject line
 is removed from the ignored-subject database.  This stops it from
 being ignored.
By default, ignored subjects are kept for 30 days."
  30
  (lambda (object) (and (real? object) (not (negative? object)))))

(define-variable news-group-ignore-hidden-subjects
  "If true, ignore all subjects in a thread, even if hidden.
If false, subject changes within the thread are not ignored."
  #t
  boolean?)

(define-variable news-group-keep-seen-headers
  "Switch controlling which headers are kept in the off-line database.
If true (the default), all headers are kept.
Otherwise, only unseen headers are kept."
  #t
  boolean?)

(define-variable news-group-keep-ignored-headers
  "Switch controlling which headers are kept in the off-line database.
If true, all headers are kept.
Otherwise (the default), ignored headers aren't kept."
  #f
  boolean?)

(define-variable news-group-show-seen-headers
  "Switch controlling whether already-seen headers are shown.
If true, group buffers show all headers.
Otherwise (the default), only unseen headers are shown.
If this switch is true, it's important to set the variable
 news-group-keep-seen-headers, as otherwise there will be a
 serious performance impact."
  #f
  boolean?)

(define-variable news-group-show-context-headers
  "Switch controlling whether a thread's context headers are shown.
If true (the default), previously read headers are shown when they
 are needed to give context for a thread that contains one or more unread
 articles.  This makes it easier to see how a thread has developed.
If false, only the unread headers are fetched from the
 server, and no additional context is available."
  #t
  boolean?)

(define-variable news-group-cache-policy
  "Controls how cacheing is used.
The value of this variable is a list.
The first element of the list describes the groups that are cached:
 SUBSCRIBED            only subscribed groups are cached
 ALL                   all groups are cached
 [list of group names] only named groups are cached
The second element of the list is a list containing zero or more of
the following symbols:
 HEADERS               headers are cached
 BODIES                bodies are cached
The default value of this variable is (SUBSCRIBED (HEADERS BODIES))."
  '(SUBSCRIBED (HEADERS BODIES))
  (lambda (object)
    (and (pair? object)
	 (or (eq? 'SUBSCRIBED (car object))
	     (eq? 'ALL (car object))
	     (list-of-strings? (car object)))
	 (pair? (cdr object))
	 (list-of-type? (cadr object)
	   (lambda (element)
	     (or (eq? 'HEADERS element)
		 (eq? 'BODIES element))))
	 (null? (cddr object)))))

(define-variable news-kept-headers
  "A list of regular expressions matching header fields to display.
Header fields matching these regexps are shown in the given order, and
  other header fields are hidden.
This variable overrides RMAIL-IGNORED-HEADERS; to use RMAIL-IGNORED-HEADERS,
  set NEWS-KEPT-HEADERS to #F."
  '("date:" "from:" "newsgroups:" "subject:")
  (lambda (obj)
    (or (not obj)
        (list-of-type? obj regular-expression?))))

(define (regular-expression? obj)
  (or (string? obj)
      (compiled-regexp? obj)
      ;++ This should be stricter about strings, and it should also
      ;++ support REXPs.
      ))

(define-command rnews
  "Start a News reader.
Normally uses the server specified by the variable news-server,
 but with a prefix arg prompts for the server name.
Only one News reader may be open per server; if a previous News reader
 is open to that server, its buffer is selected."
  "P"
  (lambda (prompt?)
    (select-buffer
     (let ((server (get-news-server-name prompt?)))
       (or (find-news-server-buffer server)
	   (make-news-server-buffer server))))))

(define (get-news-server-name prompt?)
  (let ((server (ref-variable news-server #f)))
    (if (or prompt? (string-null? server))
	(prompt-for-news-server "News server")
	server)))

(define (prompt-for-news-server prompt)
  (let ((default (ref-variable news-server #f)))
    (let ((server
	   (prompt-for-string prompt
			      (and (not (string-null? default))
				   default))))
      (if (string-null? default)
	  (set-variable! news-server server #f))
      server)))

(define-major-mode news-common read-only "News Common"
  "This is an abstract mode to be used for building other modes."
  (lambda (buffer)
    (local-set-variable!
     mode-line-process
     (lambda (window)
       (let ((buffer (news-server-buffer (window-buffer window) #f)))
	 (cond ((not buffer) "")
	       ((nntp-connection:closed?
		 (news-server-buffer:connection buffer))
		": offline")
	       (else ": online"))))
     buffer)
    (event-distributor/invoke! (ref-variable news-common-mode-hook buffer)
			       buffer)))

(define-variable news-common-mode-hook
  "An event distributor that is invoked when entering any News mode."
  (make-event-distributor))

(define-key 'news-common #\a 'news-compose-article)
(define-key 'news-common #\O 'news-toggle-online)
(define-key 'news-common #\q 'news-kill-current-buffer)
(define-key 'news-common #\m 'mail)
(define-key 'news-common #\? 'describe-mode)
(define-key 'news-common '(#\c-x #\c-s) 'news-save-server-data)

(define-command news-kill-current-buffer
  "Kill the current buffer."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (let ((parent (buffer-tree:parent buffer #f)))
	(kill-buffer buffer)
	(if parent (select-buffer parent))))))

(define-command news-save-server-data
  "Update the init file with current data."
  ()
  (lambda ()
    (news-server-buffer:save-groups (current-news-server-buffer #t))))

(define-command news-toggle-online
  "Toggle between online and offline states."
  ()
  (lambda ()
    (let ((connection (buffer-nntp-connection (selected-buffer))))
      (if (nntp-connection:closed? connection)
	  (nntp-connection:reopen connection)
	  (nntp-connection:close connection)))))

(define (buffer-nntp-connection buffer)
  (news-server-buffer:connection (news-server-buffer buffer #t)))

(define (update-nntp-connection-modeline! connection)
  (global-window-modeline-event!
   (lambda (window)
     (let ((buffer (news-server-buffer (window-buffer window) #f)))
       (and buffer
	    (eq? (news-server-buffer:connection buffer) connection)
	    'NNTP-CONNECTION-STATUS)))))

(define (news-buffer-name server prefix)
  (case (ref-variable news-server-name-appearance #f)
    ((HOST-ONLY)
     (string-append prefix
		    ":"
		    (let ((dot (string-index server #\.)))
		      (if dot
			  (string-head server dot)
			  server))))
    ((FULL) (string-append prefix ":" server))
    (else prefix)))

;;;; News-Server Buffer

(define (find-news-server-buffer server)
  (find (lambda (buffer)
	  (and (news-server-buffer? buffer)
	       (string-ci=? (news-server-buffer:server buffer) server)))
	(buffer-list)))

(define (make-news-server-buffer server)
  (create-news-buffer (news-buffer-name server "subscribed-groups")
		      (ref-mode-object news-server)
    (lambda (buffer)
      (add-kill-buffer-hook buffer news-server-buffer:kill)
      (buffer-put! buffer 'NNTP-CONNECTION
		   (make-nntp-connection-1 server buffer))
      (let ((sort? (ref-variable news-sort-groups buffer)))
	(let ((groups
	       (let ((groups
		      (read-groups-init-file
		       (news-server-buffer:connection buffer))))
		 (if sort?
		     (sort groups news-group:<)
		     groups))))
	  (if (ref-variable news-server-initial-refresh buffer)
	      (vector-for-each news-group:update-ranges! groups))
	  (buffer-put! buffer 'NEWS-GROUPS groups)
	  (install-news-groups-buffer-procedures
	   buffer
	   'SERVER
	   news-server-buffer:group-mark
	   news-server-buffer:mark-group
	   news-server-buffer:next-group
	   news-server-buffer:previous-group
	   news-server-buffer:group-adjective
	   news-server-buffer:show-group?)
	  (buffer-put! buffer 'NEWS-GROUPS-SORTED? sort?)
	  (initialize-news-groups-buffer buffer groups)))
      (find-first-property-line buffer 'NEWS-GROUP #f))))

(define (news-server-buffer:kill buffer)
  (for-each kill-buffer (buffer-tree:children buffer))
  (ignore-errors
   (lambda ()
     (news-server-buffer:save-groups buffer)
     (news-server-buffer:close-connection buffer))))

(define (news-server-buffer:groups buffer)
  (buffer-get buffer 'NEWS-GROUPS '#()))

(define (news-server-buffer:groups-sorted? buffer)
  (buffer-get buffer 'NEWS-GROUPS-SORTED? #f))

(define (news-server-buffer? buffer)
  (nntp-connection? (buffer-get buffer 'NNTP-CONNECTION #f)))

(define (news-server-buffer buffer error?)
  (if (news-server-buffer? buffer)
      buffer
      (let ((buffer (buffer-tree:parent buffer error?)))
	(and buffer
	     (news-server-buffer buffer error?)))))

(define (current-news-server-buffer error?)
  (news-server-buffer (selected-buffer) error?))

(define (news-server-buffer:connection buffer)
  (let ((connection (buffer-get buffer 'NNTP-CONNECTION #f)))
    (if (not (nntp-connection? connection))
	(error "Buffer isn't a News server buffer:" (buffer-name buffer)))
    connection))

(define (news-server-buffer:server buffer)
  (nntp-connection:server (news-server-buffer:connection buffer)))

(define (news-server-buffer:close-connection buffer)
  (nntp-connection:close (news-server-buffer:connection buffer)))

(define (make-nntp-connection-1 server buffer)
  (make-nntp-connection
   server
   (let ((entry (assoc server (ref-variable news-server-proxy-alist buffer))))
     (and entry
	  (cdr entry)))
   update-nntp-connection-modeline!))

(define (news-server-buffer:save-groups buffer)
  (write-groups-init-file buffer)
  (vector-for-each
   (lambda (group)
     (write-ignored-subjects-file group
				  (find-news-group-buffer buffer group)))
   (news-server-buffer:groups buffer)))

(define (initialize-news-groups-buffer buffer groups)
  (let ((mark (mark-left-inserting-copy (buffer-start buffer)))
	(server-buffer (news-server-buffer buffer #t)))
    (insert-string (news-groups-buffer:group-adjective buffer) mark)
    (insert-string " newsgroups on news server " mark)
    (insert-string (news-server-buffer:server server-buffer) mark)
    (insert-string ":" mark)
    (insert-newline mark)
    (vector-for-each
     (lambda (group)
       (if (news-groups-buffer:show-group? buffer group)
	   (insert-news-group-line group mark)
	   (set-news-group:index! group #f)))
     groups)
    (mark-temporary! mark)))

(define (news-server-buffer:show-group? buffer group)
  (and (or (ref-variable news-show-unsubscribed-groups buffer)
	   (news-group:subscribed? group))
       (or (ref-variable news-show-nonexistent-groups buffer)
	   (news-group:active? group))))

(define (insert-news-group-line group mark)
  (let ((start (mark-right-inserting-copy mark)))
    (call-with-values
	(lambda ()
	  (if (news-group? group)
	      (values (news-group:subscribed? group)
		      (news-group:articles-marked? group)
		      (news-group:number-of-articles group)
		      (news-group:name group))
	      (values #f #f #f group)))
      (lambda (subscribed? marked? n-articles name)
	(insert-char (if subscribed? #\space #\U) mark)
	(insert-char (if marked? #\M #\space) mark)
	(insert-char #\space mark)
	(insert-string-pad-left (if n-articles (number->string n-articles) "")
				5 #\space mark)
	(insert-string " " mark)
	(insert-string name mark)
	(insert-newline mark)))
    (if (news-server-buffer? (mark-buffer start))
	(begin
	  (region-put! start mark 'NEWS-GROUP group)
	  (set-news-group:index! group (mark-index start))))
    (mark-temporary! start)))

(define (update-news-groups-buffers buffer group)
  (let ((buffer (news-server-buffer buffer #f)))
    (if buffer
	(begin
	  (news-server-buffer:update-group buffer group)
	  (for-each (lambda (child)
		      (let ((update-group
			     (buffer-get child 'UPDATE-NEWS-GROUP #f)))
			(if update-group
			    (update-group child group))))
		    (buffer-tree:children buffer))))))

(define (news-server-buffer:update-group buffer group)
  (let ((del
	 (let ((m (news-server-buffer:group-mark buffer group #f)))
	   (and m
		(mark-left-inserting-copy m))))
	(ins
	 (and (news-server-buffer:show-group? buffer group)
	      (news-server-buffer:find-group buffer (news-group:name group)
		(lambda (i)
		  (mark-left-inserting-copy
		   (let ((groups (news-server-buffer:groups buffer)))
		     (let loop ((i (fix:+ i 1)))
		       (if (fix:= i (vector-length groups))
			   (begin
			     (guarantee-newline (buffer-end buffer))
			     (buffer-end buffer))
			   (or (news-server-buffer:group-mark
				buffer (vector-ref groups i) #f)
			       (loop (fix:+ i 1))))))))
		(lambda (i) i #f)))))
    (if (or ins del)
	(with-buffer-open-1 buffer
	  (lambda ()
	    (let ((col
		   (and del ins
			(let ((point (buffer-point buffer)))
			  (and (mark<= del point)
			       (mark<= point (line-end del 0))
			       (mark-column point))))))
	      (if del (delete-string del (line-start del 1 'LIMIT)))
	      (if ins
		  (let ((m (mark-right-inserting-copy ins)))
		    (insert-news-group-line group ins)
		    (if col
			(set-buffer-point! buffer (move-to-column m col)))
		    (mark-temporary! m))
		  (set-news-group:index! group #f)))
	    (let loop
		((ls
		  (if (or (not ins) (and del (mark< del ins)))
		      del
		      ins)))
	      (let ((group (region-get ls 'NEWS-GROUP #f)))
		(if group
		    (set-news-group:index! group (mark-index ls))))
	      (let ((ls (line-start ls 1 #f)))
		(if ls
		    (loop ls))))
	    (if ins (mark-temporary! ins))
	    (if del (mark-temporary! del))))
	(set-news-group:index! group #f))))

(define (news-server-buffer:add-group buffer group)
  (news-server-buffer:find-group buffer (news-group:name group)
    (lambda (i) i unspecific)
    (lambda (i)
      (buffer-put! buffer 'NEWS-GROUPS
		   (vector-insert (news-server-buffer:groups buffer) i
				  group))))
  (update-news-groups-buffers buffer group))

(define (news-server-buffer:remove-group buffer group)
  (news-server-buffer:find-group buffer (news-group:name group)
    (lambda (i)
      (buffer-put! buffer 'NEWS-GROUPS
		   (vector-delete (news-server-buffer:groups buffer) i)))
    (lambda (i) i unspecific))
  (update-news-groups-buffers buffer group))

(define (install-news-groups-buffer-procedures buffer key group-mark mark-group
					       next-group previous-group
					       group-adjective show-group)
  (buffer-put! buffer 'NEWS-GROUPS-KEY key)
  (buffer-put! buffer 'GROUP-MARK group-mark)
  (buffer-put! buffer 'MARK-GROUP mark-group)
  (buffer-put! buffer 'NEXT-GROUP next-group)
  (buffer-put! buffer 'PREVIOUS-GROUP previous-group)
  (buffer-put! buffer 'GROUP-ADJECTIVE group-adjective)
  (buffer-put! buffer 'SHOW-GROUP show-group))

(define (news-groups-buffer:key buffer)
  (buffer-get buffer 'NEWS-GROUPS-KEY #f))

(define (news-groups-buffer:group-mark buffer group error?)
  ((buffer-get buffer 'GROUP-MARK #f) buffer group error?))

(define (news-groups-buffer:mark-group mark error?)
  (or ((buffer-get (mark-buffer mark) 'MARK-GROUP #f) mark)
      (and error? (not-on-property-line-error "news-group"))))

(define (news-groups-buffer:next-group buffer group)
  ((buffer-get buffer 'NEXT-GROUP #f) buffer group))

(define (news-groups-buffer:previous-group buffer group)
  ((buffer-get buffer 'PREVIOUS-GROUP #f) buffer group))

(define (news-groups-buffer:group-adjective buffer)
  ((buffer-get buffer 'GROUP-ADJECTIVE #f) buffer))

(define (news-groups-buffer:show-group? buffer group)
  ((buffer-get buffer 'SHOW-GROUP #f) buffer group))

(define (news-server-buffer:group-mark buffer group error?)
  (let ((index (news-group:index group)))
    (if index
	(make-mark (buffer-group buffer) index)
	(and error?
	     (error "Buffer has no line for this group:" group buffer)))))

(define (news-server-buffer:mark-group mark)
  (region-get mark 'NEWS-GROUP #f))

(define (news-server-buffer:next-group buffer group)
  (news-server-buffer:find-group buffer (news-group:name group)
    (lambda (i)
      (let ((groups (news-server-buffer:groups buffer)))
	(let loop ((i (fix:+ i 1)))
	  (and (fix:< i (vector-length groups))
	       (let ((group (vector-ref groups i)))
		 (if (news-server-buffer:group-mark buffer group #f)
		     group
		     (loop (fix:+ i 1))))))))
    (lambda (i) i #f)))

(define (news-server-buffer:previous-group buffer group)
  (news-server-buffer:find-group buffer (news-group:name group)
    (lambda (i)
      (let ((groups (news-server-buffer:groups buffer)))
	(let loop ((i (fix:- i 1)))
	  (and (fix:>= i 0)
	       (let ((group (vector-ref groups i)))
		 (if (news-server-buffer:group-mark buffer group #f)
		     group
		     (loop (fix:- i 1))))))))
    (lambda (i) i #f)))

(define (news-server-buffer:find-group buffer name if-found if-not-found)
  (let ((groups (news-server-buffer:groups buffer)))
    (if (news-server-buffer:groups-sorted? buffer)
	(search-ordered-vector groups name news-group:name string-order
			       if-found if-not-found)
	(let ((l (vector-length groups)))
	  (let loop ((i 0))
	    (cond ((fix:= i l)
		   (if-not-found i))
		  ((string=? (news-group:name (vector-ref groups i)) name)
		   (if-found i))
		  (else
		   (loop (fix:+ i 1)))))))))

(define (news-server-buffer:listed-group? buffer group)
  (news-server-buffer:find-group buffer
				 (news-group:name group)
				 (lambda (i) i #t)
				 (lambda (i) i #f)))

(define (news-server-buffer:group-adjective buffer)
  (if (ref-variable news-show-unsubscribed-groups buffer)
      "Selected"
      "Subscribed"))

;;;; News-Server Mode

(define-major-mode news-server news-common "News Server"
  "Major mode for browsing a News server.

Each line shows one of the News groups on the server.  The number near
the left of the line is an estimate of the number of unread messages
available in that group.  A `U' character appearing in the left column
indicates that the group is Unsubscribed.

When a News-server buffer is created, the hooks news-server-mode-hook
and news-common-mode-hook are invoked.

When a News-server buffer is killed, its associated News-group and
All-groups buffers are automatically killed at the same time.

This mode's commands include:

\\[news-read-subscribed-group-headers]	get unread headers for the subscribed groups
\\[news-read-group-headers]	get unread headers for the group indicated by point
\\[news-refresh-groups]	update estimates for the subscribed groups
\\[news-refresh-group]	update estimate for the group indicated by point
\\[news-save-server-data]	write info about the subscribed groups to the init file

\\[news-subscribe-group]	subscribe to the group indicated by point
\\[news-subscribe-group-by-name]	subscribe to a named group
\\[news-unsubscribe-group]	unsubscribe from the group indicated by point
\\[news-unsubscribe-group-backwards]	back up to the previous line and unsubscribe from its group

\\[news-all-groups]	show a list of all of this server's groups
\\[news-new-groups]	show a list of new groups on this server

\\[news-select-group]	browse articles in the group indicated by point
\\[news-compose-article]	post a new article to the group indicated by point
\\[mail]	send a new email message"
  (lambda (buffer)
    (event-distributor/invoke! (ref-variable news-server-mode-hook buffer)
			       buffer)))

(define-variable news-server-mode-hook
  "An event distributor that is invoked when entering News-server mode."
  (make-event-distributor))

(define-key 'news-server #\space 'news-select-group)
(define-key 'news-server #\g 'news-read-subscribed-group-headers)
(define-key 'news-server #\M-g 'news-read-group-headers)
(define-key 'news-server #\G 'news-refresh-groups)
(define-key 'news-server #\M-G 'news-refresh-group)
(define-key 'news-server #\l 'news-all-groups)
(define-key 'news-server #\n 'news-new-groups)
(define-key 'news-server #\r 'news-read-marked-bodies)
(define-key 'news-server #\s 'news-subscribe-group)
(define-key 'news-server #\M-s 'news-subscribe-group-by-name)
(define-key 'news-server #\u 'news-unsubscribe-group)
(define-key 'news-server #\rubout 'news-unsubscribe-group-backwards)

(define (current-news-group)
  (news-groups-buffer:mark-group (current-point) #t))

(define (group-iteration argument procedure)
  (iterate-on-lines (lambda (mark) (news-groups-buffer:mark-group mark #f))
		    "news-group" #f argument
		    (lambda (g) g)
		    news-groups-buffer:next-group
		    news-groups-buffer:previous-group
    (lambda (buffer group next n)
      (if argument
	  (let ((mark (news-groups-buffer:group-mark buffer group #f)))
	    (if mark
		(set-buffer-point! buffer mark))))
      (procedure buffer group)
      (if (and argument (> n 0) next)
	  (let ((mark (news-groups-buffer:group-mark buffer next #f)))
	    (if mark
		(set-buffer-point! buffer mark)))))))

(define-command news-select-group
  "Browse the News group indicated by point.
Select a buffer showing the subject lines of the articles in the News group.
With no argument, show all unread articles in the group.
With \\[universal-argument], show all of the group's articles.
With positive argument N, show the N newest unread articles.
With negative argument -N, show the N oldest unread articles."
  "P"
  (lambda (argument)
    (let ((buffer (current-news-server-buffer #t)))
      (let ((group (current-news-group)))
	(let ((buffer
	       (or (find-news-group-buffer buffer group)
		   (make-news-group-buffer buffer group argument)))
	      (key (news-groups-buffer:key (selected-buffer))))
	  (if (and key (not (buffer-get buffer 'SELECTED-FROM #f)))
	      (buffer-put! buffer 'SELECTED-FROM key))
	  (select-buffer buffer))
	(update-news-groups-buffers buffer group)))))

(define-command news-read-subscribed-group-headers
  "Read the unread headers for all of the subscribed News groups."
  ()
  (lambda ()
    (let ((buffer (current-news-server-buffer #t)))
      (vector-for-each 
       (lambda (group)
	 (if (news-group:subscribed? group)
	     (read-news-group-headers buffer group)
	     (refresh-news-group buffer group))
	 (update-screens! '(IGNORE-INPUT)))
       (news-server-buffer:groups buffer))
      (news-server-buffer:save-groups buffer))))

(define-command news-read-group-headers
  "Read the unread headers for the News group indicated by point.
With prefix argument, updates the next several News groups."
  "P"
  (lambda (argument)
    (group-iteration argument read-news-group-headers)))

(define (read-news-group-headers buffer group)
  (news-group:get-unread-headers group buffer)
  (update-news-groups-buffers buffer group)
  (write-ignored-subjects-file group buffer)
  (write-groups-init-file buffer))

(define-command news-refresh-groups
  "Update the unread-message estimates for all of the News groups shown.
This command has no effect in the all-groups buffer."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (if (news-server-buffer? buffer)
	  (begin
	    (vector-for-each
	     (lambda (group)
	       (refresh-news-group buffer group)
	       (update-screens! '(IGNORE-INPUT)))
	     (news-server-buffer:groups buffer))
	    (news-server-buffer:save-groups buffer))))))

(define-command news-refresh-group
  "Update the unread-message estimate for the News group indicated by point.
With prefix argument, updates the next several News groups."
  "P"
  (lambda (argument)
    (group-iteration argument refresh-news-group)))

(define (refresh-news-group buffer group)
  (let ((msg
	 (string-append "Refreshing news group "
			(news-group:name group)
			"... ")))
    (message msg)
    (news-group:update-ranges! group)
    (update-news-groups-buffers buffer group)
    (message msg "done")))

(define-command news-clear-read-messages
  "Clear the read-messages list for the News group indicated by point.
With prefix argument, clears the list for the next several News groups."
  "P"
  (lambda (argument)
    (group-iteration argument
      (lambda (buffer group)
	(set-news-group:ranges-deleted! group '())
	(update-news-groups-buffers buffer group)))))

(define-command news-subscribe-group
  "Subscribe to the News group indicated by point.
With prefix argument, subscribes to the next several News groups."
  "P"
  (lambda (argument)
    (group-iteration argument subscribe-news-group)))

(define (subscribe-news-group buffer group)
  (set-news-group:subscribed?! group #t)
  (news-server-buffer:add-group (news-server-buffer buffer #t) group))

(define-command news-subscribe-group-by-name
  "Subscribe to a News group by name.
Prompts for the News-group name, with completion."
  ()
  (lambda ()
    (let ((buffer (current-news-server-buffer #t)))
      (subscribe-news-group
       buffer
       (prompt-for-active-news-group "Subscribe to news group"
				     #f
				     buffer)))))

(define (prompt-for-active-news-group prompt default server-buffer)
  (let ((connection (news-server-buffer:connection server-buffer)))
    (let ((group-names
	   (lambda () (nntp-connection:active-groups connection #f)))
	  (string->group
	   (lambda (string)
	     (let ((group (find-active-news-group connection string)))
	       (if group (news-group:apply-cache-policy group))
	       group))))
      (string->group
       (let ((convert
	      (lambda (vector) (map news-group:name (vector->list vector)))))
	 (prompt-for-completed-string prompt default
	   (lambda (string if-unique if-not-unique if-not-found)
	     (ordered-vector-minimum-match (group-names) string (lambda (s) s)
					   string-order (prefix-matcher string)
	       if-unique
	       (lambda (name gcm all-matches)
		 (if-not-unique (string-head name gcm)
				(lambda () (convert (all-matches)))))
	       if-not-found))
	   (lambda (string)
	     (convert
	      (ordered-vector-matches (group-names) string (lambda (s) s)
				      string-order (prefix-matcher string))))
	   string->group
	   'REQUIRE-MATCH? #t))))))

(define-command news-unsubscribe-group
  "Unsubscribe from the News group indicated by point.
With prefix argument, unsubscribes from the next several News groups."
  "P"
  (lambda (argument)
    (group-iteration argument unsubscribe-news-group)))

(define-command news-unsubscribe-group-backwards
  "Back up to the previous News group and unsubscribe from it.
With prefix argument, unsubscribes from the previous several News groups."
  "p"
  (lambda (argument)
    (group-iteration (- argument) unsubscribe-news-group)))

(define (unsubscribe-news-group buffer group)
  (news-group:purge-pre-read-headers group 'ALL)
  (news-group:close-database group)
  (set-news-group:subscribed?! group #f)
  (update-news-groups-buffers buffer group))

;;;; All-Groups Buffer

(define-command news-all-groups
  "Select a buffer showing all of the News groups on this server.
This buffer shows subscribed and unsubscribed groups, and is useful
 for choosing new groups to subscribe to.
Normally, the News groups list is saved in a local file, so that
 subsequent references to the list do not require interacting with the
 server.
With prefix argument, the saved list is discarded and a new list is
 obtained from the server."
  "P"
  (lambda (argument)
    (select-buffer
     (let ((server-buffer (current-news-server-buffer #t)))
       (or (buffer-tree:child server-buffer 'ALL-NEWS-GROUPS #f)
	   (make-ang-buffer server-buffer
			    (nntp-connection:active-groups
			     (news-server-buffer:connection server-buffer)
			     argument)
			    "all"
			    'ALL-NEWS-GROUPS))))))

(define (all-news-groups-buffer? buffer)
  (let ((server-buffer (news-server-buffer buffer #f)))
    (and server-buffer
	 (eq? buffer (buffer-tree:child server-buffer 'ALL-NEWS-GROUPS #f)))))

(define-command news-new-groups
  "Select a buffer showing new News groups on this server.
This shows News groups that have been created since the last time that
 the News-groups list was examined."
  ()
  (lambda ()
    (let ((server-buffer (current-news-server-buffer #t)))
      (let ((buffer (buffer-tree:child server-buffer 'NEW-NEWS-GROUPS #f)))
	(if buffer
	    (select-buffer buffer)
	    (let ((new-groups
		   (nntp-connection:new-groups
		    (news-server-buffer:connection server-buffer))))
	      (if (= (vector-length new-groups) 0)
		  (message "No new News groups since previous check")
		  (let ((all-groups-buffer
			 (buffer-tree:child server-buffer 'ALL-NEWS-GROUPS
					    #f)))
		    (if all-groups-buffer
			(vector-for-each
			 (lambda (name)
			   (ang-buffer:insert-group-line all-groups-buffer
							 name))
			 new-groups))
		    (select-buffer
		     (make-ang-buffer server-buffer
				      new-groups
				      "new"
				      'NEW-NEWS-GROUPS))))))))))

(define (new-news-groups-buffer? buffer)
  (let ((server-buffer (news-server-buffer buffer #f)))
    (and server-buffer
	 (eq? buffer (buffer-tree:child server-buffer 'NEW-NEWS-GROUPS #f)))))

(define (make-ang-buffer server-buffer group-names prefix keyword)
  (let ((name (string-append prefix "-groups")))
    (create-news-buffer
     (news-buffer-name (news-server-buffer:server server-buffer) name)
     (ref-mode-object news-server)
     (lambda (buffer)
       (buffer-tree:attach-child! server-buffer keyword buffer)
       (add-kill-buffer-hook buffer ang-buffer:kill)
       (buffer-put! buffer 'UPDATE-NEWS-GROUP ang-buffer:update-group)
       (install-news-groups-buffer-procedures
	buffer
	keyword
	ang-buffer:group-mark
	ang-buffer:mark-group
	ang-buffer:next-group
	ang-buffer:previous-group
	(lambda (buffer) buffer (string-titlecase prefix))
	(lambda (buffer group) buffer group #t))
       (let ((msg (string-append "Building " name " buffer... ")))
	 (message msg)
	 (initialize-news-groups-buffer
	  buffer
	  (vector-map (lambda (name) (name->news-group buffer name))
		      group-names))
	 (message msg "done"))
       (find-first-line buffer ang-buffer:mark-group-name)))))

(define (ang-buffer:kill buffer)
  (ignore-errors
   (lambda ()
     (let ((buffer (news-server-buffer buffer #f)))
       (if buffer
	   (nntp-connection:purge-group-cache
	    (news-server-buffer:connection buffer)
	    (lambda (group)
	      (not (news-server-buffer:listed-group? buffer group)))))))))

(define (ang-buffer:update-group buffer group)
  (let ((mark (ang-buffer:group-mark buffer group #f)))
    (if mark
	(ang-buffer:replace-group-line buffer group mark))))

(define (ang-buffer:insert-group-line buffer name)
  (let ((group (name->news-group buffer name)))
    (ang-buffer:find-line buffer name
			  (lambda (ls)
			    (ang-buffer:replace-group-line buffer group ls))
			  (lambda (ls)
			    (insert-news-group-line group ls)))))

(define (ang-buffer:replace-group-line buffer group ls)
  (with-buffer-open-1 buffer
    (lambda ()
      (let ((ls (mark-right-inserting-copy ls))
	    (col
	     (let ((point (buffer-point buffer)))
	       (and (mark<= ls point)
		    (mark<= point (line-end ls 0))
		    (mark-column point)))))
	(delete-string ls (line-start ls 1 'LIMIT))
	(let ((ls (mark-left-inserting-copy ls)))
	  (insert-news-group-line group ls)
	  (mark-temporary! ls))
	(if col (set-buffer-point! buffer (move-to-column ls col)))
	(mark-temporary! ls)))))

(define (name->news-group buffer name)
  (let ((connection
	 (let ((buffer (news-server-buffer buffer #f)))
	   (and buffer
		(news-server-buffer:connection buffer)))))
    (or (and connection
	     (find-news-group connection name))
	name)))

(define (ang-buffer:group-mark buffer group error?)
  (ang-buffer:find-line buffer
			(news-group:name group)
			(lambda (ls) ls)
			(lambda (ls)
			  ls
			  (and error?
			       (error "Buffer has no line for this group:"
				      group buffer)))))

(define (ang-buffer:find-line buffer name if-found if-not-found)
  (find-buffer-line buffer
		    ang-buffer:mark-group-name
		    (lambda (name*) (string-order name name*))
		    if-found
		    if-not-found))

(define (ang-buffer:next-group buffer group)
  (let ((m (ang-buffer:group-mark buffer group #f)))
    (and m
	 (let ((m (line-start m 1 #f)))
	   (and m
		(ang-buffer:mark-group m))))))

(define (ang-buffer:previous-group buffer group)
  (let ((m (ang-buffer:group-mark buffer group #f)))
    (and m
	 (let ((m (line-start m -1 #f)))
	   (and m
		(ang-buffer:mark-group m))))))

(define (ang-buffer:mark-group mark)
  (let ((name (ang-buffer:mark-group-name mark)))
    (and name
	 (let ((connection (buffer-nntp-connection (mark-buffer mark))))
	   (or (find-news-group connection name)
	       (make-news-group-1 connection name #f #f '() '() '()))))))

(define (ang-buffer:mark-group-name mark)
  (and (re-match-forward
	"^[ U][ M] [ 0-9][ 0-9][ 0-9][ 0-9][ 0-9] \\([^ ]+\\)$"
	(line-start mark 0)
	(line-end mark 0)
	#f)
       (extract-string (re-match-start 1) (re-match-end 1))))

;;;; News-Group Buffer

(define (find-news-group-buffer server-buffer group)
  (buffer-tree:child server-buffer group #f))

(define (make-news-group-buffer server-buffer group argument)
  (create-news-buffer (news-group-buffer-name group)
		      (ref-mode-object news-group)
    (lambda (buffer)
      (buffer-put! buffer 'NEWS-GROUP group)
      (buffer-tree:attach-child! server-buffer group buffer)
      (add-kill-buffer-hook buffer news-group-buffer:kill)
      (add-select-buffer-hook buffer news-group-buffer:select)
      (initialize-news-group-buffer buffer argument)
      (let ((ls (find-first-property-line buffer 'NEWS-HEADER #f)))
	(and ls
	     (let ((header
		    (let ((header (region-get ls 'NEWS-HEADER #f)))
		      (and (news-header:article-deleted? header)
			   (news-group-buffer:next-header
			    buffer header news-header:unread?)))))
	       (if header
		   (if (news-header:pre-read-body? header)
		       (news-group-buffer:header-mark-1 buffer header)
		       (or (news-group-buffer:header-mark buffer header)
			   (news-group-buffer:thread-start-mark
			    buffer (news-header:thread header)) ls))
		   ls)))))))

(define (news-group-buffer-name group)
  (news-buffer-name (news-group:server group) (news-group:name group)))

(define (news-group-buffer? buffer)
  (news-group? (buffer-get buffer 'NEWS-GROUP #f)))

(define (news-group-buffer:group buffer)
  (let ((group (buffer-get buffer 'NEWS-GROUP #f)))
    (if (not (news-group? group))
	(error:wrong-type-argument buffer "News-group buffer"
				   'NEWS-GROUP-BUFFER:GROUP))
    group))

(define (news-group-buffer buffer error?)
  (if (news-group-buffer? buffer)
      buffer
      (let ((buffer (buffer-tree:parent buffer error?)))
	(and buffer
	     (news-group-buffer buffer error?)))))

(define (news-group-buffer:kill buffer)
  (ignore-errors
   (lambda ()
     (let ((group (news-group-buffer:group buffer)))
       (update-news-groups-buffers buffer group)
       (write-ignored-subjects-file group buffer)
       (if (and (selected-buffer? buffer)
		(eq? (buffer-get buffer 'SELECTED-FROM #f) 'SERVER))
	   (let ((buffer (news-server-buffer buffer #t)))
	     (if (eq? group (region-get (buffer-point buffer) 'NEWS-GROUP #f))
		 (let loop ((group group))
		   (let ((next (news-server-buffer:next-group buffer group)))
		     (if next
			 (let ((n (news-group:number-of-articles next)))
			   (if (and n (> n 0))
			       (let ((ls
				      (news-server-buffer:group-mark
				       buffer next #f)))
				 (if ls
				     (set-buffer-point! buffer ls)))
			       (loop next)))))))))
       (news-group:purge-and-compact-headers! group buffer)
       (set-news-group:ignored-subjects! group 'UNKNOWN)
       (write-groups-init-file buffer)))))

(define (news-group-buffer:select group-buffer window)
  (news-group-buffer:delete-context-window group-buffer window))

(define (initialize-news-group-buffer buffer argument)
  (let ((group (news-group-buffer:group buffer)))
    (let ((mark (mark-left-inserting-copy (buffer-end buffer)))
	  (threads (news-group:get-threads group argument buffer)))
      (vector-for-each
       (let ((expanded?
	      (not (ref-variable news-initially-collapse-threads buffer))))
	 (lambda (thread)
	   (set-news-thread:expanded?! thread expanded?)))
       threads)
      (buffer-put! buffer 'NEWS-THREADS threads)
      (insert-string "Messages in news group " mark)
      (insert-string (news-group:name group) mark)
      (insert-string " on server " mark)
      (insert-string (news-group:server group) mark)
      (insert-string ":" mark)
      (insert-newline mark)
      (vector-for-each (lambda (thread)
			 (insert-news-thread-lines thread mark))
		       threads)
      (mark-temporary! mark))
    (update-news-groups-buffers buffer group)
    (news-group:close-database group)))

(define (news-group-buffer:collapse-thread buffer thread)
  (if (news-thread:expanded? thread)
      (news-group-buffer:adjust-thread-display buffer thread #f)))

(define (news-group-buffer:expand-thread buffer thread)
  (if (not (news-thread:expanded? thread))
      (news-group-buffer:adjust-thread-display buffer thread #t)))

(define (news-group-buffer:auto-expand-thread buffer thread)
  (if (not (news-thread:expanded? thread))
      (news-group-buffer:adjust-thread-display buffer thread 'AUTOMATIC)))

(define (news-group-buffer:adjust-thread-display buffer thread expanded?)
  (with-buffer-open-1 buffer
    (lambda ()
      (let ((ls
	     (mark-left-inserting-copy
	      (or (delete-news-thread-lines buffer thread)
		  (let loop ((thread thread))
		    (let ((next (news-group-buffer:next-thread buffer thread)))
		      (if next
			  (or (news-group-buffer:thread-start-mark buffer next)
			      (loop next))
			  (begin
			    (guarantee-newline (buffer-end buffer))
			    (buffer-end buffer)))))))))
	(set-news-thread:expanded?! thread expanded?)
	(insert-news-thread-lines thread ls)
	(mark-temporary! ls)
	(update-subsequent-news-header-lines ls)))))

(define (delete-news-thread-lines buffer thread)
  (let ((region (news-thread-lines-region buffer thread)))
    (and region
	 (let ((start (mark-right-inserting-copy (region-start region))))
	   (news-thread:clear-indices! thread)
	   (delete-string start (region-end region))
	   (mark-temporary! start)
	   start))))

(define (news-thread-lines-region buffer thread)
  (let ((ls (news-group-buffer:thread-start-mark buffer thread)))
    (and ls
	 (let ((start (mark-temporary-copy ls))
	       (end (mark-temporary-copy (line-start ls 1 'LIMIT))))
	   (news-thread:for-each-header thread
	     (lambda (header)
	       (let ((ls (news-group-buffer:header-mark buffer header)))
		 (if ls
		     (let ((nls (line-start ls 1 'LIMIT)))
		       (if (mark< ls start) (move-mark-to! start ls))
		       (if (mark> nls end) (move-mark-to! end nls)))))))
	   (make-region start end)))))

(define (insert-news-thread-lines thread mark)
  (if (news-thread:show-collapsed? thread)
      (insert-collapsed-news-thread-line thread mark)
      (insert-expanded-news-thread-lines thread mark))
  (news-thread:for-each-real-header thread
    (let ((buffer (mark-buffer mark)))
      (lambda (header)
	(news-header:article-browsed! header buffer)))))

(define (insert-expanded-news-thread-lines thread mark)
  (let ((subject
	 (news-header:subject
	  (news-thread:first-header thread news-header:real?))))
    (let loop ((header (news-thread:root thread)) (indentation 0))
      (if (news-header:real? header)
	  (let* ((subject* (news-header:subject header)))
	    (let ((comparison
		   (and (> indentation 0)
			(compare-subjects
			 (canonicalize-subject subject)
			 (canonicalize-subject subject*)))))
	      (insert-news-header-line header
				       indentation
				       (and (not comparison) subject*)
				       mark)
	      (if (or (not comparison)
		      ;; OK to lengthen prefix, but don't shorten.
		      (eq? 'LEFT-PREFIX comparison))
		  (set! subject subject*))))
	  (insert-dummy-header-line header indentation
				    (and (= indentation 0) subject)
				    mark))
      (for-each (let ((indentation (+ indentation 4)))
		  (lambda (header)
		    (loop header indentation)))
		(news-header:followups header)))))

(define (insert-collapsed-news-thread-line thread mark)
  (news-thread:for-each-header thread
    (lambda (header)
      (set-news-header:index! header #f)))
  (let ((header (news-thread:first-header thread news-header:real?)))
    (insert-subject-line
     (news-thread:status thread)
     (news-thread:pre-read-bodies thread)
     (lambda (mark width)
       (insert-char #\+ mark)
       (insert-string-pad-left
	(string-append
	 (number->string
	  (- (news-thread:n-articles thread news-header:real?) 1))
	 ">")
	(- width 1)
	#\space
	mark))
     0
     (news-header:subject header)
     (news-header:from header)
     header
     mark)))

(define (update-subsequent-news-header-lines ls)
  (let ((header (region-get ls 'NEWS-HEADER #f)))
    (if header
	(set-news-header:index! header (mark-index ls))))
  (let ((ls (line-start ls 1 #f)))
    (if ls
	(update-subsequent-news-header-lines ls))))

(define (insert-news-header-line header indentation subject mark)
  (insert-subject-line (news-header:status header)
		       (news-header:pre-read-body? header)
		       (news-header:n-lines header)
		       indentation
		       subject
		       (news-header:from header)
		       header
		       mark))

(define (insert-dummy-header-line header indentation subject mark)
  (insert-subject-line #\space #f "" indentation subject #f header mark))

(define (insert-subject-line status b? n indentation subject from header mark)
  (let ((start (mark-right-inserting-copy mark)))
    (insert-char status mark)
    (insert-char (case b?
		   ((#f) #\space)
		   ((SOME) #\b)
		   (else #\B))
		 mark)
    (if (string? n)
	(begin
	  (insert-char #\space mark)
	  (insert-string-pad-left n 4 #\space mark)
	  (insert-char #\space mark))
	(n mark 6))
    (insert-char #\space mark)
    (insert-chars #\space indentation mark)
    (if subject
	(let ((subject-column (mark-column mark))
	      (subject/author-spacing 5)
	      (author-columns (ref-variable news-group-author-columns mark))
	      (x-size (mark-x-size mark)))
	  (let ((subject-length
		 (max (- (- x-size 1)
			 (+ subject-column
			    subject/author-spacing
			    author-columns))
		      10)))
	    (insert-string (if (> (string-length subject) subject-length)
			       (string-head subject subject-length)
			       subject)
			   mark)
	    (if from
		(let ((delta
		       (- (+ subject-column
			     subject-length
			     subject/author-spacing)
			  (mark-column mark))))
		  (if (> delta 0)
		      (insert-chars #\space delta mark)))))))
    (if (or from (not subject))
	(begin
	  (insert-string "(" mark)
	  (insert-string (if from (compose-author-string from mark) "...")
			 mark)
	  (insert-char #\) mark)))
    (insert-newline mark)
    (region-put! start mark 'NEWS-HEADER header)
    (news-group-buffer:maybe-highlight-header header start)
    (set-news-header:index! header (mark-index start))
    (mark-temporary! start)))

(define (compose-author-string from mark)
  (let ((r
	 (and (ref-variable news-group-show-author-name mark)
	      (or (re-string-match "^\"\\(.+\\)\"[ \t]+<.+>$" from)
		  (re-string-match "^\\(.+\\)<.+>$" from)
		  (re-string-match "^[^ \t]+[ \t]+(\\(.+\\))$" from)))))
    (if r
	(string-trim (substring from
				(re-match-start-index 1 r)
				(re-match-end-index 1 r)))
	(or (rfc822:first-address from) from))))

(define (news-group-buffer:header-mark buffer header)
  (let ((index (news-header:index header)))
    (and index
	 (make-mark (buffer-group buffer) index))))

(define (news-group-buffer:thread-start-mark buffer thread)
  (let ((header
	 (news-thread:first-header thread
	   (lambda (header)
	     (news-group-buffer:header-mark buffer header)))))
    (and header
	 (news-group-buffer:header-mark buffer header))))

(define (update-buffer-news-header-status buffer header)
  (let ((mark (news-group-buffer:header-mark buffer header))
	(thread (news-header:thread header)))
    (if (and mark (not (news-thread:show-collapsed? thread)))
	(%update-buffer-news-header-status buffer mark
					   (news-header:status header)
					   (news-header:pre-read-body? header))
	(update-buffer-news-thread-status buffer thread))))

(define (update-buffer-news-thread-status buffer thread)
  (let ((header (news-thread:first-header thread news-header:real?)))
    (let ((mark (news-group-buffer:header-mark buffer header)))
      (if mark
	  (%update-buffer-news-header-status
	   buffer mark
	   (news-thread:status thread)
	   (if (news-thread:show-collapsed? thread)
	       (news-thread:pre-read-bodies thread)
	       (news-header:pre-read-body? header)))))))

(define (%update-buffer-news-header-status buffer mark status body?)
  (with-buffer-open-1 buffer
    (lambda ()
      (let ((mark (mark-right-inserting-copy mark))
	    (header (region-get mark 'NEWS-HEADER #f)))
	(let ((preserve-point? (mark= (buffer-point buffer) mark)))
	  (delete-right-char mark)
	  (delete-right-char mark)
	  (insert-char (case body?
			 ((#f) #\space)
			 ((SOME) #\b)
			 (else #\B))
		       mark)
	  (insert-char status mark)
	  ;; Grumble: must rewrite 'NEWS-HEADER property because
	  ;; inserted characters have no properties.
	  (region-put! mark (mark+ mark 2) 'NEWS-HEADER header)
	  (news-group-buffer:maybe-highlight-header header mark)
	  (if preserve-point? (set-buffer-point! buffer mark)))
	(mark-temporary! mark)))))

(define (news-group-buffer:maybe-highlight-header header mark)
  (highlight-region
   (make-region (mark+ mark 2) (mark+ mark 6))
   (if (and (ref-variable news-article-highlight-selected mark)
	    (find-news-article-buffer (mark-buffer mark)
				      header))
       (highlight-face)
       (default-face))))

(define (news-group-buffer:move-to-header buffer header)
  (let ((point (news-group-buffer:header-mark-1 buffer header))
	(header* (region-get (buffer-point buffer) 'NEWS-HEADER #f)))
    (if (not (eq? header header*))
	(begin
	  (with-editor-interrupts-disabled
	    (lambda ()
	      (set-buffer-point! buffer point)
	      (news-group-buffer:maybe-update-context-window buffer point)))
	  (let ((flag
		 (ref-variable news-automatically-collapse-threads buffer)))
	    (if (and header* (not (eq? flag 'NEVER)))
		(let ((thread (news-header:thread header*)))
		  (if (and (not (eq? thread (news-header:thread header)))
			   (let ((expanded? (news-thread:expanded? thread)))
			     (and expanded?
				  (or (eq? expanded? 'AUTOMATIC)
				      (eq? flag 'ALWAYS)))))
		      (news-group-buffer:collapse-thread buffer thread)))))))))

(define (news-group-buffer:header-mark-1 buffer header)
  (or (news-group-buffer:header-mark buffer header)
      (begin
	(news-group-buffer:auto-expand-thread buffer
					      (news-header:thread header))
	(news-group-buffer:header-mark buffer header))
      (error "News header invisible after thread expansion:" header)))

(define (news-group-buffer:threads buffer)
  (buffer-get buffer 'NEWS-THREADS '#()))

(define (news-group-buffer:next-thread buffer thread)
  (let ((threads (news-group-buffer:threads buffer)))
    (let ((index (find-thread-index threads thread)))
      (and index
	   (fix:< (fix:+ index 1) (vector-length threads))
	   (vector-ref threads (fix:+ index 1))))))

(define (news-group-buffer:previous-thread buffer thread)
  (let ((threads (news-group-buffer:threads buffer)))
    (let ((index (find-thread-index threads thread)))
      (and index
	   (fix:> index 0)
	   (vector-ref threads (fix:- index 1))))))

(define (find-thread-index threads thread)
  (search-ordered-vector threads thread (lambda (t) t)
			 (lambda (t1 t2)
			   (cond ((news-thread:< t1 t2) 'LESS)
				 ((news-thread:< t2 t1) 'GREATER)
				 (else 'EQUAL)))
			 (lambda (i) i)
			 (lambda (i) i #f)))

(define (news-group-buffer:next-header buffer header predicate)
  (or (news-thread:next-header header predicate)
      (news-group-buffer:first-header-in-next-thread buffer header predicate)))

(define (news-group-buffer:previous-header buffer header predicate)
  (or (news-thread:previous-header header predicate)
      (news-group-buffer:last-header-in-previous-thread buffer header
							predicate)))

(define (news-group-buffer:first-header-in-next-thread buffer header predicate)
  (let loop ((thread (news-header:thread header)))
    (let ((thread (news-group-buffer:next-thread buffer thread)))
      (and thread
	   (or (news-thread:first-header thread predicate)
	       (loop thread))))))

(define (news-group-buffer:last-header-in-previous-thread buffer header
							  predicate)
  (let loop ((thread (news-header:thread header)))
    (let ((thread (news-group-buffer:previous-thread buffer thread)))
      (and thread
	   (or (news-thread:last-header thread predicate)
	       (loop thread))))))

;;;; Article Context Window

(define (show-news-article-context article-window context-lines)
  (with-editor-interrupts-disabled
    (lambda ()
      (let ((context-window
	     (window-split-vertically! article-window
				       (- (window-y-size article-window)
					  context-lines)))
	    (group-buffer
	     (buffer-tree:parent (window-buffer article-window) #t)))
	(buffer-put! group-buffer 'CONTEXT-WINDOW
		     (weak-cons context-window #f))
	(select-buffer group-buffer context-window)
	(center-news-article-context context-window)))))

(define (news-group-buffer:delete-context-window group-buffer window)
  (let ((context-window (news-group-buffer:context-window group-buffer #t)))
    (if (and context-window (not (eq? window context-window)))
	(with-editor-interrupts-disabled
	  (lambda ()
	    (window-delete! context-window window)
	    (buffer-remove! group-buffer 'CONTEXT-WINDOW))))))

(define (news-group-buffer:maybe-update-context-window group-buffer mark)
  (let ((context-window (news-group-buffer:context-window group-buffer #t)))
    (if context-window
	(begin
	  (set-window-point! context-window mark)
	  (center-news-article-context context-window)))))

(define (center-news-article-context context-window)
  (window-scroll-y-absolute! context-window
			     (integer-floor (window-y-size context-window) 2)))

(define (news-group-buffer:context-window buffer require-buffer?)
  (let ((pair (buffer-get buffer 'CONTEXT-WINDOW #f)))
    (and pair
	 (let ((window
		(let ((window (weak-car pair)))
		  (and window
		       (window-visible? window)
		       (or (not require-buffer?)
			   (eq? buffer (window-buffer window)))
		       window))))
	   (if (not window)
	       (buffer-remove! buffer 'CONTEXT-WINDOW))
	   window))))

;;;; News-Group Mode

(define-major-mode news-group news-common "News Group"
  "Major mode for browsing subjects of articles in a News group.

Each line shows one of the articles in the group.  A `D' in the left
column indicates that the article has either been read or marked as
such.  The right-hand side of the line shows the subject line from the
article, followed by the author's name in parenthesis.

Articles are grouped into conversational `threads' where possible.
Such threads can be `expanded', such that all of the articles
composing the thread are shown, or `collapsed', in which only the
first article of the thread is shown.

When expanded, the subjects of followup articles are suppressed, and
the parenthesized author's name appears indented.  The indentation
shows the structure of the conversation, with follow-ups being
indented a bit more than the articles they follow-up to.  The number
appearing to the left of the column is an estimate of the number of
lines in the message; if blank it means that the associated article is
no longer available from the server.

When collapsed, a thread is represented by a single line, which shows
the subject and author of the first message in the thread.  The second
column contains a `+' character, followed by the number of articles in
the thread in addition to the one that is shown.  Moving into the
thread's other articles will cause the thread to expand automatically.

The variable news-initially-collapse-threads controls whether threads
are initially collapsed or expanded.

The variable news-automatically-collapse-threads controls whether the
thread will collapse again when it is left.

A collapsed thread's status is shown by the character in the left
column.  A space indicates that all of the articles in the thread are
unread, a `D' that all of the articles are read, and a `d' that the
thread contains both read and unread articles.  Similarly, an `I'
indicates that all of the thread's articles have been ignored, and an
`i' that only some of them have been ignored.

The variable news-group-author-columns can be used to control the
appearance of header lines.

When a News-group buffer is created, the hooks news-group-mode-hook
and news-common-mode-hook are invoked.

This mode's commands include:

\\[news-select-article]	select a buffer containing the article indicated by point
\\[news-compose-article]	post a new article to this group
\\[mail]	send a new email message

\\[news-delete-article]	mark the article indicated by point as read
\\[news-delete-thread]	mark the whole thread as read
\\[news-mark-article]	mark the article indicated by point for retrieval
\\[news-mark-thread]	mark the whole thread for retrieval
\\[news-ignore-thread]	ignore the thread indicated by point
\\[news-unmark-article]	unmark the article indicated by point
\\[news-unmark-thread]	unmark the whole thread
\\[news-unmark-article-backwards]	move to prev line and unmark article

\\[news-group-next-unread-header]	move down to the next unread article
\\[news-group-next-unread-article]	move down to the next unread article and select it
\\[news-group-next-thread]	move down to the next unread thread
\\[news-group-next-thread-article]	move down to the next unread thread and select it
\\[news-group-previous-unread-header]	move up to the previous unread article
\\[news-group-previous-unread-article]	move up to the previous unread article and select it
\\[news-group-previous-thread]	move up to the previous unread thread
\\[news-group-previous-thread-article]	move up to the previous unread thread and select it

\\[news-toggle-thread]	toggle current thread between collapsed and expanded
\\[news-collapse-threads]	collapse all threads
\\[news-expand-threads]	expand all threads

\\[news-output-article]	output this article to a mail file
\\[news-output-article-to-rmail-file]	output this article to an RMAIL file

\\[news-catch-up-group]	mark all articles as read and return to news-groups buffer
\\[news-expunge-group]	remove marked threads from the article list
\\[news-revert-group]	refresh the article list from the news server
\\[news-save-server-data]	write info about the subscribed groups to the init file

\\[news-group-show-header]	show the header of the article indicated by point
\\[news-group-show-subject]	show the subject of the article indicated by point"
  (lambda (buffer)
    (local-set-variable! truncate-lines #t buffer)
    (event-distributor/invoke! (ref-variable news-group-mode-hook buffer)
			       buffer)))

(define-variable news-group-mode-hook
  "An event distributor that is invoked when entering News-group mode."
  (make-event-distributor))

(define-key 'news-group #\space 'news-select-article)
(define-key 'news-group #\c 'news-catch-up-group)
(define-key 'news-group #\M-c 'news-collapse-threads)
(define-key 'news-group #\d 'news-delete-article)
(define-key 'news-group #\M-d 'news-delete-thread)
(define-key 'news-group #\M-e 'news-expand-threads)
(define-key 'news-group #\g 'news-revert-group)
(define-key 'news-group #\h 'news-group-show-header)
(define-key 'news-group #\i 'news-ignore-thread)
(define-key 'news-group #\m 'news-mark-article)
(define-key 'news-group #\M-m 'news-mark-thread)
(define-key 'news-group #\n 'news-group-next-unread-header)
(define-key 'news-group #\N 'news-group-next-unread-article)
(define-key 'news-group #\M-n 'news-group-next-thread)
(define-key 'news-group #\M-N 'news-group-next-thread-article)
(define-key 'news-group #\o 'news-output-article-to-rmail-file)
(define-key 'news-group #\c-o 'news-output-article)
(define-key 'news-group #\p 'news-group-previous-unread-header)
(define-key 'news-group #\P 'news-group-previous-unread-article)
(define-key 'news-group #\M-p 'news-group-previous-thread)
(define-key 'news-group #\M-P 'news-group-previous-thread-article)
(define-key 'news-group #\q 'news-group-quit)
(define-key 'news-group #\r 'news-read-marked-bodies)
(define-key 'news-group #\s 'news-group-show-subject)
(define-key 'news-group #\t 'news-toggle-thread)
(define-key 'news-group #\u 'news-unmark-article)
(define-key 'news-group #\M-u 'news-unmark-thread)
(define-key 'news-group #\x 'news-expunge-group)
(define-key 'news-group #\rubout 'news-unmark-article-backwards)

(define (current-news-header)
  (let ((header (region-get (current-point) 'NEWS-HEADER #f)))
    (if (not header)
	(not-on-property-line-error "news-article header"))
    header))

(define-command news-group-next-unread-header
  "Move down to the next unread article header.
With prefix argument, moves down several headers."
  "p"
  (lambda (n)
    (let ((b (selected-buffer))
	  (m (current-point)))
      (define (next-loop h n)
	(if (= n 0)
	    (win h)
	    (let ((next
		   (news-group-buffer:next-header b h news-header:unread?)))
	      (if next
		  (next-loop next (- n 1))
		  (partial-win h n)))))

      (define (prev-loop h n)
	(if (= n 0)
	    (win h)
	    (let ((next
		   (news-group-buffer:previous-header b h
						      news-header:unread?)))
	      (if next
		  (prev-loop next (+ n 1))
		  (partial-win h n)))))

      (define (win h)
	(news-group-buffer:move-to-header b h)
	#t)

      (define (partial-win h n*)
	(if (not (= n n*)) (news-group-buffer:move-to-header b h))
	(lose))

      (define (lose)
	(editor-failure)
	#f)

      (cond ((> n 0)
	     (cond ((region-get m 'NEWS-HEADER #f)
		    => (lambda (h) (next-loop h n)))
		   ((find-next-line-property m 'NEWS-HEADER #f)
		    => (lambda (h)
			 (next-loop h (if (news-header:unread? h) (- n 1) n))))
		   (else (lose))))
	    ((< n 0)
	     (cond ((region-get m 'NEWS-HEADER #f)
		    => (lambda (h) (prev-loop h n)))
		   ((find-previous-line-property m 'NEWS-HEADER #f)
		    => (lambda (h)
			 (prev-loop h (if (news-header:unread? h) (+ n 1) n))))
		   (else (lose))))
	    (else #f)))))

(define-command news-group-previous-unread-header
  "Move up to the previous unread article header.
With prefix argument, moves up several headers."
  "p"
  (lambda (n)
    ((ref-command news-group-next-unread-header) (- n))))

(define-command news-group-next-unread-article
  "Select the next unread article.
With prefix argument, moves down several articles."
  "p"
  (lambda (n)
    (if ((ref-command news-group-next-unread-header) n)
	((ref-command news-select-article)))))

(define-command news-group-previous-unread-article
  "Select the previous unread article.
With prefix argument, moves up several articles."
  "p"
  (lambda (n)
    ((ref-command news-group-next-unread-article) (- n))))

(define-command news-group-next-thread
  "Move to the first unread header of the next unread thread.
With prefix argument, moves down several threads."
  "p"
  (lambda (n)
    (let ((b (selected-buffer))
	  (m (current-point)))
      (define (next-loop t n)
	(if (= n 0)
	    (win t)
	    (let ((next (news-group-buffer:next-thread b t)))
	      (if next
		  (next-loop-1 next n)
		  (partial-win t n)))))

      (define (next-loop-1 t n)
	(next-loop t (if (news-thread:all-articles-deleted? t) n (- n 1))))

      (define (prev-loop t n)
	(if (= n 0)
	    (win t)
	    (let ((next (news-group-buffer:previous-thread b t)))
	      (if next
		  (prev-loop-1 next n)
		  (partial-win t n)))))

      (define (prev-loop-1 t n)
	(prev-loop t (if (news-thread:all-articles-deleted? t) n (+ n 1))))

      (define (win t)
	(news-group-buffer:move-to-header
	 b
	 (news-thread:first-header t news-header:unread?))
	#t)

      (define (partial-win t n*)
	(if (not (= n n*)) (win t))
	(lose))

      (define (lose)
	(editor-failure)
	#f)

      (cond ((> n 0)
	     (cond ((region-get m 'NEWS-HEADER #f)
		    => (lambda (h) (next-loop (news-header:thread h) n)))
		   ((find-next-line-property m 'NEWS-HEADER #f)
		    => (lambda (h) (next-loop-1 (news-header:thread h) n)))
		   (else (lose))))
	    ((< n 0)
	     (cond ((region-get m 'NEWS-HEADER #f)
		    => (lambda (h) (prev-loop (news-header:thread h) n)))
		   ((find-previous-line-property m 'NEWS-HEADER #f)
		    => (lambda (h) (prev-loop-1 (news-header:thread h) n)))
		   (else (lose))))
	    (else #f)))))

(define-command news-group-previous-thread
  "Move to the first unread header of the previous unread thread.
With prefix argument, moves up several threads."
  "p"
  (lambda (n)
    ((ref-command news-group-next-thread) (- n))))

(define-command news-group-next-thread-article
  "Select the first unread article of the next unread thread.
With prefix argument, moves down several threads."
  "p"
  (lambda (n)
    (if ((ref-command news-group-next-thread) n)
	((ref-command news-select-article)))))

(define-command news-group-previous-thread-article
  "Select the first unread article of the previous unread thread.
With prefix argument, moves up several threads."
  "p"
  (lambda (n)
    ((ref-command news-group-next-thread-article) (- n))))

(define-command news-delete-article
  "Mark as read the News article indicated by point.
With prefix argument, marks the next several articles."
  "P"
  (lambda (argument)
    (header-iteration argument
      (lambda (buffer header)
	(mark/unmark-news-header-line buffer header 'SEEN)))))

(define-command news-mark-article
  "Mark for retrieval the News article indicated by point.
With prefix argument, marks the next several articles."
  "P"
  (lambda (argument)
    (header-iteration argument
      (lambda (buffer header)
	(mark/unmark-news-header-line buffer header 'MARKED)))))

(define-command news-unmark-article
  "Unmark the News article indicated by point.
With prefix argument, unmarks the next several articles."
  "P"
  (lambda (argument)
    (header-iteration argument unmark-news-header-line)))

(define-command news-unmark-article-backwards
  "Back up to the previous article and unmark it.
With prefix argument, unmarks the previous several articles."
  "p"
  (lambda (argument)
    (header-iteration (- argument) unmark-news-header-line)))

(define (unmark-news-header-line buffer header)
  (mark/unmark-news-header-line buffer header 'UNSEEN))

(define (header-iteration argument procedure)
  (defer-marking-updates (selected-buffer)
    (lambda ()
      (iterate-on-lines
       (lambda (mark) (region-get mark 'NEWS-HEADER #f))
       "news-article header"
       news-header:real?
       argument
       (lambda (h) h)
       (lambda (buffer header)
	 (if (news-thread:expanded? (news-header:thread header))
	     (news-group-buffer:next-header buffer header news-header:real?)
	     (news-group-buffer:first-header-in-next-thread
	      buffer header news-header:real?)))
       (lambda (buffer header)
	 (if (news-thread:expanded? (news-header:thread header))
	     (news-group-buffer:previous-header buffer header
						news-header:real?)
	     (news-group-buffer:last-header-in-previous-thread
	      buffer header news-header:real?)))
       (lambda (buffer header next n)
	 (procedure buffer header)
	 (news-group-buffer:move-to-header buffer
					   (if (and next (> n 0))
					       next
					       header)))))))

(define (mark/unmark-news-header-line buffer header name)
  (let ((thread (news-header:thread header)))
    (if (news-thread:expanded? thread)
	(with-editor-interrupts-disabled
	  (lambda ()
	    ((name->article-marker name) header buffer)
	    (update-buffer-news-header-status buffer header)))
	(mark/unmark-news-thread-lines buffer thread name))))

(define (name->article-marker name)
  (case name
    ((SEEN) news-header:article-deleted!)
    ((MARKED) news-header:article-marked!)
    ((UNSEEN) news-header:article-not-deleted!)
    ((IGNORED) news-header:article-ignored!)
    (else (error "Unknown marker name:" name))))

(define-command news-read-marked-bodies
  "Download the bodies of the marked messages and save them on the disk.
Subsequent reading of the message bodies can be done offline."
  ()
  (lambda ()
    (let* ((buffer (selected-buffer))
	   (headers
	    (cond ((news-group-buffer? buffer)
		   (news-group:marked-headers
		    (news-group-buffer:group buffer)))
		  ((news-server-buffer? buffer)
		   (append-map news-group:marked-headers
			       (vector->list
				(news-server-buffer:groups buffer))))
		  (else
		   '())))
	   (n-articles (length headers))
	   (n-read 0))
      (do ((headers headers (cdr headers))
	   (n 1 (fix:+ n 1)))
	  ((null? headers))
	(let ((header (car headers)))
	  (if (news-header? header)
	      (begin
		(message
		 (string-append "Reading article "
				(number->string n)
				" of "
				(number->string n-articles)))
		(news-header:read-marked-body header buffer)
		(set! n-read (fix:+ n-read 1)))
	      (let ((group (cadr header))
		    (number (caddr header)))
		(set-news-group:ranges-marked!
		 group
		 (remove-from-ranges! (news-group:ranges-marked group) number))
		(news-group:maybe-defer-update buffer group)))))
      (cond ((news-group-buffer? buffer)
	     (news-group:close-database (news-group-buffer:group buffer)))
	    ((news-server-buffer? buffer)
	     (vector-for-each news-group:close-database
			      (news-server-buffer:groups buffer))))
      (message (number->string n-read) " articles read"))))

(define-command news-delete-thread
  "Mark as read the conversation thread indicated by point.
This marks the article indicated by point and any other articles in
 the same thread as that article.
With prefix argument, marks next several threads."
  "P"
  (lambda (argument)
    (thread-iteration argument
      (lambda (buffer thread)
	(mark/unmark-news-thread-lines buffer thread 'SEEN)))))

(define-command news-mark-thread
  "Mark for retrieval the conversation thread indicated by point.
This marks the article indicated by point and any other unread articles in
 the same thread as that article.
With prefix argument, marks next several threads."
  "P"
  (lambda (argument)
    (thread-iteration argument
      (lambda (buffer thread)
	(mark/unmark-news-thread-lines buffer thread 'MARKED)))))

(define-command news-ignore-thread
  "Ignore the conversation thread indicated by point.
With prefix argument, ignores the next several threads."
  "P"
  (lambda (argument)
    (thread-iteration argument news-group-buffer:ignore-thread)))

(define-command news-unmark-thread
  "Unmark the conversation thread indicated by point.
This unmarks the article indicated by point and any other articles in
 the same thread as that article."
  "P"
  (lambda (argument)
    (thread-iteration argument
      (lambda (buffer thread)
	(mark/unmark-news-thread-lines buffer thread 'UNSEEN)))))

(define (thread-iteration argument procedure)
  (defer-marking-updates (selected-buffer)
    (lambda ()
      (iterate-on-lines (lambda (mark) (region-get mark 'NEWS-HEADER #f))
			"news-article header" #f argument
			news-header:thread
			news-group-buffer:next-thread
			news-group-buffer:previous-thread
	(lambda (buffer thread next n)
	  (procedure buffer thread)
	  (news-group-buffer:move-to-thread buffer
					    (if (and next (> n 0))
						next
						thread)))))))

(define (news-group-buffer:move-to-thread buffer thread)
  (news-group-buffer:move-to-header
   buffer
   (news-thread:first-header thread news-header:real?)))

(define (mark/unmark-news-thread-lines buffer thread name)
  (with-editor-interrupts-disabled
    (lambda ()
      (news-thread:for-each-real-header thread
	(let ((marker
	       (let ((marker (name->article-marker name)))
		 (if (eq? name 'IGNORED)
		     marker
		     (lambda (header buffer)
		       (news-header:article-not-ignored! header buffer)
		       (marker header buffer))))))
	  (lambda (header)
	    (marker header buffer)
	    (update-buffer-news-header-status buffer header))))
      (update-buffer-news-thread-status buffer thread))))

(define (news-group-buffer:ignore-thread buffer thread)
  (if (or (ref-variable news-group-ignore-hidden-subjects buffer)
	  (news-thread:expanded? thread))
      (mark/unmark-news-thread-lines buffer thread 'IGNORED)
      (let ((header (news-thread:first-header thread news-header:real?)))
	(if header
	    (with-editor-interrupts-disabled
	      (lambda ()
		(news-thread:for-each-real-header thread
		  (let ((subject
			 (canonicalize-subject (news-header:subject header))))
		    (lambda (header)
		      (if (compare-subjects
			   subject
			   (canonicalize-subject (news-header:subject header)))
			  (news-header:article-ignored! header buffer)))))
		(update-buffer-news-thread-status buffer thread)))))))

(define-command news-select-article
  "Select a buffer containing the News article indicated by point."
  ()
  (lambda ()
    (select-buffer
     (let ((buffer (selected-buffer)))
       (cond ((news-article-buffer? buffer)
	      buffer)
	     ((news-group-buffer? buffer)
	      (call-with-values
		  (lambda ()
		    (get-article-buffer buffer (current-news-header) #t))
		(lambda (buffer new?)
		  new?
		  buffer)))
	     (else
	      (editor-error "No article selected.")))))))

(define-command news-toggle-thread
  "Expand or collapse the current thread."
  ()
  (lambda ()
    (let ((buffer (selected-buffer))
	  (thread (news-header:thread (current-news-header))))
      (if (news-thread:expanded? thread)
	  (news-group-buffer:collapse-thread buffer thread)
	  (news-group-buffer:expand-thread buffer thread))
      (news-group-buffer:move-to-thread buffer thread))))

(define-command news-collapse-threads
  "Collapse all of the threads in this News group."
  ()
  (lambda ()
    (let ((buffer (selected-buffer))
	  (header (region-get (current-point) 'NEWS-HEADER #f)))
      (vector-for-each (lambda (thread)
			 (news-group-buffer:collapse-thread buffer thread))
		       (news-group-buffer:threads buffer))
      (if header
	  (if (news-group-buffer:header-mark buffer header)
	      (news-group-buffer:move-to-header buffer header)
	      (news-group-buffer:move-to-thread
	       buffer
	       (news-header:thread header)))))))

(define-command news-expand-threads
  "Expand all of the threads in this News group."
  ()
  (lambda ()
    (let ((buffer (selected-buffer))
	  (header (region-get (current-point) 'NEWS-HEADER #f)))
      (vector-for-each  (lambda (thread)
			  (news-group-buffer:expand-thread buffer thread))
			(news-group-buffer:threads buffer))
      (if header
	  (news-group-buffer:move-to-header buffer header)))))

(define-command news-revert-group
  "Refresh the article list from the News server.
Any new unread articles are added to the list of available articles.
With \\[universal-argument], all articles in the group are shown,
 including those that were previously marked as read.
With positive argument N, show only N newest unread articles.
With negative argument -N, show only N oldest unread articles."
  "P"
  (lambda (argument)
    (let ((buffer (selected-buffer)))
      (with-buffer-open-1 buffer
	(lambda ()
	  (region-delete! (buffer-region buffer))
	  (initialize-news-group-buffer buffer argument)
	  (set-buffer-point!
	   buffer
	   (or (find-first-property-line buffer 'NEWS-HEADER news-header:real?)
	       (buffer-end buffer))))))))

(define-command news-expunge-group
  "Remove all threads marked as seen from the article list.
Any thread whose articles are all marked is removed;
 if a thread contains any unmarked articles, it is retained.
This command has no effect if the variable
 news-group-show-seen-headers is true."
  ()
  (lambda ()
    (let ((buffer (selected-buffer))
	  (on-header? (region-get (current-point) 'NEWS-HEADER #f)))
      (if (not (ref-variable news-group-show-seen-headers buffer))
	  (let ((threads (vector->list (news-group-buffer:threads buffer))))
	    (with-buffer-open-1 buffer
	      (lambda ()
		(let ((regions '()))
		  (for-each
		   (lambda (thread)
		     (if (news-thread:all-articles-deleted? thread)
			 (let ((region
				(news-thread-lines-region buffer thread)))
			   (if region
			       (set! regions
				     (cons (make-region
					    (mark-right-inserting-copy
					     (region-start region))
					    (mark-left-inserting-copy
					     (region-end region)))
					   regions)))
			   (news-thread:for-each-header thread
			     (lambda (header)
			       (news-group:discard-cached-header! header)
			       (set-news-header:index! header #f))))))
		   threads)
		  (for-each
		   (lambda (region)
		     (delete-string (region-start region) (region-end region))
		     (mark-temporary! (region-start region))
		     (mark-temporary! (region-end region)))
		   regions))
		(update-subsequent-news-header-lines (buffer-start buffer))
		(buffer-put! buffer 'NEWS-THREADS
			     (list->vector
			      (remove news-thread:all-articles-deleted?
				      threads)))
		(if (and on-header?
			 (not (region-get (current-point) 'NEWS-HEADER #f)))
		    (let ((ls
			   (find-previous-property-line (current-point)
							'NEWS-HEADER
							#f)))
		      (if ls
			  (set-current-point! ls)))))))))))

(define-command news-catch-up-group
  "Mark all of the articles as read, and return to the News server buffer.
This kills the current buffer."
  ()
  (lambda ()
    (if (prompt-for-confirmation? "Delete all articles not marked as read")
	(begin
	  (let ((buffer (selected-buffer)))
	    (vector-for-each
	     (lambda (thread)
	       (news-thread:for-each-real-header
		thread
		(lambda (header) (news-header:article-deleted! header buffer))))
	     (news-group-buffer:threads buffer)))
	  ((ref-command news-kill-current-buffer))))))

(define-command news-group-quit
  "Kill the current buffer, going back to the groups list."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (let ((alternate
	     (let ((server-buffer (news-server-buffer buffer #f)))
	       (and server-buffer
		    (or (let ((key (buffer-get buffer 'SELECTED-FROM #f)))
			  (and key
			       (not (eq? key 'SERVER))
			       (buffer-tree:child server-buffer key #f)))
			server-buffer)))))
	(kill-buffer buffer)
	(if alternate (select-buffer alternate))))))

(define-command news-group-show-subject
  "Show the subject of the current article.
Without argument, the subject is shown in the echo area if it will fit there.
Otherwise (or with argument) a buffer containing the subject is popped up.
This is useful when the subject has been truncated by the one-line display."
  "P"
  (lambda (argument)
    (let ((subject
	   (canonicalize-subject (news-header:subject (current-news-header)))))
      (if (and (not argument)
	       (< (string-columns subject 0 8
				  (ref-variable char-image-strings))
		  (window-x-size (typein-window))))
	  (message subject)
	  (pop-up-temporary-buffer " news-header subject"
				   '(READ-ONLY SHRINK-WINDOW FLUSH-ON-SPACE)
	    (lambda (buffer window)
	      window
	      (insert-string subject (buffer-point buffer))))))))

(define-command news-group-show-header
  "Show the header of the current article.
With argument, the complete header is shown.
Otherwise, the standard pruned header is shown."
  "P"
  (lambda (argument)
    (let ((header (current-news-header)))
      (if argument (news-header:guarantee-full-text! header))
      (pop-up-temporary-buffer " news-header subject"
			       '(READ-ONLY SHRINK-WINDOW FLUSH-ON-SPACE)
	(lambda (buffer window)
	  window
	  (insert-news-header header buffer (not argument))
	  ;; delete two blank lines at end
	  (let ((end (buffer-end buffer)))
	    (delete-string (mark- end 2) end)))))))

;;;; News-Article Buffer

(define (find-news-article-buffer group-buffer header)
  (buffer-tree:child group-buffer header #f))

(define (make-news-article-buffer group-buffer header)
  (news-header:guarantee-full-text! header)
  (let ((buffer (new-buffer (news-article-buffer-name header))))
    (set-buffer-major-mode! buffer (ref-mode-object news-article))
    (disable-group-undo! (buffer-group group-buffer))
    (if (let ((msg "Reading article... "))
	  (message msg)
	  (let ((value
		 (call-with-output-mark (buffer-end buffer)
		   (lambda (port)
		     (news-header:read-body header port)))))
	    (message msg "done")
	    value))
	(begin
	  (insert-news-header header buffer #t)
	  (enable-group-undo! (buffer-group group-buffer))
	  (buffer-put! buffer 'NEWS-HEADER header)
	  ;; The next two statements must be executed in this order,
	  ;; because NEWS-ARTICLE-BUFFER:KILL assumes that the
	  ;; kill-buffer hook registered by BUFFER-TREE:ATTACH-CHILD!
	  ;; has already been run.
	  (buffer-tree:attach-child! group-buffer header buffer)
	  (add-kill-buffer-hook buffer news-article-buffer:kill)
	  (set-buffer-point! buffer (buffer-start buffer))
	  (buffer-not-modified! buffer)
	  (set-buffer-read-only! buffer)
	  (news-header:article-deleted! header group-buffer)
	  (update-buffer-news-header-status group-buffer header)
	  (news-group:close-database (news-group-buffer:group group-buffer))
	  buffer)
	(begin
	  (kill-buffer buffer)
	  (news-header:article-deleted! header group-buffer)
	  (update-buffer-news-header-status group-buffer header)
	  (news-group:close-database (news-group-buffer:group group-buffer))
	  #f))))

(define (news-article-buffer-name header)
  (string-append (number->string (news-header:number header))
		 ":"
		 (news-group-buffer-name (news-header:group header))))

(define (news-article-buffer? buffer)
  (news-header? (buffer-get buffer 'NEWS-HEADER #f)))

(define (news-article-buffer:header buffer)
  (let ((header (buffer-get buffer 'NEWS-HEADER #f)))
    (if (not (news-header? header))
	(error "Buffer isn't a News article buffer:" (buffer-name buffer)))
    header))

(define (news-article-buffer:kill buffer)
  (let ((group-buffer (news-group-buffer buffer #f)))
    (if group-buffer
	(ignore-errors
	 (lambda ()
	   (update-buffer-news-header-status
	    group-buffer
	    (news-article-buffer:header buffer))
	   (news-group:close-database
	    (news-group-buffer:group group-buffer)))))))

(define (insert-news-header header buffer truncate?)
  (let ((hend (mark-left-inserting-copy (buffer-start buffer)))
        (text (news-header:text header)))
    (cond ((and truncate?
                (ref-variable news-kept-headers))
           => (lambda (regexps)
                (insert-filtered-news-header text regexps hend buffer)
                (insert-newline hend)))
          (else
           (if (and (not (string-null? text))
                    (char=? #\newline (string-ref text 0)))
               (insert-substring text 1 (string-length text) hend)
               (insert-string text hend))
           (insert-newline hend)
           (if truncate? (delete-ignored-headers (buffer-start buffer) hend))))
    (mark-temporary! hend)
    (buffer-put! buffer 'NEWS-ARTICLE-HEADER-TRUNCATED? truncate?)))

(define (insert-filtered-news-header text regexps mark buffer)
  buffer
  (for-each (lambda (regexp)
              (cond ((re-string-search-forward (string-append "^" regexp)
                                               text
                                               #t)
                     => (lambda (match)
                          (let ((start-index (re-match-start-index 0 match)))
                            (insert-substring text start-index
                                              (find-header-end text
                                                               start-index)
                                              mark))
                          (insert-newline mark)))))
            regexps))

(define (find-header-end text start-index)
  (let* ((limit (string-length text))
         (scan-line (lambda (start)
                      (cond ((string-index text #\newline start limit)
                             => fix:1+)
                            (else #f)))))
    (let loop ((index (scan-line start-index)))
      (cond ((or (not index) (fix:= index limit))
             limit)
            ((let ((char (string-ref text index)))
               (or (char=? char #\space)
                   (char=? char #\tab)))
             (loop (scan-line index)))
            (else
             ;; Lose the trailing newline.
             (fix:-1+ index))))))

(define (delete-ignored-headers hstart hend)
  (let ((regexp (ref-variable rmail-ignored-headers hstart)))
    (if regexp
	(let ((point (mark-right-inserting-copy hstart))
	      (p1 (re-compile-pattern regexp #t))
	      (p2 (re-compile-pattern "\n[^ \t]" #f)))
	  (do ()
	      ((not (re-search-forward p1 point hend)))
	    (move-mark-to! point (line-start (re-match-start 0) 0))
	    (delete-string
	     point
	     (mark-1+ (re-search-forward p2 point hend))))
	  (mark-temporary! point)))))

(define (delete-news-header buffer)
  (let ((start (buffer-start buffer)))
    (delete-string start (mark1+ (mail-header-end start)))))

(define (get-article-buffer group-buffer header error?)
  (if (not (news-header:real? header))
      (editor-error "Can't select a placeholder article."))
  (let ((buffer (find-news-article-buffer group-buffer header)))
    (if buffer
	(values buffer #f)
	(let ((buffer (make-news-article-buffer group-buffer header)))
	  (if (and error? (not buffer))
	      (editor-error "Article no longer available from server."))
	  (values buffer #t)))))

;;;; News-Article Mode

(define-major-mode news-article news-common "News Article"
  "Major mode for reading a News article.

When a News-article buffer is created, the hooks news-article-mode-hook
and news-common-mode-hook are invoked.

This mode's commands include:

\\[news-next-article]	read the next article
\\[news-previous-article]	read the previous article
\\[news-next-unread-article]	read the next unread article
\\[news-previous-unread-article]	read the previous unread article
\\[news-next-article-in-thread]	read the next article in this thread
\\[news-previous-article-in-thread]	read the previous article in this thiread
\\[news-next-unread-article-in-thread]	read the next unread article in this thread
\\[news-previous-unread-article-in-thread]	read the previous unread article in this thread
\\[news-next-thread-article]	read the first article of the next thread
\\[news-previous-thread-article]	read the first article of the previous thread

\\[news-toggle-article-header]	show/don't show all of this article's header lines
\\[news-toggle-article-context]	show/don't show window into the News group buffer

\\[news-compose-followup-article]	post a reply to this article
\\[news-reply-to-article]	reply by email to this article
\\[news-compose-article]	post a new article to this group
\\[news-forward-article]	forward this article by email
\\[mail]	send a new email message

\\[news-output-article]	output this article to a mail file
\\[news-output-article-to-rmail-file]	output this article to an RMAIL file

\\[news-save-server-data]	write info about the subscribed groups to the init file"
  (lambda (buffer)
    (event-distributor/invoke! (ref-variable news-article-mode-hook buffer)
			       buffer)))

(define-variable news-article-mode-hook
  "An event distributor that is invoked when entering News-article mode."
  (make-event-distributor))

(define-key 'news-article #\space '(news-article . #\c-v))
(define-key 'news-article #\rubout '(news-article . #\m-v))
(define-key 'news-article #\c 'news-toggle-article-context)
(define-key 'news-article #\d 'news-next-article)
(define-key 'news-article #\D 'news-next-article-in-thread)
(define-key 'news-article #\f 'news-forward-article)
(define-key 'news-article #\i 'news-ignore-article-thread)
(define-key 'news-article #\n 'news-next-unread-article)
(define-key 'news-article #\N 'news-next-unread-article-in-thread)
(define-key 'news-article #\M-n 'news-next-thread-article)
(define-key 'news-article #\o 'news-output-article-to-rmail-file)
(define-key 'news-article #\c-o 'news-output-article)
(define-key 'news-article #\p 'news-previous-unread-article)
(define-key 'news-article #\P 'news-previous-unread-article-in-thread)
(define-key 'news-article #\M-p 'news-previous-thread-article)
(define-key 'news-article #\r 'news-reply-to-article)
(define-key 'news-article #\R 'news-compose-followup-article)
(define-key 'news-article #\t 'news-toggle-article-header)
(define-key 'news-article #\u 'news-previous-article)
(define-key 'news-article #\U 'news-previous-article-in-thread)

(define-command news-next-article
  "Select a buffer containing the next article in the News group.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       (news-group-buffer:next-header buffer header news-header:real?)))))

(define-command news-previous-article
  "Select a buffer containing the previous article in the News group.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       (news-group-buffer:previous-header buffer header news-header:real?)))))

(define-command news-next-unread-article
  "Select a buffer containing the next unread article in the News group.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       (news-group-buffer:next-header buffer header news-header:unread?)))))

(define-command news-previous-unread-article
  "Select a buffer containing the previous unread article in the News group.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       (news-group-buffer:previous-header buffer header
					  news-header:unread?)))))

(define-command news-next-article-in-thread
  "Select a buffer containing the next article in this thread.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       buffer
       (news-thread:next-header header news-header:real?)))))

(define-command news-previous-article-in-thread
  "Select a buffer containing the previous article in this thread.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       buffer
       (news-thread:previous-header header news-header:real?)))))

(define-command news-next-unread-article-in-thread
  "Select a buffer containing the next unread article in this thread.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       buffer
       (news-thread:next-header header news-header:unread?)))))

(define-command news-previous-unread-article-in-thread
  "Select a buffer containing the previous unread article in this thread.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-header-motion-command
     (lambda (buffer header)
       buffer
       (news-thread:previous-header header news-header:unread?)))))

(define-command news-next-thread-article
  "Select a buffer containing the first unread article in the next thread.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-thread-motion-command news-group-buffer:next-thread)))

(define-command news-previous-thread-article
  "Select a buffer containing the first unread article in the previous thread.
If there is no such article, return to the News-group buffer.
Kill the current buffer in either case."
  ()
  (lambda ()
    (news-article-thread-motion-command news-group-buffer:previous-thread)))

(define-command news-ignore-article-thread
  "Ignore the thread that this article is in, and skip to the next thread."
  ()
  (lambda ()
    (news-article-thread-action-command news-group-buffer:next-thread
					news-group-buffer:ignore-thread)))

(define (news-article-header-motion-command select-next)
  (news-article-header-action-command select-next #f))

(define (news-article-thread-motion-command select-next)
  (news-article-thread-action-command select-next #f))

(define (news-article-thread-action-command select-next action)
  (news-article-header-action-command
   (lambda (buffer header)
     (let ((thread (select-next buffer (news-header:thread header))))
       (and thread
	    (news-thread:first-header thread news-header:unread?))))
   (and action
	(lambda (buffer header)
	  (action buffer (news-header:thread header))))))

(define (news-article-header-action-command select-next action)
  (let ((buffer (selected-buffer)))
    (let ((group-buffer (buffer-tree:parent buffer #t))
	  (header (news-article-buffer:header buffer)))
      (if action (action group-buffer header))
      (let loop ((header header))
	(let ((header (select-next group-buffer header)))
	  (if (not header)
	      (begin
		(message "No more articles.")
		(select-buffer group-buffer)
		(kill-buffer buffer)
		#f)
	      (let ((article-buffer
		     (or (find-news-article-buffer group-buffer header)
			 (make-news-article-buffer group-buffer header))))
		(if article-buffer
		    (begin
		      (news-group-buffer:move-to-header group-buffer header)
		      (select-buffer article-buffer)
		      (kill-buffer buffer)
		      #t)
		    (loop header)))))))))

(define-command news-toggle-article-header
  "Show original article header if pruned header currently shown, or vice versa.
Normally, the header lines specified in the variable rmail-ignored-headers
 are not shown; this command shows them, or hides them if they are shown."
  ()
  (lambda ()
    (let ((buffer (selected-buffer)))
      (with-buffer-open-1 buffer
	(lambda ()
	  (let ((header (news-article-buffer:header buffer)))
	    (delete-news-header buffer)
	    (insert-news-header
	     header
	     buffer
	     (not (buffer-get buffer 'NEWS-ARTICLE-HEADER-TRUNCATED? #f))))))
      (set-current-point! (buffer-start buffer)))))

(define-command news-toggle-article-context
  "Show context window into News group buffer, or hide it if currently shown.
This is a small window showing a few lines around the subject line of the
 current article.  The number of lines is specified by the variable
 news-article-context-lines, but a prefix argument overrides this."
  "P"
  (lambda (argument)
    (let ((article-window (current-window))
	  (context-lines
	   (if argument
	       (min 1 (command-argument-value argument))
	       (ref-variable news-article-context-lines))))
      (let ((article-buffer (window-buffer article-window)))
	(let ((group-buffer (buffer-tree:parent article-buffer #t)))
	  (let ((context-window
		 (news-group-buffer:context-window group-buffer #f)))
	    (let ((set-height
		   (lambda ()
		     (let ((delta
			    (- context-lines (window-y-size context-window))))
		       (if (not (= delta 0))
			   (window-grow-vertically! context-window delta)))
		     (center-news-article-context context-window))))
	      (with-editor-interrupts-disabled
		(lambda ()
		  (cond ((not context-window)
			 (show-news-article-context article-window
						    context-lines))
			((not (eq? group-buffer
				   (window-buffer context-window)))
			 (select-buffer group-buffer context-window)
			 (set-height))
			(argument
			 (set-height))
			(else
			 (window-delete! context-window article-window)
			 (buffer-remove! group-buffer
					 'CONTEXT-WINDOW))))))))))))

(define-command news-output-article-to-rmail-file
  "Append the current article to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the article is appended to the
 buffer visiting that file.
With prefix argument, appends next several articles."
  (lambda ()
    (list (prompt-for-rmail-output-filename
	   "Output article to Rmail file"
	   (ref-variable rmail-last-rmail-file))
	  (command-argument)))
  (lambda (pathname argument)
    (set-variable! rmail-last-rmail-file (->namestring pathname))
    (call-on-news-article-buffers argument
      (lambda (article-buffer)
	(with-article-output-buffer article-buffer
	  (lambda (buffer)
	    (rfc822-region->babyl (buffer-region buffer))
	    (rmail-output-to-rmail-file (buffer-region buffer) pathname)))))))

(define-command news-output-article
  "Append this article to Unix mail file named FILE-NAME.
With prefix argument, appends next several articles."
  (lambda ()
    (list (prompt-for-rmail-output-filename "Output article to Unix mail file"
					    (ref-variable rmail-last-file))
	  (command-argument)))
  (lambda (pathname argument)
    (set-variable! rmail-last-file (->namestring pathname))
    (call-on-news-article-buffers argument
      (lambda (article-buffer)
	(with-article-output-buffer article-buffer
	  (lambda (buffer)
	    (rmail-output-to-unix-mail-file (buffer-region buffer)
					    pathname)))))))

(define (call-on-news-article-buffers argument procedure)
  (let ((buffer (selected-buffer)))
    (cond ((news-article-buffer? buffer)
	   (let loop
	       ((buffer buffer)
		(n (command-argument-numeric-value argument)))
	     (if (> n 0)
		 (begin
		   (procedure buffer)
		   (if ((ref-command news-next-article))
		       (loop (selected-buffer) (- n 1)))))))
	  ((news-group-buffer? buffer)
	   (header-iteration argument
	     (lambda (group-buffer header)
	       (call-with-values
		   (lambda () (get-article-buffer group-buffer header #f))
		 (lambda (article-buffer new?)
		   (if article-buffer
		       (begin
			 (procedure article-buffer)
			 (if new? (kill-buffer article-buffer)))))))))
	  (else
	   (editor-error "No article selected.")))))

(define (with-article-output-buffer article-buffer procedure)
  (with-editor-interrupts-disabled
    (lambda ()
      (let ((buffer (temporary-buffer " news conversion")))
	(insert-region (buffer-absolute-start article-buffer)
		       (buffer-absolute-end article-buffer)
		       (buffer-start buffer))
	(delete-news-header buffer)
	(insert-news-header (news-article-buffer:header article-buffer)
			    buffer
			    #f)
	(procedure buffer)
	(kill-buffer buffer)))))

(define-command news-reply-to-article
  "Mail a reply to the author of the current News article.
While composing the reply, use \\[mail-yank-original] to yank the
 original message into it."
  ()
  (lambda ()
    (guarantee-rmail-variables-initialized)
    (let ((article-buffer (selected-buffer)))
      (if (and (not (news-article-buffer:followup-to-poster? article-buffer))
	       (prompt-for-confirmation? "Post a follow-up article"))
	  (make-news-reply-buffer
	   (merge-header-alists
	    (news-article-buffer:rfc822-reply-headers article-buffer)
	    (news-article-buffer:followup-fields article-buffer))
	   article-buffer
	   select-buffer-other-window)
	  (make-mail-buffer
	   (news-article-buffer:rfc822-reply-headers article-buffer)
	   article-buffer
	   select-buffer-other-window)))))

(define (merge-header-alists x y)
  (append
   (remove (lambda (entry)
	     (find (lambda (entry*) (string-ci=? (car entry) (car entry*)))
		   y))
	   x)
   y))

(define (news-article-buffer:rfc822-reply-headers article-buffer)
  (call-with-temporary-buffer " news conversion"
    (lambda (buffer)
      (insert-news-header (news-article-buffer:header article-buffer)
			  buffer #f)
      (rfc822-region-reply-headers (buffer-region buffer) #t))))

(define-command news-forward-article
  "Forward the current News article to another user by email."
  ()
  (lambda ()
    (let ((article-buffer (selected-buffer)))
      (make-mail-buffer
       (let ((header (news-article-buffer:header article-buffer)))
	 `(("To" "")
	   ("Subject"
	    ,(string-append
	      "["
	      (let ((from
		     (rfc822:canonicalize-address-string
		      (news-header:from header)))
		    (subject (news-header:subject header)))
		(if (string-null? from)
		    subject
		    (string-append from ": " subject)))
	      "]"))))
       #f
       (if (window-has-no-neighbors? (current-window))
	   select-buffer
	   select-buffer-other-window))
      (insert-region (buffer-start article-buffer)
		     (buffer-end article-buffer)
		     (buffer-end (selected-buffer))))))

;;;; Posting

(define-command news-compose-article
  "Begin editing a News article to be posted.
Argument means resume editing previous article (don't erase).
Once editing the article, type \\[describe-mode] to get a list of commands."
  "P"
  (lambda (no-erase?)
    (compose-news no-erase? select-buffer)))

(define-command news-compose-article-other-window
  "Like \\[news-compose-article], but display article buffer in other window."
  "P"
  (lambda (no-erase?)
    (compose-news no-erase? select-buffer-other-window)))

(define (compose-news no-erase? selector)
  (let ((server
	 (let ((buffer (current-news-server-buffer #f)))
	   (and buffer
		(news-server-buffer:server buffer))))
	(group
	 (or (region-get (current-point) 'NEWS-GROUP #f)
	     (buffer-get (selected-buffer) 'NEWS-GROUP #f)
	     (let ((header (buffer-get (selected-buffer) 'NEWS-header #f)))
	       (and header
		    (news-header:group header))))))
    (let ((buffer
	   (make-mail-buffer `(("Newsgroups"
				,(if group (news-group:name group) ""))
			       ("Subject" ""))
			     #f
			     selector
			     (if no-erase?
				 'KEEP-PREVIOUS-MAIL
				 'QUERY-DISCARD-PREVIOUS-MAIL)
			     "*news*"
			     (ref-mode-object compose-news))))
      (if buffer
	  (begin
	    (if server (buffer-put! buffer 'NEWS-SERVER server))
	    (if (not group)
		(set-buffer-point! buffer
				   (mail-position-on-field buffer
							   "Newsgroups"))))))))

(define-command news-compose-followup-article
  "Begin editing a follow-up to the current News article.
While composing the follow-up, use \\[mail-yank-original] to yank the
 original message into it."
  ()
  (lambda ()
    (guarantee-rmail-variables-initialized)
    (let ((article-buffer (selected-buffer)))
      (if (news-article-buffer:followup-to-poster? article-buffer)
	  (make-mail-buffer
	   (news-article-buffer:rfc822-reply-headers article-buffer)
	   article-buffer
	   select-buffer-other-window)
	  (make-news-reply-buffer
	   (news-article-buffer:followup-fields article-buffer)
	   article-buffer
	   select-buffer-other-window)))))

(define (news-article-buffer:followup-to-poster? buffer)
  (string-ci=? (news-header:field-value (news-article-buffer:header buffer)
					"followup-to")
	       "poster"))

(define (make-news-reply-buffer header-fields article-buffer select-buffer)
  (let ((buffer
	 (make-mail-buffer header-fields
			   article-buffer
			   select-buffer
			   'QUERY-DISCARD-PREVIOUS-MAIL
			   "*news*"
			   (ref-mode-object compose-news))))
    (if (and buffer article-buffer)
	(buffer-put! buffer 'NEWS-SERVER
		     (nntp-connection:server
		      (news-group:connection
		       (news-header:group
			(news-article-buffer:header article-buffer))))))))

(define (news-article-buffer:followup-fields buffer)
  (let ((header (news-article-buffer:header buffer)))
    `(("Newsgroups"
       ,(let ((followup-to (news-header:field-value header "followup-to")))
	  (if (string-null? followup-to)
	      (news-header:field-value header "newsgroups")
	      followup-to)))
      ("Subject" ,(let ((subject (news-header:subject header)))
		    (if (and (not (string-null? subject))
			     (not (string-prefix-ci? "re:" subject)))
			(string-append "Re: " subject)
			subject)))
      ("References" ,(let ((refs (news-header:references header))
			   (id (news-header:message-id header)))
		       (if (string-null? refs)
			   id
			   (string-append refs " " id)))
		    #T)
      ("In-reply-to"
       ,(make-in-reply-to-field (news-header:from header)
				(news-header:field-value header "date")
				(news-header:message-id header)))
      ("Distribution"
       ,(let ((distribution (news-header:field-value header "distribution")))
	  (and (not (string-null? distribution))
	       distribution))))))

(define-major-mode compose-news mail "News"
  "Major mode for editing news to be posted on USENET.

Like Text mode but with these additional commands:

C-c C-s  mail-send (post the message)           C-c C-c  mail-send-and-exit
C-c C-f	 move to a header field (and create it if there isn't):
	 C-c C-f C-n  move to Newsgroups:	C-c C-f C-s  move to Subject:
	 C-c C-f C-f  move to Followup-To:      C-c C-f C-k  move to Keywords:
	 C-c C-f C-d  move to Distribution:	C-c C-f C-a  move to Summary:
C-c C-w  mail-signature (insert ~/.signature at end).
C-c C-y  mail-yank-original (insert current message, in News reader).
C-c C-q  mail-fill-yanked-message (fill what was yanked)."
  (lambda (buffer)
    (local-set-variable! send-mail-procedure
			 (lambda () (news-post-it))
			 buffer)
    (event-distributor/invoke! (ref-variable compose-news-mode-hook buffer)
			       buffer)))

(define-variable compose-news-mode-hook
  "An event distributor that is invoked when entering Compose News mode."
  (make-event-distributor))

(define-key 'compose-news '(#\c-c #\c-f #\c-a) 'news-move-to-summary)
(define-key 'compose-news '(#\c-c #\c-f #\c-d) 'news-move-to-distribution)
(define-key 'compose-news '(#\c-c #\c-f #\c-f) 'news-move-to-followup-to)
(define-key 'compose-news '(#\c-c #\c-f #\c-k) 'news-move-to-keywords)
(define-key 'compose-news '(#\c-c #\c-f #\c-n) 'news-move-to-newsgroups)

(define ((field-mover field))
  (set-current-point! (mail-position-on-field (selected-buffer) field)))

(define-command news-move-to-newsgroups
  "Move point to end of Newsgroups: field."
  ()
  (field-mover "Newsgroups"))

(define-command news-move-to-followup-to
  "Move point to end of Followup-to: field."
  ()
  (field-mover "Followup-to"))

(define-command news-move-to-distribution
  "Move point to end of Distribution: field."
  ()
  (field-mover "Distribution"))

(define-command news-move-to-keywords
  "Move point to end of Keywords: field."
  ()
  (field-mover "Keywords"))

(define-command news-move-to-summary
  "Move point to end of Summary: field."
  ()
  (field-mover "Summary"))

(define (news-post-it)
  (let ((article-buffer (selected-buffer)))
    (prepare-mail-buffer-for-sending article-buffer
      (lambda (h-start h-end b-start b-end)
	(news-post-process-headers h-start h-end b-start b-end article-buffer)
	(let ((m (mail-field-start h-start h-end "X-Mailer")))
	  (if m
	      (let ((ls (line-start m 0)))
		(delete-string ls (mark-1+ (char-search-forward #\: ls m)))
		(insert-string "X-Newsreader" ls))))
	(finish-preparing-mail-buffer h-start h-end b-start b-end
				      article-buffer
	  (lambda (send-mail message-pathname)
	    (if (or (mail-field-start h-start h-end "To")
		    (mail-field-start h-start h-end "CC")
		    (mail-field-start h-start h-end "BCC"))
		(send-mail))
	    (post-news-buffer message-pathname article-buffer)))))))

(define (post-news-buffer message-pathname lookup-buffer)
  (let ((do-it
	 (lambda (connection)
	   (let ((msg "Posting..."))
	     (message msg)
	     (let ((error
		    (call-with-input-file message-pathname
		      (lambda (port)
			(nntp-connection:post-article connection port)))))
	       (if error
		   (string-append msg "failed: " error)
		   (begin
		     (message msg "done")
		     #f))))))
	(finish
	 (lambda (result)
	   (if result
	       (editor-error result)))))
    (let ((server
	   (or (buffer-get lookup-buffer 'NEWS-SERVER #f)
	       (get-news-server-name #f))))
      (let ((server-buffer (find-news-server-buffer server)))
	(if server-buffer
	    (finish (do-it (news-server-buffer:connection server-buffer)))
	    (let ((connection (make-nntp-connection-1 server lookup-buffer)))
	      (let ((result (do-it connection)))
		(nntp-connection:close connection)
		(finish result))))))))

(define (news-post-process-headers h-start h-end b-start b-end lookup-context)
  (let ((h-start (mark-left-inserting-copy h-start)))
    (if (not (mail-field-end h-start h-end "From"))
	(insert-string (mail-from-string #f)
		       (mail-insert-field h-start "From")))
    (if (not (mail-field-end h-start h-end "Date"))
	(insert-string (universal-time->string (get-universal-time))
		       (mail-insert-field h-start "Date")))
    (if (not (mail-field-end h-start h-end "Subject"))
	(mail-insert-field h-start "Subject"))
    (if (not (mail-field-end h-start h-end "Lines"))
	(insert-string (number->string (count-lines b-start b-end))
		       (mail-insert-field h-start "Lines")))
    (let ((region (mail-field-region h-start h-end "Newsgroups")))
      (if region
	  (news-post-canonicalize-newsgroups region)
	  (mail-insert-field h-start "Newsgroups")))
    (if (not (mail-field-end h-start h-end "Message-id"))
	(insert-string
	 (news-post-default-message-id (mail-field-region h-start h-end
							  "Subject")
				       lookup-context)
	 (mail-insert-field h-end "Message-id")))
    (if (not (mail-field-end h-start h-end "Path"))
	(insert-string (news-post-default-path)
		       (mail-insert-field h-end "Path")))
    (mark-temporary! h-start)))

(define (news-post-canonicalize-newsgroups region)
  (let ((start (mark-right-inserting-copy (region-start region)))
	(end (mark-left-inserting-copy (region-end region))))
    (let ((replace-regexp
	   (lambda (from to)
	     (let loop ((start start))
	       (let ((mark (re-search-forward from start end #f)))
		 (if mark
		     (loop (replace-match to))))))))
      (replace-regexp "\n[ \t]+" " ")
      (replace-regexp "[ \t]*,[ \t]*" ",")
      (replace-regexp "[ \t]+" ","))
    (mark-temporary! end)
    (mark-temporary! start)))

(define (news-post-default-path)
  (string-append (get-news-server-name #f) "!" (current-user-name)))

(define (news-post-default-message-id subject-region lookup-context)
  ;; From "News Article Format and Transmission, 2 June 1994, section
  ;; 6.5: The followup agent MUST not delete any message ID whose
  ;; local part ends with "_-_" (underscore (ASCII 95), hyphen (ASCII
  ;; 45), underscore); followup agents are urged to use this form to
  ;; mark subject changes, and to avoid using it otherwise.
  (string-append "<"
		 (current-user-name)
		 "."
		 (number->string (get-universal-time))
		 (if (compare-subjects
		      (canonicalize-subject
		       (let ((reply-buffer
			      (ref-variable mail-reply-buffer lookup-context)))
			 (if reply-buffer
			     (news-header:subject
			      (news-article-buffer:header reply-buffer))
			     "")))
		      (canonicalize-subject
		       (if subject-region (region->string subject-region) "")))
		     ""
		     "_-_")
		 "@"
		 (os/hostname)
		 ">"))

;;;; Init Files

(define (read-init-file pathname description get-valid-entry?)
  (if (file-exists? pathname)
      (bind-condition-handler (list condition-type:error)
	  (lambda (condition)
	    condition
	    (editor-error description
			  " in "
			  (->namestring pathname)
			  " is damaged."))
	(lambda ()
	  (let ((entries (fasload pathname)))
	    (if (not (list? entries))
		(error:datum-out-of-range entries))
	    (let ((valid-entry? (get-valid-entry? (car entries))))
	      (if (not valid-entry?)
		  (error:datum-out-of-range (car entries)))
	      (for-each (lambda (entry)
			  (if (not (valid-entry? entry))
			      (error:datum-out-of-range entry)))
			(cdr entries)))
	    (cdr entries))))
      '()))

(define (write-init-file pathname buffer key entries)
  (guarantee-init-file-directory pathname)
  (if buffer
      (begin
	(local-set-variable! version-control 'NEVER buffer)
	(backup-buffer! buffer pathname #f)))
  (fasdump (cons key entries) pathname #t)
  (message "Wrote " (->namestring pathname))
  (if buffer
      (call-with-values (lambda () (os/buffer-backup-pathname pathname buffer))
	(lambda (backup-pathname targets)
	  targets
	  (delete-file-no-errors backup-pathname)))))

(define (init-file-pathname . components)
  (init-file-specifier->pathname (cons "snr" components)))

;;;; Groups File

(define (read-groups-init-file connection)
  (list->vector
   (let ((convert-entry #f))
     (let ((entries
	    (let ((server (nntp-connection:server connection)))
	      (read-init-file (groups-init-file-pathname server)
			      (groups-init-file-description server)
		(lambda (key)
		  (case key
		    ((1)
		     (set! convert-entry convert-groups-init-file-entry-type-1)
		     validate-groups-init-file-entry-type-1)
		    ((2)
		     (set! convert-entry convert-groups-init-file-entry-type-2)
		     validate-groups-init-file-entry-type-2)
		    ((3)
		     (set! convert-entry convert-groups-init-file-entry-type-3)
		     validate-groups-init-file-entry-type-3)
		    ((4)
		     (set! convert-entry convert-groups-init-file-entry-type-4)
		     validate-groups-init-file-entry-type-4)
		    (else #f)))))))
       (if (null? entries)
	   entries
	   (map (convert-entry connection) entries))))))

(define (write-groups-init-file buffer)
  (let ((server-buffer (news-server-buffer buffer #t)))
    (let ((server (news-server-buffer:server server-buffer))
	  (groups (news-server-buffer:groups server-buffer)))
      (write-init-file
       (groups-init-file-pathname server)
       server-buffer
       4
       (let loop ((groups (vector->list groups)) (entries '()))
	 (if (null? groups)
	     entries
	     (loop
	      (cdr groups)
	      (let ((group (car groups)))
		(if (and (not (news-group:subscribed? group))
			 (ranges-empty? (news-group:ranges-deleted group))
			 (ranges-empty? (news-group:ranges-marked group))
			 (ranges-empty? (news-group:ranges-browsed group)))
		    entries
		    (cons (vector (news-group:name group)
				  (news-group:subscribed? group)
				  (news-group:server-info group)
				  (news-group:ranges-deleted group)
				  (news-group:ranges-marked group)
				  (news-group:ranges-browsed group))
			  entries))))))))))

(define (groups-init-file-pathname server)
  (init-file-pathname server "groups"))

(define (groups-init-file-description server)
  (string-append "News-groups data for " server))

(define (validate-groups-init-file-entry-type-1 entry)
  (and (list? entry)
       (>= (length entry) 2)
       (string? (car entry))
       (boolean? (cadr entry))
       (every range? (cddr entry))))

(define ((convert-groups-init-file-entry-type-1 connection) entry)
  (make-news-group-1 connection (car entry) (cadr entry) #f (cddr entry)
		     '() '()))

(define (validate-groups-init-file-entry-type-2 entry)
  (and (list? entry)
       (>= (length entry) 3)
       (string? (car entry))
       (boolean? (cadr entry))
       (valid-group-server-info? (caddr entry))
       (every range? (cdddr entry))))

(define ((convert-groups-init-file-entry-type-2 connection) entry)
  (make-news-group-1 connection
		     (car entry)
		     (cadr entry)
		     (caddr entry)
		     (cdddr entry)
		     '()
		     '()))

(define (validate-groups-init-file-entry-type-3 entry)
  (and (vector? entry)
       (= (vector-length entry) 5)
       (string? (vector-ref entry 0))
       (boolean? (vector-ref entry 1))
       (valid-group-server-info? (vector-ref entry 2))
       (every range? (vector-ref entry 3))
       (every range? (vector-ref entry 4))))

(define ((convert-groups-init-file-entry-type-3 connection) entry)
  (make-news-group-1 connection
		     (vector-ref entry 0)
		     (vector-ref entry 1)
		     (vector-ref entry 2)
		     (vector-ref entry 3)
		     (vector-ref entry 4)
		     '()))

(define (validate-groups-init-file-entry-type-4 entry)
  (and (vector? entry)
       (= (vector-length entry) 6)
       (string? (vector-ref entry 0))
       (boolean? (vector-ref entry 1))
       (valid-group-server-info? (vector-ref entry 2))
       (every range? (vector-ref entry 3))
       (every range? (vector-ref entry 4))
       (every range? (vector-ref entry 5))))

(define ((convert-groups-init-file-entry-type-4 connection) entry)
  (make-news-group-1 connection
		     (vector-ref entry 0)
		     (vector-ref entry 1)
		     (vector-ref entry 2)
		     (vector-ref entry 3)
		     (vector-ref entry 4)
		     (vector-ref entry 5)))

(define (valid-group-server-info? server-info)
  (and (vector? server-info)
       (= (vector-length server-info) 3)
       (or (equal? '#(#f #f #f) server-info)
	   (equal? '#(0 0 0) server-info)
	   (and (exact-nonnegative-integer? (vector-ref server-info 0))
		(article-number? (vector-ref server-info 1))
		(article-number? (vector-ref server-info 2))))))

;;;; Ignored-Subjects File

(define (read-ignored-subjects-file group)
  (let ((entries
	 (read-init-file (ignored-subjects-file-pathname group)
			 (ignored-subjects-file-description group)
	   (lambda (key)
	     (case key
	       ((1)
		(lambda (entry)
		  (and (pair? entry)
		       (pair? (cdr entry))
		       (null? (cddr entry))
		       (string? (car entry))
		       (not (string-null? (car entry)))
		       (exact-nonnegative-integer? (cadr entry)))))
	       (else #f))))))
    (if (null? entries)
	#f
	(let ((table (make-hash-table string=? string-hash)))
	  (for-each (lambda (entry)
		      (hash-table-set! table (car entry) (cadr entry)))
		    entries)
	  table))))

(define (write-ignored-subjects-file group buffer)
  ;; Action of NEWS-GROUP:PURGE-IGNORED-SUBJECTS! has been integrated
  ;; into this procedure to increase performance.  The
  ;; ignored-subjects lists can be quite large, and this allows the
  ;; list to be processed in a single pass rather than two.
  (let ((table
	 (and (pair? (news-group:ignored-subjects group))
	      (news-group:get-ignored-subjects group #f))))
    (and table
	 (let ((entries (hash-table->alist table))
	       (t
		(- (get-universal-time)
		   (* (ref-variable news-group-ignored-subject-retention #f)
		      86400))))
	   (and (or (news-group:ignored-subjects-modified? group)
		    (any (lambda (entry) (< (cdr entry) t)) entries))
		(begin
		  (write-init-file (ignored-subjects-file-pathname group)
				   buffer
				   1
				   (let loop ((entries entries) (result '()))
				     (cond ((null? entries)
					    result)
					   ((< (cdar entries) t)
					    (hash-table-delete! table
								(caar entries))
					    (loop (cdr entries) result))
					   (else
					    (loop (cdr entries)
						  (cons (list (caar entries)
							      (cdar entries))
							result))))))
		  (news-group:ignored-subjects-not-modified! group)
		  #t))))))

(define (ignored-subjects-file-pathname group)
  (init-file-pathname (news-group:server group)
		      "ignored-subjects"
		      (news-group:name group)))

(define (ignored-subjects-file-description group)
  (string-append "Ignored-subjects data for "
		 (news-group:server group)
		 ":"
		 (news-group:name group)))

;;;; .newsrc File

(define-command read-newsrc-file
  "Read the .newsrc file and apply it to the current subscribed-groups list.
Normally, merges the .newsrc entries into the groups list.
With prefix arg, replaces the groups list with the .newsrc entries."
  "P"
  (lambda (replace?)
    (let ((buffer (current-news-server-buffer #t)))
      (let ((connection (news-server-buffer:connection buffer)))
	(let ((entries
	       (call-with-newsrc-file-buffer connection parse-newsrc-buffer)))
	  (if replace?
	      (vector-for-each 
	       (lambda (group)
		 (if (not (assoc (news-group:name group) entries))
		     (unsubscribe-news-group buffer group)))
	       (news-server-buffer:groups buffer)))
	  (for-each
	   (lambda (entry)
	     (let ((name (car entry))
		   (subscribed? (cadr entry))
		   (ranges (cddr entry)))
	       (let ((group
		      (let ((group (find-news-group connection name)))
			(if group
			    (begin
			      (set-news-group:ranges-deleted!
			       group
			       (if replace?
				   ranges
				   (merge-ranges
				    (news-group:ranges-deleted group)
				    ranges)))
			      (news-group:guarantee-ranges-deleted group)
			      group)
			    (make-news-group-1 connection name #f #f ranges
					       '() '())))))
		 (if subscribed?
		     (subscribe-news-group buffer group)
		     (unsubscribe-news-group buffer group)))))
	   entries))))))

(define-command write-newsrc-file
  "Write the .newsrc file corresponding to the current subscribed-groups list.
Normally, merges the list information into the file.
With prefix arg, replaces the file with the list information."
  "P"
  (lambda (replace?)
    (let ((buffer (current-news-server-buffer #t)))
      (let ((connection (news-server-buffer:connection buffer)))
	(call-with-newsrc-file-buffer connection
	  (lambda (newsrc)
	    (if replace? (delete-region (buffer-unclipped-region newsrc)))
	    (vector-for-each (lambda (group) (update-newsrc-group newsrc group))
			     (news-server-buffer:groups buffer))
	    (save-buffer newsrc #f)))))))

(define (parse-newsrc-buffer buffer)
  (let loop ((start (buffer-start buffer)) (entries '()))
    (let ((end (line-end start 0)))
      (let ((entries
	     (let ((mark (re-match-forward "^[^:! \t\n]+[:!]" start end)))
	       (if mark
		   (cons (cons* (extract-string start (mark-1+ mark))
				(char=? #\: (extract-left-char mark))
				(parse-newsrc-group-ranges mark end))
			 entries)
		   entries))))
	(if (group-end? end)
	    (reverse! entries)
	    (loop (mark1+ end) entries))))))

(define (parse-newsrc-group-ranges mark end)
  (let loop ((mark mark) (ranges '()))
    (if (re-match-forward "[, \t]*\\([0-9-]+\\)" mark end)
	(let ((s (re-match-start 1))
	      (e (re-match-end 1)))
	  (loop e
		(let ((test
		       (lambda (pattern)
			 (let ((m (re-match-forward pattern s e)))
			   (and m
				(mark= m e))))))
		  (cond ((test "[0-9]+")
			 (cons (let ((n (extract-nonnegative-integer s e)))
				 (make-range n n))
			       ranges))
			((test "\\([0-9]+\\)-\\([0-9]+\\)")
			 (let ((n
				(extract-nonnegative-integer
				 (re-match-start 1)
				 (re-match-end 1)))
			       (m
				(extract-nonnegative-integer
				 (re-match-start 2)
				 (re-match-end 2))))
			   (if (< n m)
			       (cons (make-range n m) ranges)
			       ranges)))
			(else
			 ranges)))))
	(canonicalize-ranges (reverse! ranges)))))

(define (extract-nonnegative-integer start end)
  (let loop ((mark start) (n 0))
    (if (mark= mark end)
	n
	(loop (mark1+ mark)
	      (+ (* n 10)
		 (fix:- (char->integer (extract-right-char mark))
			(char->integer #\0)))))))

(define (update-newsrc-group buffer group)
  (let ((mark
	 (re-search-forward
	  (string-append "^"
			 (re-quote-string (news-group:name group))
			 "[:!]")
	  (buffer-start buffer)))
	(finish
	 (lambda (mark)
	   (insert-char (if (news-group:subscribed? group) #\: #\!) mark)
	   (let ((ranges
		  (let ((ranges (news-group:guarantee-ranges-deleted group))
			(first (news-group:first-article group)))
		    (if (> first 1)
			(canonicalize-ranges
			 (cons (make-range 1 (- first 1)) ranges))
			ranges)))
		 (write-range
		  (lambda (range)
		    (let ((f (range-first range))
			  (l (range-last range)))
		      (if (= f l)
			  (insert-string (number->string f) mark)
			  (begin
			    (insert-string (number->string f) mark)
			    (insert-char #\- mark)
			    (insert-string (number->string l) mark)))))))
	     (if (not (null? ranges))
		 (begin
		   (insert-char #\space mark)
		   (write-range (car ranges))
		   (for-each (lambda (range)
			       (insert-char #\, mark)
			       (write-range range))
			     (cdr ranges))))))))
    (if mark
	(let ((mark (mark-left-inserting-copy (mark-1+ mark))))
	  (delete-string mark (line-end mark 0))
	  (finish mark)
	  (mark-temporary! mark))
	(let ((mark (mark-left-inserting-copy (buffer-end buffer))))
	  (guarantee-newline mark)
	  (insert-string (news-group:name group) mark)
	  (finish mark)
	  (insert-newline mark)
	  (mark-temporary! mark)))))

(define (call-with-newsrc-file-buffer connection receiver)
  (let ((pathname (os/newsrc-file-name (nntp-connection:server connection))))
    (let ((buffer (pathname->buffer pathname)))
      (if buffer
	  (receiver (find-file-revert buffer))
	  (let ((buffer (find-file-noselect pathname #f)))
	    (set-variable! version-control #f buffer)
	    (let ((value (receiver buffer)))
	      (kill-buffer buffer)
	      value))))))

;;;; Line Property Items

(define (iterate-on-lines get-item adjective predicate argument
			  first-item next-item previous-item procedure)
  (call-with-values
      (lambda ()
	(start-property-iteration get-item adjective predicate argument))
    (lambda (item n)
      (cond (item
	     (let ((buffer (selected-buffer)))
	       (cond ((> n 0)
		      (let loop ((item (first-item item)) (n n))
			(let ((next (next-item buffer item)))
			  (procedure buffer item next n)
			  (if (> n 1)
			      (if next
				  (loop next (- n 1))
				  (editor-failure))))))
		     ((< n 0)
		      (let loop ((item (first-item item)) (n n))
			(let ((previous (previous-item buffer item)))
			  (procedure buffer item previous n)
			  (if (< n -1)
			      (if previous
				  (loop previous (+ n 1))
				  (editor-failure)))))))))
	    ((not (= n 0))
	     (editor-failure))))))

(define (start-property-iteration get-item adjective predicate argument)
  (let ((start (line-start (current-point) 0)))
    (if (not argument)
	(let ((item (get-item start)))
	  (if (and item (or (not predicate) (predicate item)))
	      (values item 1)
	      (not-on-property-line-error adjective)))
	(let ((n (command-argument-value argument)))
	  (cond ((> n 0)
		 (let loop ((ls start))
		   (let ((item (get-item ls)))
		     (if (and item (or (not predicate) (predicate item)))
			 (values item n)
			 (let ((ls (line-start ls 1 #f)))
			   (if ls
			       (loop ls)
			       (values #f n)))))))
		((< n 0)
		 (let ((ls (line-start start -1 #f)))
		   (if ls
		       (let loop ((ls ls))
			 (let ((item (get-item ls)))
			   (if (and item (or (not predicate) (predicate item)))
			       (values item n)
			       (let ((ls (line-start ls -1 #f)))
				 (if ls
				     (loop ls)
				     (values #f n))))))
		       (values #f n))))
		(else
		 (values #f n)))))))

(define (not-on-property-line-error adjective)
  (editor-error "Point isn't on a"
		(if (memv (string-ref adjective 0) '(#\a #\e #\i #\o #\u))
		    "n"
		    "")
		" "
		adjective
		" line."))

(define (find-next-property-line ls key predicate)
  (let loop ((ls ls))
    (let ((ls (line-start ls 1 #f)))
      (if (or (not ls)
	      (let ((item (region-get ls key #f)))
		(and item
		     (or (not predicate)
			 (predicate item)))))
	  ls
	  (loop ls)))))

(define (find-previous-property-line ls key predicate)
  (let loop ((ls ls))
    (let ((ls (line-start ls -1 #f)))
      (if (or (not ls)
	      (let ((item (region-get ls key #f)))
		(and item
		     (or (not predicate)
			 (predicate item)))))
	  ls
	  (loop ls)))))

(define (find-first-property-line buffer key predicate)
  (let ((ls (buffer-start buffer)))
    (if (let ((item (region-get ls key #f)))
	  (and item
	       (or (not predicate)
		   (predicate item))))
	ls
	(find-next-property-line ls key predicate))))

(define (find-next-line-property ls key predicate)
  (let loop ((ls ls))
    (let ((ls (line-start ls 1 #f)))
      (and ls
	   (let ((item (region-get ls key #f)))
	     (if (and item
		      (or (not predicate)
			  (predicate item)))
		 item
		 (loop ls)))))))

(define (find-previous-line-property ls key predicate)
  (let loop ((ls ls))
    (let ((ls (line-start ls -1 #f)))
      (and ls
	   (let ((item (region-get ls key #f)))
	     (if (and item
		      (or (not predicate)
			  (predicate item)))
		 item
		 (loop ls)))))))

(define (find-first-line buffer get-item)
  (let loop ((ls (buffer-start buffer)))
    (if (get-item ls)
	ls
	(let ((ls (line-start ls 1 #f)))
	  (and ls
	       (loop ls))))))

(define (find-buffer-line buffer get-item test-item if-found if-not-found)
  (let loop
      ((low (buffer-start buffer))
       (high
	(let loop ((end (buffer-end buffer)))
	  (if (and (line-start? end)
		   (not (group-start? end)))
	      (loop (mark-1+ end))
	      end))))
    (let inner ((ls (line-start (mark-average low high) 0)))
      (let ((item (get-item ls)))
	(cond (item
	       (case (test-item item)
		 ((EQUAL)
		  (if-found ls))
		 ((LESS)
		  (if (mark< low ls)
		      (loop low (mark-1+ ls))
		      (if-not-found low)))
		 (else
		  (let ((le (line-end ls 0)))
		    (if (mark< le high)
			(loop (mark1+ le) high)
			(if-not-found
			 (if (group-end? le)
			     le
			     (mark1+ le))))))))
	      ((let loop ((ls ls))
		 (let ((le (line-end ls 0)))
		   (and (mark< le high)
			(let ((ls (mark1+ le)))
			  (if (get-item ls)
			      ls
			      (loop ls))))))
	       => inner)
	      ((let loop ((ls ls))
		 (and (mark< low ls)
		      (let ((ls (line-start (mark-1+ ls) 0)))
			(if (get-item ls)
			    ls
			    (loop ls)))))
	       => inner)
	      (else
	       (if-not-found (buffer-end buffer))))))))

;;;; Miscellaneous

(define (vector-insert v i x)
  (let ((l (vector-length v)))
    (let ((v* (make-vector (fix:+ l 1))))
      (vector-copy! v* 0 v 0 i)
      (vector-set! v* i x)
      (vector-copy! v* (fix:+ i 1) v i l)
      v*)))

(define (vector-delete v i)
  (let ((l (vector-length v)))
    (let ((v* (make-vector (fix:- l 1))))
      (vector-copy! v* 0 v 0 i)
      (vector-copy! v* 1 v (fix:+ i 1) l)
      v*)))

(define (string-order x y)
  (string-compare x y
		  (lambda () 'EQUAL)
		  (lambda () 'LESS)
		  (lambda () 'GREATER)))

(define (prefix-matcher prefix)
  (let ((plen (string-length prefix)))
    (lambda (x y)
      (let ((n (string-prefix-length x y)))
	(and (fix:>= n plen)
	     n)))))

(define (create-news-buffer name mode procedure)
  (let ((buffer (new-buffer name)))
    (set-buffer-major-mode! buffer mode)
    (disable-group-undo! (buffer-group buffer))
    (set-buffer-point! buffer (or (procedure buffer) (buffer-end buffer)))
    (buffer-not-modified! buffer)
    (set-buffer-read-only! buffer)
    buffer))

(define (mark-average m1 m2)
  (make-mark (mark-group m1)
	     (fix:quotient (fix:+ (mark-index m1) (mark-index m2)) 2)))

(define (with-buffer-open-1 buffer thunk)
  (with-buffer-open buffer
    (lambda ()
      (with-editor-interrupts-disabled thunk)
      (buffer-not-modified! buffer))))

;;;; Buffer Trees

(define (buffer-tree:parent buffer error?)
  (or (let ((node (buffer-tree:node buffer #f)))
	(and node
	     (car node)))
      (and error?
	   (error "Missing parent buffer:" (buffer-name buffer)))))

(define (buffer-tree:child buffer key error?)
  (or (let ((node (buffer-tree:node buffer #f)))
	(and node
	     (let ((entry (assq key (cdr node))))
	       (and entry
		    (cdr entry)))))
      (and error?
	   (error "Missing child buffer:" key (buffer-name buffer)))))

(define (buffer-tree:children buffer)
  (let ((node (buffer-tree:node buffer #f)))
    (if node
	(map cdr (cdr node))
	'())))

(define (buffer-tree:attach-child! parent key child)
  (with-editor-interrupts-disabled
    (lambda ()
      (let ((node (buffer-tree:node parent #t)))
	(let ((entry (assq key (cdr node))))
	  (if entry
	      (set-cdr! entry child)
	      (set-cdr! node (cons (cons key child) (cdr node))))))
      (set-car! (buffer-tree:node child #t) parent))))

(define (buffer-tree:node buffer intern?)
  (or (buffer-get buffer 'BUFFER-TREE #f)
      (and intern?
	   (let ((node (cons #f '())))
	     (with-editor-interrupts-disabled
	       (lambda ()
		 (buffer-put! buffer 'BUFFER-TREE node)
		 (add-kill-buffer-hook buffer buffer-tree:kill)))
	     node))))

(define (buffer-tree:kill buffer)
  (with-editor-interrupts-disabled
    (lambda ()
      (ignore-errors
       (lambda ()
	 (let ((node (buffer-tree:node buffer #f)))
	   (if node
	       (begin
		 (let ((parent (car node)))
		   (if parent
		       (let ((node (buffer-tree:node parent #f)))
			 (and node
			      (set-cdr! node
					((list-deletor!
					  (lambda (entry)
					    (eq? buffer (cdr entry))))
					 (cdr node)))))))
		 (for-each (lambda (child)
			     (let ((node (buffer-tree:node child #f)))
			       (if node
				   (set-car! node #f))))
			   (map cdr (cdr node)))))))))))

;;;; Article Ranges

(define (range? object)
  (or (article-number? object)
      (and (pair? object)
	   (article-number? (car object))
	   (article-number? (cdr object))
	   (<= (car object) (cdr object)))))

(define (article-number? object)
  (and (exact-integer? object)
       (> object 0)))

(define (make-range f l) (if (= f l) f (cons f l)))
(define (range-first r)  (if (pair? r) (car r) r))
(define (range-last r)   (if (pair? r) (cdr r) r))
(define (range-length r) (if (pair? r) (+ (- (cdr r) (car r)) 1) 1))
(define ranges-empty? null?)

(define (count-ranges ranges)
  (let loop ((ranges ranges) (count 0))
    (if (null? ranges)
	count
	(loop (cdr ranges) (+ count (range-length (car ranges)))))))

(define (canonicalize-ranges ranges)
  (if (null? ranges)
      ranges
      (let ((ranges
	     (sort ranges (lambda (x y) (< (range-first x) (range-first y))))))
	(let loop ((ranges ranges))
	  (if (not (null? (cdr ranges)))
	      (let ((x (car ranges))
		    (y (cadr ranges)))
		(if (<= (range-first y) (+ (range-last x) 1))
		    (begin
		      (set-car! ranges
				(make-range (range-first x)
					    (max (range-last x)
						 (range-last y))))
		      (set-cdr! ranges (cddr ranges))
		      (loop ranges))
		    (loop (cdr ranges))))))
	ranges)))

(define (clip-ranges! ranges first last)
  (let ((holder
	 (cons 'HOLDER
	       (let clip-first ((ranges ranges))
		 (cond ((or (null? ranges)
			    (<= first (range-first (car ranges))))
			ranges)
		       ((< (range-last (car ranges)) first)
			(clip-first (cdr ranges)))
		       (else
			(set-car! ranges
				  (make-range first (range-last (car ranges))))
			ranges))))))
    (let clip-last ((ranges (cdr holder)) (prev holder))
      (cond ((null? ranges)
	     unspecific)
	    ((< (range-last (car ranges)) last)
	     (clip-last (cdr ranges) ranges))
	    ((> (range-first (car ranges)) last)
	     (set-cdr! prev '()))
	    (else
	     (if (> (range-last (car ranges)) last)
		 (set-car! ranges
			   (make-range (range-first (car ranges))
				       last)))
	     (set-cdr! ranges '()))))
    (cdr holder)))

(define (complement-ranges ranges first last)
  (if (null? ranges)
      (list (make-range first last))
      (let loop
	  ((e (range-last (car ranges)))
	   (ranges (cdr ranges))
	   (result
	    (let ((s (range-first (car ranges))))
	      (if (< first s)
		  (list (make-range first (- s 1)))
		  '()))))
	(if (null? ranges)
	    (reverse! (if (< e last)
			  (cons (make-range (+ e 1) last) result)
			  result))
	    (loop (range-last (car ranges))
		  (cdr ranges)
		  (cons (make-range (+ e 1) (- (range-first (car ranges)) 1))
			result))))))

(define (merge-ranges ranges ranges*)
  (cond ((null? ranges)
	 ranges*)
	((null? ranges*)
	 ranges)
	((< (range-last (car ranges)) (range-first (car ranges*)))
	 (cons (car ranges) (merge-ranges (cdr ranges) ranges*)))
	((< (range-last (car ranges*)) (range-first (car ranges)))
	 (cons (car ranges*) (merge-ranges ranges (cdr ranges*))))
	(else
	 (cons (make-range (min (range-first (car ranges))
				(range-first (car ranges*)))
			   (max (range-last (car ranges))
				(range-last (car ranges*))))
	       (merge-ranges (cdr ranges) (cdr ranges*))))))

(define (add-to-ranges! ranges number)
  (let ((holder (cons 'HOLDER ranges)))
    (let loop ((ranges ranges) (prev holder))
      (if (null? ranges)
	  (set-cdr! prev (list (make-range number number)))
	  (let ((f (range-first (car ranges)))
		(l (range-last (car ranges))))
	    (cond ((> number (+ l 1))
		   (loop (cdr ranges) ranges))
		  ((< number (- f 1))
		   (set-cdr! prev (cons (make-range number number) ranges)))
		  (else
		   (let ((f (min f number))
			 (l (max l number)))
		     (if (and (not (null? (cdr ranges)))
			      (= (+ l 1) (range-first (cadr ranges))))
			 (begin
			   (set-car! ranges
				     (make-range f (range-last (cadr ranges))))
			   (set-cdr! ranges (cddr ranges)))
			 (set-car! ranges (make-range f l)))))))))
    (cdr holder)))

(define (remove-from-ranges! ranges number)
  (let ((holder (cons 'HOLDER ranges)))
    (let loop ((ranges ranges) (prev holder))
      (if (not (null? ranges))
	  (let ((f (range-first (car ranges)))
		(l (range-last (car ranges))))
	    (cond ((> number l)
		   (loop (cdr ranges) ranges))
		  ((>= number f)
		   (if (= number f)
		       (if (= number l)
			   (set-cdr! prev (cdr ranges))
			   (set-car! ranges (make-range (+ f 1) l)))
		       (if (= number l)
			   (set-car! ranges (make-range f (- l 1)))
			   (begin
			     (set-car! ranges (make-range (+ number 1) l))
			     (set-cdr! prev
				       (cons (make-range f (- number 1))
					     ranges))))))))))
    (cdr holder)))

(define (member-of-ranges? ranges number)
  (let loop ((ranges ranges))
    (and (not (null? ranges))
	 (or (<= (range-first (car ranges)) number (range-last (car ranges)))
	     (loop (cdr ranges))))))

(define (ranges->list ranges)
  (let loop ((ranges ranges) (result '()))
    (if (null? ranges)
	(reverse! result)
	(loop (cdr ranges)
	      (let ((e (range-last (car ranges))))
		(let loop ((n (range-first (car ranges))) (result result))
		  (let ((result (cons n result)))
		    (if (= n e)
			result
			(loop (+ n 1) result)))))))))

(define (for-each-range-element procedure ranges)
  (for-each (lambda (range)
	      (let ((e (+ (range-last range) 1)))
		(do ((n (range-first range) (+ n 1)))
		    ((= n e) unspecific)
		  (procedure n))))
	    ranges))

;;;; News-Group Extensions

(define-structure (news-group-extra
		   (type vector)
		   (conc-name news-group-extra:)
		   (constructor make-news-group-extra ()))
  (subscribed? #f)
  (ranges-deleted '())
  (index #f)
  (ignored-subjects 'UNKNOWN)
  (ranges-marked '())
  (ranges-browsed '()))

(define (get-news-group-extra group write?)
  (or (news-group:reader-hook group)
      (let ((extra (make-news-group-extra)))
	(if write? (set-news-group:reader-hook! group extra))
	extra)))

(define (news-group:subscribed? group)
  (news-group-extra:subscribed? (get-news-group-extra group #f)))

(define (set-news-group:subscribed?! group value)
  (set-news-group-extra:subscribed?! (get-news-group-extra group #t) value))

(define (news-group:ranges-deleted group)
  (news-group-extra:ranges-deleted (get-news-group-extra group #f)))

(define (set-news-group:ranges-deleted! group value)
  (set-news-group-extra:ranges-deleted! (get-news-group-extra group #t) value))

(define (news-group:index group)
  (news-group-extra:index (get-news-group-extra group #f)))

(define (set-news-group:index! group value)
  (set-news-group-extra:index! (get-news-group-extra group #t) value))

(define (news-group:ignored-subjects group)
  (news-group-extra:ignored-subjects (get-news-group-extra group #f)))

(define (set-news-group:ignored-subjects! group value)
  (set-news-group-extra:ignored-subjects! (get-news-group-extra group #t)
					  value))

(define (news-group:ranges-marked group)
  (news-group-extra:ranges-marked (get-news-group-extra group #f)))

(define (set-news-group:ranges-marked! group value)
  (set-news-group-extra:ranges-marked! (get-news-group-extra group #t) value))

(define (news-group:ranges-browsed group)
  (news-group-extra:ranges-browsed (get-news-group-extra group #f)))

(define (set-news-group:ranges-browsed! group value)
  (set-news-group-extra:ranges-browsed! (get-news-group-extra group #t) value))

(define (make-news-group-1 connection name subscribed? server-info
			   ranges-deleted ranges-marked ranges-browsed)
  (let ((group (make-news-group connection name)))
    (set-news-group:subscribed?! group subscribed?)
    (set-news-group:server-info! group server-info)
    (set-news-group:ranges-deleted! group (canonicalize-ranges ranges-deleted))
    (set-news-group:ranges-marked! group (canonicalize-ranges ranges-marked))
    (set-news-group:ranges-browsed! group (canonicalize-ranges ranges-browsed))
    (news-group:clip-ranges! group)
    (news-group:apply-cache-policy group)
    group))

(define (news-group:apply-cache-policy group)
  (set-news-group:use-gdbm!
   group
   (let ((nggp (ref-variable news-group-cache-policy)))
     (if (cond ((eq? 'ALL (car nggp)) #t)
	       ((eq? 'SUBSCRIBED (car nggp)) (news-group:subscribed? group))
	       (else (member (news-group:name group) (car nggp))))
	 (cadr nggp)
	 '()))))

(define (news-group:get-threads group argument buffer)
  (let ((headers (news-group:get-headers group argument buffer))
	(msg "Threading headers... "))
    (message msg)
    (let ((threads
	   (organize-headers-into-threads
	    headers
	    (ref-variable news-group-show-context-headers buffer)
	    #f
	    (ref-variable news-split-threads-on-subject-changes buffer)
	    (ref-variable news-join-threads-with-same-subject buffer))))
      (news-group:close-database group)
      (message msg "done")
      (list->vector
       (if (or (command-argument-multiplier-only? argument)
	       (ref-variable news-group-show-seen-headers buffer))
	   threads
	   (remove news-thread:all-articles-deleted? threads))))))

(define (news-group:get-headers group argument buffer)
  (let ((connection (news-group:connection group))
        (all?
         (or (command-argument-multiplier-only? argument)
             (ref-variable news-group-show-seen-headers buffer)))
        (limit
         (and argument
              (not (command-argument-multiplier-only? argument))
              (command-argument-value argument))))
    (if (and (command-argument-multiplier-only? argument)
             (nntp-connection:closed? connection))
        (nntp-connection:reopen connection))
    (if (and (ref-variable news-refresh-group-when-selected
                           (news-server-buffer buffer #f))
             (not (nntp-connection:closed? connection)))
        (news-group:update-ranges! group))
    (receive (headers invalid)
        (split-list (news-group:headers* group all? limit buffer)
                    news-header?)
      (for-each (lambda (entry)
                  (if (not (eq? (car entry) 'UNREACHABLE-ARTICLE))
                      (article-number-seen! group (cdr entry))))
                invalid)
      headers)))

(define (news-group:get-unread-headers group buffer)
  (news-group:update-ranges! group)
  (news-group:pre-read-headers group (news-group:unread-header-numbers group))
  (if (not (ref-variable news-group-show-seen-headers buffer))
      ;; Read in the headers -- this finds the headers to be ignored
      ;; and marks them as such.
      (news-group:get-headers group #f buffer))
  (news-group:close-database group))

(define (news-group:headers* group all? limit context)
  (news-group:headers
   group
   (if all?
       (news-group:all-header-numbers group)
       (let ((ns (news-group:unread-header-numbers group)))
         (if limit
             (let ((lns (length ns)))
               (cond ((<= lns (abs limit)) ns)
                     ((< limit 0) (take ns (- limit)))
                     (else (drop ns (- (length ns) limit)))))
             ns)))
   (let ((ignore-header?
          (let ((filter (ref-variable news-header-filter context)))
            (or (and filter
                     (lambda (header)
                       (not (filter header))))
                (lambda (header) header #f))))
         (table
          (news-group:get-ignored-subjects group #f)))
     (if table
         (let ((t (get-universal-time))
               (show-ignored? (not all?)))
           (lambda (header)
             (and (ignore-header? header)
                  (news-header:ignore?! header table t)
                  (begin
                    (set-news-header:status! header #\I)
                    (article-number-seen! group
                                          (news-header:number header))
                    show-ignored?))))
         ignore-header?))))

;;;; Header Filter Combinators

(define (news-header-splitting-filter unit-filter)
  (lambda (header)
    (let* ((text (news-header:text header))
           (limit (string-length text))
           (start (if (and (fix:> limit 1)
                           (char=? (string-ref text 0) #\newline))
                      1
                      0)))
      (let loop ((start start) (index start))
        (cond ((string-index text #\newline index limit)
               => (lambda (line-end)
                    (let ((next-line-start (fix:1+ line-end)))
                      (if (fix:= next-line-start limit)
                          (unit-filter text start line-end)
                          (let ((char (string-ref text next-line-start)))
                            (if (or (char=? char #\space)
                                    (char=? char #\tab))
                                (loop start next-line-start)
                                (and (unit-filter text start line-end)
                                     (loop next-line-start
                                           next-line-start))))))))
              ((fix:= start limit)
               #t)
              (else
               (unit-filter text start limit)))))))

(define (news-header-regexp-filter specifiers)
  (news-header-splitting-filter
   (let ((table (alist->string-table
                 (map (lambda (specifier)
                        (cons (car specifier)
                              (re-compile-pattern (cdr specifier)
                                                  #f)))      ; Don't case-fold.
                      specifiers)
                 #t)))      ; Case-insensitive
     (lambda (text start end)
       (cond ((string-index text #\: start end)
              => (lambda (colon-index)
                   (cond ((string-table-get table
                                            (substring text start colon-index))
                          => (lambda (regexp)
                               (not (re-substring-match regexp text
                                                        ;; Skip colon & space.
                                                        (fix:+ colon-index 2)
                                                        end))))
                         (else #t))))
             (else #t))))))

(define (article-number-seen! group number)
  (set-news-group:ranges-deleted!
   group
   (add-to-ranges! (news-group:guarantee-ranges-deleted group) number)))

(define (news-group:unread-header-numbers group)
  (if (news-group:server-has-articles? group)
      (ranges->list
       (complement-ranges (news-group:guarantee-ranges-deleted group)
			  (news-group:first-article group)
			  (news-group:last-article group)))
      '()))

(define (news-group:all-header-numbers group)
  (if (news-group:server-has-articles? group)
      (ranges->list
       (complement-ranges '()
			  (news-group:first-article group)
			  (news-group:last-article group)))
      '()))

(define (news-group:update-ranges! group)
  (let ((msg
	 (string-append "Updating group info for "
			(news-group:name group)
			"... ")))
    (message msg)
    (news-group:update-server-info! group)
    (message msg "done"))
  (if (news-group:active? group)
      (news-group:clip-ranges! group)))

(define (news-group:purge-and-compact-headers! group buffer)
  (let ((msg
	 (string-append "Purging headers in " (news-group:name group) "... ")))
    (message msg)
    (news-group:purge-header-cache group 'ALL)
    (news-group:purge-pre-read-headers group
      (if (ref-variable news-group-keep-seen-headers buffer)
	  (if (news-group:server-has-articles? group)
	      (lambda (header)
		(let ((number (news-header:number header)))
		  (or (< number (news-group:first-article group))
		      (> number (news-group:last-article group))
		      (and (not (ref-variable news-group-keep-ignored-headers
					      buffer))
			   (news-header:ignore? header)))))
	      (lambda (header)
		header
		#t))
	  news-header:article-deleted?))
    (news-group:close-database group)
    (message msg "done")))

(define (news-group:number-of-articles group)
  (let ((estimate (news-group:estimated-n-articles group)))
    (and estimate
	 (if (and (news-group:reader-hook group)
		  (news-group:server-has-articles? group))
	     (let ((n-seen
		    (count-ranges
		     (news-group:guarantee-ranges-deleted group))))
	       (if (= n-seen 0)
		   estimate
		   (- (- (+ (news-group:last-article group) 1)
			 (news-group:first-article group))
		      n-seen)))
	     estimate))))

(define (news-group:clip-ranges! group)
  (if (news-group:server-has-articles? group)
      (let ((first (news-group:first-article group))
	    (last (news-group:last-article group)))
	(set-news-group:ranges-deleted!
	 group
	 (clip-ranges! (news-group:ranges-deleted group) first last))
	(set-news-group:ranges-marked!
	 group
	 (clip-ranges! (news-group:ranges-marked group) first last))
	(set-news-group:ranges-browsed!
	 group
	 (clip-ranges! (news-group:ranges-browsed group) first last)))
      (begin
	(set-news-group:ranges-deleted! group '())
	(set-news-group:ranges-marked! group '())
	(set-news-group:ranges-browsed! group '()))))

(define (news-group:guarantee-ranges-deleted group)
  (let ((ranges
	 (if (news-group:server-has-articles? group)
	     (clip-ranges! (news-group:ranges-deleted group)
			   (news-group:first-article group)
			   (news-group:last-article group))
	     '())))
    (set-news-group:ranges-deleted! group ranges)
    ranges))

(define ((news-group:adjust-article-status! handle-xrefs? procedure)
	 header buffer)
  (let ((do-it
	 (lambda (group number)
	   (procedure group number)
	   (news-group:maybe-defer-update buffer group))))
    (do-it (news-header:group header) (news-header:number header))
    (if handle-xrefs?
	(news-group:process-cross-posts header do-it))))

(define (news-group:process-cross-posts header process-header)
  (for-each (let ((connection
		   (news-group:connection (news-header:group header))))
	      (lambda (xref)
		(let ((group (find-news-group connection (car xref))))
		  (if (and group (news-group:subscribed? group))
		      (let ((number (token->number (cdr xref))))
			(if (not (news-group:article-browsed? group number))
			    (process-header group number)))))))
	    (news-header:xref header)))

(define (defer-marking-updates buffer thunk)
  (fluid-let ((news-group:adjust-article-status!:deferred-updates (list #t)))
    (thunk)
    (for-each (lambda (group) (update-news-groups-buffers buffer group))
	      (cdr news-group:adjust-article-status!:deferred-updates))))

(define (news-group:maybe-defer-update buffer group)
  (let ((deferred-updates news-group:adjust-article-status!:deferred-updates))
    (if deferred-updates
	(if (not (memq group (cdr deferred-updates)))
	    (set-cdr! deferred-updates (cons group (cdr deferred-updates))))
	(update-news-groups-buffers buffer group))))

(define news-group:adjust-article-status!:deferred-updates #f)

(define (news-group:articles-marked? group)
  (not (ranges-empty? (news-group:ranges-marked group))))

(define (news-group:marked-headers group)
  (map (lambda (number)
	 (let ((header (news-group:header group number)))
	   (if (news-header? header)
	       header
	       (list header group number))))
       (ranges->list (news-group:ranges-marked group))))

(define (news-header:read-marked-body header buffer)
  (news-header:guarantee-full-text! header)
  (news-header:pre-read-body header)
  (news-header:article-not-deleted! header buffer)
  (let ((buffer
	 (if (news-group-buffer? buffer)
	     buffer
	     (find-news-group-buffer buffer (news-header:group header)))))
    (if buffer
	(update-buffer-news-header-status buffer header))))

(define (news-group:order t1 t2)
  (cond ((news-group:< t1 t2) 'LESS)
	((news-group:< t2 t1) 'GREATER)
	(else 'EQUAL)))

(define ((range-predicate group-ranges) header)
  (member-of-ranges? (group-ranges (news-header:group header))
		     (news-header:number header)))

(define news-header:article-deleted?
  (range-predicate news-group:ranges-deleted))

(define news-header:article-marked?
  (range-predicate news-group:ranges-marked))

(define (news-group:article-browsed? group number)
  (member-of-ranges? (news-group:ranges-browsed group) number))

(define (ranges-marker group-ranges set-group-ranges! handle-xrefs? procedure)
  (news-group:adjust-article-status! handle-xrefs?
    (lambda (group number)
      (set-group-ranges! group (procedure (group-ranges group) number)))))

(define (ranges-deleted-marker procedure)
  (let ((marker
	 (ranges-marker news-group:ranges-deleted
			set-news-group:ranges-deleted!
			#t
			procedure)))
    (lambda (header buffer)
      (news-group:article-unmarked! header buffer)
      (marker header buffer))))

(define news-group:article-deleted!
  (ranges-deleted-marker add-to-ranges!))

(define news-group:article-not-deleted!
  (ranges-deleted-marker remove-from-ranges!))

(define news-group:article-marked!
  (let ((marker
	 (ranges-marker news-group:ranges-marked
			set-news-group:ranges-marked!
			#t
			add-to-ranges!)))
    (lambda (header buffer)
      (news-group:article-not-deleted! header buffer)
      (marker header buffer))))

(define news-group:article-unmarked!
  (ranges-marker news-group:ranges-marked
		 set-news-group:ranges-marked!
		 #t
		 remove-from-ranges!))

(define news-group:article-browsed!
  (ranges-marker news-group:ranges-browsed
		 set-news-group:ranges-browsed!
		 #f
		 add-to-ranges!))

(define (news-group:server-has-articles? group)
  (and (article-number? (news-group:first-article group))
       (article-number? (news-group:last-article group))))

;;;; Ignored-Subjects Database

(define (news-header:ignore?! header table t)
  (let ((subject (canonicalize-subject (news-header:subject header))))
    (and (not (fix:= 0 (string-length subject)))
	 (hash-table-ref/default table subject #f)
	 (let ((group (news-header:group header)))
	   (hash-table-set! table subject t)
	   (news-group:ignored-subjects-modified! group)
	   (news-group:process-cross-posts header
					   (ignore-subject-marker subject t))
	   #t))))

(define (news-header:ignore? header)
  (let ((table
	 (news-group:get-ignored-subjects (news-header:group header) #f)))
    (and table
	 (let ((subject (canonicalize-subject (news-header:subject header))))
	   (and (not (fix:= 0 (string-length subject)))
		(hash-table-ref/default table subject #f))))))

(define (news-group:article-ignored! header buffer)
  (let ((subject (canonicalize-subject (news-header:subject header))))
    (if (not (fix:= 0 (string-length subject)))
	(let ((process-header
	       (ignore-subject-marker subject (get-universal-time))))
	  (process-header (news-header:group header)
			  (news-header:number header))
	  (news-group:process-cross-posts header process-header))))
  (news-group:article-deleted! header buffer))

(define ((ignore-subject-marker subject t) group number)
  number
  (hash-table-set! (news-group:get-ignored-subjects group #t) subject t)
  (news-group:ignored-subjects-modified! group))

(define (news-group:article-not-ignored! header buffer)
  buffer
  (let ((subject (canonicalize-subject (news-header:subject header))))
    (if (not (fix:= 0 (string-length subject)))
	(let ((process-header
	       (lambda (group number)
		 number
		 (let ((table (news-group:get-ignored-subjects group #f)))
		   (if (and table (hash-table-ref/default table subject #f))
		       (begin
			 (hash-table-delete! table subject)
			 (news-group:ignored-subjects-modified! group)))))))
	  (process-header (news-header:group header)
			  (news-header:number header))
	  (news-group:process-cross-posts header process-header)))))

(define (news-group:get-ignored-subjects group intern?)
  (or (let ((table (news-group:ignored-subjects group)))
	(if (eq? table 'UNKNOWN)
	    (let ((table (read-ignored-subjects-file group)))
	      (set-news-group:ignored-subjects! group (cons table #f))
	      table)
	    (car table)))
      (and intern?
	   (let ((table (make-hash-table string=? string-hash)))
	     (set-news-group:ignored-subjects! group (cons table #f))
	     table))))

(define (news-group:ignored-subjects-modified! group)
  (set-cdr! (news-group:ignored-subjects group) #t))

(define (news-group:ignored-subjects-not-modified! group)
  (set-cdr! (news-group:ignored-subjects group) #f))

(define (news-group:ignored-subjects-modified? group)
  (and (pair? (news-group:ignored-subjects group))
       (cdr (news-group:ignored-subjects group))))

;;;; News-Header Extensions

(define-structure (news-header-extra
		   (type vector)
		   (conc-name news-header-extra:)
		   (constructor make-news-header-extra (status)))
  (status #\space)
  (index #f))

(define (get-news-header-extra header write?)
  (or (news-header:reader-hook header)
      (let ((extra (make-news-header-extra (initial-header-status header))))
	(if write? (set-news-header:reader-hook! header extra))
	extra)))

(define (initial-header-status header)
  (let ((group (news-header:group header))
	(number (news-header:number header)))
    (cond ((or (not (news-header:real? header))
	       (not number))
	   #\D)
	  ((news-header:ignore? header)
	   (set-news-group:ranges-deleted!
	    group
	    (add-to-ranges! (news-group:ranges-deleted group) number))
	   #\I)
	  ((news-header:article-deleted? header) #\D)
	  ((news-header:article-marked? header) #\M)
	  (else #\space))))

(define (news-header:status header)
  (news-header-extra:status (get-news-header-extra header #f)))

(define (set-news-header:status! header value)
  (set-news-header-extra:status! (get-news-header-extra header #t) value))

(define (news-header:index header)
  (news-header-extra:index (get-news-header-extra header #f)))

(define (set-news-header:index! header value)
  (set-news-header-extra:index! (get-news-header-extra header #t) value))

(define (news-header:article-deleted! header buffer)
  (if (not (eqv? (news-header:status header) #\I))
      (set-news-header:status! header #\D))
  (news-group:article-deleted! header buffer))

(define (news-header:article-not-deleted! header buffer)
  (set-news-header:status! header #\space)
  (news-group:article-not-deleted! header buffer))

(define (news-header:article-marked! header buffer)
  (if (not (news-header:pre-read-body? header))
      (begin
	(set-news-header:status! header #\M)
	(news-group:article-marked! header buffer))))

(define (news-header:article-browsed! header buffer)
  (news-group:article-browsed! header buffer))

(define (news-header:article-ignored! header buffer)
  (set-news-header:status! header #\I)
  (news-group:article-ignored! header buffer))

(define (news-header:article-not-ignored! header buffer)
  (set-news-header:status! header #\space)
  (news-group:article-not-ignored! header buffer))

(define (news-header:unread? header)
  (and (news-header:real? header)
       (not (news-header:article-deleted? header))))

(define (news-header:next-in-thread header)
  (let scan-down ((header header))
    (let ((children (news-header:followups header)))
      (if (null? children)
	  (let scan-up ((header header))
	    (let ((parent (news-header:followup-to header)))
	      (and parent
		   (let ((tail (memq header (news-header:followups parent))))
		     (if (null? (cdr tail))
			 (scan-up parent)
			 (cadr tail))))))
	  (car children)))))

(define (news-header:previous-in-thread header)
  (let scan-up ((header header))
    (let ((parent (news-header:followup-to header)))
      (and parent
	   (let scan-across
	       ((siblings (news-header:followups parent))
		(prev #f))
	     (cond ((not (eq? (car siblings) header))
		    (scan-across (cdr siblings) (car siblings)))
		   (prev
		    (let dive-down ((header prev))
		      (let ((children (news-header:followups header)))
			(if (null? children)
			    header
			    (dive-down (car (take-right children 1)))))))
		   (else parent)))))))

;;;; News-Thread Extensions

(define news-thread:expanded? news-thread:reader-hook)
(define set-news-thread:expanded?! set-news-thread:reader-hook!)

(define (news-thread:first-header thread predicate)
  (let ((root (news-thread:root thread)))
    (if (or (not predicate) (predicate root))
	root
	(news-thread:next-header root predicate))))

(define (news-thread:next-header header predicate)
  (let ((header (news-header:next-in-thread header)))
    (if (or (not header) (not predicate) (predicate header))
	header
	(news-thread:next-header header predicate))))

(define (news-thread:previous-header header predicate)
  (let ((header (news-header:previous-in-thread header)))
    (if (or (not header) (not predicate) (predicate header))
	header
	(news-thread:previous-header header predicate))))

(define (news-thread:last-header thread predicate)
  (let ((header (news-thread:first-header thread predicate)))
    (if header
	(let loop ((header (news-thread:first-header thread predicate)))
	  (let ((next (news-thread:next-header header predicate)))
	    (if next
		(loop next)
		header)))
	#f)))

(define (news-thread:for-each-real-header thread procedure)
  (news-thread:for-each-header thread
    (lambda (header)
      (if (news-header:real? header)
	  (procedure header)))))

(define (news-thread:n-articles thread predicate)
  (let loop ((header (news-thread:first-header thread predicate)) (n 0))
    (if header
	(loop (news-thread:next-header header predicate) (+ n 1))
	n)))

(define (news-thread:status thread)
  (let ((root (news-thread:first-header thread news-header:real?)))
    (let ((status (news-header:status root)))
      (let loop ((header root))
	(let ((header (news-thread:next-header header news-header:real?)))
	  (cond ((not header) status)
		((char=? (news-header:status header) status) (loop header))
		((or (char=? status #\I)
		     (char=? (news-header:status header) #\I))
		 #\i)
		((or (char=? status #\M)
		     (char=? (news-header:status header) #\M))
		 #\m)
		(else #\d)))))))

(define (news-thread:pre-read-bodies thread)
  (let loop
      ((header (news-thread:first-header thread news-header:real?))
       (bodies #f))
    (let ((bodies
	   (if (news-header:pre-read-body? header)
	       (case bodies
		 ((#f ALL) 'ALL)
		 ((SOME) 'SOME))
	       (case bodies
		 ((#f) #f)
		 ((SOME ALL) 'SOME)))))
      (let ((header (news-thread:next-header header news-header:real?)))
	(if (not header)
	    bodies
	    (loop header bodies))))))

(define (news-thread:all-articles-deleted? thread)
  (let loop ((header (news-thread:first-header thread news-header:real?)))
    (or (not header)
	(and (news-header:article-deleted? header)
	     (loop (news-thread:next-header header news-header:real?))))))

(define (news-thread:show-collapsed? thread)
  (and (not (news-thread:expanded? thread))
       (let ((header (news-thread:first-header thread news-header:real?)))
	 (and header
	      (news-thread:next-header header news-header:real?)))))

(define (news-thread:clear-indices! thread)
  (news-thread:for-each-header thread
    (lambda (header)
      (set-news-header:index! header #f))))