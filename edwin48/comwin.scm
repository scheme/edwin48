#| -*-Scheme-*-

$Id: comwin.scm,v 1.153 2008/01/30 20:01:59 cph Exp $

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

;;;; Combination Windows


;;; Combination windows are used to split a window into vertically or
;;; horizontally divided areas.  That window's initial superior must
;;; support the :NEW-ROOT-WINDOW! operation, but is otherwise not
;;; constrained.

;;; (==> WINDOW :NEW-ROOT-WINDOW! WINDOW*)

;;; This is called whenever the root is changed.  It need not do
;;; anything at all, but it is useful to keep track of the root.

;;; What happens is that the initial window may be split horizontally
;;; or vertically, as many times as desired.  The combination windows
;;; organize those splits into a tree.  The leaves of the tree are not
;;; combination windows, but are created from one of the other leaves
;;; by the :MAKE-LEAF operation.  Of course, the initial window is a
;;; leaf window too.

;;; If there is just one leaf window in the tree, then it is the root
;;; window also.  Otherwise, the root is a combination window.

;;; The leaf windows must be subclasses of COMBINATION-LEAF-WINDOW,
;;; and they must support these operations:

;;; (==> WINDOW :MAKE-LEAF)

;;; Make a new leaf which can be placed next to WINDOW.  For example,
;;; if WINDOW is a buffer window, the new window should also be a
;;; buffer window, visiting the same buffer, and sharing the same
;;; superior.

;;; (==> WINDOW :MINIMUM-X-SIZE)
;;; (==> WINDOW :MINIMUM-Y-SIZE)

;;; These define how small the window is allowed to be.  Since the
;;; combination window operations change the sizes of leaf windows,
;;; they need some idea of how small the leaves are allowed to get.
;;; So, no window will ever be set to a size that is below its minimum
;;; -- it will be deleted from the heirarchy instead.

;;; The values of these operations may depend on the window's position
;;; in the heirarchy, i.e. the SUPERIOR, NEXT-WINDOW, and
;;; PREVIOUS-WINDOW.  These are carefully arranged in the target
;;; configuration before the operations are invoked. This is intended
;;; to allow the leaves to have different minimums when there are
;;; optional borders which depend on their placement.

;;; Under no circumstances should the :MINIMUM-SIZE depend on the
;;; current size of a leaf window.

(define-class combination-leaf-window vanilla-window
  (next-window previous-window))

(define (window-next window)
  (with-instance-variables combination-leaf-window window ()
    next-window))

(define (set-window-next! window window*)
  (with-instance-variables combination-leaf-window window (window*)
    (set! next-window window*)))

(define (window-previous window)
  (with-instance-variables combination-leaf-window window ()
    previous-window))

(define (set-window-previous! window window*)
  (with-instance-variables combination-leaf-window window (window*)
    (set! previous-window window*)))

(define (link-windows! previous next)
  (set-window-previous! next previous)
  (set-window-next! previous next))

(define-class combination-window combination-leaf-window
  (vertical? child))

(define (combination-vertical? window)
  (with-instance-variables combination-window window ()
    vertical?))

(define (set-combination-vertical! window v)
  (with-instance-variables combination-window window (v)
    (set! vertical? v)))

(define (combination-child window)
  (with-instance-variables combination-window window ()
    child))

(define (set-combination-child! window window*)
  (with-instance-variables combination-window window (window*)
    (set! child window*)
    (set-window-previous! window* #f)))

(define (combination? window)
  (object-of-class? combination-window window))

(define (leaf? window)
  (and (object? window)
       (subclass? (object-class window) combination-leaf-window)
       (not (eq? (object-class window) combination-window))))

(define (check-leaf-window window name)
  (if (not (leaf? window))
      (error:wrong-type-argument window "window" name)))

;;;; Leaf Ordering

(define (window+ leaf n)
  (check-leaf-window leaf 'WINDOW+)
  (cond ((positive? n) (%window+ leaf n))
	((negative? n) (%window- leaf (- n)))
	(else leaf)))

(define (window- leaf n)
  (check-leaf-window leaf 'WINDOW-)
  (cond ((positive? n) (%window- leaf n))
	((negative? n) (%window+ leaf (- n)))
	(else leaf)))

(define (%window+ leaf n)
  (if (= n 1)
      (%window1+ leaf)
      (%window+ (%window1+ leaf) (-1+ n))))

(define (%window- leaf n)
  (if (= n 1)
      (%window-1+ leaf)
      (%window- (%window-1+ leaf) (-1+ n))))

(define (window1+ leaf)
  (check-leaf-window leaf 'WINDOW1+)
  (%window1+ leaf))

(define (window-1+ leaf)
  (check-leaf-window leaf 'WINDOW-1+)
  (%window-1+ leaf))

(define (window0 window)
  (if (not (and (object? window)
		(subclass? (object-class window) combination-leaf-window)))
      (error:wrong-type-argument window "window" 'WINDOW0))
  (window-leftmost-leaf (window-root window)))

(define (%window1+ leaf)
  (window-leftmost-leaf
   (or (window-next leaf)
       (if (combination? (window-superior leaf))
	   (find-window-with-next (window-superior leaf))
	   leaf))))

(define (%window-1+ leaf)
  (window-rightmost-leaf
   (or (window-previous leaf)
       (if (combination? (window-superior leaf))
	   (find-window-with-previous (window-superior leaf))
	   leaf))))

(define (find-window-with-next combination)
  (or (window-next combination)
      (if (combination? (window-superior combination))
	  (find-window-with-next (window-superior combination))
	  combination)))

(define (find-window-with-previous combination)
  (or (window-previous combination)
      (if (combination? (window-superior combination))
	  (find-window-with-previous (window-superior combination))
	  combination)))

(define (window-first window)
  (if (window-previous window)
      (window-first (window-previous window))
      window))

(define (window-last window)
  (if (window-next window)
      (window-last (window-next window))
      window))

(define (window-root window)
  (if (combination? (window-superior window))
      (window-root (window-superior window))
      window))

(define (window-leftmost-leaf window)
  (if (combination? window)
      (window-leftmost-leaf (combination-child window))
      window))

(define (window-rightmost-leaf window)
  (if (combination? window)
      (window-rightmost-leaf (window-last (combination-child window)))
      window))

(define (window-has-no-neighbors? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-NO-NEIGHBORS?)
  (not (combination? (window-superior leaf))))

(define (window-has-horizontal-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-HORIZONTAL-NEIGHBOR?)
  (%window-has-horizontal-neighbor? leaf))

(define (%window-has-horizontal-neighbor? window)
  (let ((superior (window-superior window)))
    (and (combination? superior)
	 (or (not (combination-vertical? superior))
	     (%window-has-horizontal-neighbor? superior)))))

(define (window-has-vertical-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-VERTICAL-NEIGHBOR?)
  (%window-has-vertical-neighbor? leaf))

(define (%window-has-vertical-neighbor? window)
  (let ((superior (window-superior window)))
    (and (combination? superior)
	 (or (combination-vertical? superior)
	     (%window-has-vertical-neighbor? superior)))))

(define (window-has-right-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-RIGHT-NEIGHBOR?)
  (%window-has-right-neighbor? leaf))

(define (%window-has-right-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (not (combination-vertical? (window-superior window)))
		(window-next window))
	   (%window-has-right-neighbor? (window-superior window)))))

(define (window-has-left-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-LEFT-NEIGHBOR?)
  (%window-has-left-neighbor? leaf))

(define (%window-has-left-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (not (combination-vertical? (window-superior window)))
		(window-previous window))
	   (%window-has-left-neighbor? (window-superior window)))))

(define (window-has-up-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-UP-NEIGHBOR?)
  (%window-has-up-neighbor? leaf))

(define (%window-has-up-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (combination-vertical? (window-superior window))
		(window-previous window))
	   (%window-has-up-neighbor? (window-superior window)))))

(define (window-has-down-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-DOWN-NEIGHBOR?)
  (%window-has-down-neighbor? leaf))

(define (%window-has-down-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (combination-vertical? (window-superior window))
		(window-next window))
	   (%window-has-down-neighbor? (window-superior window)))))

;;;; Creation

(define (window-split-horizontally! leaf #!optional n)
  (check-leaf-window leaf 'WINDOW-SPLIT-HORIZONTALLY!)
  (without-interrupts
   (lambda ()
     (let ((n
	    (if (or (default-object? n) (not n))
		(quotient (window-x-size leaf) 2)
		n))
	   (x (window-x-size leaf))
	   (y (window-y-size leaf)))
       (let ((n* (- x n))
	     (new (allocate-leaf! leaf #f)))
	 (let ((combination (window-superior leaf)))
	   (inferior-start (window-inferior combination leaf)
	     (lambda (x y)
	       (set-inferior-start! (window-inferior combination new)
				    (+ x n)
				    y))))
	 (if (or (< n (==> leaf :minimum-x-size))
		 (< n* (==> new :minimum-x-size)))
	     (begin
	       (deallocate-leaf! new)
	       #f)
	     (begin
	       (==> leaf :set-x-size! n)
	       (==> new :set-size! n* y)
	       new)))))))

(define (window-split-vertically! leaf #!optional n)
  (check-leaf-window leaf 'WINDOW-SPLIT-VERTICALLY!)
  (without-interrupts
   (lambda ()
     (let ((n
	    (if (or (default-object? n) (not n))
		(quotient (window-y-size leaf) 2)
		n))
	   (x (window-x-size leaf))
	   (y (window-y-size leaf)))
       (let ((n* (- y n))
	     (new (allocate-leaf! leaf #t)))
	 (let ((combination (window-superior leaf)))
	   (inferior-start (window-inferior combination leaf)
	     (lambda (x y)
	       (set-inferior-start! (window-inferior combination new)
				    x
				    (+ y n)))))
	 (if (or (< n (==> leaf :minimum-y-size))
		 (< n* (==> new :minimum-y-size)))
	     (begin
	       (deallocate-leaf! new)
	       #f)
	     (begin
	       (==> leaf :set-y-size! n)
	       (==> new :set-size! x n*)
	       new)))))))

(define (allocate-leaf! leaf v)
  (let ((superior (window-superior leaf)))
    (if (or (not (combination? superior))
	    (not (eq? v (combination-vertical? superior))))
	(let ((combination (==> superior :make-inferior combination-window)))
	  (==> superior :set-inferior-position! combination
	       (==> superior :inferior-position leaf))
	  (set-combination-vertical! combination v)
	  (window-replace! leaf combination)
	  (set-combination-child! combination leaf)
	  (set-window-next! leaf #f)
	  (==> superior :delete-inferior! leaf)
	  (add-inferior! combination leaf)
	  (set-inferior-start! (window-inferior combination leaf) 0 0)
	  (set-window-size! combination
			    (window-x-size leaf)
			    (window-y-size leaf)))))
  (let ((new (==> leaf :make-leaf)))
    (set-window-next! new (window-next leaf))
    (if (window-next leaf) (set-window-previous! (window-next leaf) new))
    (link-windows! leaf new)
    new))

(define (deallocate-leaf! leaf)
  (unlink-leaf! leaf)
  (maybe-delete-combination! (window-superior leaf)))

;;;; Deletion

(define (window-delete! leaf #!optional merge-into)
  (check-leaf-window leaf 'WINDOW-DELETE!)
  (if (window-live? leaf)
      (let ((screen (window-screen leaf)))
	(without-interrupts
	 (lambda ()
	   (let ((superior (window-superior leaf))
		 (next (window-next leaf))
		 (previous (window-previous leaf))
		 (x-size (window-x-size leaf))
		 (y-size (window-y-size leaf)))
	     (if (not (combination? superior))
		 (editor-error "Window has no neighbors; can't delete"))
	     (let ((adjust-size!
		    (lambda (window)
		      (if (current-window? leaf)
			  (select-window
			   (let loop ((window window))
			     (if (combination? window)
				 (loop (combination-child window))
				 window))))
		      (unlink-leaf! leaf)
		      (if (combination-vertical? superior)
			  (==> window :set-y-size!
			       (+ (window-y-size window) y-size))
			  (==> window :set-x-size!
			       (+ (window-x-size window) x-size))))))
	       (let ((do-next
		      (lambda ()
			(adjust-size! next)
			(let ((inferior (window-inferior superior next)))
			  (if (combination-vertical? superior)
			      (set-inferior-y-start!
			       inferior
			       (- (inferior-y-start inferior) y-size))
			      (set-inferior-x-start!
			       inferior
			       (- (inferior-x-start inferior) x-size))))))
		     (do-previous
		      (lambda ()
			(adjust-size! previous))))
		 (cond ((and (not (default-object? merge-into))
			     merge-into
			     (or (eq? merge-into next)
				 (eq? merge-into previous)))
			(if (eq? merge-into next)
			    (do-next)
			    (do-previous)))
		       (next (do-next))
		       (previous (do-previous))
		       (else
			(error "Combination with single child:" superior)))))
	     (maybe-delete-combination! superior))))
	(maybe-deselect-buffer-layout screen))))

(define (unlink-leaf! leaf)
  (let ((combination (window-superior leaf))
	(next (window-next leaf))
	(previous (window-previous leaf)))
    (==> leaf :kill!)
    (delete-inferior! combination leaf)
    (if previous
	(set-window-next! previous next)
	(set-combination-child! combination next))
    (if next
	(set-window-previous! next previous))))

(define (maybe-delete-combination! combination)
  (let ((child (combination-child combination)))
    (if (not (window-next child))
	(begin
	  (delete-inferior! combination child)
	  (==> (window-superior combination) :replace-inferior!
	       combination
	       child)
	  (window-replace! combination child)))))

(define (window-replace! old new)
  (with-instance-variables combination-leaf-window old (new)
    (cond ((not (combination? superior))
	   (==> superior :new-root-window! new))
	  ((and (combination? new)
		(eq? (combination-vertical? superior)
		     (combination-vertical? new)))
	   (let ((first (combination-child new)))
	     (inferior-start (window-inferior superior new)
	       (lambda (xs ys)
		 (define (loop window)
		   (add-inferior! superior window)
		   (inferior-start (window-inferior new window)
		     (lambda (x y)
		       (set-inferior-start! (window-inferior superior window)
					    (+ xs x)
					    (+ ys y))))
		   (if (window-next window)
		       (loop (window-next window))))
		 (delete-inferior! superior new)
		 (loop first)))
	     (if next-window
		 (link-windows! (window-last first) next-window))
	     (if previous-window
		 (link-windows! previous-window first)
		 (set-combination-child! superior first))))
	  (else
	   (if next-window
	       (link-windows! new next-window))
	   (if previous-window
	       (link-windows! previous-window new)
	       (set-combination-child! superior new))))))

;;;; Sizing

(define (window-grow! vertical? size min-size set-w-size! start set-start!
		      scale-combination-inferiors!)
  (lambda (leaf delta)
    (check-leaf-window leaf 'WINDOW-GROW!)
    (without-interrupts
     (lambda ()
       (let ((leaf
	      (let loop ((leaf leaf))
		(let ((combination (window-superior leaf)))
		  (if (not (combination? combination))
		      (editor-error "Can't grow this window "
				    (if vertical?
					"vertically"
					"horizontally")))
		  (if (boolean=? vertical? (combination-vertical? combination))
		      leaf
		      (loop combination))))))
	 (let ((new-size (+ (size leaf) delta))
	       (combination (window-superior leaf))
	       (next (window-next leaf))
	       (previous (window-previous leaf)))
	   (if (> new-size (size combination))
	       (begin
		 (set! new-size (size combination))
		 (set! delta (- new-size (size leaf)))))
	   (cond ((< new-size (min-size leaf))
		  (window-delete! leaf))
		 ((and next (>= (- (size next) delta) (min-size next)))
		  (let ((inferior (window-inferior combination next)))
		    (set-start! inferior (+ (start inferior) delta)))
		  (set-w-size! next (- (size next) delta))
		  (set-w-size! leaf new-size))
		 ((and previous
		       (>= (- (size previous) delta) (min-size previous)))
		  (let ((inferior (window-inferior combination leaf)))
		    (set-start! inferior (- (start inferior) delta)))
		  (set-w-size! previous (- (size previous) delta))
		  (set-w-size! leaf new-size))
		 (else
		  (scale-combination-inferiors! combination
						(- (size combination) new-size)
						leaf)
		  ;; Scaling may have deleted all other inferiors.
		  ;; If so, leaf has replaced combination.
		  (set-w-size! leaf
			       (if (eq? combination (window-superior leaf))
				   new-size
				   (size combination)))))))))))

;;; (SCALE-COMBINATION-INFERIORS! COMBINATION NEW-ROOM EXCEPT)

;;; Change all of the inferiors of COMBINATION (except EXCEPT) to use
;;; NEW-ROOM's worth of space.  EXCEPT, if given, should not be
;;; changed in size, but should be moved if its neighbors change.  It
;;; is assumed that EXCEPT is given only for case where the
;;; combination's VERTICAL? flag is the same as V.

;;; General strategy:

;;; If the window is growing, we can simply change the sizes of the
;;; inferiors.  However, if it is shrinking, we must be more careful
;;; because some or all of the inferiors can be deleted.  So in that
;;; case, before any sizes are changed, we find those inferiors that
;;; will be deleted and delete them.  If we delete all of the
;;; inferiors, then we are done: this window has also been deleted.
;;; Otherwise, we can then perform all of the changes, knowing that no
;;; window will grow too small.

(define (scale-combination-inferiors! v size min-size set-w-size! set-start!)
  (lambda (combination new-room except)
    (let ((kernel
	   (lambda (old-room collect-deletions change-inferiors)
	     (cond ((< old-room new-room)
		    (change-inferiors))
		   ((> old-room new-room)
		    (for-each window-delete! (collect-deletions))
		    (if (not (null? (window-inferiors combination)))
			(change-inferiors))))))
	  (child (combination-child combination))
	  (c-size (size combination)))
      (if (not (eq? (combination-vertical? combination) v))
	  (kernel
	   c-size
	   (lambda ()
	     (let loop ((window child))
	       (let ((deletions
		      (if (window-next window)
			  (loop (window-next window))
			  '())))
		 (if (< new-room (min-size window))
		     (cons window deletions)
		     deletions))))
	   (lambda ()
	     (let loop ((window child))
	       (set-w-size! window new-room)
	       (if (window-next window)
		   (loop (window-next window))))))
	  (let ((old-room (if except (- c-size (size except)) c-size)))
	    (kernel
	     old-room
	     (lambda ()
	       (let loop
		   ((window child) (old-room old-room) (new-room new-room))
		 (cond ((eq? window except)
			(if (window-next window)
			    (loop (window-next window) old-room new-room)
			    '()))
		       ((not (window-next window))
			(if (< new-room (min-size window))
			    (list window)
			    '()))
		       (else
			(let* ((old-s (size window))
			       (new-s (quotient (* old-s new-room) old-room))
			       (deletions
				(loop (window-next window)
				      (- old-room old-s)
				      (- new-room new-s))))
			  (if (< new-s (min-size window))
			      (cons window deletions)
			      deletions))))))
	     (lambda ()
	       (let loop
		   ((window child)
		    (start 0)
		    (old-room old-room)
		    (new-room new-room))
		 (set-start! (window-inferior combination window) start)
		 (cond ((eq? window except)
			(if (window-next window)
			    (loop (window-next window)
				  start
				  old-room
				  new-room)))
		       ((not (window-next window))
			(set-w-size! window new-room))
		       (else
			(let* ((old-s (size window))
			       (new-s (quotient (* old-s new-room) old-room)))
			  (set-w-size! window new-s)
			  (loop (window-next window)
				(+ start new-s)
				(- old-room old-s)
				(- new-room new-s)))))))))))))

(define (window-min-x-size window)
  (==> window :minimum-x-size))

(define (send-window-x-size! window x)
  (==> window :set-x-size! x))

(define (window-min-y-size window)
  (==> window :minimum-y-size))

(define (send-window-y-size! window y)
  (==> window :set-y-size! y))

(define scale-combination-inferiors-x!
  (scale-combination-inferiors! #f window-x-size window-min-x-size
				send-window-x-size! set-inferior-x-start!))

(define scale-combination-inferiors-y!
  (scale-combination-inferiors! #t window-y-size window-min-y-size
				send-window-y-size! set-inferior-y-start!))

(define window-grow-horizontally!
  (window-grow! #f window-x-size window-min-x-size send-window-x-size!
		inferior-x-start set-inferior-x-start!
		scale-combination-inferiors-x!))

(define window-grow-vertically!
  (window-grow! #t window-y-size window-min-y-size send-window-y-size!
		inferior-y-start set-inferior-y-start!
		scale-combination-inferiors-y!))

(define-method combination-window (:minimum-x-size combination)
  (==> (window-leftmost-leaf combination) :minimum-x-size))

(define-method combination-window (:minimum-y-size combination)
  (==> (window-leftmost-leaf combination) :minimum-y-size))

(define (set-combination-x-size! combination x)
  (scale-combination-inferiors-x! combination x #f)
  (set-window-x-size! combination x))

(define (set-combination-y-size! combination y)
  (scale-combination-inferiors-y! combination y #f)
  (set-window-y-size! combination y))

(define (set-combination-size! combination x y)
  (scale-combination-inferiors-x! combination x #f)
  (scale-combination-inferiors-y! combination y #f)
  (set-window-size! combination x y))

(define-method combination-window :set-x-size! set-combination-x-size!)
(define-method combination-window :set-y-size! set-combination-y-size!)
(define-method combination-window :set-size! set-combination-size!)

(define-method combination-window
  (:leaf-containing-coordinates combination x y)
  (inferior-containing-coordinates combination x y leaf?))

(define-method combination-leaf-window (:leaf-containing-coordinates leaf x y)
  (values leaf x y))