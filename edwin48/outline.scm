#| -*-Scheme-*-

$Id: outline.scm,v 1.15 2008/01/30 20:02:04 cph Exp $

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

;;;; Outline minor mode


(define (%forward-up-topic start end outline-pattern)
  (if (not (mark<= start end)) (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern start)))
	 (let next-topic ((start start))
	   (and start
		(re-search-forward outline-pattern (line-end start 0) end)
		(if level
		    (let ((found-level
			   (- (re-match-end-index 0)
			      (re-match-start-index 0))))
		      (if (>= found-level level)
			  (next-topic (re-match-end 0))
			  (re-match-start 0)))
		    (re-match-start 0)))))))

(define (%backward-up-topic end start outline-pattern)
  (if (not (mark<= start end)) (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern end)))
	 (and level
	      (let previous-topic ((end end))
		(and end
		     (re-search-backward outline-pattern end start)
		     (let ((found-level
			    (- (re-match-end-index 0)
			       (re-match-start-index 0))))
		       (if (>= found-level level)
			   (previous-topic (re-match-start 0))
			   (re-match-start 0)))))))))

(define (%forward-topic start end outline-pattern)
  (if (not (mark<= start end)) (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern start)))
	 (let next-topic ((start start))
	   (and start
		(re-search-forward outline-pattern (line-end start 0) end)
		(if level
		    (let ((found-level
			   (- (re-match-end-index 0)
			      (re-match-start-index 0))))
		      (cond ((= found-level level)
			     (re-match-start 0))
			    ((< found-level level)
			     #f)
			    (else (next-topic (re-match-end 0)))))
		    (re-match-start 0)))))))

(define (%backward-topic end start outline-pattern)
  (if (not (mark<= start end)) (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern end)))
	 (and level
	      (let previous-topic ((end end))
		(and end
		     (re-search-backward outline-pattern end start)
		     (let ((found-level
			    (- (re-match-end-index 0)
			       (re-match-start-index 0))))
		       (cond ((= found-level level)
			      (re-match-start 0))
			     ((< found-level level)
			      #f)
			     (else (previous-topic (re-match-start 0)))))))))))

(define (%forward-down-topic start end outline-pattern)
  (if (not (mark<= start end)) (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern start)))
	 (if level
	     (let next-topic ((start start))
	       (and start
		    (re-search-forward outline-pattern (line-end start 0) end)
		    (let ((found-level
			   (- (re-match-end-index 0)
			      (re-match-start-index 0))))
		      (if (<= found-level level)
			  (next-topic (re-match-end 0))
			  (re-match-start 0)))))
	     (and (re-search-forward outline-pattern start end)
		  (re-match-start 0))))))

(define (%backward-down-topic end start outline-pattern)
  (if (not (mark<= start end)) (error "Marks incorrectly related:" start end))
  (and (mark< start end)
       (let ((level (topic-level outline-pattern start)))
	 (if level
	     (let next-topic ((start start))
	       (and start
		    (re-search-backward outline-pattern end start)
		    (let ((found-level
			   (- (re-match-end-index 0)
			      (re-match-start-index 0))))
		      (if (<= found-level level)
			  (next-topic (re-match-end 0))
			  (re-match-start 0)))))
	     (and (re-search-forward outline-pattern start end)
		  (re-match-start 0))))))

(define (forward-one-topic mark)
  (%forward-topic mark
		  (group-end mark)
		  (ref-variable outline-pattern mark)))

(define (backward-one-topic mark)
  (%backward-topic mark
		  (group-start mark)
		  (ref-variable outline-pattern mark)))

(define (forward-up-one-topic mark)
  (%forward-up-topic mark
		     (group-end mark)
		     (ref-variable outline-pattern mark)))

(define (backward-up-one-topic mark)
  (%backward-up-topic mark
		      (group-start mark)
		      (ref-variable outline-pattern mark)))

(define (forward-down-one-topic mark)
  (%forward-down-topic mark
		       (group-end mark)
		       (ref-variable outline-pattern mark)))

(define (backward-down-one-topic mark)
  (%backward-down-topic mark
			(group-start mark)
			(ref-variable outline-pattern mark)))

(define forward-topic)
(define backward-topic)
(make-motion-pair forward-one-topic backward-one-topic
 (lambda (f b)
   (set! forward-topic f)
   (set! backward-topic b)
   unspecific))

(define forward-up-topic)
(define backward-up-topic)
(make-motion-pair forward-up-one-topic backward-up-one-topic
 (lambda (f b)
   (set! forward-up-topic f)
   (set! backward-up-topic b)
   unspecific))

(define forward-down-topic)
(define backward-down-topic)
(make-motion-pair forward-down-one-topic backward-down-one-topic
 (lambda (f b)
   (set! forward-down-topic f)
   (set! backward-down-topic b)
   unspecific))

(define-variable outline-pattern
  "Regexp describing outline topic beginnings.
The more characters match, the deeper our level in the outline."
  "^\*+"
  string?)

(define (topic-level outline-pattern mark)
  (and (re-search-backward outline-pattern
			   (line-end mark 0)
			   (group-start mark))
       (- (re-match-end-index 0) (re-match-start-index 0))))

(define (topic-region mark)
  (let ((end (group-end mark))
	(pattern (ref-variable outline-pattern mark)))
    (let ((level (topic-level pattern mark)))
      (if level
	  (make-region
	   (if (re-search-backward
		pattern (line-end mark 0) (group-start mark))
	       (re-match-start 0)
	       (error "Inconsistency detected."))
	   (or (let next-topic ((start (line-end mark 0)))
		 (and start
		      (re-search-forward pattern (line-end start 0) end)
		      (let ((found-level
			     (- (re-match-end-index 0)
				(re-match-start-index 0))))
			(if (<= found-level level)
			    (re-match-start 0)
			    (next-topic (re-match-end 0))))))
	       (group-end mark)))
	  (make-region (group-start mark) (group-end mark))))))

(define-command narrow-to-topic
  "Narrow to show the current outline level only."
  "d"
  (lambda (mark)
    (region-clip! (topic-region mark))))

(define-command forward-topic
  "Move forward to the next outline topic.  With arg, repeat, and go
backward if negative.  Outline topics match the regexp OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing forward-topic argument 'ERROR)))

(define-command backward-topic
  "Move backward to the next outline topic.  With arg, repeat, and go
forward if negative.  Outline topics match the regexp OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing backward-topic argument 'ERROR)))

(define-command forward-up-topic
  "Move forward the next-outermost outline topic.  With arg, repeat,
and go forward if negative.  Outline topics match the regexp
OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing forward-up-topic argument 'ERROR)))

(define-command backward-up-topic
  "Move backward the next-outermost outline topic.  With arg, repeat,
and go forward if negative.  Outline topics match the regexp
OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing backward-up-topic argument 'ERROR)))

(define-command forward-down-topic
  "Move forward to the next-innermost outline topic.  With arg,
repeat, and go backward if negative.  Outline topics match the regexp
OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing forward-down-topic argument 'ERROR)))

(define-command backward-down-topic
  "Move backward to the next-innermost outline topic.  With arg,
repeat, and go forward if negative.  Outline topics match the regexp
OUTLINE-PATTERN."
  "p"
  (lambda (argument)
    (move-thing backward-down-topic argument 'ERROR)))

(define-command outline-mode
  "Toggle outline mode.
With argument, turn outline mode on iff argument is positive."
  "P"
  (lambda (argument)
    (let ((mode (ref-mode-object outline)))
      (if (if argument
	      (> (command-argument-numeric-value argument) 0)
	      (not (current-minor-mode? mode)))
	  (enable-current-minor-mode! mode)
	  (disable-current-minor-mode! mode)))))

(define-minor-mode outline "Outline" "Minor mode for moving over outlines.")
(define-key 'outline '(#\C-c #\C-f) 'forward-topic)
(define-key 'outline '(#\C-c #\C-b) 'backward-topic)
(define-key 'outline '(#\C-c #\C-n) 'narrow-to-topic)
(define-key 'outline '(#\C-c #\C-a) 'forward-up-topic)
(define-key 'outline '(#\C-c #\C-u) 'backward-up-topic)
(define-key 'outline '(#\C-c #\C-d) 'forward-down-topic)