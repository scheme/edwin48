;;; -*-Scheme-*-
;;;
;;; $Id: midas.scm,v 1.27 2007/01/05 21:19:23 cph Exp $
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

;;;; Midas Mode


(define-command midas-mode
  "Enter Midas mode."
  ()
  (lambda () (set-current-major-mode! (ref-mode-object midas))))

(define-major-mode midas fundamental "Midas"
  "Major mode for editing assembly code."
  (lambda (buffer)
    (local-set-variable! syntax-table midas-mode:syntax-table buffer)
    (local-set-variable! comment-column 40 buffer)
    (local-set-variable! comment-locator-hook lisp-comment-locate buffer)
    (local-set-variable! comment-indent-hook midas-comment-indentation buffer)
    (local-set-variable! comment-start ";" buffer)
    (local-set-variable! comment-end "" buffer)
    (standard-alternate-paragraph-style! buffer)
    (local-set-variable! indent-line-procedure (ref-command insert-tab) buffer)
    (local-set-variable! local-abbrev-table
			 (ref-variable midas-mode-abbrev-table buffer)
			 buffer)
    (event-distributor/invoke! (ref-variable midas-mode-hook buffer) buffer)))

(define midas-mode:syntax-table (make-char-syntax-table))
(set-char-syntax! midas-mode:syntax-table #\; "<   ")
(set-char-syntax! midas-mode:syntax-table #\newline ">   ")
(set-char-syntax! midas-mode:syntax-table #\. "w   ")
(set-char-syntax! midas-mode:syntax-table #\' "'   ")
(set-char-syntax! midas-mode:syntax-table #\$ "'   ")
(set-char-syntax! midas-mode:syntax-table #\% "'   ")
(set-char-syntax! midas-mode:syntax-table #\# "'   ")

(define (midas-comment-indentation mark)
  (if (match-forward ";;;" mark)
      0
      (max (+ (mark-column (horizontal-space-start mark)) 1)
	   (ref-variable comment-column mark))))
