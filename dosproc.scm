#| -*-Scheme-*-

$Id: dosproc.scm,v 1.14 2007/01/05 21:19:23 cph Exp $

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

;;;; Dummy subprocess support
;; package: (edwin process)


(define subprocesses-available?
  #f)

(define (process-list)
  '())

(define (get-buffer-process buffer)
  buffer
  #f)

(define (buffer-processes buffer)
  buffer
  '())

(define (process-operation name)
  (lambda (process)
    (editor-error "Processes not implemented" name process)))

(define delete-process
  (process-operation 'DELETE-PROCESS))

(define (process-status-changes?)
  #f)

(define (process-output-available?)
  #f)