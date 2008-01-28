;;; -*- Mode: Scheme; scheme48-package: (exec) -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; load.scm - Script for loading Edgar
;;;

(user)

(config '(load "s48-let-opt.scm"
	       "srfi-packages.scm"
               "s48-packages.scm"
               "interfaces.scm"
               "packages.scm"))

;; (open 'edwin-groups)
;; (open 'edwin-marks)
;; (open 'edwin-regions)
;; (open 'edwin-motion)


