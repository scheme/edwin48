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

(config '(load "common/s48-let-opt.scm"))

(config '(load "terminfo/interfaces.scm"
               "terminfo/s48-packages.scm"))

(config '(load "edwin48/srfi-packages.scm"
               "edwin48/s48-packages.scm"
               "edwin48/interfaces.scm"
               "edwin48/packages.scm"))

;; (open 'edwin-groups)
;; (open 'edwin-marks)
;; (open 'edwin-regions)
;; (open 'edwin-motion)


