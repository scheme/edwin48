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

;;; EDWIN-EXPORT
(config '(load "config-macros.scm"))

(config '(load "terminfo/interfaces.scm"
               "terminfo/scsh-packages.scm"))

(config '(load "edwin48/srfi-packages.scm"
               "edwin48/scsh/packages.scm"
               "edwin48/interfaces.scm"
               "edwin48/packages.scm"))

;; (open 'edwin-groups)
;; (open 'edwin-marks)
;; (open 'edwin-regions)
;; (open 'edwin-motion)


