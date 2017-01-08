;;; -*- Mode: Scheme; scheme48-package: (exec) -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; load.scm - Script for loading Edwin48
;;;


(user)

(config '(load "config-macros.scm"))

;; Load srfi's before the other packages.
(config '(load "edwin48/srfi-packages.scm"
               "edwin48/scsh/packages.scm"))

(config '(load "terminfo/interfaces.scm"
               "terminfo/scsh-packages.scm"))

(config '(load "soosy/interfaces.scm"
               "soosy/packages.scm"))

(config '(load "pantene/edwin-interfaces.scm"
               "pantene/edwin-packages.scm"))

(config '(load "edwin48/interfaces.scm"
               "edwin48/packages.scm"))

(config '(load "scratch/s48-apropos.scm"))

(config '(load "cosmacs/packages.scm"))
