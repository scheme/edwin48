;;; -*- Mode: Scheme; scheme48-package: (exec) -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; This script loads the code for Edwin48 and its dependencies. To run it, execute the following on
;;; the scsh repl from this directory:
;;;
;;; > ,exec ,load load.scm

(user)

(config '(load "config-macros.scm"))

;; Load srfi's before the other packages.
(config '(load "edwin48/srfi-packages.scm"
               "edwin48/scsh/packages.scm"))

(config '(load "terminfo/interfaces.scm"
               "terminfo/scsh-packages.scm"))

(config '(load "soosy/interfaces.scm"
               "soosy/packages.scm"))

(config '(load "pantene/interfaces.scm"
               "pantene/packages.scm"))

(config '(load "edwin48/interfaces.scm"
               "edwin48/packages.scm"))

(config '(load "scratch/s48-apropos.scm"))
