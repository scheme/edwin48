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

(translate "=base" "..")

(config '(load "=base/config-macros.scm"))

(config '(load "=base/edwin48/srfi-packages.scm"
               "=base/edwin48/scsh/packages.scm"))

(config '(load "=base/terminfo/interfaces.scm"
               "=base/terminfo/scsh-packages.scm"))

(config '(load "=base/soosy/interfaces.scm"
               "=base/soosy/packages.scm"))

(config '(load "=base/pantene/interfaces.scm"
               "=base/pantene/packages.scm"))

(config '(load "=base/edwin48/interfaces.scm"
               "=base/edwin48/packages.scm"))

(config '(load "packages.scm"))