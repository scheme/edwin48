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

(config '(load "=base/pantene/edwin-interfaces.scm"
               "=base/pantene/edwin-packages.scm"))

(config '(load "=base/edwin48/scratch-interfaces.scm"
               "=base/edwin48/scratch-packages.scm"))

;(config '(load "test-groups.scm"))


