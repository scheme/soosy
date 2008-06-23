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

(config '(load "interfaces.scm"
               "packages.scm"))

(open 'soosy)
(load "test-points.scm")

