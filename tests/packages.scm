;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure 1d-table-tests
    (export run-1d-table-test)
  (open scheme formats 1d-table)
  (files 1d-table-tests))

(define-structure comtab-tests
    (export check-report)
  (open scheme edwin:command-table srfi-78)
  (files test-comtab))

