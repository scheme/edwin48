;;; -*- mode: scheme; scheme48-package: (config) -*-

(define-structures
    ((edwin-group  edwin-group-interface)
     (edwin-mark   edwin-mark-interface)
     (edwin-region edwin-region-interface))
    (open (modify scheme (hide string-fill!))
          ;; edwin-utilities
	  aliases
	  errors
          define-record-type*
	  fixnum
          weak-pair
	  srfi-1
          srfi-13
          srfi-23
          )
  (files struct
         grpops
         regops))

(define-structure
    edwin-motion edwin-motion-interface
  (open scheme
	aliases
	fixnum
	define-opt
        edwin-group
        edwin-mark
        edwin-region
        let-opt
        srfi-23
        )
  (files motion))

(define-structure edwin-string-table edwin-string-table-interface
  (open scheme aliases define-record-type* define-opt
	(modify sorting (rename (vector-sort sort)))
        mit-regexp srfi-13 srfi-43)
  (files strtab))

(define-structure
    edwin-utilities edwin-utilities-interface
  (open scheme aliases let-opt srfi-13 util)
  (files utils))

(define-structure
    edwin-ring edwin-ring-interface
  (open scheme aliases errors srfi-1)
  (files ring))

(define-structure
    edwin-doc-string edwin-doc-string-interface
  (open scheme aliases fixnum errors define-opt i/o pathname mit-fileio srfi-13
        fixme
        edwin-paths)
  (files docstr))

(define-structure edwin-paths edwin-paths-interface
  (open scheme aliases errors pathname mit-fileio)
  (files paths))

(define-structures
    ((edwin-command          edwin-command-interface)
     (edwin-variable         edwin-variable-interface)
     (edwin-variable/private (export set-variable-%default-value!
                                     set-variable-%value!)))
    (open scheme aliases define-record-type* define-opt errors
          srfi-69
          edwin-doc-string edwin-string-table
          fixme)
  (for-syntax (open scheme errors macro-helpers))
  (files comman s48-macros))

