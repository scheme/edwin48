;;; -*- mode: scheme; scheme48-package: (config) -*-

(define-structures
    ((edwin-groups  edwin-groups-interface)
     (edwin-marks   edwin-marks-interface)
     (edwin-regions edwin-regions-interface))
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
        edwin-groups
        edwin-marks
        edwin-regions
        let-opt
        srfi-23
        )
  (files motion))

(define-structure edwin-string-table edwin-string-table-interface
  (open scheme
        aliases
        define-record-type*
        define-opt
	(modify sorting (rename (vector-sort sort)))
        srfi-13)
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
  (open scheme aliases fixnum errors define-opt)
  (files docstr))