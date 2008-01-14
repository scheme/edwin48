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
	  lists
          weak-pair
          srfi-13
          srfi-23
	  strings
          )
  (files struct
         grpops
         regops))

(define-structure
    edwin-motion edwin-motion-interface
  (open scheme
	aliases
	fixnum
        edwin-groups
        edwin-marks
        edwin-regions
        let-opt
        srfi-23
	strings
        )
  (files motion))

(define-structure
    edwin-utilities edwin-utilities-interface
  (open scheme let-opt srfi-13 strings util)
  (files utils))

(define-structure
    edwin-ring edwin-ring-interface
  (open scheme aliases signals srfi-1)
  (files ring))