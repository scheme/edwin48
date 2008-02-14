;;; -*- mode: scheme; scheme48-package: (config) -*-

(define-structures
    ((edwin-buffer           edwin-buffer-interface)
     (edwin-bufferset        edwin-bufferset-interface)
     (edwin-basic-command    edwin-basic-command-interface)
     (edwin-command          edwin-command-interface)
     (edwin-command-table    edwin-command-table-interface)
     (edwin-current-state    edwin-current-state-interface)
     (edwin-group            edwin-group-interface)
     (edwin-mark             edwin-mark-interface)
     (edwin-mode             edwin-mode-interface)
     (edwin-modeline         edwin-modeline-interface)
     (edwin-motion           edwin-motion-interface)
     (edwin-region           edwin-region-interface)
     (edwin-simple-editing   edwin-simple-editing-interface)
     (edwin-text-property    edwin-text-property-interface)
     (edwin-undo             edwin-undo-interface)
     (edwin-variable         edwin-variable-interface)
     (edwin-variable/private (export set-variable-%default-value!
                                     set-variable-%value!)))
    (open (modify scheme (hide string-fill!))
          (modify sorting (rename (vector-sort sort)))
          ascii aliases define-opt define-record-type* errors event-distributor fixme fixnum
          pathname rb-tree weak-pair
          srfi-1 srfi-13 srfi-14 srfi-23 srfi-69
          edwin-doc-string edwin-ring edwin-string-table edwin-utilities)
  (for-syntax (open scheme errors macro-helpers))
  (files basic
         buffer
         bufset
         comman
         comtab
         curren
         grpops
         modes
         motion
         regops
         s48-macros
         simple
         struct
         txtprp
         undo))

(define-structure edwin-string-table edwin-string-table-interface
  (open scheme aliases define-record-type* define-opt
        (modify sorting (rename (vector-sort sort)))
        mit-regexp srfi-13 srfi-43)
  (files strtab))

(define-structure
    edwin-utilities edwin-utilities-interface
 (open scheme i/o posix-files
       aliases errors fixnum pathname util weak-pair
       srfi-13 srfi-14)
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

(define-structure edwin-command-table edwin-command-table-interface
  (open scheme (modify sorting (rename (vector-sort sort)))
        aliases define-record-type* define-opt errors fixnum
        srfi-1 srfi-14 edwin-string-table edwin-utilities)
  (files comtab))
