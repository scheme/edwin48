;;; -*- mode: scheme; scheme48-package: (config) -*-

(define-structures
    ((edwin:basic-command    edwin:basic-command/interface)
     (edwin:buffer           edwin:buffer/interface)
     (edwin:bufferset        edwin:bufferset/interface)
     (edwin:command          edwin:command/interface)
     (edwin:command-reader   edwin:command-table/interface)
     (edwin:command-table    edwin:command-table/interface)
     (edwin:current-state    edwin:current-state/interface)
     (edwin:display-imaging  edwin:display-imaging/interface)
     (edwin:display-type     edwin:display-type/interface)
     (edwin:doc-string       edwin:doc-string/interface)
     (edwin:editor           edwin:editor/interface)
     (edwin:editor-defn      edwin:editor-definition/interface)
     (edwin:group            edwin:group/interface)
     (edwin:kill-command     edwin:kill-command/interface)
     (edwin:mark             edwin:mark/interface)
     (edwin:mode             edwin:mode/interface)
     (edwin:modeline         edwin:modeline/interface)
     (edwin:motion           edwin:motion/interface)
     (edwin:region           edwin:region/interface)
     (edwin:screen           edwin:screen/interface)
     (edwin:simple-editing   edwin:simple-editing/interface)
     (edwin:text-property    edwin:text-property/interface)
     (edwin:undo             edwin:undo/interface)
     (edwin:variable         edwin:variable/interface)
     (edwin:variable/private (export set-variable-%default-value!
                                     set-variable-%value!))
     (edwin:window-system    edwin:window-system/interface))
    (open (modify scheme  (hide integer->char string-fill! vector-fill!))
          (modify sorting (rename (vector-sort sort)))
          (modify ascii   (alias  (ascii->char integer->char)))
          1d-table aliases define-record-type* errors event-distributor
          extended-ports fixme fixnum i/o
          (modify interrupts (expose call-after-gc!))
          io-support keystroke pathname queues rb-tree soosy weak-pair
          srfi-1 srfi-9 srfi-13 srfi-14 srfi-23 srfi-43 srfi-69 srfi-89
          edwin:input-event edwin:paths edwin:ring
          edwin:string-table edwin:utilities)
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros) ;; macros need to be looked at first
         struct
         grpops
         txtprp
         regops
         motion
         search
         image
         comman
         docstr
         comtab
         modes
         buffer
         bufset
         display
         screen

         ;; window system begins here
         window
         utlwin
         bufwin
         bufwfs
         bufwiu
         bufwmc
         comwin
         modwin
         buffrm
         edtfrm

         ;; calias
         edtstr
         editor
         curren
         things
         modlin
         input
         prompt
         comred
         simple
         undo
         basic
         kilcom
         wincom))

(define-structure edwin:string-table edwin:string-table/interface
  (open scheme aliases define-record-type*
        (modify sorting (rename (vector-sort sort)))
        mit-regexp srfi-13 srfi-43 srfi-89)
  (files strtab))

(define-structure edwin:utilities edwin:utilities/interface
 (open (modify scheme (hide string-fill!))
       srfi-13 srfi-14 srfi-89
       aliases errors fixnum i/o pathname terminal-support util weak-pair)
   (files utils strpad))

(define-structure edwin:ring edwin:ring/interface
  (open scheme aliases errors srfi-1)
  (files ring))

(define-structure edwin:paths edwin:paths/interface
  (open scheme aliases errors pathname io-support)
  (files paths))

(define-structure edwin:terminal-screen edwin:terminal-screen/interface
  (open aliases
        define-record-type*
        edwin:display-type
        edwin:screen
        event-distributor
        errors
        fixnum
        io-support
        keystroke
        scheme
        srfi-1
        srfi-6
        srfi-13
        terminal-support
        terminfo)
  (files terminal))

(define-structure edwin:input-event edwin:input-event/interface
  (open scheme errors srfi-9)
  (files input-event))