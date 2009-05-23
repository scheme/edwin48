;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface terminal-mode-interface
  (export ((with-current-input-terminal-mode) :syntax)
          ((with-input-terminal-mode) :syntax)
          call-with-input-terminal-mode
          input-terminal-mode
          set-input-terminal-mode))

(define-structure terminal-mode terminal-mode-interface
  (open scheme scsh-level-0 enumerated let-opt)
  (files terminal-mode))

(define-structure terminal-driver
    (export get-console-input-operations)
  (open scheme scsh-subset
        aliases ascii fixnum io-support keystroke keystroke-modifiers terminfo
        srfi-13 srfi-23)
  (files terminal-driver))

(define-structure keystroke-printer
    (export start-printer)
  (open scheme keystroke terminal-driver terminal-mode terminfo
        srfi-8 srfi-13 srfi-23)
  (files keystroke-printer))

(define-structure 1d-table-tests
    (export run-1d-table-test)
  (open scheme formats 1d-table)
  (files 1d-table-tests))

(define-structure edwin:paths edwin:paths/interface
  (open scheme aliases errors pathname io-support)
  (files ../edwin48/paths))

(define-structure edwin:doc-string edwin:doc-string/interface
  (open scheme aliases fixnum i/o pathname io-support
        srfi-13 srfi-89
        (modify errors (alias (error editor-error)))
        fixme
        edwin:paths)
  (files ../edwin48/docstr))

(define-structures
  ((edwin:command-table edwin:command-table/interface)
   (edwin:mode          edwin:mode/interface))
  (open scheme aliases edwin:command srfi-1 srfi-69 srfi-89 srfi-78
        define-record-type* errors keystroke aliases keystroke-discloser
        edwin:string-table edwin:doc-string sorting)
  (for-syntax (open scheme macro-helpers))
  (files ../edwin48/scsh/macros
         ../edwin48/modes
         ../edwin48/comtab))

(define-structures
  ((edwin:command edwin:command/interface)
   (edwin:variable/private (export set-variable-%default-value!
                                     set-variable-%value!)))
    (open (modify scheme  (hide integer->char string-fill! vector-fill!))
          (modify sorting (rename (vector-sort sort)))
          (modify ascii   (alias  (ascii->char integer->char)))
          aliases srfi-89 define-record-type* errors event-distributor fixnum
          (modify interrupts (expose call-after-gc!))
          io-support keystroke pathname rb-tree soosy weak-pair
          srfi-1 srfi-9 srfi-13 srfi-14 srfi-23 srfi-43
          edwin:string-table edwin:doc-string)
    (for-syntax (open scheme errors macro-helpers))
    (files ../edwin48/scsh/macros
           ../edwin48/comman)
    (begin
      (define editor-error error)))

(define-structure edwin:variable edwin:variable/interface
  (open scheme aliases srfi-89 srfi-69 errors
        define-record-type* edwin:string-table edwin:doc-string
        (modify edwin:command (expose editor-name/internal->external)))
  (for-syntax (open scheme errors macro-helpers))
  (files ../edwin48/scsh/macros
         ../edwin48/variable)
  (begin
    (define editor-error error)
    (define within-editor? #f)))

(define-structure edwin:string-table edwin:string-table/interface
  (open scheme aliases sorting srfi-43
        srfi-89 srfi-13 define-record-type*)
  (files ../edwin48/strtab))

(define-structure
  edwin:buffer  edwin:buffer/interface
  (open scheme aliases srfi-89 srfi-1 edwin:mode edwin:group
        srfi-13 define-record-type* errors event-distributor
        edwin:variable edwin:undo edwin:ring edwin:mark pathname)
  (for-syntax (open scheme))
  (files ../edwin48/buffer)
  (begin
    (define-variable mark-ring-maximum "maximum number of entries to keep in the mark ring" 10)
    (define-variable tab-width "Tab Width" 2)
    (define-variable mode-name "This mode name" 'fundamental)
    (define-variable editor-default-mode "Default Mode" 'fundamental)
    (define within-editor? #t)
    (define (current-buffer? x) #t)
    (define current-point 0)))

(define-structure edwin:undo edwin:undo/interface
 (open scheme aliases fixnum edwin:mark edwin:group
       srfi-1 errors edwin:variable edwin:command edwin:buffer
       (modify interrupts (expose call-after-gc!)))
 (files ../edwin48/undo)
 (begin
   (define (set-buffer-point! x y) #t)))

(define-structure edwin:ring edwin:ring/interface
  (open scheme aliases errors srfi-1 srfi-43)
  (files ../edwin48/ring))

(define-structures
  ((edwin:things edwin:things/interface)
   (edwin:region edwin:region/interface)
   (edwin:simple-editing edwin:simple-editing/interface))
  (open scheme aliases weak-pair errors fixnum srfi-89
        define-record-type* srfi-13 rb-tree)
  (files ../edwin48/things
         ../edwin48/regops
         ../edwin48/grpops
         ../edwin48/search
         ../edwin48/simple
         ../edwin48/struct))

(define-structures
  ((edwin:group edwin:group/interface)
   (edwin:mark edwin:mark/interface)
   (edwin:motion edwin:motion/interface)
   (edwin:search edwin:search/interface)
   (edwin:text-property edwin:text-property/interface))
  (open (modify scheme  (hide string-fill!))
        aliases weak-pair errors fixnum srfi-89 edwin:undo
        define-record-type* srfi-13 srfi-14 edwin:variable edwin:buffer rb-tree)
  (for-syntax (open scheme macro-helpers))
  (files ../edwin48/scsh/macros
         ../edwin48/struct
         ../edwin48/grpops
         ../edwin48/motion
         ../edwin48/utils
         ../edwin48/txtprp)
  (begin (define-variable buffer-reallocation-factor "rect fact" 2)))