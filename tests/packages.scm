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

(define-structure edwin:string-table edwin:string-table/interface
  (open scheme aliases define-record-type*
        (modify sorting (rename (vector-sort sort)))
        mit-regexp srfi-13 srfi-43 srfi-89)
  (files ../edwin48/strtab))

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

(define-interface edwin:command-table/interface
  (export comtab-entry local-comtab-entry
          make-comtab
          comtab-key?
          prefix-key-list?
          define-key
          define-prefix-key
          comtab->alist
          comtab-key-bindings))

(define-interface edwin:command/interface
  (export ((define-command)     :syntax)
          ((ref-command-object) :syntax)
          ((ref-command)        :syntax)
          command-name
          command-interactive-specification
          command-procedure
          command-description
          command-name-string
          editor-name/internal->external
          editor-name/external->internal
          make-command
          name->command
          ->command
          command?
          copy-command))

(define-structure edwin:command edwin:command/interface
    (open (modify scheme  (hide integer->char string-fill! vector-fill!))
          (modify sorting (rename (vector-sort sort)))
          (modify ascii   (alias  (ascii->char integer->char)))
          aliases srfi-89 define-record-type* errors event-distributor fixnum
          (modify interrupts (expose call-after-gc!))
          io-support keystroke pathname rb-tree soosy weak-pair
          srfi-1 srfi-9 srfi-13 srfi-14 srfi-23 srfi-43 srfi-69
          edwin:string-table edwin:doc-string)
    (for-syntax (open scheme errors macro-helpers))
    (files ../edwin48/scsh/macros
           ../edwin48/comman)
    (begin
      (define editor-error error)
      (define within-editor? #f)))

(define-interface edwin:string-table/interface
  (export make-string-table
          alist->string-table
          string-table-ci?
          string-table-get
          string-table-put!
          string-table-remove!
          string-table-complete
          string-table-completions
          string-table-apropos))

(define-structure edwin:string-table edwin:string-table/interface
  (open scheme aliases sorting srfi-43
        srfi-89 srfi-13 define-record-type*)
  (files ../edwin48/strtab))

