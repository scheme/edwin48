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
