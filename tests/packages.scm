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

(define-structure keystroke-printer
    (export start-printer)
  (open scheme scsh-subset
        aliases ascii fixnum formats io-support keystroke keystroke-modifiers terminal-mode terminfo
        srfi-8 srfi-13 srfi-23)
  (files keystroke-printer))