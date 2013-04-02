;; (define-interface cosmacs:command-reader/interface
;;   (export dispatch-on-key))

;; (define-structures
;;   ((cosmacs:command-reader cosmacs:command-reader/interface)
;;    (edwin:mode          edwin:mode/interface))
;;   (open scheme aliases edwin:command srfi-1 srfi-69 srfi-89 srfi-78 srfi-14
;;         define-record-type* errors keystroke aliases keystroke-discloser
;;         edwin:string-table edwin:doc-string sorting ascii)
;;   (for-syntax (open scheme macro-helpers))
;;   (files (../edwin48/scsh macros)
;;          ../edwin48/modes
;;          ../edwin48/comtab))

(define-interface cosmetic-emacs-interface
  (export start
          wait-for-key
          %read-char
          make-cosmacs-port
          input-terminal-raw-mode
          input-terminal-cooked-mode))

(define-structure cosmacs cosmetic-emacs-interface
  (open scsh scheme
       extended-ports
       handle
       keystroke
       i/o
       edwin:mode
       errors
       (subset edwin:command-table (define-key comtab-entry))
       edwin:command
       edwin:fundamental)
  (files cosmacs))

