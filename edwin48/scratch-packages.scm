;;; Scratch Packages -- attempt to modularize edwin48

;;; Files to be worked on:
;; buffer   - FUBAR - Needs everything but the kitchen sink
;; bufset   - Works with explicit defines
;; comman   - Works with explicit defines
;; comtab   - FUBAR - Deprecated?
;; display  - Works with no modifications
;; docstr   - Works with explicit defines
;; grpops   - Works with explicit defines
;; image    - Works with no modifications
;; modes    - Works with explicit defines
;; motion   - Works with explicit defines
;; regops   - Works with explicit defines
;; screen   - FUBAR?
;; search   - Works with explicit defines
;; struct   - Works with no modifications
;; txtprp   - Works with explicit defines
;; variable - Works with explicit defines

(define-structure edwin:basic-command edwin:basic-command/interface
  (open scheme
        aliases
        edwin:utilities
        edwin:command
        edwin:variable
        edwin:buffer
        edwin:mark
        edwin:group
        edwin:things
        edwin:simple-editing
        edwin:region
        (subset scaffolding (current-point current-buffer))
        (subset fixnum (fix:=))
;        (subset edwin:command-reader (define-key dispatch-on-key))
        (subset pathname (->namestring))
        (subset terminfo (key-message key-smessage))
        (subset edwin:screen (screen-beep))
        (subset edwin:bufferset (bufferset-buffer-list set-bufferset-buffer-list!))
        (subset tty-flags (ttyin/xon-any))

        conditions
        keystroke
        ascii
        srfi-89)
  (for-syntax (open scheme macro-helpers))
  (files (scsh macros)
         basic))

(define-structures
  ((edwin:things edwin:things/interface)
   (edwin:simple-editing edwin:simple-editing/interface))
  (open scheme aliases weak-pair errors fixnum srfi-89
        define-record-type* srfi-13 rb-tree)
  (files things
         regops
         grpops
         search
         simple
         struct))

(define-structure edwin:buffer
  (export make-buffer
          buffer?
          buffer-group
          buffer-display-start
          buffer-windows
          set-buffer-group!
          set-buffer-display-start!
          variable-local-value
          edwin-variable$tab-width)
  (open scheme
        aliases
        define-record-type*
        ;edwin:basic-command
        edwin:group
        edwin:mark
        edwin:mode
        edwin:region
        edwin:ring
        edwin:variable
        event-distributor
        srfi-89)
  (for-syntax (open scheme errors macro-helpers))
  (begin
    (define edwin-variable$tab-width 0)
    (define-record-type* buffer
      (make-buffer)
      (display-start group windows))
    (define (variable-local-value buffer variable) variable)))

(define-structure edwin:bufferset edwin:bufferset/interface
  (open scheme
        aliases
        define-record-type*
        edwin:buffer
        edwin:string-table
        edwin:variable
        errors
        pathname
        srfi-1)
  (files bufset)
  (begin
    ;; buffer.scm
    (define edwin-variable$editor-default-mode "edi-def-BROKEN")
))

(define-structure edwin:button edwin:button/interface
  (open scheme
        aliases
        big-util
        srfi-69
        srfi-89
        srfi-9)
  (for-syntax (open scheme macro-helpers))
  (files (scsh macros))
  (begin
    (define hash-table/intern! hash-table-ref)
    (define symbol concatenate-symbol)
    (define current-editor 'broken-editor-button)
    (define current-window 'broken-window-button)
    (define-record-type editor (make-editor) editor?
      (button-event editor-button-event set-editor-button-event!))
    (define-record-type window (make-window) window?
      (point-coordinates window-point-coordinates))
    (define (bucky-bits->prefix bits) (if (not (zero? bits)) 'error-T
                                          'error-F)))
  (files buttons))

(define-structure edwin:command edwin:command/interface
  (open scheme
        aliases
        define-record-type*
        edwin:doc-string
        edwin:string-table
        errors
        srfi-69
        srfi-89)
  (for-syntax (open scheme macro-helpers))
  (files (scsh macros)
         comman)
  (begin
    (define (editor-error e s) e)))

(define-structures
  ((edwin:command-table edwin:command-table/interface)
   (edwin:mode          edwin:mode/interface))
  (open scheme aliases edwin:command srfi-1 srfi-69 srfi-89 srfi-14
        define-record-type* errors keystroke aliases keystroke-discloser
        edwin:string-table edwin:doc-string sorting ascii)
  (for-syntax (open scheme macro-helpers))
  (files (scsh macros)
         modes
         comtab)
  (begin (define button? (lambda (x) #f))
         (define-command undefined
           "This command is not defined"
           "P"
           (lambda (argument)
             (write "ERROR: Undefined Command")))))

(define-structure edwin:fundamental edwin:fundamental/inteface
  (open scheme
        aliases
        keystroke
        srfi-14
        edwin:button
	edwin:command
	edwin:command-table
	edwin:mode
	edwin:variable)
  (for-syntax (open scheme macro-helpers))
  (begin
    (define (make-special-key key modifiers)
      (make-key key
                (cond ((= modifiers 0)
                       empty-modifiers)
                      (else
                       (make-key-modifier-set '(ctrl))))))
    (define current-major-mode)
    (define (set-current-major-mode! m)
      (set! current-major-mode m)))
  (files (scsh macros)
         modefs))

(define-structures
  (;(edwin:display-imaging edwin:display-imaging/interface)
   (edwin:group           edwin:group/interface)
   (edwin:mark            edwin:mark/interface)
   (edwin:motion          edwin:motion/interface)
   (edwin:region          edwin:region/interface)
   (edwin:search          edwin:search/interface)
   (edwin:text-property   edwin:text-property/interface))
  (open (modify scheme (hide string-fill! vector-fill!))
        ascii
        (modify interrupts (expose call-after-gc!))
        aliases scaffolding
        define-record-type*
;        edwin:basic-command
        edwin:buffer
        edwin:command
        edwin:things
        edwin:undo
        edwin:variable
        edwin:utilities
        errors
        fixnum
        rb-tree
        srfi-1 srfi-13 srfi-14 srfi-43 srfi-89
        weak-pair)
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros)
         struct
         grpops
         regops
         motion
         txtprp
         search
;         image
))

(define-structure edwin:display-type edwin:display-type/interface
  (open (modify scheme (hide string-fill! vector-fill!))
        define-record-type*
        srfi-1)
  (files display))

(define-structure edwin:doc-string edwin:doc-string/interface
  (open scheme
        aliases
        fixnum
        errors
        i/o
        srfi-13
        srfi-89)
  (files docstr)
  (begin (define (editor-error e) e)
         (define (procedure-arity-valid? procedure n) #t)
         (define (call-with-binary-output-file f fun) (fun))))

(define-structure edwin:editor-definition
  edwin:editor-definition/interface
  (open scheme
        aliases
        define-record-type*
        edwin:ring
        edwin:buffer)
  (files edtstr))

(define-structure edwin:ring edwin:ring/interface
  (open scheme aliases errors srfi-1)
  (files ring))

(define-structure scaffolding
  (export current-point
          current-buffer
          within-editor?
          re-compile-char-set
          edwin-variable$case-fold-search
          edwin-variable$char-image-strings
          edwin-variable$syntax-table
          window-point
          set-window-point!)
  (open scheme)
  (begin (define edwin-variable$syntax-table 0)
         (define edwin-variable$char-image-strings 0)
         (define edwin-variable$case-fold-search #f)
         (define (current-point) 0)
         (define (re-compile-char-set a b) "re-comp-BROKEN")
         (define (window-point frame) 0)
         (define (set-window-point! frame mark) 0)
         (define (current-buffer) "curbuf-BROKEN")
         (define (within-editor?) "withined-BROKEN")
         ))

(define-structure edwin:screen edwin:screen/interface
  (open (modify scheme (hide string-fill! vector-fill!))
        aliases ; vector-8b operations
        define-record-type*
        errors
        fixnum
        sorting ; vector operations
        srfi-13 ; string operations
        srfi-43 ; vector operations
        )
  (files screen))

(define-structure edwin:string-table edwin:string-table/interface
  (open scheme aliases define-record-type*
	sort
        mit-regexp srfi-13 srfi-43 srfi-89)
  (files strtab))

(define-structure edwin:things
  (export horizontal-space-start
          horizontal-space-end)
  (open scheme
        aliases
        errors
        srfi-89
        edwin:group
        edwin:region
        edwin:mark
        edwin:motion)
  (begin (define (horizontal-space-end mark) 0)
         (define (horizontal-space-start mark) 0)))

(define-structure edwin:undo
  (export undo-record-insertion!
          undo-record-deletion!
          undo-record-replacement!
          undo-record-property-changes!)
  (open scheme)
  (begin
    (define (undo-record-insertion! group start end)
      "uri-BROKEN")
    (define (undo-record-deletion! group start end)
      "urd-BROKEN")
    (define (undo-record-replacement! group start end)
      "urr-BROKEN")
    (define (undo-record-property-changes! group start end)
      "urpc-BROKEN")))

(define-structure edwin:utilities edwin:utilities/interface
  (open (modify scheme (hide string-fill!))
        srfi-13 srfi-14 srfi-89
        aliases
        errors
        fixnum
        i/o
        pathname
        terminal-support
        util
        weak-pair)
  (files utils strpad))

(define-structure edwin:variable edwin:variable/interface
  (open scheme
        aliases
        define-record-type*
        srfi-8
        srfi-69
        srfi-89
        errors
        edwin:string-table
        edwin:doc-string
        )
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros)
         variable)
  (begin (define (editor-name/internal->external e) e)))

(define-structure edwin:command-reader edwin:command-reader/interface
  (open scheme
        aliases
        ;pantene
        edwin:command
        define-record-type*
        srfi-13 srfi-14 srfi-89
        (subset edwin:command-table (local-comtab-entry comtab-entry)))
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros)
         comred))
