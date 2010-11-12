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
        (subset edwin:command-reader (last-command-key
                                      dispatch-on-command
                                      dispatch-on-key
                                      define-key))
        (subset pathname (->namestring))
        (subset terminfo (key-message key-smessage))
        (subset edwin:screen (screen-beep))
        (subset edwin:bufferset (bufferset-buffer-list
                                 set-bufferset-buffer-list!))
        (subset tty-flags (ttyin/xon-any))
        (subset edwin:current-state (set-current-point!
                                     with-current-point
                                     set-buffer-point!
                                     buffer-list
                                     current-column
                                     current-comtabs
                                     current-minor-mode?
                                     with-selected-buffer
                                     selected-screen))
        (subset edwin:motion (line-start
                              mark-column
                              line-end))
        (subset edwin:input-event (input-event?
                                   apply-input-event))
        (subset util (any))
        (subset edwin:prompting (prompt-for-command
                                 prompt-for-confirmation?
                                 prompt-for-yes-or-no?))
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
        define-record-type* srfi-13
        (subset edwin:current-state (set-current-point!
                                     push-current-mark!
                                     set-current-mark!
                                     current-point
                                     current-mark))
        (subset edwin:text-property (next-specific-property-change
                                     text-not-deleteable?
                                     update-intervals-for-deletion!
                                     add-text-property))
        (subset edwin:undo (undo-record-deletion!
                            undo-record-replacement!))
        (subset edwin:buffer (set-buffer-display-start!
                              buffer-display-start))
        (subset edwin:variable (ref-variable-object))
        (subset scaffolding (re-compile-char-set
                             set-window-point!
                             window-point))
        (subset edwin:utilities (%substring-move!))
        (subset srfi-1 (delete!))
        (subset edwin:buffer (buffer-windows
                              buffer?))
        edwin:mark
        rb-tree)
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
        srfi-1
        (subset scaffolding (within-editor?))
        (subset edwin:current-state (current-buffer)))
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
        edwin:string-table edwin:doc-string sorting ascii
        (subset edwin:basic-command (edwin-command$prefix-key))
        (subset edwin:text-property (local-comtabs)))
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
        (subset edwin:basic-command (barf-if-read-only
                                     check-first-group-modification
                                     edwin-variable$buffer-reallocation-factor))
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
        (subset edwin:fundamental (initial-buffer-name))
        (subset edwin:mode (ref-mode-object))
        (subset pathname (working-directory-pathname))
        (subset edwin:bufferset (make-bufferset))
        (subset edwin:display-type (display-type/make-screen
                                    display-type/get-input-operations
                                    display-type/with-interrupts-enabled
                                    display-type/with-interrupts-disabled))
        (subset edwin:screen (initialize-screen-root-window!))
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
        (subset edwin:bufferset (bufferset-find-or-create-buffer))
        (subset ascii (char->ascii))
        (subset edwin:prompting (make-typein-buffer-name))
        (subset edwin:editor-definition (editor-halt-update?))
        (subset locations (contents))
        (subset edwin:button (button?))
        (subset edwin:basic-command (edwin-command$undefined
                                     edwin-command$prefix-key))
        (subset edwin:text-property (local-comtabs)))
  (files screen))

(define-structure edwin:string-table edwin:string-table/interface
  (open scheme aliases define-record-type*
        sort
        (subset sorting (list-sort))
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
        weak-pair
        (subset ascii (char->ascii)))
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
        pantene
        fixnum
        errors
        queues
        (subset scaffolding (current-buffer
                             current-point
                             current-comtabs
                             window-point
                             ))
        (subset keystroke (key-name key?))
        (subset edwin:basic-command (barf-if-read-only
                                     self-insert
                                     edwin-command$self-insert-command))
        (subset edwin:region (mark-right-char
                              mark-left-char))
        (subset edwin:utilities (string-append-separated))
        (subset edwin:variable (variable-name))
        (subset srfi-1 (take-right))
        (subset edwin:current-state (current-region
                                     current-mark
                                     current-window))
        (subset edwin:input-event (apply-input-event
                                   input-event?
                                   input-event/type))
        (subset edwin:command-table (local-comtab-entry
                                     comtab-entry
                                     prefix-key-list?))
        (subset edwin:buffer (buffer-windows))
        (subset edwin:group (group-end?))
        edwin:prompting
        edwin:command
        define-record-type*
        srfi-13 srfi-14 srfi-89)
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros)
         comred))

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
        (subset edwin:current-state (update-screens!
                                     make-input-event
                                     selected-screen))
        (subset scaffolding (within-editor?))
        terminal-support
        terminfo)
  (files terminal))

(define-structure edwin:input-event edwin:input-event/interface
  (open scheme errors srfi-9)
  (files input-event))

(define-structure edwin:current-state edwin:current-state/interface
  (open scheme)
  (files (scsh macros)
         edtstr))

(define-structure edwin:modeline edwin:modeline/interface
  (open scheme
        aliases
        srfi-89
        fixnum
        (subset srfi-13 (string-pad
                         string-index))
        (subset srfi-1 (alist-delete))
        edwin:variable
        edwin:mark
        (subset edwin:region (group-clipped?))
        (subset edwin:group (group-display-start-index
                             group-display-end-index))
        (subset edwin:buffer (variable-local-value
                              buffer-group))
        (subset edwin:variable (define-variable-per-buffer
                                 variable-default-value))
        (subset edwin:mode (mode-name
                            mode-display-name
                            minor-mode?))
        (subset pathname (->namestring
                          pathname?)))
  (files modlin))

(define-structure edwin:kill-command edwin:kill-command/interface
  (open scheme)
  (files kilcom))

(define-structure edwin:prompting edwin:prompting/interface
  (open scheme)
  (files prompt))
