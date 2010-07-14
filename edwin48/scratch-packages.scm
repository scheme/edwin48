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

(define-structure edwin:buffer edwin:buffer/interface
  (open scheme
        aliases
        define-record-type*
        edwin:group-definition
        edwin:group-operations
        edwin:mark
        edwin:mode
        edwin:region-definition
        edwin:region-operations
        edwin:ring
        edwin:variable
        event-distributor
        srfi-89
        )
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros)
         buffer)
  ;; -> ,open edwin:buffer produces errors
  ;; ---- related to group, marks, and regions
  ;; ---- These might be harder to fix than the others.
  ;; ---- other needed items are ring, screen, and mode
  )

(define-structure edwin:bufferset edwin:bufferset/interface
  (open scheme
        aliases
        define-record-type*
        edwin:string-table
        edwin:variable
        errors
        pathname
        srfi-1)
  (files bufset)
  (begin
    ;; buffer.scm
    (define-record-type* buffer
      (make-buffer (name) default-directory)
      ())
    (define edwin-variable$editor-default-mode "BROKEN")
    (define (current-buffer) "BROKEN")
    (define (within-editor?) "BROKEN")))

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

(define-structure edwin:display-imaging edwin:display-imaging/interface
  (open (modify scheme  (hide integer->char string-fill! vector-fill!))
        (modify sorting (rename (vector-sort sort)))
        (modify ascii   (alias  (ascii->char integer->char)))
        edwin:group-definition
        fixnum
        srfi-43)
  (files image))

(define-structure edwin:display-type edwin:display-type/interface
  (open scheme
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
  ;; TODO : Kill this.
  (begin (define (editor-error e) e)
         (define (procedure-arity-valid? procedure n) #t)
         (define (call-with-binary-output-file f fun) (fun))))

(define-structure edwin:editor-definition edwin:editor-definition/interface
  (open scheme
        aliases
        define-record-type*
        edwin:ring
        edwin:buffer)
  (files edtstr))

(define-structures
  ((edwin:group-definition     edwin:group-definition/interface)
   (edwin:mark                 edwin:mark/interface)
   (edwin:region-definition    edwin:region-definition/interface))
  (open (modify scheme (hide integer->char string-fill! vector-fill!))
        (modify sorting (rename (vector-sort sort)))
        (modify ascii (alias (ascii->char integer-char)))
        aliases
        define-record-type*
        fixnum
        weak-pair
        errors
        srfi-1
        edwin:variable)
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros)
         struct)
  ;; TODO : Kill this.
  (begin (define-record-type* buffer
           (make-buffer group (display-start))
           ())
         (define edwin-variable$tab-width 8)
         (define edwin-variable$char-image-strings 4)
         (define edwin-variable$case-fold-search #f)
         (define edwin-variable$syntax-table #f)
         (define variable-local-value #f)))

(define-structures
  ((edwin:group-operations   edwin:group-operations/interface)
   (edwin:region-operations  edwin:region-operations/interface)
   (edwin:group    edwin:group/interface)
   (edwin:region   edwin:region/interface))
  (open (modify scheme (hide integer->char string-fill! vector-fill!))
        (modify sorting (rename (vector-sort sort)))
        (modify ascii (alias (ascii->char integer-char)))
        aliases
        define-record-type*
        edwin:group-definition
        edwin:mark
        edwin:motion
        edwin:region-definition
        edwin:text-property
        edwin:utilities
        errors
        fixnum
        srfi-1
        srfi-13
        weak-pair)
  (for-syntax (open scheme errors macro-helpers))
  (files (scsh macros)
         struct
         grpops
         regops)
  ;; TODO : Kill this
  (begin
    (define-record-type* buffer
      (make-buffer group windows (display-start))
      ())
    (define-record-type* window
      (make-window (point))
      ())
    (define edwin-variable$tab-width 8)
    (define edwin-variable$char-image-strings 4)
    (define edwin-variable$case-fold-search "BROKEN")
    (define edwin-variable$syntax-table "BROKEN")
    (define edwin-variable$buffer-reallocation-factor "BROKEN")
    (define variable-local-value #f)
    (define (barf-if-read-only) "BROKEN")
    (define (undo-record-insertion! g i n) "BROKEN")
    (define (check-first-group-modification g) "BROKEN")
    (define (undo-record-deletion! g s e) "BROKEN")
    (define (undo-record-replacement! g s e) "BROKEN")))

(define-structure edwin:mode edwin:mode/interface
  (open scheme
        aliases
        define-record-type*
        edwin:doc-string
        edwin:string-table
        errors
        srfi-89
        )
  (for-syntax (open scheme macro-helpers))
  (files (scsh macros)
         modes)
  (begin
    (define (make-comtab) "BROKEN")))

(define-structure edwin:motion edwin:motion/interface
  (open scheme
        edwin:group-definition
        edwin:mark
        edwin:region-definition
        errors
        fixnum
        srfi-89)
  (files motion)
  (begin
    ;; grpops.scm -- Circular Dependancy
    (define (group-find-previous-char g) g)
    (define (group-find-next-char g) g)
    (define (group-left-char g) g)
    (define (group-right-char g) g)
    (define (group-tab-width g) g)
    (define (group-char-image-strings g) g)
    (define (group-columns g) g)
    (define (group-column->index g) g)))

(define-structure edwin:ring edwin:ring/interface
  (open scheme aliases errors srfi-1)
  (files ring))

(define-structure edwin:screen edwin:screen/interface
  (open (modify scheme (hide integer->char string-fill! vector-fill!))
        aliases ; vector-8b operations
        define-record-type*
        errors
        fixnum
        sorting ; vector operations
        srfi-13 ; string operations
        srfi-43 ; vector operations
        )
  (files screen))

;; -> ,open edwin:screen produces errors
;; ---- related to editor (most of the them) and window

(define-structure edwin:search edwin:search/interface
  (open scheme
        aliases ; string-index-ci
        edwin:group-definition
        edwin:mark
        edwin:motion
        fixnum
        srfi-13 ; string-index
        srfi-14 ; re-compile-char-set
        srfi-23 ; error
        srfi-89 ; define*
        )
  (files search)
  (begin
    ;; grpop.scm
    (define (group-right-char g) "BROKEN")
    (define (group-left-char g) "BROKEN")
    ;; doesn't exist
    (define (re-compile-char-set b) "BROKEN")
    ;; curren.scm
    (define (current-point) "BROKEN")))

(define-structure edwin:string-table edwin:string-table/interface
  (open scheme aliases define-record-type*
        (modify sorting (rename (vector-sort sort)))
        mit-regexp srfi-13 srfi-43 srfi-89)
  (files strtab))

(define-structure edwin:text-property edwin:text-property/interface
  (open scheme
        srfi-69
        srfi-89
        errors
        fixnum
        define-record-type*
        edwin:group-definition
        edwin:region-definition
        edwin:mark
        edwin:motion
        edwin:variable
        aliases
        rb-tree)
  (for-syntax scheme errors)
  (files txtprp)
  (begin
    ;; Requires things.scm
    (define (horizontal-space-end x) "BROKEN")
    (define (horizontal-space-start x) "BROKEN")
    (define (undo-record-property-changes! x) "BROKEN")
    (define (set-group-text-properties! x) "BROKEN")))

(define-structure edwin:things edwin:things/interface
  (open scheme
        aliases
        errors
        srfi-89
        edwin:group-definition
        edwin:region-definition
        edwin:mark
        edwin:motion)
  (files things)
  ;; requires curren,
  )

(define-structure edwin:utilities edwin:utilities/interface
  (open (modify scheme (hide string-fill!))
        srfi-13 srfi-14 srfi-89
        aliases errors fixnum i/o pathname terminal-support util weak-pair)
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
  ;; TODO : Kill this.
  (begin (define (editor-name/internal->external e) e)
         (define (editor-error e s) s)
         (define (within-editor? e) #t)))

(define-structure edwin:window-system edwin:window-system/interface
  (open scheme
        fixnum
        soosy
        srfi-23)
  (files window))
