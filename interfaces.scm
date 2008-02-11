;;; -*- mode: scheme; scheme48-package: (config) -*-

(define-interface edwin-group-definition-interface
  (export make-group
          group-start-mark
          group-end-mark
          group-point
          group-length
          group-start-index
          group-end-index
          group-start-index?
          group-end-index?
          group-display-start-index
          group-display-end-index
          group-display-start-index?
          group-display-end-index?
          set-group-writeable!
          set-group-read-only!
          group-region
          group-position->index
          group-index->position
          set-group-point!
          set-group-point-index!
          group-absolute-start
          group-absolute-end
))

(define-interface edwin-group-operations-interface
  (export group-extract-string
          group-copy-substring!
          group-left-char
          group-right-char
          group-extract-and-delete-string!

          group-insert-char!
          group-insert-chars!
          group-insert-string!
          group-insert-substring!
          prepare-gap-for-insert!
          finish-group-insert!

          group-delete-left-char!
          group-delete-right-char!
          group-delete!

          group-replace-char!
          group-replace-string!
          group-replace-substring!
          prepare-gap-for-replace!
          finish-group-replace!

          grow-group!
          shrink-group!
          memoize-shrink-length!
          compute-shrink-length
          group-reallocation-factor
))

(define-interface edwin-group-interface
  (compound-interface edwin-group-definition-interface
                      edwin-group-operations-interface))


(define-interface edwin-mark-interface
  (export mark-group
          mark-index
          guarantee-mark
          make-temporary-mark
          make-mark
          move-mark-to!
          mark-temporary-copy
          mark-permanent-copy
          mark-right-inserting
          mark-right-inserting-copy
          mark-left-inserting
          mark-left-inserting-copy
          make-permanent-mark
))

(define-interface edwin-region-definition-interface
  (export make-region
          region-start
          region-end
          region-group
          region-start-index
          region-end-index
))

(define-interface edwin-region-operations-interface
  (export region-insert!
          region-insert-string!
          region-insert-substring!
          region-insert-newline!
          region-insert-char!
          region->string
          region-delete!
          mark-left-char
          mark-right-char
          mark-delete-left-char!
          mark-delete-right-char!
          region-transform!
          group-narrow!
          group-widen!
          region-clip!
          with-region-clipped!
          without-group-clipped!
          group-clipped?
          group-unclipped-region
))

(define-interface edwin-region-interface
  (compound-interface edwin-region-definition-interface
                      edwin-region-operations-interface))

(define-interface edwin-motion-interface
  (export limit-mark-motion
          mark1+
          mark-1+
          region-count-chars
          mark+
          mark-
          line-start-index
          line-end-index
          line-start-index?
          line-end-index?
          line-start
          line-end
          ))

(define-interface edwin-utilities-interface
  (export %substring-move!
          split-list
          list-of-type?))

(define-interface edwin-ring-interface
  (export make-ring
          ring-list
          ring-size
          ring-clear!
          ring-empty?
          ring-push!
          ring-pop!
          ring-ref
          ring-set!))

(define-interface edwin-string-table-interface
  (export make-string-table
          alist->string-table
          string-table-get
          string-table-put!
          string-table-remove!
          string-table-complete
          string-table-completions
          string-table-apropos))

(define-interface edwin-doc-string-interface
  (export *external-doc-strings?*
          *external-doc-strings-file*
          ->doc-string
          doc-string->posn
          description?
          description->string
          description-first-line
          description-append))

(define-interface edwin-paths-interface
  (export edwin-binary-directory
          edwin-info-directory
          edwin-etc-directory
          edwin-tutorial-pathname
          default-homedir-pathname))

(define-interface edwin-command-interface
  (export (define-command     :syntax)
          (ref-command-object :syntax)
          (ref-command        :syntax)
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
          copy-command))

(define-interface edwin-variable-interface
  (export (define-variable     :syntax)
          (ref-variable-object :syntax)
          (ref-variable        :syntax)
          (set-variable!       :syntax)
          (local-set-variable! :syntax)
          variable-name
          variable-buffer-local?
          variable-description
          variable-value
          variable-default-value
          variable-name-string
          make-variable
          normalize-variable-value
          add-variable-assignment-daemon!
          invoke-variable-assignment-daemons!
          editor-variables
          name->variable
          ->variable
          variable-permanent-local?
          variable-permanent-local!))

(define-interface edwin-buffer-interface
  (export make-buffer
          buffer-modeline-event!
          without-editor-interrupts
          buffer-reset!
          buffer-name set-buffer-name!
          buffer-default-directory
          set-buffer-default-directory!
          buffer-pathname set-buffer-pathname!
          buffer-truename set-buffer-truename!
          set-buffer-save-length!
          buffer-point
          minibuffer?
          buffer-region
          buffer-string
          buffer-unclipped-region
          buffer-widen!
          buffer-length
          buffer-start buffer-absolute-start
          buffer-end   buffer-absolute-end
          add-buffer-window!
          remove-buffer-window!
          buffer-visible?
          buffer-x-size mark-x-size
          buffer-get
          buffer-put!
          buffer-remove!
          ->buffer
          buffer-modified?
          buffer-modified! buffer-not-modified!
          verify-visited-file-modification-time?
          clear-visited-file-modification-time!
          set-buffer-auto-saved!
          buffer-auto-save-modified?
          buffer-read-only? buffer-writeable?
          set-buffer-read-only! set-buffer-writeable!
          with-read-only-defeated

          define-variable-local-value!
          undefine-variable-local-value!
          variable-local-value
          variable-local-value?
          set-variable-local-value!
          set-variable-default-value!
          set-variable-value!
          with-variable-value!

          ;; modes
          buffer-major-mode
          set-buffer-major-mode!
          buffer-minor-modes
          buffer-minor-mode?
          enable-buffer-minor-mode!
          disable-buffer-minor-mode!))

(define-interface edwin-command-table-interface
  (export comtab-entry local-comtab-entry
          prefix-key-list?
          define-key
          define-prefix-key
          comtab->alist
          comtab-key-bindings))

