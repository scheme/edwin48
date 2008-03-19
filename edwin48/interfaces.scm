;;; -*- mode: scheme; scheme48-package: (config) -*-

(define-interface edwin:group-definition/interface
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

(define-interface edwin:group-operations/interface
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

(define-interface edwin:group/interface
  (compound-interface edwin:group-definition/interface
                      edwin:group-operations/interface))


(define-interface edwin:mark/interface
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

(define-interface edwin:region-definition/interface
  (export make-region
          region-start
          region-end
          region-group
          region-start-index
          region-end-index
))

(define-interface edwin:region-operations/interface
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

(define-interface edwin:region/interface
  (compound-interface edwin:region-definition/interface
                      edwin:region-operations/interface))

(define-interface edwin:motion/interface
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

(define-interface edwin:utilities/interface
  (export %substring-move!
          split-list
          list-of-type?
          string-or-false?))

(define-interface edwin:ring/interface
  (export make-ring
          ring-list
          ring-size
          ring-clear!
          ring-empty?
          ring-push!
          ring-pop!
          ring-ref
          ring-set!))

(define-interface edwin:string-table/interface
  (export make-string-table
          alist->string-table
          string-table-get
          string-table-put!
          string-table-remove!
          string-table-complete
          string-table-completions
          string-table-apropos))

(define-interface edwin:doc-string/interface
  (export *external-doc-strings?*
          *external-doc-strings-file*
          ->doc-string
          doc-string->posn
          description?
          description->string
          description-first-line
          description-append))

(define-interface edwin:paths/interface
  (export edwin-binary-directory
          edwin-info-directory
          edwin-etc-directory
          edwin-tutorial-pathname
          default-homedir-pathname))

(define-interface edwin:command/interface
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

(define-interface edwin:variable/interface
  (export (define-variable            :syntax)
          (define-variable-per-buffer :syntax)
          (ref-variable-object        :syntax)
          (ref-variable               :syntax)
          (set-variable!              :syntax)
          (local-set-variable!        :syntax)
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

(define-interface edwin:buffer/interface
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

(define-interface edwin:command-table/interface
  (export comtab-entry local-comtab-entry
          prefix-key-list?
          define-key
          define-prefix-key
          comtab->alist
          comtab-key-bindings))

(define-interface edwin:mode/interface
  (export (ref-mode-object   :syntax)
          (define-major-mode :syntax)
          (define-minor-mode :syntax)
          make-mode
          mode-name
          mode-major?
          mode-display-name
          mode-initialization
          mode-comtabs
          editor-modes
          name->mode
          ->mode
          major-mode?
          minor-mode?
          minor-mode-comtab
          mode-description))

(define-interface edwin:modeline/interface
  (export add-minor-mode-line-entry!
          remove-minor-mode-line-entry!))

(define-interface edwin:text-property/interface
  (export add-text-property
          remove-text-property
          get-text-properties get-text-property
          next-proper-change
          previous-property-change
          next-specific-property-change
          previous-specific-property-change
          region-writeable region-read-only
          text-not-insertable?
          text-not-deleteable?
          text-not-replaceable?
          highlight-subgroup
          highlight-region
          highlight-region-excluding-indentation
          local-comtabs
          set-subgroup-local-comtabs!
          set-region-local-comtabs!
          update-intervals-for-insertion!
          update-intervals-for-deletion!
          update-intervals-for-replacement!
          group-extract-properties
          group-reinsert-properties!
          reinsert-properties-size))

(define-interface edwin:simple-editing/interface
  (export delete-left-char  delete-right-char
          delete-string     extract-and-delete-string
          extract-left-char extract-right-char
          extract-string
          find-next-property-change
          find-next-specific-property-change
          find-previous-property-change
          find-previous-specific-property-change
          guarantee-newline         guarantee-newlines
          insert                    insert-char
          insert-chars              insert-newline
          insert-newlines           insert-region
          insert-string             insert-substring
          insert-string-pad-left    insert-string-pad-right
          insert-substring-pad-left insert-substring-pad-right
          mark-flash                narrow-to-region
          region-get                region-put!
          region-remove!            reposition-window-top
          sit-for                   sleep-for
          specific-property-region  widen))

(define-interface edwin:undo/interface
  (export enable-group-undo!          disable-group-undo!
          group-undo-boundary!
          record-first-change!        record-item!
          record-point!               record-properties!
          truncate-buffer-undo-lists! truncate-undo-data!
          undo-boundary!              undo-done!
          undo-enabled?               undo-item-size
          undo-leave-window!          undo-more
          undo-one-step               undo-record-deletion!
          undo-record-insertion!      undo-record-property-changes!
          undo-record-replacement!    undo-start
          with-group-undo-disabled))

(define-interface edwin:basic-command/interface
  (export barf-if-read-only      check-first-group-modification
          editor-beep            editor-failure
          execute-extended-keys?
          extension-commands     indent-new-comment-line
          read-quoted-char
          save-buffers-and-exit  save-buffers-kill-edwin
          scheme-can-quit?       self-insert
          set-command-prompt-prefix!))

(define-interface edwin:bufferset/interface
  (export bufferset-buffer-list
          bufferset-names
          make-bufferset
          set-bufferset-buffer-list!  bufferset-buffer-list
          bufferset-bury-buffer!      bufferset-create-buffer
          bufferset-find-buffer       bufferset-find-or-create-buffer
          bufferset-guarantee-buffer! bufferset-kill-buffer!
          bufferset-rename-buffer     bufferset-select-buffer!))

(define-interface edwin:current-state/interface
  (export add-kill-buffer-hook
          add-rename-buffer-hook
          add-select-buffer-hook
          buffer-alive?
          buffer-list
          buffer-mark
          buffer-names
          bury-buffer
          clear-current-message!
          create-buffer
          current-buffer
          current-buffer?
          current-column
          current-comtabs
          current-major-mode
          current-mark
          current-message
          current-minor-mode?
          current-point
          current-process
          current-region
          current-window
          current-window?
          delete-screen!
          disable-current-minor-mode!
          enable-current-minor-mode!
          find-buffer
          find-or-create-buffer
          global-window-modeline-event!
          kill-buffer
          make-buffer-invisible
          make-screen
          maybe-deselect-buffer-layout
          multiple-screens?
          next-visible-window
          other-buffer
          other-screen
          other-window
          pop-current-mark!
          previous-buffer
          push-buffer-mark!
          push-current-mark!
          remove-kill-buffer-hook
          remove-rename-buffer-hook
          remove-select-buffer-hook
          rename-buffer
          save-excursion
          screen-list
          select-buffer
          select-buffer-no-record
          select-cursor
          select-screen
          select-window
          selected-buffer
          selected-buffer?
          selected-screen
          selected-screen?
          selected-window
          selected-window?
          set-buffer-mark!
          set-buffer-point!
          set-current-major-mode!
          set-current-mark!
          set-current-message!
          set-current-point!
          set-current-region!
          set-current-region-reversed!
          typein-window
          typein-window?
          update-screens!
          update-selected-screen!
          window-list
          window-live?
          window-visible?
          window0
          with-current-point
          with-messages-suppressed
          with-selected-buffer))

(define-interface edwin:editor-definition/interface
  (export make-editor
          editor-screens         set-editor-screens!
          editor-selected-screen set-editor-selected-screen!
          editor-bufferset
          editor-halt-update?
          editor-peek-no-hang
          editor-peek
          editor-read))

(define-interface edwin:display-type/interface
  (export make-display-type
          display-type/name
          display-type/multiple-screens?
          display-type/available?
          display-type/make-screen
          display-type/get-input-operations
          display-type/with-display-grabbed
          display-type/with-interrupts-enabled
          display-type/with-interrupts-disabled
          editor-display-types
          name->display-type))

(define-interface edwin:screen/interface
  (export make-screen
          screen-state 
          screen-x-size
          screen-y-size
          screen-root-window
          screen-visibility set-screen-visibility!
          screen-needs-update?
          guarantee-screen
          initialize-screen-root-window!

          screen-beep
          screen-enter!
          screen-exit!
          screen-discard!
          screen-modeline-event!
          screen-selected-window
          screen-select-window!
          screen-select-cursor!
          screen-window-list
          screen-window0
          screen-typein-window
          window-screen
          screen-visible?
          screen-deleted?
          update-screen!
          set-screen-size!
          screen-move-cursor
          screen-direct-output-move-cursor
          screen-output-char
          screen-direct-output-char
          screen-get-output-line
          screen-clear-rectangle
          screen-output-substring
          screen-direct-output-substring
          screen-force-update
          screen-scroll-lines-down
          screen-scroll-lines-up
          with-screen-in-update
          screen-line-draw-cost))