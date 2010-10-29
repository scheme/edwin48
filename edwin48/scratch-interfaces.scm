;; Scratch Interfaces -- attempt to modularize edwin48

(define-interface edwin:basic-command/interface+scheme
  (export barf-if-read-only      check-first-group-modification
          editor-beep            editor-failure
          execute-extended-keys?
          extension-commands     indent-new-comment-line
          read-quoted-char
          save-buffers-and-exit  save-buffers-kill-edwin
          scheme-can-quit?       self-insert
          set-command-prompt-prefix!))

(define-interface edwin:buffer/interface+edwin
  (edwin:export (variable buffer-creation-hook
                          tab-width)))

(define-interface edwin:buffer/interface+scheme
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
          buffer-group
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
          buffer-modification-time
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

(define-interface edwin:buffer/interface
  (compound-interface edwin:buffer/interface+edwin
                      edwin:buffer/interface+scheme))

(define-interface edwin:bufferset/interface
  (export bufferset-buffer-list
          bufferset-names
          make-bufferset
          set-bufferset-buffer-list!  bufferset-buffer-list
          bufferset-bury-buffer!      bufferset-create-buffer
          bufferset-find-buffer       bufferset-find-or-create-buffer
          bufferset-guarantee-buffer! bufferset-kill-buffer!
          bufferset-rename-buffer     bufferset-select-buffer!))

(define-interface edwin:button/interface
  (export button-bits
          button-down?
          button-event/window
          button-event/x
          button-event/y
          button-event?
          button-name
          button-number
          button-symbol
          button1-down
          button1-up
          button2-down
          button2-up
          button3-down
          button3-up
          button4-down
          button4-up
          button5-down
          button5-up
          button?
          buttons-table
          current-button-event
          down-button?
          make-button-event
          make-down-button
          make-up-button
          up-button?
          with-current-button-event))

(define-interface edwin:command/interface
  (export (define-command     :syntax)
          (ref-command-object :syntax)
          (ref-command        :syntax)
	  command?
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

(define-interface edwin:command-table/interface
  (export comtab-entry local-comtab-entry
	  make-comtab
          comtab-key?
          prefix-key-list?
          define-key
          define-prefix-key
          comtab->alist
          comtab-key-bindings))

(define-interface edwin:display-imaging/interface
  (export ;default-char-image-strings
          group-column->index
          group-columns
          group-line-columns
          string-columns
          group-image!
          partial-image!
          substring-image!))

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

(define-interface edwin:doc-string/interface
  (export *external-doc-strings?*
          *external-doc-strings-file*
          ->doc-string
          doc-string->posn
          description?
          description->string
          description-first-line
          description-append))

(define-interface edwin:editor-definition/interface
  (export make-editor
          editor-screens
          set-editor-screens!
          editor-selected-screen
          set-editor-selected-screen!
          editor-bufferset
          editor-halt-update?
          editor-peek-no-hang
          editor-peek
          editor-read))

(define-interface edwin:editor/interface+scheme
  (export edwin-editor
          current-editor
          within-editor?
          editor-thread editor-thread-root-continuation
          edwin-initialization
          editor-initializations
          enter-recursive-edit exit-recursive-edit
          editor-error
          abort-current-command
          ^G-signal
          quit-editor-and-signal-error quit-editor
          quit-scheme
          ))

(define-interface edwin:editor/interface+edwin
  (edwin:export (variable inhibit-startup-message
                          error-display-mode
                          debug-on-editor-error)))

(define-interface edwin:editor/interface
  (compound-interface edwin:editor/interface+scheme
                      edwin:editor/interface+edwin))

(define-interface edwin:group-definition/interface
  (export add-group-clip-daemon!

          ((group-absolute-end group-absolute-start)
           (proc (:value) :value))

          (group-buffer
           (proc (:value) :value))

          (group-case-fold-search
           (proc (:value) :value))

          (group-display-end-index
           (proc (:value) :exact-integer))

          (group-display-end-index?
           (proc (:value :exact-integer)
                 :boolean))

          (group-display-start-index
           (proc (:value) :exact-integer))

          (group-display-start-index?
           (proc (:value :exact-integer)
                 :boolean))

          (group-end
           (proc (:value) :value))

          (group-end-changes-index
           (proc (:value) :value))

          (group-end-index
           (proc (:value) :exact-integer))

          (group-end-index?
           (proc (:value :exact-integer)
                 :boolean))

          (group-end-mark
           (proc (:value) :value))

          (group-end?
           (proc (:value) :boolean))

          ((group-gap-end    group-gap-start
                             group-gap-length group-length)
           (proc (:value) :exact-integer))

          (group-index->position
           (proc (:value :exact-integer :boolean)
                 :exact-integer))

          (group-modified-tick
           (proc (:value) :exact-integer))

          (group-modified?
           (proc (:value) :boolean))

          (group-point
           (proc (:value) :value))

          (group-position->index
           (proc (:value :exact-integer)
                 :exact-integer))

          (group-region
           (proc (:value) :value))

          (group-start
           (proc (:value) :value))

          (group-start-changes-index
           (proc (:value) :value))

          (group-start-index
           (proc (:value) :exact-integer))

          (group-start-index?
           (proc (:value :exact-integer)
                 :boolean))

          (group-start-mark
           (proc (:value) :value))

          (group-text
           (proc (:value) :string))

          (group-text-properties
           (proc (:value) :value))

          (group-undo-data
           (proc (:value) :value))

          (group-writeable?
           (proc (:value) :boolean))

          (group?
           (proc (:value) :boolean))

          (make-group
           (proc (:value) :value))

          (remove-group-clip-daemon!
           (proc (:value :value)
                 :unspecific))

          (set-group-end-changes-index!
           (proc (:value :value)
                 :unspecific))

          (set-group-modified-tick!
           (proc (:value :value)
                 :unspecific))

          (set-group-modified?!
           (proc (:value :value)
                 :unspecific))

          (set-group-point!
           (proc (:value :value)
                 :unspecific))

          (set-group-point-index!
           (proc (:value :exact-integer)
                 :unspecific))

          (set-group-read-only!
           (proc (:value) :unspecific))

          (set-group-start-changes-index!
           (proc (:value :value)
                 :unspecific))

          (set-group-text-properties!
           (proc (:value :value)
                 :unspecific))

          (set-group-undo-data!
           (proc (:value :value)
                 :unspecific))

          (set-group-writeable!
           (proc (:value) :unspecific))))

(define-interface edwin:group-operations/interface
  (export ((group-extract-string group-extract-and-delete-string!)
           (proc (:value :exact-integer :exact-integer)
                 :unspecific))

          (group-copy-substring!
           (proc (:value :exact-integer :exact-integer
                         :string :exact-integer)
                 :unspecific))

          ((group-left-char  group-right-char)
           (proc (:value :exact-integer)
                 :char))

          (group-insert-char!
           (proc (:value :exact-integer :char)
                 :unspecific))

          (group-insert-chars!
           (proc (:value :exact-integer :char :exact-integer)
                 :unspecific))

          (group-insert-string!
           (proc (:value :exact-integer :string)
                 :unspecific))

          (group-insert-substring!
           (proc (:value :exact-integer :string
                         :exact-integer :exact-integer)
                 :unspecific))

          ((prepare-gap-for-insert! finish-group-insert!
                                    prepare-gap-for-replace! finish-group-replace!)
           (proc (:value :exact-integer :exact-integer)
                 :unspecific))

          ((group-delete-left-char! group-delete-right-char!)
           (proc (:value :exact-integer)
                 :unspecific))

          (group-delete!
           (proc (:value :exact-integer :exact-integer)
                 :unspecific))

          (group-replace-char!
           (proc (:value :exact-integer :char)
                 :unspecific))

          (group-replace-string!
           (proc (:value :exact-integer :string)
                 :unspecific))

          (group-replace-substring!
           (proc (:value :exact-integer :string
                         :exact-integer :exact-integer)
                 :unspecific))

          (grow-group!
           (proc (:value :exact-integer :exact-integer)
                 :unspecific))

          (shrink-group!
           (proc (:value) :unspecific))

          (memoize-shrink-length!
           (proc (:value :exact-integer)
                 :unspecific))

          (compute-shrink-length
           (proc (:exact-integer :exact-integer)
                 :unspecific))

          (group-reallocation-factor
           (proc (:value) :exact-integer))))

(define-interface edwin:group/interface
  (compound-interface edwin:group-definition/interface
                      edwin:group-operations/interface))

(define-interface edwin:mark/interface
  (export (guarantee-mark (proc (:value) :boolean))
          (make-mark (proc (:value :exact-integer) :unspecific))
          (make-permanent-mark (proc (:value) :value))
          (make-temporary-mark
           (proc (:value :exact-integer :boolean) :value))
          (mark-buffer (proc (:value) :value))
          (mark-index (proc (:value) :exact-integer))
          ((mark-left-inserting
            mark-left-inserting-copy
            mark-permanent-copy
            mark-right-inserting
            mark-right-inserting-copy
            mark-temporary-copy)
           (proc (:value) :value))
          ((mark/=
            mark/~
            mark<
            mark<=
            mark=
            mark>
            mark>=
            mark~)
           (proc (:value :value) :boolean))
          (mark? (proc (:value) :boolean))
          (move-mark-to!
           (proc (:value :exact-integer) :unspecific))
          (set-mark-index!
           (proc (:value :exact-integer) :unspecific))
          (mark-group (proc (:value) :value))))

(define-interface edwin:mode/interface
  (export (ref-mode-object   :syntax)
          (define-major-mode :syntax)
          (define-minor-mode :syntax)
          mode?
          make-mode
          mode-name
          mode-major?
          mode-super-mode
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


(define-interface edwin:motion/interface
  (export (line-end
           (proc (:value :exact-integer &opt :value)
                 :value))
          line-end-index
          line-end-index?
          line-start
          line-start-index
          line-start-index?
          mark+
          mark-
          mark-1+
          mark1+
          mark-column
          region-count-chars
          limit-mark-motion
          ))

(define-interface edwin:region-definition/interface
  (export (make-region (proc (:value :value) :value))
          ((region-start region-end region-group)
           (proc (:value) :value))
          ((region-start-index region-end-index)
           (proc (:value) :value))))

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
          (region-transform!
           (proc (:value (proc (:string) :string)) :unspecific))
          group-narrow!
          group-widen!
          region-clip!
          with-region-clipped!
          without-group-clipped!
          group-clipped?
          group-unclipped-region))

(define-interface edwin:region/interface
  (compound-interface edwin:region-definition/interface
                      edwin:region-operations/interface))

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

(define-interface edwin:search/interface
  (export
   (define-next-char-search :syntax)
   (define-prev-char-search :syntax)
   make-find-next
   group-find-next-char
   group-find-next-char-ci
   group-find-next-char-in-set
   make-find-previous
   group-find-previous-char
   group-find-previous-char-ci
   group-find-previous-char-in-set
   group-match-substring-forward
   group-match-substring-backward
   group-match-substring-forward-ci
   group-match-substring-backward-ci
   char-search-forward
   char-search-backward
   char-match-forward
   char-match-backward
   default-start-mark
   default-end-mark
   default-case-fold-search
   skip-chars-forward
   skip-chars-backward
   match-forward
   match-backward))

(define-interface edwin:string-table/interface
  (export make-string-table
          alist->string-table
          string-table-ci?
          string-table-get
          string-table-put!
          string-table-remove!
          string-table-complete
          string-table-completions
          ;; string-table-apropos
          ))

(define-interface edwin:text-property/interface
  (export add-text-property
          remove-text-property
          get-text-properties get-text-property
          next-property-change
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

(define-interface edwin:things/interface
  (export
   make-motion-pair
   move-thing
   move-thing-saving-point
   mark-thing
   kill-thing
   transpose-things
   horizontal-space-region
   horizontal-space-start
   horizontal-space-end
   compute-horizontal-space
   insert-horizontal-space
   delete-horizontal-space
   indent-to
   region-blank?
   line-blank?
   find-previous-blank-line
   find-next-blank-line
   find-previous-non-blank-line
   find-next-non-blank-line
   maybe-change-indentation
   change-indentation
   current-indentation
   mark-indentation
   indentation-end
   within-indentation?
   maybe-change-column
   change-column
   forward-line
   backward-line))

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

(define-interface edwin:utilities/interface
  (export %substring-move!
          split-list
          list-of-type?
          list-of-strings?
          string-or-false?
          string-append-separated
          string-greatest-common-prefix-ci
          string-greatest-common-prefix
          write-strings-densely
          pad-on-left-to pad-on-right-to))

(define-interface edwin:variable/interface
  (export (define-variable            :syntax)
          (define-variable-per-buffer :syntax)
          (ref-variable-object        :syntax)
          (ref-variable               :syntax)
          (set-variable!              :syntax)
          (local-set-variable!        :syntax)
          variable?
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


(define-interface edwin:window-system/interface
  (export vanilla-window
          window-root-window))

(define-interface edwin:command-reader/interface
  (export top-level-command-reader
          command-reader
          return-to-command-loop
          override-next-command!
          current-command-key
          last-command-key
          set-command-argument!
          command-argument
          auto-argument-mode?
          set-command-message!
          command-message-receive
          command-history-list
          execute-key
          execute-command
          execute-button-command
          dispatch-on-key
          dispatch-on-command
          execute-command-history-entry))

(define-interface edwin:basic-command/interface+edwin
  (edwin:export (variable buffer-reallocation-factor)
                (command abort-recursive-edit
                          control-meta-prefix
                          control-prefix
                          define-command
                          execute-extended-command
                          exit-recursive-edit
                          indent-for-comment
                          indent-new-comment-line
                          keyboard-quit
                          kill-comment
                          meta-prefix
                          narrow-to-region
                          open-line
                          prefix-key
                          quoted-insert
                          save-buffers-kill-edwin
                          save-buffers-kill-scheme
                          self-insert-command
                          set-comment-column
                          set-key
                          suspend-edwin
                          suspend-scheme
                          undefined
                          widen)))

(define-interface edwin:basic-command/interface
  (compound-interface edwin:basic-command/interface+edwin
                      edwin:basic-command/interface+scheme))

(define-interface edwin:fundamental/interface+edwin
  (edwin:export (command fundamental-mode)
		(mode fundamental-mode
		      read-only
		      read-only-noarg)
		(variable editor-default-mode)))

(define-interface edwin:fundamental/interface+scheme
  (export initial-buffer-name
	  char-set:self-insert-keys
	  global-modes))

(define-interface edwin:fundamental/inteface
  (compound-interface edwin:fundamental/interface+edwin
		      edwin:fundamental/interface+scheme))
