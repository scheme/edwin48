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

(define-interface edwin-groups-interface
  (compound-interface edwin-group-definition-interface
                      edwin-group-operations-interface))


(define-interface edwin-marks-interface
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

(define-interface edwin-regions-definition-interface
  (export make-region
          region-start
          region-end
          region-group
          region-start-index
          region-end-index
))

(define-interface edwin-regions-operations-interface
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

(define-interface edwin-regions-interface
  (compound-interface edwin-regions-definition-interface
                      edwin-regions-operations-interface))

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
  (export %substring-move!))

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
