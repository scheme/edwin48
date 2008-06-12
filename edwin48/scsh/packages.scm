;;; -*- mode: scheme; scheme48-package: (config) -*-
;;;
;;; Scheme48 specific packages
;;;

(define-structure define-record-type*
    (export (define-record-type* :syntax)
            define-record-discloser)
  (open scheme define-record-types)
  (for-syntax (open scheme define-record-type*-expander))
  (begin (define-syntax define-record-type*
           expand-define-record-type*
           (begin define define-record-type))))

(define-structure define-record-type*-expander
    (export expand-define-record-type*)
  (open scheme destructuring fluids signals receiving)
  (files records))

(define-structure weak-pair
    (export weak-pair?
            weak-cons
            weak-pair/car?
            weak-car weak-set-car!
            weak-cdr weak-set-cdr!
            weak-memq
            weak-list->list
            list->weak-list)
  (open scheme define-record-type* errors srfi-1 weak)
  (files weak-pair))

(define-structure fixnum
    (export fix:= fix:fixnum?
            fix:< fix:> fix:<= fix:>=
            fix:zero? fix:positive? fix:negative?
            fix:+ fix:- fix:*
            fix:quotient fix:remainder fix:min
            fix:1+ fix:-1+
            fix:not fix:and fix:andc
            fix:or fix:xor fix:lsh)
  (open scheme bitwise)
  (files fixnum))

(define-structure errors
    (export error syntax-error
            error:bad-range-argument
            error:datum-out-of-range
            error:file-operation
            error:wrong-type-argument
            error:not-list
            error:not-weak-list
            warn)
  (open scheme (subset signals (error syntax-error warn)))
  (files errors))

(define-structure aliases
    (export without-interrupts
            unspecific beep write-to-string write-string
            1+ -1+
            char->digit char-ascii?
            list-deletor! alist?
            vector-grow
            string-head string-tail
            get-environment-variable
            symbol-append symbol-name symbol<?
            call-with-binary-input-file call-with-binary-output-file
            exact-integer? exact-nonnegative-integer?
            vector-8b-ref vector-8b-set! vector-8b-fill! vector-8b-find-next-char
            round
            real-time-clock
            (fluid-let :syntax))
  (open (modify scheme (hide string-fill!))
        ascii errors define-opt fluids interrupts util
        (modify scsh-level-0 (rename (getenv get-environment-variable)))
        srfi-1 srfi-6 srfi-8 srfi-13 srfi-14 srfi-43)
  (files aliases))

(define-structure define-opt
    (export (define* :syntax))
  (open scheme srfi-1 let-opt)
  (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))
  (files define-opt))

(define-structure mit-regexp
    (export re-compile-pattern
            re-string-match
            re-substring-match
            re-string-search-forward   ; re-substring-search-forward
            ;; re-string-search-backward re-substring-search-backward
            re-match-start-index
            re-match-end-index
            re-match-extract
            regexp-group)
  (open scheme srfi-23)
  (files regexp))

(define-structure io-support
    (export file-eq?
            file-exists?
            read-string!/partial)
  (open scheme-with-scsh)
  (files fileio))

(define-structure pathname
    (export ->pathname
            parse-namestring
            ->namestring
            pathname-simplify

            pathname-directory
            pathname-name
            pathname-type
            pathname-version

            pathname-new-directory
            pathname-new-type
            pathname-default-directory
            pathname-default-name

            pathname?
            pathname=?
            pathname-absolute?
            directory-pathname?
            pathname-wild?
            merge-pathnames
            file-namestring
            directory-namestring
            enough-namestring
            file-pathname
            directory-pathname
            enough-pathname
            directory-pathname-as-file
            pathname-as-directory

            ;; init-file-pathname
            user-homedir-pathname
            system-library-directory-pathname
            )
  (open scheme define-record-type* ascii
        signals util methods receiving fluids cells
        io-support
        (modify scsh-level-0
                (rename (getenv         lookup-environment-variable)
                        (home-directory user-info-home-directory)
                        (user-uid       get-user-id)))
        srfi-1)
  (begin
    (define (name->user-info name)  (user-info name))
    (define (user-id->user-info id) (user-info id)))
  (files pathnames pathname-unix pathname-scsh))

(define-structure macro-helpers
    (export command-name->scheme-name
            variable-name->scheme-name
            mode-name->scheme-name
            list-ref/default
            expand-variable-assignment
            expand-variable-definition)
  (open scheme aliases errors)
  (files macros-helpers))

(define-structure fixme
    (export within-editor?
            editor-error
            procedure-arity-valid?)
  (open scheme aliases errors)
  (files fixme))

(define-structure rb-tree
    (export make-rb-tree
            rb-tree?        rb-tree/equal?
            rb-tree/empty?  rb-tree/lookup
            rb-tree/delete! rb-tree/insert!
            rb-tree->alist  alist->rb-tree
            rb-tree/copy)
  (open scheme search-trees)
  (files rbtree))

(define-structure event-distributor
    (export make-event-distributor
            event-distributor/invoke!
            add-event-receiver!
            remove-event-receiver!)
  (open scheme aliases errors define-record-type* queues srfi-1)
  (files event-distributor))

(define-interface scsh-tty/interface
    (export tty?                   tty-info
            set-tty-info/now       set-tty-info/flush
            set-tty-info:min       set-tty-info:time
            tty-info:control-chars set-tty-info:control-chars
            tty-info:control-flags set-tty-info:control-flags
            tty-info:input-flags   set-tty-info:input-flags
            tty-info:output-flags  set-tty-info:output-flags
            tty-info:local-flags   set-tty-info:local-flags

            flush-tty/output

            ttyl/canonical ttyl/echo ttyl/enable-signals ttyl/extended

            ttyin/ignore-break    ttyin/cr->nl
            ttyin/output-flow-ctl ttyin/7bits

            ttyout/enable

            ttyc/char-size8       ttyc/enable-parity

            ttychar/start  ttychar/stop ttychar/interrupt
            disable-tty-char))

(define-structure scsh-subset
    (compound-interface scsh-tty/interface
                        (export interrupt/winch
                                set-interrupt-handler))
  (open scheme scsh-level-0))

(define-structure terminal-support
    (compound-interface
     scsh-tty/interface
     (export terminal-raw-input
             terminal-raw-output
             terminal-get-interrupt-char
             terminal-set-interrupt-char!
             set-terminal-x-size! set-terminal-y-size!
             event:console-resize
             ))
  (open scheme ascii bitwise event-distributor scsh-subset srfi-23)
  (files terminal-support))