#| -*-Scheme-*-

$Id: lisppaste.scm,v 1.1 2006/11/04 20:25:17 riastradh Exp $

This code is written by Taylor R. Campbell and placed in the Public
Domain.

|#

;;;; Lisppaste XML-RPC Interface

;;; For details, see
;;;   <http://paste.lisp.org/>
;;;   <http://common-lisp.net/project/lisppaste/>

(declare (usual-integrations))

(load-option 'XML)

;;;; Paste Creation and Annotation

(define (lisppaste-insert-paste paste-number #!optional annotation-number)
  (let ((entry (lisppaste:paste-details paste-number annotation-number)))
    (if (command-argument)
        (show-lisppaste entry (mark->output-port (current-point)))
        (insert-string (lisppaste-entry/content entry)))))

(define-command lisppaste-insert-paste
  "Insert the numbered paste at the point.
With a prefix argument, also show a header describing the paste."
  "nPaste number"
  lisppaste-insert-paste)

(define-command lisppaste-insert-annotation
  "Insert the annotation of the numbered paste at the point.
With a prefix argument, also show a header describing the annotation."
  (lambda ()
    (let* ((paste-number (maybe-prompt-for-lisppaste-number #f))
           (annotation-number (prompt-for-lisppaste-annotation-number)))
      (list paste-number annotation-number)))
  lisppaste-insert-paste)

(define (lisppaste-create channel nickname title content)
  (message (lisppaste:new-paste channel nickname title content)))

(define-command lisppaste-buffer
  "Create a new paste of the current buffer."
  (lambda ()
    (read-lisppaste-creation-arguments))
  (lambda (channel nickname title)
    (lisppaste-create channel nickname title
		      (buffer-string (current-buffer)))))

(define-command lisppaste-region
  "Create a new paste of the current region."
  (lambda ()
    (cons (current-region) (read-lisppaste-creation-arguments)))
  (lambda (region channel nickname title)
    (lisppaste-create channel nickname title (region->string region))))

(define (read-lisppaste-creation-arguments)
  (let ((argument (command-argument)))
    (let* ((channel (maybe-prompt-for-lisppaste-channel argument))
           (nickname (maybe-prompt-for-lisppaste-nickname argument))
           (title (prompt-for-lisppaste-title)))
      (list channel nickname title))))

(define (lisppaste-annotate number nickname title content)
  (message (lisppaste:new-paste
            (lisppaste-entry/channel (lisppaste:paste-header number))
            nickname
            title
            content
            number)))

(define-command lisppaste-annotate-with-buffer
  "Annotate an existing paste with the current buffer."
  (lambda ()
    (read-lisppaste-annotation-arguments))
  (lambda (number nickname title)
    (lisppaste-annotate number nickname title
                        (buffer-string (current-buffer)))))

(define-command lisppaste-annotate-with-region
  "Annotate an existing paste with the region."
  (lambda ()
    (cons (current-region) (read-lisppaste-annotation-arguments)))
  (lambda (region number nickname title)
    (lisppaste-annotate number nickname title
                        (region->string region))))

(define (read-lisppaste-annotation-arguments)
  (let ((argument (command-argument)))
    (let* ((number (maybe-prompt-for-lisppaste-number argument))
           (nickname (maybe-prompt-for-lisppaste-nickname argument))
           (title (prompt-for-lisppaste-annotation-title)))
      (list number nickname title))))

;;;; Paste Listing

(define-command lisppaste-channels
  "List all the channels supported by lisppaste in a temporary buffer."
  ()
  (lambda ()
    (call-with-output-to-temporary-buffer " *lisppaste channels*"
      '(SHRINK-WINDOW FLUSH-ON-SPACE)
      (lambda (port)
        (write-strings-densely (lisppaste:list-channels) port)))))

(define (lisppaste-list-pastes entries)
  (call-with-output-to-temporary-buffer " *lisppastes*"
    '(SHRINK-WINDOW FLUSH-ON-SPACE)
    (lambda (port)
      (for-each (lambda (entry)
                  (show-lisppaste entry port))
                entries))))

(define-command lisppaste-list-pastes
  "List the headers of the last number of pastes.
With a prefix argument, list pastes starting at a certain number."
  (lambda ()
    (read-lisppaste-listing-arguments))
  (lambda (count #!optional start-number)
    (lisppaste-list-pastes
     (lisppaste:paste-headers count start-number))))

(define-command lisppaste-list-channel-pastes
  "List the headers of the last few pastes in a certain channel.
With a prefix argument, list pastes starting at a certain number."
  (lambda ()
    (let ((channel (maybe-prompt-for-lisppaste-channel #t)))
      (cons channel (read-lisppaste-listing-arguments))))
  (lambda (channel start-number #!optional count)
    (lisppaste-list-pastes
     (lisppaste:paste-headers-by-channel channel start-number count))))

(define (read-lisppaste-listing-arguments)
  (let ((count (prompt-for-number "Number of pastes to list" #f)))
    (cond ((command-argument)
           => (lambda (argument)
                (list count
                      (if (command-argument-multiplier-only? argument)
                          (prompt-for-number "Starting paste" #f)
                          (command-argument-numeric-value argument)))))
          (else (list count)))))

(define (show-lisppaste entry #!optional port)
  (let ((port (if (default-object? port)
                  (current-output-port)
                  (begin
                    (guarantee-output-port port 'SHOW-LISPPASTE)
                    port))))
    (receive (number time author channel title annotations content)
        (lisppaste-entry/components entry)
      (write-string "Paste " port)
      (write number port)
      (write-string " in " port)
      (write-string channel port)
      (write-string " by " port)
      (write-string author port)
      (write-string " at " port)
      (write-string (decoded-time->iso8601-string time) port)
      (newline port)
      (write-string "  " port)
      (write-string title port)
      (if (positive? annotations)
          (begin
            (newline port)
            (write-string "  (" port)
            (write annotations port)
            (write-string " annotations)" port)))
      (newline port)
      (newline port)
      (if content (write-string content port)))))

;;;; Argument Reading

(define lisppaste-last-channel #f)

(define (lisppaste-default-channel)
  (or lisppaste-last-channel
      (ref-variable lisppaste-default-channel)))

(define (maybe-prompt-for-lisppaste-channel argument)
  ((lambda (channel)
     (set! lisppaste-last-channel channel)
     channel)
   (or (and (not argument)
            (lisppaste-default-channel))
       (prompt-for-lazy-string-table-name
        "Channel"
        (lisppaste-default-channel)
        (delay
          (alist->string-table
           (map (lambda (channel-name)
                  (cons channel-name #f))
                (lisppaste:list-channels))))
        'CASE-INSENSITIVE-COMPLETION? #t))))

(define lisppaste-last-nickname #f)

(define (lisppaste-default-nickname)
  (or lisppaste-last-nickname
      (ref-variable lisppaste-default-nickname)))

(define (maybe-prompt-for-lisppaste-nickname argument)
  ((lambda (nickname)
     (set! lisppaste-last-nickname nickname)
     nickname)
   (or (and (not argument)
            (lisppaste-default-nickname))
       (prompt-for-string "Nickname"
                          (lisppaste-default-nickname)))))

(define (prompt-for-lisppaste-title)
  (prompt-for-string "Title"
                     ;; No default string
                     #f))

(define (prompt-for-lisppaste-annotation-title)
  (prompt-for-lisppaste-title))

(define lisppaste-last-number #f)

(define (lisppaste-default-number)
  lisppaste-last-number)

(define (maybe-prompt-for-lisppaste-number argument)
  ((lambda (number)
     (set! lisppaste-last-number number)
     number)
   (or (and argument
            (not (command-argument-multiplier-only? argument))
            (command-argument-numeric-value argument))
       (prompt-for-number "Paste number"
                          (lisppaste-default-number)))))

(define lisppaste-last-annotation-number #f)

(define (lisppaste-default-annotation-number)
  lisppaste-last-annotation-number)

(define (prompt-for-lisppaste-annotation-number)
  ((lambda (number)
     (set! lisppaste-last-annotation-number number)
     number)
   (prompt-for-number "Annotation number"
                      (lisppaste-default-annotation-number))))

;;;; Lisppaste RPC

;;; This could be used outside of Edwin if it made no reference to the
;;; Edwin variable LISPPASTE-RPC-URI.

(define (lisppaste-rpc method-name required-arguments optional-argument)
  (let ((result
         (xml-rpc (ref-variable lisppaste-rpc-uri)
                  (lisppaste-request method-name
                                     required-arguments
                                     optional-argument))))
    (if (and (string? result)
             (string-prefix? "Error" result))
        (error result)
        result)))

(define (lisppaste-request method-name required-arguments optional-argument)
  (make-xml-document (make-xml-declaration "1.0" "UTF-8" #f)
                     '()                ;misc-1
                     #f                 ;DTD
                     '()                ;misc-2
                     (apply xml-rpc:request method-name
                            (if (and (not (default-object? optional-argument))
                                     optional-argument)
                                (append required-arguments
                                        (list optional-argument))
                                required-arguments))
                     '()))              ;misc-3

(define (lisppaste:new-paste channel nickname title content
                             #!optional number-of-paste-to-annotate)
  (lisppaste-rpc "newpaste" (list channel nickname title content)
                 number-of-paste-to-annotate))

(define (lisppaste:paste-header number)
  (car (lisppaste:paste-headers 1 number)))

(define (lisppaste:paste-headers count #!optional start-number)
  (lisppaste-rpc "pasteheaders" (list count)
                 start-number))

(define (lisppaste:paste-headers-by-channel channel count
                                            #!optional start-number)
  (lisppaste-rpc "pasteheadersbychannel" (list channel count)
                 start-number))

(define (lisppaste:paste-annotation-headers paste-number)
  (lisppaste-rpc "pasteannotationheaders" (list paste-number)
                 #f))

(define (lisppaste:paste-details paste-number #!optional annotation-number)
  (lisppaste-rpc "pastedetails" (list paste-number)
                 annotation-number))

(define (lisppaste:list-channels)
  (lisppaste-rpc "listchannels" '() #f))

(define (lisppaste-entry/number entry) (list-ref entry 0))
(define (lisppaste-entry/time entry) (list-ref entry 1))
(define (lisppaste-entry/nickname entry) (list-ref entry 2))
(define (lisppaste-entry/channel entry) (list-ref entry 3))
(define (lisppaste-entry/title entry) (list-ref entry 4))
(define (lisppaste-entry/annotations entry) (list-ref entry 5))
(define (lisppaste-entry/content entry)
  (if (> (length entry) 6)
      (list-ref entry 6)
      #f))

(define (lisppaste-entry/components entry)
  (values (lisppaste-entry/number entry)
          (lisppaste-entry/time entry)
          (lisppaste-entry/nickname entry)
          (lisppaste-entry/channel entry)
          (lisppaste-entry/title entry)
          (lisppaste-entry/annotations entry)
          (lisppaste-entry/content entry)))

;;;; Random Utility

(define (prompt-for-lazy-string-table-name prompt
                                           default-string
                                           string-table-promise
                                           . options)
  (apply prompt-for-completed-string
         prompt
         default-string
         (lambda (string if-unique if-not-unique if-not-found)
           (string-table-complete (force string-table-promise)
                                  string
                                  if-unique
                                  if-not-unique
                                  if-not-found))
         (lambda (string)
           (string-table-completions (force string-table-promise) string))
         (lambda (string)
           (let ((default (list 'DEFAULT)))
             (not (eq? (string-table-get (force string-table-promise)
                                         string
                                         (lambda (index) index default))
                       default))))
         options))
