;;; Example of a command:
;;; (define-command do-something "Does something" () (lambda () (display "Did something\n")))
;;; (define-key fundamental (kbd #\s) 'do-something)
;;; (dispatch-on-key fundamental (kbd #\s))

(define-record-type* comtab
  (really-make-comtab table)
  ())

(define (make-comtab)
  (really-make-comtab (make-hash-table)))

(define fundamental (make-comtab))

;; (define (start-command-tables)
;;   (receive (halt-update? peek-no-hang peek read)
;;       (get-console-input-operations)
;;     (with-current-input-terminal-mode 'raw
;;       (let ((term   (setup-terminal))
;;             (comtab fundamental))
;;         (tputs (keypad-xmit term))
;;         (let loop ((k (read)))
;;           (dispatch-on-key comtab k))))))

;;; Given a key, evaluate the procedure if
;;; there is a command with that keystroke
(define (dispatch-on-key comtab keystroke)
  (let* ((command   (comtab-entry comtab keystroke))
         (procedure (command-procedure command)))
    (procedure)))


(define (lookup-key comtabs keystroke)
  (let ((comtabs (guarantee-comtabs comtabs 'LOOKUP-KEY)))
    (let ((simple-lookup
	   (lambda (key)
	     (let loop ((comtabs* comtabs))
	       (cond ((comtab-get (car comtabs*) key)
		      => handle-datum)
		     ((not (null? (cdr comtabs*)))
		      (loop (cdr comtabs*)))
		     (else #f))))))
      (if (key? keystroke)
          (simple-lookup keystroke)
          (error:wrong-type-argument keystroke "comtab key" 'LOOKUP-KEY)))))

;;; Checks to make sure that the object passed
;;; in has an equivelent comtab represenation in a list
(define (guarantee-comtabs object procedure)
  (cond ;((mode? object)
        ; (mode-comtabs object))          ; Get the mode's comtabs
        ;((symbol? object)
        ; (mode-comtabs (->mode object))) ; Find mode for the name then comtabs
   ((comtab? object)
    (list object))                  ; If just a comtab, put it in a list
   ((list-of-comtabs? object)
    object)
   (else
    (error:wrong-type-argument object "list of comtabs" procedure))))

;;; Get the Command for the given key in
;;; the given command table
(define (comtab-entry comtab keystroke)
  (hash-table-ref/default (comtab-table comtab)
                          (key-value keystroke)
                          (ref-command-object undefined)))

;;; Get the Command for a keystroke in a specific mark
(define (local-comtab-entry comtabs keystroke mark)
  (or (and mark
	   (let ((local-comtabs (local-comtabs mark)))
	     (and local-comtabs
		  (%comtab-entry local-comtabs keystroke))))
      (comtab-entry comtabs keystroke)))

(define (%comtab-entry comtabs key)
  (let ((object (lookup-key comtabs key)))
    (cond ((not object)
	   #f)
	  ((command? object)
	   object)
	  ((command&comtab? object)
	   (car object))
	  ((comtab? object)
	   (ref-command-object prefix-key))
	  (else
	   (error "Illegal result from lookup-key:" object)))))

;;; Add a command to the given command table
(define (comtab-put! comtab keystroke datum)
  (if (not datum)
      (comtab-remove! comtab keystroke)
      (hash-table-set! (comtab-table comtab) (key-value keystroke) datum)))

;;; Remove a command from the given command table
(define (comtab-remove! comtab keystroke)
  (hash-table-delete! (comtab-table comtab) keystroke))

(define (comtab-get comtab key)
  (hash-table-ref (comtab-table comtab) (key-value key)))

;;; TODO: Can be rebount to 'keystroke?' in intface
(define (comtab-key? object)
  (key? object))

(define (valid-comtabs? object)
  (or (symbol? object)
      (comtab? object)
      ;(mode? object)
      (list-of-comtabs? object)))

(define (list-of-comtabs? object)
  (and (not (null? object))
       (list? object)
       (every comtab? object)))

(define (comtab-alias? object)
  (and (pair? object)
       (valid-comtabs? (car object))
       (comtab-key? (cdr object))))

(define (command&comtab? object)
  (and (pair? object)
       (command? (car object))
       (comtab? (cdr object))))

(define (comtab-alias/dereference datum)
  (lookup-key (car datum) (cdr datum)))

(define (prefixed-key? object)
  (let loop ((object object))
    (and (pair? object)
	 (key? (car object))
	 (or (null? (cdr object))
	     (loop (cdr object))))))

(define (prefix-key-list? comtabs key)
  (let ((object (lookup-key comtabs key)))
    (or (comtab? object)
	(command&comtab? object))))

(define (valid-datum? object)
  (or (not object)
      (command? object)
      (comtab? object)))

;;; Evaluate the datum
(define (handle-datum datum)
  (cond ((or (command? datum)
	     (comtab? datum)
	     (command&comtab? datum))
	 datum)
	((comtab-alias? datum)
	 (comtab-alias/dereference datum))
	(else
	 (error "Illegal comtab datum:" datum))))

;;; Adds a keystroke with a corresponding command in the mode comtab
;;; mode: A mode
;;; keystoke: A kbd i.e. (kbd #\a) or (kbd (ctrl #\x) (ctrl #\f))
;;; datum: A command or another command table
(define (define-key mode keystroke datum)
  (%define-key (guarantee-comtabs mode 'DEFINE-KEY)
	       keystroke
	       (if (valid-datum? datum) datum (->command datum))
	       'DEFINE-KEY))

;;; Really define the key. Uses 'procedure' for error
;;; messages, but we are not supporting it at the moment.
(define (%define-key comtabs keystroke datum procedure)
  (let* ((comtab (car comtabs))
         (put!
          (lambda (keystroke)
            (comtab-put! comtab keystroke datum))))
    (cond ((key? keystroke)
           (put! keystroke))
           ;((char-set? keystroke)
           ; (for-each put! (char-set-members keystroke)))
          ((prefixed-key? keystroke)
           (let ((prefix (except-last-pair keystroke)))
             (comtab-put! (if (null? prefix)
                              comtab
                              (lookup-prefix comtab prefix #t))
                          (car (last-pair keystroke))
                          datum)))
          (else
           (error:wrong-type-argument keystroke "comtab key" procedure)))))

;;; A prefix key is a keystoke with a comtab and not a command
(define* (define-prefix-key mode key (command #f))
  (%define-key (guarantee-comtabs mode 'DEFINE-PREFIX-KEY)
               key
	       (let ((comtab (make-comtab)))
		 (if (not command)
		     comtab
		     (let ((command (->command command)))
		       (if (eq? command (ref-command-object prefix-key))
			   comtab
			   (cons command comtab)))))
	       'DEFINE-PREFIX-KEY))

;; (define (lookup-prefix comtab prefix intern?)
;;   (let loop ((comtab (car comtab) (prefix* prefix))
;;     (if (null? prefix*)
;; 	comtab
;; 	(let ((key (car prefix*))
;; 	      (prefix* (cdr prefix*)))
;; 	  (let datum-loop ((datum (comtab-get comtab key)))
;; 	    (cond ((not datum)
;; 		   (and intern?
;; 			(let ((datum (make-comtab)))
;; 			  ;; Note that this will clobber a comtab-alias
;; 			  ;; that points to an undefined entry.
;; 			  (comtab-put! comtab key datum)
;; 			  (loop datum prefix*))))
;; 		  ((comtab? datum)
;; 		   (loop datum prefix*))
;; 		  ((command&comtab? datum)
;; 		   (loop (cdr datum) prefix*))
;; 		  ((comtab-alias? datum)
;; 		   (datum-loop (comtab-alias/dereference datum)))
;; 		  ((command? datum)
;; 		   (error "Key sequence too long:"
;; 			  prefix
;; 			  (- (length prefix) (length prefix*))))
;; 		  (else
;; 		   (error "Illegal comtab datum:" datum)))))))))

;;; Used in keymap
(define (comtab->alist comtab)
  (let ((table (comtab-table comtab)))
    (hash-table->alist table)))

(define (comtab-key-bindings comtabs command)
  (let ((comtabs (guarantee-comtabs comtabs 'COMTAB-KEY-BINDINGS))
	(command (->command command)))
    ;; In addition to having a binding of COMMAND, every key in the
    ;; result satisfies VALID-KEY?.  This eliminates bindings that are
    ;; shadowed by other bindings.
    (let ((valid-key?
	   (lambda (key)
	     (let ((datum (lookup-key comtabs key)))
	       (cond ((command? datum)
		      (eq? command datum))
		     ((comtab? datum)
		      (eq? command (ref-command-object prefix-key)))
		     ((command&comtab? datum)
		      (eq? command (car datum)))
		     (else
		      #f))))))
      (sort (let ((v (lambda (k)
                       (cond ((char? k) 0)
                                        ;		       ((list-of-type? k char?) 1)
                             ((special-key? k) 2)
                             (else 4)))))
              (lambda (k1 k2)
                (< (v k1) (v k2))))

            (let loop ((comtabs comtabs))
	      (if (null? comtabs)
		  '()
		  (%comtab-bindings (car comtabs)
				    (loop (cdr comtabs))
				    command
				    valid-key?)))))))

(define (%comtab-bindings comtab keys command valid-key?)
  (let comtab-loop ((comtab comtab) (keys keys) (prefix '()))
    (let alist-loop ((entries (hash-table->alist (comtab-table comtab))))
      (if (null? entries)
	  keys
	  (let ((key
		 (if (pair? prefix)
		     (append prefix (list (caar entries)))
		     (caar entries))))
	    (let datum-loop
		((datum (cdar entries))
		 (keys (alist-loop (cdr entries))))
	      (cond ((not datum)
		     keys)
		    ((command? datum)
		     (if (and (eq? datum command)
			      (valid-key? key))
			 (cons key keys)
			 keys))
		    ((comtab? datum)
		     (let ((keys
			    (comtab-loop datum keys
					 (if (pair? prefix)
					     key
					     (list key)))))
		       (if (and (eq? command (ref-command-object prefix-key))
				(valid-key? key))
			   (cons key keys)
			   keys)))
		    ((command&comtab? datum)
		     (datum-loop (car datum)
				 (datum-loop (cdr datum) keys)))
		    ((comtab-alias? datum)
		     (datum-loop (comtab-alias/dereference datum) keys))
		    (else
		     (error "Illegal comtab datum:" datum)))))))))

;;; (except-last-pair list-of-pair) -> list-of-pair
;;; Removes the last pair from the given list
(define (except-last-pair lop)
  (drop-right lop 1))
