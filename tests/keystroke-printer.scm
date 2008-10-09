(define (start-printer)
  (receive (halt-update? peek-no-hang peek read)
      (get-console-input-operations)
    (with-current-input-terminal-mode 'raw
      (let loop ((k (read)))
        (if (key=? k (kbd #\q))
            (begin (display "bye")
                   (newline))
            (begin (display (key->name k))
                   (newline)
                   (loop (read))))))))

(define input-buffer-size 16)

(define (make-key-table)
  (let ((x (setup-terminal)))
    (map (lambda (name+key-seq)
           (cons (cdr name+key-seq)
                 (make-key (cdr name+key-seq)
                           '()
                           (car name+key-seq))))
         `((up    . ,(key-up    x))
           (down  . ,(key-down  x))
           (left  . ,(key-left  x))
           (right . ,(key-right x))
           (f1    . ,(key-f1    x))
           (f2    . ,(key-f2    x))
           (f3    . ,(key-f3    x))
           (f4    . ,(key-f4    x))
           (f5    . ,(key-f5    x))
           (f6    . ,(key-f6    x))
           (f7    . ,(key-f7    x))
           (f8    . ,(key-f8    x))
           (f9    . ,(key-f9    x))
           (f10   . ,(key-f10   x))
           (f11   . ,(key-f11   x))
           (f12   . ,(key-f12   x))))))

(define (get-console-input-operations)
  (let ((port   (current-input-port))
        (output (current-output-port))
        (string (make-string (* 3 input-buffer-size)))
        (start  0)
        (end    0)
        (incomplete-pending #F)
        (timeout-interval 1000)         ; 1s. Should be f(baud rate) etc
        (len     0))                    ; length of event in input characters
    ;; When the input is a prefix of the character sequence sent by some key
    ;; we are prepared to wait a little while to see if the rest of
    ;; the sequence arrives.  INCOMPLETE-PENDING is either #F, the
    ;; real time at which we timeout for waiting for the sequence to
    ;; complete, or #T if a timeout occured.
    (letrec
        ((parse-key                     ; -> #F or a keystroke
          (lambda ()
            (and (fix:< start end)
                 (let ((n-chars  (fix:- end start)))
                   (let find
                       ((key-pairs (make-key-table))
                        (possible-pending? #F))
                     (if (null? key-pairs)
                         (begin
                           (if (number? incomplete-pending)
                               (if (or (not possible-pending?)
                                       (> (real-time-clock)
                                          incomplete-pending))
                                   (set! incomplete-pending #T)))
                           (if (number? incomplete-pending)
                               #F
                               (begin
                                 (set! len 1)
                                 ;; We must explicitly map the 8th bit
                                 ;; of an incoming character to the
                                 ;; meta bit.
                                 (let ((code (vector-8b-ref string start)))
                                   (if (fix:< code #x80)
                                       (make-key code)
                                       (make-key (fix:and code #x7F)
                                                 (key-modifier meta)))))))
                         (let* ((key-seq  (caar key-pairs))
                                (n-seq    (string-length key-seq)))
;;;                            (format output "In find: ~a~%"
;;;                                    (map (lambda (c) (char->integer c)) (string->list key-seq)))
                           (cond ((and (fix:<= n-seq n-chars)
                                       (string= string key-seq
                                                start (fix:+ start n-seq)
                                                0 n-seq))
                                  (format output "Found: ~a~%" (key->name (cdar key-pairs)))
                                  (set! len n-seq)
                                  (cdar key-pairs))
                                 ((and (fix:> n-seq n-chars)
                                       (string= string key-seq
                                                start (fix:+ start n-chars)
                                                0 n-chars))
                                  (if (not incomplete-pending)
                                      (set! incomplete-pending
                                            (+ (real-time-clock)
                                               timeout-interval)))
                                  (find (cdr key-pairs) #t))
                                 (else
                                  (find (cdr key-pairs)
                                        possible-pending?))))))))))
         (read-more?                    ; -> #F or #T is some chars were read
          (lambda ()
            (let ((n (read-string!/partial string port end input-buffer-size)))
              (cond ((not n)  #F)
                    ((fix:> n 0)
                     (set! end (fix:+ end n))
                     #T)
                    ((fix:= n 0)
                     ;;(error "Reached EOF in keyboard input.")
                     #F)
                    (else
                     (error "Illegal return value:" n))))))
         (read-char
          (lambda ()
            (if (read-more?)
                (parse-key)
                #F)))
         (read-event
          (lambda (block?)
            (or (read-char)
                (let loop ()
                  (cond ;; REPL-related
                        ;; (inferior-thread-changes? event:interrupt)
                        ;; TODO, see process.scm
                        ;; ((process-output-available?) event:process-output) 
                        ((input-available-on-port? port block?)
                         (read-event block?))
                        (else #f))))))
         (guarantee-result
          (lambda ()
            (let ((event (read-event #t)))
              (cond ((key? event) event)
;;;                     ((process-change-event event)
;;;                      => (lambda (flag)
;;;                           (make-input-event
;;;                            (if (eq? flag 'FORCE-RETURN) 'RETURN 'UPDATE)
;;;                            update-screens! #f)))
                    (else (guarantee-result))))))
         (consume!
          (lambda (bytes)
            (set! start (fix:+ start bytes))
            (cond ((fix:>= start end)   ; all consumed
                   (set! end 0)
                   (set! start 0))
                  ((fix:>= start input-buffer-size)
                   (string-copy! string 0 string start end)
                   (set! end (fix:- end start))
                   (set! start 0)))
            (set! incomplete-pending #F)
            unspecific)))
      (values
       (lambda ()                       ;halt-update?
         (or (fix:< start end)
             (read-char)))
       (lambda ()                       ;peek-no-hang
         (or (parse-key)
             (let ((event (read-event #f)))
               event
;;;                (if (fix:fixnum? event)
;;;                    (begin
;;;                      (process-change-event event)
;;;                      #f)
;;;                    event)
               )))
       (lambda ()                       ;peek
         (or (parse-key)
             (guarantee-result)))
       (lambda ()                       ;read
         (let ((event (or (parse-key) (guarantee-result))))
           (consume! len)
           event))))))


