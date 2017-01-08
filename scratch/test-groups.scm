g(define-structure test-groups
     (export run-test)
   (open scheme
         formats
         srfi-13
         edwin:group
         edwin:region
         edwin:mark
         edwin:buffer
         edwin:motion)
   (begin

     (define (print . stuff)
       (for-each
        (lambda (s) (display s) (newline))
        stuff))

     (define (print-group group)
       (print
        (group-extract-string group
                              (group-start-index group)
                              (group-end-index group))))

     (define (run-test)
       (newline)
       (display "Should print \"Hello\": ")
       (test-group-insert-char)
       (newline)

       (display "Should print \"Hello World.\": ")
       (test-group-region-transform)
       (newline)

       (display "Should print \"Hello!(x3)\":  ")
       (test-region-insert)
       (newline)

       (display "Testing line-start+end:\n")
       (test-line-start+end)
       (newline))

     (define (test-group-insert-char)
       (let* ((buffer (make-buffer))
              (group  (make-group buffer)))
         (set-buffer-group! buffer group)
         (group-insert-char! (buffer-group buffer) 0 #\H)
         (group-insert-char! (buffer-group buffer) 1 #\e)
         (group-insert-char! (buffer-group buffer) 2 #\l)
         (group-insert-char! (buffer-group buffer) 3 #\l)
         (group-insert-char! (buffer-group buffer) 4 #\o)
         (print-group group)))

     (define (test-group-region-transform)
       (let* ((buffer (make-buffer))
              (group (make-group buffer))
              (_ (set-buffer-group! buffer group))
              (_ (group-insert-string! group 0 "Hello dlroW."))
              (left-mark (make-mark group 6))
              (right-mark (make-mark group 11))
              (region (make-region left-mark right-mark))
              (_ (region-transform! region string-reverse)))
         (print-group group)))

     (define (test-region-insert)
       (let* ((buffer (make-buffer))
              (group (make-group buffer))
              (_ (set-buffer-group! buffer group))
              (_ (group-insert-string! group 0 "Hello!"))
              (left-mark (make-mark group 0))
              (right-mark (make-mark group 6))
              (region (make-region left-mark right-mark)))
         (region-insert! right-mark region)
         (region-insert! right-mark region)
         (region-insert! right-mark region)
         (print-group group)))

     (define (test-line-start+end)
       (let* ((buffer (make-buffer))
              (group (make-group buffer))
              (_ (set-buffer-group! buffer group))
              (_ (group-insert-string! group 0 "aaaaa\nbbbbb\nccccc\nddddd"))
              (left-mark (make-mark group 3))
              (right-mark (make-mark group 14)))
         (cond ((not (equal? (mark-index (line-start right-mark 0)) 12))
                (display "Error, should have been 12\n"))
               ((not (equal? (mark-index (line-end right-mark -1)) 11))
                (display "Error, should have been 11\n"))
               ((not (equal? (mark-index (line-start left-mark 2)) 12))
                (display "Error, should have been 12\n"))
               ((not (equal? (mark-index (line-end left-mark 3)) 23))
                (display "Error, should have been 23\n"))
               (else
                (display "All line-start+end tests passed")))))))