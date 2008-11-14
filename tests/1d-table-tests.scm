(define (message caller expect result)
  (newline)
  (display (format #f "~a should return ~a, returned ~a instead.~%"
                   caller expect result)))

(define table (make-1d-table))

(define (run-1d-table-test)
 (let ((expect #t)
       (result (1d-table? table)))
   (if (not result)
       (message '1d-table? expect result)))

 (1d-table/put! table 'foo 'bar)
 (1d-table/put! table 'abc 'def)
 (let* ((value  (1d-table/alist table))
        (expect '((foo . bar) (abc . def)))
        (result (equal? expect value)))
   (if (not result)
       (message '1d-table/alist expect result)))

 (1d-table/remove! table 'foo)
 (let* ((value  (1d-table/alist table))
        (expect '((abc . def)))
        (result (equal? expect value)))
   (if (not result)
       (message '1d-table/remove! expect result)))

 (let* ((value  (1d-table/get table 'abc 'discard))
        (expect 'def)
        (result (equal? expect value)))
   (if (not result)
       (message '1d-table/get expect result)))

 (let* ((value  (1d-table/get table 'foo 'default))
        (expect 'default)
        (result (equal? expect value)))
   (if (not result)
       (message '1d-table/get-with-default expect result)))

 (let* ((if-found     (lambda (x) 'success))
        (if-not-found (lambda (x) 'fail))
        (value        (1d-table/lookup
                       table 'abc if-found if-not-found))
        (expect       'success)
        (result       (equal? expect value)))
   (if (not result)
       (message '1d-table/lookup expect result)))

 (1d-table/remove! table 'abc)
 (let* ((value  (1d-table/alist table))
        (result (null? value)))
   (if (not result)
       (message '1d-table/remove! '() result))))