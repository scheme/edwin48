;;; -*- mode: scheme; scheme48-package: io-support -*-

(define (input-available-on-port? port block?)
  (receive (rvec wvec evec)
      (select (vector port)
              (vector)
              (vector)
              (if block? #f 0))
    (> 0 (vector-length rvec))))

(define (file-eq? filename1 filename2)
  (let ((info1 (file-info filename1))
        (info2 (file-info filename2)))
    (= (file-info:inode info1)
       (file-info:inode info2))))

(define (file-modification-time filename)
  (file-info:mtime (file-info filename)))

(define (call-with-binary-input-file filename thunk)
  (call-with-input-file filename thunk))

(define (call-with-binary-output-file filename thunk)
  (call-with-output-file filename thunk))
