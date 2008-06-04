;;; -*- mode: scheme; scheme48-package: mit-fileio -*-

(define (file-exists? filename)
  (accessible? filename (access-mode exists)))

(define (file-eq? filename1 filename2)
  (let ((info1 (file-info filename1))
        (info2 (file-info filename2)))
    (= (file-info:inode info1)
       (file-info:inode info2))))
,