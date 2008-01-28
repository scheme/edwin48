;;; -*- Mode: Scheme; scheme48-package: pathname -*-

(initialize-pathnames! 'unix unix/origin-expansion unix/origin-expander)

(define (->namestring pathname)
  (pathname-namestring pathname))

(define (->pathname object)
  (maybe-object->pathname object))

(define (pathname=? pathname1 pathname2)
  (pathname-eq? pathname1 pathname2))

(define (pathname-name pathname)
  (pathname-filename pathname))

(define (pathname-absolute? pathname)
  (not (null? (pathname-origin pathname))))

(define (pathname-wild? pathname)
  (let ((wild? (lambda (name) (string=? "*" name))))
    (any wild? (pathname-namelist pathname))))

(define (file-pathname pathname)
  (make-pathname #f #f (pathname-filename pathname)))

(define (directory-pathname pathname)
  (make-pathname #f (pathname-directory pathname) #f))

(define (directory-pathname-as-file pathname)
  (make-filename ))

(define (pathname-new-directory pathname directory)
  (pathname-with-directory pathname directory))

(define (pathname-new-type pathname type)
  (let* ((base     (filename-base pathname))
	 (new-name (make-filename base type)))
    (pathname-with-filename pathname new-name)))

(define (pathname-default-name pathname name)
  (if (eq? #f (pathname-filename pathname))
      (pathname-with-filename pathname name)
      pathname))

(define (pathname-default-directory pathname directory)
  (if (eq? #f (pathname-directory pathname))
      (pathname-with-directory pathname directory)
      pathname))

(define (pathname-type pathname)
  (if (filename? pathname)
      (filename-type pathname)
      (error "type is not available" pathname)))

(define (pathname-version pathname)
  (if (filename? pathname)
      (filename-version pathname)
      (error "version is not available" pathname)))

(define (user-homedir-pathname)
  (let ((user (user-id->user-info (get-user-id))))
    (user-info-home-directory-pathname user)))