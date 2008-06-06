;;; -*- mode: scheme; scheme48-package: rbtree -*-

(define (make-rb-tree key=? key<?)
  (make-search-tree key=? key<?))

(define (rb-tree? obj) (search-tree? obj))

(define (rb-tree/insert! rb-tree key datum)
  (search-tree-set! rb-tree key datum))

(define (rb-tree/lookup rb-tree key default)
  (or (search-tree-ref rb-tree key)
      default))

(define (rb-tree/delete! rb-tree key)
  (search-tree-set! rb-tree key #f))

(define (rb-tree->alist rb-tree)
  (let ((alist '()))
    (walk-search-tree
     (lambda (k v) (cons (list k v) alist))
     rb-tree)
    alist))

(define (alist->rb-tree alist key=? key<?)
  (let ((tree (make-rb-tree key=? key<?)))
    (for-each (lambda (pair)
                (rb-tree/insert! tree
                                 (car pair)
                                 (cdr pair)))
              alist)
    tree))

(define (rb-tree/empty? rb-tree)
  (let ((result #t))
    (call-with-current-continuation
      (lambda (ret)
        (walk-search-tree
         (lambda (k v) (ret #f))
         rb-tree)))
    result))

(define (rb-tree/equal? rb-tree1 rb-tree2 datum=?)
  'FIXME)

(define (rb-tree/copy rb-tree)
  'FIXME)
