;;;
;;; -*- Mode: Scheme; scheme48-package: 1d-table -*-
;;;
;;; An implementation of MIT Scheme's 1D Tables
;;; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/1D-Tables.html
;;;

(define (make-1d-table) (list "1D table"))

(define (1d-table? obj)
  (and (list? obj)
       (string=? (car obj) "1D table")))

(define (1d-table/put! table key value)
  (if (not (1d-table? table))
      (error "not a 1d-table" table)
      (let* ((alist (cdr table))
             (pair  (weak-assq key alist)))
        (if pair
            (weak-set-cdr! pair value)
            (set-cdr! table
                      (cons (weak-cons key value)
                            alist))))))

(define (1d-table/remove! table key)
  (if (not (1d-table? table))
      (error "not a 1d-table" table)
      (let* ((alist (cdr table))
             (pair  (weak-assq key alist)))
        (if pair
            (set-cdr! table (delete pair alist)))
        unspecific)))

(define (1d-table/get table key default)
  (if (not (1d-table? table))
      (error "not a 1d-table" table)
      (let ((pair (weak-assq key (cdr table))))
        (if (not pair)
            default
            (weak-cdr pair)))))

(define (1d-table/lookup table key if-found if-not-found)
  (if (not (1d-table? table))
      (error "not a 1d-table" table)
      (let ((pair (weak-assq key (cdr table))))
        (if pair
            (if-found (weak-cdr pair))
            (if-not-found)))))

(define (1d-table/alist table)
  (reverse (map (lambda (pair)
                  (cons (weak-car pair)
                        (weak-cdr pair)))
                (cdr table))))
