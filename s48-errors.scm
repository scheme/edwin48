;;; -*- Mode: Scheme; scheme48-package: s48-errors -*-

(define (error:bad-range-argument datum operator)
  (error "bad range argument"
	 `(,datum)
	 `(is out of range for ,operator)))

(define (error:datum-out-of-range datum)
  (error "out of range" datum))

(define (error:file-operation filename verb noun reason operator operands)
  (error "file operation"
	 `(,filename)
	 `(unable to ,verb ,noun because ,reason)))

(define (error:wrong-type-argument datum type operator)
  (error "wrong type argument"
	 `(,operator)
	 `(expects ,datum)
	 `(to be type ,type)))