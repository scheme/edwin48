(define equivalent-keys-alist
  (list
   (list (kbd a)         (kbd #\a))
   (list (kbd backspace) (kbd #\backspace))
   (list (kbd space)     (kbd #\space))
   (list (kbd return)    (kbd #\return))
   (list (kbd abc)       (kbd "abc"))))

(for-each
   (lambda (key-pair)
     (display (apply key=? key-pair))
     (newline))
   equivalent-keys-alist)



