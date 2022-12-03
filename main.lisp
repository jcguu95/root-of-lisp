;;;; Root of Lisp

(defun read! () (read))

(defun read!<-str (str)
  (read-from-string str))

(defun eval! (expr &optional env)
  (if (atom expr)
      (case expr
        ('NIL ())
        ('t   t)
        (t (gethash expr env)))
      (let ((car (car expr))
            (cdr (cdr expr)))
        (if (atom car)
            (case car
              ('quote (nth 0 cdr))
              ('atom  (atom (eval! (nth 0 cdr) env)))
              ('eq    (eq   (eval! (nth 0 cdr) env)
                            (eval! (nth 1 cdr) env)))
              ('car   (car  (eval! (nth 0 cdr) env)))
              ('cdr   (cdr  (eval! (nth 0 cdr) env)))
              ('cons  (cons (eval! (nth 0 cdr) env)
                            (eval! (nth 1 cdr) env)))
              ('cond  (loop for lst in cdr
                            when (eq t (not (not (eval! (nth 0 lst) env))))
                              return (eval! (nth 1 lst) env)))
              ;; Attempt function call
              (t      (eval! (cons (eval! car env) cdr) env)))
            ;; Function call
            (if (eq (nth 0 car) 'lambda)
                (let ((parameters (nth 1 car))
                      (expression (nth 2 car))
                      (arguments  cdr)
                      (env-       (make-hash-table)))
                  (assert (= (length parameters) (length arguments)))
                  (loop for i from 0 to (1- (length parameters))
                        do (let ((prm (nth i parameters))
                                 (arg (nth i arguments)))
                             (assert (atom prm))
                             (setf (gethash prm env-) (eval! arg env))))
                  (eval! expression env-))
                (error "Undefined yet."))))))

(let ((test-cases
        '(;; 1. quote
          (a       . "(quote a)")
          ((a b c) . "(quote (a b c))")
          ;; 2. atom
          (t       . "(atom 'a))")
          (()      . "(atom '(a b c))")
          (t       . "(atom '())")
          (t       . "(atom (atom 'a))")
          (()      . "(atom '(atom 'a))")
          ;; 3. eq
          (t       . "(eq 'a 'a)")
          (()      . "(eq 'a 'b)")
          (t       . "(eq '() '())")
          ;; 4. car
          (a       . "(car '(a b c))")
          ;; 5. cdr
          ((b c)   . "(cdr '(a b c))")
          ;; 6. cons
          ((a b c) . "(cons 'a '(b c))")
          ((a b c) . "(cons 'a (cons 'b (cons 'c '())))")
          (a       . "(car (cons 'a '(b c)))")
          ((b c)   . "(cdr (cons 'a '(b c)))")
          ;; 7. cond
          (second  . "(cond ((eq 'a 'b) 'first) ((atom 'a) 'second))")
          ;; A. lambda
          ((a b)   . "((lambda (x) (cons x '(b))) 'a)")
          ((z b c) . "((lambda (x y) (cons x (cdr y)))
                          'z
                          '(a b c))")
          ((a b c) . "((lambda (f) (f '(b c)))
                          '(lambda (x) (cons 'a x)))"))))
  (loop for case in test-cases
        do (assert (equal (car case) (eval! (read!<-str (cdr case)))))))

(defun print! (lst)
  (format t "~a" lst))
