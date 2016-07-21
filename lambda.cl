;; atom?
;; checks if element is atom
(defun atom? (x)
  (not (listp x))
)

;; equan?
;; check if two atom s are equal
(defun equan? (x y)
  (cond
    ((and (numberp x) (numberp y)) (= x y))
    ((or (numberp x) (numberp y)) nil)
    (t
      (eq x y))
  )
)

;; eqlist?
;; checks if two lists are equal
(defun eqlist? (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)
    ((and (atom? (car l1)) (null l2))
      nil)
    ((and (atom? (car l1)) (atom? (car l2)))
      (and (equan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))
      )
    )
    ((atom? (car l1)) nil)
    ((null l2) nil)
    ((atom? (car l2)) nil)
    (t
      (and (eqlist? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))
      )
    )
  )
)

;; equal?
;; checks if two S-Expressions are equal
(defun equal? (s1 s2)
  (cond
    ((and (atom? s1) (atom? s2))
      (eq s1 s2))
    ((or (atom? s1) (atom? s2)) nil)
    (t
      (eqlist? s1 s2))
  )
)

;; rember-f
;; removes member by a passed function
(defun rember-f (test?)
  (function
    (lambda (a l)
      (cond
        ((null l) '())
        ((funcall test? a (car l)) (cdr l))
        (t
          (cons (car l) (funcall (rember-f test?) a (cdr l))))
      )
    )
  )
)

;; eq?-c
;; curry eq?
(defun eq?-c (a)
  (function
    (lambda (x)
      (eq a x)
    )
  )
)

;; eq?-salad
(setq eq?-salad (eq?-c 'salad))
