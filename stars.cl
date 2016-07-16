;; Chapter 5

;; atom?
;; checks if element is atom
(defun atom? (x)
  (not (listp x))
)

;; add1
;; ads 1 to a number
(defun add1 (x)
  (+ x 1)
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

;; rember*
;; removes all matching members in a l
(defun rember* (a l)
  (cond
    ((null l) '())
    ((atom? (car l))
      (cond
        ((eq a (car l)) (rember* a (cdr l)))
        (t (cons (car l) (rember* a (cdr l))))
      )
    )
    (t
      (cons (rember* a (car l)) (rember* a (cdr l))))
  )
)

;; insertR*
;; inserts element to the right of another in a l
(defun insertR* (new old l)
  (cond
    ((null l) '())
    ((atom? (car l))
      (cond
        ((eq old (car l))
          (cons (car l) (cons new (insertR* new old (cdr l)))))
        (t (cons (car l) (insertR* new old (cdr l))))
      )
    )
    (t
      (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
  )
)

;; occur*
;; counts occurances of element in list
(defun occur* (a l)
  (cond
    ((null l) 0)
    ((atom? (car l))
      (cond
        ((eq a (car l))
          (add1 (occur* a (cdr l))))
        (t
          (occur* a (cdr l)))
      )
    )
    (t
      (+ (occur* a (car l)) (occur* a (cdr l))))
  )
)

;; subst*
;; replace an element with another one in a list
(defun subst* (new old l)
  (cond
    ((null l) '())
    ((atom? (car l))
      (cond
        ((eq old (car l))
          (cons new (subst* new old (cdr l))))
        (t
          (cons (car l) (subst* new old (cdr l))))
      )
    )
    (t
      (cons (subst* new old (car l)) (subst* new old (cdr l))))
  )
)

;; insertL*
;; insert an element after nother on e in a l
(defun insertL* (new old l)
  (cond
    ((null l) '())
    ((atom? (car l))
      (cond
        ((eq old (car l))
          (cons new (cons old (insertL* new old (cdr l)))))
        (t
          (cons (car l) (insertL* new old (cdr l))))
      )
    )
    (t
      (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
  )
)

;; member*
;; checks if element is in list
(defun member* (a l)
  (cond
    ((null l) nil)
    ((atom? (car l))
      (cond
        ((eq a (car l))
          t)
        (t
          (member* a (cdr l)))
      )
    )
    (t
      (or (member* a (car l)) (member* a (cdr l))))
  )
)


;; leftmost
;; returns the leftmost element of a list
(defun leftmost (l)
  (cond
    ((null l) nil)
    ((atom? (car l))
      (car l))
    (t
      (leftmost (car l)))
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
