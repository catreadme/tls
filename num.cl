;; Chapter 4

;; add1
;; ads 1 to a number
(defun add1 (x)
  (+ x 1)
)

;; sub1
;; subtracts 1 from a number
(defun sub1 (x)
  (- x 1)
)

;; zero
;; returns true if number is zero
(defun zero (x)
  (cond
    ((eq 0 x) t)
    (t nil)
  )
)

;; o+
;; adds two numbers
(defun o+ (x y)
  (cond
    ((zero y) x)
    (t
      (add1 (o+ x (sub1 y))))
  )
)

;; o-
;; subtracts two numbers
(defun o- (x y)
  (cond
    ((zero y) x)
    (t
      (sub1 (o- x (sub1 y))))
  )
)

;; addtup
;; adds together a tup
(defun addtup (tup)
  (cond
    ((null tup) 0)
    (t
      (o+ (car tup) (addtup (cdr tup))))
  )
)

;; x
;; multiply two numbers
(defun x (a b)
  (cond
    ((zero b) 0)
    (t
      (o+ a (x a (sub1 b))))
  )
)

;; tup+
;; zip add two tups
(defun tup+ (tup1 tup2)
  (cond
    ((null tup1) tup2)
    ((null tup2) tup1)
    (t
      (cons (o+ (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2))
      )
    )
  )
)

;; gt
;; greater than
(defun gt (x y)
  (cond
    ((zero x) nil)
    ((zero y) t)
    (t
      (gt (sub1 x) (sub1 y)))
  )
)

;; lt
;; less than
(defun lt (x y)
  (cond
    ((zero y) nil)
    ((zero x) t)
    (t
      (lt (sub1 x) (sub1 y)))
  )
)

;; o=
;; equals
(defun o= (x y)
  (cond
    ((gt x y) nil)
    ((lt x y) nil)
    (t t)
  )
)

;; up
;; exponential
(defun up (a b)
  (cond
    ((zero b) 1)
    (t
      (x a (up a (sub1 b))))
  )
)

;; divide
;; divide two numbers
(defun divide (x y)
  (cond
    ((lt x y) 0)
    (t
      (add1 (divide (o- x y) y)))
  )
)

;; _length
;; length of lat
(defun _length (lat)
  (cond
    ((null lat) 0)
    (t
      (add1 (_length (cdr lat))))
  )
)

;; pick
;; pick nth atom from lat
(defun pick (n lat)
  (cond
    ((zero (sub1 n)) (car lat))
    (t
      (pick (sub1 n) (cdr lat)))
  )
)

;; rempick
;; remove nth atom from lat
(defun rempick (n lat)
   (cond
     ((o= n 1) (cdr lat))
     (t
       (cons (car lat)
             (rempick (sub1 n) (cdr lat))
       )
     )
   )
)

;; numberp
;; checks if element is numeric

;; no-nums
;; remove nums from lat
(defun no-nums (lat)
  (cond
    ((null lat) '())
    ((numberp (car lat)) (no-nums (cdr lat)))
    (t
      (cons (car lat) (no-nums (cdr lat))))
  )
)

;; all-nums
;; extract all nums from a lat
(defun all-nums (lat)
  (cond
    ((null lat) '())
    ((numberp (car lat)) (cons (car lat) (all-nums (cdr lat))))
    (t
      (all-nums (cdr lat)))
  )
)

;; equan?
;; check if two atom s are equal
(defun equan? (x y)
  (cond
    ((and (numberp x) (numberp y)) (o= x y))
    ((or (numberp x) (numberp y)) nil)
    (t
      (eq x y))
  )
)

;; occur
;; count occurances of atom in lat
(defun occur (x lat)
  (cond
    ((null lat) 0)
    ((equan? x (car lat)) (add1 (occur x (cdr lat))))
    (t
      (occur x (cdr lat)))
  )
)

;; one?
;; check if atom is one
(defun one? (x)
  (o= x 1)
)
