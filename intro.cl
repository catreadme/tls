;; Chapters 1 & 2

;; car
;; returns the first element of a non-empty list

;; cdr
;; returns all but the first element of a non-empty list

;; cons
;; takes an element and prepends it to a list

;; null
;; checks if is null

;; eq
;; checks if two atoms are equal

;; cond
;; chains a series of conditions to select a result

;; or
;; takes two predicates and performs a logical or

;; atom?
;; checks if element is atom
(defun atom? (x)
  (not (listp x))
)

;; lat?
;; checks if a list is a list of atoms
(defun lat? (l)
  (cond
    ((null l) t)
    ((atom? (car l)) (lat? (cdr l)))
    (t nil)
  )
)

;; member?
;; checks if an atom is a member of lat
(defun member? (a lat)
  (cond
    ((null lat) nil)
    (t(or
        (eq (car lat) a)
        (member? a (cdr lat))
      )
    )
  )
)
