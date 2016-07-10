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
    ((null l) t) ;; empy list it list of atoms
    ((atom? (car l)) (lat? (cdr l))) ;; first elem is atom, check the rest rest
    (t nil) ;; catch-all for everything else and return nil
  )
)

;; member?
;; checks if an atom is a member of lat
(defun member? (a lat)
  (cond
    ((null lat) nil)
    (t (or (eq (car lat) a) (member? a (cdr lat))))
  )
)
