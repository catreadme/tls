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

;; atom?
;; checks if element is atom
(defun atom? (x)
  (not (listp x)))
