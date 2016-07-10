;; All functions from all chapters

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
    (t(or
        (eq (car lat) a)
        (member? a (cdr lat))
      )
    )
  )
)
