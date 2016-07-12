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

;; rember
;; removes first occurance of matching atom from a list of atoms
(defun rember (a lat)
  (cond
    ((null lat) '())
    ((eq a (car lat)) (cdr lat))
    (t (cons (car lat) (rember a (cdr lat) )))
  )
)

;; firsts
;; returns a list consisting of each first element of a list of lists
(defun firsts (l)
  (cond
    ((null l) '())
    (t (cons (car (car l)) (firsts (cdr l))))
  )
)

;; insertR
;; inserts an atom after the occurance of another in a lat
(defun insertR (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat))
      (cons old (cons new (cdr lat))))
    (t
      (cons (car lat) (insertR new old (cdr lat))))
  )
)

;; insertL
;; inserts an atom before the occurance of another in a lat
(defun insertL (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat))
      (cons new lat))
    (t
      (cons (car lat) (insertL new old (cdr lat))))
  )
)

;; _subst (subst alredy a sbcl builtin)
;; substitute one atom for another in a lat
(defun _subst (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat))
      (cons new (cdr lat)))
    (t
      (cons (car lat) (_subst new old (cdr lat))))
  )
)

;; subst2
;; replace the first of two occurances in a lat
(defun subst2 (new o1 o2 lat)
  (cond
    ((null lat) '())
    ((or (eq o1 (car lat))
         (eq o2 (car lat)))
            (cons new (cdr lat)))
    (t
      (cons (car lat) (subst2 new o1 o2 (cdr lat))))
  )
)

;; multirember
;; removes all occurances of an atom in a lat
(defun multirember (a lat)
  (cond
    ((null lat) '())
    ((eq a (car lat))
      (multirember a (cdr lat)))
    (t
      (cons (car lat) (multirember a (cdr lat))))
  )
)

;; multiinsertR
;; inserts an atom after the occurances of another in a lat
(defun multiinsertR (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat))
      (cons old (cons new (multiinsertR new old (cdr lat)))))
    (t
      (cons (car lat) (multiinsertR new old (cdr lat))))
  )
)

;; multiinsertL
;; inserts an atom before the occurances of another in a lat
(defun multiinsertL (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat))
      (cons new (cons old (multiinsertL new old (cdr lat)))))
    (t
      (cons (car lat) (multiinsertL new old (cdr lat))))
  )
)

;; multisubst
;; substitute all atoms for another in a lat
(defun multisubst (new old lat)
  (cond
    ((null lat) '())
    ((eq old (car lat))
      (cons new (multisubst new old (cdr lat))))
    (t
      (cons old (multisubst new old (cdr lat))))
  )
)
