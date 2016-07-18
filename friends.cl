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

;; set?
;; checks if a lat is a set
(defun set? (lat)
  (cond
    ((null lat) t)
    ((member? (car lat) (cdr lat))
      nil)
    (t
      (set? (cdr lat)))
  )
)

;; makeset
;; removes duplicates from lat
(defun makeset (lat)
  (cond
    ((null lat) '())
    (t
      (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
  )
)

;; subset?
;; cheks if set is subset of other set
(defun subset? (set1 set2)
  (cond
    ((null set1) t)
    ((member? (car set1) set2)
      (subset? (cdr set1) set2))
    (t nil)
  )
)

;; eqset?
;; checks if two sets are equal
(defun eqset? (set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)
  )
)
