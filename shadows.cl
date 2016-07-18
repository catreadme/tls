;; number
;; checks if element is number

;; atom?
;; checks if element is atom
(defun atom? (x)
  (not (listp x))
)

;; numbered?
;; checks if expression is valid
(defun numbered? (aexp)
  (cond
    ((atom? aexp)
      (numberp aexp))
    (t
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp))))
      )
    )
  )
)

;; value
;; evaluates arithmetic expression (natural expression)
(defun value (nexp)
  (cond
    ((atom? nexp)
      nexp)
    ((eq (car (cdr nexp)) '+)
      (+
        (value (car nexp))
        (value (car (cdr (cdr nexp))))
      )
    )
    ((eq (car (cdr nexp)) '*)
      (*
        (value (car nexp))
        (value (car (cdr (cdr nexp))))
      )
    )
  )
)
