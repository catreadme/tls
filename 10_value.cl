;; Chapter 10

;; build
(defun build (s1 s2)
  (cons s1 (cons s2 '()))
)

;; new-entry
(defun new-entry (s1 s2)
  (funcall 'build s1 s2)
)

;; lookup-in-entry-help
(defun lookup-in-entry-help (name names values entry-f)
  (cond
    ((null names) (funcall entry-f name))
    ((eq name (car names)) (car values))
    (t
      (lookup-in-entry-help name
        (cdr names)
        (cdr values)
        entry-f))
  )
)

;; lookup-in-entry
(defun lookup-in-entry (name entry entry-f)
  (funcall 'lookup-in-entry-help name (first entry) (second entry) entry-f)
)

;; extend-table
(defun extend-table (t1 t2)
  (cons t1 t2)
)

;; lookup-in-table
(defun lookup-in-table (name table table-f)
  (cond
    ((null table) (funcall table-f name))
    (t
      (lookup-in-entry name (car table)
          (lambda (name)
            (lookup-in-table
              name
              (cdr table)
              table-f)
          )
      )
    )
  )
)
