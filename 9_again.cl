;; pick
(defun pick (n lat)
  (cond
    ((eq n 1) (car lat))
    (t
      (pick (- n 1) (cdr lat)))
  )
)

;; keep-looking
(defun keep-looking (a sorn lat)
  (cond
    ((numberp sorn)
      (keep-looking a (pick sorn lat) lat))
    (t
      (eq sorn a))
  )
)

;; looking
(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat)
)

;; eternity
(defun eternity (x)
  (eternity x)
)

;; build
(defun build (s1 s2)
  (cons s1 (cons s2 '()))
)

;; shift
(defun shift (pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair)))
)

;; atom?
(defun atom? (x)
  (not (listp x))
)

;; a-pair?
(defun a-pair? (x)
  (cond
    ((atom? x) nil)
    ((null x) nil)
    ((null (cdr x)) nil)
    ((null (cdr (cdr x))) t)
    (t nil)
  )
)

;; align
(defun align (pora)
  (cond
    ((atom? pora) pora)
    ((a-pair? (first pora))
      (align (shift pora)))
    (t
      (build (first pora)
             (align (second pora)))
    )
  )
)

;; length*
(defun length* (pora)
  (cond
    ((atom? pora) 1)
    (t
      (+ (length* (first pora))
         (length* (second pora))))
  )
)

;; zero
;; returns true if number is zero
(defun zero (x)
  (cond
    ((eq 0 x) t)
    (t nil)
  )
)

;; A
(defun A (n m)
  (cond
    ((zero n) (+ 1 m))
    ((zero m) (A (- 1 n) 1))
    (t
      (A (- 1 n) (A n (- 1 m))))
  )
)
