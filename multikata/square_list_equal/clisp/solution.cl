(defun comp (a b)
  (defun sqr (x) (* x x))
  (equal (sort (mapcar #'sqr a) #'<) (sort b #'<)))
