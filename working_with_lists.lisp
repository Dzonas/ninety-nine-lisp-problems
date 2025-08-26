(defun my-last (ls)
  (do ((l ls (cdr l)))
      ((not (cdr l)) l)))
