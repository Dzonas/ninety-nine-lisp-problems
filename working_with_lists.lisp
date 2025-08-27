(defun my-last (ls)
  (do ((l ls (cdr l)))
      ((not (cdr l)) l)))

(defun my-but-last (ls)
  (do ((l ls (cdr l)))
      ((not (cddr l)) l)))

(defun element-at (ls n)
  (dotimes (i (1- n)) (setf ls (cdr ls)))
  (car ls))

(defun my-length (ls)
  (do ((ls1 ls (cdr ls1))
       (counter 0 (1+ counter)))
      ((not ls1) counter)))

(defun my-reverse (ls)
  (let ((ls1 '()))
    (dolist (l ls) (push l ls1))
    ls1))

(defun palindromep (ls)
  (equal ls (reverse ls)))

(defun my-flatten (ls)
  (cond ((not ls) '())
	((listp (car ls)) (append (flatten (car ls)) (flatten (cdr ls))))
	(t (cons (car ls) (flatten (cdr ls))))))
