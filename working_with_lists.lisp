;;;; Working with lists: P01-P28

;;; P01

(defun my-last (ls)
  (do ((l ls (cdr l)))
      ((not (cdr l)) l)))

;;; P02

(defun my-but-last (ls)
  (do ((l ls (cdr l)))
      ((not (cddr l)) l)))

;;; P03

(defun element-at (ls n)
  (dotimes (i (1- n)) (setf ls (cdr ls)))
  (car ls))

;;; P04

(defun my-length (ls)
  (do ((ls1 ls (cdr ls1))
       (counter 0 (1+ counter)))
      ((not ls1) counter)))

;;; P05

(defun my-reverse (ls)
  (let ((ls1 '()))
    (dolist (l ls) (push l ls1))
    ls1))

;;; P06

(defun palindromep (ls)
  (equal ls (reverse ls)))

;;; P07

(defun my-flatten (ls)
  (cond ((not ls) '())
	((listp (car ls)) (append (flatten (car ls)) (flatten (cdr ls))))
	(t (cons (car ls) (flatten (cdr ls))))))

;;; P08

(defun compress (ls)
  (do ((l ls (cdr l))
       (out '() (cond ((or (null out)
			   (not (equal (car out) (car l))))
		       (cons (car l) out))
		      (t out))))
      ((not l) (reverse out))))

;;; P09

(defun pack (ls)
  (let ((result '()))
    (dolist (elem ls)
      (if (equal elem (caar result))
	  (push elem (car result))
	  (push (list elem) result)))
    (reverse result)))

;;; P10

(defun encode (ls)
  (mapcar (lambda (group) (list (length group) (car group))) (pack ls)))

;;; P11

(defun encode-modified (ls)
  (mapcar (lambda (group)
	    (if (= 1 (car group))
		(second group)
		group))
	  (encode ls)))

;;; P12

(defun decode (ls)
  (let ((out '()))
    (dolist (group ls)
      (let ((count (decode-extract-count group))
	    (elem (decode-extract-elem group)))
	(dotimes (i count) (push elem out))))
    (reverse out)))

(defun decode-extract-count (ls)
  (if (listp ls)
      (first ls)
      1))

(defun decode-extract-elem (ls)
  (if (listp ls)
      (second ls)
      ls))

;;; P13

(defun encode-direct (ls)
  (let ((out '()))
    (dolist (elem ls)
      (if (and out (equal elem (encode-direct-get-elem out)))
	  (encode-direct-inc-count out)
	  (setf out (cons elem out))))
  (reverse out)))

(defun encode-direct-get-elem (ls)
  (let ((fst (first ls)))
    (if (listp fst)
	(second fst)
	fst)))

(defun encode-direct-inc-count (ls)
  (if (and (first ls) (listp (first ls)))
      (incf (first (first ls)))
      (setf (first ls) (list 2 (first ls)))))

;;; P14

(defun dupli (ls)
  (let ((out '()))
    (dolist (elem ls)
      (push elem out)
      (push elem out))
    (nreverse out)))

;;; P15

(defun repli (ls n)
  (let ((out '()))
    (dolist (elem ls)
      (dotimes (i n)
	(push elem out)))
    (nreverse out)))

;;; P16

(defun drop (ls n)
  (let ((out '())
	(k 1))
    (dolist (elem ls)
      (unless (= (mod k n) 0)
	(push elem out))
      (setf k (1+ k)))
    (nreverse out)))

;;; P17

(defun split (ls n)
  (let ((left '())
	(right '())
	(k 0))
    (dolist (elem ls)
      (if (< k n)
	  (progn (push elem left) (incf k))
	  (push elem right)))
    (list (nreverse left) (nreverse right))))

;;; P18

(defun slice (ls left right)
  (let ((out '())
	(k 1))
    (dolist (elem ls)
      (cond ((> k right) (return))
	    ((<= left k right) (push elem out)))
      (incf k))
    (nreverse out)))

;;; P19

(defun rotate (ls n)
  (if (null ls)
      '()
      (let* ((len (length ls))
	     (m (mod n len)))
	(destructuring-bind (left right) (split ls m)
	  (append right left)))))
