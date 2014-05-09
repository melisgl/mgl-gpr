(in-package :mgl-gpr)

(defun max-position/vector (vector &key (test #'<))
  "Return the maximum value (according to TEST) in VECTOR and its index."
  (let ((has-max-p nil)
        max pos)
    (dotimes (i (length vector) (values max pos))
      (when (or (not has-max-p)
                (funcall test max (aref vector i)))
        (setq has-max-p t
              max (aref vector i)
              pos i)))))

(defun insert-into-sorted-vector (item vec &key
                                  (max-length (array-total-size vec))
                                  (test #'<))
  "Insert ITEM into VECTOR while keeping it sorted by TEST. Extend the
  vector if needed. Drop the first element if the size of the vector
  would be more than MAX-LENGTH."
  (let* ((len (length vec))
         (pos (or (position item vec :test test) len)))
    (cond ((< len max-length)
           (vector-push-extend nil vec)
           (replace vec vec :start1 (1+ pos) :start2 pos :end2 len)
           (setf (aref vec pos) item))
          ((<= max-length len)
           (unless (zerop pos)
             (replace vec vec :start1 0 :start2 1 :end2 pos)
             (setf (aref vec (1- pos)) item))))))

(defun flatten (x)
  (if (atom x)
      (list x)
      (mapcan #'flatten x)))

(defun select-randomly (weights)
  "Return the index of the weight in WEIGHTS that ..."
  (let ((x (random 1.0d0)))
    (loop for i below (length weights)
          for w = (aref weights i)
          summing w into sum
          while (<= sum x)
          finally (return i))))

(defun normalize-vector (vector)
  (let ((sum (loop for e across vector do (assert (<= e))
                   summing e)))
    (loop for i below (length vector)
          do (setf (aref vector i) (/ (aref vector i) sum)))))
