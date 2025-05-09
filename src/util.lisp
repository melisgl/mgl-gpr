(in-package :mgl-gpr)

(defun max-position/vector (vector &key (test #'<) key)
  "Return the maximum value (according to TEST) in VECTOR and its index."
  (let ((has-max-p nil)
        max
        pos)
    (dotimes (i (length vector) (values max pos))
      (let ((x (if key
                   (funcall key (aref vector i))
                   (aref vector i))))
        (when (or (not has-max-p)
                  (funcall test max x))
          (setq has-max-p t
                max x
                pos i))))))

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

(defun random-element (seq &key (key #'identity)
                       (start 0) (end (length seq))
                       (sum (sum-seq seq :key key :start start :end end)))
  "Choose an element randomly from the [START,END) subsequence of SEQ
  with given probabilities. KEY returns the unormalized probability of
  an element, SUM is the sum of these unnormalized probalities
  contains unnormalized probabilties. Return the element chosen and
  its index."
  (let ((x (random (float sum 0d0))))
    (do* ((i start (1+ i))
          (e (elt seq i) (elt seq i))
          (s (funcall key e) (+ s (funcall key e))))
         ((or (<= x s) (>= i (1- end))) (values e i)))))

(defun sum-seq (seq &key (key #'identity) (start 0) (end (length seq)))
  "Return the sum of elements in the [START,END) subsequence of SEQ."
  (if (typep seq 'list)
      (loop repeat (- end start)
            for l = (nthcdr start seq) then (cdr l)
            sum (funcall key (car l)))
      (loop for i upfrom start below end
            sum (funcall key (aref seq i)))))

(defun gaussian-random-1 ()
  "Return a double float of zero mean and unit variance."
  (loop
    (let* ((x1 (1- (* 2d0 (random 1d0))))
           (x2 (1- (* 2d0 (random 1d0))))
           (w (+ (* x1 x1) (* x2 x2))))
      (declare (type double-float x1 x2)
               (type double-float w)
               (optimize (speed 3)))
      (when (< w 1d0)
        ;; Now we have two random numbers but return only one. The
        ;; other would be X1 times the same.
        (return
          (* x2
             (locally (declare (optimize (safety 0)))
               (the double-float (sqrt (/ (* -2d0 (log w)) w))))))))))

(defun random-normal (mean stddev)
  (let ((x (gaussian-random-1)))
    (+ mean (* x stddev))))
