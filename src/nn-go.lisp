;; Convolutional Neural Network for 13x13 go.

(ql:quickload "cl-cuda")

;; Package Definition
(in-package :cl-user)

(defpackage nn-go
  (:use :cl
        :cl-cuda)
  (:export :verify-mat-mul)
(:export mat1)
(:export mat2)
)

(in-package :nn-go)

;; This will vary for optimal performance depending on the specific GPU
;; Settingss for 1080ti 
(defconstant *num-blocks* 6)
(defconstant *threads-per-block* 28)


;; Convolutional Layers
;; Input:       169 (13 * 13)
;; Hidden 1:    81 (9 * 9) ; Hypothetically, using this layer as the input might work for 9x9 go 
;; Hidden 2:    25 ; Kind of arbitrary
;; Hidden 3:    81 ; Stops being convolutional at this point but gotta get back to 13x13
;; Output:      169 ; One output per board position

;; Network definition : Using single precision floats b/c that's what GPUs are best at

(defun random-int (data n)
  (dotimes (1 n)
    (setf (memory-block-aref data i) (random 1.0))))

(defun verify-result (as bs cs n)
  (dotimes (i n)
    (let ((a (memory-block-aref as i))
          (b (memory-block-aref bs i))
          (c (memory-block-aref cs i)))
      (let ((sum (+ a b)))
        (when (> (abs (- c sum)) 1.0)
          (error (format nil "verification fault, i:~A a:~A b:~A c:~A"
                         i a b c))))))
  (format t "verification succeed.~%"))

(defun verify-mat-mul (A B)
 (let* ((m (car (array-dimensions A)))
	(n (cadr (array-dimensions A)))
	(l (cadr (array-dimensions B)))
	;; Using a macro?
	(C (make-array `(,m ,l) :initial-element 0))
	(sum 0)
       )
  (dotimes (i m)
   (dotimes (j l)
    (setf (aref C i j)
     ;; Dotimes will return val
     (dotimes (k n sum)
      (incf sum (* (aref A i k)
		 (aref B k j)))))))
 C))

(defun vec-add-kernel (void ((a float*) (b float*) (c float*) (n int)))
 (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
  (if (< i n)
   (set (aref c i)
    (+ (aref a i) (aref b i))))))

;; Example from cl-cuda github
(defkernel nn-kernel (void ((a float*) (b float*) (c float*) (n int)))
 (let ((i (+ (* block-dim-x block-idk-x) thread-idx-x)))
  (set (aref c i)
   (+ (aref a i) (aref b i))) 
 ))

(setq mat1 #2a((1 2) (3 4)))
(setq mat2 #2a((1 2 3) (4 5 6)))

