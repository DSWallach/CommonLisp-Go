;; Convolutional Neural Network for 13x13 go.

(ql:quickload "cl-cuda")

;; Package Definition
(in-package :cl-user)

(defpackage nn-go
  (:use :cl
        :cl-cuda)
  (:export :verify-mat-mul)
  (:export nn-kernel)
  (:export vec-add-kernel)
  (:export :main)
  (:export :tester)
  (:export mat1)
  (:export mat2))

(in-package :nn-go)

;; This will vary for optimal performance depending on the specific GPU
;; Settingss for 1080ti 
(defconstant *num-blocks* 6)
(defconstant *threads-per-block* 28)

(defconstant *input-layer* 169)
(defconstant *second-layer* 81)
(defconstant *third-layer* 25)
(defconstant *fourth-layer* 81)
(defconstant *output-layer* 169)
(defconstant *train-val-one* 1.7159)
(defconstant *train-val-two* 0.66666667)

;; Convolutional Layers
;; Input:       169 (13 * 13)
;; Hidden 1:    81 (9 * 9) ; Hypothetically, using this layer as the input might work for 9x9 go 
;; Hidden 2:    25 ; Kind of arbitrary
;; Hidden 3:    81 ; Stops being convolutional at this point but gotta get back to 13x13
;; Output:      169 ; One output per board position

;; Network definition : Using single precision floats b/c that's what GPUs are best at


(defun random-init (data n)
 (dotimes (j n)
  (setf (memory-block-aref data j) (random 1.0))))

(defun verify-result (as bs cs m n)
  (dotimes (i m)
    (dotimes (j n)
    (let ((a (memory-block-aref as (* i j)))
          (b (memory-block-aref bs (* i j)))
          (c (memory-block-aref cs (* i j))))
      (let ((sum (* a b)))
        (when (> (abs (- c sum)) 1.0)
          (error (format nil "verification fault, i:~A j:~A a:~A b:~A c:~A diff:~A"
                         i j a b c (abs (- c sum)))))))))
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

(defkernel vec-add-kernel (void ((a float*) (b float*) (c float*) (n int)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (if (< i n)
      (set (aref c i)
           (+ (aref a i) (aref b i))))))

;;;; Based on the pseudo code by Jinfeng Liu and Lei Guo

;;  K(KERNEL)-CALC-SECOND-LAYER : Kernel for Calculating the values for layer 2
;; ------------------------------------------------
;;  INPUT: gn1, Vector containing neuron values of the input layer
;;         gw1, Vector containing the connection weights between
;;              layer 1 and layer 2
;;         gn-finish, Memory allocated in MAIN for storing layer 2
;;  OUTPUT: gn-finish, Vector containing the neuron values of layer 2

(defkernel k-calc-layer (void ((gn-start float*) 
				      (gw-start float*) 
				      (gn-finish float*) 
				      (from-layer int) 
				      (to-layer int)))
 (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x))
       (j (+ (* block-dim-x block-idx-y) thread-idx-y))
       (bid block-idx-x)
       (tx thread-idx-x)
       (ty thread-idx-y)
       (wt (+ (* thread-idx-y 2 from-layer)
	    (* thread-idx-x 2)))
      )
  (if (< i from-layer)
   (if (< j to-layer)
    ;; Apply input vector to connection matrix
    ;; Set each value in the vector of the second layer nodes 
    (set (aref gn-finish (+ (* from-layer from-layer bid) (* ty j) tx))
     ;; Not 100% sure what this does
     (* 1.7159 
      ;; Hyperbolic Tangent as the sigmoid function
      (tanh (* 0.6666667 ; Same 
	     (* (aref gn-start wt)
	      (aref gw-start (* bid (+ i to-layer))))))))
   ))
))



;; Run the network using the kernels defined above
;; -----------------------------------------------
(defun main()
 (let* ((dev-id 0)
	(n 4096)
	(threads-per-block 256)
	(blocks-per-grid (/ n threads-per-block)))
  (with-cuda (dev-id)
   (with-memory-blocks ((GN1 'float *input-layer*)
			(GW1 'float (* *input-layer* *second-layer*))
			(GN2 'float *second-layer*)
			(GW2 'float (* *second-layer* *third-layer*))
			(GN3 'float *third-layer*)
			(GW3 'float (* *third-layer* *fourth-layer*))
			(GN4 'float *fourth-layer*)
			(GW4 'float (* *fourth-layer* *output-layer*))
			(GN5 'float *output-layer*)
		       )
    (random-init GN1 *input-layer*)
    (random-init GW1 (* *input-layer* *second-layer*))
    ;; Move node layers and connection weights to the GPU
    (sync-memory-block GN1 :host-to-device)
    (sync-memory-block GW1 :host-to-device)
    (sync-memory-block GN2 :host-to-device)
    (sync-memory-block GW2 :host-to-device)
    (sync-memory-block GN3 :host-to-device)
    (sync-memory-block GW3 :host-to-device)
    (sync-memory-block GN4 :host-to-device)
    (sync-memory-block GW4 :host-to-device)
    (sync-memory-block GN5 :host-to-device)

    ;; Run computation
    (k-calc-layer GN1 GW1 GN2 *input-layer* *second-layer*
     :grid-dim (list blocks-per-grid 1 1)
     :block-dim (list threads-per-block 1 1))
	
    (sync-memory-block GN2 :device-to-host)

;    (verify-result a b c n)
))))
