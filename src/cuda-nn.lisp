;; Package Definition
(in-package :cl-user)

(defpackage nn-go
  (:use :cl
        :cl-cuda)
  (:export :main)
  )

(in-package :nn-go)

;(eval-when (compile load eval)
;  (setf (sys:gc-switch :gc-old-before-expand) t) ;; Don't request more memory, use old memory
;  (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0))))

;; This will vary for optimal performance depending on the specific GPU
(defconstant *num-blocks* 6)
(defconstant *threads-per-block* 28)
(defconstant *learning-rate* 0.25)
(defconstant *input-layer* 169)
(defconstant *second-layer* 81) ; 9 * 9
(defconstant *third-layer* 49)  ; 7 * 7
(defconstant *fourth-layer* 81)
(defconstant *output-layer* 169)
(defconstant *1-2-edges* (* *input-layer* *second-layer*))
(defconstant *2-3-edges* (* *second-layer* *third-layer*))
(defconstant *3-4-edges* (* *third-layer* *fourth-layer*))
(defconstant *4-5-edges* (* *fourth-layer* *output-layer*))
(defconstant *train-val-one* 1.7159)
(defconstant *train-val-two* 0.66666667)

;; Open the file containing the most recent network
;(db.allegrocache:open-file-database "../go-nets"
; :if-does-not-exist :create
; :if-exists :supersede
; )

;(defun new-nn ()
;  (init-nn '(81 49 81)))

;(defclass nn-archive ()
;  ((id :initarg :id :reader id :index :any-unique)
;   (num-layers :initarg :num-layers :reader num-layers)
;   (layer-sizes :initarg :layer-sizes :reader layer-sizes)
;   (output-vecks :initarg :output-vecks :reader output-vecks)
;   (weight-arrays :initarg :weight-arrays :reader weight-arrays)
;   (delta-vecks :initarg :delta-vecks :reader delta-vecks)
;(nn :initarg :nn :reader nn)
;   )
;  (:metaclass persistent-class)
;  )

;(defun store-nn (nn id)
;  (make-instance 'nn-archive :id id
;                :num-layers (nn-num-layers nn)
;                :layer-sizes (nn-layer-sizes nn)
;                :output-vecks (nn-output-vecks nn)
;                :weight-arrays (nn-weight-arrays nn)
;                :delta-vecks (nn-delta-vecks nn)
;                )
; (commit)
; )

;; UNFINSIHED
(defun train-nn (nn training-files &optional (commit? nil))
  ;; Write the nn to disk
  (when commit? (store-nn nn 1234)))

(defun random-array (size)
  (let ((new-array (make-array size))
        )
    (dotimes (i size)
      (setf (aref new-array i) (random 1.0)))
    new-array))

;; Returns a randomly generated network
(defun new-random-net (id)
  (make-instance 'neural-net :id id
                 :gw1 (random-array *1-2-edges*)
                 :gw2 (random-array *2-3-edges*)
                 :gw3 (random-array *3-4-edges*)
                 :gw4 (random-array *4-5-edges*)
                 ))


;; Networks are accessed through prolog (it all comes back around!)
;;
;; (commit) writes the local changes to the file stored on disk
;; (rollback) clears all changes since the last commit

;; Convolutional Layers
;; Input:       169 (13 * 13)
;; Hidden 1:    81 (9 * 9) ; Hypothetically, using this layer as the input might work for 9x9 go 
;; Hidden 2:    25 ; Kind of arbitrary
;; Hidden 3:    81 ; Stops being convolutional at this point but gotta get back to 13x13
;; Output:      169 ; One output per board position

;; Network definition : Using single precision floats b/c that's what GPUs are best at

;;
(defun random-init (data n)
  (dotimes (j n)
    (setf (memory-block-aref data j) (random 1.0))))

;; 
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

;; Check the multiplication
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

;;;; Based on the pseudo code by Jinfeng Liu and Lei Guo

;;  K(KERNEL)-CALC-SECOND-LAYER : Kernel for Calculating the values for layer 2
;; ------------------------------------------------
;;  INPUT: gn1, Vector containing neuron values of the input layer
;;         gw1, Vector containing the connection weights between
;;              layer 1 and layer 2
;;         gn-finish, Memory allocated in MAIN for storing layer 2
;;  OUTPUT: gn-finish, Vector containing the neuron values of layer 2

(defkernel k-calc-layer 
           (void ((gn-start float*) 
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



;;  K-CALC-ERROR 
;; ---------------------------
;;  Calculates the error at a particular layer of the NN
(defkernel k-calc-error 
           (void ((gn-output float*)
                  (desired-output float*)
                  (layer-error float*)
                  ))
           (let ((bid block-idx-x)
                 )
             (set (aref layer-error bid)
                  ;; Tanh for the sigmoid function
                  (* (tanh (aref gn-output bid))
                     (- (aref gn-output bid)
                        (aref desired-output bid))))
             ))

;;  K-ADJUST-WEIGHTS
;; ---------------------------
;;  Adjusts the weights of the network in a particular layer
(defkernel k-adjust-weights 
           (void ((output-error float*) ; Of the subsequent layer (e.g. 4-adjust-weights uses 5-calc-error
                 (layer-neurons float*)
                 (layer-weights float*)
                 (layer-num int)
                 (learning-rate float)
                 (temp-weights float*)
                 (new-weights float*)
                 ))
           (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x))
                 (j (+ (* block-dim-x block-idx-y) thread-idx-y))
                 (bid block-idx-x)
                 (tx thread-idx-x)
                 (ty thread-idx-y)
                 (wt (+ (* thread-idx-y 2 from-layer)
                        (* thread-idx-x 2)))
                 )
             ;; Initialize the weights from the errors
             (set (aref temp-weights)
                  (aref output-error bid)
                  )
             ;; Calculate the new values for the weights
             (set (aref temp-weights (+ bid tx 1))
                  (+ (aref new-weights (+ bid tx 1))
                     (aref output-error tx)
                     (aref layer-neurons tx))
                  )
             ;; Set the new weights
             (set (aref new-weights (+ bid tx 1))
                  (- (aref layer-weights (+ bid tx 1))
                     (* learning-rate 
                        (aref temp-weights (+ bid tx 1))))
                  )
             ))

;; Transfer connect information from a lisp array
;; to a CUDA memory block
(defmacro array-to-mem-block (arr mem-block)
  `(dotimes (i (length ,arr))
     (setf (memory-block-aref ,mem-block i)
           (aref ,arr i))))

;; Run the network using the kernels defined above
;; -----------------------------------------------
;; INPUTS : Network, an instance of the neural-net class
;;          Input, the input to the first layer of the neural network 
(defun main (network)
  (let* ((dev-id 0)
         (n 4096)
         (threads-per-block 256)
         (blocks-per-grid (/ n threads-per-block)))
    (with-cuda (dev-id)
               (with-memory-blocks 
                 ((GN1 'float *input-layer*)
                  (GN2 'float *second-layer*)
                  (GN3 'float *third-layer*)
                  (GN4 'float *fourth-layer*)
                  (GN5 'float *output-layer*)
                  (GW1 'float *1-2-edges*)
                  (GW2 'float *2-3-edges*)
                  (GW3 'float *3-4-edges*) 
                  (GW4 'float *4-5-edges*)
                  )

                 ;; Transfer connection weights from network object to memory blocks
                 (array-to-mem-block (slot-value network 'gw1) GW1)
                 (array-to-mem-block (slot-value network 'gw2) GW2)
                 (array-to-mem-block (slot-value network 'gw3) GW3)
                 (array-to-mem-block (slot-value network 'gw4) GW4)

                 ;; For now randomly initialize inputs
                 (random-init GN1 *input-layer*)

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

                 ;; Claculate first layer
                 (k-calc-layer GN1 GW1 GN2 *input-layer* *second-layer*
                               :grid-dim (list blocks-per-grid 1 1)
                               :block-dim (list threads-per-block 1 1))
                 ;; Calculate second layer
                 (k-calc-layer GN2 GW2 GN3 *second-layer* *third-layer*
                               :grid-dim (list blocks-per-grid 1 1)
                               :block-dim (list threads-per-block 1 1))
                 ;; Calculate third layer
                 (k-calc-layer GN3 GW3 GN4 *third-layer* *fourth-layer*
                               :grid-dim (list blocks-per-grid 1 1)
                               :block-dim (list threads-per-block 1 1))
                 ;; Calculate fourth layer
                 (k-calc-layer GN4 GW4 GN5 *fourth-layer* *output-layer*
                               :grid-dim (list blocks-per-grid 1 1)
                               :block-dim (list threads-per-block 1 1))

                 (k-calc-output)


                 ;; THe outputs will be here
                 (sync-memory-block GN5 :device-to-host)))))
