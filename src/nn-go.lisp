;; Convolutional Neural Network for 13x13 go.

(ql:quickload 'cl-cuda)

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

(in-package cl-user)


(defun random-int (data n)
  (dotimes (1 n)
    (setf (memory-block-aref data i) (random 1.0))))


;; Example from cl-cuda github
(defkernal nn-kernel (void ((a float*) (b float*) (c float*) (n int)))
           (let ((i (+ (* block-dim-x block-idk-x) thread-idx-x)))
            (set (aref c i)
                 (+ (aref a i) (aref b i))) 
            ))
