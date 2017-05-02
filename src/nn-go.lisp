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
  (:export :main-add)
  (:export :tester)
  (:export mat1)
  (:export mat2))

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

(defun random-init (data m n)
  (dotimes (i m)
    (dotimes (j n)
    (setf (memory-block-aref data (* i j)) (random 1.0)))))

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


(defkernel nn-kernel (void ((a float*) (b float*) (c float*) (m int) (n int)))
           (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x))
                 (j (+ (* block-dim-y block-idx-y) thread-idx-y))
                 )
             (if (< i m) 
               (if (< j n)
               (set (aref c (* i j)) 
                    (* (aref a (* i j))
                       (aref b (* i j))))))))

(defun make-matrix (rows cols)
    (let ((new-mat (make-array `(,rows ,cols) :initial-element (random 1.0)))
          )
      new-mat))

(defun tester () 
  (verify-mat-mul (make-matrix 2048 2048) (make-matrix 2048 2048)))

(defun random-init (data n)
  (dotimes (i n)
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

(defkernel vec-add-kernel (void ((a float*) (b float*) (c float*) (n int)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (if (< i n)
        (set (aref c i)
             (+ (aref a i) (aref b i))))))

(defun main-add ()
  (let* ((dev-id 0)
         (n 4096)
         (threads-per-block 256)
         (blocks-per-grid (/ n threads-per-block)))
    (with-cuda (dev-id)
      (with-memory-blocks ((a 'float n)
                           (b 'float n)
                           (c 'float n))
        (random-init a n)
        (random-init b n)
        (sync-memory-block a :host-to-device)
        (sync-memory-block b :host-to-device)
        (vec-add-kernel a b c n
                        :grid-dim (list blocks-per-grid 1 1)
                        :block-dim (list threads-per-block 1 1))
        (sync-memory-block c :device-to-host)
        (verify-result a b c n)))))

(defun main ()
  (let* ((dev-id 0)
         (m 4096)
         (n 4096)
         (threads-per-block 1024)
         (blocks-per-grid (/ (* m n) threads-per-block)))
    (with-cuda (dev-id)
      (with-memory-blocks ((a 'float m n)
                           (b 'float m n)
                           (c 'float m n))
        (random-init a m n)
        (random-init b m n)
        (sync-memory-block a :host-to-device)
        (sync-memory-block b :host-to-device)
        (nn-kernel a b c m n
                        :grid-dim (list blocks-per-grid 1 1)
                        :block-dim (list threads-per-block 1 1))
        (sync-memory-block c :device-to-host)
        (verify-result a b c m n)))))

