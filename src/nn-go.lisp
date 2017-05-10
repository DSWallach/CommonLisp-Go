;; Convolutional Neural Network for 13x13 go.
;;(ql:quickload "cl-cuda")

;; Package Definition
(in-package :cl-user)

(defpackage nn-go
  (:use :cl
        :db.allegrocache)
  (:export :verify-mat-mul)
  (:export nn-kernel)
  (:export vec-add-kernel)
  (:export :main)
  (:export :main-add)
  (:export :tester)
  (:export :make)
  (:export :analyze-board)
  (:export mat1)
  (:export mat2))

(in-package :nn-go)

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
(defconstant *board-size* 81)


(require :acache "acache-3.0.9.fasl")
;; Open the file containing the most recent network
(db.allegrocache:open-file-database "../go-nets"
 :if-does-not-exist :create
 :if-exists :supersede
 )

(defun new-nn ()
  (init-nn '(81 49 81)))

(defclass nn-archive ()
  ((id :initarg :id :reader id :index :any-unique)
;   (num-layers :initarg :num-layers :reader num-layers)
;   (layer-sizes :initarg :layer-sizes :reader layer-sizes)
;   (output-vecks :initarg :output-vecks :reader output-vecks)
;   (weight-arrays :initarg :weight-arrays :reader weight-arrays)
;   (delta-vecks :initarg :delta-vecks :reader delta-vecks)
(nn :initarg :nn :reader nn)
   )
  (:metaclass persistent-class))

(defun store-nn (nn id)
  (let ((new-nn nil))
  (setq new-nn (make-instance 'nn-archive :id id
                 :num-layers (nn-num-layers nn)
                 :layer-sizes (nn-layer-sizes nn)
                 :output-vecks (nn-output-vecks nn)
                 :weight-arrays (nn-weight-arrays nn)
                 :delta-vecks (nn-delta-vecks nn)
                 ))
  (commit)))

(defun train-nn (nn training-files &optional (commit? nil))
 
;; Write the nn to disk
(when commit? (store-nn nn)))


;;  ANALYZE-BOARD : NN BOARD LEGAL-MOVES
;; ------------------------------------------
;;  Analyze the scores of the various moves at
;;  the given board position
(defun analyze-board (board legal-moves &optional (pid 0))
  ;; Get the output of the nn for the given input (board state)
  (let ((output (get-output-for (svref *nn-vec* pid)  board))
        (move-scores (make-array (length legal-moves) :initial-element 0))
        )
    ;; For each legal move 
    (dotimes (i (length legal-moves))

      ;; Skip passing
      (unless (= *board-size* (svref legal-moves i))
        ;; Set the score of the move to the probability of 
        ;; the move being selected by the network
        (setf (svref move-scores i)
              ;; i.e. the value of the network's output for 
              ;; that board position
              (svref output (svref legal-moves i))))
      )
    ;; Return the scores
    move-scores))


;; Create a list of pathnames to the files to parse
(defun make-parse-list 
  (num)
  (let ((path-list (list )))
    (dotimes (i num)
      (push (make-pathname :name 
                           (concatenate 'string 
                                              "../../CSV-Data-9x9/data"
                                              (write-to-string i)
                                              ".csv"))
            path-list))
    path-list))

;;  Load the training data into memory
;; -------------------------------------
;;  INPUT: List of files
(defun load-data (lof)
  (let ((data (list))
        (in-arr (make-array *board-size* :initial-element 0))
        (out-arr (make-array *board-size* :initial-element 0))
        )
    ;; For evey files
    (dolist (file lof)
      (with-open-file (line file :direction :input)
        ;; Get the input
        (dotimes (i *board-size*)
          (setf (svref in-arr i) (read line nil)))
        ;; Get the output
        (dotimes (i *board-size*)
          (setf (svref out-arr i) (read line nil)))
        )
      ;; Add the array pair to the list
      (push (list in-arr out-arr) data)
      )
    ;; Return the lists
    data))


(defun load-files (num-files)
  (load-data (make-parse-list num-files)))

;; Class for storing a network in AllegroCache
(defclass neural-net ()
  ((id :initarg :id :reader id :index :any-unique)
   (gw1 :initarg :gw1 :reader gw1) ; Input to Second connections
   (gw2 :initarg :gw2 :reader gw2) ; Second to Third connections
   (gw3 :initarg :gw3 :reader gw3) ; Third to Fourth connections
   (gw4 :initarg :gw4 :reader gw4)  ; Fourth to Output connections
   ; (gn1) ; Input Layer
   ; (gn2) ; Second Layer
   ; (gn3) ; Third Layer
   ; (gn4) ; Fourth Layer
   ; (gn5) ; Output Layer
   )
  (:metaclass persistent-class))

(defun make ()
  (compile-file "nn-go")
  (load "nn-go")
  (compile-file "2014-new-nn")
  (load "2014-new-nn"))



