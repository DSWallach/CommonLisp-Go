

;;  ANNALYZE-BOARD : NN BOARD LEGAL-MOVES
;; ------------------------------------------
;;  Analyze the scores of the various moves at
;;  the given board position
(defun annalyze-board (board legal-moves nn)
  ;; Get the output of the nn for the given input (board state)
  (let ((output (get-output-for nn  board))
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

;;  ANNALYZE-move : NN BOARD LEGAL-MOVES
;; ------------------------------------------
;;  Analyze the scores of the various moves at
;;  the given board position
(defun annalyze-move (nn board legal-moves)
  ;; Get the output of the nn for the given input (board state)
  (let ((output (get-output-for nn  board))
        (move 0)
        (best-move 0)
        (best-score -1000000)
        )
    ;; For each legal move 
    (dotimes (i (length legal-moves))
      (setq move (svref legal-moves i))
      ;; Skip passing
      (unless (= *board-size* move)
        (when (> (svref output move) best-score)
          (setq best-score (svref output move))
          (setq best-move move)
          )))
    ;;(format t "Best Move ~A Score ~A~%" best-move best-score)
    ;; Return the scores
    best-move))



;; NOTE: I cannot believe that it's this easy to read/write from files

;;  WRITE-NETWORK : 
;;  INPUT: FILENAME - A string representing where the file will be written
;;         NN - A neural network struct
;;  OPTIONAL: FORCE? - Overwrite file with same name if one exists
;; ---------------------------------------------
;;  Writes a NN to a space deliminated text file
(defun write-network 
  (nn filename &optional (force? nil))
  ;; Network files are put in the 'networks' sub directory
  (let ((file-path (make-pathname :name (concatenate 'string "../networks/" filename)))
        (file nil))
    ;; Overwrite the existing file on close if the force flag is set
    (if force? 
      (setq file (open file-path :direction :output :if-exists :supersede))
      (setq file (open file-path :direction :output)))
    ;; The number of layers is inferred from the length
    ;; of the layer sizes

    ;; Layer Sizes
    (write-string "layer-sizes " file)
    (write-line (write-to-string (nn-layer-sizes nn)) file)

    ;; No need to store the output vecks

    ;; Write weight arrays
    (write-line "weight-arrays " file)
    ;; For each layer
    (dotimes (i (nn-num-layers nn))
      ;; Write the  connections from that layer
      ;; Write each layer's weights on a new line
      (write-line (write-to-string (nn-weight-arrays nn)) file)
      )
    ;; No need to store the delta-vecks
    ;; All done
    (close file)
    ))

;;  READ-NETWORK
;; --------------------------------
;;  INPUT: FILENAME, the filename of the network to read in 
;;                   (assumed to be in the networks directory)
;;  OUTPUT: NN, A new NN struct with the same NUM-LAYERS, LAYER-SIZES, 
;;              and WEIGHT-ARRAYS as the network written to FILENAME
(defun read-network (filename)
  (let ((file-path (make-pathname :name (concatenate 'string "../networks/" filename)))
        (layer-sizes nil)
        (weight-arrays nil)
        (size-list (list))
        )
    (with-open-file (line file-path)

      (format t "Read layer sizes~%")
      ;; Match layer-sizes
      (when (string-equal "layer-sizes" (read line))
        (setq layer-sizes (read line))
        )

      (format t "Read weight arrays~%")
      ;; Match weight-arrays
      (when (string-equal "weight-arrays" (read line))
        (setq weight-arrays (read line))))

    (format t "Close File~%")
    ;; Convert layer sizes to list from vector
    (dotimes (i (length layer-sizes))
      (push (svref layer-sizes (- (length layer-sizes) i 1))
            size-list))

    (init-nn size-list weight-arrays)))

;; Create a list of pathnames to the files to parse
(defun make-parse-list 
  (num)
  (let ((path-list (list )))
    (dotimes (i num)
      (push (make-pathname :name 
                           (concatenate 'string 
                                        "../../9x9/9x9game"
                                        (write-to-string i)
                                        ".csv"))
            path-list))
    path-list))

;;  Load the training data into memory
;; -------------------------------------
;;  INPUT: List of files
(defun load-data (lof &optional (verbose? nil))
  (let ((data (list))
        (in-arr (make-array *board-size* :initial-element 0))
        (out-arr (make-array *board-size* :initial-element 0))
        (line nil)
        )
    ;; For evey files
    (dolist (file-path lof)
      (when verbose? (format t "Reading file ~A" file-path))
      (with-open-file (file file-path :direction :input)
        (when verbose? (format t "~A lines to read~%" (floor (/ (file-length file) 
                                                                (* 2 *board-size*)))))
        ;; For every line in the file
        (dotimes (j (floor (/ (file-length file) (* 2 *board-size*))))
          ;; Ensure the file isn't empty
          (unless (peek-char t file nil nil)
            (return))
          ;; Get the input
          (dotimes (i *board-size*)
            (setf (svref in-arr i) (read file)))
          ;; Get the output
          (dotimes (i *board-size*)
            (setf (svref out-arr i) (read file)))
          ;; Add it to the list of training data
          (push (list in-arr out-arr) data)
          ))
      ;; Add the array pair to the list
      )
    (format t "Created ~A (state, action) pairs~%" (length data))
    ;; Return the lists
    data))


;; Open NUM files and convert them into training data
(defun load-files (num-files)
  (load-data (make-parse-list num-files)))

;; Synchonized pool for storing instances of the trained network
(defstruct (pool (:include synchronizing-structure))
  nets
  )

(defun init-pool (nn num-nets)
  (let ((p (make-pool)))
    (with-locked-structure 
      (p)
      (dotimes (i num-nets)
        (push (deep-copy-nn-outputs nn) (pool-nets p))))
    p))

(defun init-nn-pool (&optional (net-name nil))
  (cond
    (net-name
      (init-pool (read-network net-name) *num-cores*)
      )
    (t (let* ((files (load-files 60000))
              (nn (init-nn (list 81 65 49 65 81)))
              )
         ;; Train
         (train-all nn 0.25 files)
         ;; Make Copies
         (init-pool nn *num-cores*)
         ))
    ))


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




;; Class for storing a network in AllegroCache
;(defclass neural-net ()
; ((id :initarg :id :reader id :index :any-unique)
;  (gw1 :initarg :gw1 :reader gw1) ; Input to Second connections
;  (gw2 :initarg :gw2 :reader gw2) ; Second to Third connections
;  (gw3 :initarg :gw3 :reader gw3) ; Third to Fourth connections
;  (gw4 :initarg :gw4 :reader gw4)  ; Fourth to Output connections
;  ; (gn1) ; Input Layer
;  ; (gn2) ; Second Layer
;  ; (gn3) ; Third Layer
;  ; (gn4) ; Fourth Layer
;  ; (gn5) ; Output Layer
;  )
; (:metaclass persistent-class))

;(defun make-nn ()
;  (compile-file "nn-go")
;  (load "nn-go")
;  (compile-file "2014-new-nn")
;  (load "2014-new-nn"))



