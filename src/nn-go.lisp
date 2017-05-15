;; Network Training 
;; Trained-Network '(81 65 49 65 81) 0.25
;; policy-network-1a '(81 81 81) 0.5
;; policy-network-1b '(81 49 81) 0.5
;; policy-network-1c '(81 65 81) 0.5
;; policy-network-1d '(81 81 81) 0.25
;; policy-network-1e '(81 49 81) 0.25
;; policy-network-1f '(81 65 81) 0.25
;; policy-network-1g '(81 81 81) 0.75
;; policy-network-1h '(81 49 81) 0.75
;; policy-network-1i '(81 65 81) 0.75


;;  INVERT-BOARD
;; -------------------------------
;;  Inverts the positions of black's and white's pieces
;;  Non-Desctructive so it doesn't need to be undone
(defmacro invert-board (board)
  `(let ((new-board (make-array (length ,board) :initial-element 0)) 
          )
      (dotimes (pos (length ,board) new-board)
        (cond 
          ((= -1 (svref ,board pos))
           (setf (svref new-board pos) 1))
          ((= 1 (svref ,board pos))
           (setf (svref new-board pos) -1))))
      new-board))


;;  ANNALYZE-BOARD : NN BOARD LEGAL-MOVES PLAYER
;; ------------------------------------------
;;  Analyze the scores of the various moves at
;;  the given board position
(defun annalyze-board (nn board legal-moves player)
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

;;  ANNALYZE-move : NN BOARD LEGAL-MOVES PLAYER
;; ------------------------------------------
;;  Analyze the scores of the various moves at
;;  the given board position
(defun annalyze-move (nn board legal-moves player)
  ;; Get the output of the nn for the given input (board state)
  (let ((output nil)
        (move 0)
        (best-move 0)
        (best-score -1000000)
        )
    ;; If it's white's turn invert the board state
    (if (eq player *white*)
      (setq output (get-output-for nn (invert-board board)))
      (setq output (get-output-for nn board)))

    ;(format t "Scores: ~A~%" output)
    ;; For each legal move 
    (dotimes (i (length legal-moves))
      (setq move (svref legal-moves i))
      ;; Skip passing
      (unless (= *board-size* move)
        (when (> (svref output move) best-score)
          (setq best-score (svref output move))
          (setq best-move move)
          )))
    ;(format t "Best Move ~A Score ~A~%" best-move best-score)
    ;; Return the best move
    (if (< 0.0004 best-move)
      best-move
      ;; If isn't above the threashold return pass as the best move 
      *board-size*)
    ))

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
    ;; Write the family-name
    (write-string "family-name " file) 
    (write-line (nn-family-name nn) file)

    ;; Write the ID
    (write-string "ID " file)
    (write-line (write-to-string (nn-id nn)) file)

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
      (write-line (write-to-string (nn-weight-arrays nn)) file))
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
        (name nil)
        (id 0)
        (layer-sizes nil)
        (weight-arrays nil)
        (size-list (list))
        )
    (with-open-file (line file-path)
      (when (string-equal "family-name" (read line))
        (setq name (read line)))
      (when (string-equal "ID" (read line))
        (setq id (read line)))


      ;(format t "Read layer sizes~%")
      ;; Match layer-sizes
      (when (string-equal "layer-sizes" (read line))
        (setq layer-sizes (read line)))

      ;(format t "Read weight arrays~%")
      ;; Match weight-arrays
      (when (string-equal "weight-arrays" (read line))
        (setq weight-arrays (read line))))

    (format t "Close File~%")
    ;; Convert layer sizes to list from vector
    (dotimes (i (length layer-sizes))
      (push (svref layer-sizes (- (length layer-sizes) i 1))
            size-list))

    (init-nn size-list weight-arrays name id)))

;; Create a list of pathnames to the files to parse
(defun make-parse-list 
  (num)
  (let ((path-list (list )))
    (dotimes (i num)
      (push (make-pathname :name 
                           (concatenate 'string 
                                        "../../9x9/game"
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
      (when verbose? (format t "Reading file ~A " file-path))
      (with-open-file (file file-path :direction :input)
        ;; For every line in the file
        (loop while (peek-char t file nil nil)
          do 
          ;; Get the input
          (setq in-arr (read file))
          ;; Get the output
          (setq out-arr (read file))
          ;; Add it to the list of training data
          (push (list in-arr out-arr) data)
          ))
      ;; Add the array pair to the list
      )
    (when verbose? (format t "Created ~A (state, action) pairs~%" (length data)))
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
              (nn (init-nn (list 81 81 49 81 81)))
              )
         ;; Train
         (train-all nn 0.25 files)
         ;; Make Copies
         (init-pool nn *num-cores*)
         ))
    ))

(defmacro process-store-nn (name layers rate files)
  `(mp:process-run-function ,name #'store-nn ,name ,layers ,rate ,files))

(defun store-nn (name layers rate files)
  (let ((nn (init-nn layers nil name)))
    (format t "Network ~A~%" (nn-family-name nn))
    (train-all nn rate files)
    (format t "Training Network ~A~%" (nn-family-name nn))
    (write-network nn name t)
    (format t "Network ~A ~A written~%" (nn-family-name nn) (nn-id nn))
    ))


(defconstant *lot*
             (list 
               (list "full-conn-0.25" (list 81 81 81 81 81) 0.25)
               (list "full-conn-0.5" (list 81 81 81 81 81) 0.5)
               (list "full-conn-0.75" (list 81 81 81 81 81) 0.75)
               (list "full-conn-1" (list 81 81 81 81 81) 1)
               (list "small-conn-0.25" (list 81 81 81) 0.25)
               (list "small-conn-0.5" (list 81 81 81) 0.5)
               (list "small-conn-0.75" (list 81 81 81) 0.75)
               (list "small-conn-1" (list 81 81 81) 1)
               (list "full-conv-0.25" (list 81 65 49 65 81) 0.25)
               (list "full-conv-0.5" (list 81 65 49 65 81) 0.5)
               (list "full-conv-0.75" (list 81 65 49 65 81) 0.75)
               (list "full-conv-1" (list 81 65 49 65 81) 1)
               (list "small-conv-0.25" (list 81 49 81) 0.25)
               (list "small-conv-0.5" (list 81 49 81) 0.5)
               (list "small-conv-0.75" (list 81 49 81) 0.75)
               (list "small-conv-1" (list 81 49 81) 1)
               ))

;;  TRAIN-NETWORKS
;; ---------------------
;; INPUTS: LOT, a list of triples 
;;              first = network-name
;;              second = layer-sizes 
;;              third = training-rate
;; For training multiple networks at the same time
;; so the files don't have to be opened mulitple times
(defun train-networks (lot)
  (let ((files (load-files 6))
        (triple nil))
    (dolist (triple lot)
      (setq triple (concatenate 'list triple files))
      (format t "Applying ~A~%" triple)
      (apply #'process-store-nn triple))))

(defmacro p-train-networks (lot)
  `(mp:process-run-function (write-to-string ,lot) #'train-networks ,lot))




(defun evolve-networks (lon)
  
  )
