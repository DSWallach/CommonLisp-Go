;;; ===============================
;;;  CMPU-365, Fall 2010
;;;  NEW-NN.LISP
;;; ===============================
;;;  Implementation of neural networks

;; These calculations need to happen quickly and flaoting
;; point operations don't always need to return the same 
;; results for the lowest bits
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0))))


;;;  NN struct
;;; ------------------------------------------------------
;;;  Neurons and edges are not explicitly represented.  Instead, the 
;;;  information for all the neurons and edges (e.g., output values,
;;;  weights, delta values) is contained in various vectors and arrays.
;;;  For example, in a 3-layer network with 3 input neurons, 4 hidden neurons,
;;;  and 2 output neurons, the NN struct would have:
;;;     NUM-LAYERS = 3
;;;     LAYER-SIZES = (3 4 2)
;;;     OUTPUT-VECKS = vector of 3 vectors (one for each layer)
;;;        output-veck #0 would have 3 entries
;;;        output-veck #1 would have 4 entries
;;;        output-veck #2 would have 2 entries
;;;     DELTA-VECKS = vector of 3 vectors (similar to OUTPUT-VECKS)
;;;     WEIGHT-ARRAYS = vector of 2 weight arrays
;;;        weight-array #0 would be a 3-by-4 array of weights
;;;          (corresponding to weights between 3 input neurons
;;;           and 4 hidden neurons)
;;;        weight-array #1 would be a 4-by-2 array of weights
;;;          (corresponding to weights between 4 hidden neurons
;;;           and 2 output neurons)

;(in-package :nn-go)

(defstruct nn
  ;; FAMILY-NAME: A string for identifying this network's family
  family-name
  ;; ID: An identifier for this network that is unique within this lineage
  id
  ;; NUM-LAYERS:  the number of layers in the neural network
  num-layers    
  ;; LAYER-SIZES:  a vector specifying the number of neurons in each layer
  ;;   (The input layer is layer 0; the output layer is layer (1- num-layers)
  layer-sizes   
  ;; OUTPUT-VECKS:  A vector of vectors.
  ;;  Each neuron has an associated output value.
  ;;  (svref output-vecks i) -- is a vector of the computed output
  ;;        values for the neurons in layer i.
  output-vecks 
  ;; WEIGHT-ARRAYS:  A vector of arrays.
  ;;  (svref weight-arrays i) -- is an MxN array holding the weights
  ;;        for all edges between layer i and layer (i+1).
  ;;        M = number of neurons in layer i; N = number of neurons in layer (i+1)
  weight-arrays 
  ;; DELTA-VECKS:  A vector of vectors.
  ;;  Each neuron (except those in the input layer) has an associated
  ;;  DELTA value computed during back-propagation.
  ;;  (svref delta-vecks i) -- is a vector of the delta values for
  ;;       the neurons in layer i.
  delta-vecks   
  )

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

;;  ANNALYZE-BOARD : NN BOARD PLAYER
;; ------------------------------------------
;;  Analyze the scores of the various moves at
;;  the given board position
(defmacro annalyze-board (nn board player)
  ;; Get the output of the nn for the given input (board state)
  `(if (eq ,player *white*)
     (get-output-for ,nn (invert-board ,board))
     (get-output-for ,nn ,board)))

;;  ANNALYZE-MOVES : NN BOARD LEGAL-MOVES PLAYER
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
          (setq best-move move))))
    ;(format t "Best Move ~A Score ~A~%" best-move best-score)
    ;(if (< 0.001 (abs best-score))
      best-move
      ;; If isn't above the threashold return pass as the best move 
    ;  *board-size*)
    ))


(defun net-to-string (nn)
  (string-downcase 
    (concatenate 'string 
               (write-to-string (nn-family-name nn))
               "-"
               (write-to-string (nn-id nn)))))

;;  WRITE-NETWORK : 
;;  INPUT: FILENAME - A string representing where the file will be written
;;         NN - A neural network struct
;;  OPTIONAL: FORCE? - Overwrite file with same name if one exists
;; ---------------------------------------------
;;  Writes a NN to a space deliminated text file
(defun write-network 
  (nn &optional (force? nil))
  ;; Network files are put in the 'networks' sub directory
  (let* ((filename (net-to-string nn))
         (file-path (make-pathname :name (concatenate 'string "../networks/" filename ".net")))
         (file nil))
    ;; Overwrite the existing file on close if the force flag is set
    (if force? 
      (setq file (open file-path :direction :output 
                       :if-does-not-exist :create
                       :if-exists :supersede))
      (setq file (open file-path :direction :output 
                       :if-does-not-exist :create
                       :if-exists :rename)))
    ;; Write the family-name
    (write-string "family-name " file) 
    (write-line (write-to-string (nn-family-name nn)) file)

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
  (let ((file-path (make-pathname :name (string-downcase (concatenate 'string "../networks/" filename ".net"))))
        (read-nn nil)
        (name nil)
        (id 0)
        (layer-sizes nil)
        (weight-arrays nil)
        (size-list (list))
        )
    (with-open-file (line file-path)
      (when (string-equal "family-name" (read line))
        (setq name (read line))
        )
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

    ;(format t "Close File~%")
    ;; Convert layer sizes to list from vector
    (dotimes (i (length layer-sizes))
      (push (svref layer-sizes (- (length layer-sizes) i 1))
            size-list))

    ;; Make the new nn
    (setq read-nn (init-nn size-list weight-arrays name id))
    ;; Explicitly clear the read in values
    (setq layer-sizes nil)
    (setq layer-weights nil)
    (setq size-list nil)
    ;; Return the networks
    read-nn))

;; Only for 2-d arrays
(defun copy-array (arr)
  (let* ((dims (array-dimensions arr))
        (new-arr (make-array dims))
        )
    (dotimes (i (first dims))
      (dotimes (j (second dims))
        (setf (aref new-arr i j)
              (aref arr i j))))
    new-arr))

;;  NN-MUTATE
;; -----------------------------
;;  Returns a fresh NN with weights slightly modified from NN
(defun nn-mutate 
  (nn mutation-rate)
  (let ((name (nn-family-name nn))
        (id (+ 1 (nn-id nn)))
        (num-layers (nn-num-layers nn))
        (layer-sizes (copy-vector (nn-layer-sizes nn)))
        (size-list (list))
        (weight-arrays (copy-vector (nn-weight-arrays nn) #'copy-array))
        )
    (dotimes (lay-num (1- num-layers))
      (let ((weight-array (svref weight-arrays lay-num)))
        ;; For each neuron N_i in that layer...
        (dotimes (i (svref layer-sizes lay-num))
          ;; For each neuron N_j in the following layer...
          (dotimes (j (svref layer-sizes (1+ lay-num)))
            ;; If random is less than the mutation-rate
            (when (> mutation-rate (random 1.0))
              ;; Should be about 50%
              (if (= 0 (random 2))
                ;; Modify the weights by some amount less than the mutation-rate
                (incf (aref weight-array i j)
                      (random mutation-rate))
                (decf (aref weight-array i j)
                      (random mutation-rate))))))))

    ;; Convert layer sizes to list from vector
    (dotimes (i (length layer-sizes))
      (push (svref layer-sizes (- (length layer-sizes) i 1))
            size-list))
    (format t "Mutated ~A-~A~%" name id)
    ;; Return a fresh 
    (init-nn size-list weight-arrays name id)))


;;  NN-EQUAL
;; ------------------------
;; Equality test for two NN's
(defun nn-equal (nn1 nn2)
  (if (and (= (nn-num-layers nn1)
              (nn-num-layers nn2))
           (equalp (nn-layer-sizes nn1)
                   (nn-layer-sizes nn2))
           (equalp (nn-weight-arrays nn1)
                   (nn-weight-arrays nn2))
           )
    t 
    nil))

;;  DEEP-COPY-NN-OUTPUS : NN - A NN struct
;; --------------------------------------
;;  Creates a new neural network with pointers to 
;;  all the network properties of the original network
;;  except the outputs. It creates a new vector for the 
;;  output values of the neuron. This is used to create
;;  multiple copies of a single network for use by the 
;;  multiple threads during MCTS. As the networks are not
;;  trained during the search only the values of the outputs
;;  need to be unique between the networks
(defun deep-copy-nn-outputs (nn)
  (let ((name (nn-family-name nn))
        (id (nn-id nn))
        (layers (nn-num-layers nn))
        (sizes (nn-layer-sizes nn))
        ;; New memory is allocated for outputs 
        (outputs (make-array (length (nn-output-vecks nn))))
        (weights (nn-weight-arrays nn))
        (deltas (nn-delta-vecks nn))
        )

    (dotimes (i layers)
      (setf (svref outputs i) 
            (make-array (svref sizes i)
                        :initial-element 0.0)))

    (make-nn :family-name name
             :id id
             :num-layers layers
             :layer-sizes sizes
             :output-vecks outputs
             :weight-arrays weights
             :delta-vecks deltas)))

(defun deep-copy-nn (nn)
  (let ((name (nn-family-name nn))
        (id (nn-id nn))
        (sizes (nn-layer-sizes nn))
        (size-list (list))
        ;; New memory is allocated for outputs 
        (weights (nn-weight-arrays nn))
        )
    (dotimes (i (length sizes))
      (push (svref sizes (- (length sizes) i 1))
            size-list))


    (init-nn size-list weights name id)))


(defun map-short (arr)
  ;; Create the new array
  (let* ((num-rows (first (array-dimensions arr)))
         (num-cols (second (array-dimensions arr)))
         (new-arr (make-array (list num-rows num-cols) :element-type 'short-float))
         )
    (dotimes (i num-rows)
      (dotimes (j num-cols)
        (setf (aref new-arr i j)
              (coerce (aref arr i j) 'short-float))))
    ;; Return the finished array
    new-arr))



;;;  INIT-NN
;;; -----------------------------------------
;;;  INPUT:  SIZES-OF-LAYERS, a list of numbers indicating how
;;;           many neurons are in each layer.  (Layer 0 corresponds
;;;           to the input layer).
;;;  OUTPUT:  A neural network (NN struct) of that size, initialized
;;;           with weights randomly selected between -0.5 and +0.5.

(defun init-nn (sizes-of-layers &optional 
                                (weight-arrays-init nil)
                                (name "default")
                                (id 0))
  (let* (;; NUM-LAYERS:  the number of layers in the network
         (num-layers (length sizes-of-layers))
         ;; LAYER-SIZES:  a vector whose ith element will say how many
         ;;  neurons are in layer i
         (layer-sizes (make-array num-layers))
         ;; OUTPUT-VECKS:  a vector of vectors.  The ith vector will
         ;;  contain output values for each neuron in layer i
         (output-vecks (make-array num-layers ));:element-type 'short-float)) 
         ;; Short floats to save memory
         ;; NN's are basically approximation functions, they don't need to be super accurate
         ;; DELTA-VECKS:  similar to output-vecks, except they contain
         ;;  the delta values for each neuron
         (delta-vecks (make-array num-layers ));:element-type 'short-float))
         ;; WEIGHT-ARRAYS:  see documentation of NN struct
         (weight-arrays (make-array (1- num-layers)))
         ;; NN: the network
         (nn (make-nn :family-name name
                      :id id 
                      :num-layers num-layers
                      :layer-sizes layer-sizes
                      :output-vecks output-vecks
                      :weight-arrays weight-arrays
                      :delta-vecks delta-vecks)))

    (format t "Init: ~A-~A~%" name id)
    ;; For each layer...
    (dotimes (i num-layers)
      ;; Set the size of that layer (i.e., how many neurons)
      (setf (svref layer-sizes i) (nth i sizes-of-layers))
      ;; Create a vector of output values for the neurons in that layer
      (setf (svref output-vecks i) (make-array (svref layer-sizes i)
                                              ;;:element-type 'short-float
                                               :initial-element ;0.0
                                               nil
                                               ))
      ;; Create a vector of delta values for the neurons in that layer
      (setf (svref delta-vecks i) (make-array (svref layer-sizes i)
                                            ;;  :element-type 'short-float
                                             :initial-element ;0.0
                                             nil
                                              ))
      ;; For non-input neurons, create an array of weights
      ;; corresponding to edges between current layer and previous layer
      (when (> i 0)
        (if weight-arrays-init 
            ;; The array of weights
            (setf (svref weight-arrays (1- i)) 
                  (map-short (svref weight-arrays-init (1- i))))

          ;; Otherwise use random values
          (let* ((num-rows (svref layer-sizes (1- i)))
                 (num-cols (svref layer-sizes i))
                 ;; The array of weights
                 (harry (make-array (list num-rows num-cols))))
            (setf (svref weight-arrays (1- i)) harry)
            ;; randomize weights
            (dotimes (i num-rows)
              (dotimes (j num-cols)
                (setf (aref harry i j) 
                      (- (/ (random 100) 100) 0.5))))))))
    ;; return the NN
    nn))



;;;  ERASE-OUTPUTS
;;; -----------------------------------------------------
;;;  INPUT:  NN, a neural network
;;;  OUTPUT:  T
;;;  SIDE-EFFECT: Destructively modifies NN by setting all output
;;;   values to NIL (usually done before the FEED-FORWARD process).

(defun erase-outputs (nn)
  (let ((out-vecks (nn-output-vecks nn))
        (num (nn-num-layers nn))
        (lay-sizes (nn-layer-sizes nn)))
    ;; For each layer...
    (dotimes (i num)
      (let ((num-neurons (svref lay-sizes i))
            (outputs (svref out-vecks i)))
        ;; For each neuron in that layer...
        (dotimes (j num-neurons)
          ;; Set that neuron's output value to NIL
          ;; svref is for short arrays
          (setf (svref outputs j) 0.0))))
    t))

;;;  SET-INPUTS
;;; --------------------------------------------------
;;;  INPUT:  NN, a neural network
;;;          INPUTS, a list of input values for the input neurons of NN
;;;  OUTPUT: NN
;;;  SIDE EFFECT:  Sets the "output" value of each neuron in the
;;;    input layer to the corresponding value in INPUTS.

(defun set-inputs (nn inputs)
  (declare (:explain :types :variables :calls))
  (let* ((out-vecks (nn-output-vecks nn))
	 ;; OUT-VECK-ZERO:  the vector of "output" values for layer 0 
	 (out-veck-zero (svref out-vecks 0))
	 (num-inputs (svref (nn-layer-sizes nn) 0)))
    (cond
     ;; CASE 1:  INPUTS has the right number of input values
     ((= num-inputs (length inputs))
      ;; For each input value...
      (dotimes (i num-inputs)
	;; Set the "output" value for the corresponding neuron in layer 0 
	(setf (svref out-veck-zero i) (coerce (svref inputs i) 'float)))
      ;; return the NN
      nn)
     ;; Case 2:  Error!
     (t
      (format t "Whoops!  Wrong number of input values for this NN!~%")))))

;;;  SIGMOID
;;; ------------------------------
;;;  SIGMOID(X) = 1/(1 + e^(-x)) -- the sigmoid (or logistic) function
     
(defun sigmoid (x)
  (declare (type 'single-float x))
  (/ 1.0 (+ 1 (exp (- x)))))

;;;  FEED-FORWARD
;;; ----------------------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           INPUTS, a list of input values for the input neurons in NN
;;;  OUTPUT:  NN
;;;  SIDE-EFFECT:  Applies the given INPUT values to the input layer of NN
;;;   and propagates them forward to generate output values for all neurons
;;;   in the network.

(defun feed-forward (nn inputs)
  (declare (:explain :types :variables :calls))
  ;; First, set the output value for each neuron to NIL
  (erase-outputs nn)
  ;; Next, set the "output" value for each neuron in the input layer
  ;; to the corresponding value in INPUTS
  (set-inputs nn inputs)

  (let ((num-layers (nn-num-layers nn))
        (layer-sizes (nn-layer-sizes nn))
        (output-vecks (nn-output-vecks nn))
        (weight-arrays (nn-weight-arrays nn)))

    ;; For each LAYER from 1 onward (i.e., not including the input layer)
    (do ((lay-num 1 (1+ lay-num)))

      ;; Exit Condition
      ((= lay-num num-layers)
       nn)

      ;; Body of DO
      (let* ((outputs (svref output-vecks lay-num))
             (prev-outputs (svref output-vecks (1- lay-num)))
             (num-prev-outputs (length prev-outputs))
             (weight-array (svref weight-arrays (1- lay-num))))
        ;; For each neuron in that layer...
        (dotimes (neuron-num (svref layer-sizes lay-num))
          ;; Compute output value of that neuron 
          (setf (svref outputs neuron-num)
                ;; SIGMOID of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
                ;;  (INPUTS for this neuron are OUTPUTS from neurons 
                ;;     in previous layer)
                (sigmoid (let ((dot-prod 0))
                           (dotimes (j num-prev-outputs)
                             (incf dot-prod
                                   (* (svref prev-outputs j)
                                      (aref weight-array j neuron-num))))
                           dot-prod))))))))

;;;  TRAIN-ONE
;;; ----------------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           ALPHA, a small positive number that specifies the sensitivity
;;;             of updates to the error
;;;           INPUTS, a list of input values for the neurons in the input layer
;;;           TARGET-OUTPUTS, the desired outputs for neurons in the output
;;;             layer, given the specified INPUTS.
;;;  OUTPUT:  NN
;;;  SIDE EFFECT:  Uses FEED-FORWARD to generate output values for
;;;   the given inputs; then uses the BACK-PROPAGATION algorithm to
;;;   generate DELTA values for each neuron (starting from output layer
;;;   and working back to first hidden layer); then uses the DELTA values
;;;   to update each non-input neuron.

(defun train-one (nn alpha inputs target-outputs)
  (declare (:explain :types :variables :calls))
  (feed-forward nn inputs)

  ;; Back prop algorithm...
  (let* ((num-layers (nn-num-layers nn))
	 (layer-sizes (nn-layer-sizes nn))
	 ;; The index for the output layer
	 (last-layer-index (1- num-layers))
	 (num-output-neurons (svref layer-sizes last-layer-index))
	 ;; The index for the layer just before the output layer
	 (penult-layer-index (1- last-layer-index))
	 ;;(num-penult-neurons (svref layer-sizes penult-layer-index))
	 (output-vecks (nn-output-vecks nn))
	 ;;(penult-output-veck (svref output-vecks penult-layer-index))
	 (last-output-veck (svref output-vecks last-layer-index))
	 (delta-vecks (nn-delta-vecks nn))
	 (last-delta-veck (svref delta-vecks last-layer-index))
	 (weight-arrays (nn-weight-arrays nn))
	 ;;(last-weight-array (svref weight-arrays penult-layer-index))
	 )

    ;; for each neuron in the output layer:
    (dotimes (neuron-num num-output-neurons)
      (let* ((target-output (svref target-outputs neuron-num))
             (my-output  (svref last-output-veck neuron-num))
             (diffy (- target-output my-output)))
        ;;   DELTA_J = G'(IN_J) * (Y_J - A_J)
        ;;           = G(IN_J)*(1 - G(IN_J))*(Y_J - A_J)
        ;;           = A_J * (1 - A_J) * (Y_J - A_J)
        (setf (svref last-delta-veck neuron-num)
              (* my-output (- 1 my-output) diffy))))

    ;; for each hidden layer...
    (do ((lay-num penult-layer-index (1- lay-num)))
      ;; exit
      ((= lay-num 0))
      ;; BODY of DO
      ;; ---------------------------
      (let* ((num-neurons (svref layer-sizes lay-num))
             (curr-out-veck (svref output-vecks lay-num))
             (next-delta-veck (svref delta-vecks (1+ lay-num)))
             (my-delta-veck (svref delta-vecks lay-num))
             (num-neurons-next-layer (svref layer-sizes (1+ lay-num)))
             (curr-weight-array (svref weight-arrays lay-num))
             )
        ;; for each neuron in that layer...
        (dotimes (i num-neurons)
          ;; DELTA_I = G'(IN_I) SUM [W_I_J DELTA_J]
          ;;         = G(IN_I) * (1 - G(IN_I)) * SUM [ W_I_J DELTA_J ]
          ;;         = A_I * (1 - A_I) * SUM [ W_I_J DELTA_J ]
          (let* ((my-output (svref curr-out-veck i))
                 (sum (let ((dotty 0))
                        (dotimes (j num-neurons-next-layer)
                          (incf dotty (* (aref curr-weight-array i j)
                                         (svref next-delta-veck j))))
                        dotty)))
            (setf (svref my-delta-veck i)
                  (* my-output (- 1 my-output) sum))))))

    ;; Now, update all of the weights in the network using the DELTA values
    ;;  For each layer...
    (dotimes (lay-num (1- num-layers))
      (let ((weight-array (svref weight-arrays lay-num))
            (delta-veck (svref delta-vecks (1+ lay-num)))
            (output-veck (svref output-vecks lay-num)))
        ;; For each neuron N_i in that layer...
        (dotimes (i (svref layer-sizes lay-num))
          ;; For each neuron N_j in the following layer...
          (dotimes (j (svref layer-sizes (1+ lay-num)))
            ;; Update the weight on the edge from N_i to N_j
            ;; W_I_J += ALPHA * A_I * DELTA_J
            (incf (aref weight-array i j)
                  (* alpha 
                     (svref output-veck i) 
                     (svref delta-veck j)))))))

    ;; return the NN
    nn))

;;;  TRAIN-ALL
;;; ------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           ALPHA, a training sensitivity parameter
;;;           IN-OUT-PAIRS, a list of training data (input-output pairs)
;;;  OUTPUT: NN
;;;  SIDE EFFECT:  Performs feed-forward/back-propagation on each 
;;;                  input-output pair.

(defun train-all (nn alpha in-out-pairs)
  (dolist (pair in-out-pairs)
    (train-one nn alpha (first pair) (second pair)))
  nn)

;;;  TRAIN-FOR-SINE
;;; ---------------------------------------------------
;;;  INPUT:  ALPHA, training sensitivity parameter
;;;          LISTY, a list of row-lengths for the neural network
;;;          (e.g., '(1 4 3 1))
;;;          NUM-TRIALS, number of training examples to run
;;;  OUTPUT:  trained network
;;;  SIDE EFFECT:  creates a neural network and performs NUM-TRIALS
;;;                rounds of training so that the network can "learn"
;;;                how to simulate the sine function

(defun train-for-sine (alpha listy num-trials)
  (let ((nn (init-nn listy)))
    (dotimes (i num-trials)
      (let* ((x (/ (random 100) 16.0))
	     (y (sin (/ x 2))))
	(train-one nn alpha (list x) (list y))))
    nn))

	      		      

;;;  GET-OUTPUT-FOR
;;; --------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           INPUTS, a list of input values for the neurons in the
;;;             input layer of NN
;;;  OUTPUT:  A vector of output values corresponding to those inputs
;;;            (resulting from doing FEED-FORWARD)

(defun get-output-for (nn inputs)
  (declare (:explain :types :variables :calls))
  (feed-forward nn inputs)
  (let* ((num-layers (nn-num-layers nn))
	 (out-vecks (nn-output-vecks nn))
	 )
    (svref out-vecks (1- num-layers))))

;;;  VECTOR->LIST
;;; --------------------------------------------
;;;  INPUT:  VECK, a vector
;;;  OUTPUT:  A list containing the same elements as VECK

(defun vector->list (veck)
  (let ((listy nil))
    (dotimes (i (length veck))
      (push (svref veck i) listy))
    (nreverse listy)))

;;;  COMPARE-VALUES
;;; ------------------------------------------------
;;;  INPUT: NN, a neural network trained to simulate the SINE function
;;;         NUM, number of data points to compare NN vs. actual SINE func.
;;;  OUTPUT: NIL
;;;  SIDE EFFECT:  Displays a comparison of the true SINE values
;;;    and those computed by the network for a variety of inputs.

(defun compare-values (nn num)
  (dotimes (i num)
    (let* ((x (/ i 16))
	   (output (first (vector->list (get-output-for nn (list x)))))
	   (realout (sin (/ x 2))))
      (format t "X: ~6,3F, NN: ~6,3F, REAL: ~6,3F, DIFF: ~6,3F~%"
	      x output realout (- output realout)))))

;;(setf nn (train-for-sine .2 '(1 4 5 4 1) 100000))
;;(setf nn (train-for-sine .2 '(1 4 3 1) 100000))
;;(compare-values nn 50)


;;  Training neural network for XOR

(defun xor (x y)
  (if (= (+ x y) 1) 1 0))

(defun train-for-xor (alpha listy num-trials)
  (let ((nn (init-nn listy)))
    (dotimes (i num-trials)
      (let* ((x (random 2))
	     (y (random 2)))
	(train-one nn alpha (list x y) (list (xor x y)))))
    nn))

(defun train-for-binary (alpha listy num-trials func)
  (let ((nn (init-nn listy)))
    (dotimes (i num-trials)
      (let* ((x (random 2))
	     (y (random 2)))
	(train-one nn alpha (list x y) (list (funcall func x y)))))
    nn))

(defun show-binary-results (nn func)
  (dotimes (x 2)
    (dotimes (y 2)
      (format t "(funk ~A ~A) ==> ~A; NN got: ~A~%" x y
	      (funcall func x y) (get-output-for nn (list x y))))))

(defun show-xor-results (nn)
  (dolist (listy '((0 0 0) (0 1 1) (1 0 1) (1 1 0)))
    (let ((x (first listy))
	  (y (second listy)))
      (format t "(xor ~A ~A) ==> ~A; NN got: ~A~%" x y (xor x y)
	      (get-output-for nn (list x y))))))


;; (setf nn (train-for-xor 1 '(2 4 1) 10000))
;; (show-xor-results nn)
