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



(defun net-to-string (nn)
  (concatenate 'string 
               (write-to-string (nn-family-name nn))
               "-"
               (write-to-string (nn-id nn))))

;; NOTE: I cannot believe that it's this easy to read/write from files

;;  WRITE-NETWORK : 
;;  INPUT: FILENAME - A string representing where the file will be written
;;         NN - A neural network struct
;;  OPTIONAL: FORCE? - Overwrite file with same name if one exists
;; ---------------------------------------------
;;  Writes a NN to a space deliminated text file
(defun write-network 
  (nn &optional (filename nil) (force? nil))
      (unless filename
        (setq filename (string-downcase (net-to-string nn))))
  ;; Network files are put in the 'networks' sub directory
  (let ((file-path (make-pathname :name (concatenate 'string "../networks/" filename ".net")))
        (file nil))
    ;; Overwrite the existing file on close if the force flag is set
    (if force? 
      (setq file (open file-path :direction :output :if-exists :supersede))
      (setq file (open file-path :direction :output)))
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

    (init-nn size-list weight-arrays name id)))


(defmacro read-nets (lof)
  `(let ((lon (list)))
     (dolist (filename ,lof)
       (push (read-network filename) lon)
       )
     lon))

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
               (list "charles" (list 81 67 81) 0.9)
               (list "direct" (list 81 81) 0.25)
               ))

(defconstant *lon*
             (reverse (list 
                              "full-conn-0.25-0"
                              "full-conn-0.5-0" 
                              "full-conn-0.75-0"
                              "full-conn-1-0" 
                              "small-conn-0.25-0"
                              "small-conn-0.5-0"
                              "small-conn-0.75-0"
                              "small-conn-1-0"
                              "full-conv-0.25-0"
                              "full-conv-0.5-0"
                              "full-conv-0.75-0"
                              "full-conv-1-0"
                              "small-conv-0.25-0"
                              "small-conv-0.5-0"
                              "small-conv-0.75-0"
                              "small-conv-1-0"
                              "charles-0" 
;                              "direct-0" 
                              )))
             ;;  TRAIN-NETWORKS
;; ---------------------
;; INPUTS: LOT, a list of triples 
;;              first = network-name
;;              second = layer-sizes 
;;              third = training-rate
;; For training multiple networks at the same time
;; so the files don't have to be opened mulitple times
(defun train-networks (lot)
  (let ((files (load-files 60000)))
    (dolist (triple lot)
      ;; Can't use apply cause files is a list
      ;(format t "Applying ~A~%" triple)
      (process-store-nn (first triple)
                        (second triple)
                        (third triple)
                        files
                        ))))
;; Trains the networks with the names and parameters 
;; passed in as a list of triples
(defmacro p-train-networks (lot)
  `(mp:process-run-function (write-to-string ,lot) #'train-networks ,lot))

;;  COMPETE : BLACK-NUM-SIMS BLACK-C WHITE-NUM-SIMS WHITE-C
;; --------------------------------------------------
;;  NOTE:  Compete has a lot of functionality for various situations
;;           for simply watching two A.I.'s player it's recommended to 
;;           use one of the macros defined below
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           WHITE-NUM-SIMS, the number of simulations for each of white's moves
;;           WHITE-C, the exploration/exploitation constant used by white
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun compete
  (black-num-sims black-c white-num-sims white-c 
                  &optional 
                  (black-threads? nil)
                  (white-threads? nil)
                  (black-network  nil)
                  (white-network  nil)
                  (pool nil)
                  (return-game? nil)
                  (filename nil)
                  (verbose? t)
                  )
  (let ((g (init-game))
        (b-p nil)
        (w-p nil)
        )

    ;; Record a random board state from the game along with who won the game
    (labels ((record-game 
               (game)
               ;; Get a random board state
               (let* (
                      (store-board  (nth (random (length (gg-board-history game)))
                                         (gg-board-history game)))
                      (score (- (svref (gg-subtotals game) *black*)
                                (svref (gg-subtotals game) *white*)))
                      (winner nil)
                      (set-struct (make-compete-settings 
                                    :b-sims black-num-sims 
                                    :b-c black-c 
                                    :w-sims white-num-sims 
                                    :w-c white-c 
                                    :b-t black-threads?
                                    :w-t white-threads?
                                    :b-net black-network
                                    :w-net white-network
                                    :pool pool
                                    )
                                  )
                      )
                 ;; If the score is greater than 0, black won
                 (if (> score 0)
                   ;; The network represents black as a 1
                   (setq winner 1)
                   ;; Otherwise white won
                   (setq winner -1))

                 (with-locked-structure 
                   (filename)
                   (with-open-file (file (file-lock-path filename) :direction :output 
                                         :if-does-not-exist :create
                                         :if-exists :append)
                     ;; Write the board state
                     (write-string (write-to-string store-board) file) 
                     ;; Write who won the game
                     (write-string (write-to-string winner) file)
                     (write-line (write-to-string (compete-to-list set-struct )) file)))
                 ))
             )
      (when pool
        (format t "Pool ~A~%" pool)
        (setq b-p pool)
        (setq w-p pool))

      (when black-network
        (setq b-p (init-nn-pool black-network)))

      (when white-network
        (setq w-p (init-nn-pool white-network)))

      (while (not (game-over? g))
             (cond
               ((= (gg-whose-turn? g) *black*)
                (when verbose? (format t "BLACK'S TURN!~%"))
                (if verbose? (time (do-move! g (uct-search g black-num-sims black-c nil black-threads? b-p)))
                  (do-move! g (uct-search g black-num-sims black-c nil black-threads? b-p)))
                (when verbose? (print-go g t nil t nil nil)))
               (t
                 (when verbose? (format t "WHITE'S TURN!~%"))
                 (if verbose? (time (do-move! g (uct-search g white-num-sims white-c nil white-threads? w-p)))
                   (do-move! g (uct-search g white-num-sims white-c nil white-threads? w-p)))
                 (when verbose? (print-go g t nil t nil nil)))))

      ;; Show all game information
      (when verbose? (print-go g t nil t t))

      ;; If a record from the game is to be used
      (when filename
        ;; Record 4 states b/c don'thave much time
        (dotimes (i 4) 
          (record-game g)
          )
        )
      ;; Explicitly clear the pool
      (setq w-p nil)
      (setq b-p nil)


    ;; Return the final game state if requested
    (when return-game? g))))

(defun play-nets-no-t (net1 net2)
  (compete 81 2 1 2 nil nil net1 net2)
  )

(defun play-nets (net1 net2)
  (compete 10 1 1 1 t t net1 net2 nil nil t)
  )

(defun play-b-net (net)
  (compete 250 1 81 1 4 4 net nil nil nil t)
  )

(defun play-mcts (b-num w-num)
  (compete b-num 2 w-num 2 nil nil nil nil nil))


;; I'm using an evolutionary algorithm instead of 
;; training on the results of the matches b/c the network
;; trained that way wasn't better when combined with the 
;; Value network in the original paper so I'm going to 
;; try something else
(defstruct (competetor
             (:conc-name c-)
             (:print-function print-comp)
             (:include synchronizing-structure))
  id        ;; A number that is unique within each generation
  net       ;; A NN
  age       ;; The number of gens this network has been a competetor
  fitness   ;; The fitness of this competetor in the current generation
  dominated ;; A parameter for use with AFPO
  )
(defun print-comp (comp str depth)
  (declare (ignore depth))
  (format t "~A-~A~%" (nn-family-name (c-net comp))
          (nn-id(c-net comp))))

(defun new-competetor (net id)
  (make-competetor :net net 
                  :age 0
                  :fitness 0
                  :dominated nil))

;; Returns a list of pairs containing every combination of 
;; pairs of two networks provided
;; These lists are akin to the pareto fronts in AFPO
(defmacro make-pairs (loc)
  `(let ((pairs (list))
         (comp1 nil)
         )
     (loop while (< 0 (length ,loc))
           do (setq comp1 (pop ,loc))
           (dolist (comp2 ,loc)
             (push (list comp1 comp2) pairs)))
     pairs))


;; Non-dominated before dominated then 
;; by fitness
(defun most-fit (comp1 comp2)
  (if (or (and (not (c-domniated comp1))
               (c-dominated comp2))
          (> (c-fit comp1) 
             (c-fit comp2)))
    t nil))

;; Macro for returning the next generation of 
;; competetors
(defmacro get-next-gen (generation)
  `(let ((next-gen (list))
         (child nil)
         (gen-id 0)
         )
     (format t "Sorting gen~%")
     ;; Sort the list by highest fitness
     (sort ,generation :test #'most-fit)
     (format t "Sorting gen~%")

     ;; For each member of the population
     (dolist (comp population)
       ;; Every non-dominated ID gets a mutant offspring 
       ;; added to the next-generation
       (unless (c-domniated comp)
         (setq child (new-competetor (nn-mutate (c-net comp)) gen-id))
         (write-network child)
         (push child 
               next-gen)
         (incf gen-id))
       ;; Increment age
       (incf (c-age comp))
       ;; Reset fitness
       (setf (c-fitness comp) 0)
       ;; Reset dominated
       (setf (c-domniated comp) nil)
       ;; Add it to the next generation
       (push comp next-gen)

       ;; Return when all non-dominated networks
       ;; are added and the previous population size
       ;; has been reached
       (when (and 
               (c-dominated comp)
               (>= (length next-gen)
                   (length ,generation)))
         (return)))
     ;; Return the next generation
     next-gen))

;; Basically a wrapper for compete
(defun gaunlet (net1 net2 file-lock)
  (compete 100 1 100 1 2 2
           (net-to-string net1)
           (net-to-string net2) 
           nil t 
           file-lock
           nil))

;; Has two competitors play two games against each other 
;; once each as black and white. The player with the combined
;; higher score from both games is the winner (ties are a one 
;; point win for white)
(defun face-off (pair gen barrier file-lock)
  (let ((comp1 (first pair))
        (comp2 (second pair))
        (score1 0)
        (score2 0)
        (game nil)
        )

    ;; Play a game recording two random board states 
    ;; as well as who won in this generation's game-history
    (setq game (gaunlet (c-net comp1) (c-net comp2) file-lock))
    (cond
      ;; If a tie count as a one point victory for white
      ((= (svref (gg-subtotals game) *black*)
          (svref (gg-subtotals game) *white*))
       (incf score2 1))
      (t 
        ;; First comp1 is black
        (incf score1 (svref (gg-subtotals game) *black*))
        ;; And comp2 is white
        (incf score2 (svref (gg-subtotals game) *white*))
        ))

    ;; Then they switch
    (setq game (gaunlet (c-net comp2) (c-net comp1) file-lock))
    (cond
      ;; If a tie count as a one point victory for white
      ((= (svref (gg-subtotals game) *black*)
          (svref (gg-subtotals game) *white*))
       (incf score1 1))
      (t 
        ;; First comp1 is black
        (incf score2 (svref (gg-subtotals game) *black*))
        ;; And comp2 is white
        (incf score1 (svref (gg-subtotals game) *white*))
        ))
    ;; Evaluate the winner
    (cond 
      ;; If comp1 was the victor
      ((> score1 score2)
       ;; Update it's fitness
       (with-locked-structure 
         (comp1)
         (incf (c-fitness comp1)))
       (with-locked-structure 
         (comp2)
         ;; Update the other fitness
         (decf (c-fitness comp2))
         ;; Set dominated 
         (setf (c-dominated comp2) t)))
      ;; If comp2 was the victor
      ((> score2 score1)
       ;; Update it's fitness
       (with-locked-structure 
         (comp2)
         (incf (c-fitness comp2)))
       (with-locked-structure 
         (comp1)
         ;; Update the other fitness
         (decf (c-fitness comp1))
         ;; Set dominated 
         (setf (c-dominated comp1) t))))
    ;; If there is a tie, neither is dominated 
    ;; or has a change in fitness

    (mp:barrier-pass-through barrier)
    (format t "Thread completing ~A ~%" barrier)))

;; Run evolutionary trials pitting networks against
;; networks and store a few game state pairs from the 
;; game
(defun evolve-networks (lon generations num-fronts &optional (threads? t))
  (let ((lineage-counters (make-array (length lon) :initial-element 1))
        (fronts (make-array num-fronts :initial-element (list)))
        (generation (list))
        (pairs (list)) ; A list of pairs containing every combination of networks in the generation
        (gen-id 0)
        (barrier nil)
        (file-lock  nil)
        (front-count 0)
        )
    ;(format t "LON: ~A~%" lon)
    (dolist (net lon)
      (push (new-competetor net gen-id)
            generation)
      (incf gen-id))

    (format t "Initial Population ~A~%" generation)
    ;; Run the evolutionary algorithm
    (dotimes (gen generations)
      (format t "Running Generation ~A~%" gen)
      ;; Create the lock for this gen's file
      (setq file-lock
            (make-file-lock :path 
                            (make-pathname :name 
                                           (concatenate 'string 
                                                        "../game-records/game-history-gen-" 
                                                        (write-to-string gen)
                                                        "-"
                                                        (short-site-name) ;; Diff machines record different files
                                                        ))))

      ;; Randomly distribute the networks among the fronts
      (dolist (comp generation)
        (push comp (svref fronts front-count))
        (incf front-count)
        (when (= num-fronts front-count)
          (setq front-count 0))
        )
      ;; Reset the fronts as pairs
      (dotimes (i num-fronts)
        (setf (svref fronts i)
              (make-pairs (svref fronts i))))

      ;; Add all the pairs into one list
      (dotimes (i num-fronts)
        (dolist (pair (svref fronts i))
          (push pair pairs)))
      ;; Evaluate each network's fitness
      ;; Do the fronts one at a time to make better use of memory
      ;(dotimes (i num-fronts)
      ;; Reset the barrier
      (setq barrier (mp:make-barrier (+ 1 (length pairs))))
      (dolist (pair pairs);(svref fronts i))
        (format t "Face Off: ~A ~%" pair)
        (if threads? 
          (mp:process-run-function (write-to-string pair) 
                                   #'face-off
                                   pair
                                   gen
                                   barrier
                                   file-lock)
          (face-off pair gen barrier file-lock))
        ;    )
        (format t "Waiting for threads to finish~%")
        ;; Wait for all the trials to finish
        )
      (mp:barrier-wait barrier)
      (format t "Creating next generation~%")
      ;; Get the next generation
      (setq generation (get-next-gen generation))
      ;; Reset pairs
      (setq pairs (list))
      )
    (format t "Done!~%")
    ))
