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

(defun make-value-parse-list 
  (num)
  (let ((path-list (list )))
    (dotimes (i num)
      (push (make-pathname :name 
                           (concatenate 'string 
                                        "../../9x9-Values/game"
                                        (write-to-string i)
                                        ".csv"))
            path-list))
    path-list))



(defun shuffle-listy (listy)
  (do* ((listy (copy-listy listy) listy)
        (length  (length listy) (1- length))
        (current listy (cdr listy)))
    ((< 2 length) listy)
    (rotatef (first current) (elt current (random length)))))

(defun make-fix (num)
  (coerce num 'fixnum))

;;  Load the training data into memory
;; -------------------------------------
;;  INPUT: List of files
(defun load-data (lof &optional (verbose? nil))
  (let ((data (list))
        (in-arr (make-array *board-size* :initial-element 0))
        (out-arr (make-array *board-size* :initial-element 0))
        (line nil))
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
              (push (list (map 'vector #'make-fix in-arr)
                          (map 'vector #'make-fix out-arr))
                    data))))
    (when verbose? (format t "Created ~A (state, action/value) pairs~%" (length data)))
    ;; Return the lists
    data))

;; Open NUM files and convert them into training data
(defun load-files (num-files)
  (load-data (make-parse-list num-files)))

;; Synchonized pool for storing instances of the trained network
(defstruct (pool (:include synchronizing-structure))
  nets
  )


;; Needs to start with a fresh nn
;; having multiple pools of the same nn causes memory issues
(defun init-pool (nn num-nets)
  (let ((p (make-pool))
        (net (deep-copy-nn nn)))
    (with-locked-structure 
      (p)
      (dotimes (i num-nets)
        (track (push (deep-copy-nn-outputs nn) (pool-nets p)))))
    (format t "Create pool of size ~A from net ~A~%"
            (length (pool-nets p))
            (net-to-string (first (pool-nets p)))
            )
    p))

(defun init-nn-pool (net-name num-cores)
  (init-pool (read-network net-name) num-cores))

(defmacro process-store-nn (triple files)
      ;; Can't use apply cause files is a list
  `(mp:process-run-function (write-to-string ,triple) #'store-nn ,triple ,files))

(defun store-nn (triple files)
;; Triple is : Name, Layers, Training-Rate
  (let ((nn (init-nn (second triple) nil (first triple)))
        )
    (format t "Network ~A~%" (nn-family-name nn))
    (train-all nn (third triple) files)
    (format t "Training Network ~A~%" (nn-family-name nn))
    (write-network nn t)
    (format t "Network ~A ~A written~%" (nn-family-name nn) (nn-id nn))
    ))

(defconstant *lov*
             (list 
               (list "v-full-conn-0.25" (list 81 49 25 9 1) 0.25)
               (list "v-full-conn-0.5" (list 81 49 25 9 1) 0.5)
               (list "v-full-conn-0.75" (list 81 49 25 9 1) 0.75)
               (list "v-full-conn-1" (list 81 49 25 1) 1)
               (list "v-full-conv-0.25" (list 81 49 25 1) 0.25)
               (list "v-full-conv-0.5" (list 81 49 25 1) 0.5)
               (list "v-full-conv-0.75" (list 81 49 25 1) 0.75)
               (list "v-full-conv-1" (list 81 49 25 1) 1)
               (list "v-small-conn-0.25" (list 81 49 1) 0.25)
               (list "v-small-conn-0.5" (list 81 49 1) 0.5)
               (list "v-small-conn-0.75" (list 81 49 1) 0.75)
               (list "v-small-conn-1" (list 81 49 1) 1)
               (list "v-small-conv-0.25" (list 81 25 1) 0.25)
               (list "v-small-conv-0.5" (list 81 25 1) 0.5)
               (list "v-small-conv-0.75" (list 81 25 1) 0.75)
               (list "v-small-conv-1" (list 81 25 1) 1)))
(defconstant *lov-full* 
             (list 
               (list "v-full-conn-0.25" (list 81 49 25 9 1) 0.25)
               (list "v-full-conn-0.5" (list 81 49 25 9 1) 0.5)
               (list "v-full-conn-0.75" (list 81 49 25 9 1) 0.75)
               (list "v-full-conn-1" (list 81 49 25 1) 1)
               (list "v-full-conv-0.25" (list 81 49 25 1) 0.25)
               (list "v-full-conv-0.5" (list 81 49 25 1) 0.5)
               (list "v-full-conv-0.75" (list 81 49 25 1) 0.75)
               (list "v-full-conv-1" (list 81 49 25 1) 1)))

(defconstant *lov-small* 
             (list 
               (list "v-small-conn-0.25" (list 81 49 1) 0.25)
               (list "v-small-conn-0.5" (list 81 49 1) 0.5)
               (list "v-small-conn-0.75" (list 81 49 1) 0.75)
               (list "v-small-conn-1" (list 81 49 1) 1)
               (list "v-small-conv-0.25" (list 81 25 1) 0.25)
               (list "v-small-conv-0.5" (list 81 25 1) 0.5)
               (list "v-small-conv-0.75" (list 81 25 1) 0.75)
               (list "v-small-conv-1" (list 81 25 1) 1)))
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
               (list "direct" (list 81 81) 0.25)))
(defconstant *s-conn* (list
                              "small-conn-0.25-0"
                              "small-conn-0.5-0"
                              "small-conn-0.75-0"
                              "small-conn-1-0"))
(defconstant *s-conv* (list
                              "small-conv-0.25-0"
                              "small-conv-0.5-0"
                              "small-conv-0.75-0"
                              "small-conv-1-0"))
(defconstant *f-conv* (list
                              "full-conv-0.25-0"
                              "full-conv-0.5-0"
                              "full-conv-0.75-0"
                              "full-conv-1-0"))
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
             ;                 "charles-0" 
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
      (format t "Training ~A~%" triple)
      (process-store-nn triple
                        files))))
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
                  (black-p-network  nil)
                  (white-p-network  nil)
                  (black-v-network  nil)
                  (white-v-network  nil)
                  (pool nil)
                  (return-game? nil)
                  (filename nil)
                  (verbose? t)
                  )

  ;; Record a random board state from the game along with who won the game
  (labels ((record-game 
             (game)
             ;; Get a random board state
             (let* ((store-board  (nth (random (length (gg-board-history game)))
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
                                  :b-nets (list black-v-network black-p-network)
                                  :w-nets (list black-v-network white-p-network)
                                  :pool pool)))
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
                   (write-line (write-to-string (compete-to-list set-struct)) file))))))
    (let ((g (init-game))
          (b-p-pool nil)
          (w-p-pool nil)
          (b-v-pool nil)
          (w-v-pool nil)
          )
      ;;; Initialize policy nets
      (when black-p-network
        ;; When provided a string
        (if (stringp black-p-network)
          ;; Load the network
          (setq b-p-pool (init-nn-pool black-p-network black-threads?))
          ;; Otherwise were provided a network so use it
          (setq b-p-pool (new-pool black-p-network black-threads?))))

      (when white-p-network
        ;; When provided a string
        (if (stringp white-p-network)
          ;; Load the network
          (setq w-p-pool (init-nn-pool white-p-network white-threads?))
          ;; Otherwise were provided a network so use it
          (setq w-p-pool (new-pool white-p-network white-threads?))))

      ;; Initialize value nets
      (when black-v-network
        ;; When provided a string
        (if (stringp black-v-network)
          ;; Load the network
          (setq b-v-pool (init-nn-pool black-v-network black-threads?))
          ;; Otherwise were provided a network so use it
          (setq b-v-pool (new-pool black-v-network black-threads?))))

      (when white-v-network
        ;; When provided a string
        (if (stringp white-v-network)
          ;; Load the network
          (setq w-v-pool (init-nn-pool white-v-network white-threads?))
          ;; Otherwise were provided a network so use it
          (setq w-v-pool (new-pool white-v-network white-threads?))))

      (loop while (not (game-over? g))
            do (cond
                 ((eq (gg-whose-turn? g) *black*)
                  (when verbose? (format t "BLACK'S TURN!~%"))
                  (if verbose? (time (do-move! g (uct-search g black-num-sims black-c nil black-threads? b-p-pool b-v-pool)))
                    (do-move! g (uct-search g black-num-sims black-c nil black-threads? b-p)))
                  (when verbose? (print-go g t nil nil nil nil)))
                 (t
                   (when verbose? (format t "WHITE'S TURN!~%"))
                   (if verbose? (time (do-move! g (uct-search g white-num-sims white-c nil white-threads? w-p-pool b-v-pool)))
                     (do-move! g (uct-search g white-num-sims white-c nil white-threads? w-p)))
                   (when verbose? (print-go g t nil nil nil nil)))))

      ;; Show most game information at the end
      (when verbose? (print-go g t nil t t))

      ;; If a record from the game is to be used
      (when filename
        ;; Record 4 states b/c don'thave much time
        (dotimes (i 4) 
          (record-game g)))
      ;; Return the final game state if requested
      (when return-game? g))))

(defun play-nets-no-t (net1 net2)
  (compete 81 2 1 2 nil nil net1 net2)
  (gc t))

(defun play-p-nets (net1 net2 file-lock)
  (compete 750 1 750 1 16 16 net1 net2 nil nil nil nil file-lock))

(defun play-nets (net1 net2 net3 net4 file-lock)
  (compete 750 1 750 1 16 16 net1 net2 net3 net4 nil nil file-lock))

(defun play-nets-sims (net1 net2 sims1 sims2 file-lock)
  (compete sims2 (+ 1 (random 4)) sims1 (+ 1 (random 4)) 16 16 net1 net2 nil nil file-lock))

(defun play-b-net (net)
  (compete 750 1 750 1 1 1 net nil nil nil file-lock))

(defun play-mcts (b-num w-num)
  (compete b-num 2 w-num 2 nil nil nil nil nil))

(defun run-sims (num)
  (dolist (net1 *lon*)
    (dolist (net2 *lon*)
      (unless (equalp net1 net2)
        (dotimes (i num)
          (play-nets-sims net1 net2 (+ 250 i) (+ 250 i) file-lock))))))

(defmacro p-run-sims (num)
  `(mp:process-run-function (concatenate 'string "number-" (write-to-string ,num))
                            #'run-sims
                            ,num
                            ))


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
             (when (and comp1 comp2)
               (push (list comp1 comp2) pairs)))
           )
     pairs))

;; Non-dominated before dominated then 
;; by fitness
(defun most-fit (comp1 comp2)
  (if (or (and (not (c-dominated comp1))
               (c-dominated comp2))
          (> (c-fitness comp1) 
             (c-fitness comp2)))
    t nil))

;; Macro for returning the next generation of 
;; competetors
(defmacro get-next-gen (generation)
  `(let ((next-gen (list))
         (gen nil)
         (child nil)
         (gen-id 0)
         )
     ;; Sort the list by highest fitness
     (setq gen (sort ,generation #'most-fit))

     ;; For each member of the population
     (dolist (comp gen)
       ;; Every non-dominated ID gets a mutant offspring 
       ;; added to the next-generation
       (unless (c-dominated comp)
         (setq child (new-competetor (nn-mutate (c-net comp) 0.25) gen-id))
         (format t "Child ~A~%" child)
         (write-network (c-net child))
         (push child 
               next-gen)
         (incf gen-id))
       ;; Increment age
       (incf (c-age comp))
       ;; Reset fitness
       (setf (c-fitness comp) 0)
       ;; Reset dominated
       (setf (c-dominated comp) nil)

       ;; Return when all non-dominated networks
       ;; are added and the previous population size
       ;; has been reached
       (if (or (not (c-dominated comp))
               (<= (length next-gen)
                   (length ,generation)))
       ;; Add it to the next generation
       (push comp next-gen)
       ;; Otherwise discard it
       (setq comp nil)))

     ;; Return the next generation
     next-gen))

;; Basically a wrapper for compete
(defun gaunlet (net1 net2 file-lock)
  (compete 250 1 250 1 2 2
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
    (format t "Round 1 ~A" pair)

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

    (format t "Round 2 ~A" pair)
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
(defun evolve-networks (lon generations num-fronts file-lock &optional (threads? nil))
  (let ((lineage-counters (make-array (length lon) :initial-element 1))
        (fronts (make-array num-fronts :initial-element (list)))
        (generation (list))
        (pairs (list)) ; A list of pairs containing every combination of networks in the generation
        (gen-id 0)
        (barrier nil)
        (front-count 0)
        )
    ;(format t "LON: ~A~%" lon)
    (time (dolist (net lon)
            (push (new-competetor net gen-id)
                  generation)
            (incf gen-id)))

    (format t "Initial Population ~A~%" generation)

    ;; Run the evolutionary algorithm
    (dotimes (gen generations)
      (format t "Running Generation ~A~%" gen)

      ;; Randomly distribute the networks among the fronts
      (dolist (comp generation)
              (push comp (svref fronts front-count))
              (incf front-count)
              (when (= num-fronts front-count)
                (setq front-count 0)))

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
      (format t "Pairs: ~A ~%" pairs)
      (dolist (pair pairs);(svref fronts i))
(when (and (first pair)
(second pair))
        (format t "Face Off: ~A ~%" pair)
        (if threads? 
          (mp:process-run-function (write-to-string pair) 
                                   #'face-off
                                   pair
                                   gen
                                   barrier
                                   file-lock)
          (face-off pair gen barrier file-lock)
          )
)
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
      (setq file-lock nil) 
      )
    (format t "Done!~%")
    ))


(defmacro init-lock (gen)
      ;; Create the lock for this gen's file
      `(setq file-lock
            (make-file-lock :path 
                            (make-pathname :name 
                                           (concatenate 'string 
                                                        "../game-records/game-history-gen-" 
                                                        (write-to-string ,gen)
                                                        "-"
                                                        (short-site-name) ;; Diff machines record different files
							".dat"
                                                        )))))

(defmacro prep-nets (num)
  `(read-nets (subseq *lon* 0 ,num)))

(defmacro run-evo (nets gens fronts file-lock)
  `(mp:process-run-function (concatenate 'string "EVO-" (write-to-string (length ,nets))
"-"
(write-to-string ,gens)
"-"
(write-to-string ,fronts)
)
                            #'evolve-networks ,nets ,gens ,fronts ,file-lock nil))



(defmacro run-evos (num lon lock)
`(dotimes (i ,num)
(setq nets (nth (random (length ,lon)) ,lon))
(run-evo nets (random 10) 2 ,lock)
)
)
