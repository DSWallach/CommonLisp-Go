
;;;  WRAPPERS FOR COMPETE
;;; ------------------------------------

;; Use these to compare the performance of MCTS with and without the network
(defmacro play ()
 `(mp:process-run-function "asdf" #'play-p-nets "small-conv-0.75-0" "small-conn-0.75-0"))
(defmacro play-b ()
 `(mp:process-run-function "asdf" #'play-b-net "small-conv-0.75-0"))

(defun play-threaded-vs-seq ()
  ;; Give a very large number of sims so they'll be restricted
  ;; by the time limit for a fair comparison
  (compete 10000 0.75 10000 0.75 :black-threads 16 :time-limit 15000 :print-game? t))

(defun play-nets-no-t (net1 net2)
  (compete 81 2 1 2 nil nil net1 net2)
  (gc t))

(defun play-p-nets (net1 net2 &optional (file-lock nil))
  (compete 750 1 750 1
           :black-threads 1
           :white-threads 1
           :black-p-network net1
           :white-p-network net2
           :file-lock file-lock))

(defun play-nets (net1 net2 net3 net4 file-lock)
  (compete 750 1 750 1
           :black-threads 1
           :white-threads 1
           :black-p-network net1
           :white-p-network net2
           :black-v-network net3
           :white-v-network net4
           :file file-lock))

(defun play-nets-sims (net1 net2 sims1 sims2 file-lock)
  (compete sims2 (+ 1 (random 4)) sims1 (+ 1 (random 4)) 16 16 net1 net2 nil nil file-lock))


(defun play-b-net (net)
  (compete 500 0.75 500 0.75 :black-threads 1 :white-threads 1 :black-p-network net :print-game? t :time-limit 10000))



;; Used during network evolution
(defun gauntlet (net1 net2 file-lock)
  (compete 750 0.5 750 0.5
           :black-threads 8
           :white-threads 8
           :black-p-network net1
           :white-p-network net2
           :file file-lock
           :time-limit 10000
           ))

;; Read in a bunch of nn's
(defun read-nets (lof)
  (let ((lon (list)))
    (dolist (filename lof)
      (push (read-network filename) lon))
    lon))

;; Create a list of pathnames to the files to parse for policy training
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
;; Create a list of pathnames to the file to parse for value training
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

(defun make-fix (num)
  (coerce num 'fixnum))

;;  Load the training data into memory
;; -------------------------------------
;;  INPUT: List of files
(defun load-data (lof &optional (verbose? nil))
  (let ((data (list))
        (in-arr (make-array *board-size* :initial-element 0))
        (out-arr (make-array *board-size* :initial-element 0)))
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
  nets)

;; Needs to start with a fresh nn otherwise creating 
;; multiple pools of the same nn causes memory issues
(defun init-pool (nn num-nets)
  (format t "INital pool~%")
  ;; Handle a non-threaded run of compete
  (unless num-nets
    (setq num-nets 1))
  (let ((p (make-pool))
        (net (deep-copy-nn nn)))
    (with-locked-structure 
      (p)
      (dotimes (i num-nets)
        (push (deep-copy-nn-outputs net) (pool-nets p))))
    (format t "Create pool of size ~A from net ~A~%"
            (length (pool-nets p))
            (net-to-string (first (pool-nets p))))
    ;; Return the network pool
    p))

(defmacro init-nn-pool (net-name num-cores)
  `(init-pool (read-network ,net-name) ,num-cores))

(defmacro process-store-nn (triple files)
  ;; Can't use apply cause files is a list
  `(mp:process-run-function (write-to-string ,triple) #'store-nn ,triple ,files))

(defun store-nn (triple files)
  ;; Triple is : Name, Layers, Training-Rate
  (let ((nn (init-nn (second triple) nil (first triple))))
    (format t "Network ~A~%" (nn-family-name nn))
    (train-all nn (third triple) files)
    (format t "Training Network ~A~%" (nn-family-name nn))
    (write-network nn t)
    (format t "Network ~A ~A written~%" (nn-family-name nn) (nn-id nn))))

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
                        "small-conv-1-0"
                        ))
(defconstant *f-conv* (list
                        "full-conv-0.25-0"
                        "full-conv-0.5-0"
                        "full-conv-0.75-0"
                        "full-conv-1-0"
                        ))
(defconstant *conv* (list
                        "small-conv-0.25-0"
                        "small-conv-0.5-0"
                        "small-conv-0.75-0"
                        "small-conv-1-0"
                        "full-conv-0.25-0"
                        "full-conv-0.5-0"
                        "full-conv-0.75-0"
                        "full-conv-1-0"
                      )
             )
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
;; passed in as a list of triples. Runs in a new process.
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
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun compete
  (black-num-sims black-c white-num-sims white-c 
                  &key
                  (black-threads nil)
                  (white-threads nil)
                  (black-p-network  nil)
                  (white-p-network  nil)
                  (black-v-network  nil)
                  (white-v-network  nil)
                  (time-limit 15000)
                  (return-game? nil)
                  (file nil)
                  (print-game? nil)
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
                    (set-list (list
                                  black-num-sims 
                                  black-c 
                                  white-num-sims 
                                  white-c 
                                  black-threads
                                  white-threads
                                  (list black-v-network black-p-network)
                                  (list black-v-network white-p-network)
                                  )))
               ;; If the score is greater than 0, black won
               (if (> score 0)
                 ;; The network represents black as a 1
                 (setq winner 1)
                 ;; Otherwise white won
                 (setq winner -1))

               (with-locked-structure 
                 (file)
                 (with-open-file (file (file-lock-path file) :direction :output 
                                       :if-does-not-exist :create
                                       :if-exists :append)
                   ;; Write the board state
                   (write-string (write-to-string store-board) file) 
                   ;; Write who won the game
                   (write-string (write-to-string winner) file)
                   (write-line (write-to-string set-list) file))))))

    (let ((g (init-game))
          (b-p-pool nil)
          (w-p-pool nil)
          (b-v-pool nil)
          (w-v-pool nil)
          )
      ;;; Initialize policy nets
      (when black-p-network
        (format t "Loading Black Policy Networks ~%")
        ;; When provided a string
        (if (stringp black-p-network)
          ;; Load the network
          (setq b-p-pool (init-nn-pool black-p-network black-threads))
          ;; Otherwise were provided a network so use it
          (setq b-p-pool (init-pool black-p-network black-threads))))

      (when white-p-network
        (format t "Loading White Policy Networks ~%")
        ;; When provided a string
        (if (stringp white-p-network)
          ;; Load the network
          (setq w-p-pool (init-nn-pool white-p-network white-threads))
          ;; Otherwise were provided a network so use it
          (setq w-p-pool (init-pool white-p-network white-threads))))

      ;; Initialize value nets
      (when black-v-network
        (format t "Loading Black Value Networks ~%")
        ;; When provided a string
        (if (stringp black-v-network)
          ;; Load the network
          (setq b-v-pool (init-nn-pool black-v-network black-threads))
          ;; Otherwise were provided a network so use it
          (setq b-v-pool (init-pool black-v-network black-threads))))

      (when white-v-network
        (format t "Loading White Value Networks ~%")
        ;; When provided a string
        (if (stringp white-v-network)
          ;; Load the network
          (setq w-v-pool (init-nn-pool white-v-network white-threads))
          ;; Otherwise were provided a network so use it
          (setq w-v-pool (init-pool white-v-network white-threads))))

      (loop while (not (game-over? g))
            do (cond
                 ((eq (gg-whose-turn? g) *black*)
                  (when print-game? (format t "BLACK'S TURN!~%"))
                  (if print-game? (time (do-move! g (uct-search g black-num-sims black-c nil black-threads b-p-pool b-v-pool time-limit)))
                    (do-move! g (uct-search g black-num-sims black-c nil black-threads b-p-pool b-v-pool time-limit)))
                  (when print-game? (print-go g t nil nil nil nil)))
                 (t
                   (when print-game? (format t "WHITE'S TURN!~%"))
                   (if print-game? (time (do-move! g (uct-search g white-num-sims white-c nil white-threads w-p-pool b-v-pool time-limit)))
                     (do-move! g (uct-search g white-num-sims white-c nil white-threads w-p-pool w-v-pool time-limit)))
                   (when print-game? (print-go g t nil nil nil nil)))))

      ;; Show most game information at the end
      (when print-game? (print-go g t nil t t))

      ;; If a record from the game is to be used
      (when file
        ;; Record 4 states b/c don'thave much time
        (dotimes (i 4) 
          (record-game g)))
      ;; Return the final game state if requested
      (when return-game? g))))

(defmacro p-run-sims (num)
  `(mp:process-run-function (concatenate 'string "number-" (write-to-string ,num)) #'run-sims ,num))


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
;; The parallel implementation of this uses too much memory
  `(evolve-networks (read-nets ,nets) ,gens ,fronts ,file-lock nil))


(defmacro p-run-evo (nets gens fronts file-lock)
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
     (run-evo nets (random 10) 2 ,lock)))
