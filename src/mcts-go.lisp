;; ========================================
;;  CMPU-365, Spring 2017
;;  FILE:  mcts-go.lisp
;;  NAME: David Wallach
;; ========================================


;;  MC-NODE struct : KEY WHOSE-TURN NUM-VISITS VECK-MOVES VECK-VISITS VECK-SCORES 
;; -------------------
(defstruct (mc-node (:print-function print-mc-node)
                    (:include synchronizing-structure))
  key             ;; hash-table key:  compact rep'n of current state of game
  whose-turn      ;;  *BLACK* or *WHITE*
  (num-visits 0)  ;; number of times this state has been visited
  veck-moves      ;; a VECTOR of moves
  veck-visits     ;; a VECTOR recording number of times each move visited
  veck-scores     ;; a VECTOR recording average score for each move
  )

(defun print-mc-node (node str depth)
  (declare (ignore depth))
  (format str "Key: ~A," (mc-node-key node))
  (format str "Whose Turn: ~A," (mc-node-whose-turn node))
  (format str "Num Visits: ~A~%" (mc-node-num-visits node))
  (format str "Vec Moves: ~A~%" (mc-node-veck-moves node))
  (format str "Vec Visits: ~A~%" (mc-node-veck-visits node))
  (format str "Vec Scores: ~A~%" (mc-node-veck-scores node)))

;;  MC-TREE : HASHY ROOT-KEY
;; --------------------------
(defstruct (mc-tree (:print-function print-mc-tree)
                    (:include synchronizing-structure))
  ;; hash-table:  key = compact repn of state, value = mc-node
  hashy 
  root-key
  )

(defun deep-copy-mc-tree (tree)
  (let ((newTree (make-mc-tree
                   :root-key (mc-tree-root-key tree)))
        )
    (labels
      ((add-to-tree
         (key value)
         (setf (gethash key (mc-tree-hashy newTree)) value)))
      (maphash #'add-to-tree (mc-tree-hashy tree)))
    newTree))


(defun hash-print (key value)
  (format t "~A~%~A~%" key value))


(defun print-mc-tree (tree str depth)
  (declare (ignore depth))
  (format str "MC-TREE~%")
  (maphash #'hash-print (mc-tree-hashy tree)))

;;  GET-ROOT-NODE : GAME
;; -----------------------------
(defmacro get-root-node
  (tree)
  `(gethash (mc-tree-root-key ,tree) (mc-tree-hashy ,tree)))

;;  NEW-MC-TREE : GAME
;; ------------------------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived from GAME.
(defun new-mc-tree (game &optional (num-sims 10000))
  (let* ((root-key (make-hash-key-from-game game))
         (new-tree (make-mc-tree :root-key root-key
                                 :hashy (make-hash-table
                                          :test #'equal
    ;                                      :weak-keys t ;; Lets the garbage collector 
                                                       ;; collect entries when their keys
                                                       ;; have no more references
                                          :size num-sims))))
    (insert-new-node game new-tree root-key)
    new-tree))


;;  MERGE-MC-TREES! : TREE-ONE TREE-TWO - DEFUNCT
;; -------------------------------------
;;  INPUT: TREE-ONE,TREE-TWO, two montecarlo tree structs with the same root node
;;  OUTPUT: NEW-TREE, TREE-ONE destructively modified to include the information 
;;                    from TREE-TWO
(defun merge-mc-trees! (tree-one tree-two)
  (let ((table-one (mc-tree-hashy tree-one))
        (table-two (mc-tree-hashy tree-two))
        )
    ;; Define a function for use by maphash
    (labels ((merge-into-tree-one 
               (key value)
               (unless (gethash key table-one)

                 ;; Add the key, value pair from table-two
                 (setf (gethash key table-one) value))))

      ;; Apply the label
      (maphash #'merge-into-tree-one table-two)

      ;; Assign tree-one the updated hashtable and return it
      (with-locked-structure (tree-one)
                             (setf (mc-tree-hashy tree-one) table-one))
      )))

;;  MERGE-MC-TREES! : TREE-ONE TREE-TWO - DEFUNCT
;; -------------------------------------
;;  INPUT: TREE-ONE,TREE-TWO, two montecarlo tree structs with the same root node
;;  OUTPUT: NEW-TREE, TREE-ONE destructively modified to exclude the information 
;;                    from TREE-TWO
(defun prune-mc-trees! (tree-one tree-two)
  (let ((table-one (mc-tree-hashy tree-one))
        (table-two (mc-tree-hashy tree-two))
        )
    ;; Define a function for use by maphash
    (labels ((prune-into-tree-one 
               (key value)
               (declare (ignore value))
               (when (gethash key table-one)
                 ;; Remove the key, value pair from table-one
                 (remhash key table-one))))

      ;; Apply the label
      (maphash #'prune-into-tree-one table-two)
      (with-locked-structure (tree-one)
                             ;; Assign tree-one the updated hashtable and return it
                             (setf (mc-tree-hashy tree-one) table-one)))))

;;  INSERT-NEW-NODE : GAME TREE KEY
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.
(defun insert-new-node (game tree key)
  (let ((moves (legal-moves game))
        (new-node nil))
    (setq new-node (make-mc-node 
                     :key key
                     :whose-turn (gg-whose-turn? game)
                     :num-visits 1
                     :veck-moves moves
                     :veck-visits (make-array (length moves) :initial-element 0)
                     :veck-scores (make-array (length moves) :initial-element 0)
                     ))
    ;; Lock the tree
    (with-locked-structure 
      (tree)
      ;; Insert the node
      (setf (gethash key (mc-tree-hashy tree)) new-node))
    new-node))

;;  INSERT-NEW-NODE-NN
;; --------------------------------------
(defun insert-new-node-nn (game tree key nn)
  (let ((moves (legal-moves game))
        (node-score (annalyze-board nn (gg-board game) (gg-whose-turn? game)))
        (new-node nil))
    (setq new-node (make-mc-node 
                     :key key
                     :whose-turn (gg-whose-turn? game)
                     :num-visits 1
                     :veck-moves moves
                     :veck-visits (make-array (length moves) :initial-element 0)
                     :veck-scores (make-array (length moves) :initial-element 0)))
    ;; If the node has been created by another thread
    (if (gethash key (mc-tree-hashy tree))
      ;; Lock the node
      (with-locked-structure
        ((gethash key (mc-tree-hashy tree)))
        ;; Update N(S_t)
        (incf (mc-node-num-visits (gethash key (mc-tree-hashy tree)))))
      ;; Otherwise just insert the node
      (setf (gethash key (mc-tree-hashy tree)) new-node))
    ;; Use the network allocated for the current process
   (list new-node node-score))) 

;;  SELECT-MOVE : NODEY C
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector

;; Methods should be whatever functions in the go-game
;; file that are called by the montecarlo tree search
(defmacro select-move (nodey c &optional (game-move nil))
  `(let ((scores (mc-node-veck-scores ,nodey))
        (visits (mc-node-veck-visits ,nodey))
        (node-visits (mc-node-num-visits ,nodey))
        (player (mc-node-whose-turn ,nodey))
        (max-so-far 0)
        (best-move-so-far 0)
        (new_q 0))

    ;; Compare all the potential moves
    (dotimes (i (length scores))
          (cond
            ;; Immediately select an unexplored move
            ((and (> ,c 0)
                  (= 0 (svref visits i)))
             (setq best-move-so-far i)
             (return))
            ;; Otherwise
            (t 
              ;; Calculate the monte-carlo value
              (setq new_q (* ,c  (sqrt (/ (log node-visits)
                                          (/ (svref visits i)
                                             node-visits)))))))
          ;; Set the value, adding or subtracting depending on the player
          (cond
            ;; If it's black the best score is the highest
            ((eq player *black*)
             (with-locked-structure 
               (,nodey) 
               (incf (svref scores i) new_q)
               )

             (when (< max-so-far (svref scores i))
               (setq max-so-far (svref scores i))
               (setq best-move-so-far i)))
            ; If it's white the best score is the lowest
            (t (with-locked-structure 
                 (,nodey)
                 (decf (svref scores i) new_q))
               (when (and (> max-so-far (svref scores i))
                          ;; Don't pass until the late game
                          ;; pass is always the last legal move
                          (or (not (= (- (length scores) 1) i))
                              (> 10 (length scores))))
                 (setq max-so-far (svref scores i))
                 (setq best-move-so-far i)))))

    (with-locked-structure 
      (,nodey)
      ;; Update the visits to the chosen move
      (incf (svref (mc-node-veck-visits ,nodey) best-move-so-far))

      ;; Increment the number of visits to this node
      (incf (mc-node-num-visits ,nodey)))

   (when ,game-move (format t "Move score: ~A, Move: ~A ~%" max-so-far best-move-so-far))

    ;; Return the best move found
    best-move-so-far))


;;  SIM-TREE : GAME TREE C
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.
(defun sim-tree 
  (game tree c)
  ;; When you hit a node not in the hastable then you are done with
  ;; sim tree. Add the one move and then start using the random play out
  ;; value only keeping track of the store at the end.
  (let ((state-move-list nil)
        (moves (legal-moves game))
        (s_t nil)
        (hash nil)
        (m_t 0))
    (dotimes (i 1000000)
      ;; Get the state (hash key)
      (setq s_t (make-hash-key-from-game game))
      (setq state-move-list
            (append state-move-list (list s_t)))
      ;; If the state doesn't already exist
      (unless (gethash s_t (mc-tree-hashy tree) nil)
        (let ((cur-score 0)
              (best-score 0)
              (best-move 0))
          ;; Add the new state
          (insert-new-node game tree s_t)
          (dotimes (move (length moves))
            ;; Update the best-score and best-move
            (when (< best-score cur-score)
              (setq best-move move)
              (setq best-score cur-score)))
          ;; Return the state-move list
          (return-from sim-tree (append state-move-list
                                        (list best-move)))))

      (setq hash (gethash (make-hash-key-from-game game)
                                      (mc-tree-hashy tree)) )

      ;; Otherwise the state does already exist so use select-move
      (setq m_t (select-move hash c))

      ;; Do the move
      (do-move! game (svref moves m_t))

      ;; Add it to the move list
      (setq state-move-list
            (append state-move-list (list m_t)))

      ;; When the game is over break out of the loop
      (when (game-over? game) 
        (return-from sim-tree state-move-list)))

    ;; Once the game is over return the state-move list
    state-move-list))

;;  SIM-TREE : GAME TREE C NN
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;           NN, A network struct
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.
(defun sim-tree-nn
  (game tree c nn)
  ;; When you hit a node not in the hastable then you are done with
  ;; sim tree. Add the one move and then start using the random play out
  ;; value only keeping track of the store at the end.
  (let ((state-move-list nil)
        (moves (legal-moves game))
        (s_t nil)
        (m_t 0))
    (dotimes (i 1000000)
      ;; Get the state (hash key)
      (setq s_t (make-hash-key-from-game game))
      (setq state-move-list
            (append state-move-list (list s_t)))
      ;; If the state doesn't already exist
      (unless (gethash s_t (mc-tree-hashy tree) nil)
        (let ((cur-score 0)
              (best-score 0)
              (best-move 0)
              (node-val (annalyze-board nn (gg-board game) (gg-whose-turn? game))))
          ;; Add the new state
          (insert-new-node game tree s_t)
          (dotimes (move (length moves))
            ;; Update the best-score and best-move
            (when (< best-score cur-score)
              (setq best-move move)
              (setq best-score cur-score)))
          ;; Return the state-move list
          (return-from sim-tree-nn (append node-val state-move-list
                                        (list best-move)))))

      ;; Otherwise the state does already exist so use select-move
      (setq m_t (select-move (gethash (make-hash-key-from-game game)
                                      (mc-tree-hashy tree)) c))
      ;; Do the move
      (do-move! game (svref moves m_t))

      ;; Add it to the move list
      (setq state-move-list
            (append state-move-list (list m_t)))

      ;; When the game is over break out of the loop
      (when (game-over? game) 
        ;; If no leaf nodes the value-network isn't used
        (return-from sim-tree-nn (push 0 state-move-list))))

    ;; Once the game is over return the state-move list
    state-move-list))

;; Like sim-tree but doesn't insert a new node in the hash table
(defun sim-no-tree
  (game tree c &optional (p-nn nil) (v-nn nil))
  (let ((state-move-list nil)
        (moves (legal-moves game))
        (s_t nil)
        (m_t 0))
    (dotimes (i 1000000)
      ;; Get the state (hash key)
      (setq s_t (make-hash-key-from-game game))
      ;; If the state doesn't already exist
      (cond 
        ((gethash s_t (mc-tree-hashy tree) nil)

         ;; Append the state
         (setq state-move-list
               (append state-move-list (list s_t)))

         ;; The state does already exist so use select-move
         (setq m_t (select-move (gethash (make-hash-key-from-game game)
                                         (mc-tree-hashy tree)) c))

         ;; Do the move
         (do-move! game (svref moves m_t))

         ;; Add it to the move list
         (setq state-move-list
               (append state-move-list (list m_t))))
        (t 
          ;; Otherwise use the nn if provided
          (if p-nn
            (setq m_t (annalyze-move p-nn (gg-board game) moves (gg-whose-turn? game)))
            ;; Random if no nn
            (setq m_t (random (length moves))))
          ;; Do the move
          (do-move! game (svref moves m_t))))

      ;; When the game is over break out of the loop
      (when (game-over? game) 
        ;; If no leaf nodes the value-network isn't used
        (if v-nn
          (return-from sim-no-tree (push 0 state-move-list))
          (return-from sim-no-tree state-move-list))))

    ;; Once the game is over return the state-move list
    state-move-list))


;;  BACKUP : HASHY KEY-MOVE-ACC RESULT
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY
(defun backup (tree key-move-acc result)
      (while key-move-acc
             (let* ((key (pop key-move-acc))
                    (hashy (mc-tree-hashy tree))
                    (nodey (gethash key hashy))
                    (mv-index (pop key-move-acc))
                    (visitz (mc-node-veck-visits nodey))
                    (scorez (mc-node-veck-scores nodey)))
               ;; incremenet node num visits
               (incf (mc-node-num-visits nodey))
               ;; increment num times did this move from this state
               (incf (svref visitz mv-index))
               ;; increment the SCORE
               (incf (svref scorez mv-index)
                     (/ (- result (svref scorez mv-index))
                        (svref visitz mv-index)))))
      ;; Tree post
      tree)

;; For use by each thread of uct-search
(defun sim-ops
  (orig-game c orig-tree total-sim p-pool v-pool barrier start-time time-limit)
  (let* ((state-move-list nil)
         (z 0)
         (add-node 1)
         (p-nn nil)
         (v-nn nil)
         (game nil))

    ;; When provided a policy pool
    (when (and p-pool 
               (< 0 (length (pool-nets p-pool))))
      ;; Get a network 
      (with-locked-structure 
        (p-pool)
        (setq p-nn (pop (pool-nets p-pool))))
      (format t "Got Policy Net ~A~%" (net-to-string p-nn))
      )

    ;; When provided a value pool
    (when (and v-pool 
               (< 0 (length (pool-nets v-pool))))
      ;; Get a network 
      (with-locked-structure 
        (v-pool)
        (setq v-nn (pop (pool-nets v-pool))))
      (format t "Got Value Net ~A~%" (net-to-string v-nn))
      )

    ;; Keep the logic dealing with different settings out
    ;; of the main loop
    (cond 
      ;; Search with both networks
      ((and p-nn v-nn)
       (dotimes (i total-sim)
         ;; Make a copy of the game state
         (setq game (deep-copy-go orig-game))
         ;; Run sim-tree, 
         ;; State move list now has a value for the node inserted as 
         ;; its first element
         ;; Add a node node every add-node sims
         (if (= 0 (mod i add-node))
           (setq state-move-list (sim-tree-nn game orig-tree c v-nn))
           (setq state-move-list (sim-no-tree game orig-tree c p-nn v-nn)))
         ;; Run default
         (setq z (+ (default-policy game p-nn) (pop state-move-list)))
         ;; Run backup if any states were explored
         (when state-move-list
           (backup orig-tree state-move-list z))
         ;; When it's over the time limit return
         (when (< time-limit (- (get-internal-real-time) start-time))
           (return))))
      ;; Search only with policy network
      (p-nn 
        (dotimes (i total-sim)
          ;; Make a copy of the game state
          (setq game (deep-copy-go orig-game))
          ;; Run sim-tree
          (setq state-move-list (sim-tree game orig-tree c))
          ;; Add a node node every add-node sims
          ;(if (= 0 (mod i add-node))
          ;  (setq state-move-list (sim-tree game orig-tree c))
          ;  (setq state-move-list (sim-no-tree game orig-tree c p-nn)))
          ;; Run default
          (setq z (default-policy game p-nn))
          ;; Run backup if any states were explored
          (when state-move-list
            (backup orig-tree state-move-list z))
          ;; When it's over the time limit return
          (when (< time-limit (- (get-internal-real-time) start-time))
            (return))))
      ;; Search only with value network
      (v-nn 
        (dotimes (i total-sim)
          ;; Make a copy of the game state
          (setq game (deep-copy-go orig-game))
          ;; Add a node node every add-node sims
          (if (= 0 (mod i add-node))
            (setq state-move-list (sim-tree-nn game orig-tree c v-nn))
            (setq state-move-list (sim-no-tree game orig-tree c)))
          ;; Run default
          (setq z (+ (random-policy game) (pop state-move-list)))
          ;; Run backup if any states were explored
          (when state-move-list
            (backup orig-tree state-move-list z))
          ;; When it's over the time limit return
          (when (< time-limit (- (get-internal-real-time) start-time))
            (return))))
      ;; Search with no networks
      (t 
        (dotimes (i total-sim)
          ;; Make a copy of the game state
          (setq game (deep-copy-go orig-game))

          (setq state-move-list (sim-tree game orig-tree c))
          ;; Add a node node every add-node sims
          ;(if (= 0 (mod i add-node))
          ;  (setq state-move-list (sim-tree game orig-tree c))
          ;  (setq state-move-list (sim-no-tree game orig-tree c)))
          ;; Run default
          (setq z (random-policy game))
          ;; Run backup if any states were explored
          (when state-move-list
            (backup orig-tree state-move-list z))
          ;; When it's over the time limit return
          (when (< time-limit (- (get-internal-real-time) start-time))
            (return)))))

    ;; When you got a policynetwork
    (when p-nn 
      ;; Return the network
      (with-locked-structure 
        (p-pool)
        (push p-nn (pool-nets p-pool))))

    ;; When you got a value network
    (when v-nn 
      ;; Return the network
      (with-locked-structure 
        (v-pool)
        (push v-nn (pool-nets v-pool))))

    ;; Pass through the barrier to signal the thread is done 
    (when barrier (mp:barrier-pass-through barrier))
    ))


;;  UCT-SEARCH : ORIG-GAME NUM-SIMS C
;; -------------------------------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  The best move according to monte-carlo tree search.
(defun uct-search (orig-game num-sims c &optional 
                             (return-tree? nil) 
                             (use-threads nil)
                             (p-pool nil)
                             (v-pool nil)
                             (time-limit 10000))
  (let ((start-time (get-internal-real-time))
        (tree (new-mc-tree orig-game num-sims)))
    (cond
      ;; Use threaded implementation
      (use-threads
        (let ((barrier (mp:make-barrier (+ use-threads 1)))
              (sims-per-thread (ceiling (/ num-sims use-threads))))
          ;; Spawn the threads
          (dotimes (i use-threads)
            ;; Create a process and start it running with the tree
            ;; The process is killed and cleaned up after when the 
            ;; function returns.
            (mp:process-run-function (write-to-string i) ; Process identifier
                                     #'sim-ops           ; Function Call
                                     orig-game           ; The game
                                     c tree
                                     sims-per-thread
                                     p-pool v-pool
                                     barrier
                                     start-time
                                     time-limit))
          ;; Wait until all the threads are finished
          (mp:barrier-wait barrier)

          (when return-tree? (format t "Number of nodes ~A~%" 
                                     (hash-table-count (mc-tree-hashy tree))))
          ;; Select the best move
          (svref (legal-moves orig-game) 
                 (select-move (gethash (make-hash-key-from-game orig-game)
                                       (mc-tree-hashy tree)) c t))))

      ;; Otherwise perform the operations sequentially
      (t (sim-ops orig-game c tree num-sims p-pool v-pool nil start-time time-limit)

         ;; Select the best move
         (svref (legal-moves orig-game) 
                (select-move (gethash (make-hash-key-from-game orig-game)
                                      (mc-tree-hashy tree)) c t))))))
