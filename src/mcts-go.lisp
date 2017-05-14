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
  (declare (ignore depth) (ignore str))
  (format str "MC-TREE~%")
  (maphash #'hash-print (mc-tree-hashy tree)))

;;  GET-ROOT-NODE : GAME
;; -----------------------------
(defun get-root-node
  (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))


(defun equal-key (key1 key2)
  (if 
    (and (= (first key1) (first key2))
         (equal (second key1) (second key2)))
    t
    nil))


;;  NEW-MC-TREE : GAME
;; ------------------------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived from GAME.
(defun new-mc-tree (game &optional (num-sims 10000))
  (let* ((root-key (make-hash-key-from-game game))
         (new-tree (make-mc-tree :root-key root-key
                                 :hashy (make-hash-table
                                          :test #'equal
                                          :size num-sims)))
         )
    (insert-new-node game new-tree root-key)
    new-tree))


;;  MERGE-MC-TREES! : TREE-ONE TREE-TWO
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

;;  MERGE-MC-TREES! : TREE-ONE TREE-TWO
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
(defun insert-new-node (game tree key &optional (pid nil))
  (declare (ignore pid))
  ;; (format t "Insert new node~%")
  (let ((moves (legal-moves game))
        (node-holder nil)
        (new-node nil))
    (setq new-node (make-mc-node 
                     :key key
                     :whose-turn (gg-whose-turn? game)
                     :veck-moves moves
                     :veck-visits (make-array (length moves) :initial-element 0)
                     :veck-scores (make-array (length moves) :initial-element 0)
                     ))

    ;; Use the network allocated for the current process


    ;; If the node has been created by another thread
    (if (gethash key (mc-tree-hashy tree))
      ;; Lock the node
      (with-locked-structure
        ((gethash key (mc-tree-hashy tree)))

        (setq node-holder (gethash key (mc-tree-hashy tree)))
        ;; Update N(S_t)
        (setf (mc-node-num-visits node-holder)
              (+ (mc-node-num-visits node-holder)
                 (mc-node-num-visits new-node)))

          ;; Update the tree
          (setf (gethash key (mc-tree-hashy tree))
                node-holder))

      ;; Otherwise just insert the node
      (setf (gethash key (mc-tree-hashy tree)) new-node))

    new-node))

;;  SELECT-MOVE : NODEY C
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector

;; Methods should be whatever functions in the go-game
;; file that are called by the montecarlo tree search
(defun select-move (nodey c &optional (game-move nil))
  ;; (format t "Selecet Move~%")
  (let ((scores (mc-node-veck-scores nodey))
        (visits (mc-node-veck-visits nodey))
        (node-visits (mc-node-num-visits nodey))
        (player (mc-node-whose-turn nodey))
        (max-so-far 0)
        (best-move-so-far 0)
        (new_q 0)
        (ifblack 1)
        )

  ;  (when game-move (format t "Node: ~A~%" nodey))
    ;; Compare all the potential moves
    (dotimes (i (length scores))
      (cond
        ;; Immediately select an unexplored move
        ((and (> c 0)
              (= 0 (svref visits i)))
         (setq best-move-so-far i)
         (return))
        ;; Otherwise
        (t 
          ;; Calculate the monte-carlo value
          (setq new_q (* c  (sqrt (/ (log node-visits)
                                     (/ (svref visits i)
                                        node-visits)))))))
      ;; Set the value, adding or subtracting depending on the player
      (cond
        ;; If it's black the best score is the highest
        ((= player *black*)
         (setf (svref scores i)
               (+ (svref scores i)
                  new_q))
         (when (< max-so-far (svref scores i))
           (setq max-so-far (svref scores i))
           (setq best-move-so-far i)))

        (t ; If it's white the best score is the lowest
          (setf (svref scores i)
                (- (svref scores i)
                   new_q))
          (when (> max-so-far (svref scores i))
            (setq max-so-far (svref scores i))
            (setq best-move-so-far i))))
      )

  ;  (when game-move (format t "Post scores: ~A~%" scores) )
    (with-locked-structure 
      (nodey)
      ;; Update the scores in the node
      (setf (mc-node-veck-scores nodey) scores)

      ;; Update the visits to the chosen move
      (incf (svref (mc-node-veck-visits nodey) best-move-so-far))

      ;; Increment the number of visits to this node
      (incf (mc-node-num-visits nodey)))

   ; (when game-move (format t "Move score: ~A~%" max-so-far))

   ; (when game-move (format t "Post post scores: ~A~%" scores) )
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
  (game tree c &optional (imp t) (pid 0))

 ; (format t "Sim Tree~%")
  ;; When you hit a node not in the hastable then you are done with
  ;; sim tree. Add the one move and then start using the random play out
  ;; value only keeping track of the store at the end.
  (cond 
    (imp
      (let (;; KEY-MOVE-ACC:  accumulator of KEYs and MOVEs
            (key-move-acc nil)
            (hashy (mc-tree-hashy tree)))
        (while (not (game-over? game))
               (let* (;; KEY:  Hash key for current state of game
                      (key (make-hash-key-from-game game))
                      ;; NODEY:  The MC-NODE corresponding to KEY (or NIL if not in tree)
                      (nodey (gethash key hashy)))
                 ;; Case 1:  When key not yet in tree...
                 (when (null nodey)
                   ;; Create new node and insert it into tree
                   (setf nodey (insert-new-node game tree key pid))
                  ; (format t "NODEY: ~A~%" nodey)
                   (let* ((mv-index (select-move nodey c))
                          (move-veck (mc-node-veck-moves nodey))
                          (move (svref move-veck mv-index)))
                     ;(format t "Move: <% <%  %> %>")
                     (do-move! game move)
                     (push key key-move-acc)
                     (push mv-index key-move-acc)
                     ;; return the accumulator prepended with selected MOVE
                     ;; and KEY for current state
                     (return-from sim-tree (reverse key-move-acc))))

                 ;; Case 2:  Key already in tree!
                 (let* ((mv-index (select-move nodey c))
                        (move-veck (mc-node-veck-moves nodey))
                        (move (svref move-veck mv-index)))
                   (do-move! game move)
                   (push key key-move-acc)
                   (push mv-index key-move-acc))))

        ;; After the WHILE... return the accumulated key/move list
        (reverse key-move-acc)))

    (t 
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
                  )
              ;; Add the new state
              (insert-new-node game tree s_t pid)
              (dotimes (move (length moves))
                ;; Update the best-score and best-move
                (when (< best-score cur-score)
                  (setq best-move move)
                  (setq best-score cur-score)))
              ;; Return the state-move list
              (return-from sim-tree (append state-move-list
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
            (return-from sim-tree state-move-list)))

        ;; Once the game is over return the state-move list
        state-move-list))
    )
  
 ; (format t "SIm Root: ~A~%" (get-root-node tree))
  )
;;  SIM-DEFAULT : GAME
;; ----------------------------------------------
;;  INPUT:  GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
  (game &optional (nn nil))
  (if nn
    (default-policy game nn)
    (random-policy game)))


;;  BACKUP : HASHY KEY-MOVE-ACC RESULT
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY
(defun backup (tree key-move-acc result &optional (imp t))
 ; (format t "Length list ~A, size tree ~A~%~%" (length key-move-acc) (hash-table-count (mc-tree-hashy tree)))
  (cond 
    (imp
      (while key-move-acc
             (let* ((key (pop key-move-acc))
                    (hashy (mc-tree-hashy tree))
                    (nodey (gethash key hashy))
                    (mv-index (pop key-move-acc))
                    (visitz (mc-node-veck-visits nodey))
                    (scorez (mc-node-veck-scores nodey)))
    ;           (format t "Key ~A~%" key)
    ;           (format t "++++++++Tree pre backup~%")
    ;           (print-mc-tree tree t nil)
               ;; incremenet node num visits
               (incf (mc-node-num-visits nodey))
               ;; increment num times did this move from this state
               (incf (svref visitz mv-index))
               ;; increment the SCORE
               (incf (svref scorez mv-index)
                     (/ (- result (svref scorez mv-index))
                        (svref visitz mv-index)))
               ;; Tree post
 ;              (format t "=========Tree post backup~%")
 ;              (print-mc-tree tree t nil)
 ;              (break)
               )))
    (t (let ((node-holder nil)
             (key nil)
             (move nil)
             (hashy (mc-tree-hashy tree))
             )
         ;; Iterate through the list, two elements at a time
         (dotimes (i (/ (length key-move-acc) 2))
           ;; Get the first element (key)
           (setq key (pop key-move-acc))
           ;; Get the second element (move)
           (setq move (pop key-move-acc))
           ;; Get the matching node
           (setq node-holder (gethash key hashy))

           ;; Update N(S_t)
           (incf (mc-node-num-visits node-holder))
           ;; Update N(S_t, A_t)
           (incf (svref (mc-node-veck-visits node-holder) move))

           ;; Update Q(S_t, A_t)
           (setf(svref (mc-node-veck-scores node-holder) move)
             (+ (svref (mc-node-veck-scores node-holder) move)
                (/ (- result (svref (mc-node-veck-scores node-holder) move))
                   (svref (mc-node-veck-visits node-holder) move))))

           ;; Update the tree
           (with-locked-structure ((gethash key (mc-tree-hashy tree)))
                                  (setf (gethash key (mc-tree-hashy tree))
                                        node-holder)))
         ))
    )
  tree)

;; For use by each thread of uct-search
(defun sim-ops
  (orig-game c orig-tree total-sim pool id barrier start-time time-limit)
  (let* ((state-move-list nil)
         (z 0)
         (nn nil)
         (game nil))

    ;; When provided a pool
    (when pool
      ;; Get a network 
      (with-locked-structure 
        (pool)
        (setq nn (pop (pool-nets pool)))))

    (dotimes (i total-sim)
      ;; Make a copy of the game state
      (setq game (deep-copy-go orig-game))
      ;; Run sim-tree
      (setq state-move-list (sim-tree game orig-tree c id))
      ;; Run default
      (setq z (sim-default game nn))
      ;; Run backup
      (backup orig-tree state-move-list z)

      ;; When it's over the time limit return
      (when (< time-limit (- (get-internal-real-time) start-time))
        (return))
      )

    ;; When you got a network
    (when nn 
      ;; Return the network
      (with-locked-structure 
        (pool)
        (push nn (pool-nets pool)))
      )

    ;; Pass through the barrier to signal the thread is done 
    (mp:barrier-pass-through barrier)
    ))


;;  UCT-SEARCH : ORIG-GAME NUM-SIMS C
;; -------------------------------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  The best move according to monte-carlo tree search.
(defun uct-search (orig-game num-sims c &optional 
                             (return-tree nil) 
                             (use-threads nil)
                             (pool nil)
                             (time-limit 10000))
  (let ((start-time (get-internal-real-time))
        )
    (cond
      ;; Use threaded implementation
      (use-threads
        (let* ((tree (new-mc-tree orig-game))
               (barrier (mp:make-barrier (+ *num-cores* 1)))
               (sims-per-thread (ceiling (/ num-sims *num-cores*)))
               (nn nil)
               )
          ;; Spawn the threads
          (dotimes (i *num-cores*)
            ;; Create a process and start it running with the tree
            ;; The process is killed and cleaned up after when the 
            ;; function returns.
            (mp:process-run-function (write-to-string i) ; Process identifier
                                     #'sim-ops           ; Function Call
                                     orig-game           ; The game
                                     c tree
                                     sims-per-thread
                                     pool
                                     i barrier
                                     start-time
                                     time-limit))
          ;; Wait until all the threads are finished
          (mp:barrier-wait barrier)

          (format t "Number of nodes ~A, num of moves ~A~%" 
                  (hash-table-count (mc-tree-hashy tree))
                  (length (legal-moves orig-game)))
          ;; Select the best move
          (svref (legal-moves orig-game) 
                 (select-move (gethash (make-hash-key-from-game orig-game)
                                       (mc-tree-hashy tree)) c t))))

      ;; Otherwise perform the operations sequentially
      (t
        (let ((state-move-list nil)
              (tree (new-mc-tree orig-game))
              (nn nil)
              (z 0)
              (cur-time 0)
              (game nil))
          (when pool
            (setq nn (pop (pool-nets pool)))
            )
          (dotimes (i num-sims)
            ;; Make a copy of the game state
            (setq game (deep-copy-go orig-game))
            ;; Run sim-tree
            (setq state-move-list (sim-tree game tree c))
            ;; Run default
            (setq z (sim-default game nn))
            ;; Run backup
            (backup tree state-move-list z)
            (when (= 0 (mod i 100))
              (setq cur-time (get-internal-real-time))
              (when (< time-limit (- cur-time start-time))
                (return))))
          (when nn
            (push nn (pool-nets pool))
            )

          ;; Select the best move
          (svref (legal-moves orig-game) 
                 (select-move (gethash (make-hash-key-from-game orig-game)
                                       (mc-tree-hashy tree)) c t)))))))

