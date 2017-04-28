;; ========================================
;;  CMPU-365, Spring 2017
;;  FILE:  mcts-go.lisp
;;  NAME: David Wallach
;; ========================================


;;  MC-NODE struct : KEY WHOSE-TURN NUM-VISITS VECK-MOVES VECK-VISITS VECK-SCORES 
;; -------------------
(defstruct (mc-node (:print-function print-mc-node))
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
  (hashy (cl:make-hash-table :test #'equal :weak-keys t))
  root-key
  )

(defun deep-copy-mc-tree (tree)
  (let ((newTree (make-mc-tree
                   :root-key (mc-tree-root-key tree)))
        )
    (labels
      ((add-to-tree
         (key value)
         (setf (gethash key (mc-tree-hashy newTree)) value))
       )
      (maphash #'add-to-tree (mc-tree-hashy tree)))
    newTree))


(defun hash-print (key value)
  (format t "Key: ~A, Value: ~A~%" key value))


(defun print-mc-tree (tree str depth)
  (declare (ignore depth) (ignore str))
  (maphash #'hash-print (mc-tree-hashy tree)))

;;  GET-ROOT-NODE : GAME
;; -----------------------------
(defun get-root-node
    (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))

;;  NEW-MC-TREE : GAME
;; ------------------------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived from GAME.
(defun new-mc-tree (game)
  (let* ((root-key (make-hash-key-from-game game))
         (new-tree (make-mc-tree :root-key root-key)))
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
        (node-holder nil)
        )
    ;; Define a function for use by maphash
    (labels ((merge-into-tree-one 
               (key value)
               ;; If the key already exists in table
               (cond

                 ;; store it in node holder
                 ((setq node-holder (gethash key table-one))

                  ;; Update the number of visits information
                  (setf (mc-node-num-visits node-holder)
                        (+ (mc-node-num-visits node-holder)
                           (mc-node-num-visits value)))

                  ;; Update the visits to each move
                  (dotimes (i (length (mc-node-veck-visits node-holder)))
                    (setf (svref (mc-node-veck-visits node-holder) i)
                          (+ (svref (mc-node-veck-visits node-holder) i)
                             (svref (mc-node-veck-visits value) i))))

                  ;; Update the scores of each move
                  (dotimes (i (length (mc-node-veck-scores node-holder)))
                    (setf (svref (mc-node-veck-scores node-holder) i)
                          (+ (svref (mc-node-veck-scores node-holder) i)
                             (svref (mc-node-veck-scores value) i))))
                  )
                  ;; Otherwise if the key doesn't exist
                  (t
                    ;; Add the key, value pair from table-two
                    (setf (gethash key table-one) value)
                    )))
             )

      ;; Apply the label
      (maphash #'merge-into-tree-one table-two)
      ;; Assign tree-one the updated hashtable and return it
      (setf (mc-tree-hashy tree-one) table-one))))

;;  INSERT-NEW-NODE : GAME TREE KEY
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.
(defun insert-new-node (game tree key)
 ;; (format t "Insert new node~%")
  (let* ((moves (legal-moves game))
        (new-node (make-mc-node :key key
                                :whose-turn (gg-whose-turn? game)
                                :veck-moves moves
                                :veck-visits (make-array (length moves) :initial-element 0)
                                :veck-scores (make-array (length moves) :initial-element 0))))
    (setf (gethash key (mc-tree-hashy tree)) new-node)
    new-node))

;;  SELECT-MOVE : NODEY C
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector

;; Methods should be whatever functions in the go-game
;; file that are called by the montecarlo tree search
(defun select-move (nodey c &optional (use-threads nil))
  ;; (format t "Selecet Move~%")
  (let ((scores (mc-node-veck-scores nodey))
        (visits (mc-node-veck-visits nodey))
        (node-visits (mc-node-num-visits nodey))
        (player (mc-node-whose-turn nodey))
        (max-so-far 0)
        (best-move-so-far 0)
        (new_q 0)
        )

    (cond 
      ;; Use threaded implementation of move selection
      (use-threads
        
        ;; Compare all the potential moves
        (dotimes (i (length scores))
          ;; If there isn't gonan be problems dividing by 0
          (if (and (< 0 node-visits)
                   (< 0 (svref visits i)))
            ;; Calculate the monte-carlo value
            (setq new_q (* c  (sqrt (/ (log node-visits)
                                       (/ (svref visits i)
                                          node-visits)))))
            (setq new_q 0))
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
                (setq best-move-so-far i)))))

        ;; Update the scores in the node
        (setf (mc-node-veck-scores nodey) scores)
        ;; Update the visits to the chosen move
        (setf (svref (mc-node-veck-visits nodey) best-move-so-far)
              (+ 1 (svref visits best-move-so-far)))
        ;; Increment the number of visits to this node
        (setf (mc-node-num-visits nodey)
              (+ 1 (mc-node-num-visits nodey)))

        )
      ;; Use non threaded selection
      (t 
        ;; Compare all the potential moves
        (dotimes (i (length scores))
          ;; If there isn't gonan be problems dividing by 0
          (if (and (< 0 node-visits)
                   (< 0 (svref visits i)))
            ;; Calculate the monte-carlo value
            (setq new_q (* c  (sqrt (/ (log node-visits)
                                       (/ (svref visits i)
                                          node-visits)))))
            (setq new_q 0))
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
                (setq best-move-so-far i)))))

        ;; Update the scores in the node
        (setf (mc-node-veck-scores nodey) scores)
        ;; Update the visits to the chosen move
        (setf (svref (mc-node-veck-visits nodey) best-move-so-far)
              (+ 1 (svref visits best-move-so-far)))
        ;; Increment the number of visits to this node
        (setf (mc-node-num-visits nodey)
              (+ 1 (mc-node-num-visits nodey)))

        ))
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
(defun sim-tree (game tree c)
  ;; (format t "Sim Tree~%")
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
              )
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

;;  SIM-DEFAULT : GAME
;; ----------------------------------------------
;;  INPUT:  GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
    (game)
    (default-policy game))


;;  BACKUP : HASHY KEY-MOVE-ACC RESULT
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY
(defun backup (hashy key-move-acc result)
  (let ((node-holder nil)
        (key nil)
        (move nil)
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
      (setf (mc-node-num-visits node-holder)
            (+ 1 (mc-node-num-visits node-holder)))
      ;; Update N(S_t, A_t)
      (setf (svref (mc-node-veck-visits node-holder) move)
            (+ 1 (svref (mc-node-veck-visits node-holder) move)))
      ;; Update Q(S_t, A_t)
      (setf (svref (mc-node-veck-scores node-holder) move)
            (- result (svref (mc-node-veck-scores node-holder) move))))))


;; For use by each thread of uct-search
(defun sim-ops (orig-game c orig-tree total-sim tree-array id barrier)
  (let* ((state-move-list nil)
         (z 0)
         (game nil)
         (tree (svref tree-array id)) ; Get the tree created for this thread
         (times (random total-sim))   ; Add some randomness so the threads aren't all 
         ; trying to update the main tree at once
         (second-round (- total-sim times))
         )
    (dotimes (j 2)
      (dotimes (i times)
        ;; Make a copy of the game state
        (setq game (deep-copy-go orig-game))
        ;; Run sim-tree
        (setq state-move-list (sim-tree game tree c))
        ;; Run default
        (setq z (sim-default game))
        ;; Run backup
        (backup (mc-tree-hashy tree) state-move-list z))

      ;; This is a critical section
      (with-locked-structure (orig-tree)
                             ;; Merge the threads copy back into the main tree
                             (merge-mc-trees! orig-tree tree))

      ;; This is non-critical as each thread modifies only its own tree
      (merge-mc-trees! (svref tree-array id) tree)
      ;; Update the number of simulations to run
      (setq times second-round)
      )
    ;; Pass through the barrier to signal the thread is done 
    (mp:barrier-pass-through barrier)))


;; For use by each thread of uct-search
(defun sim-ops-new (orig-game c orig-tree total-sim id barrier)
  (let* ((state-move-list nil)
         (z 0)
         (game nil)
         (tree (new-mc-tree orig-game)) ; Get the tree created for this thread
         (times (- total-sim (* (/ total-sim *num-cores*) id)))   ; Add some randomness so the threads aren't all 
                                      ; trying to update the main tree at once
         (second-round (- total-sim times))
         )
    (dotimes (j 2)
      (dotimes (i times)
        ;; Make a copy of the game state
        (setq game (deep-copy-go orig-game))
        ;; Run sim-tree
        (setq state-move-list (sim-tree game tree c))
        ;; Run default
        (setq z (sim-default game))
        ;; Run backup
        (backup (mc-tree-hashy tree) state-move-list z))

      ;; This is a critical section
      (with-locked-structure (orig-tree)
                             ;; Merge the threads copy back into the main tree
                             (merge-mc-trees! orig-tree tree))

      ;; This is non-critical as each thread modifies only its own tree
      (merge-mc-trees! tree orig-tree)
      ;; Update the number of simulations to run
      (setq times second-round)
      )
    ;; Pass through the barrier to signal the thread is done 
    (mp:barrier-pass-through barrier)))


(defstruct (tree-q (:include synchronizing-structure)
                   (:print-function print-tree)
                   )
  (bar (mp:make-barrier 2))
  (ref-tree nil) ;;; The reference tree
  (q (list )))


(defun print-tree (tree str depth)
  (declare (ignore depth))
  (format str "~A, ~A" (tree-q-bar tree) (tree-q-q tree)))

(defun new-tree-q (tree)
  (make-tree-q :ref-tree tree))

;; For use by each thread of uct-search
(defun sim-ops-alt (orig-game c total-sims queue id barrier)
  (let* ((state-move-list nil)
         (z 0)
         (game nil)
         (tree (new-mc-tree orig-game)) ; Get the tree created for this thread
         (tree-copy (new-mc-tree orig-game))
         (tree-holder nil)

         (times (floor (/ (random total-sims) (- *mc-rounds* 1))))   ; Add some randomness so the threads aren't all 
         ; trying to update the main tree at once
         (sims-so-far 0)
         )
    (dotimes (j *mc-rounds*)
      (dotimes (i times)
        ;; Make a copy of the game state
        (setq game (deep-copy-go orig-game))
        ;; Run sim-tree
        (setq state-move-list (sim-tree game tree c))
        ;; Run default
        (setq z (sim-default game))
        ;; Run backup
        (backup (mc-tree-hashy tree) state-move-list z))

      ;; Update the copy
      (merge-mc-trees! tree-copy tree)

      (with-locked-structure (queue)
                             ;; If there aren't any queued trees
                             (if (= 0 (length (tree-q-q queue)))
                               ;; Add your tree to the queue
                               (push tree-copy (tree-q-q queue)))
                             ;; Otherwise get the tree 
                             (setq tree-holder (pop (tree-q-q queue))))

      ;; When the thread popped a tree
      (when tree-holder
        (merge-mc-trees! tree tree-holder)
        ;; Update the main tree
        (with-locked-structure (queue)
                               (merge-mc-trees! (tree-q-ref-tree queue) tree))

        (setq tree-holder nil)
        )
      ;; Update tree from the main tree
      (merge-mc-trees! tree (tree-q-ref-tree queue))

      ;; Update info
      (incf sims-so-far times)

      ;; Except for the last round
      (if (and (not (= total-sims sims-so-far)) 
               (< j (- *mc-rounds* 1)))

        ;; Get times for next round
        (setq times (random (- total-sims sims-so-far)))

        ;; On the last round do the remaining sims
        (setq times (- total-sims sims-so-far))))

    ;; Pass through the barrier to signal the thread is done 
    (mp:barrier-pass-through barrier)))

;; For use by each thread of uct-search
(defun sim-ops-alt-new (orig-game c total-sims queue id barrier)
  (let* ((state-move-list nil)
         (z 0)
         (game nil)
         (tree (new-mc-tree orig-game)) ; Get the tree created for this thread
         (tree-copy (new-mc-tree orig-game))
         (tree-holder nil)

         (times (floor (/ (random total-sims) (- *mc-rounds* 1))))   ; Add some randomness so the threads aren't all 
         ; trying to update the main tree at once
         (sims-so-far 0)
         )
    (dotimes (j *mc-rounds*)
      (dotimes (i times)
        ;; Make a copy of the game state
        (setq game (deep-copy-go orig-game))
        ;; Run sim-tree
        (setq state-move-list (sim-tree game tree c))
        ;; Run default
        (setq z (sim-default game))
        ;; Run backup
        (backup (mc-tree-hashy tree) state-move-list z))

      ;; Update the copy
      (merge-mc-trees! tree-copy tree)

      (with-locked-structure (queue)
                              ;; Add your tree to the queue
                             (push tree-copy (tree-q-q queue))
                             ;; If there are at least two trees on the q
                             (when (< 1 (length (tree-q-q queue)))
                               ;; Spawn a thread to merge and update them
                               (mp:process-run-function (write-to-string (+ *num-cores* id)) #'maintain-tree-q  queue )))

      ;; When the thread popped a tree
;     (when tree-holder
;       (merge-mc-trees! tree tree-holder)
;       ;; Update the main tree
;       (with-locked-structure (queue)
;                              (merge-mc-trees! (tree-q-ref-tree queue) tree))

;       (setq tree-holder nil)
;       )
      ;; Update tree from the main tree
      (merge-mc-trees! tree (tree-q-ref-tree queue))

      ;; Update info
      (incf sims-so-far times)

      ;; Except for the last round
      (if (and (not (= total-sims sims-so-far)) 
               (< j (- *mc-rounds* 1)))

        ;; Get times for next round
        (setq times (random (- total-sims sims-so-far)))

        ;; On the last round do the remaining sims
        (setq times (- total-sims sims-so-far))))

    ;; Pass through the barrier to signal the thread is done 
    (mp:barrier-pass-through barrier)))




(defun maintain-tree-q (queue)
  (let ((tree1 nil)
        (tree2 nil)
        )

      ;; Critical
      (with-locked-structure (queue)
                             ;; Get the trees
                             (setq tree1 (pop (tree-q-q queue)))
                             (setq tree2 (pop (tree-q-q queue))))

      ;; Merge them
      (merge-mc-trees! tree1 tree2)

      ;; Add the info to the main tree
      (with-locked-structure (queue)
                             (merge-mc-trees! (tree-q-q queue) tree1))))

;;  UCT-SEARCH : ORIG-GAME NUM-SIMS C
;; -------------------------------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  The best move according to monte-carlo tree search.
(defun uct-search (orig-game num-sims c &optional 
                             (return-tree nil) (use-threads nil))
  (cond
    ;; Use threaded implementation
    ((= 1 use-threads )

     (let* ((tree (new-mc-tree orig-game))
            (tree-queue (new-tree-q tree))
            (name nil)
            ;; Wait for the uct threads and the tree-q maintainers
            (barrier (mp:make-barrier (+ *num-cores* 1)))
            (sims-per-thread (ceiling (/ num-sims *num-cores*)))
            )
       ;; Spawn the threads
       (dotimes (i *num-cores*)
         ;; Create a process and start it running with the tree
         (mp:process-run-function (write-to-string i) #'sim-ops-new orig-game c tree 
                                  sims-per-thread i barrier))

       ;; Wait until all the threads are finished
       (mp:barrier-wait barrier)

       ;; Select the best move
       (svref (legal-moves orig-game) 
              (select-move (gethash (make-hash-key-from-game orig-game)
                                    (mc-tree-hashy tree)) c))))
    ;; Use threaded implementation
    ((= 2 use-threads )
     (let* ((tree (new-mc-tree orig-game))
            (tree-array (make-array *num-cores* :initial-element (deep-copy-mc-tree tree)))
            (name nil)
            (barrier (mp:make-barrier (+ *num-cores* 1)))
            (sims-per-thread (floor (/ num-sims *num-cores*)))
            )
       ;; Spawn the threads
       (dotimes (i *num-cores*)
         (setq name (write-to-string i))
         ;; Create a process and start it running with the tree
         (mp:process-run-function name #'sim-ops orig-game c tree 
                                  ;; Add some variance in the number of sims for each thread so they don't complete all once
                                  (* 2 i sims-per-thread)
                                  tree-array i barrier))
       ;; Wait until all the threads are finished
       (mp:barrier-wait barrier)

       ;; Select the best move
       (svref (legal-moves orig-game) 
              (select-move (gethash (make-hash-key-from-game orig-game)
                                    (mc-tree-hashy tree)) c))))

    ((= 3 use-threads)

     (let* ((tree (new-mc-tree orig-game))
            (tree-queue (new-tree-q tree))
            (name nil)
            ;; Wait for the uct threads and the tree-q maintainers
            (barrier (mp:make-barrier (+ *num-cores* 1)))
            (sims-per-thread (ceiling (/ num-sims *num-cores*)))
            )
       ;; Spawn the threads
       (dotimes (i *num-cores*)
         (setq name (write-to-string i))
         ;; Create a process and start it running with the tree
         (mp:process-run-function name #'sim-ops-alt orig-game c 
                                  sims-per-thread tree-queue i barrier))

       ;; Wait until all the threads are finished
       (mp:barrier-wait barrier)

       ;; Select the best move
       (svref (legal-moves orig-game) 
              (select-move (gethash (make-hash-key-from-game orig-game)
                                    (mc-tree-hashy tree)) c))))

    ((= 4 use-threads)

     (let* ((tree (new-mc-tree orig-game))
            (tree-queue (new-tree-q tree))
            (name nil)
            ;; Wait for the uct threads and the tree-q maintainers
            (barrier (mp:make-barrier (+ *num-cores* 1)))
            (sims-per-thread (ceiling (/ num-sims *num-cores*)))
            )
       ;; Spawn the threads
       (dotimes (i *num-cores*)
         (setq name (write-to-string i))
         ;; Create a process and start it running with the tree
         (mp:process-run-function name #'sim-ops-alt-new orig-game c 
                                  sims-per-thread tree-queue i barrier))

       ;; Wait until all the threads are finished
       (mp:barrier-wait barrier)

       ;; Select the best move
       (svref (legal-moves orig-game) 
              (select-move (gethash (make-hash-key-from-game orig-game)
                                    (mc-tree-hashy tree)) c))))
;; Otherwise perform the operations sequentially
    (t
      (let ((state-move-list nil)
            (tree (new-mc-tree orig-game))
            (z 0)
            (game nil))
        (dotimes (i num-sims)
          ;; Make a copy of the game state
          (setq game (deep-copy-go orig-game))
          ;; Run sim-tree
          (setq state-move-list (sim-tree game tree c))
          ;; Run default
          (setq z (sim-default game))
          ;; Run backup
          (backup (mc-tree-hashy tree) state-move-list z))
        ;; For testing
        (when return-tree 
          (return-from uct-search tree))

        ;; Select the best move
        (svref (legal-moves orig-game) 
               (select-move (gethash (make-hash-key-from-game orig-game)
                                     (mc-tree-hashy tree)) c)))
      )
    ))

;;  COMPETE : BLACK-NUM-SIMS BLACK-C WHITE-NUM-SIMS WHITE-C
;; --------------------------------------------------
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           WHITE-NUM-SIMS, the number of simulations for each of white's moves
;;           WHITE-C, the exploration/exploitation constant used by white
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun compete
  (black-num-sims black-c white-num-sims white-c 
                  &optional (black-threads? nil)(white-threads? nil))
  (let ((g (init-game)))
    (while (not (game-over? g))
           (cond
             ((eq (gg-whose-turn? g) *black*)
              (format t "BLACK'S TURN!~%")
              ;;(format t "~A~%" 
              (print-go 
                (do-move! g (uct-search g black-num-sims black-c nil black-threads?)) t nil t t nil))
             (t
               (format t "WHITE'S TURN!~%")
               ;;(format t "~A~%"
               (print-go
                 (do-move! g (uct-search g white-num-sims white-c nil white-threads?))t nil t t nil))))))

