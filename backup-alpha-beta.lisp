;; ====================================
;;  CMPU-365, Spring 2017
;;  Asmt. 4
;;  alpha-beta-dsw.lisp
;;  Feb. 28, 2017
;;  Name: David S. Wallach
;; ====================================


;;  COMPUTE-MIN-MAX
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;           MIN-OR-MAX, a function, either COMPUTE-MIN or COMPUTE-MAX
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-min-max (g curr-depth alpha beta statty cutoff-depth is-max?)
  
  (labels 
    ;; Compute the maximum score for all child nodes 
    ((compute-max (moves node-val)

                  ;; Do each move
                  (dolist (move moves alpha)

                    ;; Modify the game state 
                    (apply #'do-move! g nil move) 

                    ;; Compute the node's value
                    (setq node-val (compute-min-max 
                                     g (+ curr-depth 1) alpha beta 
                                     statty cutoff-depth nil))

                    ;; Undo modification
                    (undo-move! g)

                    ;; When it returns a value greater than alpha
                    (when (> node-val alpha)
                      ;; Update alpha
                      (setq alpha node-val)

                      ;; Check if anything can be pruned
                      (when (>= alpha beta)

                        ;; When it can, skip the remaining nodes
                        (return-from compute-min-max alpha))))
                  
                  ;; Return alpha
                  alpha)

     ;; Compute the minimum score for all child nodes 
     (compute-min (moves node-val)

                  ;; Do each move
                  (dolist (move moves beta)

                    ;; Modify the game state 
                    (apply #'do-move! g nil move) 

                    ;; Compute the node's value
                    (setq node-val (compute-min-max 
                                     g (+ curr-depth 1) alpha beta 
                                     statty cutoff-depth t))

                    ;; Undo modification
                    (undo-move! g)

                    (when (< node-val beta)

                      ;; Update beta
                      (setq beta node-val)

                      ;; Check if anything can be pruned
                      (when (>= alpha beta)

                        ;; When it can, skip the remaining nodes
                        (return-from compute-min-max beta))))

                  ;; Return beta
                  beta))

    ;; Increment the number of moves "done"
    (setf (stats-num-moves-done statty) (+ (stats-num-moves-done statty) 1)) 

    (cond
      ;; If the current depth is 0 
      ((= curr-depth 0) 
       (let ((moves (legal-moves g)) ; fetch legal moves  
             (best-so-far nil)
             (best-score -1000000)
             (current-score 0))

         ;; Increment stats data
         (setf (stats-num-potential-moves statty)
               (+ (stats-num-potential-moves statty) (length moves)))

         ;; Do each move
         (dolist (move moves best-so-far)

           (apply #'do-move! g nil move)

           ;; Get the score
           (setq current-score (compute-min-max g 1 alpha beta statty cutoff-depth nil)) 

           ;; Reset the game state
           (undo-move! g)

           ;; When the generated node is the best so far
           (when (> current-score best-score)

             ;; Update the variables
             (setq best-so-far move)
             (setq best-score current-score)))

         ;; Print out the final alpha of the root node
         (format t "Root Alpha: ~A~%" alpha)
         
         ;; Return the best move found
         best-so-far))

      ;; Get each players score 
      ((game-over? g) 
       
         ;; If it is the player's turn
         (if is-max?

             ;; Player loses, return the value of 
             ;; value of losing plus the current depth
             (+ *loss-value* curr-depth)

             ;; Otherwise player wins, return the 
             ;; winning minus the current depth
             (- *win-value* curr-depth)))

      ;; If the cutoff depth is reached
      ;; Return result of the static evaluation function
      ((= cutoff-depth curr-depth)  (eval-func g))

      ;; Get the legal moves
      (t (let ((moves (legal-moves g))
               (node-val nil))

           ;; Increment stats data
           (setf (stats-num-potential-moves statty)
                 (+ (stats-num-potential-moves statty) (length moves)))

           (if is-max? ; If the current node is a max node 
             (setq alpha (compute-max moves node-val))
             (setq beta (compute-min moves node-val)))

           ;; Return alpha/beta
           (if is-max? alpha beta))))))


  ;;  COMPUTE-MOVE
  ;; -------------------------------------------------------------
  ;;  INPUTS:  G, a CHESS struct
  ;;           CUTOFF-DEPTH, to limit depth of minimax search
  ;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
  ;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)
  ;; Compute the optimal move
  (let* ((alpha -10000)
         (beta 10000) 
         (my-stats (make-stats))
         (my-move (compute-min-max g 0 alpha beta my-stats cutoff-depth t)))
    (format t "Potential Moves: ~A~%" 
            (stats-num-potential-moves my-stats))
    (format t "Moves Dones: ~A~%" 
            (stats-num-moves-done my-stats)) 
    (format t "Moves Pruned: ~A~%" 
            (- (stats-num-potential-moves my-stats) 
               (stats-num-moves-done my-stats)))
    (format t "My move: ~A~%" my-move)

    ;; return my-move
    my-move))


;;  MY-TEST
;; -------------------------------
;;  Fill in the contract

(defun my-test
  ()
  'THIS_FUNCTION_SHOULD_TEST_A_SAMPLE_CHESS_PROBLEM!
  )
