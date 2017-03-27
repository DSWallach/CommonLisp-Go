;; ====================================
;;  CMPU-365, Spring 2017
;;  Name: David S. Wallach
;; ====================================

;;  WIN-LOSS VALUES

(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)

;;  STATS struct
;; ---------------------------
;;  Stats compiled during minimax search

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))

;;  COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-min (g curr-depth alpha beta statty cutoff-depth &optional (debug? nil))

  ;; Increment the number of moves "done"
  (setf (stats-num-moves-done statty) (+ (stats-num-moves-done statty) 1)) 

  (cond
    ;; If the game is over 
    ;; Player wins, return the 
    ;; winning minus the current depth
    ((game-over? g) 

     (let ((player (gg-whose-turn? g))
           (subtotals (gg-subtotals g))
           )
       (cond 
         ;; If the opponent has a higher score
         ((< (svref subtotals (- 1 player))
             (svref subtotals player))
          ;; Give loss val
          (return-from compute-min (+ *loss-value* curr-depth)))
         ;; If player has a higher score 
         ((> (svref subtotals (- 1 player))
             (svref subtotals player))
          ;; Give win val
          (return-from compute-min (- *win-value* curr-depth)))
         ;; Otherwise it's a tie so white wins
         (t 
           (if (= player *white*) 
             (return-from compute-min (- *win-value* curr-depth))
             (return-from compute-min (+ *loss-value* curr-depth))))
         ))
     )

    ;; If the cutoff depth is reached
    ;; Return result of the static evaluation function
    ((= cutoff-depth curr-depth)  (eval-func g))

    ;; Otherwise
    (t 
      (let ((moves (legal-moves g))
            (node-val nil)
            )

        ;; Update stats data
        (setf (stats-num-potential-moves statty)
              (+ (stats-num-potential-moves statty) (length moves)))

        ;; Do each move
        (dolist (move moves)

          ;; Modify the game state 
          (do-move! g move debug?) 

          ;; Compute the node's value
          (setq node-val (compute-max 
                           g (+ curr-depth 1) 
                           alpha beta 
                           statty cutoff-depth))

          ;; Undo modification
          (undo-move! g debug?)

          (when (< node-val beta)

            ;; Update beta
            (setq beta node-val)

            ;; Check if anything can be pruned
            (when (>= alpha beta)

              ;; When it can, skip the remaining nodes
              (return-from compute-min beta)))))

      ;; Return Beta
      beta)))

;;  COMPUTE-MAX
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth &optional (debug? nil))

  ;; Increment the number of moves "done"
  (setf (stats-num-moves-done statty) (+ (stats-num-moves-done statty) 1)) 

  (cond
    ;; Get each players score 
    ;; Player loses, return the value of 
    ;; value of losing plus the current depth
    ((game-over? g) 
     (let ((player (gg-whose-turn? g))
           (subtotals (gg-subtotals g))
           )
       (cond 
         ;; If the opponent has a higher score
         ((> (svref subtotals (- 1 player))
             (svref subtotals player))
          ;; Give loss val
          (return-from compute-max (+ *loss-value* curr-depth)))
         ;; If player has a higher score 
         ((< (svref subtotals (- 1 player))
             (svref subtotals player))
          ;; Give win val
          (return-from compute-max (- *win-value* curr-depth)))
         ;; Otherwise it's a tie
         (t 
           (if (= player *white*) 
             (return-from compute-max (- *win-value* curr-depth))
             (return-from compute-max (+ *loss-value* curr-depth))))
         ))
     )

    ;; If the cutoff depth is reached
    ;; Return result of the static evaluation function
    ((= cutoff-depth curr-depth)  (eval-func g))

    ;; Get the legal moves
    (t 
      (let ((moves (legal-moves g))
            (node-val nil))

        ;; Update stats data
        (setf (stats-num-potential-moves statty)
              (+ (stats-num-potential-moves statty) (length moves)))

        ;; Do each move
        (dolist (move moves)

          ;; Modify the game state 
          (do-move! g move debug?) 

          ;; Compute the node's value
          (setq node-val (compute-min 
                           g (+ curr-depth 1)
                           alpha beta 
                           statty cutoff-depth))

          ;; Undo modification
          (undo-move! g debug?)

          ;; When it returns a value greater than alpha
          (when (> node-val alpha)
            ;; Update alpha
            (setq alpha node-val)

            ;; Check if anything can be pruned
            (when (>= alpha beta)

              ;; When it can, skip the remaining nodes
              (return-from compute-max alpha)))))
      ;; Return alpha
      alpha)))

;;  COMPUTE-MOVE : G CUTOFF-DEPTH
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth &optional (print-to t) (debug? nil))
  (format print-to "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)

  ;; Compute move handles the initial call to 
  ;; compute-min each child of the node at depth 0
  ;; Compute the optimal move
  (let* ((alpha -1000000)
         (beta 1000000) 
         (statty (make-stats))
         (moves (legal-moves g)) ; fetch legal moves  
         (best-so-far nil)
         (best-score -1000000)
         (counter 0)
         (current-score 0))

    ;; Increment stats data
    (setf (stats-num-potential-moves statty)
          (+ (stats-num-potential-moves statty) (length moves)))

    ;; Do each move
    (dolist (move moves)

      (do-move! g move)

      ;; Get the score
      (setq current-score (compute-min g 1 alpha beta statty cutoff-depth debug?)) 

      ;; Reset the game state
      (undo-move! g debug?)

      (setq counter (+ counter 1))
      ;; When the generated node is the best so far
      (when (> current-score best-score)

        ;; Update the variables
        (setq best-so-far move)
        (setq best-score current-score)))

    ;; Print out the final alpha of the root node
    (format print-to "Root Alpha: ~A~%" best-score)

    (format print-to "Potential Moves: ~A~%" 
            (stats-num-potential-moves statty))
    (format print-to "Moves Done: ~A~%" 
            (stats-num-moves-done statty)) 
    (format print-to "Moves Pruned: ~A~%" 
            (- (stats-num-potential-moves statty) 
               (stats-num-moves-done statty)))
    (format print-to "My move: ~A~%" best-so-far)

    ;; return my-move
    best-so-far))
