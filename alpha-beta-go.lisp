;; ====================================
;;  CMPU-365, Spring 2017
;;  Name: David S. Wallach
;; ====================================

;; Tell the copiler to speed things up
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

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

(defun compute-min (g curr-depth alpha beta statty cutoff-depth)

  ;; Increment the number of moves "done"
  (setf (stats-num-moves-done statty) (+ (stats-num-moves-done statty) 1)) 

  (cond
    ;; If the game is over 
    ;; Player wins, return the 
    ;; winning minus the current depth
    ((game-over? g) (- *win-value* curr-depth))

    ;; If the cutoff depth is reached
    ;; Return result of the static evaluation function
    ((= cutoff-depth curr-depth)  (eval-func g))

    ;; Otherwise
    (t 
      (let ((moves (legal-moves g))
            (node-val nil))

        ;; Update stats data
        (setf (stats-num-potential-moves statty)
              (+ (stats-num-potential-moves statty) (length moves)))

        ;; Do each move
        (dolist (move moves)

          ;; Modify the game state 
          (do-move! g move) 

          ;; Compute the node's value
          (setq node-val (compute-max 
                           g (+ curr-depth 1) 
                           alpha beta 
                           statty cutoff-depth))

          ;; Undo modification
          (undo-move! g)

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

(defun compute-max (g curr-depth alpha beta statty cutoff-depth)

  ;; Increment the number of moves "done"
  (setf (stats-num-moves-done statty) (+ (stats-num-moves-done statty) 1)) 

  (cond
    ;; Get each players score 
    ;; Player loses, return the value of 
    ;; value of losing plus the current depth
    ((game-over? g) (+ *loss-value* curr-depth))

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
          (do-move! g move) 

          ;; Compute the node's value
          (setq node-val (compute-min 
                           g (+ curr-depth 1)
                           alpha beta 
                           statty cutoff-depth))

          ;; Undo modification
          (undo-move! g)

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

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)

  ;; Compute move handles the initial call to 
  ;; compute-min each child of the node at depth 0
  ;; Compute the optimal move
  (let* ((alpha -1000000)
         (beta 1000000) 
         (statty (make-stats))
         (moves (legal-moves g)) ; fetch legal moves  
         (best-so-far nil)
         (best-score -1000000)
         (current-score 0))

    ;; Increment stats data
    (setf (stats-num-potential-moves statty)
          (+ (stats-num-potential-moves statty) (length moves)))

    ;; Do each move
    (dolist (move moves)

      (do-move! g move)

      ;; Get the score
      (setq current-score (compute-min g 1 alpha beta statty cutoff-depth)) 

      ;; Reset the game state
      (undo-move! g)

      ;; When the generated node is the best so far
      (when (> current-score best-score)

        ;; Update the variables
        (setq best-so-far move)
        (setq best-score current-score)))

    ;; Print out the final alpha of the root node
    (format t "Root Alpha: ~A~%" best-score)

    (format t "Potential Moves: ~A~%" 
            (stats-num-potential-moves statty))
    (format t "Moves Done: ~A~%" 
            (stats-num-moves-done statty)) 
    (format t "Moves Pruned: ~A~%" 
            (- (stats-num-potential-moves statty) 
               (stats-num-moves-done statty)))
    (format t "My move: ~A~%" best-so-far)

    ;; return my-move
    best-so-far))

;;  PLAY-GAME
;; ---------------------------------------------
;; A function for setting to A.I.'s with different
;; depths against each other. For fun.
(defun play-game (game depth-one depth-two one?)
  (if (game-over? game)
    (format t "Game Over~%~A" game)
    (when (if one? 
            (do-move! game (compute-move game depth-one))
            (do-move! game (compute-move game depth-two)))
      (format t "Game State~%~A" game)
      (play-game game depth-one depth-two (not one?)))))
