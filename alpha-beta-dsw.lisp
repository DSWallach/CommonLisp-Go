;; ====================================
;;  CMPU-365, Spring 2017
;;  Asmt. 4
;;  alpha-beta-dsw.lisp
;;  Feb. 28, 2017
;;  Name: David S. Wallach
;; ====================================
;; NOTE: Thank you for your help in office hours
;;       it seemed obvious the moment you pointed
;;       out but I was stuck there for a while. 
;;       I changed my implementation again because
;;       it was very slow before. Maybe it's 
;;       because I ssh in but I still wouldn't 
;;       call a depth of 6 "comfertable"

;; Tell the copiler to speed things up
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

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
          (apply #'do-move! g nil move) 

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
          (apply #'do-move! g nil move) 

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

      (apply #'do-move! g nil move)

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


;; A function for setting to A.I.'s with different
;; depths against each other. For fun.
(defun play-game (game depth-one depth-two one?)
  (if (game-over? game)
    (format t "Game Over~%~A" game)
    (when (if one? 
            (apply #'do-move! game nil (compute-move game depth-one))
            (apply #'do-move! game nil (compute-move game depth-two)))
      (format t "Game State~%~A" game)
      (play-game game depth-one depth-two (not one?)))))

;;  MY-TEST
;; -------------------------------
;; Source: http://chesstempo.com/chess-problems/31965 
;; Note: I had to reduce the search depth from 6 to get it
;;       to find the locally optimal solution
(defun my-test ()
  (problem "MY-TEST: White should sacrifice it's bishop to take black's Knight and Queen!")
  (let ((g (init-game 
             ;; White
             (list (list *pawn* 1 0) 
               (list *pawn* 1 1) 
               (list *pawn* 1 5) 
               (list *pawn* 1 6) 
               (list *pawn* 2 7)
               (list *knight* 0 1) 
               (list *bishop* 0 2) 
               (list *bishop* 2 5) 
               (list *rook* 0 0) 
               (list *rook* 0 5) 
               (list *queen* 0 3) 
               (list *king* 0 6))

             ;; Black
             (list (list *pawn* 6 0) 
               (list *pawn* 6 1) 
               (list *pawn* 5 4)
               (list *pawn* 6 5) 
               (list *pawn* 6 6) 
               (list *pawn* 6 7)
               (list *knight* 5 2) 
               (list *knight* 5 5) 
               (list *bishop* 7 5) 
               (list *rook* 7 0) 
               (list *rook* 7 7) 
               (list *queen* 3 3) 
               (list *king* 7 4)))))
           ;; White should check in three
           (compute-do-and-show-n-moves g 3 4)
    (format t "White should have taken black's Queen ~%~%~%")))

;;  MY-TEST
;; -------------------------------
;; Source: https://www.chess.com/forum/view/daily-puzzles/3-9-2017-rook-and-pawn-endgames 
;; White should
(defun my-test-2 (&optional (cutoff-depth 4))
  (problem "MY-TEST: White should be able to win the rook in two moves!")
  (let ((g (init-game 

             ;; White
             (list (list *king* 0 6)
                   (list *rook* 0 4)
                   (list *bishop* 1 4)
                   (list *pawn* 1 5)
                   (list *pawn* 1 6)
                   (list *pawn* 1 7)
                   (list *pawn* 5 6)
                   )

             ;; Black
             (list (list *king* 6 1)
                   (list *queen* 4 5)
                   (list *bishop* 6 0)
                   ))))

    ;; White should check in three
    (compute-do-and-show-n-moves g cutoff-depth 5)
    (format t "White should have put Black's king in check!~%~%~%")))
