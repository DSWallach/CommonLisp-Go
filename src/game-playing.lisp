;;;;; GAME PLAYING
;;  PUT-PIECE! : GAME POS &DEBUG? 
;; -------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Restores given piece to the board at the
;;    location specified by its ROW and COL fields. 
;;  Find any groups the move at row col should belong to
;;  If there are multiple groups merge them into one

(defun put-piece! (game pos)
  (let* ((new-group nil)
         (vec (vector (pos->row pos)
                      (pos->col pos)))
         (row (svref vec 0))
         (col (svref vec 1))
         (group-dist 1) ; A variable to allow for potentially using different values
         (player (gg-whose-turn? game))
         (connected-groups ())
         (player-groups (svref (gg-groups game) player)))

    ;; Recursive subroutines to locate and retrive the 
    ;; group the move at (row, col) should be added to 
    (labels 
      ;; ADD-PIECE (GROUP), Add the piece at row col to group
      ((add-piece! 
         (group)
         ;; Destructively modify the group
         (push (row-col->pos row col) 
               (group-pieces group))
         ;; Update area
         (calc-new-area! group row col)
         ;; Update group 
         (update-group! group game))

       ;; FIND-GROUP (GROUP), Check it the piece at row col, is connected to GROUP
       (find-group 
         (group)
         (let ((vec nil)
               )
           (dolist (pos (group-pieces group))
             ;; Get the row/col of the piece
             (setq vec (vector (pos->row pos) (pos->col pos)))
             ;; If the new piece at (row, col) is to the right or left of the piece
             (when (or (and (= (svref vec 0) row)
                            (or (= (svref vec 1) (- col *group-dist*))
                                (= (svref vec 1) (+ col *group-dist*))))
                       ;; Or if it is above or below the piece
                       (and (= (svref vec 1) col)
                            (or (= (svref vec 0) (- row *group-dist*))
                                (= (svref vec 0) (+ row *group-dist*)))))
               ;; Return true
               (return-from find-group t)))))

       ;; CHECK-GROUPS, Find the group the piece should belong to if it exists
       (check-groups 
         ()
         ;; Do for each group 
         (dolist (group player-groups)
           ;; When it's in the area of the group
           (when (and (>= (+ row group-dist) 
                          (svref (group-area group) *min-row*))
                      (>= (+ col group-dist) 
                          (svref (group-area group) *min-col*))
                      (<= (- row group-dist) 
                          (svref (group-area group) *max-row*))
                      (<= (- col group-dist) 
                          (svref (group-area group) *max-col*))
                      (find-group group)
                      )

             ;; Push it onto the connected groups
             (push group connected-groups)

             (setf (svref (gg-groups game) player)
                   (remove group
                           (svref (gg-groups game) player)
                           :test #'equal-group?)))) 
         ;; Return true
         t)
       )

      ;; Put the piece on the board 
      (incf (svref (gg-board game) pos) 
            (player->piece player))
      ;; Update the hash key

      (setf (gg-board-hash game)
            (bit-xor (gg-board-hash game)
                     (svref (svref *zobrist-vectors* player) pos)))

      ;; If there are no groups 
      (if (null (svref (gg-groups game) player)) 
        ;; Make a new group 
        (when (setq new-group (init-group row col))

          ;; Update area
          (calc-area! new-group)
          ;; Update group
          (update-group! new-group game)

          ;; Make a list of groups containing the current move
          (setf (svref (gg-groups game) player) (list new-group))

          ;; Return 0
          0)

        ;; Otherwise check the groups of the player whose turn it is
        (when (check-groups)
          (cond
            ;; If no group was found
            ;; Make a new group
            ((= 0 (length connected-groups)) 
             ;; Make a new group
             (setq new-group (init-group row col))
             ;; Update area
             (calc-area! new-group)
             ;; Update group
             (update-group! new-group game)
             ;; Add it to the list of groups
             (push new-group (svref (gg-groups game) player))
             ;; Return 0
             0)
            ;; If one group was found  
            ((= 1 (length connected-groups)) 

             ;; Get the group
             (setq new-group (pop connected-groups))

             ;; Add the piece and update the group
             (add-piece! new-group) 

             ;; Add it back at the front of the groups list
             (push new-group 
                   (svref (gg-groups game) player))
             ;; Return 1
             1)

            ;; Otherwise merge the groups
            (t 
              (dolist (group connected-groups)
                (setf (svref (gg-groups game) player)
                      (remove group 
                              (svref (gg-groups game) 
                                     player)
                              :test #'equal-group?)))

              ;; Get the first group to merge
              (setq new-group (pop connected-groups))            

              ;; Merge it with the remaining groups 
              (dolist (group connected-groups)
                (setq new-group (merge-groups! new-group group)))

              ;; Update it with the new piece 
              (add-piece! new-group)

              ;; Push it back onto the players groups
              (push new-group (svref (gg-groups game) player))

              ;; Return the number of groups merged
              (+ (length connected-groups) 1))))))))

;;  DO-MOVE! : GAME POS
;; --------------------------------------
;;  Works like the chess-solns function of the same name
(defun do-move! (game pos &optional (verbose? nil))
  (declare (ignore verbose?))
  (format t "Groups ~A~%" (gg-groups game))
  (let* ((captured 0)
         (board (gg-board game))
         (player (gg-whose-turn? game))
         (opponent (- 1 player))
         (groups (svref (gg-groups game) (- 1 player)))
         )

    ;; Function for finding and capturing groups
    (labels
      ((groups-capture
         (group groups-length)
         (let ((capture-list nil)
               )
           ;; Check if any groups were captured
           (dotimes (i groups-length) 

             ;; Get the group
             (setq group (nth i groups))

             ;; Update liberties
             (calc-liberties! group (gg-board game))

             ;; When a group is to be captured
             (when (= 0 (group-liberties group)) 
               ;; Mark the group's position
               (setf (group-last-pos group) i)
               ;; Add it to the list with placement information
               (push (nth i groups) capture-list)))

           ;; Capture each group in the list
           (dolist (c-group capture-list)

             ;; Increment the capture flag
             (setq captured (+ 1 captured))

             ;; Capture the group
             (capture-group! c-group game)

             (when (find c-group (svref (gg-groups game) (- 1 player)))
               ;;(format debug? "Group wasn't removed~%")
               (break))
             )

           ;; If only one group containing only one piece 
           ;; was captured set the flag to check for ko
           (when (and (= captured 1) (= 1 (length (group-pieces group)))
                      (setf (gg-ko? game) t)))))
       )

      ;; Push the board onto board history
      (push (copy-vector board) 
            (gg-board-history game))

    ;; If the move is a pass
    (cond
      ((= *board-size* pos)
       ;; Push the pass
       (push (vector pos 0 0) 
             (gg-move-history game)))

      ;; Otherwise Put their piece at pos 
      (t ;; Track if any groups were merged in this round
        (let ((groups-merged nil)
              (group nil)
              )

          ;; Get the number of groups merged by the move
          (setq groups-merged (put-piece! game pos))

          ;; Capture any dead groups
          (groups-capture group (length groups))

          ;; If more groups were captured remove the flag
          (when (< 1 captured)
            (setf (gg-ko? game) nil))

          (when (= captured 0)
            (setf (gg-ko? game) nil))

          ;; Evaluate each players score
          (eval-subtotals! game)

          (push (vector pos captured groups-merged) 
                (gg-move-history game))
          )

        ;; Update player's groups
        (dolist (group (svref (gg-groups game) player))
          (calc-area! group)
          (update-group! group game))

        ;; Update opponent's groups
        (dolist (group (svref (gg-groups game) opponent))
          (calc-area! group)
          (update-group! group game))

        ;; Evaluate each players score
        (eval-subtotals! game)
        ))
    ;; Change turn
    (setf (gg-whose-turn? game) 
          (- 1 player))

    ;; Return the game
    game)))

;;  UNDO-MOVE! : GAME
;; ----------------------------------------
;;  Undo the most recently played move
(defun undo-move! (game &optional (verbose? nil))
  (declare (ignore verbose?))
  (if
    ;; If undoing the last move of the game
    ;; reset the groups, over flag, and turn
    (gg-over? game)
    ;; Get the copies of the group lists
    (let ((b-groups (svref (gg-groups game) 2))
          (w-groups (svref (gg-groups game) 3))
          (b-caps (svref (gg-captures game) 2))
          (w-caps (svref (gg-captures game) 3))
          )
      ;; Reset the groups
      (setf (gg-groups game)
            (vector b-groups w-groups))
      ;; Reset the captues
      (setf (gg-captures game)
            (vector b-caps w-caps))

      ;; Update groups
      (dolist (group (svref (gg-groups game) *black*))
        (update-group! group game))
      (dolist (group (svref (gg-groups game) *white*))
        (update-group! group game))
      ;; Update scores
      (eval-subtotals! game)
      ;; Pop the move
      (pop (gg-move-history game))
      ;; Reset the board
      (setf (gg-board game)
            (pop (gg-board-history game)))
      ;; Reset the over falg
      (setf (gg-over? game) nil)
      ;; Reset turn
      (setf (gg-whose-turn? game) 
            (- 1 (gg-whose-turn? game)))
      )
    (let* ((captured 0)
           (move (pop (gg-move-history game)))
           (pos (svref move 0))
           (player (gg-whose-turn? game))
           (opponent (- 1 player))
           )

      ;; Delete the previous board 
      (pop (gg-board-history game))

      ;; Unless the previous move was a pass
      (unless (= *board-size* pos)

        ;; Remove the piece from the board and from player's groups
        (pull-piece! game move) 

        ;; If nothing was captured reset the ko flag
        (when (= captured 0)
          (setf (gg-ko? game) nil))


        ;; If necessary return captured groups
        (when (< 0 (svref move 1))
          ;;(format debug? "Return captured groups ~%")
          (let ((group nil)
                )

            ;; Return each captured group
            (dotimes (i (svref move 1))

              (setq group (pop (svref (gg-captures game) opponent)))

              ;; For each piece in the group
              (dolist (p (group-pieces group))
              ;; Add the group's pieces to the board
                (setf (svref (gg-board game) p) 
                      (player->piece player))
                ;; Update the hash
                (setf (gg-board-hash game)
                      (bit-xor (gg-board-hash game)
                               (svref (svref *zobrist-vectors* player) p)))
                )

              ;; Add the group back in it's previous position
              (setf (svref (gg-groups game) player)
                    ;; Append the groups that came before group
                    (append (subseq (svref (gg-groups game) player) 
                                    0 (group-last-pos group))  
                            (list group) ; A list containing group
                            ;; The groups that came after group
                            (subseq (svref (gg-groups game) player) 
                                    (group-last-pos group)))))))

        ;; Update player's groups
        (dolist (group (svref (gg-groups game) player))
          (calc-area! group)
          (update-group! group game))

        ;; Update opponent's groups
        (dolist (group (svref (gg-groups game) opponent))
          (calc-area! group)
          (update-group! group game))

        ;; Evaluate each players score
        (eval-subtotals! game))

      ;; Change turn
      (setf (gg-whose-turn? game) opponent)))
)

;;  PULL-PIECE! : GAME MOVE &DEBUG? 
;; ---------------------------------------------------------------
;;  INPUTS:  GAME, a GO-GAME struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Removes given piece from the board. Removes the
;;                  piece from the group it is a member of, seperating
;;                  the group if necessary.

(defun pull-piece! (game move)
  ;; The player of the previous turn is the opponent this turn
  (let* ((player (gg-whose-turn? game))
         (opponent (- 1 player))
         (pos (svref move 0))
         (group (pop (svref (gg-groups game)
                            opponent)))
         )
    (labels (
             (history-test? (pos move)
                            (= pos (svref move 0)))
             (group-order?
               (group-one group-two)
               (when (< (position (first (group-pieces group-one))
                                  (gg-move-history game)
                                  :test #'history-test?)
                        (position (first (group-pieces group-two))
                                  (gg-move-history game)
                                  :test #'history-test?))

                 ;; Return true
                 t))
             )

      ;; Remove the piece from the game board
      (setf (svref (gg-board game) pos) 0)

      ;; Remove the piece from the hash key
      (setf (gg-board-hash game)
            (bit-xor (gg-board-hash game)
                     (svref (svref *zobrist-vectors* player) pos)))

      ;; Remove the piece from its group
      (pop (group-pieces group))

      (cond
        ;; If necessary seperate groups
        ((< 1 (svref move 2))
         (let ((mod-groups (list group nil)) 
               (new-groups nil)
               )
           (dotimes (i (- (svref move 2) 1))
             ;; Get the seperated group
             (setq mod-groups
                   (seperate-group! (first mod-groups) game))
             ;; Add the new-group to the list of new groups
             (push (second mod-groups) new-groups))
           ;; Push the modified onto the list of groups 
           (push (first mod-groups) new-groups)
           ;; Add the group back into opponent's groups with the 
           ;; correct ordering
           (setf (svref (gg-groups game) opponent)
                 (merge 'list
                        (svref (gg-groups game) opponent)
                        (sort new-groups #'group-order?)
                        #'group-order?
                        )))
         )

        ;; If the group has more pieces  
        ((< 0 (length (group-pieces group)))
         ;; Update and add back to groups
         (calc-area! group) 
         (update-group! group game)
         (setf (svref (gg-groups game) opponent)
               (merge 'list
                      (svref (gg-groups game) opponent)
                      (list group)
                      #'group-order?
                      ))
         ))
      )))

;;  PLAY-MOVE!
;; ------------------------------
;;  Basic wrapper for DO-MOVE!
(defun play-move! (game row col)
  (do-move! game (row-col->pos row col)))

;;  LEGAL-MOVE?
;; -------------------------------
(defmacro legal-move? (game pos)
    `(= 0 (svref (gg-board ,game) ,pos)))

(defmacro random-policy (game &optional (nn nil))
  `(let ((moves nil)
         (rand 0)
         (score 0)
         )
     (dotimes (i 1000000)
       (setq moves (legal-moves ,game))
       (setq rand (svref moves (random (length moves))))
       (do-move! ,game rand)
       (when (game-over? ,game)
         (return)
         ))

     (setq score (- (svref (gg-subtotals ,game) *black*)
                    (svref (gg-subtotals ,game) *white*)))
     (cond 
       ;; White wins in case of a tie
       ((= score 0)
        (setq score -1))
       ;; If black won
       ((> score 0)
        (setq score (sqrt (abs score))))
       ;; If White won
       (t 
         (setq score (* -1 (sqrt (abs score)))))
       )

     score))

(defmacro default-policy (game &optional (nn nil))
  `(let ((moves nil)
         (move 0)
         (score 0)
         )
     (dotimes (i 1000000)
       (setq moves (legal-moves ,game))
       (setq move (annalyze-move ,nn (gg-board ,game) moves (gg-whose-turn? ,game)))
       (do-move! ,game move)
       (when (game-over? ,game)
         (return)))

     (setq score (- (svref (gg-subtotals ,game) *black*)
                    (svref (gg-subtotals ,game) *white*)))
     (cond 

       ;; If White won
       ((< score 0)
         (setq score (* -1 (sqrt (abs score)))))
       ;; If black won
       ((> score 0)
        (setq score (sqrt (abs score))))
       ;; White wins in case of a tie
       (t (setq score -1))
       )

     ;(format t "End Score: ~$~%" (svref (gg-subtotals ,game) *black*))
     score))


;;  LEGAL-MOVES? : GAME 
;; -----------------------------
;;  INPUT:  GAME, A go game struct
;;  OUTPUT: A list of legal moves 
(defun legal-moves (game &optional (fast? t))
  (let ((legal-moves (list ))  ; Passing is always legal
        (valid-moves (list *board-size*))
        (player (gg-whose-turn? game))
        (moves nil) 
        (board (gg-board game))
        )
    (if fast? 
      ;; At the opening only allow decent opening moves
      (cond
        ;; If the game has just begun
        ((> 4 (length (gg-move-history game)))
         (dolist (pos *opening-moves*)
           (when (legal-move? game pos)
             (push pos valid-moves)
             )
           ))

        ;; More lenient in the mid game
        ((> 20 (length (gg-move-history game)))
         (dotimes (row (- *board-length* 1))
           (when (> row 0)
             (dotimes (col (- *board-length* 1))
               (when (and (> col 0) 
                          (= 0 (svref board 
                                      (row-col->pos row col))))
                 (push (row-col->pos row col) moves))))))


        (t (dotimes (pos *board-size*)
             (when (= 0 (svref board pos))
               (push pos moves))))
        )

      ;; Allow all moves
      (dotimes (pos *board-size*)
        (when (= 0 (svref board pos))
          (push pos moves))))

    ;; Check for suicidal play, not allowed under Chinese or
    ;; Japanese rules so it's not allowed here.
    (dolist (pos moves)
        ;; If there is a space adjacent to the move, it's not suicidal
        (cond 
          ((or (= 0 (check-board pos board *check-left*))
               (= 0 (check-board pos board *check-right*))
               (= 0 (check-board pos board *check-above*))
               (= 0 (check-board pos board *check-below*)))

           ;; Add the move to legal-moves
           (push pos valid-moves))

          ;; Or if the move connects to a group 
          ;; Check that the group has more than one liberty 
          ;; (i.e. a group that wont be captured by the move)
          ((and (= (+ 1 player) (check-board pos board *check-left*))
                (> 1 (group-liberties (find-and-return-group (- pos 1) game))))
           (push pos valid-moves))

          ((and (= (+ 1 player) (check-board pos board *check-right*))
                (> 1 (group-liberties (find-and-return-group (+ pos 1) game))))
           (push pos valid-moves))

          ((and (= (+ 1 player) (check-board pos board *check-above*))
                (> 1 (group-liberties (find-and-return-group (- pos *board-length*) game))))
           (push pos valid-moves))

          ((and (= (+ 1 player) (check-board pos board *check-below*))
                (> 1 (group-liberties (find-and-return-group (+ pos *board-length*) game))))
           (push pos valid-moves))))

    ;; If necessary...
    (if (and (gg-ko? game) 
             (< 3 (length (gg-board-history game))))
      ;; Check for for infringement of the Ko rule
      (unless 
      (format t "Ko Check!~%")
        (dolist (move valid-moves)
                ;; Passing is always an options
                (if (= move *board-size*)
                  (push move legal-moves)
                  ;; Check that the board is not returning to 
                  ;; the state prior to your opponents last
                  ;; move. (Ko Rule)

                  ;; Playing with fire
                  (let ((new-board nil)
                        (old-board nil))

                    ;; Get the old board
                    (setq old-board (first (gg-board-history game)))

                    ;; Mod Game
                    (do-move! game move)
                    ;; Get board
                    (setq new-board (copy-vector (gg-board game)))
                    ;; Unmod game 
                    (undo-move! game)
                    (unless (equal-board? new-board old-board)
                      (push move legal-moves))))
                )

        ;; Return legal moves
        (make-array (length legal-moves) :initial-contents legal-moves))

      ;; Otherwise return the valid moves
      (make-array (length valid-moves) :initial-contents valid-moves))))
