;; =================================
;;   CMPU-365, Spring 2017
;;   Go Implementation
;; =================================

;;  COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

;; Tell the copiler to speed things up
(eval-when (compile)
  (declaim (optimize (speed 0) (safety 3) (space 0) (debug 3))))

;;  GLOBAL CONSTANTS

;;  The players
(defconstant *black* 0)
(defconstant *white* 1)
(defconstant *group-dist* 1)
(defconstant *board-length* 9)
(defconstant *board-size* (* *board-length* *board-length*)) 

(defun cl (filename)
  ;;  COMPILER-FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t) 
  (compile-file filename :verbose nil)
  (load filename :verbose nil)
  t
  )

(defconstant *min-row* 0)
(defconstant *min-col* 1)
(defconstant *max-row* 2)
(defconstant *max-col* 3)

;; Load Alpha/Beta AI
(cl "alpha-beta-go")

(defun find-pos (row col)
  (+ (* row *board-length*) col)) 

(defun find-row-col (pos)
  (let ((col  (mod pos *board-length*))
        (row (floor (/ pos *board-length*)))
        )
    (vector row col)))

;;;;; GO-GAME STRUCT/FUNCS 
;;  GO-GAME struct
(defstruct (go-game (:print-function print-go)
                    (:conc-name gg-))
  ;; The board is a simple vector. Positions on the
  ;; board are referenced using the go-position function 
  (board (make-array *board-size* :initial-element 0))
  ;; Groups captured by each player
  ;; necessary to be able to destructively undo moves
  (captures (vector () ()))
  ;; Vector with pointers to the lists containing Black
  ;; and white groups
  (groups (vector () ()))
  ;; White's pieces, .....
  ;; Same as for chess
  (whose-turn? *black*)
  ;; When true get-legal-moves will check for Ko situations
  (atari? nil)
  (komi 0)
  ;; Flag for ko
  (ko? nil)
  (subtotals (vector 0 0))
  ;; List of vectors #(a b c) where
  ;; a == row played at
  ;; b == col played at
  ;; c == number of groups captured by the move
  (move-history nil)
  (game-history nil) 
  )

;;  DEEP-COPY-LIST : L(ist) COPY-FUNC
;; ------------------------------
;;  Creates a deep copy of a list
(defun deep-copy-list (l copy-func)
  (let ((nl ())
        )
    (dolist (el l)
      (push (funcall copy-func el) nl))
    (reverse nl)))

;;  DEEP-COPY-GO : GAME
;; ---------------------------------
;;  Creates a deep copy of a go game struct
;;  Doesn't dopy copy game history
(defun deep-copy-go (game)
  (let ((b-caps (deep-copy-list (svref (gg-captures game) *black*) 'deep-copy-group))
        (w-caps (deep-copy-list (svref (gg-captures game) *white*) 'deep-copy-group))
        (b-groups (deep-copy-list (svref (gg-groups game) *black*) 'deep-copy-group))
        (w-groups (deep-copy-list (svref (gg-groups game) *white*) 'deep-copy-group))
        (b-subs (svref (gg-subtotals game) *black*))
        (w-subs (svref (gg-subtotals game) *white*))
        )
    (make-go-game :board (copy-seq (gg-board game)) 
                  :captures (vector b-caps w-caps)
                  :groups (vector b-groups w-groups)
                  :whose-turn? (gg-whose-turn? game)
                  :atari? (gg-atari? game)
                  :komi (gg-komi game)
                  :ko? (gg-ko? game)
                  :subtotals (vector b-subs w-subs)
                  :move-history (deep-copy-list (gg-move-history game) 'copy-seq)
                  )))

;;  EVAL-FUNC : GAME
;; ------------------------------------
;;  Static evaluation function
(defun eval-func (game)
  (- (svref (gg-subtotals game) (gg-whose-turn? game))
     (svref (gg-subtotals game) (- 1 (gg-whose-turn? game)))))

;;  FIND-GROUP : POS GAME
;; -------------------------------------
;;  Used by PRINT-GO to find the group for each piece
(defun find-group (pos game)
  (let ((piece (svref (gg-board game) pos))
        )
    (if (< 0 piece)
        (let* ((player (- piece 1)) 
              (groups (svref (gg-groups game) player)))
          (dolist (group groups)
            (when (find pos (group-pieces group))
              (return-from find-group 
                           (group-territory group)))))
    '-)))

;;  PRINT-GO : GAME STR DEPTH &op VERBOSE? GROUPS? 
;; ----------------------------
;;  Print function for the GO-GAME struct
(defun print-go (game str depth &optional (verbose? t) (groups? nil))
  (declare (ignore depth))
  (let ((board (gg-board game))
        (evals (gg-subtotals game))
        (player (gg-whose-turn? game)))
    (format str "~% ======== TURN[~A] ========~%" (length (gg-move-history game)))
    (format str "    0  1  2  3  4  5  6  7  8~%")
    (format str "  -----------------------------~%")
    (dotimes (row *board-length*)
      (format str "~A  " row)
      (dotimes (col *board-length*)
        (let* ((p (svref board (find-pos row col)))
               )
          (if (= 0 p) 
            (format str " - ")
            (if verbose? 
              (let ((pos (find-group (find-pos row col) game))
                    )
                ;; Black
                (cond
                  ((= 1 p) (if (or (< 9 pos) (< pos 0))
                             (format str " ~A" pos)
                             (format str " ~A " pos)))
                  ;; White
                  ((= 2 p) (if (or (< 9 pos) (< pos 0))
                             (format str "~A'" pos)
                             (format str " ~A'" pos)))))

              (cond 
                ((= 1 p) (format str " x "))
                ((= 2 p) (format str " o ")))))))
      (format str "~%"))
    (format str "  -----------------------------~%")
    (format str "             Black    White  ~%")
    (format str "Est. Score:    ~A       ~A~%"
            (svref evals *black*)
            (svref evals *white*))
    (format str "Current Val: ~A, Whose Turn ~A, Game Over? ~A~%" 
            (eval-func game)
            player
            (game-over? game)))
  (when groups?
    (format str "~%Black Groups:~%~A" (svref (gg-groups game) *black*)) 
    (format str "~%Black Captures:~%~A" (svref (gg-captures game) *black*)) 
    (format str "~%White Groups:~%~A" (svref (gg-groups game) *white*))
    (format str "~%White Captures:~%~A" (svref (gg-captures game) *white*))
    (format str "~%Game Length: ~A, " (length (gg-game-history game)))
    (format str "Move history~%~A~%" (gg-move-history game)) 
    )) 

;;  EQUAL-GO?
;; -----------------------
(defun equal-go? (game0 game1)
    (let ((board0 (gg-board game0))
          (board1 (gg-board game1)))
      (dotimes (i *board-size*)
        (when (not (= (svref board0 i) (svref board1 i)))
          (return-from equal-go? nil)))      t))

(defun equal-board? (board0 board1)
  (dotimes (i *board-size*)
    (when (not (= (svref board0 i) (svref board1 i)))
      (return-from equal-board? nil)))
  t)

;;  GAME-OVER? : GAME
;; -------------------------------
(defun game-over? (game)
  (if (or (> (length (gg-move-history game)) *board-size*) 
          (and (> (length (gg-move-history game)) 2) 
               (= (svref (first (gg-move-history game))0)
                  (svref (second (gg-move-history game))0))))

    t
    nil))

;;  INIT-GAME
;; ---------------------------------------
;;  INPUTS:  HANDICAP, an integer representing the number 
;;           of pieces to be set by the black player 
;;           before the game begins. A value of 0
;;           will have start the game with a komi of 0
;;           effectively a handicap for the black player.
;;
;;           KOMI, the number of points given to the white 
;;           player at the beginning of non-handicap games 
;;           to balance out black getting to place the first move
;;   
;;  OUTPUS: A go-game struct. If no handicap 
;;      or komi is provided, komi is set at
;;      6.5 for komi. Standard under most 
;;      go rules. Since the effect of the ".5" is
;;      to ensure white wins if there otherwise would 
;;      a tie. I'm using 6 and setting it as a rule of
;;      game evaluation function that white wins if there
;;      is a tie.
(defun init-game (&optional (handicap -1) (komi 6))
  (when (= handicap -1)
    (make-go-game :komi komi)))

;;;;; GROUP STRUCT/FUNCS
;;  GROUP
;; ----------------------------
;;  A group of pieces enclosing some amount
;;  of territory in the game of Go. The 
;;  territory of each of a player's groups
;;  is added to their score estimate
(defstruct (group (:print-function print-group))
  (alive? nil)
  (pieces ())
  (area (vector 0 0 0 0))
  (liberties 0)
  (territory 0))

;;  DEEP-COPY-GROUP
;; ------------------------------
;;  Creates a deep copy of a group
(defun deep-copy-group (group)
  (make-group :alive? (group-alive? group)
              :pieces (copy-seq (group-pieces group))
              :area (copy-seq (group-area group))
              :territory (group-territory group)))

;;  PRINT-GROUP : GROUP STR DEPTH 
;; ---------------------------------
;;  Prints put a group 
(defun print-group (group str depth)
  (declare (ignore depth))
  (format str "{ ~A," (group-alive? group))
  (format str "~A," (group-pieces group))
  (format str "~A," (group-area group))
  (format str "~A," (group-liberties group))
  (format str "~A} " (group-territory group)))

;;  INIT-GROUP : ROW COL BOARD
;; --------------------------------------
;;  Initializes a group with a piece at ROW, COL
(defun init-group (row col)
  (make-group :pieces (list (find-pos row col)) 
              :area (vector row col row col) 
              :territory 0))

;;  CALC-AREA: GROUP 
;; ----------------------------------
(defun calc-area (group)
  (let ((min-row *board-length*)
        (min-col *board-length*)
        (max-row 0)
        (max-col 0)
        (vec nil))
    (dolist (pos (group-pieces group))
      (when (setq vec (find-row-col pos))
        (cond ; Update min/max row
          ((< (svref vec 0) min-row)
           (setq min-row (svref vec 0)))
          ((> (svref vec 0) max-row)
           (setq max-row (svref vec 0)))
          )
        (cond ; Update min/max col
          ((< (svref vec 1) min-col)
           (setq min-col (svref vec 1)))
          ((> (svref vec 1) max-col)
           (setq max-col (svref vec 1)))
          )))
    ;; Return the new area
    (vector min-row min-col max-row max-col)))

;;  CALC-NEW-AREA: AREA ROW COL
;; ----------------------
(defun calc-new-area (area row col)
  ;; Check row
  (cond
    ;; If it's smaller than the min row 
    ((< row (svref area *min-row*))
     ;; Update the min row
       (setf (svref area *min-row*) row))

    ;; Else if it's larger than the max row
    ((> row (svref area *max-row*))
     ;; Update row
       (setf (svref area *max-row*) row))
    )
  ;; Check Col
  (cond
    ;; If it's smaller than the min column
    ((< col (svref area *min-col*))
     ;; Update the min column
       (setf (svref area *min-col*) col))

    ;; Check max col
    ((> col (svref area *max-col*))
     ;; Update column
       (setf (svref area *max-col*) col))
    )
  ;; Return area
  area)

;;  CALC-TERRITORY: AREA BOARD PLAYER
;; -------------------------------------
;; Calculate the area of the square with 
;; dimensions defined by area. Subtract one for the
;; space taken up by the piece.
(defun calc-territory (area board player)
  ;; +1 accounts for subtracting 1 for the space of the piece
  (let ((terr (+ 1 (* (- (svref area *max-col*) (svref area *min-col*))
                 (- (svref area *max-row*) (svref area *min-row*)))))
        (min-row (svref area 0))
        (min-col (svref area 1))
        (max-row (svref area 2))
        (max-col (svref area 3))
        (opponent (- 1 player))
        (territory 0)
        (player? nil)
        (total 0)
        )

    ;; For the area of the group 
    (dotimes (row *board-length*)
      (when (and (<= min-row row) (>= max-row row))
        ;; When a wall is reached 
        (when (or (= 0 row) (= (- *board-length* 1) row)) 
          ;; Set the player's flag
          (setq player? t))

        ;; Check each column 
        (dotimes (col *board-length*)
          ;; When a wall is reached 
          (when (or (=(- *board-length* 1) col) (= 0 col))
            ;; Set the player's flag
            (setq player? t))
          ;; Check each row 
          (when (and (<= min-col col) (>= max-col col))
            ;; Check the board
            (case (svref board (find-pos row col)) 
              ;; If it's player's piece, set flag
              ;; If the player's flag is set
              (player (when player?  
                        ;; Add territory to the total
                        (setq total (+ total territory))
                        ;; Reset territory
                        (setq territory 0))
                      ;; Set player flag
                      (setq player? t))

              ;; If it's an opponent's piece, remove flag, clear territory
              (opponent (setq player? nil)
                        (setq territory 0))
              ;; If the space is open
              ;; If the player's flag is set and 
              (0 (when player? (setq territory (+ 1 territory)))))))

          ;; If the player;s flag is set
          (when player? 
            ;; Update total
            (setq total (+ total territory))
            ;; Reset territory
            (setq territory 0))))

  total))

;;  CHECK-GROUP? : BOARD GROUP
;; -------------------------
;;  INPUT: BOARD, a board from a go game struct
;;         GROUP, a group of pieces
;;  OUTPUT: T, if GROUP is alive
;;          nil, otherwise
(defun check-group? (board group)
  ;; If the group is definitely alive
  (if (group-alive? group)
    t ; Return T
    ;; Otherwise check it's life
    (labels 
      ((check-left? (piece)
                    (cond
                      ;; Check if it's on the left edge
                      ((= 0 (mod piece *board-length*)) nil)
                      ;; Check if there is space to the left
                      ((= 0 (svref board (- piece 1))) t)
                      ;; Otherwise return nil
                      (t nil)))
       (check-right? (piece)
                     (cond
                       ;; Check if it's on the right edge
                       ((= (- *board-length* 1) (mod piece *board-length*)) nil)
                       ;; Check if there is space to the right
                       ((= 0 (svref board (+ piece 1))) t)
                       ;; Otherwise return nil
                       (t nil)))
       (check-above? (piece)
                     (cond
                       ;; Check if it's on the top edge
                       ((> *board-length* piece) nil)
                       ;; Check if there is space above 
                       ((= 0 (svref board (- piece *board-length*))) t)
                       ;; Otherwise return nil
                       (t nil)))
       (check-below? (piece)
                     (cond
                       ;; Check if it's on the bottom edge
                       ((<= (* (- *board-length* 1) *board-length*) piece) nil)
                       ;; Check if there is space to below 
                       ((= 0 (svref board (+ piece *board-length*))) t)
                       ;; Otherwise return nil
                       (t nil))))
      (let ((libs 0)
            )
        (dolist (p (group-pieces group))
          ;; Calculate the group's liberties
          (when (check-left? p)
            (setq libs (+ libs 1)))

          (when (check-right? p)
            (setq libs (+ libs 1)))

          (when (check-above? p)
            (setq libs (+ libs 1)))
          (when (check-below? p)
            (setq libs (+ libs 1))))

        ;; Update the group's liberties
        (setf (group-liberties group) libs) 
        ;; If the group has liberties it's alive
        (if (< 0 (group-liberties group))
          t
          nil)
          ))))

;;  CAPTURE-GROUP! : GROUP GAME PLAYER
;; ---------------------------------
;;  INPUTS
;;  SIDE-EFFECT: Destructively modify the game state by 
;;          capturing GROUP
(defun capture-group! (group game player)
  ;;(format t "Capture group: ~A ~%" group)
  (let ((opponent (- 1 player)))
    ;; Remove the groups from the opponent's groups
    (setf (svref (gg-groups game) opponent) 
          (delete group (svref (gg-groups game) opponent)))
    ;; Add them to player's captures
    (push group 
          (svref (gg-captures game) (- 1 opponent)))
    ;; Remove the pieces from the board
   (dolist (p (group-pieces group)) 
     (setf (svref (gg-board game) p) 0))))

;;  MERGE-GROUPS! : GAME GROUP-ONE GROUP-TWO
;; -------------------------------------------
;;  Merge two groups into one larger group
(defun merge-groups! (group-one group-two)
  (let ((min-row (svref (group-area group-two) 0))
        (min-col (svref (group-area group-two) 1))
        (max-row (svref (group-area group-two) 2))
        (max-col (svref (group-area group-two) 3))
        )

    ;; Push all the pieces of group-two onto group-one
    (dolist (piece (group-pieces group-two))
      (push piece (group-pieces group-one)))

    ;; Update Area where necessary
    (when (< min-row (svref (group-area group-one) *min-row*))
      (setf (svref (group-area group-one) *min-row*) min-row))

    (when (< min-col (svref (group-area group-one) *min-col*))
      (setf (svref (group-area group-one) *min-col*) min-col))

    (when (> max-row (svref (group-area group-one) *max-row*))
      (setf (svref (group-area group-one) *max-row*) max-row))

    (when (> max-col (svref (group-area group-one) *max-col*))
      (setf (svref (group-area group-one) *max-col*) max-col))
    ))

;;  UPDATE-GROUP! : GAME GROUP
;; -----------------------------------------------
;;  Update a group's liberties and territory
(defun update-group! (game group)

    ;; Recompute Territory
    (setf (group-territory group) 
          (calc-territory (group-area group) (gg-board game) (gg-whose-turn? game)))

    ;; Recompute liberties 
    (check-group? (gg-board game) group))

;;  FIND-ADD-GROUP! : GAME ROW COL
;; ----------------------
;;  Find any groups the move at row col should belong to
;;  If there are multiple groups merge them into one
(defun find-add-group! (game row col)
  (let* ((new-group nil)
        (player (gg-whose-turn? game))
        (connected-groups ())
        (player-groups (svref (gg-groups game) player)))
    
    ;; Recursive subroutines to locate and retrive the 
    ;; group the move at (row, col) should be added to 
    (labels 
      ((add-piece! ; (GROUP), Add the piece at row col to group
         (group)
         ;; Destructively modify the group
         (push (find-pos row col) (group-pieces group))
         (setf (group-area group) (calc-new-area (group-area group) row col)))

       (find-group ; (GROUP), Check it the piece at row col, is connected to GROUP
         (group)
         (let ((vec nil)
               )
           (dolist (p (group-pieces group))
             ;; Get the row/col of the piece
             (setq vec (find-row-col p))
             ;; If the new piece at (row, col) is to the right or left of the piece
             (when (or (and (= (svref vec 0) row)
                            (or (= (svref vec 1) (- col 1))
                                (= (svref vec 1) (+ col 1))))
                       ;; Or if it is above or below the piece
                       (and (= (svref vec 1) col)
                            (or (= (svref vec 0) (- row 1))
                                (= (svref vec 0) (+ row 1)))))
               ;; Return true
               (return-from find-group t)))))

       (check-groups ; (INDEX), Find the group the piece should belong to if it exists
         (index)
         ;; Do for each group 
         (if (< index (length player-groups))
           ;;(format t "Index:~A, P-Groups:~A~%" index player-groups)
           ;; Search for an existing group it could fit in
           (let ((group (nth index player-groups))
                 )
             (cond 
               ;; When it's in the area of the group
               ((and (>= (+ row *group-dist*) (svref (group-area group) *min-row*))
                     (>= (+ col *group-dist*) (svref (group-area group) *min-col*))
                     (<= (- row *group-dist*) (svref (group-area group) *max-row*))
                     (<= (- col *group-dist*) (svref (group-area group) *max-col*)))

                ;; If there is a connection to the group
                (when (find-group group)
                  ;; Push it onto the connected groups
                  (push (deep-copy-group group) connected-groups)
                  ;; Remove it from the groups list for player
                  (setf (svref (gg-groups game) player) 
                        (delete group (svref (gg-groups game) player))))

                ;; Check the next group
                (check-groups (+ index 1))
                )

               ;; Otherwise check next group
               (t (check-groups (+ index 1)))
               )
           ;; Return t
           t))
       ))

      ;; If there are no groups 
      (if (eq nil (svref (gg-groups game) player)) 
        (when (setq new-group (init-group row col))
          ;; Update liberties
          (check-group? (gg-board game) new-group)
          ;; Make a list of groups containing the current move
          (setf (svref (gg-groups game) player) (list new-group)))
        ;; Otherwise check the groups of the player whose turn it is
        (when (check-groups 0)
          (cond
            ;; If no group was found
            ;; Make a new group
            ((= 0 (length connected-groups)) 
             (setq new-group (init-group row col))
             (check-group? (gg-board game) new-group)
             (push new-group 
                   (svref (gg-groups game) player))
             )
             ;; If one group was found  
             ((= 1 (length connected-groups)) 
              ;; Get the group
              (setq new-group (pop connected-groups))
              ;; Add the piece 
              (add-piece! new-group) 
              ;; Update the fully merged group
              (update-group! game new-group)
              ;; Add it back at the front of the groups list
              (push new-group 
                    (svref (gg-groups game) player))
              )
            ;; Otherwise get the first group
            (t 
              (setq new-group (pop connected-groups))            
              ;; Update it with the new piece 
              (add-piece! new-group)
              ;; Merge it with the remaining groups 
              (dolist (group connected-groups)
                (merge-groups! new-group group)) 
              ;; Update the fully merged group
              (update-group! game new-group)
              ;; Push it back onto the players groups
              (push new-group (svref (gg-groups game) player))
              )))))))

;;  GROUP-REMOVE! : GROUP BOARD PLAYER
;; -------------------------------
;;  INPUTS: GROUP, a group struct
;;          POS, The position of the piece to be removed
(defun group-remove! (group board player)

  ;; Remove the most recent piece
  (pop (group-pieces group))
  ;; Unless there are no more pieces in the group
  (unless (null (group-pieces group))

    (when ; Otherwise
      (setf (group-area group) (calc-area group)) 
      (setf (group-territory group) (calc-territory (group-area group) board player))

      ;; Return the modified group
      group)))

;;;;; GAME PLAYING
;;  EVAL-SUBTOTALS! : GAME
;; ------------------------
;;  INPUT: GAME, a GO-GAME struct
;;  SIDE EFFECT: A modified game struct
;;      updated with the most recent score-estimates
(defun eval-subtotals! (game)
  (let ((b-score 0)
        (w-score (gg-komi game)) 
        (b-captures (svref (gg-captures game) *black*))
        (w-captures (svref (gg-captures game) *white*))
        (b-groups (svref (gg-groups game) *black*))
        (w-groups (svref (gg-groups game) *white*))
        )

    ;; Calc black's score
    (dolist (group b-groups)
      ;; Add 1/2 of the liberties and all the territory 
      ;; to the score
      (setq b-score (+ b-score 
                       (group-territory group) 
                       (floor (/ (group-liberties group) 2)))))
    (dolist (capd b-captures)
      (setq b-score (+ b-score (length (group-pieces capd)))))

    ;; Calc white's score
    (dolist (group w-groups)
      (setq w-score (+ w-score 
                       (group-territory group) 
                       (floor (/ (group-liberties group) 2)))))

      (dolist (capd w-captures)
        (setq w-score (+ w-score (length (group-pieces capd)))))
      ;; Update the game struct
      (setf (gg-subtotals game) (vector b-score w-score))))

;;  PULL-PIECE!  -- used by UNDO-MOVE!
;; ---------------------------------------------------------------
;;  INPUTS:  GAME, a GO-GAME struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Removes given piece from the board.
;;  NOTE:  Removing piece from the board does not affect the
;;         values of its ROW and COL fields.  (See PUT-PIECE!.)

(defun pull-piece! (game player pos)
  ;;(format t "Pull player: ~A, pos: ~A~%" player pos)
  ;; Remove the piece from the game board
  (setf (svref (gg-board game) pos) 0)

  (dolist (group (svref (gg-groups game) player))
    ;; When the group contains the piece
    (when (= pos (first (group-pieces group)))
      ;; If the group has more than one piece
      (if (< 1 (length (group-pieces group)))
      ;; Save the group
      (let ((new-group group)) 
        ;; Remove it from groups
        (setf 
          (svref (gg-groups game) player)
          (delete group (svref (gg-groups game) player)))
        ;; Modify it 
        (setq new-group (group-remove! group (gg-board game) (gg-whose-turn? game)))
        ;; Push it back on groups if it's not empty
        (when (group-pieces new-group) 
          (push new-group (svref (gg-groups game) player)))
        ;; Return
        (return-from pull-piece! t))

    ;; Otherwise delete the group    
    (return-from pull-piece! 
                 (setf (svref (gg-groups game) player)
                 (delete group (svref (gg-groups game) player))))))))

;;  PUT-PIECE!  -- used by DO-MOVE!
;; -------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Restores given piece to the board at the
;;    location specified by its ROW and COL fields. 

(defun put-piece! (game player pos)
  (setf (svref (gg-board game) pos) (+ 1 player))
  (let ((vec (find-row-col pos)))
    (find-add-group! game (svref vec 0) (svref vec 1)))
  ;; Return true
  t)

;;  DO-MOVE! : GAME POS
;; --------------------------------------
;;  Works like the chess-solns function of the same name
(defun do-move! (game pos)
    
  ;; Before doing anything else, push the
  ;; previous board state onto the game history
  (push (deep-copy-go game) (gg-game-history game))

  (let ((captured 0)
        (player (gg-whose-turn? game)))

    ;; If the move is a pass
    (cond
      ((= *board-size* pos)
       ;; Change which players turn it is 
       ;; Push the pass
       (push (vector pos 0) (gg-move-history game))
       )

      ;; Otherwise Put their piece at pos 
      ((put-piece! game player pos)

        ;; Check if any groups were captured
        (dolist (group (svref (gg-groups game) (- 1 player)))
          ;; If a group was
          (unless (check-group? (gg-board game) group)
            ;; Increment the capture flag
            (setq captured (+ 1 captured))
            ;; Capture the group
            (capture-group! group game player))
          (when (= 1 (length (group-pieces group)))
            (setf (gg-ko? game) t)))
        ;; Check if the move would kill the current group
        (unless (check-group? (gg-board game) 
                              (first (svref (gg-groups game) player)))

          (capture-group! (first (svref (gg-groups game) player))
                          game (- 1 player)))

        (when (= captured 0)
          (setf (gg-ko? game) nil))

        ;; Evaluate each players score
        (eval-subtotals! game)

        (push (vector pos captured) (gg-move-history game))
        ))

    ;; Change turn
    (setf (gg-whose-turn? game) (- 1 player))))

;;  PLAY-MOVE!
;; ------------------------------
;;  Basic wrapper for DO-MOVE!
(defun play-move! (game row col)
  (do-move! game (find-pos row col)))

;;  UNDO-MOVE! : GAME
;; ----------------------------------------
;;  Undo the most recently played move
(defun undo-move! (game)
  (let* ((history (gg-game-history game))
        (new-game (pop history))
        )
    ;; Update the game properties
    (setf (gg-board game) (gg-board new-game))
    (setf (gg-groups game) (gg-groups new-game))
    (setf (gg-captures game) (gg-captures new-game))
    (setf (gg-whose-turn? game) (gg-whose-turn? new-game))
    (setf (gg-atari? game) (gg-atari? new-game))
    (setf (gg-ko? game) (gg-ko? new-game))
    (setf (gg-subtotals game) (gg-subtotals new-game))
    (setf (gg-move-history game) (gg-move-history new-game))
    (setf (gg-game-history game) history)
    ))

;;  LEGAL-MOVE? : GAME POS
;; -----------------------------
;;  INPUT:  GAME, A go game struct
;;          ROW, The row of the move to check 
;;          COL, The column of the move to check
;;  OUTPUT: A boolean value indicating if the move is legal

(defun legal-moves (game)
  (let ((legal-moves (list *board-size*))
        (moves (list *board-size*))
        )
    ;; Check each row
    ;; Check each col
    (dotimes (pos *board-size*)
      (when (= 0 (svref (gg-board game) pos))
        (push pos moves)))

    (when (gg-ko? game)
      (dolist (move moves)

        ;; When a group was captured
        (if (and (< 1 (length (gg-move-history game))) 
                 (svref (first (gg-move-history game)) 1))
          ;; Check that the board is not returning to 
          ;; the state prior to your opponents last 
          ;; move. (Ko Rule)

          ;; Playing with fire
          (let ((new-board nil)
                (old-board nil))
            ;; Mod Game
            (do-move! game move)
            ;; Get board
            (setq new-board (gg-board game))
            ;; Unmod game 
            (undo-move! game)

            ;; Get the old board
            (setq old-board (gg-board (first (gg-game-history game))))
            (unless (equal-board? new-board old-board)
              (push move legal-moves)))
          (push move legal-moves)))
      (return-from legal-moves legal-moves))

    moves))

;;;;; TESTING
;; Testing CALC-TERRITORY
(defun test-terr ()
  (let ((new-g (init-game))
        )
  (print-go new-g t nil t nil )
    (play-move! new-g 0 4)
    (play-move! new-g 8 4)
    
  (print-go new-g t nil t nil )
    (play-move! new-g 1 4)
    (play-move! new-g 7 4)

  (print-go new-g t nil t  nil )
    (play-move! new-g 2 4)
    (play-move! new-g 6 4)

  (print-go new-g t nil t nil )
    (play-move! new-g 3 4)
    (play-move! new-g 5 4)

  (print-go new-g t nil t nil )
    (play-move! new-g 3 3)
    (play-move! new-g 5 3)

  (print-go new-g t nil t nil)
    (play-move! new-g 3 2)
    (play-move! new-g 5 2)

  (print-go new-g t nil t nil)
    (play-move! new-g 3 1)
    (play-move! new-g 5 1)

  (print-go new-g t nil t nil)
    (play-move! new-g 3 0)
    (play-move! new-g 5 0)
    
  (print-go new-g t nil t t)
  ))

;; Testing FIND-ADD-GROUP
(defun test-grouping-one ()
  (let ((new-g (init-game))
        )
    (play-move! new-g 0 4)
    (do-move! new-g *board-size*)

    (play-move! new-g 1 4)
    (do-move! new-g *board-size*)

    (play-move! new-g 2 4)
    (do-move! new-g *board-size*)

    (play-move! new-g 3 4)
    (do-move! new-g *board-size*)

    (play-move! new-g 3 3)
    (do-move! new-g *board-size*)
    (print-go new-g t nil t t)

    (play-move! new-g 3 2)
    (do-move! new-g *board-size*)
    (print-go new-g t nil t t)

    (play-move! new-g 3 1)
    (do-move! new-g *board-size*)
    (print-go new-g t nil t t)

    (play-move! new-g 3 0)
    (do-move! new-g *board-size*)

    (print-go new-g t nil t t)
    ))

;; Test GROUP-MERGE
(defun test-grouping-two ()
  (let ((new-g (init-game))
        )
    (play-move! new-g 0 4)
    (do-move! new-g *board-size*)
    (print-go new-g t nil nil t)

    (play-move! new-g 1 4)
    (do-move! new-g *board-size*)
    (print-go new-g t nil nil t)

    (play-move! new-g 2 4)
    (do-move! new-g *board-size*)
    (print-go new-g t nil nil t)

    (format t "____ NEW GROUP ____~%")
    (play-move! new-g 3 3)
    (do-move! new-g *board-size*)
    (print-go new-g t nil nil t)

    (play-move! new-g 3 2)
    (do-move! new-g *board-size*)
    (print-go new-g t nil nil t)

    (play-move! new-g 3 1)
    (do-move! new-g *board-size*)
    (print-go new-g t nil nil t)

    (play-move! new-g 3 0)
    (do-move! new-g *board-size*)
    (print-go new-g t nil t t)

    (format t "____ MERGE GROUP ____~%")
    (play-move! new-g 3 4)
    (do-move! new-g *board-size*)
    (print-go new-g t nil t t)
    ))

;; For testing Capture
(defun test-caps ()
  (let ((new-g (init-game)))
    (play-move! new-g 1 1)
    (print-go new-g t nil t)
    (play-move! new-g 2 1)
    (play-move! new-g 1 0)
    (play-move! new-g 2 2)
    (play-move! new-g 0 1)
    (play-move! new-g 1 2) 
    (play-move! new-g 0 2)
    (play-move! new-g 0 3)
    (play-move! new-g 2 0) 
    (play-move! new-g 3 0)
    (print-go new-g t nil t)
    (play-move! new-g 8 8) 
    (print-go new-g t nil t)
    (play-move! new-g 7 7)
    (print-go new-g t nil t)
    (play-move! new-g 0 8)
    (print-go new-g t nil t)
    (play-move! new-g 0 0)
    (print-go new-g t nil t)
    (undo-move! new-g)
    (print-go new-g t nil t)
    (undo-move! new-g)
    (print-go new-g t nil t)
    (undo-move! new-g)
    (print-go new-g t nil t)
    (undo-move! new-g)
    (print-go new-g t nil t)
    (undo-move! new-g)
    (print-go new-g t nil t )
    (undo-move! new-g)
    (print-go new-g t nil t)
    (undo-move! new-g)
    (undo-move! new-g)
    (undo-move! new-g)
    (undo-move! new-g)
    (undo-move! new-g)
    (undo-move! new-g)
    (undo-move! new-g)
    (undo-move! new-g)
    (print-go new-g t nil t )
    ))

;; For testing problems with undo/do
(defun test-short ()
  (let ((new-g (init-game)))
    (do-move! new-g 11)
    (print-go new-g t nil t )
    (format t "UNDO")
    (undo-move! new-g)
    (print-go new-g t nil t )
    (format t "REDO")
    (do-move! new-g 11)
    (print-go new-g t nil t )
    (do-move! new-g 35)
    (print-go new-g t nil t )
    (format t "UNDO")
    (undo-move! new-g)
    (format t "REDO")
    (do-move! new-g 35)
    (print-go new-g t nil t )
  )) 

;; Test corner capture
(defun test-corner ()
  (let ((new-g (init-game))
        )
    (do-move! new-g 0)
    (print-go new-g t nil t nil)
    (do-move! new-g 1)
    (print-go new-g t nil t nil)
    (do-move! new-g 5)
    (print-go new-g t nil t nil)
    (do-move! new-g 9)
    (print-go new-g t nil t t)
    ))

(defun test-compute ()
  (let ((next-move nil)
        (new-g (init-game)))
    (format t "~%***** COMPUTE *****~%")
    (setq next-move (compute-move new-g 2))
    (print-go new-g t nil t )
    (format t "~%------ DOMOVE ------~%")
    (do-move! new-g next-move) 
    (print-go new-g t nil t )

    (format t "~%***** COMPUTE *****~%")
    (setq next-move (compute-move new-g 2))
    (print-go new-g t nil t )
    (format t "~%------ DOMOVE ------~%")
    (do-move! new-g next-move) 
    (print-go new-g t nil t )

    (format t "~%***** COMPUTE *****~%")
    (setq next-move (compute-move new-g 2))
    (print-go new-g t nil t )
    (format t "~%------ DOMOVE ------~%")
    (do-move! new-g next-move) 
    (print-go new-g t nil t )
    ))

(defun test-ko ()
  (let ((moves nil)
        (new-g (init-game)))
    (play-move! new-g 1 2)
    (play-move! new-g 1 1)
    (play-move! new-g 0 3) 
    (play-move! new-g 0 0)
    (play-move! new-g 0 1)
    (play-move! new-g 0 2)
    (print-go new-g t nil t t)
    (setq moves  (legal-moves new-g))
    (format t  "~%legal Moves ~A~%" moves)
    ))

(defun do-moves (game times)
  (dotimes (i times)
    (do-move! game i)
    (print-go game t nil nil)
    ))

(defun test-undo (ng)
    (do-moves ng 10))

;; TEST-BENCH : Test performance of game
(defun test-bench ()
  (time (let ((new-g (init-game))
              )
          (dotimes (i *board-size*)
            (do-move! new-g i)
            (undo-move! new-g)
            (do-move! new-g i)
            )
        (print-go new-g t nil t t)
        (setq new-g (init-game))
        (dotimes (i *board-size*)
          (do-move! new-g i)
          (do-move! new-g *board-size*))
        (print-go new-g t nil t t)
        )))
