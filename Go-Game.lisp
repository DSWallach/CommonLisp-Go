;; =================================
;;   CMPU-365, Spring 2017
;;   Go Implemental
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


(defun find-pos (row col)
  (+ (* col *board-length*) row)) 

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
  (subtotals (vector 0 0))
  (move-history nil))

;;  PRINT-GO
;; ----------------------------
;;  Print function for the GO-GAME struct
(defun print-go (game str depth &optional (verbose? nil))
  (declare (ignore depth))
  (let ((board (gg-board game))
        (evals (gg-subtotals game))
        (whose-turn? (gg-whose-turn? game)))
    (format str "   0 1 2 3 4 5 6 7 8~%")
    (format str "  -------------------~%")
    (dotimes (row *board-length*)
      (format str "~A  " row)
      (dotimes (col *board-length*)
        (let ((p (svref board (find-pos col row))))
          (cond 
            ((= 0 p) (format str "- "))
            ((= 1 p) (format str "X "))
            ((= 2 p) (format str "o ")))))
      (format str "~%"))
    (format str "  -------------------~%")
    (format str "             Black    White  ~%")
    (format str "Est. Score:    ~A       ~A~%"
            (svref evals *black*)
            (svref evals *white*))
    (format str "Current Val: ~A, Whose Turn ~A~%" 
            (- (svref evals whose-turn?)
               (svref evals (- 1 whose-turn?)))
            whose-turn?))
  (when verbose?
    (format str "~%Black Groups:~%~A" (svref (gg-groups game) *black*)) 
    (format str "~%White Groups:~%~A~%" (svref (gg-groups game) *white*)))) 

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
  (territory 0))

(defun print-group (group str depth)
  (declare (ignore depth))
  (format str "~%Alive?: ~A~%" (group-alive? group))
  (format str "Pieces: ~A~%" (group-pieces group))
  (format str "Area: ~A~%" (group-area group))
  (format str "Territory: ~A~%" (group-territory group)))

(defun init-group (row col)
  (let ((terr 4))
    (labels ((pos-min 
               (pos)
               (if (= 0 pos)
                 (when (setq terr (- terr 1)) pos)
                 (- pos 1)))
             (pos-max 
               (pos)
               (if (= *board-length* pos)
                 (when (setq terr (- terr 1)) pos)
                 (+ pos 1))))
      (let ((min-row (pos-min row))
            (min-col (pos-min col))
            (max-row (pos-max row))
            (max-col (pos-max col))
            (posi (find-pos row col)))
        (make-group :pieces (list posi) 
                    :area (vector min-row min-col max-row max-col)
                    :territory terr)))))

;;  CALC-AREA
;; ----------------------
(defun calc-area (area row col)
  ;; When it's in the area of the group
  (cond
    ;; If it's smaller than the min row 
    ((< row (svref area *min-row*))
     ;; If it's smaller than the min column
     (when (< col (svref area *min-col*))
       ;; Update the min column
       (setf (svref area *min-col*) col))
     ;; Update the min row
     (setf (svref area *min-row*) row))

    ;; Else if it's larger than the max row
    ((> row (svref area *max-row*))
     ;; If it's larger than the max column
     (when (> col (svref area *max-col*))
       ;; Update column
       (setf (svref area *max-col*) col))
     ;; Update row
     (setf (svref area *max-row*) row)))
  ;; Return area
  area)

;;  CHECK-GROUP?
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
                       ((>= (* (- *board-length* 1) *board-length*) piece) nil)
                       ;; Check if there is space to below 
                       ((= 0 (svref board (- piece *board-length*))) t)
                       ;; Otherwise return nil
                       (t nil))))
      (dolist (p (group-pieces group))
        ;; If there is space next to any piece
        (when (or (check-left? p)
                  (check-right? p)
                  (check-above? p)
                  (check-below? p))

          ;; Return T
          (return-from check-group? t)))

      ;; Otherwise return false
      nil)))

;;  CAPTURE-GROUP!
;; ---------------------------------
;;  INPUTS
;;  SIDE-EFFECT: Destructively modify the game state by 
;;          capturing GROUP
(defun capture-group! (group game)
  (format t "Capture group: ~A ~%" group)
  (let ((opponent (- 1 (gg-whose-turn? game))))
    ;; Remove the groups from the opponent's groups
    (setf (svref (gg-groups game) opponent) 
          (delete group (svref (gg-groups game) opponent)))
    ;; Add them to player's captures
    (push group 
          (svref (gg-captures game) (- 1 opponent)))
    ;; Remove the pieces from the board
   (dolist (p (group-pieces group)) 
     (setf (svref (gg-board game) p) 0))))

;;  CALC-TERRITORY
;; -------------------------------------
;; Calculate the area of the square with 
;; dimensions defined by area
(defun calc-territory (area)
  (* (- (svref area *max-col*) (svref area *min-col*))
     (- (svref area *max-row*) (svref area *min-row*)))) 

;;  FIND-ADD-GROUP
;; ----------------------
;;  INPUTS
;;  OUTPUS
(defun find-add-group (game row col)
  (format t "Find-Add-Group~%")
  (let* ((new-group nil)
        (player (gg-whose-turn? game))
        (player-groups (svref (gg-groups game) player)))
    ;; Recursive subroutine to locate and retrive the 
    ;; group the move at (row, col) should be added to 
    (labels ((update-group 
               (group)
               ;; Destructively modify the group
               (push (find-pos row col) (group-pieces group))
               (setf (group-area group) (calc-area (group-area group) row col))
               (setf (group-territory group) (calc-territory (group-area group))))

             ;; Find the group the piece should beling to if it exists
             (check-groups 
               (index)
               (when (< index (length player-groups))
                 ;; Search for an existing group it could fit in
                 (let* ((group (nth index player-groups))
                        (row-min (abs (- row (svref (group-area group) 0))))
                        (col-min (abs (- col (svref (group-area group) 1))))
                        (row-max (abs (- row (svref (group-area group) 2))))
                        (col-max (abs (- col (svref (group-area group) 3)))))

                   (if 
                     ;; When it's in the area of the group
                     (and (>= 2 row-min)
                           (>= 2 col-min)
                           (>= 2 row-max)
                           (>= 2 col-max))
                      ;; Remove the group and return
                      (when
                        (setq new-group group)
                        (setf (svref (gg-groups game) player) (delete new-group player-groups))
                        new-group)

                      ;; Otherwise check next group
                      (check-groups (+ index 1)))))))

      ;; If there are no groups 
      (if (eq nil (svref (gg-groups game) player)) 
        ;; Make a list of groups containing the current move
        (setf (svref (gg-groups game) player) (list (init-group row col)))
        ;; Otherwise check the groups of the player whose turn it is
          ;; If a group was found
          (if (check-groups 0) 
            (when 
              ;; Update the group
              (update-group new-group)
              ;; Add it back at the front of the groups list
              (push new-group (svref (gg-groups game) player)))
            ;; Otherwise make a new group
            (push (init-group row col) (svref (gg-groups game) player)))))))

;;  EVAL-SUBTOTALS!
;; ------------------------
;;  INPUT: GAME, a GO-GAME struct
;;  SIDE EFFECT: A modified game struct
;;      updated with the most recent score-estimates
(defun eval-subtotals! (game)
 (format t "Evaluate subtotals~%")
  (let ((b-score 0)
        (w-score (gg-komi game)) 
        (b-captures (svref (gg-captures game) *black*))
        (w-captures (svref (gg-captures game) *white*))
        (b-groups (svref (gg-groups game) *black*))
        (w-groups (svref (gg-groups game) *white*))
        )

    ;; Calc black's score
    (dolist (group b-groups)
      (setq b-score (+ b-score (group-territory group))))
    (dolist (capd b-captures)
      (setq b-score (+ b-score (length (group-pieces capd)))))

    ;; Calc white's score
    (dolist (group w-groups)
      (setq w-score (+ w-score (group-territory group))))
    (dolist (capd w-captures)
      (setq w-score (+ w-score (length (group-pieces capd)))))
    
    ;; Update the game struct
    (setf (gg-subtotals game) (vector b-score w-score))))

;;  SHOW-GO
;; ------------------
;; Convenient function
(defun show-go (game) 
  (print-go game t nil t))

;;  PULL-PIECE!  -- used by CREATE-SET-NEW-PIECE! and DO-MOVE!
;; ---------------------------------------------------------------
;;  INPUTS:  GAME, a GO-GAME struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Removes given piece from the board.
;;  NOTE:  Removing piece from the board does not affect the
;;         values of its ROW and COL fields.  (See PUT-PIECE!.)

(defun pull-piece! (game row col)
  (setf (svref (gg-board game) (find-pos row col)) 0))

;;  PUT-PIECE!  -- used by UNDO-MOVE!
;; -------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Restores given piece to the board at the
;;    location specified by its ROW and COL fields.

(defun put-piece! (game player row col)
  (setf (svref (gg-board game) (find-pos row col)) (+ 1 player))
  (find-add-group game row col)
  (dolist (group (svref (gg-groups game) (- 1 (gg-whose-turn? game))))
    (unless (check-group? (gg-board game) group)
      (capture-group! group game)))
  (eval-subtotals! game))

;;  PLAY-MOVE!
;; --------------------------------------
;;  Mostly a wrapper for PUT-PIECE! also
;;  updates the value of GG-WHOSE-TURN?
(defun play-move! (game row col)
  (let ((player (gg-whose-turn? game)))
    (format t "~%Play at row: ~A, Col: ~A, Pos: ~A. Player: ~A~%" row col (find-pos row col) player)
    (put-piece! game player row col)
    (setf (gg-whose-turn? game) (- 1 player))
    (show-go game)
    ))

;;  LEGAL-MOVE?
;; -----------------------------
;;  INPUT:  GAME, A go game struct
;;          ROW, The row of the move to check 
;;          COL, The column of the move to check
;;  OUTPUT: A boolean value indicating if the move is legal
(defun legal-move? (game row col)
  (when (= 0 (svref (gg-board game) (find-pos row col))) t))

;; Like legal move but uses the array position rather than 
;; row & col
(defun fast-legal-move? (game pos)
  (when (= 0 (svref (gg-board game) pos)) t))

;;  LEGAL-MOVES
(defun legal-moves (game)
  (let ((moves ()))
    ;; Check each row
    (dotimes (row *board-length*)
      ;; Check each col
      (dotimes (col *board-length*)
        (when (legal-move? game row col)
          (append moves (list (vector row col))))))
    moves))

(defun fast-legal-moves (game)
  (let ((moves ()))
    ;; Check each row
    ;; Check each col
    (dotimes (pos *board-size*)
      (when (fast-legal-move? game pos)
        (push pos moves)))
    moves))

(defun test-caps ()
  (let ((new-g (init-game)))
    (play-move! new-g 1 1)
    (play-move! new-g 2 1)
    (play-move! new-g 1 0)
    (play-move! new-g 2 2)
    (play-move! new-g 0 1)
    (play-move! new-g 1 2) 
    (play-move! new-g 0 2)
    (play-move! new-g 0 3)
    (play-move! new-g 2 0) 
    (play-move! new-g 3 0)
    (play-move! new-g 8 8) 
    (play-move! new-g 7 7)
    (play-move! new-g 0 8)
    (play-move! new-g 0 0)
    ))
