;; =================================
;;   CMPU-365, Spring 2017
;;   Go Implementation
;; =================================

;;  COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

;; Tell the copiler to speed things up
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0))))

;;  GLOBAL CONSTANTS

;; Game Properties 
(defconstant *black* 0)
(defconstant *white* 1)
(defconstant *board-length* 9)
(defconstant *board-size* (* *board-length* 
                             *board-length*)) 

;; For compiling
(defun cl (filename)
  ;;  COMPILER-FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t) 
  (compile-file filename :verbose nil)
  (load filename :verbose nil)
  t
  )

;; Used to reference group-area
(defconstant *min-row* 0)
(defconstant *min-col* 1)
(defconstant *max-row* 2)
(defconstant *max-col* 3)

;; Used by check board
(defconstant *check-left* 0)
(defconstant *check-right* 1)
(defconstant *check-above* 2)
(defconstant *check-below* 3)

;; Load Alpha/Beta AI
(cl "alpha-beta-go")

;;  PLAY-GAME : GAME DEPTH-ONE DEPTH-TWO ONE?
;; ---------------------------------------------
;; A function for setting to A.I.'s with different
;; depths against each other. For fun.
(defun play-game (game depth-one depth-two one?) 
  (if (game-over? game)
    (unless (format t "++++++++ Game Over +++++++++~%")
      (print-go game t nil t t))
    (when (if one? 
            (do-move! game (compute-move game depth-one))
            (do-move! game (compute-move game depth-two)))
      (format t "Game State~%")
      (print-go game t nil nil nil)
      (play-game game depth-one depth-two (not one?)))))

;;  PLAY-GAME-DEBUG
;; -------------------------
;;  For debugging a game
(defun play-game-debug 
  (game depth-one depth-two one? 
        &optional (debug-depth 5) (debug? nil))
  (when (> (length (gg-move-history game))
           debug-depth)
    (setq debug? t))
  (if (game-over? game)
    (unless (format t "++++++++ Game Over +++++++++~%")
      (print-go game t nil t t))
    (when (if one? 
            (do-move! game (compute-move game depth-one) debug?)
            (do-move! game (compute-move game depth-two) debug?))
      (format t "Game State~%")
      (print-go game t nil t t)
      (when (= 64 (svref (first (gg-move-history game)) 0))
        (return-from play-game-debug game))
      (play-game-debug game depth-one depth-two 
                       (not one?) debug-depth debug?))))

(defun pg (d1 d2)
  (play-game (init-game) d1 d2 t))

(defun copy-vector (in-vec)
  (let ((out-vec (make-array (length in-vec)))
        )
    (dotimes (i (length in-vec))
      (setf (svref out-vec i) (svref in-vec i)))
    out-vec))

(defun find-pos (row col)
  (declare (type fixnum row col))
  (+ (* row *board-length*) col)) 

(defun find-row-col (pos)
  (declare (type fixnum pos))
  (let ((col  (mod pos *board-length*))
        (row (floor (/ pos *board-length*)))
        )
    (vector row col)))

;;;;; GO-GAME STRUCT/FUNCS 
;;  GO-GAME struct
(defstruct (go-game;; (:print-function print-go)
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
  (board-history nil) 
  (game-history nil)
  )

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
        (multiplier 0)
        )

    (cond 
      ((< (length (gg-move-history game)) 5)
       (setq multiplier 4))
      ((< (length (gg-move-history game)) 10)
       (setq multiplier 3))
      ((< (length (gg-move-history game)) 15)
       (setq multiplier 2))
      ((< (length (gg-move-history game)) 20)
       (setq multiplier 1))
      (t
       (setq multiplier 0))
      )
    
    ;; Calc black's score
    (dolist (group b-groups)
      ;; Add 1/2 of the liberties and all the territory 
      ;; to the score
      (setq b-score (+ b-score 
                       (group-territory group) 
                      (* multiplier (group-liberties group))
                       )))
    (dolist (capd b-captures)
      (setq b-score (+ b-score (length (group-pieces capd)))))

    ;; Calc white's score
    (dolist (group w-groups)
      (setq w-score (+ w-score 
                       (group-territory group) 
                      (* multiplier (group-liberties group))
                       )))

      (dolist (capd w-captures)
        (setq w-score (+ w-score (length (group-pieces capd)))))
      ;; Update the game struct
      (setf (gg-subtotals game) (vector b-score w-score))))

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
                  :board-history (deep-copy-list (gg-board-history game) 'copy-vector)
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
(defun print-go (game str depth 
                      &optional (verbose? nil) (groups? nil) (boards? nil))
  (declare (ignore depth))
  (let ((board (gg-board game))
        (evals (gg-subtotals game))
        (player (gg-whose-turn? game)))

    ;; Print game info
    (format str "Turn: ~A       Black    White  ~%" (length (gg-move-history game)))
    (format str "Est. Score:    ~A       ~A~%"
            (svref evals *black*)
            (svref evals *white*))
    (format str "Current Val: ~A, Whose Turn ~A, Game Over? ~A~%" 
            (eval-func game)
            player
            (game-over? game))

  ;; Print group info
  (when groups?
    (format str "~%Black Groups:~%~A" (svref (gg-groups game) *black*)) 
    (format str "~%Black Captures:~%~A" (svref (gg-captures game) *black*)) 
    (format str "~%White Groups:~%~A" (svref (gg-groups game) *white*))
    (format str "~%White Captures:~%~A" (svref (gg-captures game) *white*))
    (format str "~%Game Length: ~A, " (length (gg-board-history game)))
    (format str "Move history~%~A~%" (gg-move-history game)) 
    )

  ;; Print board history
  (when boards?
    (dotimes (i (length (gg-board-history game)))
      (format str "Board at turn ~A : ~A~%" 
              (- (length (gg-board-history game)) i)
              (nth i (gg-board-history game)))))

    ;; Print Header 
    (cond
      ((= 19 *board-length*) (format str "~%  ===="))
      ((= 9 *board-length*) (format str "~% ")))

    (dotimes (i (/ *board-length* 2))
      (format str "=="))
    (format str " TURN[~A] " (length (gg-move-history game)))
    (dotimes (i (/ *board-length* 2))
      (format str "=="))
    (cond
      ((= 19 *board-length*) (format str "  ====~%"))
      ((= 9 *board-length*) (format str "~%")))

    ;; Print column numbers
    (format str "    ")
    (dotimes (i *board-length*)
      (if (< 9 i)
        (format str "~A " i)
        (format str " ~A " i)))
    (format str "~% ")
    (format str "   ")
    (dotimes (i *board-length*)
      (if (= i (- *board-length* 1))
        (format str "--")
        (format str "---")))
    (format str "~%")

    ;; Print the board
    (dotimes (row *board-length*)
      (if (< 9 row)
        (format str "~A |" row)
        (format str " ~A |" row))
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
    
    ;; Print seperator
    (format str "    ")
    (dotimes (i *board-length*)
      (if (= i (- *board-length* 1))
        (format str "--")
        (format str "---")))
    (format str "~%")

  )) 

;;  EQUAL-GO?
;; -----------------------
(defun equal-go? (game0 game1 &optional (print-to nil))
  (let ((board-0 (gg-board game0))
        (board-1 (gg-board game1))
        (black-groups-0 (svref (gg-groups game0) *black*))
        (white-groups-0 (svref (gg-groups game0) *white*))
        (black-groups-1 (svref (gg-groups game1) *black*))
        (white-groups-1 (svref (gg-groups game1) *white*))
        (black-captures-0 (svref (gg-captures game0) *black*))
        (white-captures-0 (svref (gg-captures game0) *white*))
        (black-captures-1 (svref (gg-captures game1) *black*))
        (white-captures-1 (svref (gg-captures game1) *white*))
        (move-history-0 (gg-move-history game0))
        (move-history-1 (gg-move-history game1))
        (board-history-0 (gg-board-history game0))
        (board-history-1 (gg-board-history game1))
        )

      ;; Ensure all list are matching lengths
      (unless (and (= (length black-groups-0)
                      (length black-groups-1))
                   (= (length white-groups-0)
                      (length white-groups-1))
                   (= (length black-captures-0)
                      (length black-captures-1))
                   (= (length white-captures-0)
                      (length white-captures-1))
                   (= (length move-history-0)
                      (length move-history-1)))
        (return-from equal-go? nil))

    ;; Check the equality of the board
    (unless (equal-board? board-0 board-1)
      (format print-to "Boards are not equal~%")
      (return-from equal-go? nil))

    ;; Check the equality of black's groups (the order doesn't have to be the same)
    (when (mismatch black-groups-0 black-groups-1 :test #'equal-group?)
        (format print-to "Black groups are not equal~%~A~%~A~%"
                black-groups-0 black-groups-1)
        (return-from equal-go? nil))

    ;; Check the equality of white's groups (the order doesn't have to be the same)
    (when (mismatch white-groups-0 white-groups-1 :test #'equal-group?)
        (format print-to "white groups are not equal~%~A~%~A~%"
                white-groups-0 white-groups-1)
        (return-from equal-go? nil))

    ;; Check the equality of black's captures (the order doesn't have to be the same)
    (when (mismatch black-captures-0 black-captures-1 :test #'equal-group?)
      (format print-to "Black captures are not equal~%~A~%~A~%"
              black-captures-0 black-captures-1)
      (return-from equal-go? nil))

    ;; Check the equality of white's captures (the order doesn't have to be the same)
    (when (mismatch white-captures-0 white-captures-1 :test #'equal-group?)
      (format print-to "white captures are not equal~%~A~%~A~%"
              white-captures-0 white-captures-1)
      (return-from equal-go? nil))

    ;; Check the equality of the move history
    (when (mismatch move-history-0 move-history-1 :test #'equalp)
      (format print-to "Move Histories are not equal~% ~A~% ~A ~%"
              (mismatch move-history-0 move-history-1 :test #'equalp))
      (return-from equal-go? nil))

    ;; Check the equality of the board history
    (when (mismatch board-history-0 board-history-1 :test #'equalp)
      (format print-to "board Histories are not equal~% ~A~%"
              (mismatch board-history-0 board-history-1 :test #'equalp))
      (return-from equal-go? nil))

    ;; Check the equality of the ko-flag
    (unless (equal (gg-ko? game0) (gg-ko? game1))
      (format print-to "Game ko's are not equal. ~%~A, ~A~%" 
              (gg-ko? game0) (gg-ko? game1)))

    ;; If all tests pass return true 
    t))

;;  EQUAL-BOARD? : BOARD0 BOARD1
;; -----------------------------------
;;  Equality test for a GO-GAME board
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

;;  CHECK-ORDER? : GAME
;; -----------------------------------
;;  Check that the ordering of groups is correct.
;;  For debugging.
(defun check-order? (game)
  (let ((hist (gg-move-history game))
        (b-groups (svref (gg-groups game) *black*))
        (w-groups (svref (gg-groups game) *white*))
        (counter 0)
        )
    (dolist (group b-groups)
      (dotimes (i (length hist))
        (when (= (first (group-pieces group))
                 (svref (nth i hist) 0))
          (if (< i counter)
            (unless (format t "Black Groups out of order~%")
              (print-go game t nil t t)
              (break)
              )
            (setq counter i))
          (return))))

    (setq counter 0)

    (dolist (group w-groups)
      (dotimes (i (length hist))
        (when (= (first (group-pieces group))
                 (svref (nth i hist) 0))
          (if (< i counter)
            (unless (format t "White Groups out of order~%")
              (print-go game t nil t t)
              (break)
              )
            (setq counter i))
          (return)
          )))
    t
))

;;  CHECK-BOARD? : POS BOARD CHECK-WHERE?
;; ----------------------------------
;;  Check if the relevent space on the board is empty
(defun check-board? (pos board check-where?)
  (case check-where?
    (0 ;;*check-left* 
      (cond
        ;; Check if it's on the left edge
        ((= 0 (mod pos *board-length*)) nil)
        ;; Check if there is space to the left
        ((= 0 (svref board (- pos 1))) t)
        ;; Otherwise return nil
        (t nil)))
    (1 ;;*check-right*
      (cond
        ;; Check if it's on the right edge
        ((= (- *board-length* 1) (mod pos *board-length*)) nil)
        ;; Check if there is space to the right
        ((= 0 (svref board (+ pos 1))) t)
        ;; Otherwise return nil
        (t nil)))
    (2 ;;*check-above*
      (cond
        ;; Check if it's on the top edge
        ((> *board-length* pos) nil)
        ;; Check if there is space above 
        ((= 0 (svref board (- pos *board-length*))) t)
        ;; Otherwise return nil
        (t nil)))
    (3 ;;*check-below*
      (cond
        ;; Check if it's on the bottom edge
        ((<= (* (- *board-length* 1) *board-length*) pos) nil)
        ;; Check if there is space to below 
        ((= 0 (svref board (+ pos *board-length*))) t)
        ;; Otherwise return nil
        (t nil)))
    )
  )

;;;;; GROUP STRUCT/FUNCS
;;  GROUP
;; ----------------------------
;;  A group of pieces enclosing some amount
;;  of territory in the game of Go. The 
;;  territory of each of a player's groups
;;  is added to their score estimate
(defstruct (group (:print-function print-group)
                  )
  (alive? nil)
  (pieces ())
  (area (vector 0 0 0 0))
  (liberties 0)
  (merge-marker nil)
  (last-pos 0) ; Set when a group is captured
  (territory 0))

;;  EQUAL-GROUP? GROUP-ONE GROUP-TWO
;; ------------------------------------
;;  Equality test for two groups
(defun equal-group? (group-one group-two)
  (and  (equalp (group-pieces group-one)
                (group-pieces group-two))
        (equalp (group-area group-one)
                (group-area group-two))
        (= (group-liberties group-one)
           (group-liberties group-two))
        (= (group-territory group-one)
           (group-territory group-two))))

;;  DEEP-COPY-GROUP
;; ------------------------------
;;  Creates a deep copy of a group
(defun deep-copy-group (group)
  (make-group :alive? (group-alive? group)
              :pieces (copy-seq (group-pieces group))
              :area (copy-seq (group-area group))
              :liberties (group-liberties group)
              :merge-marker (group-merge-marker group)
              :last-pos (group-last-pos group)
              :territory (group-territory group)))

;;  PRINT-GROUP : GROUP STR DEPTH 
;; ---------------------------------
;;  Prints put a group 
(defun print-group (group str depth)
  (declare (ignore depth))
  (format str "{~A," (group-alive? group))
  (format str "~A," (group-pieces group))
  (format str "~A," (group-merge-marker group))
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

;;  CALC-AREA!: GROUP 
;; ----------------------------------
(defun calc-area! (group)
  (let ((min-row *board-length*)
        (min-col *board-length*)
        (max-row 0)
        (max-col 0)
        (vec nil))

    (dolist (pos (group-pieces group))
      (setq vec (find-row-col pos))

      ;; Update min/max row
      (when (< (svref vec 0) min-row)
        (setq min-row (svref vec 0)))
      (when (> (svref vec 0) max-row)
        (setq max-row (svref vec 0)))

      ;; Update min/max col
      (when (< (svref vec 1) min-col)
        (setq min-col (svref vec 1)))
      (when (> (svref vec 1) max-col)
        (setq max-col (svref vec 1)))
      )
    ;; Set the new area
    (setf (group-area group)
          (vector min-row min-col max-row max-col))))

;;  CALC-NEW-AREA!: GROUP ROW COL
;; ----------------------
(defun calc-new-area! (group row col)
  (let ((area (group-area group))
        )

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

    ;; Update area
    (setf (group-area group) area)
    
    
    ))

;;  CALC-TERRITORY!: GROUP BOARD PLAYER
;; -------------------------------------
;; Calculate the area of the square with 
;; dimensions defined by area. Subtract one for the
;; space taken up by the piece.
(defun calc-territory! (group board player)
  ;; +1 accounts for subtracting 1 for the space of the piece
  (let* ((area (group-area group))
        (min-row (svref area 0))
        (min-col (svref area 1))
        (max-row (svref area 2))
        (max-col (svref area 3))
        (territory 0)
        (opponent (- 1 player))
        (player? nil)
        (total 0)
        )

    ;; This variable is used as a case
    ;; for the switch (case) block
    (declare (ignore opponent))

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

    ;; Update the territory
    (setf (group-territory group) total)))

;;  CALC-LIBERTIES! : GROUP BOARD
;; -------------------------
;;  INPUT: BOARD, a board from a go game struct
;;         GROUP, a group of pieces
;;  OUTPUT: T, if GROUP is alive
;;          nil, otherwise
(defun calc-liberties! (group board)
  ;; If the group is definitely alive
  (if (group-alive? group)
    t ; Return T
    ;; Otherwise check it's life
    (let ((libs 0)
          )
      (dolist (p (group-pieces group))
        ;; Calculate the group's liberties
        (when (check-board? p board *check-left*)
          (setq libs (+ libs 1)))

        (when (check-board? p board *check-right*)
          (setq libs (+ libs 1)))

        (when (check-board? p board *check-above*)
          (setq libs (+ libs 1)))

        (when (check-board? p board *check-below*)
          (setq libs (+ libs 1))))

      ;; Update the group's liberties
      (setf (group-liberties group) libs)
      )))

;;  CAPTURE-GROUP! : GROUP GAME
;; ---------------------------------
;;  INPUTS
;;  SIDE-EFFECT: Destructively modify the game state by 
;;          capturing GROUP
(defun capture-group! (group game)
  (let* ((player (gg-whose-turn? game))
         (opponent (- 1 player))
         )
    
    ;; Remove the groups from the opponent's groups
    (setf (svref (gg-groups game) opponent) 
          (remove group (svref (gg-groups game) opponent)))

    ;; Add them to player's captures
    (push group (svref (gg-captures game) player))

    ;; Remove the pieces from the board
   (dolist (p (group-pieces group)) 
     (setf (svref (gg-board game) p) 0))
   ))

;;  MERGE-GROUPS! : GAME GROUP-ONE GROUP-TWO
;; -------------------------------------------
;;  Merge two groups into one larger group
(defun merge-groups! (group-one group-two)
  (let ((min-row (svref (group-area group-two) *min-row*))
        (min-col (svref (group-area group-two) *min-col*))
        (max-row (svref (group-area group-two) *max-row*))
        (max-col (svref (group-area group-two) *max-col*))
        (merger nil)
        )
    
    ;; Create the merger from the first piece of a group
    ;; and its merge markers
    (setq merger (vector (first (group-pieces group-two))
                         (group-merge-marker group-two)))

    ;; Push the first piece from the group onto the merge-marker
    (push merger 
          (group-merge-marker group-one))

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

;;  SEPERATE-GROUP! : GROUP GAME
;; -------------------------------
;;  Reverse MERGE-GROUPS!, for use by UNDO-MOVE!
(defun seperate-group! (group game)
  (let* ((new-group (make-group))
        (piece nil)
        (mark-vec (pop (group-merge-marker group)))
        (mark (svref mark-vec 0)) 
        (merge-marker (svref mark-vec 1))
        )

    ;; Seperate-group! shouldn't be called if there are no 
    ;; merged groups.
  ;;  (unless mark-vec
   ;;   (print-go game t nil nil t nil)
    ;;  (break))

    (labels ((get-pieces 
               (times)
               ;; Remove all the the pieces from the most recent merge 
               ;; and add them to the new group
               (dotimes (i times)
                 (setq piece (pop (group-pieces group)))

                 ;; If the piece is equal to the mark
                 (cond 
                   ((= mark piece) 
                    ;; Add it to new-group
                    (push piece (group-pieces new-group))
                    ;; Return
                    (return-from get-pieces t))
                   ;; Otherwise
                   (t
                     ;; Push the piece
                     (push piece (group-pieces new-group))))))
             )

      ;; Get the pieces
      (get-pieces (length (group-pieces group)))

      ;; Return the merge marker
      (setf (group-merge-marker new-group)
            merge-marker)
      
      ;; Calculate areas
      (calc-area! group)
      (calc-area! new-group)

      ;; Update groups
      (update-group! group game)
      (update-group! new-group game)

      ;; Return the new group 
      new-group)))

;;  UPDATE-GROUP! : GAME GROUP
;; -----------------------------------------------
;;  Update a group's liberties and territory
(defun update-group! (group game)
  ;; Recompure Area
;;  (calc-area! group)
  ;; Recompute liberties 
  (calc-liberties! group (gg-board game))
  ;; Recompute Territory
  (calc-territory! group (gg-board game) (gg-whose-turn? game))
  )

;;  GROUP-REMOVE! : GROUP POS GAME
;; -------------------------------
;;  INPUTS: GROUP, a group struct
;;          POS, The position of the piece to be removed
(defun group-remove! (group pos game)

  ;; Remove the most recent piece
  (setf (group-pieces group)
        (remove pos (group-pieces group)))

  ;; Unless there are no more pieces in the group
  (if (< 0 (length (group-pieces group)))
    ;; Update the group and return it
    (when (update-group! group game)
      group)
    nil ; Otherwise return nil
    ))

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
         (vec (find-row-col pos))
         (row (svref vec 0))
         (col (svref vec 1))
         (player (gg-whose-turn? game))
         (connected-groups ())
         (group-dist 0)
         (player-groups (svref (gg-groups game) player)))
    (cond 
      ((< (length (gg-move-history game)) 5)
       (setq group-dist 5))
      ((< (length (gg-move-history game)) 10)
       (setq group-dist 4))
      ((< (length (gg-move-history game)) 15)
       (setq group-dist 3))
      ((< (length (gg-move-history game)) 20)
       (setq group-dist 2))
      (t
       (setq group-dist 1))
      )

    ;; Recursive subroutines to locate and retrive the 
    ;; group the move at (row, col) should be added to 
    (labels 
      ((add-piece! ; (GROUP), Add the piece at row col to group
         (group)
         ;; Destructively modify the group
         (push (find-pos row col) 
               (group-pieces group))
         ;; Update area
         (calc-new-area! group row col)
         ;; Update group 
         (update-group! group game))

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
           ;; Search for an existing group it could fit in
           (let ((group (nth index player-groups))
                 )
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

                 ;;(format debug? "Merge Group~%")
                  ;; Push it onto the connected groups
                  (push group connected-groups)

                 (setf (svref (gg-groups game) player)
                      (remove group
                              (svref (gg-groups game) player)
                              :test #'equal-group?))
                  )

                ;; Check the next group
                (check-groups (+ index 1))
           ;; Return t
           t)))
       )

      ;; Put the piece on the board 
      (setf (svref (gg-board game) pos) (+ 1 player))
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
        (when (check-groups 0)
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
              ;; Remove all groups from the groups list for player
            ; (setf (svref (gg-groups game) player) 
            ;       (remove-if (lambda (x) 
            ;                    (find x 
            ;                          connected-groups
            ;                          :test #'equal-group?))
            ;                  player-groups
            ;                  ))

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
                (merge-groups! new-group group)) 

              ;; Update it with the new piece 
              (add-piece! new-group)

              ;;(format debug? " ++++++++++++++++++++  ~% New Group ~A~%" new-group)
              ;; Push it back onto the players groups
              (push new-group (svref (gg-groups game) player))

              ;; Return the number of groups merged
              (+ (length connected-groups) 1))))))))

;;  DO-MOVE! : GAME POS
;; --------------------------------------
;;  Works like the chess-solns function of the same name
(defun do-move! (game pos &optional (debug? nil))
  ;;(format debug? "Do Move ~A ~%" pos)
  (let* ((captured 0)
         (board (gg-board game))
         (player (gg-whose-turn? game))
         (opponent (- 1 player))
         (groups (svref (gg-groups game) (- 1 player)))
         )

    ;; Function for finding and capturing groups
    (labels (
             (groups-capture
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
                     (push (nth i groups) capture-list))
                   )
                 
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
    ;; Push game onto game history (For debug)
    (when debug? (push (deep-copy-go game) 
                       (gg-game-history game)))

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

   (check-order? game) 
    )))

;;  UNDO-MOVE! : GAME
;; ----------------------------------------
;;  Undo the most recently played move
(defun undo-move! (game &optional (debug? nil))
  (let* ((captured 0)
         (move (pop (gg-move-history game)))
         (pos (svref move 0))
         (player (gg-whose-turn? game))
         (opponent (- 1 player))
         (deep-copy nil) 
         )
    ;;(format debug? "Undo Move ~A~%" move)
    (labels (
             ;; Test if a play at POSI matches a move from the
             ;; game's move history
             (history-test? 
               (posi move)
               (= posi (svref move 0)))
             ;; Predicate for determining the ordering of groups
             ;; based on the move history
             (group-order? 
               (group-one group-two)
               ;;(format debug? "ONe: ~A~% Two: ~A~%" group-one group-two)
               ;; When group-one was modified more recently that group-two 
               (when (< (position (first (group-pieces group-one)) 
                                  (gg-move-history game)
                                  :test #'history-test?) 
                        (position (first (group-pieces group-two)) 
                                  (gg-move-history game)
                                  :test #'history-test?))

                 ;; Return true
                 t))
             )

      (when debug? 
        (setq deep-copy (pop (gg-game-history game))) 
        (check-order? deep-copy)
        )
      ;; Delete the previous board 
      (pop (gg-board-history game))

      ;; Unless the previous move was a pass
      (unless (= *board-size* pos)

        ;; Remove the piece from the board and from player's groups
        (pull-piece! game move) 

      (when (= captured 0)
        (setf (gg-ko? game) nil))

      (cond 
        ;; If the last move was a suicide
        ((= -1 (svref move 1))
         (let ((group nil)
               )
           ;; Remove the piece from your opponent's captures
           (setq group (pop (svref (gg-captures game) player)))

           ;; Add the group back to player's groups
           (setf (svref (gg-groups game) opponent)
                 (merge 'list
                        (list group)
                        (svref (gg-groups game) opponent)
                        #'group-order?))

           ;; Return pieces to the board except for the last move
           (dolist (p (group-pieces group))
             (unless (= p (svref move 0))
               (setf (svref (gg-board game) p) 
                     (+ 1 opponent))))))

        ;; If necessary return captured groups
        ((< 0 (svref move 1))
         ;;(format debug? "Return captured groups ~%")
         (let ((group nil)
               )

           ;; Return each captured group
           (dotimes (i (svref move 1))

             (setq group (pop (svref (gg-captures game) opponent)))

             ;; Add the group's pieces to the board
             (dolist (p (group-pieces group))
               (setf (svref (gg-board game) p) (+ 1 player)))

             ;; Add the group back in it's previous position
             (setf (svref (gg-groups game) player)
                   ;; Append the groups that came before group
                   (append (subseq (svref (gg-groups game) player) 
                                   0 (group-last-pos group))  
                           (list group) ; A list containing group
                           ;; The groups that came after group
                           (subseq (svref (gg-groups game) player) 
                                   (group-last-pos group)))))))
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
      (eval-subtotals! game))

    ;; Change turn
    (setf (gg-whose-turn? game) opponent)

    ;; Compare to the deep-copy board
    ;; break if there's a difference
    (when debug?  
      (unless (equal-go? game deep-copy)
        ;;(format debug? "Undo doesn't match copy: Copy ~% ~A Game ~% ~A ~%" deep-copy game)
        (break)))

   )))

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
         (group (pop (svref (gg-groups game) opponent)))
         )

    (labels (
             (history-test? (pos move)
                            (= pos (svref move 0)))
             (group-order? 
               (group-one group-two)
               ;; So the compiler doesn't complain
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

      ;; Remove the piece from its group
      (pop (group-pieces group))

      (cond
        ;; If necessary seperate groups
        ((< 1 (svref move 2))
         (let (
               (new-group nil) 
               (new-groups nil)
               )

           (dotimes (i (- (svref move 2) 1))

             ;; Get the seperated group
             (setq new-group 
                   (seperate-group! group game))

             ;; Add the new-group to the list of new groups
             (push new-group new-groups)
             )

           ;; Push the modified onto the list of groups 
           (push group new-groups)

           ;; Add the group back into opponent's groups with the 
           ;; correct ordering
           (setf (svref (gg-groups game) opponent)
                 (merge 'list 
                        (svref (gg-groups game) opponent)
                        (sort new-groups #'group-order?)
                        #'group-order?
                        ))
           ))

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
         )
        )
      )))

;;  PLAY-MOVE!
;; ------------------------------
;;  Basic wrapper for DO-MOVE!
(defun play-move! (game row col)
  (do-move! game (find-pos row col)))

;;  LEGAL-MOVES? : GAME 
;; -----------------------------
;;  INPUT:  GAME, A go game struct
;;  OUTPUT: A list of legal moves 

(defun legal-moves (game &optional (fast? t))
  (let ((legal-moves (list *board-size*)); Passing is always legal
        (valid-moves nil)
        (moves nil) 
        (board (gg-board game))
        )
    ;;
    (if fast? 
      ;; At the opening only allow decent opening moves
      (cond 
        ;; If the game has just begun
        ((> 8 (length (gg-move-history game)))
         (dotimes (row (- *board-length* 2))
           (when (> row 1)
             (dotimes (col (- *board-length* 2))
               (when (and (> col 1) 
                          (= 0 (svref (gg-board game) (find-pos row col))))
                 (push (find-pos row col) moves))))))

        ;; More lenient in the mid game
        ((> 16 (length (gg-move-history game)))
         (dotimes (row (- *board-length* 1))
           (when (> row 0)
             (dotimes (col (- *board-length* 1))
               (when (and (> col 0) 
                          (= 0 (svref (gg-board game) (find-pos row col))))
                 (push (find-pos row col) moves))))))


        (t (dotimes (pos *board-size*)
             (when (= 0 (svref (gg-board game) pos))
               (push pos moves))))
        )

      ;; Allow all moves
      (dotimes (pos *board-size*)
        (when (= 0 (svref (gg-board game) pos))
          (push pos moves))))

    ;; Check for suicidal play, not allowed under Chinese or
    ;; Japanese rules so it's not allowed here.
    (dolist (pos moves)
      ;; If there is a space adjacent to the move, it's not suicidal
      (if (or (check-board? pos board *check-left*)
              (check-board? pos board *check-right*)
              (check-board? pos board *check-above*)
              (check-board? pos board *check-below*))
        ;; Add the move to legal-moves
        (unless 
          (push pos valid-moves)
          )
        ;; Otherwise check if it would merge with a group
        ;; or capture a group. Making it legal
        (let ((merged (put-piece! game pos))
              (hold-libs 0)
              )
          (cond 
            ;; When the move is part of a living group
            ((< 0 (calc-liberties! (first (svref (gg-groups game)
                                                (gg-whose-turn? game)))
                                  (gg-board game)))
             ;; Restore the game state
             (setf (gg-whose-turn? game)
                   (- 1 (gg-whose-turn? game)))
             (pull-piece! game (vector pos 0 merged))
             (setf (gg-whose-turn? game)
                   (- 1 (gg-whose-turn? game)))
             ;; It's valid
             (push pos valid-moves))
            ;; Otherwise
            (t 
              ;; Check if any opponents groups are captured
              (dolist (group (svref (gg-groups game)
                                    (- 1 (gg-whose-turn? game))))
                ;; Store the previous liberties
                (setq hold-libs (group-liberties group))
                ;; Calculate new liberties
                (calc-liberties! group (gg-board game))
                ;; When a group is captured
                (when (= 0 (group-liberties group))
                  ;; The move is valid
                  (push pos valid-moves)
                  ;; Reset group's libs 
                  (setf (group-liberties group)
                        hold-libs)
                  ;; Break out of loop
                  (return)
                  )
                ;; Reset group's libs
                (setf (group-liberties group)
                      hold-libs))

              ;; Reset the game
              (setf (gg-whose-turn? game)
                    (- 1 (gg-whose-turn? game)))
              (pull-piece! game (vector pos 0 merged))
              (setf (gg-whose-turn? game)
                    (- 1 (gg-whose-turn? game)))
              )
            ))))

     ;; If necessary...
     (if (and (gg-ko? game) 
              (< 3 (length (gg-board-history game))))
       ;; Check for for infringement of the Ko rule
       (unless (dolist (move valid-moves)
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
         ;; Return legal moves
         legal-moves)

       ;; Otherwise return the valid moves
       valid-moves)))

;;;;; TESTING
;;  TEST : TESTNAME PASSED?
;; -----------------------------
;;  Testing function
(defun test (testname passed?)
  (format t "Test ~A passed? ~A~%" testname passed?))

;;; Testing Capture
;;  TEST-CORNER-CAPTURE
;; ------------------------
;;  Testing Group-Capture! 
(defun test-corner-capture ()
  (let ((new-g (init-game)))
    (play-move! new-g 0 0)
    (play-move! new-g 0 1)
    (play-move! new-g 1 1)
    (play-move! new-g 1 0)
    (test "Single-Corner-Capture" (= (length (svref (gg-captures new-g) *white*)) 1))))

;;; Testing Capture
;;  TEST-LARGE-CAPTURE
;; --------------------------------------
;; Testing the correctness of capturing a large group 
;; along the side of the board
(defun test-large-capture (&optional (verbose? nil))
  (let ((new-g (init-game))
        (black-group nil)
        )
    (play-move! new-g 0 0)
    (play-move! new-g 0 1)
    
    (play-move! new-g 1 0)
    (play-move! new-g 1 1)

    (play-move! new-g 2 0)
    (play-move! new-g 2 1)

    (play-move! new-g 3 0)
    (play-move! new-g 3 1)

    (play-move! new-g 4 0)
    (play-move! new-g 4 1)

    (play-move! new-g 5 0)
    (play-move! new-g 5 1)

    (play-move! new-g 6 0)
    (play-move! new-g 6 1)

    (play-move! new-g 7 0)
    (play-move! new-g 7 1)

    (when verbose? (print-go new-g t nil t t))
    (play-move! new-g 8 0)
    (setq black-group (first (svref (gg-groups new-g) *black*))) 

    (play-move! new-g 8 1)

    (when verbose? (print-go new-g t nil t t))
    (test "Large-Capture" 
          (equal-group? black-group (first (svref (gg-captures new-g) *white*))))
    ))

;;  TEST-BOTTOM-CAPTURE
;; ----------------------------------
(defun test-bottom-capture (&optional (verbose? nil))
    (let ((new-g (init-game))
          )
      (play-move! new-g 8 4)
      (do-move! new-g *board-size*)

      (play-move! new-g 7 4)
      (do-move! new-g *board-size*)

      (play-move! new-g 7 5)
      (play-move! new-g 8 5)

      (play-move! new-g 7 6)
      (play-move! new-g 8 6)

      (play-move! new-g 8 7)
      (play-move! new-g 8 6)

      (play-move! new-g 7 7)
      (play-move! new-g 8 8)

      (when verbose? (print-go new-g t nil t t))
      (test "Bottom-Capture" 
            (and (= 1 
                    (length (svref (gg-captures new-g) 
                                   *black*)))
                 (= 2 
                    (length (group-pieces (first (svref (gg-captures new-g) 
                                                        *black*)))))))


      ))

;;  TEST-SURROUND-CAPTURE
;; -------------------------
;;  Test the capture of a group surrounded
(defun test-surround-capture (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (play-move! new-g 6 4)
    (play-move! new-g 5 6)

    (play-move! new-g 6 3)
    (play-move! new-g 6 5)

    (play-move! new-g 5 5)
    (play-move! new-g 5 4)

    (play-move! new-g 6 6)
    (play-move! new-g 4 5)

    (when verbose? (print-go new-g t nil t t))
    (test "Surround-Capture" (= (length (svref (gg-captures new-g) *white*)) 
                                     1))))

;;  TEST-TWO-CAPTURES
;; ---------------------------
;;  Testing the correctness of capturing two groups with
;;  one move
(defun test-two-captures (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (play-move! new-g 3 0)
    (play-move! new-g 2 0)

    (play-move! new-g 5 0)
    (play-move! new-g 6 0)

    (do-move! new-g *board-size*)
    (play-move! new-g 3 1)

    (do-move! new-g *board-size*)
    (play-move! new-g 5 1)
    (when verbose? (print-go new-g t nil t t))

    (play-move! new-g 4 1)
    (when verbose? (print-go new-g t nil t t))
    (play-move! new-g 4 0) ; Capture
    (when verbose? (print-go new-g t nil t t))

    (test "Two-Capture" (and (= 1 (length (svref (gg-groups new-g) *black*)))
                                  (= 2 (length (svref (gg-captures new-g) *white*)))))
))

;;; Testing Groups
;;  TEST-CORNER-TERRITORY
;; ---------------------------------------
;;  Testing territory calculation
(defun test-corner-territory()
  (let ((new-g (init-game)))
    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (test "Corner-Territory" (= (group-territory (first (svref (gg-groups new-g) *black*))) 4))))

;;  TEST-CORNER-AREA
;; ----------------------------------
;;  Testing the correctness of the area calculation for a group
(defun test-corner-area()
  (let ((new-g (init-game))
        (correct? t)
        )
    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (unless (equalp (group-area (first (svref (gg-groups new-g) *black*)))
                    (vector (- *board-length* 3) 
                            (- *board-length* 1) 
                            (- *board-length* 3) 
                            (- *board-length* 1)))
      (setq correct? nil))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (unless (equalp (group-area (first (svref (gg-groups new-g) *black*)))
                    (vector (- *board-length* 3) 
                            (- *board-length* 2) 
                            (- *board-length* 3) 
                            (- *board-length* 1)))
      (setq correct? nil))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (unless (equalp (group-area (first (svref (gg-groups new-g) *black*)))
                    (vector (- *board-length* 3) 
                            (- *board-length* 3) 
                            (- *board-length* 3) 
                            (- *board-length* 1)))
      (setq correct? nil))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (unless (equalp (group-area (first (svref (gg-groups new-g) *black*)))
                    (vector (- *board-length* 3) 
                            (- *board-length* 3) 
                            (- *board-length* 2) 
                            (- *board-length* 1)))
      (setq correct? nil))

    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (unless (equalp (group-area (first (svref (gg-groups new-g) *black*)))
                    (vector (- *board-length* 3) 
                            (- *board-length* 3) 
                            (- *board-length* 1) 
                            (- *board-length* 1)))
      (setq correct? nil))
    (test "Corner-Area" correct?))) 

;;  TEST-CORNER-LIBERTIES
;; -----------------------------
(defun test-corner-liberties (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (do-move! new-g 0)
    (do-move! new-g *board-size*)

    (do-move! new-g 1)
    (do-move! new-g *board-size*)

    (do-move! new-g 2)
    (do-move! new-g *board-size*)

    (do-move! new-g 3)
    (play-move! new-g 1 3)
    
    (when verbose? (print-go new-g t nil t t))
    
    (test "Corner-Liberties" (= 4 (group-liberties 
                                    (first (svref (gg-groups new-g) 
                                                  *black*)))))))

;;  TEST-GROUPING-ONE : OP (VERBOSE?)
;; --------------------------------------------
;;  Testing the correctness of how pieces are grouped
(defun test-grouping-one (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (play-move! new-g 0 2)
    
    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (play-move! new-g 1 2)

    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (play-move! new-g 2 2)

    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (play-move! new-g 2 1)

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (play-move! new-g 2 0)
    (when verbose? (print-go new-g t nil t t))
    ;; each player should have only one group
    (test "Grouping-One" (and (= 1 (length (svref (gg-groups new-g) *white*)))
                               (= 1 (length (svref (gg-groups new-g) *black*)))))))

;;  TEST-GROUPING-TWO 
;; ---------------------------------
;;  Testing the correctness of how pieces are grouped
(defun test-grouping-two (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (play-move! new-g 1 2)

    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (play-move! new-g 0 2)
    
    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (play-move! new-g 2 1)

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (play-move! new-g 2 0)

    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (play-move! new-g 2 2)
    (when verbose? (print-go new-g t nil t t))
    ;; each player should have only one group
    (test "Grouping-Two" (and (= 1 (length (svref (gg-groups new-g) *white*)))
                               (= 1 (length (svref (gg-groups new-g) *black*)))))))

;;  TEST-GROUPING-THREE 
;; ---------------------------------
;;  Testing the correctness of how pieces are grouped
(defun test-grouping-three (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (play-move! new-g 1 2)

    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (play-move! new-g 0 2)
    
    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (play-move! new-g 2 1)

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (play-move! new-g 2 0)

    (play-move! new-g (- *board-length* 4) (- *board-length* 3))
    (play-move! new-g 2 3)

    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (play-move! new-g 2 2)

    (when verbose? (print-go new-g t nil t t))

    ;; each player should have only one group
    (test "Grouping-Three" (and (= 1 (length (svref (gg-groups new-g) *white*)))
                               (= 1 (length (svref (gg-groups new-g) *black*)))))))

;;  TEST-GROUPING-FOUR 
;; ---------------------------------
;;  Testing the correctness of how pieces are grouped
(defun test-grouping-four (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (play-move! new-g 1 2)

    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (play-move! new-g 0 2)
    
    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (play-move! new-g 2 1)

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (play-move! new-g 2 0)

    (play-move! new-g (- *board-length* 4) (- *board-length* 3))
    (play-move! new-g 2 3)

    (play-move! new-g (- *board-length* 3) (- *board-length* 4))
    (play-move! new-g 3 2)

    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (play-move! new-g 2 2)

    (when verbose? (print-go new-g t nil t t))

    ;; each player should have only one group
    (test "Grouping-Four" (and (= 1 (length (svref (gg-groups new-g) *white*)))
                               (= 1 (length (svref (gg-groups new-g) *black*)))))))

;;  TEST-GROUPING-NEAR-OPPONENT
;; ------------------------------
(defun test-grouping-near-opponent (&optional (verbose? nil))
  (let ((new-g (init-game))
        )
    (do-move! new-g 20)
    (do-move! new-g 21)

    (do-move! new-g 22)
    (do-move! new-g 23)

    (do-move! new-g 24)
    (do-move! new-g 29)

    (do-move! new-g 30)
    (do-move! new-g 31)

    (do-move! new-g 12)
    (do-move! new-g 39)

    (when verbose? (print-go new-g t nil t t))

    (do-move! new-g 21 verbose?)

    (when verbose? (print-go new-g t nil t t))

    (test "Grouping-Near-Opponent" 
          (= 2 (length (svref (gg-groups new-g) 
                              *black*))))))

;;; Testing Undo
;;  TEST-UNDO-CAPTURE
;; ---------------------------------
;;  Testing the correctness of undo after a group is captured
(defun test-undo-capture (&optional (verbose? nil))
  (let ((new-g (init-game))
        (old-game nil)
        )
    (play-move! new-g 0 0)
    (play-move! new-g 1 0)
                         
    (play-move! new-g 0 1)
    (play-move! new-g 1 1)
                         
    (play-move! new-g 0 2)
    (play-move! new-g 1 2)
                         
    (play-move! new-g 0 3)
    (play-move! new-g 1 3)
                         
    (play-move! new-g 0 4)
    (play-move! new-g 1 4)
                         
    (play-move! new-g 0 5)
    (play-move! new-g 1 5)
                         
    (play-move! new-g 0 6)
    (play-move! new-g 1 6)
                         
    (play-move! new-g 0 7)
    (play-move! new-g 1 7)

    (play-move! new-g 0 8)
    (setq old-game (deep-copy-go new-g))

    (when verbose? (print-go new-g t nil t t))
    (play-move! new-g 1 8)
    (undo-move! new-g)
    (when verbose? (print-go new-g t nil t t))

    (test "Undo-Capture" (equal-go? new-g old-game verbose?))))

;;  TEST-UNDO-SURROUND-CAPTURE
;; -------------------------
;;  Test the capture of a group surrounded
(defun test-undo-surround-capture (&optional (verbose? nil))
  (let ((new-g (init-game))
        (old-game nil)
        )
    (play-move! new-g 6 4)
    (play-move! new-g 5 6)

    (play-move! new-g 6 3)
    (play-move! new-g 6 5)

    (play-move! new-g 5 5)
    (play-move! new-g 5 4)

    (play-move! new-g 6 6)
    (when verbose? (print-go new-g t nil t t))
    (setq old-game (deep-copy-go new-g))
    (play-move! new-g 4 5)

    (when verbose? (print-go new-g t nil t t))
    (undo-move! new-g)

    (when verbose? (print-go new-g t nil t t))
    (test "Undo-Surround-Capture" (equal-go? new-g old-game verbose?))))

;;  TEST-UNDO-TWO-CAPTURES
;; ---------------------------
;;  Testing the correctness of capturing two groups with
;;  one move
(defun test-undo-two-captures (&optional (verbose? nil))
  (let ((new-g (init-game))
        (old-game nil)
        )
    (play-move! new-g 3 0)
    (play-move! new-g 2 0)

    (play-move! new-g 5 0)
    (play-move! new-g 6 0)

    (do-move! new-g *board-size*)
    (play-move! new-g 3 1)

    (do-move! new-g *board-size*)
    (play-move! new-g 5 1)

    (play-move! new-g 4 1)
    (setq old-game (deep-copy-go new-g)) 

    (play-move! new-g 4 0) ; Capture

    (when verbose? (print-go new-g t nil t t))

    (undo-move! new-g)
    (when verbose? (print-go new-g t nil t t))

    (test "Undo-Two-Captures" (equal-go? old-game new-g verbose?))
))

;;  TEST-UNDO-MERGE-GROUP
;; ---------------------------
;;  Testing UNDO-MOVE! after two groups are merged
(defun test-undo-merge-group (&optional (verbose? nil))
  (let ((new-g (init-game))
        (old-game nil)
        )
    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (play-move! new-g 1 2)

    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (play-move! new-g 0 2)
    
    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (play-move! new-g 2 1)

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (play-move! new-g 2 0)

    (setq old-game (deep-copy-go new-g))
    (when verbose? (print-go new-g t nil t t)) 
    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (play-move! new-g 2 2)

    (undo-move! new-g)
    (undo-move! new-g)

    (when verbose? (print-go new-g t nil t t)) 
    ;; each player should have two groups
    (test "Undo-Merge-Group" (equal-go? new-g old-game verbose?)))) 

;;  TEST-UNDO-MERGE-MANY-GROUP
;; ---------------------------
;;  Testing UNDO-MOVE! after two groups are merged
(defun test-undo-merge-many-group (&optional (verbose? nil))
  (let ((new-g (init-game))
        (old-game nil)
        )
    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (do-move! new-g *board-size*)

    (when verbose? (print-go new-g t nil t t)) 
    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (do-move! new-g *board-size*)

    (when verbose? (print-go new-g t nil t t)) 
    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (do-move! new-g *board-size*)

    (when verbose? (print-go new-g t nil t t)) 
    (play-move! new-g (- *board-length* 3) (- *board-length* 4))
    (do-move! new-g *board-size*)

    (when verbose? (print-go new-g t nil t t)) 
    (play-move! new-g (- *board-length* 1) (- *board-length* 3))
    (do-move! new-g *board-size*)

    (setq old-game (deep-copy-go new-g))
    (when verbose? (print-go new-g t nil t t)) 
    (play-move! new-g (- *board-length* 3) (- *board-length* 3))

    (undo-move! new-g)

    (when verbose? (print-go new-g t nil t t)) 
    ;; each player should have two groups
    (test "Undo-Merge-Many-Group" (equal-go? new-g old-game verbose?))))

;;  TEST-UNDO : OP (VERBOSE?)
;; ------------------------------------
;;  Testing the correctness of UNDO-MOVE!
(defun test-undo (num-moves &optional (verbose? nil)) 
  (let ((new-g (init-game))
        (new-g-copy nil)
        )
    ;; Allow the A.I. to play to some random point
    (dotimes (i num-moves)
      (when verbose? (print-go new-g t nil t t))
      (do-move! new-g (compute-move new-g 2 nil)))

    (when verbose? (print-go new-g t nil t t t))
    ;; Copy the game 
    (setq new-g-copy (deep-copy-go new-g))
    ;; Do one more move
    (do-move! new-g (compute-move new-g 2 nil))
    (when verbose? (print-go new-g t nil t t t))
    ;; Undo the move
    (undo-move! new-g)
    (when verbose? (print-go new-g t nil t t t))
    ;; Check if it's equal to the previous game state
    (test "Undo" (equal-go? new-g new-g-copy verbose?))))

;;; Testing Game Play
;;  TEST-LEGAL-MOVES
;; ------------------------
(defun test-legal-moves (&optional (verbose? nil))
  (let* ((new-g (init-game))
         (moves (legal-moves new-g))
         (passed? t)
         )
    (dolist (move moves)
      (do-move! new-g move verbose?)
      (undo-move! new-g)
      (when verbose? 
        (print-go new-g t nil t t))
      (unless (equal-go? new-g (init-game))
        (setq passed? nil)
        (return)))
    (test "Legal-Moves" passed?))) 

;;  TEST-KO-LEGALITY 
;; ---------------------
(defun test-ko-legality (&optional (verbose? nil))
  (let ((new-g (init-game))
        (moves nil)
        )
    (play-move! new-g 3 4)
    (play-move! new-g 3 5)

    (play-move! new-g 4 3)
    (play-move! new-g 4 6)

    (play-move! new-g 5 4)
    (play-move! new-g 5 5)

    (play-move! new-g 4 5)
    (play-move! new-g 4 4)

    (when verbose? (print-go new-g t nil t t))

    (setq moves (legal-moves new-g))
    (when verbose? (format t "Should not contain 41~%"))
    (when verbose? (format t "Legal Moves: ~A~%" moves))

    (test "Ko-Legality" 
          (not (find 41 moves :test #'=)))
  ))

;;  TEST-PUT-PULL-PIECE
;; --------------------------
(defun test-put-pull-piece
  (&optional (verbose? nil))
  (let ((new-g (init-game))
        (test-one nil)
        (test-two t) 
        (test-three t) 
        )
    (put-piece! new-g 59)
    (setf (gg-whose-turn? new-g)
          (- 1 (gg-whose-turn? new-g)))
    (pull-piece! new-g (vector 59 0 0))
    (setq test-one 
          (equal-go? (init-game)
                     new-g))
    (when verbose? (print-go new-g t nil t t))
    (format verbose? 
            "One: ~A, Two: ~A, Three: ~A~%" 
            test-one test-two test-three)
    (test "Put-Pull-Piece" 
          (and test-one 
               test-two 
               test-three))))

;;  TEST-ROBUST 
;; ------------------------------
;;  Testing the robustness of the game system
(defun test-robust ()
  (pg 4 1))

;;  DO-ALL-TESTS
;; -----------------------
(defun do-all-tests ()
  (test-corner-capture)
  (test-large-capture)
  (test-surround-capture)
  (test-two-captures)
  (test-corner-liberties)
  (test-corner-area)
  (test-corner-territory)
  (test-grouping-one)
  (test-grouping-two)
  (test-grouping-three)
  (test-grouping-four)
  (test-grouping-near-opponent)
  (test-put-pull-piece)
  (test-legal-moves)
  (test-ko-legality)
  (test-undo-capture)
  (test-undo-surround-capture)
  (test-undo-two-captures)
  (test-undo-merge-group)
  (test-undo-merge-many-group)
  (test-undo 5)
  (test-undo 10)
  (test-undo 15)
  (test-undo 30)
  (test-undo 40)
  )

;; TEST-BENCH : Test performance of game
(defun test-bench ()
  (time 
    (dotimes (j 1000) 
      (let ((new-g (init-game))
            )
        (dotimes (i *board-size*)
          (do-move! new-g i)
          (undo-move! new-g)
          (do-move! new-g i)
          )
        (setq new-g (init-game))
        (dotimes (i *board-size*)
          (do-move! new-g i)
          (do-move! new-g *board-size*))
        ))))

;;  BENCH-ALPHA-BETA
;; -----------------------
(defun bench (times depth)
  (let ((new-g (init-game))
        )
    (time (dotimes (i times)
            (do-move! new-g (compute-move new-g depth nil))
            ))))
