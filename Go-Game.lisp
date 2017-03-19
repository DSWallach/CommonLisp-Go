;; =================================
;;   CMPU-365, Spring 2017
;;   Go Implementation
;; =================================

;;  COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

;; Tell the copiler to speed things up
(eval-when (compile)
  (declaim (optimize (speed 0) (safety 0) (space 0) (debug 3))))

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

(defun copy-vector (in-vec)
  (let ((out-vec (make-array (length in-vec)))
        )
    (dotimes (i (length in-vec))
      (setf (svref out-vec i) (svref in-vec i)))
    out-vec))

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
  (board-history nil) 
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
        )

    ;; Calc black's score
    (dolist (group b-groups)
      ;; Add 1/2 of the liberties and all the territory 
      ;; to the score
      (setq b-score (+ b-score 
                       (group-territory group) 
                       (group-liberties group))))
    (dolist (capd b-captures)
      (setq b-score (+ b-score (length (group-pieces capd)))))

    ;; Calc white's score
    (dolist (group w-groups)
      (setq w-score (+ w-score 
                       (group-territory group) 
                       (group-liberties group) 2)))

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
(defun print-go (game str depth &optional (verbose? t) (groups? t) (boards? t))
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
    (format str "~%Game Length: ~A, " (length (gg-board-history game)))
    (format str "Move history~%~A~%" (gg-move-history game)) 
    )
    (when boards?
      (dotimes (i (length (gg-board-history game)))
        (format str "Board at turn ~A : ~A~%" 
                (- (length (gg-board-history game)) i)
                (nth i (gg-board-history game)))))
  ) 

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

    ;; If the results are to be printed
    (if print-to
      ;; Do the check with printing
      (when t 
        (unless (= (length black-groups-0)
                   (length black-groups-1))
          (format print-to "Black Group lengths don't match~% ~A~% ~A~%"
                  black-groups-0
                  black-groups-1
                  )
          (return-from equal-go? nil))
        (unless (= (length white-groups-0)
                   (length white-groups-1))
          (format print-to "white Group lengths don't match~% ~A~% ~A~%"
                  white-groups-0
                  white-groups-1
                  )
          )
        (unless (= (length black-captures-0)
                   (length black-captures-1))
          (format print-to "Black capture lengths don't match~% ~A~% ~A~%"
                  black-captures-0
                  black-captures-1
                  )
          (return-from equal-go? nil))

        (unless (= (length white-captures-0)
                   (length white-captures-1))
          (format print-to "white capture lengths don't match~% ~A~% ~A~%"
                  white-captures-0
                  white-captures-1
                  )

          (return-from equal-go? nil))

        (unless (= (length move-history-0)
                   (length move-history-1))

          (format print-to "Move history lengths don't match~% ~A~% ~A~%"
                  move-history-0
                  move-history-1
                  )
          (return-from equal-go? nil))
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
        (format print-to "List Lengths don't match~%")
        (return-from equal-go? nil))
      )

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
  (format nil "Calc Area ~%")
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
  (format nil "Calc-New-Area ~A row ~A col ~A~%" group row col)
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
    
    
    (format nil "Return area ~A~%" (group-area group))
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

    (format nil "Opponet used in case block ~A" opponent)
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
        
        ))))

;;  CAPTURE-GROUP! : GROUP GAME
;; ---------------------------------
;;  INPUTS
;;  SIDE-EFFECT: Destructively modify the game state by 
;;          capturing GROUP
(defun capture-group! (group game)
  (let* ((player (gg-whose-turn? game))
         (opponent (- 1 player))
         )
   ;; Break if the same group is to be captured twice 
    (when (find group (svref (gg-captures game) player) :test #'equal-group?)
      (break))
    
    ;; Remove the groups from the opponent's groups
    (setf (svref (gg-groups game) opponent) 
          (delete group (svref (gg-groups game) opponent)))

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
        )
    ;; Push the first piece from the group onto the merge-marker
    (push (first (group-pieces group-two))
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
  (let ((new-group (make-group))
        (piece nil)
        (mark (pop (group-merge-marker group)))
        )
    ;; Seperate-group! shouldn't be called if there are no 
    ;; merged groups.
    (when (null mark)
      (break))

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
           ;;(format nil "Index:~A, P-Groups:~A~%" index player-groups)
           ;; Search for an existing group it could fit in
           (let ((group (nth index player-groups))
                 )
             (cond 
               ;; When it's in the area of the group
               ((and (>= (+ row *group-dist*) 
                         (svref (group-area group) *min-row*))
                     (>= (+ col *group-dist*) 
                         (svref (group-area group) *min-col*))
                     (<= (- row *group-dist*) 
                         (svref (group-area group) *max-row*))
                     (<= (- col *group-dist*) 
                         (svref (group-area group) *max-col*)))

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
               ))
           ;; Return t
           t)
       ))

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
              (format nil "One Group~%")
              ;; Get the group
              (setq new-group (pop connected-groups))

              ;; Add the piece and update the group
              (add-piece! new-group) 
              
              ;; Add it back at the front of the groups list
              (push new-group 
                    (svref (gg-groups game) player))
              ;; Return 1
              1)
            ;; Otherwise get the first group
            (t 
              (format nil "More than one Group~%")
              (setq new-group (pop connected-groups))            
              ;; Update it with the new piece 
              (add-piece! new-group)
              ;; Merge it with the remaining groups 
              (dolist (group connected-groups)
                (merge-groups! new-group group)) 
              ;; Update the fully merged group
              (update-group! new-group game)
              ;; Push it back onto the players groups
              (push new-group (svref (gg-groups game) player))
              ;; Return the number of groups merged
              (+ (length connected-groups) 1))))))))

;;  GROUP-REMOVE! : GROUP POS GAME
;; -------------------------------
;;  INPUTS: GROUP, a group struct
;;          POS, The position of the piece to be removed
(defun group-remove! (group pos game)

  ;; Remove the most recent piece
  (setf (group-pieces group)
        (delete pos (group-pieces group)))

  ;; Unless there are no more pieces in the group
  (if (< 0 (length (group-pieces group)))
    ;; Update the group and return it
    (when (update-group! group game)
      group)
    nil ; Otherwise return nil
    ))

;;;;; GAME PLAYING
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
    ;; Return the result of find-add-group
    (find-add-group! game (svref vec 0) (svref vec 1))))

;;  DO-MOVE! : GAME POS
;; --------------------------------------
;;  Works like the chess-solns function of the same name
(defun do-move! (game pos)
  (let* ((captured 0)
         (board (gg-board game))
         (player (gg-whose-turn? game))
         (groups (svref (gg-groups game) (- 1 player)))
         )
    ;; Function for finding and capturing groups
    (labels ((groups-capture
               (group groups-length)
               ;; Check if any groups were captured
               (dotimes (i groups-length) 
                 ;; Get the group
                 (setq group (nth i groups))
                 ;; There shouldn't be a nil group
                 (unless group 
                   (break))

                 ;; Update liberties
                 (calc-liberties! group (gg-board game))

                 ;; When a group was captured
                 (when (= 0 (group-liberties group)) 
                   ;; Increment the capture flag
                   (setq captured (+ 1 captured))
                   ;; Mark the group's position
                   (setf (group-last-pos group) i)
                   ;; Capture the group
                   (capture-group! group game)
                   ;; Reset i 
                   (setq i (- i 1)) 
                   )

                 ;; If only one group containing only one piece 
                 ;; was captured set the flag to check for ko
                 (when (and (= captured 1) (= 1 (length (group-pieces group)))
                            (setf (gg-ko? game) t)))
                 ;; If there is no next group
                 (unless (nth (+ i 1) groups)
                   ;; Return
                   (return-from groups-capture t)) 
                 ))
             )

    ;; Push the board onto board history
    (push (vector-copy board) (gg-board-history game))

    ;; If the move is a pass
    (cond
      ((= *board-size* pos)
       ;; Push the pass
       (push (vector pos 0 0) 
             (gg-move-history game)))

      ;; Otherwise Put their piece at pos 
      ;; Track if any groups were merged in this round
      (t (let ((groups-merged (put-piece! game player pos))
               (group nil)
               )

           ;; Capture any dead groups
           (groups-capture group (length groups))

           ;; If more groups were captured remove the flag
           (when (< 1 captured)
            (setf (gg-ko? game) nil))

          ;; Check if the move would kill the current group
          (calc-liberties! (first (svref (gg-groups game) player))
                           (gg-board game))

          ;; If so capture the group
          (when (= 0 (group-liberties (first (svref (gg-groups game) player))))
            (capture-group! (first (svref (gg-groups game) player)) game))

          (when (= captured 0)
            (setf (gg-ko? game) nil))

          ;; Evaluate each players score
          (eval-subtotals! game)

          (push (vector pos captured groups-merged) 
                (gg-move-history game))
          )))

    ;; Change turn
    (setf (gg-whose-turn? game) 
          (- 1 player)))))

;;  UNDO-MOVE! : GAME
;; ----------------------------------------
;;  Undo the most recently played move
(defun undo-move! (game)
  (let* ((captured 0)
         (move (pop (gg-move-history game)))
         (pos (svref move 0))
         (board nil)
         (player (gg-whose-turn? game))
         (opponent (- 1 player))
         )
    ;; Delete the previous board 
    (setq board (pop (gg-board-history game)))
    ;; Unless the previous move was a pass
    (unless (= *board-size* pos)

      ;; If necessary seperate groups
      (when (< 0 (svref move 2))
        (let ((mod-group (pop (svref (gg-groups game) opponent)))
              (new-group nil) 
              (new-groups nil)
              )
          (dotimes (i (- (svref move 2) 1))
            ;; Get the seperated group
            (setq new-group 
                  (seperate-group! mod-group game))

            ;; Add the new-group to the list of new groups
            (push new-group new-groups) ;;(svref (gg-groups game) opponent))
            )
            ;; Push the modified onto the list of groups 
            (push mod-group 
                  (svref (gg-groups game) opponent))

            (dolist (g new-groups)
              (push g (svref (gg-groups game) opponent)))

          ))

      ;; Remove the piece from the board and from player's groups
      (pull-piece! game pos)

      (when (= captured 0)
        (setf (gg-ko? game) nil))

      ;; If necessary return captured groups
      (when (< 0 (svref move 1))

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
                                  (group-last-pos group))))
            )))

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
    ))

;;  PULL-PIECE!  -- used by UNDO-MOVE!
;; ---------------------------------------------------------------
;;  INPUTS:  GAME, a GO-GAME struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Removes given piece from the board.
;;  NOTE:  Removing piece from the board does not affect the
;;         values of its ROW and COL fields.  (See PUT-PIECE!.)

(defun pull-piece! (game pos)
  ;; The player of the previous turn is the opponent this turn
  (let* ((player (- 1 (gg-whose-turn? game)))
        (player-groups (svref (gg-groups game) player))
        (group nil)
        )
    ;; Remove the piece from the game board
    (setf (svref (gg-board game) pos) 0)

    (dotimes (i (length player-groups))

      ;; When the group contains the piece
      (when (find pos (group-pieces (nth i player-groups)))
        ;; Set the group
        (setq group (nth i player-groups))

        (cond 
        ;; CASE If the group has more than one piece
        ((< 1 (length (group-pieces group))) 

           ;; Remove the piece 
           (setq group 
                 (group-remove! group pos game))

           ;; Recompute the group's area
           (calc-area! group)

           ;; Update the group's properties
           (update-group! group game)

           ;; Add it back to groups in the same place 
           (setf (nth i (svref (gg-groups game) player))
                group)

          ;; (format t "Return ~%") 
           ;; Return
           (return-from pull-piece! t)
         )

        ;; Otherwise delete the group    
        (t 
         ;; (format t  "One piece ~%")
           (setf (svref (gg-groups game) player)
                 (delete group (svref (gg-groups game) player)))
          ;; (format t "Return nil~%")
           (return-from pull-piece! nil)
           )
        ); End Cond
        ))))

;;  PLAY-MOVE!
;; ------------------------------
;;  Basic wrapper for DO-MOVE!
(defun play-move! (game row col)
  (do-move! game (find-pos row col)))

;;  LEGAL-MOVES? : GAME 
;; -----------------------------
;;  INPUT:  GAME, A go game struct
;;          ROW, The row of the move to check 
;;          COL, The column of the move to check
;;  OUTPUT: A boolean value indicating if the move is legal

(defun legal-moves (game &optional (fast? t))
  (let ((legal-moves (list *board-size*))
        (moves (list *board-size*))
        )
    ;; Check each row
    ;; Check each col
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
      (dotimes (pos *board-size*)
        (when (= 0 (svref (gg-board game) pos))
          (push pos moves))))

    (when (and (gg-ko? game) (< 3 (length (gg-board-history game))))
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
            (setq old-board (first (gg-board-history game)))

            (unless (equal-board? new-board old-board)
              (push move legal-moves)))
          (push move legal-moves)))
      (return-from legal-moves legal-moves))

    moves))

;;;;; TESTING
;;  TEST : TESTNAME PASSED?
;; -----------------------------
;;  Testing function
(defun test (testname passed?)
  (format t "Test ~A passed? ~A~%" testname passed?))

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
    (test "Test-Large-Capture" 
          (equal-group? black-group (first (svref (gg-captures new-g) *white*))))
    ))

;;  TEST-SURROUND-CAPTURE
;; -------------------------
;;  Test the capture of a group surrounded
(defun test-surround-capture ()
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

    (test "Test-Surround-Capture" (= (length (svref (gg-captures new-g) *white*)) 1))))

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

    (test "Test-Two-Capture" (and (= 1 (length (svref (gg-groups new-g) *black*)))
                                  (= 2 (length (svref (gg-captures new-g) *white*)))))
))

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
    (test "Test-Corner-Territory" (= (group-territory (first (svref (gg-groups new-g) *black*))) 4))))

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
    (test "Test-Corner-Area" correct?))) 

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
    (test "Test-Grouping-One" (and (= 1 (length (svref (gg-groups new-g) *white*)))
                               (= 1 (length (svref (gg-groups new-g) *black*)))))))

;;  TEST-GROUPING-TWO 
;; ---------------------------------
;;  Testing the correctness of how pieces are grouped
(defun test-grouping-two ()
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
    ;; each player should have only one group
    (test "Test-Grouping-Two" (and (= 1 (length (svref (gg-groups new-g) *white*)))
                               (= 1 (length (svref (gg-groups new-g) *black*)))))))

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

    (test "Test-Undo-Capture" (equal-go? new-g old-game verbose?))))

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
    (test "Test-Undo-Surround-Capture" (equal-go? new-g old-game verbose?))))

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

    (test "Test-Undo-Two-Captures" (equal-go? old-game new-g verbose?))
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
    (test "Test-Merge-Group" (equal-go? new-g old-game verbose?)))) 

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
    (test "Test-Undo-Merge-Many-Group" (equal-go? new-g old-game verbose?))))

;;  TEST-UNDO : OP (VERBOSE?)
;; ------------------------------------
;;  Testing the correctness of UNDO-MOVE!
(defun test-undo (num-moves &optional (verbose? nil)) 
  (let ((new-g (init-game))
        (new-g-copy nil)
        )
    ;; Allow the A.I. to play to some random point
    (dotimes (i (random num-moves))
      (when verbose? (print-go new-g t nil t t))
      (do-move! new-g (compute-move new-g 2 nil)))

    (when verbose? (print-go new-g t nil t t))
    ;; Copy the game 
    (setq new-g-copy (deep-copy-go new-g))
    ;; Do one more move
    (do-move! new-g (compute-move new-g 2 nil))
    (when verbose? (print-go new-g t nil t t))
    ;; Undo the move
    (undo-move! new-g)
    (when verbose? (print-go new-g t nil t t))
    ;; Check if it's equal to the previous game state
    (test "Test-Undo" (equal-go? new-g new-g-copy t))))

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
  (test-corner-area)
  (test-corner-territory)
  (test-grouping-one)
  (test-grouping-two)
  (test-undo-capture)
  (test-undo-surround-capture)
  (test-undo-two-captures)
  (test-undo-merge-group)
  (test-undo-merge-many-group)
  (test-undo 5)
  (test-undo 10)
  (test-undo 15)
  (test-undo 30)
  ;; (test-robust)
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
