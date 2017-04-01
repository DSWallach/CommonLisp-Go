;; =================================
;;   CMPU-365, Spring 2017
;;   Go Implementation
;; =================================


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
        (let* ((p (svref board (row-col->pos row col)))
               )
          (if (= 0 p) 
            (format str " - ")
            (if verbose? 
              (let ((pos (find-group (row-col->pos row col) game))
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

