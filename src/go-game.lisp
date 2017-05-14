;; =================================
;;   CMPU-365, Spring 2017
;;   Go Implementation
;; =================================


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
  ;; Vector with pointers to the lists containing Black and White groups
  (groups (vector () ()))
  ;; Black plays first
  (whose-turn? *black*)
  ;; Points for white
  (komi 0)
  ;; When true get-legal-moves will check for Ko situations
  (ko? nil)
  ;; Current scores for each player (hold over from Alpha Beta)
  (subtotals (vector 0 0))
  ;; Arrays indicating what board positions are eyes
  (eyes (vector (make-array *board-size* :initial-element 0)
                (make-array *board-size* :initial-element 0)))
  ;; List of vectors #(a b c) where
  ;; a == row played at
  ;; b == col played at
  ;; c == number of groups captured by the move
  (move-history nil)
  ;; History of the board state, used to check for ko
  (board-history nil) 
  )

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

; (format str "Eyes ~% ~A~% ~A~%" 
;         (svref (gg-eyes game) *black*)
;         (svref (gg-eyes game) *white*)
;         )
  ;; Print the board
  (dotimes (row *board-length*)
    (if (< 9 row)
      (format str "~A |" row)
      (format str " ~A |" row))

    (dotimes (col *board-length*)
      (let* ((p (svref board (row-col->pos row col)))
             (g (find-group (row-col->pos row col) game))
             (pos nil)
             )
        (if (= 0 p) 
          (cond 
            ((and verbose? (< 0 (svref (svref (gg-eyes game) *black*) 
                                       (row-col->pos row col)))
                  )
             (format str " x ")
             )
            ((and verbose? (< 0 (svref (svref (gg-eyes game) *white*) 
                                       (row-col->pos row col)))
                  )
             (format str " o ")
             )
            (t (format str " - ")
               )
            )

          (if verbose? 
            (when g ; If there is a group
              (setq pos (group-territory g))
            ;; Black
            (cond
              ((= 1 p) (if (or (< 9 pos) (< pos 0))
                         (format str " ~A" pos)
                         (format str " ~A " pos)))
              ;; White
              ((= -1 p) (if (or (< 9 pos) (< pos 0))
                          (format str "~A'" pos)
                          (format str " ~A'" pos)))
              (t (format t "Shouldn't print this, there's a ~A on the board" p))
              )
            )

            (cond 
              ((= 1 p) (format str " x "))
              ((= -1 p) (format str " o ")))))))
    (format str "~%"))

  ;; Print seperator
  (format str "    ")
  (dotimes (i *board-length*)
    (if (= i (- *board-length* 1))
      (format str "--")
      (format str "---")))
  (format str "~%")))

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
      (format print-to "Move Histories are not equal~% ~A~%"
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

;;  MAKE-HASH-KEY-FROM-GAME : GAME
;; ---------------------------------
;; The hash key is the current state of the board.
(defun make-hash-key-from-game (game)
  (copy-vector (gg-board game)))


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
        (life 0)
        )

    ;; Calc black's score
    (dolist (group b-groups)
      (setq life (group-alive? group))
      ;; If the group's alive add it's territory 
      ;; to black's score
      (cond  
        ((= 1 life)
         (setq b-score (+ b-score 
                          (group-territory group))))

        ;; Otherwise add the pieces to white's score
        ((= 0 life) (setq w-score (+ w-score 
                                     (length (group-pieces group)))))))

    ;; Add black's captures to their score
    (dolist (capd b-captures)
      (setq b-score (+ b-score 
                       (length (group-pieces capd)))))

    ;; Calc white's score
    (dolist (group w-groups)

      (setq life (group-alive? group))
      (cond 
        ((= 1 life) 
         (setq w-score (+ w-score 
                          (group-territory group))))
        ((= 0 life) 
         (setq b-score (+ b-score 
                          (length (group-pieces group))))))
      )
    ;; White's Captures
    (dolist (capd w-captures)
      (setq w-score (+ w-score (length (group-pieces capd)))))

    ;; Update the game struct
    (setf (gg-subtotals game) (vector b-score w-score))))

;;  DEEP-COPY-LIST : L(ist) COPY-FUNC
;; ------------------------------
;;  Creates a deep copy of a list
(defmacro deep-copy-list (l copy-func)
  `(let ((nl ()))
     (dolist (el ,l)
       (push (funcall ,copy-func el) nl))
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
        (b-eyes (copy-vector (svref (gg-eyes game) *black*)))
        (w-eyes (copy-vector (svref (gg-eyes game) *white*)))
        )
    (make-go-game :board (copy-vector (gg-board game)) 
                  :captures (vector b-caps w-caps)
                  :groups (vector b-groups w-groups)
                  :whose-turn? (gg-whose-turn? game)
                  :komi (gg-komi game)
                  :ko? (gg-ko? game)
                  :subtotals (vector b-subs w-subs)
                  :eyes (vector b-eyes w-eyes)
                  :board-history (deep-copy-list (gg-board-history game) 'copy-vector)
                  :move-history (deep-copy-list (gg-move-history game) 'copy-seq)
                  )))

;;  EVAL-FUNC : GAME
;; ------------------------------------
;;  Static evaluation function
(defun eval-func (game)
  (- (svref (gg-subtotals game) *black*)
     (svref (gg-subtotals game) *white*)))

;;  FIND-GROUP : POS GAME
;; -------------------------------------
;;  Used by PRINT-GO to find the group for each piece
(defun find-group (pos game)
  (let ((piece (svref (gg-board game) pos)))
    (unless (= 0 piece)
      (let* ((player (piece->player piece)) 
             (groups (svref (gg-groups game) player)))
        (dolist (group groups)
          (when (find pos (group-pieces group))
            (return-from find-group group)
            ))
            (format t "Group not found at ~A, ~A:  ~A~%" (pos->row pos) (pos->col pos) piece)
        ))))

;;  FIND-AND-RETURN-GROUP : POS GAME
;; -------------------------------------
;;  Used by PRINT-GO to find the group for each piece
(defun find-and-return-group (pos game)
  (let ((piece (svref (gg-board game) pos))
        )
    (when (< 0 piece)
      (let* ((player (- piece 1)) 
             (groups (svref (gg-groups game) player)))
        (dolist (group groups)
          (when (find pos (group-pieces group))
            (return-from find-and-return-group 
                         group)))))))



(defun num-liberties (group1 group2)
    (if (< (group-liberties group1) 
           (group-liberties group2))
      t nil))

;;  REMOVE-DEAD-GROUPS! : GAME
;; ----------------------------------
;;  Captures all groups that don't have two eyes or
;;  room for two eyes
(defun remove-dead-groups! (game)

  (let ((b-groups (sort (svref (gg-groups game) *black*) #'num-liberties))
        (w-groups (sort (svref (gg-groups game) *white*) #'num-liberties))
        (turn (gg-whose-turn? game))
        (group nil)
        )
    (dotimes (i (+ (length b-groups) 
                   (length w-groups)))
      (cond 
        ((= turn *black*)
         (setq group (first b-groups))
         (setq b-groups (rest b-groups))
         (when group
           (update-group! group game *black*)
           ;; Capture dead groups
           (when (= 0(group-alive? group))
             (capture-group! group game *white*))
           )

         (when (< 0 (length w-groups)) 
           (setq turn *white*))
         )
        (t

          (setq group (first w-groups))
          (setq w-groups (rest w-groups))
          (when group
            (update-group! group game *white*)
            ;; Capture dead groups
            (when (= 0(group-alive? group))
              (capture-group! group game *black*))
            )
          (when (< 0 (length b-groups))
            (setq turn *black*))
          )
        ))
    ;(print-go game t nil t t)

    ;; Update territories of remaining groups
    (dolist (ggroup (svref (gg-groups game) *black*))
      (update-group! ggroup game *black*))
    (dolist (ggroup (svref (gg-groups game) *white*))
      (update-group! ggroup game *white*))
    ))


;;  GAME-OVER? : GAME
;; -------------------------------
(defun game-over? (game)
  (when (or (> (length (gg-move-history game)) 
               ;; To protect against triple kos (very rare but could cause an infinite loop)
               ;; Twice as many moves as positions on the board is many more than games typically
               ;; go for. Usually it would be fewer than there are positions on the board.
               (* 2 *board-size*))
            (and (> (length (gg-move-history game)) 2) 
                 ;; If the past two moves were the same move the 
                 ;; game is over. This is because the only circumstance 
                 ;; underwhich the same move can be made by each player
                 ;; consecutively is when that move is a pass. This signals
                 ;; the end of the game.
                 (= (svref (first (gg-move-history game)) 0)
                    (svref (second (gg-move-history game)) 0))))

    ;; Remove dead groups
    (remove-dead-groups! game)

    ;; Update scores
    (eval-subtotals! game)
    ;; Return true
    t
    ;; Otherwise the game is still going
    ))

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
    t))

;;  CHECK-BOARD : POS BOARD CHECK-WHERE?
;; ----------------------------------
;;  Check the staus of the relevant place on the
;;  0 = empty
;;  1 = *black*
;;  2 = *white*
;;  3 = board-edge
(defmacro check-board (pos board check-where?)
  `(case ,check-where?
    (0 ;;*check-left* 
      (cond
        ;; Check if it's on the left edge
        ((= 0 (mod ,pos *board-length*)) 3)
        ;; Return the value of the ,position to the left
        (t  (svref ,board (- ,pos 1)))))
    (1 ;;*check-right*
      (cond
        ;; Check if it's on the right edge
        ((= (- *board-length* 1) (mod ,pos *board-length*)) 3)
        ;; Check if there is space to the right
        (t  (svref ,board (+ ,pos 1)))))
    (2 ;;*check-above*
      (cond
        ;; Check if it's on the top edge
        ((> *board-length* ,pos) 3)
        ;; Check if there is space above 
        (t  (svref ,board (- ,pos *board-length*)))))
    (3 ;;*check-below*
      (cond
        ;; Check if it's on the bottom edge
        ((<= (* (- *board-length* 1) *board-length*) ,pos) 3)
        ;; Check if there is space to below 
        (t (svref ,board (+ ,pos *board-length*)))))))

