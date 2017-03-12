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
 (+ (* row *board-length*) col)) 


;;  GO-GAME struct
(defstruct (go-game (:print-function print-go)
                    (:conc-name gg-))
  ;; The board is a simple vector. Positions on the
  ;; board are referenced using the go-position function 
  (board (make-array *board-size* :initial-element 0))
  ;; The pieces captured by each player. A simple of the 
  ;; number of pieces that have been captured
  (captures (vector 0 0))
  ;; Vector with pointers to the lists containing Black
  ;; and white groups
  (groups (vector () ()))
  ;; White's pieces, .....
  ;; Same as for chess
  (whose-turn? *black*)
  ;; When true get-legal-moves will check for Ko situations
  (atari? nil)
  (subtotals (vector 0 0))
  (move-history nil))

;;  PRINT-GO
;; ----------------------------
;;  Print function for the GO-GAME struct
(defun print-go (game str depth)
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
    (format str "Groups:        ~A       ~A~%" 
            (length (svref (gg-groups game) *black*)) 
            (length (svref (gg-groups game) *white*)))
    (format str "Est. Score:    ~A       ~A~%"
            (svref evals *black*)
            (svref evals *white*))
    (format str "Current Val: ~A, Whose Turn ~A~%" 
            (- (svref evals whose-turn?)
               (svref evals (- 1 whose-turn?)))
            whose-turn? 
            )))

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
    (make-go-game :captures (vector 0 komi))))




;;  GROUP
;; ----------------------------
;;  A group of pieces enclosing some amount
;;  of territory in the game of Go. The 
;;  territory of each of a player's groups
;;  is added to their score estimate
(defstruct (group (:print-function print-group))
  (pieces nil)
  (num-pieces 0)
  (area (vector 0 0 0 0))
  (territory 0))

(defun print-group (group str depth)
  (declare (ignore depth))
  (format str "Pieces: ~A~%" (group-pieces group))
  (format str "Area: ~A~%" (group-area group))
  (format str "Territory: ~A~%" (group-area group)))

(defun init-group (row col)
  (let ((terr 4))
    (labels ((pos-min (pos)
                      (if (= 0 pos)
                        (when (setq terr (- terr 1)) 
                          pos)
                        (- pos 1)))
             (pos-max (pos)
                      (if (= *board-length* pos)
                        (when (setq terr (- terr 1)) 
                          pos)
                        (+ pos 1))))
      (let ((min-row (pos-min row))
            (min-col (pos-min col))
            (max-row (pos-max row))
            (max-col (pos-max col)))
        (make-group :pieces '(#(row col)) 
                    :num-pieces 1 
                    :area (vector min-row min-col max-row max-col)
                    :territory terr)))))


;; CALC-AREA
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


;; Caluclate territory basic method
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
  (let ((new-group nil))
    ;; Recursive subroutine to locate and retrive the 
    ;; group the move at (row, col) should be added to 
    (labels ((update-group 
               (group)
               (format t "Group: ~A~%" group)
               ;; Get the properties of group
               (let ((new-pieces (group-pieces group))
                     (new-area (group-area group))
                     (new-terr (group-territory group)))
                 ;; Destructively modify the group
                 (setf new-pieces (append new-pieces (list (find-pos row col))))
                 (setf new-area (calc-area new-area row col))
                 (setf new-terr (calc-territory new-area))))

             ;; Find the group the piece should beling to if it exists
             (check-group 
               (groups) 
               (format t "check ~A~%" (first groups))
               ;; Search for an existing group it could fit in
               (let ((group (first groups)))
                 (cond 
                   ;; When groups is empty (Base Case)
                   ;; End the recursion
                   ((eq group nil) nil)

                   ;; When it's in the area of the group
                   ((and (>= 2 (abs (- row (svref (group-area group) 0))))
                         (>= 2 (abs (- col (svref (group-area group) 1))))
                         (< 2 (abs (- row (svref (group-area group) 2))))
                         (< 2 (abs (- col (svref (group-area group) 3)))))
                    ;; Pop the group off the list of groups 
                    ;; and store it in new-group
                    (setq new-group (pop groups))
                    ;; Then return the rest of the list 
                    groups)
                   ;; Otherwise check the next group
                   (t (append '(first groups) (check-group (rest groups))))))))

      ;; If there are no groups 
      (if (eq nil (svref (gg-groups game) (gg-whose-turn? game))) 
        ;; Make a list of groups containing the current move
        (setf (svref (gg-groups game) (gg-whose-turn? game)) (list (init-group row col)))
        ;; Otherwise get the groups of the player whose turn it is
        (let ((new-groups (check-group (svref (gg-groups game) (gg-whose-turn? game)))))
          (if new-group ; If a group was found 
            ;; Modify the group and put it at the front of the list 
            (push (update-group new-group) new-groups)
            ;; Otherwise make a new group
            (push (init-group row col) (svref (gg-groups game) (gg-whose-turn? game)))))))))

;;  EVAL-SUBTOTALS!
;; ------------------------
;;  INPUT: GAME, a GO-GAME struct
;;  SIDE EFFECT: A modified game struct
;;      updated with the most recent score-estimates
(defun eval-subtotals! (game)
  (let* ((caps (gg-captures game))
         (b-score (svref caps *black*))
         (w-score (svref caps *white*)) 
         (b-groups (svref (gg-groups game) *black*))
         (w-groups (svref (gg-groups game) *white*)))
    ;; Calc black's score
    (dolist (group b-groups)
      (setq b-score (+ b-score (group-territory group))))
    ;; Calc white's score
    (dolist (group w-groups)
      (setq w-score (+ w-score (group-territory group))))
    ;; Update the game struct
    (setf (gg-subtotals game) (vector b-score w-score))))


;;  SHOW-GO
;; ------------------
;; Convenient function
(defun show-go (game) 
  (eval-subtotals! game)
  (print-go game t nil))



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
    (find-add-group game row col))

;;  PLAY-MOVE!
;; --------------------------------------
;;  Mostly a wrapper for PUT-PIECE! also
;;  updates the value of GG-WHOSE-TURN?
(defun play-move! (game row col)
  (let ((player (gg-whose-turn? game)))
    (put-piece! game player row col)
    (setf (gg-whose-turn? game) (- 1 player))
    (show-go game)
    ))
