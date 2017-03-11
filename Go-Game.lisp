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

(defstruct (chess (:print-function print-chess))
  (board (make-array '(8 8) :initial-element nil))
  (pieces (make-array '(2 16) :initial-element nil))
  (whose-turn? *white*)
  (eval-subtotals (vector 0 0))
  (move-history nil)
  )

(defun print-chess (game str depth)
  (declare (ignore depth))
  (let* ((bored (chess-board game))
	 (pieces (chess-pieces game))
	 (deaduns (vector nil nil))
	 (evals (chess-eval-subtotals game))
	 (whose-turn? (chess-whose-turn? game)))
    (format str "    0 1 2 3 4 5 6 7~%")
    (format str "  -------------------~%")
    (dotimes (row 8)
      (format str "~A:  " (- 7 row))
      (dotimes (col 8)
	(let ((elt (aref bored (- 7 row) col)))
	  (if elt
	      (format str "~A " elt)
	    (format str "- "))))
      (format str "~%"))
    (format str "  -------------------~%")
    (format t "White live values: ~A, black live values: ~A, Current Val: ~A~%"
	    (svref evals *white*)
	    (svref evals *black*)
	    (- (svref evals whose-turn?)
	       (svref evals (other-plr whose-turn?))))
    ;; Walk through pieces, accumulating deadwood...
    (dotimes (i 16)
      (dotimes (plr 2)
	(let ((p (aref pieces plr i)))
	  (when (not (piece-live? p))
	    (push p (aref deaduns plr))))))
    ;; Show deadwood
    (format t "White deadwood: ~A~%" (aref deaduns *white*))
    (format t "Black deadwood: ~A~%" (aref deaduns *black*))
    (format str "It is ~A's turn!~%" 
	    (if (eq *white* whose-turn?) "white" "black"))
    ))

(defun find-pos (row col)
 (+ (* row *board-length*) col)) 


;;  GO-GAME struct
(defstruct (go-game (:print-function print-go)
                    (:conc-name gg-))
  ;; The board is a simple vector. Positions on the
  ;; board are referenced using the go-position function 
  (board (make-array *board-size* :initial-element 0))
  ;; The pieces captured by each player. A simple
  ;; vector of two lists containing the pieces.
  (captures (vector nil nil))
  ;; Black's pieces, grouped by connectedness 
  (black-groups nil)
  ;; White's pieces, .....
  (white-groups nil)
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
        (format str "Est. Score: White ~A, Black ~A, Current Val: ~A~%" 
                (svref evals *white*)
                (svref evals *black*)
                (- (svref evals whose-turn?)
                   (svref evals (- 1 whose-turn?))))))

;;  GROUP
;; ----------------------------
;;  A group of pieces enclosing some amount
;;  of territory in the game of Go. The 
;;  territory of each of a player's groups
;;  is added to their score estimate
(defstruct group
  (pieces nil)
  (num-pieces 0)
  (area (vector 0 0 0 0))
  (territory 0))


;;  EVAL-SUBTOTALS!
;; ------------------------
;;  INPUT: G, a game struct
;;  SIDE EFFECT: A modified game struct
;;      updated with the most recent score-estimates
(defun eval-subtotals! (g)
  (let* ((caps (gg-captures g))
         (b-score (svref caps *black*))
         (w-score (svref caps *white*)) 
         (b-groups (gg-black-groups g))
         (w-groups (gg-white-groups g)))
    ;; Calc black's score
    (dolist (group b-groups)
      (setq b-score (+ b-score (group-territory group))))
    ;; Calc white's score
    (dolist (group w-groups)
      (setq w-score (+ w-score (group-territory group))))
    ;; Update the game struct
    (setf (gg-subtotals g) #(b-score w-score))))



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
;;      go rules.
(defun init-game (&optional (handicap -1) (komi 6.5))
  (when (= handicap -1)
    (make-go-game :subtotals (vector 0 6.5))))


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
  (setf (svref (gg-board game) (find-pos row col)) (+ 1 player)))
