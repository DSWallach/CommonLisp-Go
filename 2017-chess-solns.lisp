;; =================================
;;   CMPU-365, Spring 2017
;;   Asmt. 4
;;   2017-chess-solns.lisp
;;   Sample chess implementation
;; =================================

;;  COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

;;  GLOBAL CONSTANTS

;;  The players

(defconstant *white* 0)
(defconstant *black* 1)

;;  Piece types (used as indices into *piece-names*, for example)

(defconstant *pawn* 0)
(defconstant *knight* 1)
(defconstant *bishop* 2)
(defconstant *rook* 3)
(defconstant *queen* 4)
(defconstant *king* 5)

(defparameter *white-info*
    '((0 1 0) (0 1 1) (0 1 2) (0 1 3) (0 1 4) (0 1 5) (0 1 6) (0 1 7)
      (1 0 1) (1 0 6) (2 0 2) (2 0 5) (3 0 0) (3 0 7) (4 0 3) (5 0 4)))

(defparameter *black-info*
    '((0 6 0) (0 6 1) (0 6 2) (0 6 3) (0 6 4) (0 6 5) (0 6 6) (0 6 7)
      (1 7 1) (1 7 6) (2 7 2) (2 7 5) (3 7 0) (3 7 7) (4 7 3) (5 7 4)))

;;  Piece Names:  used by the print functions

(defconstant *piece-names* #2A (("P" "N" "B" "R" "Q" "K")
				("p" "n" "b" "r" "q" "k")))

;;  PIECE-VALUES:  For the static evaluation function

(defconstant *piece-values* #(100 300 350 500 975 100000))

;;  WIN-LOSS VALUES

(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)

;;  NEGATIVE and POSITIVE INFINITY
;; ----------------------------------------

(defconstant *neg-inf* -10000000)
(defconstant *pos-inf*  10000000)


;;  KING-INDEX:  Used by the GAME-OVER? function to access a player's king

(defconstant *king-index* 15)


;;  PIECE struct
;; -------------------------------------------------------
;;  Fields:
;;    OWNER     --  either *black* or *white*
;;    TYPE      --  *pawn*, *knight*, etc.
;;    ROW, COL  --  indicates position of piece on the board
;;    LIVE?     --  T if piece on the board, NIL if captured
;;    VALUE     --  used by static evaluation function

(defstruct (piece (:print-function print-piece))
  owner   
  type    
  row     
  col
  live?   
  value   
  )

;;  PRINT-PIECE
;; --------------------------------------------------------
;;  Print function for PIECE structs

(defun print-piece (p str depth)
  (declare (ignore depth))
  (format str "~A" (aref *piece-names* (piece-owner p) (piece-type p))))


;;  CHESS struct
;; -----------------------------------------------------------------------
;;  Fields:
;;   BOARD   --  An 8-by-8 array containing PIECE structs or NIL
;;   PIECES  --  A 2-by-16 array for accessing pieces (even if not on board)
;;   WHOSE-TURN?  --  Either *white* or *black*
;;   EVAL-SUBTOTALS -- Vector containing two values:  the sum of the
;;      values of *white*'s pieces, and the sum of the values of *black*'s
;;      pieces
;;   MOVE-HISTORY  -- A list of the moves that got us from initial 
;;      state to the current state
;; -----------------------------------------------------------------------
;;  NOTE:  White's home rows are 0 and 1.
;;         Black's home rows are 6 and 7.

(defstruct (chess (:print-function print-chess))
  (board (make-array '(8 8) :initial-element nil))
  (pieces (make-array '(2 16) :initial-element nil))
  (whose-turn? *white*)
  (eval-subtotals (vector 0 0))
  (move-history nil)
  )

(defun find-piece
    (game player p-type)
  (let ((pieces (chess-pieces game)))
    (dotimes (i 16)
      (let ((pc (aref pieces player i)))
	(when (and (eq (piece-type pc) p-type)
		   (not (piece-live? pc)))
	  (return-from find-piece pc))))
    (format t "whoops! can't find piece!~%")
    nil))
         

;;  PRINT-CHESS
;; -------------------------------------------------------------------
;;  Print function for CHESS struct

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


;;  CREATE-SET-NEW-PIECE
;; ---------------------------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           OWNER, either *black* or *white*
;;           TYPE, one of:  *pawn*, *knight*, etc.
;;           ROW, COL, integers between 0 and 7
;;           I, index between 0 and 15
;;  OUTPUT:  T
;;  SIDE EFFECTS:  Creates a PIECE struct using the given information, places
;;    it onto the board, and inserts it into the PIECES array with index I.

(defun create-set-new-piece (game owner type row col i)
  (let ((pieces (chess-pieces game))
	;; create the new piece
	(peace (make-piece :owner owner
			   :type type
			   :row row
			   :col col
			   :live? t
			   :value (svref *piece-values* type))))
    ;; enter piece into array of pieces at appropriate index
    (setf (aref pieces owner i) peace)
    ;; put piedce onto the board
    (put-piece! game peace))
  ;; return T just for fun
  t)

(defun create-but-dont-set-piece (game owner type i)
  (let ((pieces (chess-pieces game))
	(peace (make-piece :owner owner
			   :type type
			   :row nil
			   :col nil
			   :live? nil
			   :value (svref *piece-values* type))))
    (setf (aref pieces owner i) peace)
    t))

;;  INIT-GAME
;; ----------------------------------------------------------------
;;  INPUTS:  None
;;  OUTPUT:  A CHESS struct corresponding to a new game of chess

    
(defun init-game (&optional (white-info *white-info*)
			    (black-info *black-info*))
  (let* ((game (make-chess))
	 (ctr 0))
    ;; Create all of the pieces
    (dolist (triple *white-info*)
      (let ((p-type (first triple)))
	(create-but-dont-set-piece game *white* p-type ctr)
	(create-but-dont-set-piece game *black* p-type ctr)
	(incf ctr)))

    (dolist (triple white-info)
      (let ((p-type (first triple))
	    (row (second triple))
	    (col (third triple)))
	;; Find first piece of this type 
	(let ((pc (find-piece game *white* p-type)))
	  (setf (piece-row pc) row)
	  (setf (piece-col pc) col)
	  (put-piece! game pc))))
    (dolist (triple black-info)
      (let ((p-type (first triple))
	    (row (second triple))
	    (col (third triple)))
	(let ((pc (find-piece game *black* p-type)))
	  (setf (piece-row pc) row)
	  (setf (piece-col pc) col)
	  (put-piece! game pc))))
	;; RETURN THE GAME!
    game))

;;  PULL-PIECE!  -- used by CREATE-SET-NEW-PIECE! and DO-MOVE!
;; ---------------------------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Removes given piece from the board.
;;  NOTE:  Removing piece from the board does not affect the
;;         values of its ROW and COL fields.  (See PUT-PIECE!.)

(defun pull-piece! (game pc)
  (setf (piece-live? pc) nil)
  (setf (aref (chess-board game) (piece-row pc) (piece-col pc)) 
    nil)
  ;; ==> decrement the value-subtotals for appropriate color
  (decf (svref (chess-eval-subtotals game) (piece-owner pc))
	(piece-value pc)))


;;  PUT-PIECE!  -- used by UNDO-MOVE!
;; -------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           PC, a PIECE struct
;;  OUTPUT:  None
;;  SIDE EFFECT:  Restores given piece to the board at the
;;    location specified by its ROW and COL fields.

(defun put-piece! (game pc)
  (setf (piece-live? pc) t)
  ;; ==> increment eval-subtotal for owner of piece
  (incf (svref (chess-eval-subtotals game) (piece-owner pc))
	(piece-value pc))
  (setf (aref (chess-board game) (piece-row pc) (piece-col pc))
    pc))

;;  PATH-CLEAR?  --  Used by the LEGAL-MOVE? function
;; --------------------------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           R1, C1, integers between 0 and 7
;;           R-DIRN, C-DIRN, integers from the set {-1, 0, 1}
;;           DIST, a non-negative integer
;;  OUTPUT:  T if the squares along the straight-line path from
;;             (R1,C1) in the direction of (R-DIRN,C-DIRN) are
;;              unoccupied for a distance of DIST.
;;             (Does *not* check starting or ending square.)
;;  NOTE:  This function is used to determine whether a given
;;         path is free of obstacles for a BISHOP, ROOK or QUEEN
;;         to move from a given starting point, along a given
;;         direction, for a given distance.  It is used by the
;;         LEGAL-MOVE? function to determine whether a given
;;         move is indeed legal.

(defun path-clear? (game r1 c1 r-dirn c-dirn dist)
  (let ((bored (chess-board game)))
    ;; Just walk along the given direction checking the squares you
    ;; encounter as you go...
    (dotimes (n (1- dist))
      (incf r1 r-dirn)
      (incf c1 c-dirn)
      ;; If the square is occupied... report failure
      (when (aref bored r1 c1)
	(return-from path-clear? nil)))
    
    ;; If got here, then the path must be clear!
    t))


;;  LEGAL-MOVE?
;; -------------------------------------------
;;  INPUTS:  GAME, a CHESS struct
;;           R1,C1,R2,C2, integers between 0 and 7
;;  OUTPUT:  T if moving the piece currently at (R1,C1) to (R2,C2)
;;           would be a legal move
;;  NOTE:  Does not consider "en passant" or "castling"

;;  WARNING:  This function should *NOT* be used during MINIMAX
;;            with ALPHA-BETA PRUNING because it would slow down
;;            the search too much.  Besides, the LEGAL-MOVES function
;;            is guaranteed to return only *legal* moves; so there is
;;            no need to check the legality of each move.
;;    This function is intended to be used to check the legality
;;    of a move entered by a person!

(defun legal-move? (game r1 c1 r2 c2)
  (let* ((bored (chess-board game))
	 (whose-turn? (chess-whose-turn? game)))
    (cond
     ;; Case 1:  Easy-to-check illegal situations...
     ((or ;; The squares are not on the board
          (< r1 0)
	  (< r2 0)
	  (< c1 0)
	  (< c2 0)
	  (> r1 7)
	  (> r2 7)
	  (> c1 7)
	  (> c2 7)
	  ;; There's no piece in the FROM position
	  (not (aref bored r1 c1))
	  ;; There's a piece in the FROM position, but of the wrong color
	  (not (eq (piece-owner (aref bored r1 c1)) whose-turn?))
	  ;; There's a piece on the TO position of the SAME color
	  (and (aref bored r2 c2)
	       (eq (piece-owner (aref bored r2 c2)) whose-turn?)))
      nil)
     
     ;; Case 2:  The squares are on the board, there's a piece of the right 
     ;;   color on the source square and, if there's a piece on the
     ;;   destination square, it is of the opposite color
     (t
      ;; The legality of the specified move depends on the
      ;; type of piece currently located at (R1,C1)
      (let* ((piece (aref bored r1 c1))
	     (type (piece-type piece))
	     (dr (- r2 r1))
	     (dc (- c2 c1))
	     (dirn (if (eq whose-turn? *white*) 1 -1))
	     (destn (aref bored r2 c2)))
	(cond
	 
	 ;;  Case A:  Pawn to move
	 ;; -----------------------------------------------------
	 
	 ((eq type *pawn*)
	  (or ;; move one ahead into blank square
	      (and (= dr dirn)
		   (= dc 0)
		   (not (aref bored (+ r1 dirn) c1)))
	      ;; move ahead 2 squares 
	      (and (= dc 0)
		   (= dr (* 2 dirn))
		   (not (aref bored r2 c2))
		   (not (aref bored (+ r1 dirn) c1))
		   (or (and (eq whose-turn? *white*)
			    (= r1 1))
		       (and (eq whose-turn? *black*)
			    (= r1 6))))
	      ;; capture diagonally
	      (and (= (abs dc) 1)
		   (= dr dirn)
		   ;; Note:  Already ruled out having own piece on destn square
		   destn)))

	 ;;  Case B:  Knight to move
	 ;; -----------------------------------------------------------------
	 ;;  Note:  Already checked that if there's a piece on destn square, 
	 ;;         it must belong to the opponent
	 
	 ((eq type *knight*)
	  (or (and (= (abs dc) 1) 
		   (= (abs dr) 2))
	      (and (= (abs dc) 2)
		   (= (abs dr) 1))))
	
	 ;;  Case C:  Bishop to move
	 ;; -----------------------------------------------------
	 
	 ((eq type *bishop*)
	  (and (= (abs dc) (abs dr))
	       (not (zerop dc))
	       (path-clear? game r1 c1 
			    (/ dr (abs dr))  ;; r-dirn
			    (/ dc (abs dc))  ;; c-dirn
			    (abs dc))        ;; dist
	       ))
	 
	 ;;  Case D:  Rook to move
	 ;; -------------------------------------------------------
	 
	 ((eq type *rook*)
	  (or ;; Horizontal move
	      (and (zerop dc) 
		   (not (zerop dr))
		   (path-clear? game r1 c1
				(/ dr (abs dr)) ;; r-dirn
				0               ;; c-dirn
				(abs dr)))      ;; dist
	      ;; Vertical move
	      (and (zerop dr) 
		   (not (zerop dc))
		   (path-clear? game r1 c1
				0               ;; r-dirn
				(/ dc (abs dc)) ;; c-dirn
				(abs dc)))))    ;; dist
	  
	 ;;  Case E: King to move
	 ;; -------------------------------------------------------
	 
	 ((eq type *king*)
	  (and (< (abs dc) 2)
	       (< (abs dr) 2)
	       (or (not (zerop dc))
		   (not (zerop dr)))))
	 
	 ;;  Case F: Queen to move
	 ;; ---------------------------------------------------------
	 
	 ((eq type *queen*)
	  (or ;; move along a row
	      (and (zerop dc) 
		   (not (zerop dr))
		   (path-clear? game r1 c1 
				(/ dr (abs dr)) ;; r-dirn 
				0               ;; c-dirn
				(abs dr)))      ;; dist
	      ;; move along a column
	      (and (zerop dr)
		   (not (zerop dc))
		   (path-clear? game r1 c1 
				0               ;; r-dirn
				(/ dc (abs dc)) ;; c-dirn
				(abs dc)))      ;; dist
	      ;; diagonal move
	      (and (= (abs dc) (abs dr))
		   (not (zerop dc))
		   (path-clear? game r1 c1 
				(/ dr (abs dr))  ;; r-dirn
				(/ dc (abs dc))  ;; c-dirn
				(abs dc))        ;; dist
		 )))
	 
	  ;; Other!!
	 (t
	  (format t "Illegal piece!")
	  nil)))))))

;;  TOGGLE-TURN!
;; -------------------------------------------------------
;;  INPUT:  GAME, a CHESS struct
;;  OUTPUT:  none
;;  SIDE EFFECT:  Changes whose turn it is

(defun toggle-turn! (game)
  (let ((current-turn (chess-whose-turn? game)))
    (setf (chess-whose-turn? game) (other-plr current-turn))))

;;  OTHER-PLR
;; --------------------------------------------------------
;;  INPUT:  PLR, either *black* or *white*
;;  OUTPUT:  The other player (i.e., *white* or *black*, respectively)

(defun other-plr (plr)
  (- 1 plr))

;;  DO-MOVE!
;; -------------------------------------------
;;  INPUTS:  GAME, chess struct
;;           CHECK-LEGAL?, a boolean flag
;;           R1,C1, position of piece to move
;;           R2,C2, position of destination
;;  OUTPUT:  Resulting CHESS struct if move legal and made
;;           NIL otherwise
;;  NOTE:  Only checks legality of proposed move if CHECK-LEGAL? set

(defun do-move! (game check-legal? r1 c1 r2 c2 &key (show nil))
  (cond
   ((and check-legal? (not (legal-move? game r1 c1 r2 c2)))
    (format t "Umm... Can't do illegal move: ~A,~A ==> ~A,~A~%" r1 c1 r2 c2)
    nil)
   
   (t
    (let* ((bored (chess-board game))
	   (piece (aref bored r1 c1))
	   (destn (aref bored r2 c2)))
      ;; remove piece from (r1,c1)
      (setf (aref bored r1 c1) nil)
      
      ;; if destination has a piece... must capture it
      (when destn (pull-piece! game destn))
      ;; put piece on destination square
      (setf (aref bored r2 c2) piece)
      (setf (piece-row piece) r2)
      (setf (piece-col piece) c2)
      ;; toggle the turn
      (toggle-turn! game)
      ;; push move info onto move-history
      (push (list r1 c1 r2 c2 destn) (chess-move-history game))
      (when show
	(format t "Moved ~A from (~A,~A) to (~A,~A)..." piece r1 c1 r2 c2)
	(when destn
	  (format t " captured ~A~%" destn))
	(format t "~%"))
   
      ;; return the GAME
      game))))

;;  UNDO-MOVE!
;; ----------------------------------------------------------------
;;  INPUT:  G, a CHESS struct
;;  OUTPUT:  The modified chess struct
;;  SIDE EFFECT:  Destructively undoes the most recent move on
;;    the move history

(defun undo-move! (g)
  (cond
   ;; Case 1:  No moves on move history!
   ((null (chess-move-history g))
    (format t "Umm... Can't undo move... empty history!~%")
    g)
   
   ;; Case 2:  There is a move to undo...
   (t
    (let* ((move (pop (chess-move-history g)))
	   (bored (chess-board g))
	   (r1 (first move))
	   (c1 (second move))
	   (r2 (third move))
	   (c2 (fourth move))
	   (piece (aref bored r2 c2))
	   (destn (fifth move))
	   (opponent (chess-whose-turn? g)))
      (when (aref bored r1 c1)
	(format t "Gonna undo move, but something was on source square!~%"))
      (when (null piece)
	(format t "Wanna undo a move, but there's no piece at destn!~%"))
      (when (and destn (not (eq opponent (piece-owner destn))))
	(format t "Umm... opponent not owner of captured piece~%"))
      ;; remove piece from (r2,c2)
      (setf (aref bored r2 c2) nil)
      ;; if necessary, restore previously captured piece to (r2,c2)
      (when destn (put-piece! g destn))
      ;; move piece back to (r1,c1)
      (setf (aref bored r1 c1) piece)
      (setf (piece-row piece) r1)
      (setf (piece-col piece) c1)
      ;; Toggle the turn!
      (toggle-turn! g)
      ;; Return the CHESS struct
      g))))

;;  LEGAL-PAWN-MOVES
;; -------------------------------------------
;;  INPUTS:  G, chess struct
;;           ROW, COL, coordinates of source square
;;           MOVES, a move accumulator
;;  OUTPUT:  The move accumulator that has been augmented to include
;;           all the legal moves for a pawn starting from (row,col) and
;;           pointing in the given DIRN.
;;           Note:  Assumes there's a pawn at (row,col).

(defun legal-pawn-moves (g row col moves)
  (let* ((whose-turn? (chess-whose-turn? g))
	 (bored (chess-board g))
	 (dirn (if (eq whose-turn? *white*) 1 -1))
	 (col-left (1- col))
	 (col-right (1+ col))
	 (row-plus (+ row dirn))
	 (row-plus-plus (+ row-plus dirn)))
    ;; Legal Leftward Capture
    (when (and (<= 0 row-plus 7)
	       (<= 0 col-left 7)
	       (aref bored row-plus col-left)
	       (not (eq (piece-owner (aref bored row-plus col-left))
			whose-turn?)))
      (push (list row col row-plus col-left) moves))
    ;; Legal Rightward Capture
    (when (and (<= 0 row-plus 7)
	       (<= 0 col-right 7)
	       (aref bored row-plus col-right)
	       (not (eq (piece-owner (aref bored row-plus col-right)) 
			whose-turn?)))
      (push (list row col row-plus col-right) moves))
    ;; Legal Advance One Square
    (when (and (<= 0 row-plus 7)
	       (not (aref bored row-plus col)))
      (push (list row col row-plus col) moves)
      ;; Legal Advance Two Squares
      (when (and (or (and (= row 1) (= dirn 1))
		     (and (= row 6) (= dirn -1)))
		 (not (aref bored row-plus-plus col)))
	(push (list row col row-plus-plus col) moves)))
    ;; return the accumulated list of moves!
    moves))

;;  LEGAL-BISHOP-MOVES / LEGAL-ROOK-MOVES / etc.
;; -------------------------------------------------------------
;;  Same as legal-pawn-moves, except that it checks for legal
;;  moves for other kinds of pieces.

(defun legal-bishop-moves (g row col moves)
  (let* ((bored (chess-board g))
	 (whose-turn? (chess-whose-turn? g)))
    ;; Legal directions for a bishop are:  (-1,-1), (-1,1), (1,-1) and (1,1)
    (dolist (dr '(-1 1))
      (dolist (dc '(-1 1))
	(let ((newrow (+ row dr))
	      (newcol (+ col dc))
	      (blocked? nil))
	  ;; Continue looking in direction (DR,DC) until blocked by a piece
	  (while (and (not blocked?)
		      (<= 0 newrow 7)
		      (<= 0 newcol 7)
		      ;; Okay to capture opponent's piece
		      (or (not (aref bored newrow newcol))
			  (not (eq whose-turn? 
				   (piece-owner (aref bored newrow newcol))))))
	    (push (list row col newrow newcol) moves)
	    ;; If there was a piece at (newrow,newcol), we're now blocked
	    (when (aref bored newrow newcol)
	      (setf blocked? t))
	    ;; prepare for next iteration
	    (incf newrow dr)
	    (incf newcol dc)))))
    ;; return accumulated moves
    moves))

(defun legal-rook-moves (g row col moves)
  (let* ((bored (chess-board g))
	 (whose-turn? (chess-whose-turn? g)))
    ;; Legal directions for a rook...
    (dolist (veck '((-1 0) (1 0) (0 -1) (0 1)))
      (let* ((dr (first veck))
	     (dc (second veck))
	     (newrow (+ row dr))
	     (newcol (+ col dc))
	     (blocked? nil))
	(while (and (not blocked?)
		    (<= 0 newrow 7)
		    (<= 0 newcol 7)
		    (or (not (aref bored newrow newcol))
			(not (eq whose-turn? 
				 (piece-owner (aref bored newrow newcol))))))
	  (push (list row col newrow newcol) moves)
	  (when (aref bored newrow newcol)
	    (setf blocked? t))
	  (incf newrow dr)
	  (incf newcol dc))))
    ;; return the accumulated list of moves
    moves))

(defun legal-queen-moves (g row col moves)
  ;; Queen can move like a ROOK or a BISHOP
  (setf moves (legal-rook-moves g row col moves))
  (legal-bishop-moves g row col moves))

(defun legal-knight-moves (g row col moves)
  (let* ((bored (chess-board g))
	 (whose-turn? (chess-whose-turn? g)))
    ;; There are 8 possible destination squares for a legal knight move
    (dolist (veck '((1 2) (2 1) (-2 1) (-1 2) (-1 -2) (-2 -1) (2 -1) (1 -2)))
      (let* ((dr (first veck))
	     (dc (second veck))
	     (newrow (+ row dr))
	     (newcol (+ col dc)))
	(when (and (<= 0 newrow 7)
		   (<= 0 newcol 7)
		   (or (not (aref bored newrow newcol))
		       (not (eq whose-turn?
				(piece-owner (aref bored newrow newcol))))))
	  (push (list row col newrow newcol) moves))))
    ;; return accumulated list of moves
    moves))

(defun legal-king-moves (g row col moves)
  (let* ((bored (chess-board g))
	 (whose-turn? (chess-whose-turn? g)))
    ;; King can move 1 square in any direction...
    (dolist (veck '((-1 -1) (-1 0) (-1 1) (0 1) (0 -1) (1 1) (1 0) (1 -1)))
      (let* ((dr (first veck))
	     (dc (second veck))
	     (newrow (+ row dr))
	     (newcol (+ col dc)))
	(when (and (<= 0 newrow 7)
		   (<= 0 newcol 7)
		   (or (not (aref bored newrow newcol))
		       (not (eq whose-turn?
				(piece-owner (aref bored newrow newcol))))))
	  (push (list row col newrow newcol) moves))))
    ;; return accumulated list of moves
    moves))

;;  *MOVE-FUNCS*
;; ---------------------------------------------------------------------
;;  A global parameter used by LEGAL-MOVES.

(defparameter *move-funcs* (vector #'legal-pawn-moves #'legal-knight-moves
				   #'legal-bishop-moves #'legal-rook-moves
				   #'legal-queen-moves #'legal-king-moves))

;;  LEGAL-MOVES
;; ------------------------------------------------------
;;  INPUT:  G, a CHESS game struct
;;  OUTPUT:  A list of the legal moves for whoever's turn it is.
;;  NOTE:  Fetches legal moves for all the LIVE pieces of whoever's 
;;         turn it is.  Uses the various LEGAL-X-MOVES helper functions.
;;         Note that each of them uses a MOVE accumulator!

(defun legal-moves (g &optional (plr nil))
       
       
  (let* ((whose-turn? (if plr plr (chess-whose-turn? g)))
	 (pieces (chess-pieces g))
	 ;; an accumulator
	 (moves nil))

    ;; For each piece...
    (dotimes (i 16)
      (let* ((p (aref pieces whose-turn? i))
	     (type (piece-type p))
	     (row (piece-row p))
	     (col (piece-col p)))
	;; If the piece is live...
	(when (and p (piece-live? p))
	  ;; Accumulate moves for that piece...
	  (setf moves
	    (funcall (svref *move-funcs* type) g row col moves)))))
    
    ;; return the accumulated moves	 
    moves))
  

;;  GAME-OVER?
;; -------------------------------------------------------
;;  INPUT:  G, a CHESS struct
;;  OUTPUT:  T if both players still have their kings on the board
;;  NOTE:  Doesn't check for "check", "checkmate", "stalemate"
;;          or any other kind of draw...

(defun game-over? (g)
  (let ((pieces (chess-pieces g)))
    (not (and (piece-live? (aref pieces *black* *king-index*))
	      (piece-live? (aref pieces *white* *king-index*))))))
  

;;  STATS struct
;; ---------------------------
;;  Stats compiled during minimax search

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))



;;  EVAL-FUNC
;; -------------------------------------------------
;;  INPUT:  G, a CHESS struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;            based on the difference in piece values held by 
;;            the two players.

(defun eval-func (g)
  (let* ((whose-turn? (chess-whose-turn? g))
	 (other (other-plr whose-turn?))
	 (evals (chess-eval-subtotals g)))
    (+ (- (length (legal-moves g whose-turn?))
          (length (legal-moves g other)))
       (- (svref evals whose-turn?)
	  (svref evals other)))))


;; ----------------------------------------------------
;;  Extra stuff
;; ----------------------------------------------------

;;  DO-N-RANDOM-MOVES
;; ---------------------------------------
;;  INPUTS:  G, a CHESS game struct
;;           N, number of moves to do
;;  OUTPUT:  Performs N randomly chosen, legal moves, destructively
;;           modifying the game.

(defun do-n-random-moves (g n)
  (dotimes (i n)
    (apply #'do-move! g nil (nconc (let* ((moves (legal-moves g))
					  (len (length moves))
					  (rnd (random len)))
				     (nth rnd moves))
				   '(:show t))))
  g)


;;  RANDOMIZE
;; -----------------------------------------------------
;;  INPUT:  LISTY, a list
;;  OUTPUT:  A shuffled version of LISTY
;;  Note:  Randomly "deals" elements of LISTY, discarding them
;;         into a "discard pile" until there are no more elements
;;         left.  Returns the accumulated list of elements.

(defun randomize (listy)
  (let* (;; Create a vector out of listy for efficiency
	 (veck (apply #'vector listy))
	 (len (length veck))
	 ;; A list accumulator 
	 (randy nil))
    ;; Randomly "deal" elements from VECK until there are no more
    (dotimes (i len)
      (let* (;; RND = index into UNDEALT elements of VECK
	     (rnd (random len))
	     ;; ELT = randomly chosen element of VECK to deal next
	     (elt (svref veck rnd)))
	;; Number of undealt elements is one less...
	(decf len)
	;; Swap the chosen element into the discard pile (at the bottom of VECK)
	(setf (svref veck rnd) (svref veck len))
	(setf (svref veck len) elt)
	;; Push randomly chosen element onto list accumulator
	(push elt randy)))
    randy))


(defun mover! (g r1 c1 r2 c2)
  (do-move! g t r1 c1 r2 c2 :show t))


;;  SETUP-CHESS
;; ---------------------------------------
;;  INPUT:  LIST-O-MOVES, a list of "moves" where each move is 
;;             a list of 4 numbers (of the kind used by do-move!)
;;  OUTPUT:  A CHESS struct representing a new game in which
;;    the moves in LIST-O-MOVES have been done.

(defun setup-chess (list-o-moves)
  (let ((g (init-game)))
    ;; For each move in the LIST-O-MOVES...
    (mapcar #'(lambda (movie)
		;; Do the move...
		(apply #'do-move! g nil movie))
	    list-o-moves)
    g
    ))
 
