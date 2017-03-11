;; ==============================
;;  CMPU-365, Spring 2017
;;  Asmt. 4
;;  test-chess.lisp
;; ==============================
;;  Some functions for testing the COMPUTE-MOVE function
;;  in the CHESS domain.

(load "asmt-helper.lisp")

;;  CL  (compile and load)
;; -------------------------------------
;;  INPUT:   FILENAME, a STRING naming a file, but without any 
;;             extension (e.g., "2017-chess-solns").
;;  OUTPUT:  NIL
;;  SIDE-EFFECT:  Compiles-and-loads the file with the compiler
;;                flags set to optimize for tail recursion.

(defun cl (filename)
  ;;  COMPILER-FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t) 
  (compile-file filename :verbose nil)
  (load filename :verbose nil)
  t
  )


;;  COMPUTE-DO-AND-SHOW-N-MOVES
;; ------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           N, a positive integer
;;           CUTOFF-DEPTH, the cutoff depth for minimax
;;  OUTPUT:  don't care
;;  SIDE EFFECT:  Computes, does, and shows the results of N
;;                moves generated using COMPUTE-MOVE.

(defun compute-do-and-show-n-moves
    (g n cutoff-depth)
  (let ((mv nil))
    (dotimes (i n)
      (format t "~%~A~%" g)
      (setf mv (compute-move g cutoff-depth))
      (apply #'do-move! g nil mv))
    (format t "~%~A~%" g)))

;;  TEST-ONE -- A game with an easy checkmate

(defun test-one ()
  (problem "TEST-ONE:  White should find an easy checkmate")
  (let ((g (setup-chess '((1 4 3 4)
			  (6 0 4 0)
			  (0 3 2 5)
			  (7 0 5 0)
			  (0 5 3 2)
			  (6 7 4 7))
			;; It will be white's turn when COMPUTE-MOVE is called
			)))
    (compute-do-and-show-n-moves g 3 6)
    (format t "White should have taken black's king by now!~%")
    (format t "Game over? ~A~%~%~%" (game-over? g))))


;;  TEST-TWO -- Should be able to win the rook in two moves!

(defun test-two ()
  (problem "TEST-TWO: White should be able to win the rook in two moves!")
  (let ((g (setup-chess '((1 4 3 4)
			  (6 4 4 4)
			  (0 6 2 5)
			  (6 3 5 3)
			  (0 5 4 1)
			  (7 4 6 4)
			  (2 5 4 6)
			  (7 1 5 0)
			  (4 1 3 2)
			  (6 7 5 7)
			  ;; It will be white's turn when COMPUTE-MOVE is called
			  ))))
    (compute-do-and-show-n-moves g 3 6)
    (format t "White should have taken black's rook by now!~%~%~%")))


;;  TEST-THREE -- Should move the queen to avoid getting captured

(defun test-three ()
  (problem "TEST-THREE: Black should move the queen to avoid its capture")
  (let ((g (setup-chess '((1 4 3 4)
			  (6 4 4 4)
			  (0 6 2 5)
			  (6 3 5 3)
			  (0 5 4 1)
			  (7 4 6 4)
			  (2 5 4 6)
			  (7 1 5 0)
			  (4 1 3 2)
			  (6 7 5 7)
			  (4 6 6 5)
			  ;; It will be black's turn when COMPUTE-MOVE is called
			  ))))
    (compute-do-and-show-n-moves g 1 6)
    (format t "Black should have moved its queen!~%~%~%")))

;; From:  https://www.sparkchess.com/difficulty/easy-chess-puzzles

(defun mate-in-two ()
  (problem "MATE-IN-TWO")
  (let ((g (init-game (list (list *king* 0 2)
			    (list *rook* 0 3)
			    (list *pawn* 1 0)
			    (list *pawn* 1 1)
			    (list *pawn* 1 2)
			    (list *pawn* 1 5)
			    (list *pawn* 1 6)
			    (list *pawn* 1 7)
			    (list *queen* 2 1)
			    (list *pawn* 3 4)
			    (list *bishop* 4 6))
		      (list (list *king* 7 4)
			    (list *bishop* 7 5)
			    (list *rook* 7 7)
			    (list *pawn* 6 0)
			    (list *knight* 6 3)
			    (list *pawn* 6 5)
			    (list *pawn* 6 6)
			    (list *pawn* 6 7)
			    (list *queen* 5 4)
			    (list *pawn* 4 4)))))
    (compute-do-and-show-n-moves g 5 6)
    (format t "White should have taken black's king by now!~%")
    (format t "Game over? ~A~%~%~%" (game-over? g))))
	

(defun mate-in-three (&optional (cutoff-depth 6))
  (problem "MATE-IN-THREE")
  (let ((g (init-game (list (list *king* 0 7)
			    (list *pawn* 1 7)
			    (list *bishop* 4 4)
			    (list *rook* 5 5))
		      (list (list *rook* 7 0)
			    (list *rook* 7 6)
			    (list *king* 7 7)
			    (list *pawn* 6 5)
			    (list *pawn* 6 7)))))
    (compute-do-and-show-n-moves g 7 cutoff-depth)
    (format t "White should have taken black's king by now!~%")
    (format t "Game over? ~A~%" (game-over? g))))


(defun do-all-tests
    ()
  (header "David Wallach" "4")
  (test-one)
  (test-two)
  (test-three)
  (mate-in-two)
  (mate-in-three)
  (my-test))
