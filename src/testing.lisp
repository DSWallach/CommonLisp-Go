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
(defun test-corner-territory (&optional (verbose? nil))
  (let ((new-g (init-game)))
    (play-move! new-g (- *board-length* 3) (- *board-length* 1))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 3) (- *board-length* 2))
    (do-move! new-g *board-size*)

    (play-move! new-g (- *board-length* 3) (- *board-length* 3))
    (do-move! new-g *board-size*)

    (when verbose? 
      (print-go new-g t nil t t))

    (play-move! new-g (- *board-length* 2) (- *board-length* 3))
    (do-move! new-g *board-size*)

    (when verbose? 
      (print-go new-g nil t t))

    (play-move! new-g (- *board-length* 1) (- *board-length* 3))

    (when verbose? 
      (print-go new-g t nil t t))

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
      (do-move! new-g (compute-move new-g 2 nil)))

    (when verbose? (print-go new-g t nil t t t))
    ;; Copy the game 
    (setq new-g-copy (deep-copy-go new-g))
    ;; Do one more move
    (do-move! new-g (compute-move new-g 2 nil))
    ;(when verbose? (print-go new-g t nil t t t))
    ;; Undo the move
    (undo-move! new-g)
    (when verbose? (print-go new-g t nil t t t))
    ;; Check if it's equal to the previous game state
    (test (concatenate 'string "Undo after " (write-to-string num-moves) " moves") (equal-go? new-g new-g-copy verbose?))))

;;; Testing Game Play
;;  TEST-LEGAL-MOVES
;; ------------------------
(defun test-legal-moves (&optional (verbose? nil))
  (let* ((new-g (init-game))
         (moves (legal-moves new-g))
         (passed? t)
         )
    (dotimes (move (length moves))
      (do-move! new-g (svref moves move) verbose?)
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


;; TEST-MERGE-TREES
(defun test-merge-trees ()
  (format t "Begin Setup~%")
  (let* ((tree1 (uct-search (init-game) 10 2 t))
         (game (ab-vs-mc *black* 2 10 2 10))
         (tree2 (uct-search game 10 2 t))
         (test-tree (merge-mc-trees! tree1 tree2))
         )
    (labels ((check-entry (key value)
                          (unless (gethash key test-tree)
                            (format t "Key: ~A, Value: ~A missing from test tree~%" key value)
                            (return-from test-merge-trees nil))
                          t)
             )
      (format t "Begin Test~%")
      (maphash #'check-entry (mc-tree-hashy tree1))
      (maphash #'check-entry (mc-tree-hashy tree2))
      (format t "Test Passed~%")
      )
    ))

;; TEST-DEEP-COPY
(defun test-deep-copy ()
  (let* ((new-g (init-game))
         (old-g (deep-copy-go new-g))
         )
    (play-game new-g 1 1 t nil)
    (test "Deep-Copy" 
          (equal-go? old-g (init-game)))))


;; TEST-EYE-AT
(defun test-eye-at ()

  (let* ((new-g (init-game))
         (old-g (deep-copy-go new-g))
         )
    (play-game new-g 1 1 t nil)
    (test "Deep-Copy" 
          (equal-go? old-g (init-game)))))


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
  (test-deep-copy)
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
