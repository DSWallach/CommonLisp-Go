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
