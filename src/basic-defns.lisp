;;  COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 


;; Tell the copiler to speed things up
(eval-when (compile load eval)
 (require :smputil) ;; Load Allegro mutlithreading
  (require :asdf)    ;; Load asdf package manager
  (require :process)
  (sys:resize-areas :new 3000000000) ;; Allocate extra memory to minize garbage collection
  (setf (sys:gc-switch :gc-old-before-expand) t) ;; Don't request more memory, use old memory
  (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0))))


(defun ttest (num threads?)
  (uct-search (init-game) num 2 nil threads?)
  )

;;  GLOBAL CONSTANTS

;; Game Properties 
(defconstant *black* 0)
(defconstant *white* 1)
(defconstant *board-length* 9)
(defconstant *group-dist* 1)
(defconstant *num-cores* 48)
(defconstant *merge-rounds* 4)
(defconstant *board-size* (* *board-length*
                             *board-length*))
(defconstant *board-middle*
             (- (/ *board-length* 2) 1))

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

(defun maker (lof)
  (mapcar #'cl lof))

;; MACROS
(defmacro row-col->pos (row col)
  `(+ (* *board-length* ,row) ,col))

(defmacro pos->row (pos)
  `(floor (/ ,pos *board-length*)))

(defmacro pos->col (pos)
  `(mod ,pos *board-length*))

(defun pos->middle-dist (pos)
  (+ (abs (- (pos->col pos) *board-middle*))
     (abs (- (pos->row pos) *board-middle*)))
  )

(defconstant *opening-moves*
             (list (row-col->pos 3 3)
                   (row-col->pos 2 3)
                   (row-col->pos 2 2)
                   (row-col->pos 3 2)

                   (row-col->pos (- *board-length* 4) (- *board-length* 4))
                   (row-col->pos 2 (- *board-length* 4))
                   (row-col->pos 2 2)
                   (row-col->pos (- *board-length* 4) 2)

                   (row-col->pos (- *board-length* 4) (- *board-length* 4))
                   (row-col->pos (- *board-length* 3) (- *board-length* 4))
                   (row-col->pos (- *board-length* 3) (- *board-length* 3))
                   (row-col->pos (- *board-length* 4) (- *board-length* 3))

                   (row-col->pos 3 3)
                   (row-col->pos (- *board-length* 3) 3)
                   (row-col->pos (- *board-length* 3) (- *board-length* 3))
                   (row-col->pos 3 (- *board-length* 3))
                   ))


(defun ab-vs-mc (whose-monte? depth num-sims c &optional 
                              (return-after nil) (use-threads nil)
                              )
  (let ((g (init-game)))
    (cond 
      ((= *black* whose-monte?)
       (while (not (game-over? g))
              (cond
                ((eq (gg-whose-turn? g) *black*)
                 (format t "BLACK'S TURN!~%")
                 (format t "~A~%" 
                         (do-move! g (uct-search g num-sims c return-after use-threads))))
                (t
                  (format t "WHITE'S TURN!~%")
                  (format t "~A~%"
                          (do-move! g (compute-move g depth)))))
              (when (and return-after
                         (< return-after (length (gg-move-history g)))
                         )
                (return-from ab-vs-mc g)
                )
              )
       )
      ;; Otherwise white get monte-carlo-search
      (t
       (while (not (game-over? g))
              (cond
                ((eq (gg-whose-turn? g) *black*)
                 (format t "BLACK'S TURN!~%")
                 (format t "~A~%" 
                         (do-move! g (compute-move g depth))))
                (t
                  (format t "WHITE'S TURN!~%")
                  (format t "~A~%"
                          (do-move! g (uct-search g num-sims c))))))
       ))))

;; Compile and load all files
(defun make ()
  ;; Load the alexandria system (required by bordeaux-threads
  ;;(asdf:load-system 'alexandria)
  ;; Load bordeaux-threads
  ;;(asdf:load-system 'bordeaux-threads)
  (maker '("basic-defns"
           "go-game"
           "group"
           "game-playing"
           "alpha-beta-go"
           "mcts-go"
           "testing"
           )))
