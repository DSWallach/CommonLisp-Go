;;  COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

;; Tell the copiler to speed things up
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0))))

;;  GLOBAL CONSTANTS

;; Game Properties 
(defconstant *black* 0)
(defconstant *white* 1)
(defconstant *board-length* 9)
(defconstant *board-size* (* *board-length* 
                             *board-length*)) 

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

;; Load Alpha/Beta AI
(cl "alpha-beta-go")

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

(defmacro row-col->pos (row col)
  `(+ (* *board-length* row) col))

(defmacro pos->row (pos)
  `(floor (/pos *board-length*)))

(defmacro pos-col (pos)
  `(mod pos *board-length*))
