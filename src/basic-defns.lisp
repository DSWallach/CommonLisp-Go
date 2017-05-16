;;  COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

;; Tell the copiler to speed things up
(eval-when (compile load eval)
  ;; Require AllegroCache for storing networks
  (require :smputil) ;; Load Allegro mutlithreading
  (require :gcpath) ;; For tracking down memory
  (require :process)
  ;; Not being used currently
  ;(require :acache "acache-3.0.9.fasl")
  ;; Need more newspace than old as the nn outputs and mc-trees are regularly thrown out
  (sys:resize-areas :new 4000000000 :old 4000000000) ;; Allocate extra memory to minize garbage collection
  (setf (sys:gc-parameter :helper-threads-requested) 8)
  ;(setf (sys:gc-parameter :generation-spread) 25) ;; Hold off on tenuring. Networks will last a while before they are defunct
  ;(setf (sys:gc-switch :gc-old-before-expand) t) ;; Don't request more memory, use old memory
  (declaim (optimize (speed 2) (safety 1) (space 0) (debug 0))))

(defmacro track (funcal)
  `(gcpath:collected-newstuff () ,funcal)
  )


(defun ttest (num threads?)
  (uct-search (init-game) num 4 nil threads?))

(defun print-board (game)
            (dotimes (i *board-length*)
              (dotimes (j *board-length*)
                (format t "~A " (svref (gg-board game) (row-col->pos i j)))
                )
              (format t "~%")
              ))

;;  GLOBAL CONSTANTS

;; Game Properties 
(defconstant *black* 0)
(defconstant *white* 1)
(defconstant *board-length* 9)
(defconstant *group-dist* 1)
(defconstant *mc-rounds* 2)
(defconstant *board-size* (* *board-length*
                             *board-length*))
(defconstant *board-middle*
             (- (/ *board-length* 2) 1))

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

(defconstant *zobrist-vectors*
             (vector
               ;; Black
               (make-array *board-size*)
               ;; White
               (make-array *board-size*)))

(defun init-z-vectors ()
  (dotimes (i 2)
    (dotimes (j *board-size*)
      ;; Set the vector
      (setf (svref (svref *zobrist-vectors* i) j)
            (make-array (* 2 *board-size*) :element-type 'bit :initial-element 0))
      ;; Give it a unique bit
      (setf (sbit (svref (svref *zobrist-vectors* i) j) (* (+ 1 i) j)) 1))))
;; Initialize vectors
(init-z-vectors)

;; For compiling
(defun cl (filename)
  ;;  COMPILER-FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t) 
  (compile-file filename :verbose nil)
  (load filename :verbose nil)
  t
  )


(defun maker (lof)
  (mapcar #'cl lof))
;; Compile and load all files
(defun make ()
  (maker '(
           "basic-defns"
           "nn-go"
           "2014-new-nn"
           "go-game"
           "group"
           "game-playing"
           "alpha-beta-go"
           "mcts-go"
           "testing"
           )))


;;   MACROS
;; --------------------------------
(defmacro other-player (player)
  `(if (eq ,player *black*)
     *white*
     *black*))

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

(defmacro player->piece (player)
  `(if (= ,player *black*)
     1  ; Black
     -1 ; White
     ))

(defmacro piece->player (piece)
  `(if (= ,piece 1)
     *black*
     *white*
     ))

;;  PLAY-GAME : GAME DEPTH-ONE DEPTH-TWO ONE?
;; ---------------------------------------------
;; A function for setting to A.I.'s with different
;; depths against each other. For fun.
(defun play-game (game depth-one depth-two one? &optional (verbose? nil
                                                                  )) 
  (if (game-over? game)
    (when verbose?
      (unless (format t "++++++++ Game Over +++++++++~%")
        (print-go game t nil t t)))
    (when (if one? 
            (do-move! game (compute-move game depth-one))
            (do-move! game (compute-move game depth-two)))
      (when verbose? 
        (format t "Game State~%")
        (print-go game t nil nil nil))
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

(defun copy-vector (in-vec &optional (copy-func nil))
  (let ((out-vec (make-array (length in-vec)))
        )
    (if copy-func
    (dotimes (i (length in-vec))
      (setf (svref out-vec i) 
            (funcall copy-func (svref in-vec i))))
    (dotimes (i (length in-vec))
      (setf (svref out-vec i) (svref in-vec i)))
    )
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

(defconstant *opening-moves*
             (list (row-col->pos 2 2)
                   (row-col->pos 1 2)
                   (row-col->pos 1 1)
                   (row-col->pos 2 1)

                   (row-col->pos (- *board-length* 3) (- *board-length* 3))
                   (row-col->pos 2 (- *board-length* 3))
                   (row-col->pos 2 2)
                   (row-col->pos (- *board-length* 3) 2)

                   (row-col->pos (- *board-length* 3) (- *board-length* 3))
                   (row-col->pos (- *board-length* 2) (- *board-length* 3))
                   (row-col->pos (- *board-length* 2) (- *board-length* 2))
                   (row-col->pos (- *board-length* 3) (- *board-length* 2))

                   (row-col->pos 2 2)
                   (row-col->pos (- *board-length* 2) 2)
                   (row-col->pos (- *board-length* 2) (- *board-length* 2))
                   (row-col->pos 2 (- *board-length* 2))
                   (floor  (/ *board-size* 2))

                   (floor (+ (/ *board-size* 2) 1))
                   (floor (- (/ *board-size* 2) 1))
                   (floor (+ (/ *board-size* 2) *board-length*))
                   (floor (- (/ *board-size* 2) *board-length*))
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

(defstruct (compete-settings 
             (:conc-name cs-)
             (:print-function print-settings)
             )
  (b-sims 0)
  (b-c 0)
  (b-t nil)
  (b-net nil)
  (w-sims 0)
  (w-c 0)
  (w-t nil)
  (w-net nil)
  (pool nil)
  )

(defun compete-to-list (settings)
  (let ((set-list (list)))
    (push (cs-b-sims settings) set-list)
    (push (cs-b-c settings) set-list)
    (push (cs-b-t settings) set-list)
    (push (cs-b-net settings) set-list)
    (push (cs-w-sims settings) set-list)
    (push (cs-w-c settings) set-list)
    (push (cs-w-t settings) set-list)
    (push (cs-w-net settings) set-list)
    (push (cs-pool settings) set-list)
    set-list))

(defun print-settings (settings str depth)
  (declare (ignore depth))
  (format str "  ~A ~A ~A ~A ~A ~A ~A ~A ~A" 
          (cs-b-sims settings)
          (cs-b-c settings)
          (cs-b-t settings)
          (cs-b-net settings)
          (cs-w-sims settings)
          (cs-w-c settings)
          (cs-w-t settings)
          (cs-w-net settings)
          (cs-pool settings)
          ))

(defstruct (file-lock 
             (:include synchronizing-structure))
  path)

(defconstant *game-file*
             (make-file-lock :path (make-pathname :name 
                                            "../game-records/main-record")))

(defconstant *num-cores* 2)
