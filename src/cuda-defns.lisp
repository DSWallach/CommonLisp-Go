(ql:quickload "cl-cuda")

;; Tell the copiler to speed things up
(eval-when (compile load eval)
  ;; Require AllegroCache for storing networks
  (require :smputil) ;; Load Allegro mutlithreading
  (require :process)
;  (require :acache "acache-3.0.9.fasl")
  (sys:resize-areas :new 600000000 :old 10000000) ;; Allocate extra memory to minize garbage collection
  (setf (sys:gc-switch :gc-old-before-expand) t) ;; Don't request more memory, use old memory
  (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0))))


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
  (maker '("cuda-defns"
           "cuda-nn")))
