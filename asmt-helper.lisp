;;; =========================
;;;  CMPU-365, Spring 2017
;;;  asmt-helper.lisp
;;; =========================
;;;  Some Helpful Functions when doing assignments.

;;;  NEWLINE
;;; ------------------------
;;;  INPUTS:  none
;;;  OUTPUT:  none
;;;  SIDE-EFFECT: Prints a newline in the interactions window

(defun newline 
    () 
  (format t "~%"))

;;;  TESTER
;;; ----------------------------
;;;  INPUT:  EXPR, anything
;;;  OUTPUT:  NIL
;;;  SIDE EFFECT:  Displays EXPR both before and after evaluation
;;; ----------------------------
;;;  Best used when the evaluation of EXPR does not cause any
;;;  side-effect printing.  Note, when calling this function
;;;  you should quote the expression of interest.  For example,
;;;  compare (tester (+ 1 2)) and (tester '(+ 1 2)) in the
;;;  Interactions Window.

(defun tester (expr)
  ;; NOTE:  FORMAT expressions evaluate to NIL
  (format t "~A ===> ~%" expr)
  (format t "~A~%~%" (eval expr)))

;;;  FANCY-TESTER
;;; ----------------------------
;;;  Same as TESTER, except that it displays any side-effect
;;;  printing caused by the evaluation of EXPR.  For example,
;;;  compare (tester '(format t "hi")) and (fancy-tester '(format t "hi"))
;;;  in the Interactions Window.

(defun fancy-tester (expr)
  ;; First, display EXPR (unevaluated)
  (format t "~%~%")
  (format t "Input Expression: ~A~%" expr)
  (format t "----------------------~%")
  (let (;; Evaluating EXPR may cause side-effect printing HERE!
	(result (eval expr))) 
    (format t "~%----------------------~%")
    ;; After side-effect printing done, we now display the RESULT
    ;; (i.e., output value) of evaluating EXPR
    (format t "===> ~A~%~%" result))
  (newline)
  ;; NOTE:  FORMAT returns NIL
  )

;;;  HEADER
;;; ------------------------------
;;;  INPUTS:  NAME, your name (a string or symbol)
;;;           NUM, the assignment number (an integer)
;;;  OUTPUT:  nil
;;;  SIDE EFFECT: Displays a nice header in the "interactions window".

(defun header (name num)
  (format t "~%~%====================================~%")
  (format t "  CMPU-365, Spring 2017~%")
  (format t "  Asmt. ~A~%" num)
  (format t "  ~A~%" name)
  (format t "====================================~%~%"))

;;;  PROBLEM
;;; ---------------------------------
;;;  INPUT:  INFO, some information about an assignment problem
;;;  OUTPUT:  NIL
;;;  SIDE EFFECT:  Prints a nice header for an assignment problem
;;;   in the interactions window.

(defun problem (info)
  (format t "-------------------------~%")
  (format t " PROBLEM ~A~%" info)
  (format t "-------------------------~%~%"))


;;;  COMPILE-AND-LOAD
;;; ------------------------------
;;;  INPUT:  FILENAME, a string
;;;  OUTPUT:  None
;;;  SIDE EFFECT:  Compiles the indicated file and then loads it.
;;;  NOTE:  Do NOT include the ".lisp" extension in the filename;
;;;         otherwise it will load the UNcompiled file!!

(defun compile-and-load (filename)
  (compile-file filename :verbose nil)
  (load filename :verbose nil))

;;;  MAKER
;;; ------------------------------
;;;  INPUT:  LIST-O-FILES, a list of strings
;;;  OUTPUT:  T, if all files compiled and loaded successfully
;;;  SIDE EFFECT:  Compiles and loads all indicated files

(defun maker
    (list-o-files)
  (mapcar #'compile-and-load
	  list-o-files))