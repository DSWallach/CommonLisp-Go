;;   Evolutionary Pipline Functions
;; ======================================

;; I'm using an evolutionary algorithm instead of 
;; training on the results of the matches b/c the network
;; trained that way wasn't better when combined with the 
;; Value network in the original paper so I'm going to 
;; try something else
(defstruct (competetor
             (:conc-name c-)
             (:print-function print-comp)
             (:include synchronizing-structure))
  id        ;; A number that is unique within each generation
  net       ;; A NN
  age       ;; The number of gens this network has been a competetor
  fitness   ;; The fitness of this competetor in the current generation
  dominated ;; A parameter for use with AFPO
  )

(defun print-comp (comp str depth)
  (declare (ignore depth))
  (format t "~A-~A~%" (nn-family-name (c-net comp))
          (nn-id(c-net comp))))

(defun new-competetor (net id)
  (make-competetor :net net 
                   :age 0
                   :fitness 0
                   :dominated nil))

;; Returns a list of pairs containing every combination of 
;; pairs of two networks provided
;; These lists are akin to the pareto fronts in AFPO
(defmacro make-pairs (loc)
  `(let ((pairs (list))
         (comp1 nil)
         )
     (loop while (< 0 (length ,loc))
           do (setq comp1 (pop ,loc))
           (dolist (comp2 ,loc)
             (when (and comp1 comp2)
               (push (list comp1 comp2) pairs)))
           )
     pairs))

;; Non-dominated before dominated then 
;; by fitness
(defun most-fit (comp1 comp2)
  (if (or (and (not (c-dominated comp1))
               (c-dominated comp2))
          (> (c-fitness comp1) 
             (c-fitness comp2)))
    t nil))

;; Macro for returning the next generation of 
;; competetors
(defmacro get-next-gen (generation)
  `(let ((next-gen (list))
         (gen nil)
         (child nil)
         (gen-id 0)
         )
     ;; Sort the list by highest fitness
     (setq gen (sort ,generation #'most-fit))

     ;; For each member of the population
     (dolist (comp gen)
       ;; Every non-dominated ID gets a mutant offspring 
       ;; added to the next-generation
       (unless (c-dominated comp)
         (setq child (new-competetor (nn-mutate (c-net comp) 0.25) gen-id))
         (format t "Child ~A~%" child)
         (write-network (c-net child))
         (push child 
               next-gen)
         (incf gen-id))
       ;; Increment age
       (incf (c-age comp))
       ;; Reset fitness
       (setf (c-fitness comp) 0)
       ;; Reset dominated
       (setf (c-dominated comp) nil)

       ;; Return when all non-dominated networks
       ;; are added and the previous population size
       ;; has been reached
       (if (or (not (c-dominated comp))
               (<= (length next-gen)
                   (length ,generation)))
         ;; Add it to the next generation
         (push comp next-gen)
         ;; Otherwise discard it
         (setq comp nil)))

     ;; Return the next generation
     next-gen))


;; Has two competitors play two games against each other 
;; once each as black and white. The player with the combined
;; higher score from both games is the winner (ties are a one 
;; point win for white)
(defun face-off (pair gen barrier file-lock)
  (let ((comp1 (first pair))
        (comp2 (second pair))
        (score1 0)
        (score2 0)
        (game nil)
        )
    (format t "Round 1 ~A" pair)

    ;; Play a game recording two random board states 
    ;; as well as who won in this generation's game-history
    (setq game (gauntlet (c-net comp1) (c-net comp2) file-lock))

    (cond
      ;; If a tie count as a one point victory for white
      ((= (svref (gg-subtotals game) *black*)
          (svref (gg-subtotals game) *white*))
       (incf score2 1))
      (t 
        ;; First comp1 is black
        (incf score1 (svref (gg-subtotals game) *black*))
        ;; And comp2 is white
        (incf score2 (svref (gg-subtotals game) *white*))
        ))

    (format t "Round 2 ~A" pair)
    ;; Then they switch
    (setq game (gaunlet (c-net comp2) (c-net comp1) file-lock))
    (cond
      ;; If a tie count as a one point victory for white
      ((= (svref (gg-subtotals game) *black*)
          (svref (gg-subtotals game) *white*))
       (incf score1 1))
      (t 
        ;; First comp1 is black
        (incf score2 (svref (gg-subtotals game) *black*))
        ;; And comp2 is white
        (incf score1 (svref (gg-subtotals game) *white*))
        ))
    ;; Evaluate the winner
    (cond 
      ;; If comp1 was the victor
      ((> score1 score2)
       ;; Update it's fitness
       (with-locked-structure 
         (comp1)
         (incf (c-fitness comp1)))
       (with-locked-structure 
         (comp2)
         ;; Update the other fitness
         (decf (c-fitness comp2))
         ;; Set dominated 
         (setf (c-dominated comp2) t)))
      ;; If comp2 was the victor
      ((> score2 score1)
       ;; Update it's fitness
       (with-locked-structure 
         (comp2)
         (incf (c-fitness comp2)))
       (with-locked-structure 
         (comp1)
         ;; Update the other fitness
         (decf (c-fitness comp1))
         ;; Set dominated 
         (setf (c-dominated comp1) t))))
    ;; If there is a tie, neither is dominated 
    ;; or has a change in fitness

    (mp:barrier-pass-through barrier)
    (format t "Thread completing ~A ~%" barrier)))

;; Run evolutionary trials pitting networks against
;; networks and store a few game state pairs from the 
;; game
(defun evolve-networks (lon generations num-fronts file-lock &optional (threads? nil))
  (let ((fronts (make-array num-fronts :initial-element (list)))
        (generation (list))
        (gen-id 0)
        (barrier nil)
        (front-count 0))

    (dolist (net lon)
            (push (new-competetor net gen-id)
                  generation)
            (incf gen-id))

    (format t "Initial Population ~A~%" generation)

    ;; Run the evolutionary algorithm
    (dotimes (gen generations)
      (format t "Running Generation ~A~%" gen)

      ;; Randomly distribute the networks among the fronts
      (dolist (comp generation)
        (push comp (svref fronts front-count))
        (incf front-count)
        (when (= num-fronts front-count)
          (setq front-count 0)))

      ;; Reset the fronts as pairs
      (dotimes (i num-fronts)
        (setf (svref fronts i)
              (make-pairs (svref fronts i))))

      ;; Evaluate each network's fitness
      ;; Do the competetitions in rounds so as not to
      ;; overwelm allegro with threads
      (dotimes (i num-fronts)
        ;; Reset the barrier
        (setq barrier (mp:make-barrier (+ 1 (length (svref fronts i)))))
        (dolist (pair (svref fronts i))
          (when (and (first pair)
                     (second pair))
            (format t "Face Off: ~A ~%" pair)
            (if threads? 
              (mp:process-run-function (write-to-string pair) 
                                       #'face-off
                                       pair
                                       gen
                                       barrier
                                       file-lock)
              (face-off pair gen barrier file-lock))
            )
          (format t "Waiting for threads to finish~%")
            ;; So the OS doesn't freak out over memory requests
            ;; Only necessary when spawning many threads
            (when (> (length (svref fronts i)) 2) 
              (sleep 1)))

        ;; Wait for all the networks in the front to finish
        (mp:barrier-wait barrier))

      (format t "Creating next generation~%")
      ;; Get the next generation
      (setq generation (get-next-gen generation)))

    (format t "Done!~%")))

