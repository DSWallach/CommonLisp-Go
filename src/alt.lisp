
;; For use by each thread of uct-search
(defun sim-ops-alt (orig-game c total-sims queue id barrier)
  (let* ((state-move-list nil)
         (z 0)
         (game nil)
         (tree (new-mc-tree orig-game)) ; Get the tree created for this thread
         (tree-copy (new-mc-tree orig-game))
         (tree-holder nil)

         (times (floor (/ (random total-sims) (- *mc-rounds* 1))))   ; Add some randomness so the threads aren't all 
         ; trying to update the main tree at once
         (sims-so-far 0)
         )
    (dotimes (j *mc-rounds*)
      (dotimes (i times)
        ;; Make a copy of the game state
        (setq game (deep-copy-go orig-game))
        ;; Run sim-tree
        (setq state-move-list (sim-tree game tree c))
        ;; Run default
        (setq z (sim-default game))
        ;; Run backup
        (backup (mc-tree-hashy tree) state-move-list z))

      ;; Update the copy
      (merge-mc-trees! tree-copy tree)

      
      (with-locked-structure (queue)
                              ;; Add your tree to the queue
                             (push tree-copy (tree-q-q queue))
                             ;; If there are at least two trees on the q
                             (when (length (tree-q-q))
                               ;; Spawn a thread to merge and update them
                               (mp:process-run-function #'maintain-tree-q (append (mp:process-name) "-Child") queue )))

      ;; When the thread popped a tree
;     (when tree-holder
;       (merge-mc-trees! tree tree-holder)
;       ;; Update the main tree
;       (with-locked-structure (queue)
;                              (merge-mc-trees! (tree-q-ref-tree queue) tree))

;       (setq tree-holder nil)
;       )
      ;; Update tree from the main tree
      (merge-mc-trees! tree (tree-q-ref-tree queue))

      ;; Update info
      (incf sims-so-far times)

      ;; Except for the last round
      (if (and (not (= total-sims sims-so-far)) 
               (< j (- *mc-rounds* 1)))

        ;; Get times for next round
        (setq times (random (- total-sims sims-so-far)))

        ;; On the last round do the remaining sims
        (setq times (- total-sims sims-so-far))))

    ;; Pass through the barrier to signal the thread is done 
    (mp:barrier-pass-through barrier)))




(defun maintain-tree-q (queue)
  (let ((tree1 nil)
        (tree2 nil)
        )

      ;; Critical
      (with-locked-structure (queue)
                             ;; Get the trees
                             (setq tree1 (pop (tree-q-q queue)))
                             (setq tree2 (pop (tree-q-q queue))))

      ;; Merge them
      (merge-mc-trees! tree1 tree2)

      ;; Add the info to the main tree
      (with-locked-structure (queue)
                             (merge-mc-trees! (tree-q-q queue) tree1))))
