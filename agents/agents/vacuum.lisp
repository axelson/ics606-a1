;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Jason Axelson and Chris Ho

(progn
  ;;Constants
  (defparameter +maxFill+ 0.4 "Maximum amount of things agent can hold")

  ;;Global Variables
  (defparameter *explored* NIL "true if entire map has been explored")
  (defparameter *moveNo* 1 "total number of times vacuum has moved")
  (defparameter *visitNo* 1 "total ")
  (defparameter *stepCounter* 0)
  ;; TODO read *mapX* and *mapY* from the *map* file
  (defparameter *mapX* 12)
  (defparameter *mapY* 8)
  (defparameter *currX* 1)
  (defparameter *currY* 1)
  (defparameter *heading* 1)
  (defparameter *choiceDir* 0)
  (defparameter *amount* 0)
  (defparameter *plan* 0 "1 when there is a planned path")
  (defparameter *planX* 0)
  (defparameter *planY* 0)
  (defparameter *goHome* 0)
  (defparameter *lastMove* -1)
  (defparameter *map* (make-array (list *mapY* *mapX*) :initial-element 0))
  (defparameter *visited* (make-array (list *mapY* *mapX*) :initial-element 0)
    "Map showing when each cell was last visited")
  ;;(defparameter *seenCurrentPatrol* (make-array (list *mapY* *mapX*) :initial-element 0)
    ;;"Map showing when each cell was last visited while patrolling")
  (defparameter *floodMap* (make-array (list *mapY* *mapX*))))

(defun chris-play ()
  "Reset vacuum variables and launches vacuum"
  (let ((room-x-y-cons (read-room)))
    (setf *explored* NIL)
    (setf *moveNo* 1)
    (setf *visitNo* 1)
    (setf *stepCounter* 0)
    ;; TODO read *mapX* and *mapY* from the *map* file
    (setf *mapX* (car room-x-y-cons))
    (setf *mapY* (cdr room-x-y-cons))
    (setf *currX* 1)
    (setf *currY* 1)
    (setf *heading* 1)
    (setf *choiceDir* 0)
    (setf *amount* 0)
    (setf *plan* 0)
    (setf *planX* 0)
    (setf *planY* 0)
    (setf *goHome* 0)
    (setf *lastMove* -1)
    (setf *map* (make-array (list *mapY* *mapX*) :initial-element 0))
    (setf *visited* (make-array (list *mapY* *mapX*) :initial-element 0))
    ;;(setf *seenCurrentPatrol* (make-array (list *mapY* *mapX*) :initial-element 0))
    (setf *floodMap* (make-array (list *mapY* *mapX*)))
    (format t "All variables have been reset")
    ;; Create walls
    (createWalls)

    (run-environment (make-vacuum-world :aspec '(jason-vacuum)))))

;;;; Some simple agents for the vacuum world

(defstructure (stupid-vacuum
   (:include agent
    (program
     #'(lambda (percept)
         (destructuring-bind (bump dirt home directions dirlist furnitureList catList charge fillPercent) percept
           (read-line)
           (format t "~%Output: ~A ~A ~A ~A ~A ~A ~A ~A ~A" ;Want to get rid of some style-warnings
                   bump dirt home directions dirlist furnitureList catList charge fillPercent)
	   (cond (dirt 'suck)
		 (bump
		  (format t "Bump!!~%")
		  '(turn right))
		 (home 'dump)
                 (t 'up)))))))
		 ;;(t (random-element '(forward forward forward shut-off
		;;			      (turn right) (turn left))))))))))
  "A very stupid agent: ignore percept and choose a random action.")

(defstructure (random-vacuum-agent 
   (:include agent
    (program 
     #'(lambda (percept)
	 (declare (ignore percept))
	 (random-element 
	  '(suck forward (turn right) (turn left) shut-off))))))
  "A very stupid agent: ignore percept and choose a random action.")

(defstructure (reactive-vacuum-agent 
   (:include agent
    (program 
     #'(lambda (percept)
	 (destructuring-bind (bump dirt home) percept
	   (cond (dirt 'suck)
		 (home (random-element '(shut-off forward (turn right))))
		 (bump (random-element '((turn right) (turn left))))
		 (t (random-element '(forward forward forward
					      (turn right) (turn left))))))))))
  "When you bump, turn randomly; otherwise mostly go forward, but
  occasionally turn.  Always suck when there is dirt.")

(defun jason-vacuum-agent (percept)
  (destructuring-bind (bump dirt atHome directionsList dirtList catList furnitureList charge fillPercent) percept
    (format t "~%BEGINNING OF CODE~%")
    ;;(if (> *moveNo* 48)
    (read-line)

    ;; If there was a bump, undo last move (if applicable)
    (when bump
      (setf *plan* 0)
      (undoLastMove))

    ;; Update state
    (updateMap directionsList dirtList catList furnitureList)
    (updateHeading)

    (if (not (eq *choiceDir* -1))
        (updateVisited))

    ;;(format t "~%currX: ~A" *currX*)
    ;;(format t "~%currY: ~A" *currY*)
    ;;(format t "~%Heading: ")
    ;;(cond
    ;;((eq 0 *heading*) (format t "North"))
    ;;((eq 1 *heading*) (format t "East"))
    ;;((eq 2 *heading*) (format t "South"))
    ;;((eq 3 *heading*) (format t "West"))
    ;;(T (format t "INVALID")))
    ;;(format t "~%")
    ;;(format t "~%Output:~%")

    ;; Maps
    ;;(printMap *visited* *mapY* *mapX*)
    ;;(format t "~%")
    (format t "*map*:~%")
    (printMap *map* *mapY* *mapX*)

    ;;(format t "*seenCurrentPatrol*:~%")
    ;;(printMap *seenCurrentPatrol* *mapX* *mapY*)

    ;;(format t "~%")
    ;;(printMap *floodMap* *mapY* *mapX*)

    ;; If at home and need to dump or charge, do so

    ;; Check if need to charge
    (when (needCharge? charge)
      (goHome))

    ;; Check if need to dump
    (when (needDump? fillPercent)
      (goHome))

    ;; Check if destination has been reached on planned path
    (when (and (eq *plan* 1)
               (and (eq *currX* *planX*)
                    (eq *currY* *planY*)))
      (setf *plan* 0)                   ;No longer planning a path
      (when (and (eq 1 *planX*) (eq 1 *planY*))
        (setf *goHome* 0)))

      (when *explored*
        (format t "ROOM EXPLORED! Now Patrolling (much better than before!) explored: ~A~%" *explored*)
        (setf *explored* T)
        (format t "ROOM EXPLORED! Now Patrolling (much better than before!) explored: ~A~%" *explored*)
        (patrol))

    ;; Final action
    (cond
      ;; If dirt in current cell, suck it (unless currently going home)
      ((and dirt (eq *goHome* 0))
       (setf *choiceDir* -1)
       (updateAction 'suck))

      ;; If at home check if need to charge or dump
      ((and atHome (needCharge? charge))
       (updateAction 'charge))
      ((and atHome (needDump? fillPercent))
       (updateAction 'dump))
      
      ;; There's currently a plan, so follow it
      ((eq *plan* 1)
       (let ((*choiceDir* 0)
             (*amount* (aref *floodMap* (1+ *currY*) *currX*)))
         ;; East
         (if (< (aref *floodMap* *currY* (1+ *currX*)) *amount*)
             (progn
               (setf *amount* (aref *floodMap* *currY* (1+ *currX*)))
               (setf *choiceDir* 1)))
         ;; South
         (if (< (aref *floodMap* (1- *currY*) *currX*) *amount*)
             (progn
               (setf *amount* (aref *floodMap* (1- *currY*) *currX*))
               (setf *choiceDir* 2)))
         ;; West
         (if (< (aref *floodMap* *currY* (1- *currX*)) *amount*)
             (progn
               (setf *amount* (aref *floodMap* *currY* (1- *currX*)))
               (setf *choiceDir* 3)))
         
         ;; Action
         (cond
           ((eq 0 *choiceDir*) (updateAction 'up))
           ((eq 1 *choiceDir*) (updateAction 'right))
           ((eq 2 *choiceDir*) (updateAction 'down))
           ((eq 3 *choiceDir*) (updateAction 'left))
           (t (format t "this should not happen: *choiceDir* = ~A~%" *choiceDir*)
            NIL))
         ))
      
      (T
       ;; North
       (let ((amountT (aref *map* (1+ *currY*) *currX*))
             (visitNoT (aref *visited* (1+ *currY*) *currX*)))
         ;; TODO why are the global variables being set here
         (setf *amount* amountT)
         (setf *visitNo* visitNoT)
         (setf *choiceDir* 0))
       ;; East
       (let ((amountT (aref *map* *currY* (1+ *currX*)))
             (visitNoT (aref *visited* *currY* (1+ *currX*))))
         (if (or (> amountT *amount*) (and (eq *amount* amountT) (< visitNoT *visitNo*) (>= visitNoT 0)))
             (progn
               (setf *amount* amountT)
               (setf *visitNo* visitNoT)
               (setf *choiceDir* 1))))
       ;; South
       (let ((amountT (aref *map* (1- *currY*) *currX*))
             (visitNoT (aref *visited* (1- *currY*) *currX*)))
         (if (or (> amountT *amount*) (and (eq *amount* amountT) (< visitNoT *visitNo*) (>= visitNoT 0)))
             (progn
               (setf *amount* amountT)
               (setf *visitNo* visitNoT)
               (setf *choiceDir* 2))))
       ;; West
       (let ((amountT (aref *map* *currY* (1- *currX*)))
             (visitNoT (aref *visited* *currY* (1- *currX*))))
         (if (or (> amountT *amount*) (and (eq *amount* amountT) (< visitNoT *visitNo*) (>= visitNoT 0)))
             (progn
               (setf *amount* amountT)
               (setf *visitNo* visitNoT)
               (setf *choiceDir* 3))))

       
       (format t "~%Visited Map:~%")
       (printMap *visited* *mapY* *mapX*)
       (if (allAdjVisited)
           (progn
             (format t "NO MORE NOTHING!!!!~%")
             (moveToClosest))

           (cond
             ((eq 0 *choiceDir*) (updateAction 'up))
             ((eq 1 *choiceDir*) (updateAction 'right))
             ((eq 2 *choiceDir*) (updateAction 'down))
             ((eq 3 *choiceDir*) (updateAction 'left))
             (T (format t "This should never happen: *choiceDir* is ~A, shutting off~%" *choiceDir*)
              (updateAction 'shut-off))))
       )
      (T (format t "This should not happen, unable to choose what to do, so doing nothing (nil)~%")
       NIL))))

(defstructure
    (jason-vacuum 
     (:include
      agent
      (program
       #'jason-vacuum-agent)))
    "A very stupid agent")

;;Agent needs
(defun needCharge? (charge)
  "Checks if the agent currently needs a charge"
  ;;TODO - figure out what this does
  (< charge (* (/ 0.25 2) (* *mapY* *mapX*))))

(defun needDump? (fillPercent)
  "Checks if the agent currently needs to dump"
  (> fillPercent (* 0.75 +maxFill+)))

(defun updateHeading ()
  "Update *heading*"
  (setf *heading* (mod *heading* 4)))

(defun updateVisited ()
  (setf (aref *visited* *currY* *currX*) *moveNo*)
  (incf *moveNo*)
  )

;; (defun updateSeen ()
;;   "Updates adjacent squares to say they've been seen"
;;   ;; Assumes updateMap was called
;;   (let ((thisMap *seenCurrentPatrol*))
;;     ;;Current square
;;     (setMapInDirection thisMap 'current T)
;;     ;;North
;;     (setMapInDirection thisMap 'north T)
;;     ;;East
;;     (setMapInDirection thisMap 'east T)
;;     ;;South
;;     (setMapInDirection thisMap 'south T)
;;     ;;West
;;     (setMapInDirection thisMap 'west T)
;;     ))

(defun updateMap (directionsList dirtList catList furnitureList)
  "Updates *map* accordingly"
  (let ((tempDirect (car directionsList))
        (tempDirt (car dirtList))
        (tempFurn (car furnitureList))
        (tempCat (car catList)))
    ;; North
    (cond
      ((numberp tempDirt) (setf (aref *map* (1+ *currY*) *currX*) (1+ tempDirt)))
      (tempFurn (progn (setf (aref *map* (1+ *currY*) *currX*) -1)
                       (setf (aref *visited* (1+ *currY*) *currX*) -1)))
      (tempCat (setf (aref *map* (1+ *currY*) *currX*) 0))
      (tempDirect (setf (aref *map* (1+ *currY*) *currX*) 1))
      ;; Walls
      (T (progn (setf (aref *map* (1+ *currY*) *currX*) -1)
                (setf (aref *visited* (1+ *currY*) *currX*) -1)))))
  
  ;; East
  (let ((tempDirect (cadr directionsList))
        (tempDirt (cadr dirtList))
        (tempFurn (cadr furnitureList))
        (tempCat (cadr catList)))

    (cond
      ((numberp tempDirt) (setf (aref *map* *currY* (1+ *currX*)) (1+ tempDirt)))
      (tempFurn (progn (setf (aref *map* *currY* (1+ *currX*)) -1)
                       (setf (aref *visited* *currY* (1+ *currX*)) -1)))
      (tempCat (setf (aref *map* *currY* (1+ *currX*)) 0))
      (tempDirect (setf (aref *map* *currY* (1+ *currX*)) 1))
      ;; Walls
      (T (progn (setf (aref *map* *currY* (1+ *currX*)) -1)
                (setf (aref *visited* *currY* (1+ *currX*)) -1)))))

  ;; South
  (let ((tempDirect (caddr directionsList))
        (tempDirt (caddr dirtList))
        (tempFurn (caddr furnitureList))
        (tempCat (caddr catList)))

    (cond
      ((numberp tempDirt) (setf (aref *map* (1- *currY*) *currX*) (1+ tempDirt)))
      (tempFurn (progn (setf (aref *map* (1- *currY*) *currX*) -1)
                       (setf (aref *visited* (1- *currY*) *currX*) -1)))
      (tempCat (setf (aref *map* (1- *currY*) *currX*) 0))
      (tempDirect (setf (aref *map* (1- *currY*) *currX*) 1))
      ;; Walls
      (T (progn (setf (aref *map* (1- *currY*) *currX*) -1)
                (setf (aref *visited* (1- *currY*) *currX*) -1)))))

  ;; West
  (let ((tempDirect (cadddr directionsList))
        (tempDirt (cadddr dirtList))
        (tempFurn (cadddr furnitureList))
        (tempCat (cadddr catList)))

    (cond
      ((numberp tempDirt) (setf (aref *map* *currY* (1- *currX*)) (1+ tempDirt)))
      (tempFurn (progn (setf (aref *map* *currY* (1- *currX*)) -1)
                       (setf (aref *visited* *currY* (1- *currX*)) -1)))
      (tempCat (setf (aref *map* *currY* (1- *currX*)) 0))
      (tempDirect (setf (aref *map* *currY* (1- *currX*)) 1))
      ;; Walls
      (T (progn (setf (aref *map* *currY* (1- *currX*)) -1)
                (setf (aref *visited* *currY* (1- *currX*)) -1)))))
  )

(defun updateAction (action)
  "Performs various updates per action taken and returns action to be done"
  ;; suck forward turn (L,R) shut-off up down left right charge dump
  (cond
    ((eq action 'suck) (progn
			 (if (> (aref *map* *currY* *currX*) 1)
			     (decf (aref *map* *currY* *currX*)))
			 'suck))
    ((eq action 'forward) (progn
			    (cond
			      ((eq *heading* 0) (incf *currY*))
			      ((eq *heading* 1) (incf *currX*))
			      ((eq *heading* 2) (decf *currY*))
			      ((eq *heading* 3) (decf *currX*))
			      )
			    'forward))
    ((eq action 'turnL) (progn
			  (decf *heading*)
			  '(turn left)))
    ((eq action 'turnR) (progn
			  (incf *heading*)
			  '(turn right)))
    ((eq action 'shut-off) (progn
			     (format t "shut-off")))
    ((eq action 'up) (progn
		       (setf *lastMove* 0)
		       (setf *heading* 0)
		       (incf *currY*)
		       'up))
    ((eq action 'down) (progn
			 (setf *lastMove* 2)
			 (setf *heading* 2)
			 (decf *currY*)
			 'down))
    ((eq action 'left) (progn
			 (setf *lastMove* 3)
			 (setf *heading* 3)
			 (decf *currX*)
			 'left))
    ((eq action 'right) (progn
			  (setf *lastMove* 1)
			  (setf *heading* 1)
			  (incf *currX*)
			  'right))
    ((eq action 'charge)
     'charge)
    ((eq action 'dump)
     'dump)
    (T
     (format t "updateAction: Unable to do action ~A, doing nil instead~%" action)
     nil)))

(defun undoLastMove ()
  (cond
    ((eq *lastMove* 0) (decf *currY*))
    ((eq *lastMove* 1) (decf *currX*))
    ((eq *lastMove* 2) (incf *currY*))
    ((eq *lastMove* 3) (incf *currX*)))
)

(defun goHome ()
  (moveTo 1 1)
  (setf *goHome* 1)
)

(defun moveTo (destX destY)
  (planPath destX destY)
  (setf *planX* destX)
  (setf *planY* destY)
)

(defun planPath (destX destY)
  (floodDest destX destY)
  (setf *plan* 1)
)

(defun initialFlood ()
  (loop for i from 0 to (1- *mapY*) do
       (loop for j from 0 to (1- *mapX*) do
		      (setf (aref *floodMap* i j) (* *mapY* *mapX*))))
)

(defun floodDest (destX destY)
  ;; Initial flood values
  (loop for i from 0 to (1- *mapY*) do
    (loop for j from 0 to (1- *mapX*) do
      (if (and (eq i destY) (eq j destX))
          (setf (aref *floodMap* i j) 0)
          (setf (aref *floodMap* i j) (* *mapY* *mapX*)))))

  ;; Continue fill
  (let ((*stepCounter* 1))
    (loop while (and (not (fillStatus)) (< *stepCounter* (* *mapY* *mapX*))) do
      (progn
        (loop for i from 0 to (1- *mapY*) do
          (loop for j from 0 to (1- *mapX*) do
            (if (eq (aref *floodMap* i j) (1- *stepCounter*))
                (assign j i))))
        (incf *stepCounter*))))
  )

(defun assign (coorX coorY)
  (let ((x coorX) (y (1+ coorY)))
    ;; North
    (if (and (> (aref *floodMap* y x) *stepCounter*) (> (aref *map* y x) 0))
	(setf (aref *floodMap* y x) *stepCounter*))
    ;; East
    (setf x (1+ coorX))
    (setf y coorY)
    (if (and (> (aref *floodMap* y x) *stepCounter*) (> (aref *map* y x) 0))
	(setf (aref *floodMap* y x) *stepCounter*))
    ;; South
    (setf x coorX)
    (setf y (1- coorY))
    (if (and (> (aref *floodMap* y x) *stepCounter*) (> (aref *map* y x) 0))
	(setf (aref *floodMap* y x) *stepCounter*))
    ;; West
    (setf x (1- coorX))
    (setf y coorY)
    (if (and (> (aref *floodMap* y x) *stepCounter*) (> (aref *map* y x) 0))
	(setf (aref *floodMap* y x) *stepCounter*))
    )
  )

(defun fillStatus ()
  (let ((fillStatus T))
    (loop for i from 0 to (1- *mapY*) do
      (loop for j from 0 to (1- *mapX*) do
        (if (and (> (aref *map* i j) 0) (eq (aref *floodMap* i j) (* *mapY* *mapX*)))
            (setf fillStatus NIL)))))
  )

(defun printMap (mapToPrint height width)
  "Prints the main Map"
  (loop for i from (1- height) downto 0
        do (progn (loop for j from 0 to (1- width)
                        do (format t " ~A " (aref mapToPrint i j)))
                  (format t "~%"))))

(defun findNext ()
  (let ((dir 0) (x *currX*) (y *currY*))
    (loop while (eq *plan* 0) do
      (setf dir (findLastAdj x y (aref *visited* y x)))
      (cond
        ((eq dir 0) (incf y))
        ((eq dir 1) (incf x))
        ((eq dir 2) (decf y))
        ((eq dir 3) (decf x))))
    ;; Action
    (format t "MOVING TO: (~A ~A)" x y)
    (moveTo x y))
  )

(defun findLastAdj (x y currValue)
  (cond
    ;; North
    ((or (eq (aref *visited* (1+ y) x) 0) (eq (aref *visited* (1+ y) x) (1- currValue)))
     (progn
       (if (eq (aref *visited* (1+ y) x) 0)
	   (setf *plan* 1))
       0))
    ;; East
    ((or (eq (aref *visited* y (1+ x)) 0) (eq (aref *visited* y (1+ x)) (1- currValue)))
     (progn
       (if (eq (aref *visited* y (1+ x)) 0)
	   (setf *plan* 1))
       1))
    ;; South
    ((or (eq (aref *visited* (1- y) x) 0) (eq (aref *visited* (1- y) x) (1- currValue)))
     (progn
       (if (eq (aref *visited* (1- y) x) 0)
	   (setf *plan* 1))
       2))
    ;; West
    ((or (eq (aref *visited* y (1- x)) 0) (eq (aref *visited* y (1- x)) (1- currValue)))
     (progn
       (if (eq (aref *visited* y (1- x)) 0)
	   (setf *plan* 1))
       3))
    ;; If there is no lower value, the current cell is the lowest value
    (T (progn (setf *plan* 1) -1)))
  )

(defun allAdjVisited ()
  (if (and
       (or (eq -1 (aref *visited* (1+ *currY*) *currX*)) (> (aref *visited* (1+ *currY*) *currX*) 0))
       (or (eq -1 (aref *visited* *currY* (1+ *currX*))) (> (aref *visited* *currY* (1+ *currX*)) 0))
       (or (eq -1 (aref *visited* (1- *currY*) *currX*)) (> (aref *visited* (1- *currY*) *currX*) 0))
       (or (eq -1 (aref *visited* *currY* (1- *currX*))) (> (aref *visited* *currY* (1- *currX*)) 0)))
      T
      NIL)
  )

(defun anyAdjVisited (x y)
  (if (or
       (> (aref *map* (1+ y) x) 0)
       (> (aref *map*  y (1+ x)) 0)
       (> (aref *map* (1- y) x) 0)
       (> (aref *map*  y (1- x)) 0))
      T
      NIL)
  )

(defun moveToClosest ()
  (generalFlood)
  (if (not *explored*)
      (traceToClosest)
      (progn
        ;; Patrol
        ;; TODO fill in
        ))
  )


(defun findCandidates ()
  (let ((count 0))
    (loop for i from 1 to (1- *mapY*) do
      (loop for j from 1 to (1- *mapX* ) do
        (if (and (or (eq (aref *map* i j) 0) (> (aref *map* i j) 1)) (anyAdjVisited j i))
            (progn
              (setf (aref *floodMap* i j) 0)
              (incf count)))))
    (format t "CANDIDATE COUNT: ~A~%" count)
    (if (eq 0 count)
	(progn
	  (setf *explored* T)
	  (format t "~%findCandidates: Set MAP EXPLORED!")))
    ))

(defun generalFlood ()
  (initialFlood)
  (findCandidates)
  ;; Continue fill
  (let ((*stepCounter* 1))
    (loop while (and (not (fillStatus)) (< *stepCounter* (* *mapY* *mapX*))) do
      (progn
        (loop for i from 0 to (1- *mapY*) do
          (loop for j from 0 to (1- *mapX*) do
            (if (eq (aref *floodMap* i j) (1- *stepCounter*))
                (assign j i))))
        (incf *stepCounter*)))
    (printMap *floodMap* *mapY* *mapX*))
  )

(defun patrol ()
  "Called after entire *map* has been *visited*, used to clean up after cats"
  (format t "*map*~%")
  (printMap *map* *mapY* *mapX*)

  ;;(format t "*seenCurrentPatrol*~%")
  ;;(printMap *seenCurrentPatrol* *mapX* *mapY*)
  (format t "Now Patrolling~%")

  ;; Start at home
  (goHome)

  (when (atHome?)
    ;; Reset maps
    (setf *visited* (make-array (list *mapY* *mapX*) :initial-element 0))
    (setf *map* (make-array (list *mapY* *mapX*) :initial-element 0))
    ;; Reset explored status
    (setf *explored* NIL)
    (createWalls))
  )

(defun traceToClosest ()
  (format t "IN TRACE TO CLOSEST")
  (let* ((x *currX*) (y *currY*) (dir 0)
         (fValue (aref *floodMap* y x)))
    (format t "fValue: ~A~%" fValue)
    (format t "Curr: ~A ~A ~%" x y)
    (if (> fValue 0)
	(progn
	  (loop while (> fValue 0) do
            (progn
              ;; North
              (if (< (aref *floodMap* (1+ y) x) fValue)
                  (progn
                    (setf fValue (aref *floodMap* (1+ y) x))
                    (setf dir 0)))
              ;; East
              (if (< (aref *floodMap* y (1+ x)) fValue)
                  (progn
                    (setf fValue (aref *floodMap* y (1+ x)))
                    (setf dir 1)))
              ;; South
              (if (< (aref *floodMap* (1- y) x) fValue)
                  (progn
                    (setf fValue (aref *floodMap* (1- y) x))
                    (setf dir 2)))
              ;; West
              (if (< (aref *floodMap* y (1- x)) fValue)
                  (progn
                    (setf fValue (aref *floodMap* y (1- x)))
                    (setf dir 3)))
              (cond
                ((eq 0 dir) (incf y))
                ((eq 1 dir) (incf x))
                ((eq 2 dir) (decf y))
                ((eq 3 dir) (decf x))
                (T (format t "SHOULD NOT HAPPEN")))))
	  (format t "MOVE TO: ~A ~A~%" x y)
	  (moveTo x y))
	(progn
	  (setf (aref *visited* y x) *visitNo*)
	  (incf *visitNo*))))
  )


;; Helper
(defun getXCoordinate (direction)
  "Returns x coordinate in the given direction from the current location"
  (cond
    ((eq direction 'current) *currX*)
    ((eq direction 'north) *currX*)
    ((eq direction 'south) *currX*)
    ((eq direction 'east) (1- *currX*))
    ((eq direction 'west) (1+ *currX*))
    (t (format t "Error: getXCoordinate: direction ~A not recognized~%" direction))))

(defun getYCoordinate (direction)
  "Returns y coordinate in the given direction from the current location"
  (cond
    ((eq direction 'current) *currY*)
    ((eq direction 'north) (1+ *currY*))
    ((eq direction 'south) (1- *currY*))
    ((eq direction 'east) *currY*)
    ((eq direction 'west) *currY*)
    (t (format t "Error: getYCoordinate: direction ~A not recognized~%" direction))))

(defun atHome? ()
  "Checks if the vacuum is currently at home"
  (if (and (eq *currX* 1)
           (eq *currY* 1))
      T
      NIL))

(defun setMapInDirection (thisMap direction value)
  (setf (aref thisMap (getXCoordinate direction) (getYCoordinate direction)) value))

;;  ((numberp tempDirt) (setf (aref *map* (1+ *currY*) *currX*) (1+ tempDirt)))

(defun createWalls ()
  (loop for i from 0 to (1- *mapY*) do
    (progn
      (setf (aref *map* i 0) -1)
      (setf (aref *visited* i 0) -1)
      (setf (aref *map* i (1- *mapX*)) -1)
      (setf (aref *visited* i (1- *mapX*)) -1)))
  (loop for i from 0 to (1- *mapX*) do
    (progn
      (setf (aref *map* 0 i) -1)
      (setf (aref *visited* 0 i) -1)
      (setf (aref *map* (1- *mapY*) i) -1)
      (setf (aref *visited* (1- *mapY*) i) -1))))
