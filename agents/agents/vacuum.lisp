;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

(progn
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
  (defparameter *plan* 0)
  (defparameter *planX* 0)
  (defparameter *planY* 0)
  (defparameter *goHome* 0)
  (defparameter *lastMove* -1)
  (defparameter *map* (make-array (list *mapY* *mapX*) :initial-element 0))
  (defparameter *visited* (make-array (list *mapY* *mapX*) :initial-element 0)
    "Map showing when each cell was last visited")
  (defparameter *floodMap* (make-array (list *mapY* *mapX*))))

(defun chris-play ()
  "Reset vacuum variables and launches vacuum"
  (setf *explored* NIL)
  (setf *moveNo* 1)
  (setf *visitNo* 1)
  (setf *stepCounter* 0)
  ;; TODO read *mapX* and *mapY* from the *map* file
  (setf *mapX* 12)
  (setf *mapY* 8)
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
  (setf *mapExplored* NIL)
  (setf *map* (make-array (list *mapY* *mapX*) :initial-element 0))
  (setf *visited* (make-array (list *mapY* *mapX*) :initial-element 0))
  (setf *floodMap* (make-array (list *mapY* *mapX*)))
  (format t "All variables have been reset")
                                        ; Create walls
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
      (setf (aref *visited* (1- *mapY*) i) -1)))

  (read-a-room "default.txt")
  (run-environment (make-vacuum-world :aspec '(jason-vacuum))))

;;;; Some simple agents for the vacuum world

(defstructure (stupid-vacuum
   (:include agent
    (program
     #'(lambda (percept)
         (destructuring-bind (bump dirt home directions dirlist furnitureList catList charge fillPercent) percept
           (read-line)
           (format t "~%Output: ")
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


(defstructure
    (jason-vacuum 
     (:include
      agent
      (program
       #'(lambda (percept)
	   (destructuring-bind (bump dirt home directionsList dirtList catList furnitureList charge fillPercent) percept
	     (format t "~%BEGINNING OF CODE~%")
             ;;(if (> *moveNo* 48)
             ;;(read-line))
	     (read-line)
	     (format t "Beginning of code")
	     ;; If there was a bump, undo last move (if applicable)
	     (if bump
		 (progn
		   (setf *plan* 0)
		   (undoLastMove)))

	     (updateMap percept)
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
	     ;;(printDamnMap *visited* *mapY* *mapX*)
	     ;;(format t "~%")
	     (printDamnMap *map* *mapY* *mapX*)
	     ;;(format t "~%")
	     ;;(printDamnMap *floodMap* *mapY* *mapX*)

	     ;; Charge
	     (if (< charge (* (/ 0.25 2) (* *mapY* *mapX*)))
		 (*goHome*))

	     ;; Check for status on planned path
	     (if (and (eq *plan* 1) (and (eq *currX* *planX*) (eq *currY* *planY*)))
		 (progn
		   (setf *plan* 0)
		   (if (and (eq 1 *planX*) (eq 1 *planY*))
		       (setf *goHome* 0))))

	     (when *explored*
               (format t "ROOM EXPLORED! Now Patrolling (much better than before!)")
               (setf *mapExplored* T)
               (patrol))

	     ;; Final action
	     (if (and dirt (eq *goHome* 0))
		 (progn
		   (setf *choiceDir* -1)
		   (updateAction 'suck))
		 (cond
		   ((eq *plan* 1)
		    (progn
		      (let ((*choiceDir* 0) (*amount* (aref *floodMap* (1+ *currY*) *currX*)))
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
			  ((eq 3 *choiceDir*) (updateAction 'left)))
                        )))
		   
		   (T (progn
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

                        
			(format t "~%Visited~%")
			(printDamnMap *visited* *mapY* *mapX*)
			(if (allAdjVisited)
			    (progn
			      (format t "NO MORE NOTHING!!!!~%")
			      (moveToClosest))

			    (cond
			      ((eq 0 *choiceDir*) (updateAction 'up))
			      ((eq 1 *choiceDir*) (updateAction 'right))
			      ((eq 2 *choiceDir*) (updateAction 'down))
			      ((eq 3 *choiceDir*) (updateAction 'left))
			      (T (progn
				   (flood)
				   (updateAction 'shut-off)))))
                        )))))))))
    "A very stupid agent")

(defun updateHeading ()
  "Update *heading*"
  (setf *heading* (mod *heading* 4)))

(defun updateVisited ()
  (setf (aref *visited* *currY* *currX*) *moveNo*)
  (incf *moveNo*)
  )

(defun updateMap (percept)
  "Updates *map* accordingly"
  (destructuring-bind (bump dirt home directionsList dirtList catList furnitureList charge fillPercent) percept
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
    ))

(defun updateAction (action)
  "Performs various updates per action taken"
  ; suck forward turn (L,R) shut-off up down left right
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
			  'right))))

(defun undoLastMove ()
  (cond
    ((eq *lastMove* 0) (decf *currY*))
    ((eq *lastMove* 1) (decf *currX*))
    ((eq *lastMove* 2) (incf *currY*))
    ((eq *lastMove* 3) (incf *currX*)))
)

(defun *goHome* ()
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
  ;; North
  (let ((x coorX) (y (1+ coorY)))
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

(defun printDamnMap (thisMap height width)
  "Prints the damn thisMap"
  (loop for i from (1- height) downto 0
       do (progn (loop for j from 0 to (1- width)
	       do (format t " ~A " (aref thisMap i j)))
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
      (return-from allAdjVisited T)
      (return-from allAdjVisited NIL))
  )

(defun anyAdjVisited (x y)
  (if (or
       (> (aref *map* (1+ y) x) 0)
       (> (aref *map*  y (1+ x)) 0)
       (> (aref *map* (1- y) x) 0)
       (> (aref *map*  y (1- x)) 0))
      (return-from anyAdjVisited T)
      (return-from anyAdjVisited NIL))
  )

(defun moveToClosest ()
  (generalFlood)
  (if (not *explored*)
      (traceToClosest))
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
	  (format t "~%MAP EXPLORED!")))
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
    (printDamnMap *floodMap* *mapY* *mapX*))
  )

(defun patrol ()
  "Called after entire *map* has been *visited*, used to clean up after cats"
  (printDamnMap *map* *mapY* *mapX*)
  
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


