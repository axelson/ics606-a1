;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Some simple agents for the vacuum world

(defstructure (stupid-vacuum
   (:include agent
    (program
     #'(lambda (percept)
         (destructuring-bind (bump dirt home directions dirlist furnitureList catList charge) percept
           (read-line)
           (format t "~%Output: ")
	   (cond (dirt 'suck)
		 (bump
		  (format t "Bump!!~%")
		  '(turn right))
		 (home 'forward)
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
	   (destructuring-bind (bump dirt home directionsList dirtList catList furnitureList) percept

	     ;(if (> moveNo 48)
		 ;(read-line))
	     (read-line)
	     ;; If there was a bump, undo last move (if applicable)
	     (if bump
		   (undoLastMove))

	     (updateMap percept)
	     (updateHeading)
	     (if (not (eq choiceDir -1))
		 (updateVisited))

	     ;(format t "~%currX: ~A" currX)
	     ;(format t "~%currY: ~A" currY)
	     ;(format t "~%Heading: ")
	     ;(cond
	       ;((eq 0 heading) (format t "North"))
	       ;((eq 1 heading) (format t "East"))
	       ;((eq 2 heading) (format t "South"))
	       ;((eq 3 heading) (format t "West"))
	       ;(T (format t "INVALID")))
	     ;(format t "~%")
	     ;(format t "~%Output:~%")

	     ;; Maps
	     (printDamnMap visited mapY mapX)
	     (format t "~%")
	     (printDamnMap map mapY mapX)
	     (format t "~%")
	     ;;(printDamnMap floodMap mapY mapX)


	     ;(if (and (eq moveNo 50) (eq plan 0))
		; (goHome))
	     
	     ;; Check for status on planned path
	     (if (and (eq plan 1) (and (eq currX planX) (eq currY planY)))
		 (progn
		   (setf plan 0)
		   (if (and (eq 1 planX) (eq 1 planY))
		       (setf goHome 0))))

	     ;; Final action
	     (if (and dirt (eq goHome 0))
		 (progn
		   (setf choiceDir -1)
		   (updateAction 'suck))
		 (cond
		   ((eq plan 1)
		    (progn
		      (let ((choiceDir 0) (amount (aref floodMap (1+ currY) currX)))
			;; East
			(if (< (aref floodMap currY (1+ currX)) amount)
			    (progn
			      (setf amount (aref floodMap currY (1+ currX)))
			      (setf choiceDir 1)))
			;; South
			(if (< (aref floodMap (1- currY) currX) amount)
			    (progn
			      (setf amount (aref floodMap (1- currY) currX))
			      (setf choiceDir 2)))
			;; West
			(if (< (aref floodMap currY (1- currX)) amount)
			    (progn
			      (setf amount (aref floodMap currY (1- currX)))
			      (setf choiceDir 3)))

			;; Action
			(cond
			  ((eq 0 choiceDir) (updateAction 'up))
			  ((eq 1 choiceDir) (updateAction 'right))
			  ((eq 2 choiceDir) (updateAction 'down))
			  ((eq 3 choiceDir) (updateAction 'left)))
		      )))
		   
		   (T (progn
			;; North
			(setf amountT (aref map (1+ currY) currX))
			(setf visitNoT (aref visited (1+ currY) currX))
			(setf amount amountT)
			(setf visitNo visitNoT)
			(setf choiceDir 0)

			;; East
			(setf amountT (aref map currY (1+ currX)))
			(setf visitNoT (aref visited currY (1+ currX)))
			(if (or (> amountT amount) (and (eq amount amountT) (< visitNoT visitNo) (> visitNoT 0)))
			    (progn
			      (setf amount amountT)
			      (setf visitNo visitNoT)
			      (setf choiceDir 1)))
				
			;; South
			(setf amountT (aref map (1- currY) currX))
			(setf visitNoT (aref visited (1- currY) currX))
			(if (or (> amountT amount) (and (eq amount amountT) (< visitNoT visitNo) (> visitNoT 0)))
			    (progn
			      (setf amount amountT)
			      (setf visitNo visitNoT)
			      (setf choiceDir 2)))

			;; West
			(setf amountT (aref map currY (1- currX)))
			(setf visitNoT (aref visited currY (1- currX)))
			(if (or (> amountT amount) (and (eq amount amountT) (< visitNoT visitNo) (> visitNoT 0)))
			    (progn
			      (setf amount amountT)
			      (setf visitNo visitNoT)
			      (setf choiceDir 3)))
			
			;;
			(if (eq (allAdjVisited) 1)
			    (progn
			      (format t "NO NOTHING!!!!")
			      (findNext))

			    (cond
			      ((eq 0 choiceDir) (updateAction 'up))
			      ((eq 1 choiceDir) (updateAction 'right))
			      ((eq 2 choiceDir) (updateAction 'down))
			      ((eq 3 choiceDir) (updateAction 'left))
			      (T (progn
				   (flood)
				   (updateAction 'shut-off)))))
			    )))))))))
     "A very stupid agent")

(defun chris-play ()
  "Reset vacuum variables and launches vacuum"
  (progn
    (defparameter suckNo 0)
    (defparameter moveNo 1)
    (defparameter mapX 12)
    (defparameter mapY 8)
    (defparameter currX 1)
    (defparameter currY 1)
    (defparameter heading 1)
    (defparameter choiceDir 0)
    (defparameter amount 0)
    (defparameter plan 0)
    (defparameter planX 0)
    (defparameter planY 0)
    (defparameter goHome 0)
    (defparameter lastMove -1)
    (defparameter mapExplored 0)
    (defparameter map (make-array (list mapY mapX) :initial-element 0))
    (defparameter visited (make-array (list mapY mapX) :initial-element 0))
    (defparameter floodMap (make-array (list mapY mapX)))
    (format t "All variables have been reset")
    ; Create walls
    (loop for i from 0 to (1- mapY) do
	 (progn
	   (setf (aref map i 0) -1)
	   (setf (aref visited i 0) -1)
	   (setf (aref map i (1- mapX)) -1)
	   (setf (aref visited i (1- mapX)) -1)))
    (loop for i from 0 to (1- mapX) do
	 (progn
	   (setf (aref map 0 i) -1)
	   (setf (aref visited 0 i) -1)
	   (setf (aref map (1- mapY) i) -1)
	   (setf (aref visited (1- mapY) i) -1)))

    (read-a-room "a1inputspecification.txt")
    (run-environment (make-vacuum-world :aspec '(jason-vacuum)))))

(defun updateHeading ()
  "Update heading"
  (setf heading (mod heading 4)))

(defun updateVisited ()
  (setf (aref visited currY currX) moveNo)
  (incf moveNo)
  )

(defun updateMap (percept)
  "Updates map accordingly"
  (progn
     (destructuring-bind (bump dirt home directionsList dirtList catList furnitureList) percept
       ;; North
       (setf tempDirect (car directionsList))
       (setf tempDirt (car dirtList))
       (setf tempFurn (car furnitureList))
       (setf tempCat (car catList))

       (cond
	 ((numberp tempDirt) (setf (aref map (1+ currY) currX) (1+ tempDirt)))
	 (tempFurn (progn (setf (aref map (1+ currY) currX) -1)
			  (setf (aref visited (1+ currY) currX) -1)))
	 (tempCat (setf (aref map (1+ currY) currX) 0))
	 (tempDirect (setf (aref map (1+ currY) currX) 1))
	 ;; Walls
	 (T (progn (setf (aref map (1+ currY) currX) -1)
		   (setf (aref visited (1+ currY) currX) -1))))

       ;; East
       (setf tempDirect (cadr directionsList))
       (setf tempDirt (cadr dirtList))
       (setf tempFurn (cadr furnitureList))
       (setf tempCat (cadr catList))

       (cond
	 ((numberp tempDirt) (setf (aref map currY (1+ currX)) (1+ tempDirt)))
	 (tempFurn (progn (setf (aref map currY (1+ currX)) -1)
			  (setf (aref visited currY (1+ currX)) -1)))
	 (tempCat (setf (aref map currY (1+ currX)) 0))
	 (tempDirect (setf (aref map currY (1+ currX)) 1))
	 ;; Walls
	 (T (progn (setf (aref map currY (1+ currX)) -1)
		   (setf (aref visited currY (1+ currX)) -1))))

       ;; South
       (setf tempDirect (caddr directionsList))
       (setf tempDirt (caddr dirtList))
       (setf tempFurn (caddr furnitureList))
       (setf tempCat (caddr catList))

       (cond
	 ((numberp tempDirt) (setf (aref map (1- currY) currX) (1+ tempDirt)))
	 (tempFurn (progn (setf (aref map (1- currY) currX) -1)
			  (setf (aref visited (1- currY) currX) -1)))
	 (tempCat (setf (aref map (1- currY) currX) 0))
	 (tempDirect (setf (aref map (1- currY) currX) 1))
	 ;; Walls
	 (T (progn (setf (aref map (1- currY) currX) -1)
		   (setf (aref visited (1- currY) currX) -1))))

       ;; West
       (setf tempDirect (cadddr directionsList))
       (setf tempDirt (cadddr dirtList))
       (setf tempFurn (cadddr furnitureList))
       (setf tempCat (cadddr catList))

       (cond
	 ((numberp tempDirt) (setf (aref map currY (1- currX)) (1+ tempDirt)))
	 (tempFurn (progn (setf (aref map currY (1- currX)) -1)
			  (setf (aref visited currY (1- currX)) -1)))
	 (tempCat (setf (aref map currY (1- currX)) 0))
	 (tempDirect (setf (aref map currY (1- currX)) 1))
	 ;; Walls
	 (T (progn (setf (aref map currY (1- currX)) -1)
		   (setf (aref visited currY (1- currX)) -1))))
    )))

(defun updateAction (action)
  "Performs various updates per action taken"
  ; suck forward turn (L,R) shut-off up down left right
  (cond
    ((eq action 'suck) (progn
			 (if (> (aref map currY currX) 1)
			     (decf (aref map currY currX)))
			 (incf suckNo)
			 'suck))
    ((eq action 'forward) (progn
			    (cond
			      ((eq heading 0) (incf currY))
			      ((eq heading 1) (incf currX))
			      ((eq heading 2) (decf currY))
			      ((eq heading 3) (decf currX))
			      )
			    'forward))
    ((eq action 'turnL) (progn
			  (decf heading)
			  '(turn left)))
    ((eq action 'turnR) (progn
			  (incf heading)
			  '(turn right)))
    ((eq action 'shut-off) (progn
			     (format t "shut-off")))
    ((eq action 'up) (progn
		       (setf lastMove 0)
		       (setf heading 0)
		       (incf currY)
		       'up))
    ((eq action 'down) (progn
			 (setf lastMove 2)
			 (setf heading 2)
			 (decf currY)
			 'down))
    ((eq action 'left) (progn
			 (setf lastMove 3)
			 (setf heading 3)
			 (decf currX)
			 'left))
    ((eq action 'right) (progn
			  (setf lastMove 1)
			  (setf heading 1)
			  (incf currX)
			  'right))))

(defun undoLastMove ()
  (cond
    ((eq lastMove 0) (decf currY))
    ((eq lastMove 1) (decf currX))
    ((eq lastMove 2) (incf currY))
    ((eq lastMove 3) (incf currX)))
)

(defun goHome ()
  (moveTo 1 1)
  (setf goHome 1)
)

(defun moveTo (destX destY)
  (planPath destX destY)
  (setf planX destX)
  (setf planY destY)
)

(defun planPath (destX destY)
  (floodDest destX destY)
  (setf plan 1)
)

(defun flood ()
  (loop for i from 0 to (1- mapY) do
       (loop for j from 0 to (1- mapX) do
	    (progn
	      (setf cell (aref map i j))
	      (if (> cell 1)
		  (setf (aref floodMap i j) 0)
		  (if (eq cell 1)
		      (setf (aref floodMap i j) 9)
		      (setf (aref floodMap i j) (* mapY mapX)))))))
)

(defun floodDest (destX destY)
  ;; Initial flood values
  (loop for i from 0 to (1- mapY) do
       (loop for j from 0 to (1- mapX) do
	    (if (and (eq i destY) (eq j destX))
		(setf (aref floodMap i j) 0)
		(setf (aref floodMap i j) (* mapY mapX)))))

  ;; Continue fill
  (setf stepCounter 1)
  (setFillStatus)
  (loop while (and (eq 0 fillStatus) (< stepCounter (* mapY mapX))) do
       (progn
	 (loop for i from 0 to (1- mapY) do
	      (loop for j from 0 to (1- mapX) do
		   (if (eq (aref floodMap i j) (1- stepCounter))
		       (assign j i))))
	 (incf stepCounter)
	 (setFillStatus)))
)

(defun assign (coorX coorY)
  ;; North
  (let ((x coorX) (y (1+ coorY)))
    (if (and (> (aref floodMap y x) stepCounter) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCounter))
  ;; East
    (setf x (1+ coorX))
    (setf y coorY)
    (if (and (> (aref floodMap y x) stepCounter) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCounter))
  ;; South
    (setf x coorX)
    (setf y (1- coorY))
    (if (and (> (aref floodMap y x) stepCounter) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCounter))
  ;; West
    (setf x (1- coorX))
    (setf y coorY)
    (if (and (> (aref floodMap y x) stepCounter) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCounter))
    )
)

(defun setFillStatus ()
  (setf fillStatus 1)
  (loop for i from 0 to (1- mapY) do
       (loop for j from 0 to (1- mapX) do
	    (if (and (> (aref map i j) 0) (eq (aref floodMap i j) (* mapY mapX)))
		(setf fillStatus 0))))
)

(defun printDamnMap (map height width)
  "Prints the damn map"
  (loop for i from (1- height) downto 0
       do (progn (loop for j from 0 to (1- width)
	       do (format t " ~A " (aref map i j)))
		 (format t "~%"))))

(defun findNext ()
  "Instead of using the visited map to find the closest desired point, we trace our steps back to ensure that the point is reachable"
  (let ((dir 0) (x currX) (y currY))
    (loop while (eq plan 0) do
	 (setf dir (findLastAdj x y (aref visited y x)))
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
    ((or (eq (aref visited (1+ y) x) 0) (eq (aref visited (1+ y) x) (1- currValue)))
     (progn
       (if (eq (aref visited (1+ y) x) 0)
	   (setf plan 1))
       0))
    ;; East
    ((or (eq (aref visited y (1+ x)) 0) (eq (aref visited y (1+ x)) (1- currValue)))
     (progn
       (if (eq (aref visited y (1+ x)) 0)
	   (setf plan 1))
       1))
    ;; South
    ((or (eq (aref visited (1- y) x) 0) (eq (aref visited (1- y) x) (1- currValue)))
     (progn
       (if (eq (aref visited (1- y) x) 0)
	   (setf plan 1))
       2))
    ;; West
    ((or (eq (aref visited y (1- x)) 0) (eq (aref visited y (1- x)) (1- currValue)))
     (progn
       (if (eq (aref visited y (1- x)) 0)
	   (setf plan 1))
       3))
    ;; If there is no lower value, the current cell is the lowest value
    (T (progn (setf plan 1) -1)))
  )

(defun allAdjVisited ()
  (if (and
       (or (eq -1 (aref visited (1+ currY) currX)) (> (aref visited (1+ currY) currX) 0))
       (or (eq -1 (aref visited currY (1+ currX))) (> (aref visited currY (1+ currX)) 0))
       (or (eq -1 (aref visited (1- currY) currX)) (> (aref visited (1- currY) currX) 0))
       (or (eq -1 (aref visited currY (1- currX))) (> (aref visited currY (1- currX)) 0)))
      (return-from allAdjVisited 1)
      (return-from allAdjVisited 0))
  )

(defun findClosest ()
    
  )


