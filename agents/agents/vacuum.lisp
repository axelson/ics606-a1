;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Some simple agents for the vacuum world

(defstructure (stupid-vacuum
   (:include agent
    (program
     #'(lambda (percept)
         (destructuring-bind (bump dirt home directions dirlist furnitureList catList) percept
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
	     (read-line)
	     ;; If there was a bump, undo last move (if applicable)
	     (if bump
		   (undoLastMove))

	     (updateMap percept)
	     (updateHeading)
	     (if (not (eq choiceDir -1))
		 (updateVisited))

	     (format t "~%currX: ~A" currX)
	     (format t "~%currY: ~A" currY)
	     (format t "~%Heading: ")
	     (cond
	       ((eq 0 heading) (format t "North"))
	       ((eq 1 heading) (format t "East"))
	       ((eq 2 heading) (format t "South"))
	       ((eq 3 heading) (format t "West"))
	       (T (format t "INVALID")))
	     (format t "~%")
	     (format t "~%Output:~%")

	     ;; Maps
	     (printDamnMap visited mapY mapX)
	     (format t "~%")
	     (printDamnMap map mapY mapX)
	     (format t "~%")
	     ;(printDamnMap floodMap mapY mapX)


	     (if (> moveNo 40)
		 (goHome))
	     
	     ;; Final action
	     (if dirt
		 (progn
		   (setf choiceDir -1)
		   (updateAction 'suck))
		 (cond
		   ((> plan 0) ())
		   
		   (T (progn
			(setf amount 0)
			(setf choiceDir -1)
			;; North
			(setf amountT (aref map (1+ currY) currX))
			(setf visitNoT (aref visited (1+ currY) currX))
			(if (> amountT amount)
			    (progn
			      (setf amount amountT)
			      (setf visitNo visitNoT)
			      (setf choiceDir 0)))

			;; East
			(setf amountT (aref map currY (1+ currX)))
			(setf visitNoT (aref visited currY (1+ currX)))
			(if (or (> amountT amount) (and (eq amount amountT) (< visitNoT visitNo)))
			    (progn
			      (setf amount amountT)
			      (setf choiceDir 1)))
				
			;; South
			(setf amountT (aref map (1- currY) currX))
			(setf visitNoT (aref visited (1- currY) currX))
			(if (or (> amountT amount) (and (eq amount amountT) (< visitNoT visitNo)))
			    (progn
			      (setf amount amountT)
			      (setf choiceDir 2)))

			;; West
			(setf amountT (aref map currY (1- currX)))
			(setf visitNoT (aref visited currY (1- currX)))
			(if (or (> amountT amount) (and (eq amount amountT) (< visitNoT visitNo)))
			    (progn
			      (setf amount amountT)
			      (setf choiceDir 3)))
			
			(cond
			  ((eq 0 choiceDir) (updateAction 'up))
			  ((eq 1 choiceDir) (updateAction 'right))
			  ((eq 2 choiceDir) (updateAction 'down))
			  ((eq 3 choiceDir) (updateAction 'left))
			  (T (progn
			       (flood)
			       (updateAction 'shut-off))))
			)))))))))
    "A very stupid agent")

(defun chris-play ()
  "Reset vacuum variables and launches vacuum"
  (progn
    (defparameter moveNo 1)
    (defparameter mapX 12)
    (defparameter mapY 8)
    (defparameter currX 1)
    (defparameter currY 1)
    (defparameter heading 1)
    (defparameter choiceDir 0)
    (defparameter amount 0)
    (defparameter plan 0)
    (defparameter lastMove -1)
    (defparameter map (make-array (list mapY mapX) :initial-element 0))
    (defparameter visited (make-array (list mapY mapX) :initial-element 0))
    (defparameter floodMap (make-array (list mapY mapX)))
    (format t "All variables have been reset")
    ; Create walls
    (loop for i from 0 to (1- mapY) do
	 (progn
	   (setf (aref map i 0) -1)
	   (setf (aref map i (1- mapX)) -1)))
    (loop for i from 0 to (1- mapX) do
	 (progn
	   (setf (aref map 0 i) -1)
	   (setf (aref map (1- mapY) i) -1)))

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
	 ((numberp tempDirt) (setf (aref map (1+ currY) currX) (1+ 1)))
	 (tempFurn (setf (aref map (1+ currY) currX) -1))
	 (tempCat (setf (aref map (1+ currY) currX) 0))
	 (tempDirect (setf (aref map (1+ currY) currX) 1))
	 ;; Walls
	 (T (setf (aref map (1+ currY) currX) -1)))

       ;; East
       (setf tempDirect (cadr directionsList))
       (setf tempDirt (cadr dirtList))
       (setf tempFurn (cadr furnitureList))
       (setf tempCat (cadr catList))

       (cond
	 ((numberp tempDirt) (setf (aref map currY (1+ currX)) (1+ 1)))
	 (tempFurn (setf (aref map currY (1+ currX)) -1))
	 (tempCat (setf (aref map currY (1+ currX)) 0))
	 (tempDirect (setf (aref map currY (1+ currX)) 1))
	 ;; Walls
	 (T (setf (aref map currY (1+ currX)) -1)))

       ;; South
       (setf tempDirect (caddr directionsList))
       (setf tempDirt (caddr dirtList))
       (setf tempFurn (caddr furnitureList))
       (setf tempCat (caddr catList))

       (cond
	 ((numberp tempDirt) (setf (aref map (1- currY) currX) (1+ 1)))
	 (tempFurn (setf (aref map (1- currY) currX) -1))
	 (tempCat (setf (aref map (1- currY) currX) 0))
	 (tempDirect (setf (aref map (1- currY) currX) 1))
	 ;; Walls
	 (T (setf (aref map (1- currY) currX) -1)))

       ;; West
       (setf tempDirect (cadddr directionsList))
       (setf tempDirt (cadddr dirtList))
       (setf tempFurn (cadddr furnitureList))
       (setf tempCat (cadddr catList))

       (cond
	 ((numberp tempDirt) (setf (aref map currY (1- currX)) (1+ 1)))
	 (tempFurn (setf (aref map currY (1- currX)) -1))
	 (tempCat (setf (aref map currY (1- currX)) 0))
	 (tempDirect (setf (aref map currY (1- currX)) 1))
	 ;; Walls
	 (T (setf (aref map currY (1- currX)) -1)))
    )))

(defun updateAction (action)
  "Performs various updates per action taken"
  ; suck forward turn (L,R) shut-off up down left right
  (cond
    ((eq action 'suck) (progn
			 (if (> (aref map currY currX) 1)
			     (decf (aref map currY currX))) 
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
)

(defun moveTo (destX destY)
  (planPath destX destY)
)

(defun planPath (destX destY)
  (floodDest destX destY)
  (printDamnMap floodMap mapY mapX)
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
       (loop for i from 0 to (1- mapY) do
	    (loop for j from 0 to (1- mapX) do
		 (if (eq (aref floodMap i j) (1- stepCounter))
		     (assign i j)
		     (incf stepCounter)
		     (setFillStatus)))))
)

(defun assign (coorX coorY)
  ;; North
  (let ((x coorX) (y (1+ coorY)))
    (if (and (> (aref floodMap y x) stepCount) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCount))
  ;; East
    (setf x (1+ coorX))
    (setf y coorY)
    (if (and (> (aref floodMap y x) stepCount) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCount))
  ;; South
    (setf x coorX)
    (setf y (1- coorY))
    (if (and (> (aref floodMap y x) stepCount) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCount))
  ;; West
    (setf x (1- coorX))
    (setf y coorY)
    (if (and (> (aref floodMap y x) stepCount) (> (aref map y x) 0))
	(setf (aref floodMap y x) stepCount))
    )
)

(defun setFillStatus ()
  (setf fillStatus 1)
  (loop for i from 0 to (1- mapY) do
       (loop for j from 0 to (1- mapX) do
	    (if (and (> 0 (aref map i j)) (eq (aref floodMap i j) (* mapY mapX)))
		(setf fillStatus 0))))
)

(defun printDamnMap (map height width)
  "Prints the damn map"
  (loop for i from (1- height) downto 0
       do (progn (loop for j from 0 to (1- width)
	       do (format t " ~A " (aref map i j)))
		 (format t "~%"))))