;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Some simple agents for the vacuum world

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


(defstructure (jason-vacuum 
   (:include agent
    (program
     #'(lambda (percept)
         (destructuring-bind (bump dirt home directionsList dirtList) percept
	   (read-line)
	   ; Progam variables
	   (progn
	     (defvar currX 1)
	     (defvar currY 1)
	     (defvar choiceDir 0)
	     (defvar amount 0)
	     ; north (0) east (1) south (2) west (3)
	     (defvar heading 1) ; start heading east
	     )

	   (updateMap percept)
	   (updateHeading)

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

	   (printDamnMap map 8 12)

	   ; Final action
	   (cond
	     (dirt (updateAction 'suck))
	     (T (progn
		  (setf amount 0)
					; North
		  (setf amountT (aref map (1+ currY) currX))
		  (if (> amountT amount)
		      (progn
			(setf amount amountT)
			(setf choiceDir 0)))
					; East
		  (setf amountT (aref map currY (1+ currX)))
		  (if (> amountT amount)
		      (progn
			(setf amount amountT)
			(setf choiceDir 1)))
					; South
		  (setf amountT (aref map (1- currY) currX))
		  (if (> amountT amount)
		      (progn
			(setf amount amountT)
			(setf choiceDir 2)))
					; West
		  (setf amountT (aref map currY (1+ currX)))
		  (if (> amountT amount)
		      (progn
			(setf amount amountT)
			(setf choiceDir 3)))
		  (cond
		    ((eq 0 choiceDir) (updateAction 'up))
		    ((eq 1 choiceDir) (updateAction 'right))
		    ((eq 2 choiceDir) (updateAction 'down))
		    ((eq 3 choiceDir) (updateAction 'left))))))


	   ;(cond (dirt (updateAction 'suck))
		; (bump (progn (updateAction 'turnR)))
		 ;(home (updateAction 'forward))
                 ;(t (progn (updateAction 'up))))
	   )))))
		 ;;(t (random-element '(forward forward forward shut-off
		;;			      (turn right) (turn left))))))))))
  "A very stupid agent: ignore percept and choose a random action.")

(defun chris-play ()
  "Reset vacuum variables and launches vacuum"
  (progn
    (defparameter currX 1)
    (defparameter currY 1)
    (defparameter heading 1)
    (defparameter choiceDir 0)
    (defparameter amount 0)
    (defparameter map (make-array '(8 12) :initial-element 'u))
    (format t "All variables have been reset")
    (run-environment (make-vacuum-world :aspec '(jason-vacuum)))))

(defun updateHeading ()
  "Update heading"
  (setf heading (mod heading 4)))

(defun updateMap (percept)
  "Updates map accordingly"
  (progn
     (destructuring-bind (bump dirt home directionsList dirtList) percept
       ; North
       (setf tempDirect (car directionsList))
       (setf tempDirt (car dirtList))

       (if (not tempDirt)
	   (if tempDirect
	       (setf (aref map (1+ currY) currX) 0)
	       (setf (aref map (1+ currY) currX) -1))
	   (setf (aref map (1+ currY) currX) 1))

       ; East
       (setf tempDirect (cadr directionsList))
       (setf tempDirt (cadr dirtList))

       (if (not tempDirt)
	   (if tempDirect
	       (setf (aref map currY (1+ currX)) 0)
	       (setf (aref map currY (1+ currX)) -1))
	   (setf (aref map currY (1+ currX)) 1))

       ; South
       (setf tempDirect (caddr directionsList))
       (setf tempDirt (caddr dirtList))
       (if (not tempDirt)
	   (if tempDirect
	       (setf (aref map (1- currY) currX) 0)
	       (setf (aref map (1- currY) currX) -1))
	   (setf (aref map (1- currY) currX) 1))

       ; West
       (setf tempDirect (cadddr directionsList))
       (setf tempDirt (cadddr dirtList))

       (if (not tempDirt)
	   (if tempDirect
	       (setf (aref map currY (1- currX)) 0)
	       (setf (aref map currY (1- currX)) -1))
	   (setf (aref map currY (1- currX)) 1))
    )))

(defun updateAction (action)
  "Performs various updates per action taken"
  ; Legal actions:
  ; suck forward turn (L,R) shut-off up down left right
  (cond
    ((eq action 'suck) 'suck)
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
		       (setf heading 0)
		       (incf currY)
		       'up))
    ((eq action 'down) (progn
			 (setf heading 2)
			 (decf currY)
			 'down))
    ((eq action 'left) (progn
			 (setf heading 3)
			 (decf currX)
			 'left))
    ((eq action 'right) (progn
			  (setf heading 1)
			  (incf currX)
			  'right))))

(defun printDamnMap (map height width)
  "Prints the damn map"
  (loop for i from (1- height) downto 0
       do (progn (loop for j from 0 to (1- width)
	       do (format t " ~A " (aref map i j)))
		 (format t "~%"))))