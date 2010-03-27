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
         (destructuring-bind (bump dirt home directions) percept
           (read-line)
           (format t "~%Output: ")
	   (cond (dirt 'suck)
		 (bump '(turn right))
		 (home 'forward)
                 (t 'up)))))))
		 ;;(t (random-element '(forward forward forward shut-off
		;;			      (turn right) (turn left))))))))))
  "A very stupid agent: ignore percept and choose a random action.")

