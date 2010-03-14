;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Vacuum World: cleaning up dirt in a grid

(defstructure (dirt (:include object (name "*") (size 0.01))))

(defstructure (vacuum-world (:include grid-environment
    (size (@ 8 8))
    (aspec '(random-vacuum-agent))
    (cspec '((at all (P 0.25 dirt))))))
  "A grid with some dirt in it, and by default a reactive vacuum agent.")

;;;; Defining the generic functions

(defmethod performance-measure ((env vacuum-world) agent)
  "100 points for each piece of dirt vacuumed up, -1 point for each 
  step taken, and -1000 points if the agent does not return home."
  (- (* 100 (count-if #'dirt-p (object-contents (agent-body agent))))
     (environment-step env)
     (if (equal (object-loc (agent-body agent))
		(grid-environment-start env))
	 0
       1000)))

(defmethod get-percept ((env vacuum-world) agent)
  "Percept is a three-element sequence: bump, dirt and home."
  (let ((loc (object-loc (agent-body agent))))
    (list (if (object-bump (agent-body agent)) 'bump)
	  (if (find-object-if #'dirt-p loc env) 'dirt)
	  (if (equal loc (grid-environment-start env)) 'home))))

(defmethod legal-actions ((env vacuum-world))
  '(suck forward turn shut-off up down left right))

;;;; Actions (other than the basic grid actions of forward and turn)

(defmethod suck ((env vacuum-world) agent-body)
  (let ((dirt (find-object-if #'dirt-p (object-loc agent-body) env)))
    (when dirt
      (place-in-container dirt agent-body env))))

(defmethod shut-off ((env environment) agent-body)
  (declare-ignore env)
  (setf (object-alive? agent-body) nil))

(dolist (name-direction '((up (0 1)) ((down (0 -1))) ((left (-1 0))) ((right (1 0)))))
  (format t "name: ~A direction: ~A~%" (car name-direction) (cadr name-direction))
  (defmethod (car name-direction) ((env vacuum-world) agent-body)
    (setf (object-heading agent-body)
	  (second name-direction))
    (forward env agent-body)
    (check-sides env agent-body)))

(defmacro direction-generator (name direction)
  `(defmethod ,name ((env vacuum-world) agent-body)
     (setf (object-heading agent-body)
	   ,direction)
     (forward env agent-body)
     (check-sides env agent-body)))

(progn
  (direction-generator up '(0 1))
  (direction-generator down '(0 -1))
  (direction-generator left '(-1 0))
  (direction-generator right '(1 0)))


;;;; Sensor-related functions
(defun check-sides (env agent-body)
  (let* ((loc (object-loc agent-body))
	 (up (list (first loc) (1+ (second loc))))
	 (down (list (first loc) (1- (second loc))))
	 (left (list (1- (first loc)) (second loc)))
	 (right (list (1+ (first loc)) (second loc))))
    (format t "direction is: ~A~%" (map 'list #'(lambda (in) (open-loc? in env)) (list up down left right)))
    (format t "right is: ~A~%" (free-loc? right env))
    (format t "left is: ~A~%" (free-loc? left env))))

(defun open-loc? (loc env)
  "A location is open if there is no obstacle there."
  (not (find-object-if #'obstacle-p loc env)))
