;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Vacuum World: cleaning up dirt in a grid

(defstructure (dirt (:include object (name "*") (size 0.01))))

(defstructure (furniture (:include obstacle (name "F") (size 0.01))))

(defstructure
    (cat
     (:include
      agent
      (name "C")
      (program
       #'(lambda (percept)
	   (declare (ignore percept))
	   (random-element
	    '(shed cat-up cat-down cat-left cat-right cat-sleep cat-sleep cat-sleep cat-sleep cat-sleep cat-sleep cat-sleep cat-sleep))))))
	    ;;'(right))))))
    "A very stupid agent: ignore percept and choose a random action.")


(defun play ()
  (read-room)
  (run-environment (make-vacuum-world :aspec '(stupid-vacuum))))


(progn
  (defparameter room-x 0)
  (defparameter room-y 0)
  (defparameter max-time 0)
  (defparameter dirt-factor 0)
  (defparameter num-cats 0)
  (defparameter cats '())
  (defparameter furniture '())
  )

(defun read-next-line (infile)
  (loop for line = (read-line infile nil 'eof)
        until (or (eq line 'eof)
                  (not (string= line "")))
        finally (return line)))

(defun read-room ()
  (loop with room-ok
        do (format t "Please enter room file: ")
             (let ((infile (read-line)))
               (when (string= infile "")
                 (setf infile "a1input.txt")
                 (format t "Changed input~%"))
               (setf room-ok (read-a-room infile)))
        until room-ok))

(defun read-a-room (file)
  ;; Get the file to read
  (with-open-file (infile file)
    (setf cats '())
    (setf furniture '())
    (let ((num 0)
          (line (read-next-line infile)))

      ;;Room size
      (read-both-numbers line room-x room-y)
      (format t "~%room is: ~A by ~A~%" room-x room-y)
      (when (or (> room-x 20)
                (> room-y 20))
        (format t "Error: Room Size too Large: ~A by ~A~%" room-x room-y)
        (return-from read-a-room nil))

      ;;time limit
      (setf line (read-next-line infile))
      (setf max-time (read-first-number line))
      (format t "~%max-time: ~A~%" max-time)
      ;;Dirt factor
      (setf line (read-next-line infile))
      (setf dirt-factor (read-first-number line))
      (format t "~%dirt-factor: ~A~%" dirt-factor)

      ;;Num Cats
      (setf line (read-next-line infile))
      (setf num-cats (read-first-number line))
      (format t "~%num-cats: ~A~%" num-cats)

      ;;Reading in Cats
      (setf line (read-next-line infile))
      (dotimes (num num-cats)
        (let ((cat-x) (cat-y) (shedding-factor))
          (read-both-numbers line cat-x cat-y)
          (setf line (read-next-line infile))
          (setf shedding-factor (read-first-number line))
          (format t "~%cat ~A: loc(~A,~A) ~A~%" num cat-x cat-y shedding-factor)
          (push
           (list cat-x cat-y shedding-factor)
           cats)
          ;; Read next line unless done reading cats
          (when (not (= (1+ num) num-cats))
            (setf line (read-next-line infile)))
          ))

      ;;Reading in Furniture
      (setf line (read-next-line infile))
      (loop until (eq line 'eof)
            do (let ((furniture-startx) (furniture-starty) (furniture-endx) (furniture-endy))
                 (read-both-numbers line furniture-startx furniture-starty)
                 (setf line (read-next-line infile))
                 (when (not (eq line 'eof))
                   (read-both-numbers line furniture-endx furniture-endy)
                   (format t "~%furniture ~A: start(~A,~A) end(~A,~A)~%"
                           num furniture-startx furniture-starty furniture-endx furniture-endy)
                   (incf num)            ;Num is just for printing number of furniture
                   (push
                    (list (list furniture-startx furniture-starty)
                          (list furniture-endx furniture-endy))
                    furniture)
                   (format t "current furniture: ~A~%" furniture)
                   )))))
  t)


(defun read-first-number (instring)
  (parse-integer instring :junk-allowed t))

(defun read-second-number (instring)
  (let ((first-number)
        (second-number)
        (position))
    (multiple-value-setq (first-number position)
      (parse-integer instring :junk-allowed t))
    (setf second-number (parse-integer instring :start position :junk-allowed t))
    (format t "one: ~A two: ~A~%" first-number second-number)
    second-number))

(defmacro read-both-numbers (instring first-number second-number)
  `(progn
     (let ((position))
       (multiple-value-setq (,first-number position)
         (parse-integer ,instring :junk-allowed t))
       (setf ,second-number (parse-integer ,instring :start position :junk-allowed t)))
     ))
  

;; Call (read-room) before this function to populate global variables
(defstructure (vacuum-world (:include grid-environment
    (size (@ room-x room-y))
    ;;    (size (@ 12 8))
    (aspec '(random-vacuum-agent))
    (cspec (generate-cspec))
    ))
  "A grid with some dirt in it, and by default a reactive vacuum agent.")

(defun generate-cspec ()
  "Generates appropriate cspec for vacuum world"
  ;; Read file to set the global variables
  (let ((returnlist '()))
    ;; Cats
    (setf returnlist
          (append returnlist
                  (loop for cat in cats
                        collecting (list 'at (list (car cat) (cadr cat)) 'cat) into mycats
                        do (format t "cat:~A~%" cat)
                        finally (return mycats))))

    ;; Furniture
    (setf returnlist
          (append returnlist
                  (loop for item in furniture
                        for startloc = (first item)
                        for endloc = (second item)
                        appending (generate-furniture startloc endloc) into returnlist
                        finally (return returnlist))))
    ;; Dirt factor
    (setf returnlist
          (append returnlist
                  (list (list 'at 'all (list 'p (/ dirt-factor 10) 'dirt)))))
    returnlist))

(defun generate-furniture (startloc endloc)
  "Generates furniture information given start and end locations"
  ;; Ensure that start location is "smaller" than end location
  (if (< (car endloc)
          (car startloc))
      (format t "error!:one start: ~A end: ~A~%" startloc endloc))
  (if (< (second endloc)
          (second startloc))
      (format t "error!:two start: ~A end: ~A~%" startloc endloc))

  (loop for i from (car startloc) to (car endloc)
        appending (loop for j from (second startloc) to (second endloc)
                        collecting (list 'at (list i j) 'furniture) into furniture-list
                        finally (return furniture-list))
          into furniture-list
        finally (return furniture-list)))


(defun flatten (inlist)
  "Removes nestings from a list."
  (cond ((atom inlist) inlist)
        ((listp (car inlist))
         (append (flatten (car inlist)) (flatten (cdr inlist)))
         )
        (t (append (list (car inlist)) (flatten (cdr inlist))))))


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
	  (if (equal loc (grid-environment-start env)) 'home)
	  (check-sides env (agent-body agent))
          (check-dirt env (agent-body agent))
          (check-cats env (agent-body agent))
          (check-furniture env (agent-body agent)))
	  ))

(defmethod legal-actions ((env vacuum-world))
  '(suck forward turn shut-off charge up down left right cat-sleep shed cat-up cat-down cat-left cat-right))

;;;; Actions (other than the basic grid actions of forward and turn)

(defmethod expend-energy ((env vacuum-world) agent-body &optional (energy 0.25))
  (format t "expend-energy: expending ~A energy leaving " energy)
  (setf (agent-body-charge agent-body) (- (agent-body-charge agent-body)
                                          energy))
  (format t "~A~%" (agent-body-charge agent-body))
  )


(defmethod suck ((env vacuum-world) agent-body)
  (expend-energy env agent-body)
  (let ((dirt (find-object-if #'dirt-p (object-loc agent-body) env)))
    (when dirt
      (place-in-container dirt agent-body env))))

(defmethod shed ((env vacuum-world) agent-body)
  (format t "I'm shedding!~%")
  (let ((dirt (make-dirt)))
    (place-object dirt (agent-body-loc agent-body) env)))

(defmethod cat-sleep ((env vacuum-world) agent-body)
  (format t "Cat sleeping~%"))

(defmethod shut-off ((env environment) agent-body)
  (declare-ignore env)
  (setf (object-alive? agent-body) nil))

(defmethod charge ((env environment) agent-body)
  "Charge if in home square"
  (if (equal (agent-body-loc agent-body) (grid-environment-start env))
      (progn
	(format t "Charging from ~A" (agent-body-charge agent-body))
        (setf (agent-body-charge agent-body) 100)
	(format t " to ~A~%" (agent-body-charge agent-body)))
      (format t "Cannot if not in home square~%")))

(defmacro direction-generator (name direction)
  `(defmethod ,name ((env vacuum-world) agent-body)
     (expend-energy env agent-body)
     (setf (object-heading agent-body)
	   ,direction)
     (forward env agent-body)))

(progn
  (direction-generator up '(0 1))
  (direction-generator down '(0 -1))
  (direction-generator left '(-1 0))
  (direction-generator right '(1 0)))


(defmacro cat-direction-generator (name direction)
  `(defmethod ,name ((env vacuum-world) agent-body)
     (setf (object-heading agent-body)
	   ,direction)
     (forward env agent-body)
     (forward env agent-body)))

(progn
  (cat-direction-generator cat-up '(0 1))
  (cat-direction-generator cat-down '(0 -1))
  (cat-direction-generator cat-left '(-1 0))
  (cat-direction-generator cat-right '(1 0)))


;;;; Sensor-related functions
(defmacro make-check-fn (name check-fn)
  `(defun ,name (env agent-body)
     (let* ((loc (object-loc agent-body))
            (up (list (first loc) (1+ (second loc))))
            (down (list (first loc) (1- (second loc))))
            (left (list (1- (first loc)) (second loc)))
            (right (list (1+ (first loc)) (second loc))))
       (map 'list ,check-fn (list up right down left)))))


(progn
  (make-check-fn check-sides #'(lambda (in) (open-loc? in env)))
  (make-check-fn check-furniture #'(lambda (in) (furniture-loc? in env)))
  (make-check-fn check-cats #'(lambda (in) (cat-loc? in env)))
  (make-check-fn check-dirt #'(lambda (in) (get-dirt in env))))

(defun open-loc? (loc env)
  "A location is open if there is no obstacle there."
  (not (find-object-if #'obstacle-p loc env)))

(defun furniture-loc? (loc env)
  "Checks if a location has furniture"
  (if (find-object-if #'furniture-p loc env)
      t
      nil))

(defun cat-loc? (loc env)
  "Checks if a location has furniture"
  (if (find-object-if #'cat-p loc env)
      t
      nil))


(defun get-dirt (loc env)
  "Gets the amount of dirt in that location"
  ;(find-object-if #'(lambda (in) (string= (object-name in) "dirt")) loc env))
  (if (find-object-if #'dirt-p loc env)
      (length (grid-contents env loc))
      nil))

