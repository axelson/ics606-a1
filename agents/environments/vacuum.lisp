;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Vacuum World: cleaning up dirt in a grid

(defstructure (dirt (:include object (name "*") (size 0.01))))

(defstructure (furniture (:include obstacle (name "F") (size 0.01))))

(defstructure (cat (:include obstacle (name "C") (size 0.01))))

(progn
  (defparameter room-x 0)
  (defparameter room-y 0)
  (defparameter max-time 0)
  (defparameter dirt-factor 0)
  (defparameter num-cats 0)
  (defparameter cats '())
  (defparameter furniture '())
  )


(defun read-room ()
  (with-open-file (infile "a1inputspecification.txt")
    (setf cats '())
    (setf furniture '())
    (let ((num 0))
      (format t "room-x: ~A~%" room-x)
      (loop
        with section = 1
        for line = (read-line infile nil 'eof)
        until (eq line 'eof)
        do (progn
             (print line)
             (cond
               ((= section 1)           ;Room size
                (read-both-numbers line room-x room-y)
                (format t "room is: ~A by ~A~%" room-x room-y)
                (incf section))
               ((= section 2)           ;time limit
                (setf max-time (read-first-number line))
                (format t "~%max-time: ~A~%" max-time)
                (incf section))
               ((= section 3)           ;Dirt factor
                (setf dirt-factor (read-first-number line))
                (format t "~%dirt-factor: ~A~%" dirt-factor)
                (incf section))
               ((= section 4)           ;Num Cats
                (setf num-cats (read-first-number line))
                (format t "~%num-cats: ~A~%" num-cats)
                (incf section))
               ((= section 5)           ;Reading in Cats
                (dotimes (num num-cats)
                  (let ((cat-x) (cat-y) (shedding-factor))
                    (read-both-numbers line cat-x cat-y)
                    (setf line (read-line infile))
                    (setf shedding-factor (read-first-number line))
                    (format t "~%cat ~A: loc(~A,~A) ~A~%" num cat-x cat-y shedding-factor)
                    (push
                     (list cat-x cat-y shedding-factor)
                    cats)
                    (setf line (read-line infile))
                    ))
                (incf section))
               ((= section 6)           ;Reading in Furniture
                (let ((furniture-startx) (furniture-starty) (furniture-endx) (furniture-endy))
                  (read-both-numbers line furniture-startx furniture-starty)
                  (setf line (read-line infile nil))
                  (when line
                    (read-both-numbers line furniture-endx furniture-endy)
                    (format t "~%furniture ~A: start(~A,~A) end(~A,~A)~%"
                            num furniture-startx furniture-starty furniture-endx furniture-endy)
                    (incf num)            ;Num is just for printing number of furniture
                    (push
                     (list (list furniture-startx furniture-starty)
                           (list furniture-endx furniture-endy))
                     furniture)
                    (format t "current furniture: ~A~%" furniture)
                    )))
               (t (format t "~%~%at end~%"))
               ;;(format t "~%on section: ~A" section)
               ))))))


;; Only works in interpretter
(defmacro print-var (var1)
  `(progn
     (format t "var: ~A" ,var1)
     (format t "value: ~A" (eval ,var1))))

;; Only works in interpretter
(defmacro print-vars (&rest vars)
  `(progn
     (dolist (var ',vars)
       (format t "~A: ~A " var (eval var)))))

;  `(format t "vars: ~A" ,vars))

(defun read-room-size-simple (instring)
  (let ((first-number)
        (position))
    (multiple-value-setq (first-number position)
      (parse-integer instring))
    (format t "one: ~A two: ~A~%" first-number position)))

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
     ;(format t "first: ~A second: ~A~%" ,first-number ,second-number)
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
  (read-room)
  (let ((returnlist '()))
    (setf returnlist
          (list '(at all (P 0.08 furniture))))

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
    '(at all (P 0.08 cat))
    '(at free? (P (/ dirt-factor 10) dirt))
    returnlist))

(defun generate-furniture (startloc endloc)
  "Generates furniture information given start and end locations"
  ;; Ensure that start location is "smaller" than end location
  (if (> (car startloc)
         (car endloc))
      (format t "error!"))
  (if (> (second startloc)
         (second endloc))
      (format t "error!"))

  (loop for i from (car startloc) to (car endloc)
        appending (loop for j from (second startloc) to (second endloc)
                        collecting (list 'at (list i j) 'furniture) into furniture-list
                        do (format t "i: ~A j: ~A~%" i j)
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
          (check-furniture env (agent-body agent)))
	  ))

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

(defmacro direction-generator (name direction)
  `(defmethod ,name ((env vacuum-world) agent-body)
     (setf (object-heading agent-body)
	   ,direction)
     (forward env agent-body)
     (format t "~%object max contents: ~A" (object-max-contents agent-body))
     (format t "~%current contents: ~A" (sum (object-contents agent-body) #'object-size))
     (check-sides env agent-body)))

(progn
  (direction-generator up '(0 1))
  (direction-generator down '(0 -1))
  (direction-generator left '(-1 0))
  (direction-generator right '(1 0)))


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
  (make-check-fn check-dirt #'(lambda (in) (get-dirt in env))))

(defun open-loc? (loc env)
  "A location is open if there is no obstacle there."
  (not (find-object-if #'obstacle-p loc env)))

(defun furniture-loc? (loc env)
  "Checks if a location has furniture"
  (find-object-if #'furniture-p loc env))


(defun get-dirt (loc env)
  "Gets the amount of dirt in that location"
  ;(find-object-if #'(lambda (in) (string= (object-name in) "dirt")) loc env))
  (find-object-if #'dirt-p loc env))
