;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Vacuum World: cleaning up dirt in a grid

(defstructure (dirt (:include object (name "*") (size 0.01))))

(defstructure (furniture (:include obstacle (name "F") (size 0.01))))

(defstructure (cat (:include obstacle (name "C") (size 0.01))))

(defun read-room ()
  (with-open-file (infile "a1inputspecification.txt")
    (let ((room-x) (room-y) (max-time) (dirt-factor) (num-cats) (cats) (furniture) (num 0))
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
                    (setf cats (append cats '((cat-x cat-y shedding-factor))))
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
                    (setf furniture (append furniture '(((furniture-startx furniture-starty)
                                                         (furniture-endx furniture-endy)))))
                    )))
               (t (format t "~%~%at end~%"))
               ;;(format t "~%on section: ~A" section)
               )))))
  (@ 12 8))


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
        (second-number)
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
  

(defstructure (vacuum-world (:include grid-environment
    (size (read-room))
    ;;    (size (@ 12 8))
    (aspec '(random-vacuum-agent))
    (cspec '((at all (P 0.55 dirt))
             (at all (P 0.008 furniture))
             (at all (P 0.008 cat))
             (at (2 3) (* 8 dirt))
             ))
    (file "file")))
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
	  (if (equal loc (grid-environment-start env)) 'home)
	  (check-sides env (agent-body agent))
          (check-dirt env (agent-body agent)))
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
  (make-check-fn check-dirt #'(lambda (in) (get-dirt in env))))

(defun open-loc? (loc env)
  "A location is open if there is no obstacle there."
  (not (find-object-if #'obstacle-p loc env)))

(defun get-dirt (loc env)
  "Gets the amount of dirt in that location"
  ;(find-object-if #'(lambda (in) (string= (object-name in) "dirt")) loc env))
  (find-object-if #'dirt-p loc env))