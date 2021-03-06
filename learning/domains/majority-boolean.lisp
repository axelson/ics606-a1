(defvar *majority-boolean* (majority-perceptron 11))
(defvar *majority-boolean-problem*)

(setq *majority-boolean-problem*
      (make-learning-problem
       :attributes '((a1 0 1) (a2 0 1) (a3 0 1) (a4 0 1) (a5 0 1)
		     (a6 0 1) (a7 0 1) (a8 0 1) (a9 0 1) (a10 0 1) (a11 0 1))
       :goals      '((g1 0 1))
       :examples   '(
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 0) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 0) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 1) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 1) (A1 . 1) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 1) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 0) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 1) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 0) (A4 . 1) (A5 . 1) (A6 . 1) (A7 . 1) (A8 . 0) (A9 . 0) (A10 . 1) (A11 . 1)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 0)) 
  ((G1 . 0) (A1 . 1) (A2 . 0) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 0) (A10 . 0) (A11 . 0)) 
  ((G1 . 1) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 1) (A5 . 0) (A6 . 0) (A7 . 1) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  ((G1 . 1) (A1 . 0) (A2 . 0) (A3 . 1) (A4 . 0) (A5 . 1) (A6 . 1) (A7 . 0) (A8 . 1) (A9 . 1) (A10 . 1) (A11 . 0)) 
  ((G1 . 0) (A1 . 0) (A2 . 1) (A3 . 1) (A4 . 0) (A5 . 0) (A6 . 1) (A7 . 0) (A8 . 0) (A9 . 1) (A10 . 0) (A11 . 1)) 
  )))
