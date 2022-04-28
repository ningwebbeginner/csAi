;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (backtrack '() n delta)) 
  
  
; takes in a partial assignment and a cnf of n integers, and returns the model
; returns NIL if not satisfiable

(defun backtrack (l n delta)
  (cond ((equal delta 'invalid) NIL) 
	((null delta) 
	 (complete l n))
	(t 
	 (let* ((posV (+ (length l) 1))
		(negV (- posV))
		(posl (getNextCnf posV delta)) 
		(negl (getNextCnf negV delta))) 
	   (cond ((null posl) 
		  (backtrack (append l (list posV)) n posl))
		 ((null negl)
		  (backtrack (append l (list negV)) n negl))
		 (t 
		  (or (backtrack (append l (list posV)) n posl)
		      (backtrack (append l (list negV)) n negl))))))))
 

; check if value doesn't violate any constraints of the cnf
; an value violates the constraints of the cnf if it causes any of
; the clauses to evaluate to false

(defun valid (v cnf)
  (cond ((null cnf) t) 
	(t 
	 (if (equal (list (- v)) (first cnf))
	     NIL 
	   (valid v (rest cnf)))))) 

; applies value to a single clause and returns resulting clause

(defun applyValueToClause (v clause)
  (if (member v clause) 
      NIL 
    (remove (- v) clause)))
    
; apply value to cnf and return the resulting cnf

(defun applyV(v cnf)
  (cond ((null cnf)  NIL) 
	(t 
	 (let ((update (applyValueToClause v (first cnf))))
	   (if update
	       (cons update (applyV v (rest cnf)))
	     (applyV v (rest cnf)))))))

; takes in a cnf and an value to apply, and attempts to apply that value
; returns next cnf if the value is valid

(defun getNextCnf (v cnf)
  (if (valid v cnf) 
      (applyV v cnf) 
      'invalid))


; complete the list

(defun complete (l n)
  (if (equal (length l) n)
      l
    (complete (append l (list (+ (length l) 1))) n)))

	  
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

