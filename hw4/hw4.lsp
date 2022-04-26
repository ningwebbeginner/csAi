;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
    (backtrack n '() delta)

)


;back track search recursivly
;check wheter the clause is valid, if valid
(defun backTrack (n l delta)
  (cond ((vaild l delta) 
	 (cond ((= (length l) n) l) 
	       ((or (backTrack n (addLast l 1) delta) 
		    (backTrack n (addLast l -1) delta)))
	       ))))

;check the validity of clauses based on current list values
(defun vaild (l delta)
  (cond ((not delta) t)
	(t (and (deterClause l (car delta)) (vaild  l (cdr delta))))))

;check the clause's validity with current mlist values
(defun deterClause (l clause) 
  (cond ((not clause) nil)
	(t (cond ((deter l (car clause)) t)
		 (t (deterClause l (cdr clause)))))
))




;if element in the current mlist equal to the element passed in
;return true if not in the mlist
(defun deter (l c)
    (cond ((not l) t)
	  ((= c (car l)) t)
	  ((= (+ c (car l)) 0) nil)
	  (t (deter (cdr l) c))
	 
	 )
	  
)


;add a value to model list
(defun addLast (l value)
    (cond ((> value 0) (append l (list (+ (length l) 1))))
	  (t (append l (list (- 0 (+ (length l) 1)))))
	  )

)



	  
	 

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

