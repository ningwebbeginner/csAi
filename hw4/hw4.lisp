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

; backtrack searching delta
(defun backtrack (n l delta)
    (cond ((vaild l delta) (cond ((= (length l) n) l)
                                    ((or (backTrack n (addLast l 1) clauses) 
		                                (backTrack n (addLast l -1) clauses)))
                                    
                                ))
    
    )

)
;check the validity of clauses based on current mlist values
(defun vaild (list clauselist)
    (cond ((not clauselist) nil)
            ((t (and (deterClause list (car clauselist)) (helperFunc list (cdr clauselist)))))
    )
)

(defun deterClause (list clause)
    (cond ((not clause) nil)
          (t (cond ((deter list (car clause)) t)
                    (t (deterClause list (cdr clause)))
          
          ))
    
    )
)
(defun deter (list c)
    (cond ((not list) t)
          ((= (car mlist) c) t)
          ((= (+ c (car mlist)) 0) nil)
          (t (deter (cdr list) c))
    )
)

;add a value to model list
(defun addLast (list value)
    (cond ((> val 0) (append list (list (+ (length l) 1))))
	  (t (append list (list (- 0 (+ (length l) 1)))))
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

