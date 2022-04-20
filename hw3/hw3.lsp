;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4);goal
(setq boxstar 5);box+goal, a box on top of goal
(setq keeperstar 6);keeper+goal, a keeper on top of goal

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;


(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 



;to check if there is isBox or keeper return true

(defun existBoxOrKeeper(s_list)
  (cond ((null s_list) nil)
	((or (isKeeper (car s_list)) (isBox (car s_list))) t)
	(t (existBoxOrKeeper (cdr s_list)))
	)
)


; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) t)
	((existBoxOrKeeper (car s)) nil)
	(t (goal-test (cdr s)))
  );end cond
);end defun



; Input: postion c d and direction
; Output: postion list of target '(c r), up or left would test margin.
(defun getValue (s listp)
    (cond ((not listp) nil)
        (t (car (nthcdr (car listp)  (car (nthcdr (cadr listp) s)))))
    )
)

; Input: postion c d and direction
; Output: postion list of target '(c r), up or left would test margin.
(defun getNextCR (c r dir)
    (cond ((and (= dir 1) (> r 0)) (list c (- r 1)))
        ((= dir 2) (list c (+ r 1)))
        ((= dir 4) (list (+ c 1)  r))
        ((and (= dir 3) (> c 0)) (list (- c 1)  r))
        (t nil)
    )
)



(defun setRow(s c v)
  (cond ((null s) nil) 
	((= 0 c) (cons v (cdr s)))
	(t (cons (car s) (setRow (cdr s) (- c 1) v))))
)

; Input: postion c d and target v
; Output: set target (c r)
(defun setValuePost (s c r v)
    (cond ((not s) nil)
        ((= 0 r) (cons (setRow (car s) c v) (cdr s))) 
	    (t (cons (car s) (setValuePost (cdr s) c (- r 1) v)))
	    
    
    )
)

; move keeper or box to blank
(defun move-to-blank (s c r listq)
    (cond ((not s) nil)
        (( not listq) nil)
        (t (let ((v (getValue s (list c r))))
            (cond 
                ((not v) nil)
                ((isboxstar v) (setValuePost (setValuePost s (car listq) (cadr listq) box) c r star) )
                ((iskeeperstar v) (setValuePost (setValuePost s (car listq) (cadr listq) keeper) c r star) )
                ((isbox v) (setValuePost (setValuePost s (car listq) (cadr listq) (getValue s (list c r))) c r blank))
                ((iskeeper v) (setValuePost (setValuePost s (car listq) (cadr listq) (getValue s (list c r))) c r blank))
                (t nil)
            )
        ))
        
    )
)
; move keeper or box to goal
(defun move-to-goal (s c r listq)
    (cond ((not s) nil)
        (( not listq) nil)
        (t (let ((v (getValue s (list c r))))
             (cond
                ((not v) nil)
                ((isboxstar v) (setValuePost (setValuePost s (car listq) (cadr listq) boxstar) c r star) )
                ((iskeeperstar v) (setValuePost (setValuePost s (car listq) (cadr listq) keeperstar) c r star) )
                ((isbox v) (setValuePost (setValuePost s (car listq) (cadr listq) boxstar) c r blank))
                ((iskeeper v) (setValuePost (setValuePost s (car listq) (cadr listq) keeperstar) c r blank))
                (t nil)
            )
        ))
    )
)


; move keeper to box
(defun move-to-box (s c r listq dir)
    (cond ((not s) nil)
        (( not listq) nil)
        (t (let ((v (getValue s (getNextCR (car listq) (cadr listq) dir))))
             (cond
                ((not v) nil)
                ((isstar v) 
                    (move-to-blank (move-to-goal s (car listq) (cadr listq) (getNextCR (car listq) (cadr listq) dir)) c r listq) )
                ((isblank v) 
                    (move-to-blank (move-to-blank s (car listq) (cadr listq) (getNextCR (car listq) (cadr listq) dir)) c r listq) )
                (t nil)
            )
            )
        )
    )
)
; move keeper to boxstar
(defun move-to-boxstar (s c r listq dir)
    (cond ((not s) nil)
        (( not listq) nil)
        (t (let ((v (getValue s (getNextCR (car listq) (cadr listq) dir))))
             (cond
                ((not v) nil)
                ((isstar v) 
                    (move-to-goal (move-to-goal s (car listq) (cadr listq) (getNextCR (car listq) (cadr listq) dir)) c r listq) )
                ((isblank v) 
                    (move-to-goal (move-to-blank s (car listq) (cadr listq) (getNextCR (car listq) (cadr listq) dir)) c r listq) )
                (t nil)
            )
            ))
    )
)
; dir 1=UP,2=DOWN,3=LEFT,4=RIGHT
(defun try-move (s c r dir)
    (cond ((not (getValue s (getNextCR c r dir))) nil)
        ((isBlank (getValue s (getNextCR c r dir))) (move-to-blank s c r (getNextCR c r dir)))
        ((isStar (getValue s (getNextCR c r dir))) (move-to-goal s c r (getNextCR c r dir)))
        ((isBox (getValue s (getNextCR c r dir))) 
                (move-to-box s c r (getNextCR c r dir) dir))
        ((isBoxStar (getValue s (getNextCR c r dir))) 
                (move-to-boxstar s c r (getNextCR c r dir) dir))
        (t nil)
    
    )
)
;self test
(setq p1 '((1 1 1 1 1 1)
	   (2 6 0 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 1 1 3))
(setq p1 '((1 1 1 1 1 1)
	   (5 6 0 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 1 1 3))
(setq p1 '((1 1 1 1 1 1)
	   (0 5 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 2 1 4))
(setq p1 '((1 1 1 1 1 1)
	   (1 0 6 2 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 2 1 4))
(setq p1 '((1 1 1 1 1 1)
	   (1 0 6 5 2 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 2 1 4))
(setq p1 '((1 1 1 1 1 1)
	   (1 0 6 2 2 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 2 1 4))
(setq p1 '((1 1 1 1 1 1)
	   (1 0 0 2 6 5)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 4 1 4))
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 2 0 3)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))
;(print (try-move p1 5 1 4))
; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s x y 1) (try-move s x y 2) (try-move s x y 3) (try-move s x y 4)))
	 )
    (cleanUpList result);end
   );end let
  );


;test exsample
(setq s1 '((1 1 1 1 1)
 (1 4 0 0 1)
 (1 0 2 0 1)
 (1 0 3 0 1)
 (1 0 0 0 1)
 (1 1 1 1 1)
 ))
 (setq s2 '((1 1 1 1 1)
 (1 0 0 4 1)
 (1 0 2 3 1)
 (1 0 0 0 1)
 (1 0 0 4 1)
 (1 1 1 1 1)
 ))
(setq s3 '((1 1 1 1 1)
 (1 0 0 6 1)
 (1 0 2 0 1)
 (1 0 0 0 1)
 (1 4 0 4 1)
 (1 1 1 1 1)
 ))
 (setq s4 '((1 1 1 1 1)
 (1 0 0 4 1)
 (1 0 0 0 1)
 (1 0 0 0 1)
 (1 0 5 3 1)
 (1 1 1 1 1)
 ))
;( print (next-states s4))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
    0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; count the number of misplaced box on maze
; iterations each position on the state
; It is not heuristic admissible.
(defun h1h (s)
  (cond ((not s) 0)
	((isBox (car s)) (+ 1 (h1h (cdr s))))
	(t (h1h (cdr s)) )
	)
)

(defun h1 (s)
  (cond ((not s) 0)
	(t(+ (h1h (car s)) (h1 (cdr s)))))
)

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h2 (s)
    (let* ((Keeperpos (getKeeperPosition s 0))
        (boxpos (getBoxPosition s 0))
        (starpos (getStarPosition s 0))
	 )
	 (cond ((not boxpos) (*(Totalcost Keeperpos starpos) (h1 s)))
	    (t (*(TotalcostBS Keeperpos boxpos starpos) (h1 s)))
	 )
    
 ))
;get the sum of the totaldistanct from player to each star
(defun Totalcost (keeperPos StarPosList)
  (cond ((null StarPosList) 0)
   (t (+ (dis keeperPos (car StarPosList)) (Totalcost keeperPos (cdr StarPosList))))
));end totalCost

;get the sum of the totaldistanct from player to each box and star
(defun TotalcostBS (keeperPos BoxPosList StarPosList)
  (cond ((null BoxPosList) 0)
    ((null StarPosList) 0)
   (t (+ (+ (dis keeperPos (car BoxPosList)) (dis keeperPos (car StarPosList))) (Totalcost TotalcostBS (cdr BoxPosList) (cdr StarPosList))))
));end totalCost


;A helper function to find box's position___________________________
;I implemented this based on the given getKeeperPosition function
(defun getBoxColumn (r c currR)
  (cond
   ((null r) NIL)
   ((isBox (car r)) (cons (list c currR) (getBoxColumn (cdr r) (+ c 1) currR)))
   (t(getBoxColumn (cdr r) (+ c 1) currR))
));end defun


;Get a list that contains all boxes' position 
(defun getBoxPosition(s r)
  (cond ((null s) NIL)
	(t(append (getBoxColumn (car s) 0 r) (getBoxPosition (cdr s) (+ r 1))))
	);end cond
);end defun
;A helper function to find star's position___________________________

(defun getStarColumn (r c currR)
  (cond
   ((null r) NIL)
   ((isStar (car r)) (cons (list c currR) (getStarColumn (cdr r) (+ c 1) currR)))
   (t(getStarColumn (cdr r) (+ c 1) currR))
));end defun


;Get a list that contains all boxes' position 
(defun getStarPosition(s r)
  (cond ((null s) NIL)
	(t(append (getStarColumn (car s) 0 r) (getStarPosition (cdr s) (+ r 1))))
	);end cond
);end defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0  0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
