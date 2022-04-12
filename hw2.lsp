
;performs a breadth-first search of a tree.
;;Imput: the list representation of the tree
;;;OUtput: a single, top-level list of the terminal nodes
;;;
(defun BFS (x)
(cond ((not x) nil)
      ((atom (car x)) (append (list (car x)) (BFS (cdr x))));find the target
      (t (BFS ( append (cdr x) (car x)))) ;deque of the list

)
)
            





;a depth-first search of a tree.
;;Imput: the list representation of the tree
;;;OUtput: a single, top-level list of the terminal nodes
;;;
(defun DFS (x)
(cond  ((not x) nil)
       ((not (cdr x)) (car x))
       ((atom (car x)) (append (list (car x)) (DFS (cdr x))))
       (t (append (DFS (car x)) (DFS (cdr x))))


)

)



;function DFSRL
;Input: the list representation of the tree, and an integer representing the limited depth of the tree, 
;Output: a single top-level list of the terminal nodes in the order that they would be visited 
;by a right-to-left depth-first iterative-deepening search
(defun DFSRL (x n)
(cond  ((not x) nil)
       ((atom  x) (cons x nil) )
       ((= n 0) nil)
       (t (append (DFSRL (rest x) n) (DFSRL (first x) (- n 1)) ))

)
)
;function DFID
;Input: the list representation of the tree, and an integer representing the maximum depth of the tree, 
;Output: a single top-level list of the terminal nodes in the order that they would be visited 
;by a right-to-left depth-first iterative-deepening search
(defun DFID (x n)
(cond ((= n 0) nil)
      (t (append (DFID x (- n 1)) (DFSRL x n)) ) 

)
)



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
(cond ((equal s '(3 3 nil)) t)
	(t NIL)
)
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
(cond ((> (+ m c) 2) nil)
      ((< (- (car s) m) 0) nil)
      ((< (- (cadr s) c) 0) nil)
      ((and (< (- (car s) m) (- (cadr s) c)) (not (= (- (car s) m) 0))) nil)
	  ((and (< (+ (- 3 (car s)) m) (+ (- 3 (cadr s)) c)) (not (= (- 3 (car s)) 0))) nil)
      (t (list (append (list (+ (- 3 (car s)) m)) (list (+ (- 3 (cadr s)) c)) (list (not (caddr s))))))
)
)
 
; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
 (append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 0 2) (next-state s 2 0))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
(cond ((not states) nil)
      (t (or (equal s (car states)) (on-path s (cdr states))))
)
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
(cond ((not states) nil)
      ((mc-dfs (car states) path) (mc-dfs (car states) path))
      (t (mult-dfs (cdr states) path))

)
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
(cond 
      ((final-state s) (append path (list s)))
      ((not (on-path s path)) (mult-dfs (succ-fn s) (append path (list s))))
      (t nil)


)
)
  
; Function execution examples

; Applying this operator would result in an invalid state, with more O's
; than X's on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one O and zero X on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

