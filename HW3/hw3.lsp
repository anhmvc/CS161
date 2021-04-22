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
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

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

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; check-row (L)
; returns NIL if any element in the L row is a box/keeper that is not on a
; non-goal square
(defun check-row (L)
	(cond ; returns T when every element is valid
	      ((null (car L)) t)
	      ; returns NIL if current element is either of the invalid states
		  ((or (isBox (car L)) (isKeeper (car L))) NIL)
		  ; recursively calls the function with the next element in L
		  (t (check-row (cdr L)))
	);end cond
  );end defun

(defun goal-test (s)
  (cond ; terminate when finishes iterating through every row in matrix
  		((null (car s)) t) 
		; returns NIL if a row contains invalid element
        ((null (check-row (car s))) NIL) 
		; recursively calls the function with the next row
		(t (goal-test (cdr s)))
	);end cond
  );end defun


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

; get-col (r c)
; returns the integer content at the column number by iterating through row
(defun get-col (r c)
	(cond ; returns wall if out of scope 
		  ((or (> c (- (length r) 1)) (null r)) wall)
		  ; once c hits 0, returns the current element at that position
		  ((= c 0) (car r))
		  ; else, recursively call function with column number decremented
		  (t (get-col (cdr r) (- c 1))) 
	);end cond
 );end defun

; get-square (s r c)
; returns the integer content of state s at square (r,c)
(defun get-square (s r c)
	(cond ; return wall if out of scope
		  ((or (> r (- (length s) 1)) (null s)) wall)
		  ; if r = 0, start iterating through the current row to get column
		  ((= r 0) (get-col (car s) c))
		  ; else, recursively call function with row number decremented
		  (t (get-square (cdr s) (- r 1) c))
	);end cond
 );end defun

; set-col (r c v)
; returns a new row with element at col c set to value v
(defun set-col (r c v)
	(cond ; add v to the rest of the list and terminate if c is 0
		  ((= c 0) (cons v (cdr r)))
		  ; else add current element to result
		  (t (cons (car r) (set-col (cdr r) (- c 1) v)))
	);end cond
 );end defun

; set-square (s r c v)
; returns a new state s' by setting square (r,c) to value v
(defun set-square (s r c v)
	(cond ((= r 0) (cons (set-col (car s) c v) (cdr s)))
		  (t (cons (car s) (set-square (cdr s) (- r 1) c v)))
	);end cond
 );end defun

(defun try-move (s r c d)
	(let* ((new-pos
			(cond ((equal d 'UP) (list (- r 1) c))
				  ((equal d 'DOWN) (list (+ r 1) c))
				  ((equal d 'LEFT) (list r (- c 1)))
				  ((equal d 'RIGHT) (list r (+ c 1)))
				);end cond
			 );end new-pos
		   (new-r (car new-pos))
		   (new-c (cadr new-pos))
		   (curr-value (get-square s r c))
		   (new-value (get-square s new-r new-c)));end def
		(cond ; if new position is wall, do nothing return NIL
			  ((isWall new-value) NIL)
			  ; if new position is blank, move keeper to new position
			  ; and set current position to blank if current is keeper, star if current is already on a goal
			  ((isBlank new-value) 
			  		(let ((move-keeper (set-square s new-r new-c keeper)))
					  	(cond ((isKeeper curr-value) (set-square move-keeper r c blank))
							  ((isKeeperStar curr-value) (set-square move-keeper r c star)) 
						);end cond
					);end let
			  );end isBlank
			  ; if new position is goal, move keeper to goal at new position
			  ; and set current position to blank if current is keeper, star if current is already on a goal
			  ((isStar new-value) 
					(let ((move-keeper (set-square s new-r new-c keeperstar)))
						(cond ((isKeeper curr-value) (set-square move-keeper r c blank))
							  ((isKeeperStar curr-value) (set-square move-keeper r c star))
						);end cond
					);end let
			  );end isStar
			  ; if new position is box or box star
			  ((or (isBox new-value) (isBoxStar new-value))
			  		(let* ((after-box-pos
							(cond ((equal d 'UP) (list (- new-r 1) new-c))
				  				  ((equal d 'DOWN) (list (+ new-r 1) new-c))
				  				  ((equal d 'LEFT) (list new-r (- new-c 1)))
				  				  ((equal d 'RIGHT) (list new-r (+ new-c 1)))
							);end cond
			 			   );end after-box-pos
		   				  (after-r (car after-box-pos))
		   				  (after-c (cadr after-box-pos))
		   				  (after-box (get-square s after-r after-c)));end def
						(cond 
						  	; illegal moves
							((or (isWall after-box) (isBox after-box) (isBoxStar after-box)) NIL);end isWall/isBox/isBoxStar
							; if the space next to the box is blank, move the box to the blank,
							; move the keeper to the box, and set current to blank if current is keeper,
							; star if current is already on a goal
						  	((isBlank after-box) 
							  	(cond ((isBoxStar new-value) 
								  		 (let* ((move-box (set-square s after-r after-c box))
									  		   (move-keeper (set-square move-box new-r new-c keeperstar)))
											(cond ((isKeeper curr-value) (set-square move-keeper r c blank))
							  					  ((isKeeperStar curr-value) (set-square move-keeper r c star))
											);end cond
									 	 );end let
								  	  );end isBoxStar
								  	  ((isBox new-value) 
										(let* ((move-box (set-square s after-r after-c box))
									  		   (move-keeper (set-square move-box new-r new-c keeper)))
											(cond ((isKeeper curr-value) (set-square move-keeper r c blank))
							  					  ((isKeeperStar curr-value) (set-square move-keeper r c star))
											);end cond
									 	);end let
									  );end isBox
								);end cond
						  	);end isBlank
							; if the space next to the box is a goal, move the box to goal,
							; move the keeper to the box, and set current to blank if current is keeper,
							; star if current is already on a goal
							((isStar after-box) 
								(cond ((isBoxStar new-value) 
								  		 (let* ((move-box (set-square s after-r after-c boxstar))
									  		   (move-keeper (set-square move-box new-r new-c keeperstar)))
											(cond ((isKeeper curr-value) (set-square move-keeper r c blank))
							  					  ((isKeeperStar curr-value) (set-square move-keeper r c star))
											);end cond
									 	 );end let
								  	  );end isBoxStar
								  	  ((isBox new-value) 
										(let* ((move-box (set-square s after-r after-c boxstar))
									  		   (move-keeper (set-square move-box new-r new-c keeper)))
											(cond ((isKeeper curr-value) (set-square move-keeper r c blank))
							  					  ((isKeeperStar curr-value) (set-square move-keeper r c star))
											);end cond
									 	);end let
									  );end isBox
								);end cond
						  	);end isStar
					   	);end cond
					);end let
			  );end isBox
		);end cond
	);end let
 );end defun

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s y x 'UP) (try-move s y x 'DOWN) (try-move s y x 'LEFT) (try-move s y x 'RIGHT)))
	 )
    (cleanUpList result);end
   );end let
  )

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0 ; returns the constant 0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
;
(defun count-boxes-in-row (L)
	(cond ; returns 0 when the current row is empty
		  ((null (car L)) 0)
		  ; if current element is a box (2), add 1 to result
		  ((isBox (car L)) (+ (count-boxes-in-row (cdr L)) 1))
		  ; recursively calls function with the rest of the list
		  (t (count-boxes-in-row (cdr L)))
	);end cond
  );end defun

; h1 (s)
; returns the number of boxes which are not on goal positions in the given state
; This heuristic is admissible because it does not overestimate the distance, or the cost, of
; reaching the goal state. Since our goal is to move all boxes into a goal space, counting the number
; of boxes which are not on goal positions accurately represents how many more steps, or how many 
; more boxes need to be moved for us to reach the goal state and will not overestimate the real cost
; to solve the problem.
(defun h1 (s)
	(cond ; returns 0 if the row is empty
		  ((null (car s)) 0)
		  ; add the total boxes per row then recursively call with the rest of the rows
		  (t (+ (count-boxes-in-row (car s)) (h1 (cdr s))))
	);end cond
  );end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; manhattan-dist (x1 x2 y1 y2)
; returns the manhattan distance between two points calculated as |x1-x2|+|y1-y2|
(defun manhattan-dist (x1 x2 y1 y2)
	(+ (abs (- x1 x2)) (abs (- y1 y2)))
 )

; check-row-for-type (type row r-num c-num)
; returns a list of all the coordinates of 'type' elements found in row
; if forms of (r-num, c-num)
(defun check-row-for-type (type row r-num c-num)
	(cond ; return NIL is row is empty
		  ((null row) NIL)
		  ; if current element matches type, add the coordinates to result
		  ((= (car row) type) (cons (list r-num c-num) (check-row-for-type type (cdr row) r-num (+ c-num 1))))
		  ; else, iterate through the rest of the row
		  (t (check-row-for-type type (cdr row) r-num (+ c-num 1)))
	);end cond
 );end defun

; get-all-coords-for-type (type matrix r-num c-num)
; returns a list of all the coordinates of 'type' elements found in matrix
; utilizing helper function check-row-for-type
(defun get-all-coords-for-type (type matrix r-num c-num)
	(cond ; returns NIL if matrix is empty
		  ((null matrix) NIL)
		  ; iterate through every row to find all coordinates and append them
		  ; together to accumulate result
		  (t (append (check-row-for-type type (car matrix) r-num c-num) (get-all-coords-for-type type (cdr matrix) (+ r-num 1) c-num)))
	);end cond
 );end defun

; shortest-distance (goals item)
; returnsthe shortest distance between all goals and 1 item given list of (r,c) goals
; coordinates and (r,c) item coordinates
(defun shortest-distance (goals item)
	(let* ((item-r (car item))
		   (item-c (cadr item))
		   (goal-pos (car goals))
		   (goal-r (car goal-pos))
		   (goal-c (cadr goal-pos))
		   (curr-dist (manhattan-dist item-r goal-r item-c goal-c)))
		(cond ; returns the distance between item and goal if there's only 1 goal
			  ((= (length goals) 1) curr-dist)
			  ; returns the minimum between current distance and min distances of
			  ; the item with rest of the goals
			  (t (min curr-dist (shortest-distance (cdr goals) item)))
		);end cond
	);end let
 );end defun

; total-distances (goals boxes)
; returns the total distances between all the boxes that are not at the goal 
; to its closest goal
(defun total-distances (goals boxes)
	(cond ((null boxes) 0)
		   (t (+ (shortest-distance goals (car boxes)) (total-distances goals (cdr boxes))))
	);end cond
 );end defun

(defun h905111606 (s)
	(let* ((keeper-pos (getKeeperPosition s 0))
		   (boxes (get-all-coords-for-type box s 0 0))
		   (goals (get-all-coords-for-type star s 0 0))
		   (boxes-goals-score (total-distances goals boxes)))
		(cond ; goal state return 0
			  ((isKeeperStar (get-square s (second keeper-pos) (first keeper-pos))) 0)
			  ; there are still boxes not in goals
			  ((not (= boxes-goals-score 0)) 
				(cond ; return the current score if there are no more boxes
					  ((null boxes) boxes-goals-score)
					  ; else add the shortest distance of the keeper with the closest
					  ; box to the score
					  (t (+ (shortest-distance boxes keeper-pos)) boxes-goals-score)
				);end cond
			  )
			  ; else, all boxes are in-place, only have to move keeper
			  (t (cond ((null boxes) 0)
			  			(t (shortest-distance goals keeper-pos))
			  	 );end cond
			  );end otherwise
		);end cond
	);end let
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
	    (0 2 1 4 0 4 0)
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
