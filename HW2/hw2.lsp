; Name: Anh Mac
; UID: 905-11-606
; CS161, Spring 2021

; 1. BFS -- performs a breadth-first search of a tree.
; args: TREE - list; returns - list of the terminal nodes in the order they would be visited by a left-to-right BFS.
; For the base case, we check if TREE is empty then return NIL, or if it's a leaf node, then add to result and 
; process the rest of the TREE. The function treats TREE similar to a queue, recursively moving the first element 
; to the back of the queue if it is not an atom to process all terminal nodes in each level before moving to 
; the next.
(defun BFS (TREE)
    (cond 
        ; current node is empty, return empty list
        ((NULL TREE) NIL)
        ; current node is a leaf node, add it to the result and continues to
        ; the next iteration with the next recursion call
        ((atom (car TREE)) (cons (car TREE) (BFS (cdr TREE))))
        ; else, run BFS with the rest of the TREE by appending the current node
        ; to the end of the list and move other nodes forward until it finds a 
        ; leaf node in the current level 
        (t (BFS (append (cdr TREE) (car TREE))))))

; 2. DFS -- performs a depth-first search of a tree
; args: TREE - list; returns - list of terminal nodes in the order they would be visited by a right-to-left DFS.
; For the base case, we check if the TREE is empty or if it is a leaf node, then return NIL or that element.
; We recursively iterate through each level until we reached the max depth of the current path. Since we need
; to go from a right-to-left order, we process the last elements of the list before moving towards the beginning.
(defun DFS (TREE)
    (cond 
        ; if current node is empty, return empty list
        ((NULL TREE) NIL)
        ; if current node is a leaf node, we have reached the max depth so add it to result
        ((atom TREE) (list TREE))
        ; else, run DFS by calling function on the end of the list, and appending it to
        ; the DFS ran on the beginning of a tree to find the terminal nodes in right-to-left
        ; order 
        (t (append (DFS (cdr TREE)) (DFS (car TREE))))))

; 3. DFID-DFS -- helper function for DFID. Performs a limited depth-first search and returns
; a list of terminal nodes in left-to-right order.
; args: TREE - list, DEPTH - int; returns - list of terminal nodes in the order they would be
; visited by a left-to-right DFS.
; For the base case, the function checks whether TREE is empty or that we have reached the
; depth limit allowed as passed in as parameter, then return NIL. Else, recursively call the
; function with depth decremented, processing from left-to-right order, meaning from beginning
; towards end. 
(defun DFID-DFS (TREE DEPTH)
    (cond 
        ; base case: stop iteration when TREE is empty or we have reached the depth limit
        ((or (NULL TREE) (< DEPTH 0)) NIL)
        ; if current node is a leaf node, return the element to add to result
        ((atom TREE) (list TREE))
        ; else, run DFS on the next level by calling function on the same first element with 
        ; depth decremented and append it to the result of running DFS on the rest of the TREE 
        ; to follow the left-to-right order
        (t (append (DFID-DFS (car TREE) (- DEPTH 1)) (DFID-DFS (cdr TREE) DEPTH)))))

; DFID -- top level function for depth-first iterative-deepning. It returns
; a list of the terminal nodes in the order that they would be visited by a
; left-to-right depth-first iterative-deepening search.
; args: TREE - list, DEPTH - int; returns - list of terminal nodes
; For the base case, we return once we hit the max depth limit or that current node is empty.
; We then perform the iterative-deepening by recursively calling DFID-DFS with given DEPTH
; then decrementing DEPTH and call DFID to perform DFS again. 
(defun DFID (TREE DEPTH)
    (cond
        ; base case: returns when we reached the depth limit or node is empty
        ((or (NULL TREE) (< DEPTH 0)) NIL)
        ; recursively call DFID with depth decremented while appending results
        ; found by DFID-DFS to get the list of terminal nodes visited at each
        ; depth
        (t (append (DFID TREE (- DEPTH 1)) (DFID-DFS TREE DEPTH)))))

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let* ; get current state of the current side from parameters
        ((CURR-M (first s))
         (CURR-C (second s))
         (SIDE (third s))
         ; get current state of the other side by subtracting total (3) from what
         ; is at our current side
         (OTHER-M (- 3 CURR-M))
         (OTHER-C (- 3 CURR-C))
         (OTHERSIDE (not SIDE)))
      (cond 
          ; check invalid states/actions
          ((or ; after moving, number of cannibals > number of missionaries 
               ; on current side
               (and (> (- CURR-C c) (- CURR-M m))
                    (> (- CURR-M m) 0))
               ; after moving, number of cannibals > number of missionaries
               ; on opposite side 
               (and (> (+ OTHER-C c) (+ OTHER-M m)) 
                    (> (+ OTHER-M m) 0))
               ; trying to move more number of missionaries/cannibals than available
               ; trying to move more than 2 that would fit in a boat
               (> m CURR-M) (> c CURR-C) (> (+ m c) 2)) NIL)
          (t (list (list (+ OTHER-M m) (+ OTHER-C c) OTHERSIDE))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  ; applying all valid actions to the current state to get all successor 
  ; states
  (append (next-state s 1 0)
          (next-state s 0 1)
          (next-state s 1 1)
          (next-state s 2 0)
          (next-state s 0 2)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ; if states is empty, return NIL
        ((NULL states) NIL)
        ; if the first element is equal to the current state, return T
        ((equal s (car states)) t)
        ; else, search through the rest of the list
        (t (on-path s (cdr states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond ; if states is empty or we searched through everything, return NIL
        ((NULL states) NIL)
        ; if there exists a path from the first state to the final state
        ; in path, return that path using mc-dfs
        ((mc-dfs (car states) path) (mc-dfs (car states) path))
        ; else, recursively search for a path in the rest of the states
        (t (mult-dfs (cdr states) path))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ; stops searching if s in already on the search path or there is
        ; no sucessor states
        ((or (on-path s path) (NULL (succ-fn s))) NIL)
        ; if it reaches the goal state, add state to current path and returns
        ; the entire path in reversed order from initial -> state
        ((final-state s) (reverse (cons s path)))
        ; else, recursively call the function with generated successor states
        ; and append current state to path
        (t (mult-dfs (succ-fn s) (cons s path)))))