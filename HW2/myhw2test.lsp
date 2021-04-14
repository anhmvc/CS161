(print "BFS TESTS")
(print (equal (BFS '()) '()))
(print (equal (BFS '(A)) '(A)))
(print (equal (BFS '(A (B))) '(A B)))
(print (equal (BFS '((W X) (Y Z))) '(W X Y Z)))
(print (equal (BFS '((A (B)) C (D))) '(C A D B)))
(print (equal (BFS '(A (B C) (D) (E (F G)))) '(A B C D E F G)))

(print "DFS TESTS")
(print (equal (DFS '()) '()))
(print (equal (DFS '(A)) '(A)))
(print (equal (DFS '(A (B))) '(B A)))
(print (equal (DFS '((W X) (Y Z))) '(Z Y X W)))
(print (equal (DFS '((A (B)) C (D))) '(D C B A)))
(print (equal (DFS '(A (B C) (D) (E (F G)))) '(G F E D C B A)))

(print "DFID TESTS")
(print (equal (DFID '((A (B)) C (D)) 3) '(C A C D A B C D)))
(print (equal (DFID '(A (B C) (D) (E (F G))) 3) '(A A B C D E A B C D E F G)))

(print "NEXT-STATE TESTS")
(print (equal (next-state '(3 3 t) 1 0) NIL)) ; -> NIL
(print (equal (next-state '(3 3 t) 2 0) NIL))
(print (equal (next-state '(2 2 t) 1 0) NIL))
(print (equal (next-state '(1 1 t) 1 1) '((3 3 NIL))))
(print (equal (next-state '(3 3 t) 0 1) '((0 1 NIL))))
(print (equal (next-state '(1 0 t) 1 0) '((3 3 NIL))))

(print "SUCC-FN TESTS")
; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
(print (equal (succ-fn '(3 3 t)) '((0 1 NIL) (1 1 NIL) (0 2 NIL)))) ; -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
(print (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL)))) ; -> ((3 2 NIL) (3 3 NIL))

(print "ON-PATH TESTS")
(print (equal (on-path '(0 1 NIL) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) T))
(print (equal (on-path '(1 1 NIL) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) T))
(print (equal (on-path '(0 2 NIL) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) T))
(print (equal (on-path '(0 2 t) '((0 1 NIL) (1 1 NIL) (0 2 NIL))) NIL))

(print "MC-DFS TESTS")
(print (equal (mc-dfs '(3 3 T) NIL) '((3 3 T) (1 1 NIL) (3 2 T) (0 3 NIL) (3 1 T) (2 2 NIL) (2 2 T) (3 1 NIL) (0 3 T) (3 2 NIL) (1 1 T)
 (3 3 NIL))))