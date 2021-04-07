(print "TREE-CONTAINS TESTS")
(print (equal (TREE-CONTAINS 3 3) T)) ; returns T
(print (equal (TREE-CONTAINS 3 '((1 2 3) 7 8)) T)) ; returns T
(print (equal (TREE-CONTAINS 4 '((1 2 3) 7 8)) NIL)) ; returns NIL
(print (equal (TREE-CONTAINS 4 '((1 2 3) 7 (8 9 10))) NIL)) ; returns NIL
(print (equal (TREE-CONTAINS 8 '((1 2 3) 7 (8 9 10))) T)) ; returns T
(print (equal (TREE-CONTAINS 4 '(((0 1) 2 (3 4)) 7 ((8 9) 10 11))) T)) ; returns T
(print (equal (TREE-CONTAINS 5 '(((0 1) 2 (3 4)) 7 ((8 9) 10 11))) NIL)) ; returns NIL

(print "TREE-MIN TESTS")
(print (equal (TREE-MIN '()) NIL)) ; returns NIL
(print (equal (TREE-MIN '((1 2 3) 7 8)) 1)) ; returns 1
(print (equal (TREE-MIN '((2 3) 7 8)) 2)) ; returns 2
(print (equal (TREE-MIN 1) 1)) ; returns 1

(print "TREE-ORDER TESTS")
(print (equal (TREE-ORDER 3) '(3))) ; returns (3)
(print (equal (TREE-ORDER '((1 2 3) 7 8)) '(7 2 1 3 8))) ; returns (7 2 1 3 8)

(print "SUB-LIST TESTS")
(print (equal (SUB-LIST '(a b c d) 0 3) '(a b c))) ; returns (a b c) 
(print (equal (SUB-LIST '(a b c d) 3 1) '(d))) ; returns (d) 
(print (equal (SUB-LIST '(a b c d) 2 0) NIL)) ; returns NIL
(print (equal (SUB-LIST '(a b c d) 5 9) NIL)) ; return NIL
(print (equal (SUB-LIST '(a b c d) 3 9) '(d))) ; return (d)

(print "SPLIT-LIST TESTS")
(print (or (equal (SPLIT-LIST '()) '(NIL NIL)) (equal (SPLIT-LIST '()) NIL))) ; returns (NIL NIL) or NIL
(print (equal (SPLIT-LIST '(a b c d)) '((a b) (c d)))) ; returns ((a b) (c d)) 
(print (equal (SPLIT-LIST '(a b c d e)) '((a b c) (d e)))) ; returns ((a b c) (d e)) 
(print (equal (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f)))) ;returns ((a b c) (d e f))

(print "BTREE-HEIGHT TESTS")
(print (equal (BTREE-HEIGHT '()) NIL)) ; returns 0
(print (equal (BTREE-HEIGHT 1) 0)) ; returns 0
(print (equal (BTREE-HEIGHT '(1 2)) 1)) ; returns 1
(print (equal (BTREE-HEIGHT '(1 (2 3))) 2)) ; returns 2 
(print (equal (BTREE-HEIGHT '((1 2) (3 4))) 2)) ; returns 2 
(print (equal (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3)) ; returns 3 
(print (equal (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3)) ; returns 3

(print "LIST2BTREE TESTS")
(print (equal (LIST2BTREE '(1)) 1)) ; returns 1
(print (equal (LIST2BTREE '(1 2)) '(1 2))) ; returns (1 2)
(print (equal (LIST2BTREE '(1 2 3)) '((1 2) 3))) ; returns ((1 2) 3)
(print (equal (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4)))) ; returns ((1 2) (3 4))
(print (equal (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7)))) ; returns (((1 2) (3 4)) ((5 6) 7))
(print (equal (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8))))) ; returns (((1 2) (3 4)) ((5 6) (7 8)))

(print "BTREE2LIST TESTS")
(print (equal (BTREE2LIST 1) '(1))) ; returns (1)
(print (equal (BTREE2LIST '(1 2)) '(1 2))) ; returns (1 2)
(print (equal (BTREE2LIST '((1 2) 3)) '(1 2 3))) ; returns (1 2 3)
(print (equal (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4))) ; returns (1 2 3 4)
(print (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7))) ; returns (1 2 3 4 5 6 7)
(print (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8))) ; returns (1 2 3 4 5 6 7 8)

(print "IS-SAME TESTS")
(print (equal (IS-SAME '() '()) T)) ; returns T
(print (equal (IS-SAME '() '(1)) NIL)) ; returns NIL
(print (equal (IS-SAME '(1) '()) NIL)) ; returns NIL
(print (equal (IS-SAME '((1)) '(1)) NIL)) ; returns NIL
(print (equal (IS-SAME '((1)) '((1))) T)) ; returns NIL
(print (equal (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T)) ; returns T 
(print (equal (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) NIL)) ; returns NIL