; Name: Anh Mac
; UID: 905-11-606
; CS161, Spring 2021

; 1. TREE-CONTAINS -- checks whether number N appears in the ordered tree TREE
; args: N - int, TREE - ordered-tree int/list (L m R); returns - bool
; For the base case, the function checks whether TREE is a number (an atom),
; then returns NIL if TREE is NIL or TREE /= N and returns T if TREE = N.
; For the recursive case, it binds the 3 elements (L, m, R) to variables
; then performs binary search by recursively calling TREE-CONTAINS with 
; L if N < m and R if N > m.
(defun TREE-CONTAINS (N TREE)
    (cond ((atom TREE) (cond ((OR (NULL TREE) (NOT (= N TREE))) NIL)
                              ((= N TREE) t)))
          (t (let ((L (car TREE))
                   (m (cadr TREE))
                   (R (caddr TREE)))
            (cond ((< N m) (TREE-CONTAINS N L))
                  ((> N m) (TREE-CONTAINS N R))
                  ((= N m) t))))))

; 2. TREE-MIN -- returns the minimum number in ordered tree TREE
; args: TREE - ordered-tree int/list (L m R); returns - int
; Since TREE is an ordered tree, the minimum number is simply the left-most
; number in the list. The function recursively calls itself with the first
; element of the list, which will be the left-most number once it reaches the
; base case, which is when TREE is a number.
(defun TREE-MIN (TREE)
    (cond ((atom TREE) TREE)
           (t (TREE-MIN (car TREE)))))

; 3. TREE-ORDER -- returns a pre-ordered list of the numbers in TREE
; args: TREE - ordered-tree int/list (L m R); returns - list
; Pre-order traversal visits the tree in the following order: root, left,
; right. The base case is when the TREE is a number, which it needs to
; convert to a list. The recursion calls TREE-ORDER by appending list results
; with root (m), pre-ordered list of L, and pre-ordered list of R.
(defun TREE-ORDER (TREE)
    (cond ((atom TREE) (list TREE))
           (t (let ((L (car TREE))
                    (m (cadr TREE))
                    (R (caddr TREE)))
          (append (list m) (TREE-ORDER L) (TREE-ORDER R))))))

; 4. SUB-LIST -- returns sub-list of L starting at position START and having length LEN
; args: L - list, START/LEN - non-negative int; returns - list
; The base cases account for conditions where LEN or START is 0. To find the starting
; point, it recursively calls the function with the rest of the list and START
; decremented. The function starts appending the first element of the sublist when
; START = 0, and recursively calls the function with the rest of the list and LEN
; decremented. It reaches the stopping point once LEN hits 0 and returns our 
; resulting sublist. 
(defun SUB-LIST (L START LEN)
    (cond ((or (NULL L) (= LEN 0)) NIL)
          ((= START 0) (append (list (car L)) (SUB-LIST (cdr L) 0 (- LEN 1))))
           (t (SUB-LIST (cdr L) (- START 1) LEN))))

; 5. SPLIT-LIST -- returns a list of two lists L1 and L2 such that L is the result
; of appending L1 and L2, and |L1|-|L2| = 0 or 1.
; args: L - list; returns - (L1 L2) where L1-list L2-list
; To split the list L into L1 and L2, we find the midpoint by taking the ceiling of
; length of L divided by 2. We get the lists L1 and L2 by calling SUB-LIST with 
; START=0 and LEN=mid for L1, and START=midpoint and LEN=LENGTH(L)-mid for L2.
(defun SPLIT-LIST (L)
    (cond ((NULL L) NIL)
          (t (let ((mid (ceiling (/ (length L) 2))))
        (list (SUB-LIST L 0 mid) (SUB-LIST L mid (- (length L) mid)))))))

; 6. BTREE-HEIGHT -- returns the height of a binary tree or the length of the longest
; path from the root node to the farthest leaf node
; args: TREE - binary tree list (L R); returns - int
(defun BTREE-HEIGHT (TREE)
    (cond ((NULL TREE) NIL)
          ((atom TREE) 0)
           (t (let ((L (car TREE))
                    (R (cadr TREE)))
        (cond ((> (BTREE-HEIGHT L) (BTREE-HEIGHT R)) (+ 1 (BTREE-HEIGHT L)))
               (t (+ 1 (BTREE-HEIGHT R))))))))

; 7. LIST2BTREE -- takes a non-empty list of atom LEAVES, and returns a binary tree
; args: LEAVES - list of atoms; returns - TREE - binary tree list (L R)
; For the base case, if LEAVES is a leaf node, or a list of length 1,
; returns that number. For the recursive step, recursively split LEAVES
; into two halves and stops once it hits the base case (1 element), then
; append results.
(defun LIST2BTREE (LEAVES)
    (cond ((= (length LEAVES) 1) (car LEAVES))
          (t (cons (LIST2BTREE (car (SPLIT-LIST LEAVES))) (cons (LIST2BTREE (cadr (SPLIT-LIST LEAVES))) '())))))

; 8. BTREE2LIST -- takes in a binary tree and returns list of atoms
; args: TREE - binary tree list (L R); returns - list of atoms
; For the base case, if TREE is a single atom, returns it. Then, recurisvely
; split the TREE into L and R and append the results of them together to
; flatten all nested elements into a single list of atoms.
(defun BTREE2LIST (TREE)
    (cond ((atom TREE) (list TREE))
          (t (let ((L (car TREE))
                   (R (cadr TREE)))
                (append (BTREE2LIST L) (BTREE2LIST R))))))

; 9. IS-SAME -- checks whether E1 and E2 are idential using only '=' to
; test equality
; args: E1/E2 - expressions whose atoms are all numbers; returns - bool
; Base case: If both expressions are either NULL or they are both atom
; then return T. If one is not an atom while the other is, vice versa,
; or if they're both atoms but are not equal, return NIL. Recursively
; compare the first element of each expression until it reaches the
; base case where they are both atoms.
(defun IS-SAME (E1 E2)
    (cond ((and (NULL E1) (NULL E2)) t)
          ((or (and (atom E1) (not (atom E2))) 
               (and (not (atom E1)) (atom E2)) 
               (and (atom E1) (atom E2) (not (= E1 E2)))) NIL)
          ((and (atom E1) (atom E2) (= E1 E2)) t)
          (t (IS-SAME (car E1) (car E2)))))