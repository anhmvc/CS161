;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (backtrackSearch n 1 delta '()))


; addAssignment (index value result) 
; returns a list of assignments of literals 1 to index
; args: index - int index of the literal that we want to add, result - list of assignments
; value - boolean value to assign the literal at current index
; returns: result - list of assigned values
(defun addAssignment (index value result)
  (cond ((null value) (append result (list (- index))))
        (t (append result (list index)))
  )
 )

; isLiteralTrue (literal assignments) 
; returns T if the literal returns true for any of the literal in the assignment, NIL otherwise. 
; The function recursively checks every literal in the assignments with the current literal.
; args: literal - int value, assignments - list of assignments
; returns: boolean - T if literal matches any assignment, NIL otherwise.
(defun isLiteralTrue (literal assignments)
  (cond ((null assignments) T)
        ((equal (abs literal) (abs (first assignments))) (equal literal (first assignments)))
        (t (isLiteralTrue literal (rest assignments)))
  )
)

; isClauseTrue (clause assignments) 
; returns T if any literal within the clause returns T with the current assignments, NIL otherwise. 
; The function recursively check the assignments with every literal in the clause.
; args: clause - list of literals in CNF form, assignment - list of current literals assignments
; returns: boolean - T if any literal returns True, else NIL
(defun isClauseTrue (clause assignments)
  (cond ; if clause is empty, returns NIL
        ((null clause) NIL)
        ; check the first literal with all the items in the assignments
        ((isLiteralTrue (first clause) assignments) T)
        ; else, check the other literals with the assignments
        (t (isClauseTrue (rest clause) assignments))
  )
)

; isCnfTrue (cnf assignments) 
; returns T if the all the clauses returns T with given assignments, NIL otherwise. The function 
; recurisvely checks the assignments with every clause in the CNF.
; args: cnf - list of clauses, assignments - list of current ltierals assignments
; returns: boolean - T if every clause returns T, NIL otherwise
(defun isCnfTrue (cnf assignments)
  (cond ((or (null cnf) (and (not (null cnf)) (null assignments))) T)
        (t (and (isClauseTrue (first cnf) assignments) (isCnfTrue (rest cnf) assignments))) 
  )
)

; backtrackSearch (n level delta assignmentws) 
; performs DFS on the search tree, assigning T/NIL value to a literal at each level.
; args: n - int number of variables in delta, level - current level of the search (starts at 1),
; delta - list of clauses in CNF, assignments - list of assignments of variables
; returns: list - assignments of each variable that would satisfy to SAT problem 
(defun backtrackSearch (n level delta assignments)
    (cond ; if CNF can't be true with current assignments, prune that node
          ((not (isCnfTrue delta assignments)) NIL)
          ; base case if the search reaches the max depth (n = level-1)
          ((= n (- level 1)) 
            (cond ; returns the current assignments if the CNF returns true
                  ((isCnfTrue delta assignments) assignments)
                  ; else, returns NIL
                  (t NIL)))
          ; search the next level by assigning the next variable and continue search
          (t (or (backtrackSearch n (+ level 1) delta (addAssignment level T assignments))
                 (backtrackSearch n (+ level 1) delta (addAssignment level NIL assignments))))
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

