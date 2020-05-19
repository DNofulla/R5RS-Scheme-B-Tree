;; This gets the collection of Nodes from RootNode
(define (getCollection rootNode)
  (car(cdr(cdr(cdr rootNode)))))

;; This gets the nextNode from the Collection of Rootnodes
(define (getNext rootNode)
  (car(cdr(car(cdr(cdr(cdr rootNode)))))))

;; This gets the current/First Node of the Collection of Rootnodes
(define (getCurr rootNode)
  (car(car(cdr(cdr(cdr rootNode))))))

;; This gets the Minimum of the rootNode
(define (getMin rootNode)
  (car(cdr rootNode)))

;; This gets the Maximum of the RootNode
(define (getMax rootNode)
  (car(cdr(cdr rootNode))))

;; This gets the Leaf Number List, without the "L" in the beginning
(define (getLeafNumberCollection leaf)
  (cdr leaf)
  )

;; This gets the type of any node ---> "L" or "R"
(define (getType node)
  (car node)
  )


;; If the number is not null and the value is equal to the current number, return true
;; if the number is not null and the value is not equal to the current number, do leafsearch on the list starting from the next value
;; else we return false
(define (leafSearch leaf value)
     (cond((= (length leaf) 0)     #f)
          ((null? '(car leaf)) #f)
          ((equal? '() '(car leaf)) #f)
          ((and (not (null? '(car leaf))) (= value (car leaf)) ) #t)
          ((and (not (null? '(car leaf))) (not(= value (car leaf))) (leafSearch (cdr leaf) value)))
          (else #f)
          
  ))


  
;; We check for bounds if we reach the end of the list (just like in LeafSearch
;; Check if the minimum and maximum bounds match, if they don't return false
;; if the first part of the current Node is equal to "R", it's a rootNode,
;; so we check bounds in that rootnode if they match with the value we are searching for, if not, return false,
;; otherwise recursively search the collection of nodes of the current rootNode.
;; If we find a leafNode, we do leafSearch on that node, and at the very end, if all else fails, we move onto the next Node.
(define (search tree value)

  ;; I know that this code looks like a mess, but all it does it just check for RootNode bounds and makes sure that just in case
  ;; the order of LeafNodes and RootNodes is all over the place, that it doesn't skip any of them and that it's always checking
  ;; for more nodes until there is none left
  (if (or (> value (getMax tree)) (< value (getMin tree))) #f
      (if (equal? "R" (getType (getCurr tree)))
          (if (and (>= value (getMin (getCurr tree))) (<= value (getMax (getCurr tree)))) (search (getCurr tree) value)
              (if (null? (getNext tree)) #f (search (getNext tree) value)))
          (if (equal? "L" (getType (getCurr tree)))
              (if (leafSearch (getLeafNumberCollection (getCurr tree)) value) #t
                  (if (null? (cdr(getCollection tree))) #f
                      (if (equal? "L" (getType(getNext tree)))
                          (if (leafSearch (getLeafNumberCollection(getNext tree)) value) #t (search (car(cdr(cdr(getCollection tree)))) value))
                          (if (equal? "R" (getType(getNext tree))) (search (getNext tree) value))))))))
  
  
  )
;; This was the original tree

;;(define tree ' ( "R" 100 999     
;;                     (       
;;                      ("R" 100 199 
;;                           (
;;                            ("L" 120 140 160 180)
;;                            )
;;                           )       
;;                      ("R" 200 299 
;;                           (
;;                            ("L" 220 240 260 280)
;;                            )
;;                          )      
;;                      )  )  )

(define tree '("R" 100 999
                   (
                    ("R" 100 199
                         (
                          ("L" 120 140 160 180)
                          )
                         )
                    ("R" 200 500
                         (
                          ("L" 220 240 260 280)
                          ("L" 290 300 310 400)
                          ("R" 401 500
                                     (
                                           ("L" 402 450 451 470 500)
                                     )
                               )
                         )
                         )
                    )))

(search tree 160) ;;t
(search tree 140) ;;t
(search tree 105) ;;f
(search tree 110) ;;f
(search tree 230) ;;f
(search tree 450) ;;t
(search tree 310) ;;t






