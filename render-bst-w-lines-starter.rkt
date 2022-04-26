
;; render-bst-w-lines-starter.rkt

(require 2htdp/image)

;; Constants

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define KEY-VAL-SEPARATOR ":")

(define MTTREE (rectangle 20 1 "solid" "white"))



;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree
; .

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false)) 
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100 
  (make-node 100 "large" BST10 false))


#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions:

; 
; Here is a sketch of one way the lines could work. What 
; this sketch does is allows us to see the structure of
; the functions pretty clearly. We'll have one helper for
; the key value image, and one helper to draw the lines.
; Each of those produces a rectangular image of course.
; 
; .
; 
; And here is a sketch of the helper that draws the lines:
; .  
; where lw means width of left subtree image and
;       rw means width of right subtree image



;; Functions:

;; BST -> Image
;; produce a SUMPLE rendering of the BST with lines
;; ASSUME BST is relatively well balanced
;(define (render-bst bst) MTTREE)

(check-expect (render-bst false) MTTREE)

(check-expect (render-bst BST1) (above
                                 (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)  
                                 (overlay
                                  (beside
                                   (line (- (/ (image-width MTTREE) 2))
                                         (/ (+
                                             (image-width MTTREE)     
                                             (image-width MTTREE)) 4)
                                         "black")
                                   (line (/ (image-width MTTREE) 2)     
                                         (/ (+
                                             (image-width MTTREE)     
                                             (image-width MTTREE)) 4)
                                         "black"))
                                  (rectangle
                                   (+
                                    (image-width MTTREE)       
                                    (image-width MTTREE))
                                   (/ (+
                                       (image-width MTTREE)    
                                       (image-width MTTREE)) 4)
                                   "solid"
                                   "white"))
                                 (beside
                                  (render-bst false)
                                  (rectangle (- (/ (+
                                                    (image-width MTTREE)     
                                                    (image-width MTTREE)) 2)
                                                (/ (image-width MTTREE) 2)
                                                (/ (image-width MTTREE) 2)) 1 "solid" "white")
                                  (render-bst false))))

(check-expect (render-bst BST4) (above
                                 (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)   
                                 (overlay
                                  (beside
                                   (line (- (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2))
                                         (/ (+
                                             (image-width MTTREE)     
                                             (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                         "black")
                                   (line (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2) 
                                         (/ (+
                                             (image-width MTTREE)     
                                             (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                         "black"))
                                  (rectangle
                                   (+
                                    (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))        
                                    (image-width MTTREE))
                                   (/ (+
                                       (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))     
                                       (image-width MTTREE)) 4)
                                   "solid"
                                   "white"))
                                 (beside
                                  (render-bst false)
                                  (rectangle (- (/ (+
                                                    (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))     
                                                    (image-width MTTREE)) 2)
                                                (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2)
                                                (/ (image-width MTTREE) 2)) 1 "solid" "white")
                                  (render-bst BST7))))


(check-expect (render-bst BST3) (above
                                 (text (string-append "3" KEY-VAL-SEPARATOR "ilk") TEXT-SIZE TEXT-COLOR)   
                                 (overlay (beside
                                           (line (- (/ (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)) 2))
                                                 (/ (+
                                                     (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                                     (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 4)
                                                 "black")
                                           (line (/ (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)) 2)
                                                 (/ (+
                                                     (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                                     (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 4)
                                                 "black"))
                                          (rectangle
                                           (+
                                            (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))        
                                            (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)))
                                           (/ (+
                                               (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                               (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 4)
                                           "solid"
                                           "white"))
                                 (beside
                                  (render-bst BST1)
                                  (rectangle (- (/ (+
                                                    (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                                    (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 2)
                                                (/ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)) 2)
                                                (/ (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)) 2)) 1 "solid" "white")
                                  (render-bst BST4))))

(define (render-bst t)
  (cond [(false? t) MTTREE]
        [else
         (above
          (text (string-append (number->string (node-key t)) KEY-VAL-SEPARATOR (node-val t)) TEXT-SIZE TEXT-COLOR)
          (render-lines t)
          (beside
           (render-bst (node-l t))
           (render-hspace t)
           (render-bst (node-r t))))]))

;; BST -> Image
;; produce an image of lines of the tree
;(define (render-lines t) MTTREE)

(check-expect (render-lines BST3) (overlay (beside
                                            (line (- (/ (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)) 2))
                                                  (/ (+
                                                      (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                                      (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 4)
                                                  "black")
                                            (line (/ (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)) 2)
                                                  (/ (+
                                                      (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                                      (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 4)
                                                  "black"))
                                           (rectangle
                                            (+
                                             (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                             (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)))
                                            (/ (+
                                                (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                                (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 4)
                                            "solid"
                                            "white")))


(check-expect (render-lines BST1) (overlay
                                   (beside
                                    (line (- (/ (image-width MTTREE) 2))
                                          (/ (+
                                              (image-width MTTREE)     
                                              (image-width MTTREE)) 4)
                                          "black")
                                    (line (/ (image-width MTTREE) 2)     
                                          (/ (+
                                              (image-width MTTREE)     
                                              (image-width MTTREE)) 4)
                                          "black"))
                                   (rectangle
                                    (+
                                     (image-width MTTREE)       
                                     (image-width MTTREE))
                                    (/ (+
                                        (image-width MTTREE)    
                                        (image-width MTTREE)) 4)
                                    "solid"
                                    "white")))

(check-expect (render-lines BST4) (overlay
                                   (beside
                                    (line (- (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2))
                                          (/ (+
                                              (image-width MTTREE)     
                                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                          "black")
                                    (line (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2) 
                                          (/ (+
                                              (image-width MTTREE)     
                                              (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))) 4)
                                          "black"))
                                   (rectangle
                                    (+
                                     (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))        
                                     (image-width MTTREE))
                                    (/ (+
                                        (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))     
                                        (image-width MTTREE)) 4)
                                    "solid"
                                    "white")))

(define (render-lines t)
  (overlay (beside
            (line
             (- (/ (image-width (render-main-node (node-r t))) 2))
             (/
              (+
               (image-width (render-main-node (node-l t)))
               (image-width (render-main-node (node-r t)))) 4)
             "black")
            (line
             (/ (image-width (render-main-node (node-r t))) 2)
             (/
              (+
               (image-width (render-main-node (node-l t)))
               (image-width (render-main-node (node-r t)))) 4)
             "black"))
           (rectangle
            (+
             (image-width (render-main-node (node-l t)))
             (image-width (render-main-node (node-r t))))
            (/ (+
                (image-width (render-main-node (node-l t)))
                (image-width (render-main-node (node-r t)))) 4)
            "solid"
            "white")))

;; BST -> Image
;; produce an image of main node of the tree, if false produce MTTREE
;(define (render-main-node t) MTTREE)

(check-expect (render-main-node false) MTTREE)
(check-expect (render-main-node BST1) (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))

(define (render-main-node t)
  (cond [(false? t) MTTREE]
        [else
         (text
          (string-append
           (number->string (node-key t)) KEY-VAL-SEPARATOR (node-val t))
          TEXT-SIZE
          TEXT-COLOR)]))

;; BST -> Image
;; produce an image of space between tree elements
;(define (render-hspace t) MTTREE)

(check-expect (render-hspace BST1) (rectangle (- (/ (+
                                                     (image-width MTTREE)     
                                                     (image-width MTTREE)) 2)
                                                 (/ (image-width MTTREE) 2)
                                                 (/ (image-width MTTREE) 2)) 1 "solid" "white"))

(check-expect (render-hspace BST4) (rectangle (- (/ (+
                                                     (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR))     
                                                     (image-width MTTREE)) 2)
                                                 (/ (image-width (text (string-append "7" KEY-VAL-SEPARATOR "ruf") TEXT-SIZE TEXT-COLOR)) 2)
                                                 (/ (image-width MTTREE) 2)) 1 "solid" "white"))

(check-expect (render-hspace BST3) (rectangle (- (/ (+
                                                     (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR))     
                                                     (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR))) 2)
                                                 (/ (image-width (text (string-append "1" KEY-VAL-SEPARATOR "abc") TEXT-SIZE TEXT-COLOR)) 2)
                                                 (/ (image-width (text (string-append "4" KEY-VAL-SEPARATOR "dcj") TEXT-SIZE TEXT-COLOR)) 2)) 1 "solid" "white"))

(define (render-hspace t) (rectangle (-
                                      (/ (+
                                          (image-width (render-main-node (node-l t)))
                                          (image-width (render-main-node (node-r t)))) 2)
                                      (/ (image-width (render-main-node (node-l t))) 2)
                                      (/ (image-width (render-main-node (node-r t))) 2)) 1 "solid" "white"))


(render-bst BST100)













