;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Goal: To check Simplified Garter Snake (SGS) program
;;       for potential infinite recursion.

(require rackunit)
(require "sets.rkt")
(require "extras.rkt")

(check-location "08" "q1.rkt")

(provide make-def)
(provide make-varexp)
(provide make-appexp)
(provide any-loops?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; A Variable is a Symbol.

;; A ListOfSymbol (LOSY) is one of:
;; -- empty
;;    interp: a sequence of Symbol with no elements
;; -- (cons String LOSY)
;;    interp: (cons String LOSY) represents a sequence of Symbol
;;            whose first element is a Symbol and whose
;;            other elements are represented by a LOSY.
;; INTERP: is a list of symbols.

;; TEMPLATE:
;; losy-fn : LOSY -> ??
;; HALTING MEASURE: (length losy)
#; (define (losy-fn losy)
     (cond
       [(empty? losy) ...]
       [else (... (first losy)
                  (losy-fn (rest losy)))]))

;; A ListOfVariable (LOV) is a LOSY.

;; EXAMPLES:
(define THREE-VARIABLES (list 'x 'y 'z))

(define-struct varexp (name))
;; A VarExp is a (make-varexp Variable)
;; name is the name of the variable
;; INTERP: represents a variable expression

;; TEMPLATE:
#; (define (varexp-fn vexp)
     (... (varexp-name vexp)))

;; EXAMPLES:
(define VEXP-U (make-varexp 'u))
(define VEXP-W (make-varexp 'w))
(define VEXP-X (make-varexp 'x))
(define VEXP-Y (make-varexp 'y))
(define VEXP-Z (make-varexp 'z))

(define-struct appexp (fn args))
;; An AppExp is a (make-appexp Variable ListOfExp)
;; fn is the name of the function being applied
;; args is the list of arguments to the function
;; INTERP: represents a function application expression

;; TEMPLATE:
#; (define (appexp-fn aexp)
     (... (appexp-fn aexp)
          (loe-fn (appexp-args aexp))))

;; EXAMPLES:
(define AEXP-F1-X (make-appexp 'f1 (list VEXP-X)))
(define AEXP-F1-Y (make-appexp 'f1 (list VEXP-Y)))
(define AEXP-F2   (make-appexp 'f2 (list VEXP-Z VEXP-Y)))
(define AEXP-F4   (make-appexp 'f4 (list VEXP-U VEXP-W)))
(define AEXP-F1-F2-Z (make-appexp 'f1 (list AEXP-F2 VEXP-Z)))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; TEMPLATE:
#; (define (exp-fn e)
     (cond
       [(varexp? e) (... (varexp-fn e))]
       [(appexp? e) (... (appexp-fn e))]))

;; EXAMPLES:
;; same as defined above

;; A ListOfExp (LOE) is one of:
;; -- empty
;;    interp: a sequence of Exp with no elements
;; -- (cons Exp LOE)
;;    interp: (cons Exp LOE) represents a sequence of Exp
;;            whose first element is a Exp and whose
;;            other elements are represented by a LOE.
;; INTERP: is a list of expressions.

;; TEMPLATE:
;; loe-fn : LOE -> ??
;; HALTING MEASURE: (length exps)
#; (define (loe-fn exps)
     (cond
       [(empty? exps) ...]
       [else (... (exp-fn (first exps))
                  (loe-fn (rest exps)))]))

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function
;; INTERP: represents a definition in the SGS syntax-tree representation

;; TEMPLATE:
;; def-fn : Definition -> ??
#; (define (def-fn d)
     (... (def-name d)
          (def-args d)
          (def-body d)))

;; EXAMPLES:
;; a definition with no arguments
(define DEF-NO-ARGS (make-def 'a-not-so-long-name '() (make-varexp 'x)))

;; a definition with a very long definition name
(define DEF-LONG-NAME (make-def 'a-very-long-function-name (list 'x) AEXP-F1-X))

;; a definition with multiple arguments
(define DEF-MULTIPLE-ARGS (make-def 'multiple-args (list 'x 'y 'z 'a) AEXP-F2))

;; a definition with many arguments that requires to be stacked
(define DEF-STACK-ARGS (make-def 'stack-args (list 'u 'x 'y 'z 'a) AEXP-F2))

;; A ListOfDefinition (LOD) is one of:
;; -- empty
;;    interp: a sequence of Definition with no elements
;; -- (cons Definition LOD)
;;    interp: (cons Definition LOD) represents a sequence of Definition
;;            whose first element is a Definition and whose
;;            other elements are represented by a LOD.
;; INTERP: is a list of definitions.

;; TEMPLATE:
;; lod-fn : LOD -> ??
;; HALTING MEASURE: (length defs)
#; (define (lod-fn defs)
     (cond
       [(empty? defs) ...]
       [else (... (def-fn (first defs))
                  (lod-fn (rest defs)))]))

;; A Program is a ListOfDefinition.

;; EXAMPLES:
;; a sample program as demonstrated on the course page
(define SAMPLE-PROGRAM-WITH-LOOP
  (list
   (make-def 'f1 (list 'x)
             (make-appexp 'no-loop (list VEXP-X)))
   (make-def 'f2 (list 'u 'y)
             (make-appexp 'f1 (list VEXP-Y)))
   (make-def 'f3 (list 'x 'u)
             (make-appexp 'f1 (list AEXP-F4 VEXP-Z)))
   (make-def 'f4 (list 'x 'y)
             (make-appexp 'f5 (list VEXP-Y VEXP-U)))
   (make-def 'f5 (list 'u)
             (make-appexp 'f2  (list (make-appexp 'f3 empty))))
   (make-def 'no-loop (list 'x) VEXP-X)))

;; a sample program with no loops
(define SAMPLE-PROGRAM-WITH-NO-LOOP
  (list
   (make-def 'f1 (list 'x)
             (make-appexp 'f2
                          (list (make-appexp 'f3 (list AEXP-F4 AEXP-F4 VEXP-W))
                                (make-appexp 'f4 (list VEXP-U VEXP-W))
                                VEXP-X)))))

;; A Node is a Symbol.
;; INTERP: represents a vertex of a Graph.
(define NODE-F1 'f1)
(define NODE-F2 'f2)
(define NODE-F3 'f3)
(define NODE-F4 'f4)

;; A ListOfNodes (LON) is a LOSY.
;; INTERP: representing a list of Nodes of a Graph.

(define-struct edge (from to))
;; An Edge is a (make-edge Node Node)
;; from is the starting vertex of an Edge of a Graph.
;; to is the ending vertex of an Edge of a Graph.
;; INTERP: is an edge of a Graph.

;; TEMPLATE:
;; edge-fn : Edge -> ??
(define (edge-fn ed)
  (... (edge-from ed)
       (edge-to ed)))

;; EXAMPLES:
(define EDGE-F1-F2 (make-edge NODE-F1 NODE-F2))
(define EDGE-F1-F3 (make-edge NODE-F1 NODE-F3))
(define EDGE-F2-F3 (make-edge NODE-F2 NODE-F3))
(define EDGE-F3-F4 (make-edge NODE-F3 NODE-F4))
(define EDGE-F4-F1 (make-edge NODE-F4 NODE-F1))
(define EDGE-F2-F2 (make-edge NODE-F2 NODE-F2))

;; A ListOfEdges (LOED) is one of:
;; -- empty
;;    interp: a sequence of Edge with no elements
;; -- (cons Edge LOED)
;;    interp: (cons Edge LOED) represents a sequence of Edge
;;            whose first element is a Edge and whose
;;            other elements are represented by a LOED.
;; INTERP: is a list of edges.

;; TEMPLATE:
;; loed-fn : LOED -> ??
;; HALTING MEASURE: (length edges)
#; (define (loed-fn edges)
     (cond
       [(empty? edges) ...]
       [else (... (edge-fn (first edges))
                  (loed-fn (rest edges)))]))

;; A ListOfLOED (LOLOED) is one of:
;; -- empty
;;    interp: a sequence of LOED with no elements
;; -- (cons LOED LOLOED)
;;    interp: (cons LOED LOLOED) represents a sequence of LOED
;;            whose first element is a LOED and whose
;;            other elements are represented by a LOLOED.
;; INTERP: is a list of LOED.

;; TEMPLATE:
;; loloed-fn : LOLOED -> ??
;; HALTING MEASURE: (length loeds)
#; (define (loloed-fn loeds)
     (cond
       [(empty? loeds) ...]
       [else (... (loed-fn (first loeds))
                  (loloed-fn (rest loeds)))]))

;; A Graph is a ListOfEdges.

;; EXAMPLES:
;; a graph with a self-loop
(define GRAPH-WITH-SELF-LOOP (list EDGE-F1-F2
                                   EDGE-F2-F2
                                   EDGE-F3-F4
                                   EDGE-F4-F1))

;; a graph with an indirect loop
(define GRAPH-WITH-INDIRECT-LOOP (list EDGE-F1-F2
                                       EDGE-F2-F3
                                       EDGE-F3-F4
                                       EDGE-F4-F1))

;; a graph that represents the SAMPLE-PROGRAM-WITH-LOOP
(define GRAPH-FOR-SAMPLE-PROGRAM-WITH-LOOP (list
                                            (make-edge 'f1 'no-loop)
                                            (make-edge 'f2 'f1)
                                            (make-edge 'f3 'f1)
                                            (make-edge 'f1 'f4)
                                            (make-edge 'f4 'f5)
                                            (make-edge 'f5 'f2)
                                            (make-edge 'f2 'f3)))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-loops? : Program -> Boolean
;; GIVEN: a valid SGS program p (that is, a GS program that obeys the
;; restrictions listed in the question).
;; RETURNS: true iff there is some function f in p that calls itself
;; either directly or indirectly, as in the example above.
;; EXAMPLES:
;; (any-loops? SAMPLE-PROGRAM-WITH-LOOP) = #true
;; (any-loops? SAMPLE-PROGRAM-WITH-NO-LOOP) = #false
;; DESIGN STRATEGY: Use HOF ormap on UNIQ-NODES
(define (any-loops? p)
  (local
    (;; construct a graph from the given program representing function calls
     (define GRAPH (construct-graph p))
     
     ;; obtain all unique edges of the graph
     (define UNIQ-EDGES (unique-edges GRAPH))
     
     ;; obtain all the nodes of the graph from the unique edges of the graph
     (define NODES (map
                    ;; Edge -> Node
                    ;; GIVEN: an Edge e
                    ;; RETURNS: a Node representing the starting vertex of e
                    ;; DESIGN STRATEGY: Use template for Edge on e
                    (lambda (e) (edge-from e)) UNIQ-EDGES))
     
     ;; obtain all unique nodes of the graph from all nodes of the graph
     (define UNIQ-NODES (unique-nodes NODES)))
    
    (ormap
     ;; Node -> Boolean
     ;; GIVEN: a Node n
     ;; RETURNS: true iff a depth-first traversal from n leads to a loop
     ;; DESIGN STRATEGY: Combine simpler functions
     (lambda (n) (dfs-leads-to-loop? n UNIQ-EDGES)) UNIQ-NODES)))
;; TESTS
(begin-for-test
  (check-equal? (any-loops? SAMPLE-PROGRAM-WITH-LOOP) #true
                "check for loops in a sample program with loop")
  (check-equal? (any-loops? SAMPLE-PROGRAM-WITH-NO-LOOP) #false
                "check for loops in a sample program without a loop"))

;; construct-graph : Program -> Graph
;; GIVEN: a Program p
;; RETURNS: a Graph g representing the function calls of the given program p
;; EXAMPLES:
;; (construct-graph SAMPLE-PROGRAM-WITH-LOOP)
;; = GRAPH-FOR-SAMPLE-PROGRAM-WITH-LOOP
;; DESIGN STRATEGY: Combine simpler functions
(define (construct-graph p)
  (local
    (;; obtain-edges : Program -> LOLOED
     ;; GIVEN: a Program p
     ;; RETURNS: a LOLOED representing the function calls of program p
     ;; DESIGN STRATEGY: Use HOF map on p
     (define (obtain-edges p)
       (map
        ;; Definition -> LOLOED
        ;; GIVEN: a Definition d
        ;; RETURNS: a LOLOED representing the function calls for d
        ;; DESIGN STRATEGY: Use template for Definition on d
        (lambda (d) (add-edges (def-name d) (def-body d))) p)))
    
    (flatten (obtain-edges p))))
;; TESTS
(begin-for-test
  (check-equal? (construct-graph SAMPLE-PROGRAM-WITH-LOOP)
                GRAPH-FOR-SAMPLE-PROGRAM-WITH-LOOP
                "constructs a graph representing the sample program with loop"))

;; add-edges : Symbol Exp -> LOLOED
;; GIVEN: a Symbol and Exp representing the name of a definition
;; and the body of a definition respectively
;; RETURNS: a LOLOED representing the function calls of the given definition
;; EXAMPLES:
;; (add-edges 'f1 VEXP-X) = '()
;; (flatten (add-edges 'f1 AEXP-F1-F2-Z))
;; = (list (make-edge 'f1 'f1) (make-edge 'f1 'f2))
;; DESIGN STRATEGY: Use template for Exp on e
(define (add-edges s e)
  (local
    (;; add-edges/appexp : AppExp -> LOLOED
     ;; GIVEN: a AppExp
     ;; RETURNS: a LOLOED representing the funcion calls for the given aexp
     ;; DESIGN STRATEGY: Use template for AppExp on aexp
     (define (add-edges/appexp aexp)
       (append (list (make-edge s (appexp-fn aexp)))
               (add-edges-from-args (appexp-fn aexp)
                                    (appexp-args aexp)))))
    
    (cond
      [(varexp? e) '()]
      [(appexp? e) (add-edges/appexp e)])))
;; TESTS
(begin-for-test
  (check-equal? (add-edges 'f1 VEXP-X) '()
                "no edges added for the given varexp")
  (check-equal? (flatten (add-edges 'f1 AEXP-F1-F2-Z))
                (list (make-edge 'f1 'f1) (make-edge 'f1 'f2))
                "edges added for the given appexp"))

;; add-edges-from-args : Node LON -> LOLOED
;; GIVEN: a Node and a LON representing the starting vertex of an edge
;; and a list of ending vertices of the edge
;; RETURNS: a LOLOED representing edges from the given Node to each of
;; the Node given in the LON
;; EXAMPLES:
;; (flatten (add-edges-from-args 'f3 (list (make-appexp 'f4 '())
;;                                         (make-appexp 'f4 '())
;;                                         (make-varexp 'w))))
;; = (list (make-edge 'f3 'f4) (make-edge 'f3 'f4))
;; DESIGN STRATEGY: Use HOF map on to-args
(define (add-edges-from-args from to-args)
  (map
   ;; Node -> Edge
   ;; GIVEN: a Node toarg
   ;; RETURNS: an Edge with from as the starting vertex and
   ;; toarg as the ending vertex of the edge
   ;; DESIGN STRATEGY: Combine simpler functions
   (lambda (toarg) (add-edges from toarg)) to-args))
;; TESTS
(begin-for-test
  (check-equal? (flatten (add-edges-from-args
                          'f3 (list (make-appexp 'f4 '())
                                    (make-appexp 'f4 '())
                                    (make-varexp 'w))))
                (list (make-edge 'f3 'f4) (make-edge 'f3 'f4))
                "edges added from the given from node to each of the to-args"))

;; unique-nodes : LON -> LON
;; GIVEN: a LON
;; RETURNS: a LON like the original but with duplicates nodes removed
;; EXAMPLES:
;; (unique-nodes (list NODE-F2 NODE-F1 NODE-F4 NODE-F1))
;; = (list NODE-F2 NODE-F1 NODE-F4)
;; DESIGN STRATEGY: Call a more general function
(define (unique-nodes nodes)
  (unique nodes nodes-differ?))
;; TESTS
(begin-for-test
  (check-equal? (unique-nodes (list NODE-F2 NODE-F1 NODE-F4 NODE-F1))
                (list NODE-F2 NODE-F1 NODE-F4)
                "unique nodes, one duplicate node removed"))

;; nodes-differ? : Node Node -> Boolean
;; GIVEN: two Nodes
;; RETURNS: true iff the two given nodes are not the same
;; EXAMPLES:
;; (nodes-differ? NODE-F1 NODE-F4) = #true
;; DESIGN STRATEGY: Combine simpler functions
(define (nodes-differ? n1 n2)
  (not (symbol=? n1 n2)))
;; TESTS
(begin-for-test
  (check-equal? (nodes-differ? NODE-F1 NODE-F4) #true
                "the given nodes differ"))

;; unique-edges : LOED -> LOED
;; GIVEN: a LOED
;; RETURNS: a LOED like the original but with duplicates edges removed
;; EXAMPLES:
;; (unique-edges (list EDGE-F2-F3 EDGE-F2-F3 EDGE-F1-F2))
;; = (list EDGE-F2-F3 EDGE-F1-F2)
;; DESIGN STRATEGY: Call a more general function
(define (unique-edges edges)
  (unique edges edges-differ?))
;; TESTS
(begin-for-test
  (check-equal? (unique-edges (list EDGE-F2-F3 EDGE-F2-F3 EDGE-F1-F2))
                (list EDGE-F2-F3 EDGE-F1-F2)
                "unique edges, one duplicate edge removed"))

;; edges-differ? : Edge Edge -> Boolean
;; GIVEN: two Edges
;; RETURNS: true iff the two given edges are not the same
;; EXAMPLES:
;; (edges-differ? EDGE-F2-F3 EDGE-F2-F3) = #false
;; DESIGN STRATEGY: Use template for Edge on e1 and e2
(define (edges-differ? e1 e2)
  (not (and (symbol=? (edge-from e1) (edge-from e2))
            (symbol=? (edge-to e1) (edge-to e2)))))
;; TESTS
(begin-for-test
  (check-equal? (edges-differ? EDGE-F2-F3 EDGE-F2-F3) #false
                "the given edges don't differ"))

;; unique : list? (X X -> Boolean) -> list?
;; GIVEN: a list? and a function that takes two elements
;; of the same type as that of the given lst and returns a boolean
;; WHERE: X is of the same type as that of the given lst
;; RETURNS: a list? like the original after applying fdiffer?
;; representing the unique elements of the given list?
;; EXAMPLES:
;; (unique (list EDGE-F2-F3 EDGE-F2-F3 EDGE-F1-F2) edges-differ?)
;; = (list EDGE-F2-F3 EDGE-F1-F2)
;; DESIGN STRATEGY: Use HOF foldr on lst
(define (unique lst fdiffer?)
  (local 
    (;; lst-except-elem : list? Any -> list?
     ;; GIVEN: a list? and an element of the given list
     ;; WHERE: lstelem is a list of the type of the given elem
     ;; RETURNS: a list? with the given elem removed
     ;; DESIGN STRATEGY: Use HOF filter on lstelem
     (define (lst-except-elem lstelem elem)
       (filter
        ;; Any -> Boolean
        ;; GIVEN: an Any representing an element
        ;; RETURNS: true iff fdiffer? evaluates to true
        ;; DESIGN STRATEGY: Combine simpler functions
        (lambda (l) (fdiffer? l elem)) lstelem))
     
     ;; unique-elem : Any list? -> list?
     ;; GIVEN: an element and a list? of the given element
     ;; WHERE: lstelem is a list of the type of the given elem
     ;; RETURNS: a list? like the original but with duplicate elem removed
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (unique-elem elem lstelem)
       (cons elem (lst-except-elem lstelem elem))))
    
    (foldr (lambda (elem lstelem) (unique-elem elem lstelem)) '() lst)))
;; TESTS
(begin-for-test
  (check-equal? (unique (list EDGE-F2-F3 EDGE-F2-F3 EDGE-F1-F2) edges-differ?)
                (list EDGE-F2-F3 EDGE-F1-F2)
                "unique edges, one duplicate edge removed, edges-differ? used"))

;; dfs-leads-to-loop? : Node Graph -> Boolean
;; GIVEN: a Node n and a Graph g
;; WHERE: duplicate edges from the Graph g have been removed
;; RETURNS: true iff a depth-first traversal starting from the given Node
;; on the given graph G leads to a loop (or inifinite recursion)
;; EXAMPLES:
;; (dfs-leads-to-loop?
;;   'f10 (unique-edges (construct-graph SAMPLE-PROGRAM-WITH-LOOP))) = #false
;; DESIGN STRATEGY: Combine simpler functions
(define (dfs-leads-to-loop? n g)
  (dfs-cycle? (list n) g (list n)))
;; TESTS
(begin-for-test
  (check-equal? (dfs-leads-to-loop?
                 'f10 (unique-edges (construct-graph SAMPLE-PROGRAM-WITH-LOOP)))
                #false "dfs doesn't lead to loop, n doesn't exist in g"))

;; dfs-cycle? : LON Graph LON -> Boolean
;; GIVEN: a LON, lon, representing a list of nodes to traverse on given graph g,
;; and another LON, path, that representes the list of traversed nodes
;; WHERE: duplicate edges from g have been removed and path always increases
;; RETURNS: true iff there exists a cycle/loop starting at nodes in given lon
;; EXAMPLES:
;; (dfs-cycle? (list 'f2)
;;             (unique-edges (construct-graph SAMPLE-PROGRAM-WITH-NO-LOOP))
;;             (list 'f2))
;; = #false
;; HALTING MEASURE: (length nbrs)
;; DESIGN STRATEGY: Use template for LON on nbrs
(define (dfs-cycle? lon g path)
  (local
    (;; a node or a boolean representing path has duplicate nodes
     ;; depending on whether or not the given lon is empty
     (define node/#b (if (empty? lon) (dup? path) (first lon)))
     
     ;; neighbors of the node/#b iff node/#b is a symbol, otherwise empty
     (define nbrs (if (symbol? node/#b) (get-neighbors node/#b g) '()))
     
     ;; visited?/#f : LON LON -> Boolean
     ;; GIVEN: two LON, one representing the neighbors of node/#b
     ;; and other representing the list of nodes that can be reached
     ;; from the (first lon) node
     ;; WHERE: (empty? nbrs) = #false
     ;; RETURNS: true iff there exists a cycle while traversing the
     ;; neighbors of (first lon) node
     ;; DESIGN STRATEGY: Use template for LON on nbrs 
     (define (visited?/#f nbrs path)
       (or (dfs-cycle? (list (first nbrs)) g (append nbrs path))
           (dfs-cycle? (rest nbrs) g (append nbrs path))))
     
     ;; visited? : LON LON -> Boolean
     ;; GIVEN: two LON, one representing the neighbors of node/#b
     ;; and other representing the list of nodes that can be reached
     ;; from the (first lon) node
     ;; WHERE: (empty? nbrs) = #false
     ;; RETURNS: true iff the (first lon) node has been visited in path
     ;; DESIGN STRATEGY: Cases on whether or not the path contains visited nodes
     (define (visited? nbrs path)
       (if (dup? path) #true (visited?/#f nbrs path))))
    
    (cond
      [(empty? nbrs) #false]
      [else (visited? nbrs path)])))
;; TESTS
(begin-for-test
  (check-equal? (dfs-cycle?
                 (list 'f1)
                 (unique-edges (construct-graph SAMPLE-PROGRAM-WITH-LOOP))
                 (list 'f1))
                #true
                "there is a cycle starting at node 'f1 on program w/. loop")
  (check-equal? (dfs-cycle?
                 (list 'f2)
                 (unique-edges (construct-graph SAMPLE-PROGRAM-WITH-NO-LOOP))
                 (list 'f2))
                #false
                "there is no cycle starting at node 'f2 on program w/o. loop"))

;; get-neighbors : Node Graph -> LON
;; GIVEN: a Node n, and a Graph g
;; RETURNS: a LON representing all immediate neighbors of the given node n in g
;; EXAMPLES:
;; (get-neighbors NODE-F2
;;                (unique-edges (construct-graph SAMPLE-PROGRAM-WITH-NO-LOOP)))
;; = (list NODE-F3 NODE-F4)
;; DESIGN STRATEGY: Use HOF map on (out-degree-vertices n g)
(define (get-neighbors n g)
  (local
    (
     ;; out-degree-vertices : Node Graph -> LOED
     ;; GIVEN: a Node n, and a Graph g
     ;; RETURNS: a LOED representing the outgoing edges
     ;; from the given node n
     ;; DESIGN STRATEGY: Use HOF filter on g
     (define (out-degree-vertices n g)
       (filter
        ;; Edge -> Boolean
        ;; GIVEN: an Edge e of Graph g
        ;; RETURNS: true iff there exists an outgoing edge from node n
        ;; DESIGN STRATEGY: Use template for Edge on e
        (lambda (e) (symbol=? (edge-from e) n)) g)))
    
    (map
     ;; Edge -> Node
     ;; GIVEN: an outgoing Edge e from given Node n in Graph g
     ;; RETURNS: a Node representing the ending vertex of the outgoing edge e
     ;; DESIGN STRATEGY: Use template for Edge on e
     (lambda (e) (edge-to e)) (out-degree-vertices n g))))
;; TESTS
(begin-for-test
  (check-equal? (get-neighbors
                 NODE-F2
                 (unique-edges (construct-graph SAMPLE-PROGRAM-WITH-NO-LOOP)))
                (list NODE-F3 NODE-F4)
                "neighbors of a node with two immediate neighbors"))

;; dup? : LON -> Boolean
;; GIVEN: a LON representing the visited nodes
;; during a traversal on a Node in a Graph
;; RETURNS: true iff there exists nodes in vnodes
;; that have been visited earlier
;; EXAMPLES:
;; (dup? '()) = #false
;; (dup? '(f2 f1 f3 f2)) = #true
;; DESIGN STRATEGY: Use HOF ormap on vnodes
(define (dup? vnodes)
  (ormap
   ;; Node -> Boolean
   ;; GIVEN: a Node
   ;; RETURNS: true iff the given Node n exists more than once in vnodes
   ;; DESIGN STRATEGY: Combine simpler functions
   (lambda (n) (member? n (remove n vnodes))) vnodes))
;; TESTS
(begin-for-test
  (check-equal? (dup? '()) #false "no visited nodes in empty vnodes")
  (check-equal? (dup? '(f2 f1 f3 f2)) #true "f2 visited earlier"))

;; flatten : Any -> list?
;; GIVEN: Any
;; RETURNS: true iff lst is a list, either the empty list,
;; or a pair whose second element is a list.
;; EXAMPLES:
;; (flatten (list "pdp" 5 4 (list "rocks"))) = (list "pdp 5 4 "rocks")
;; HALTING MEASURE: (length lst)
;; DESIGN STRATEGY: Use template for list? on lst
(define (flatten lst)
  (cond
    [(empty? lst) lst]
    [(not (list? lst)) (list lst)]
    [else (append (flatten (first lst))
                  (flatten (rest lst)))]))
;; TESTS
(begin-for-test
  (check-equal? (flatten (list "pdp" 5 4 (list "rocks")))
                (list "pdp" 5 4 "rocks")
                "flattens a test nested list"))
