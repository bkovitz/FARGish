#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         describe "graph.rkt" "make-graph.rkt")

(define (bind-all-letters g)
  (for/fold ([g g])
            ([letter-pair '((a b) (b a) (a c) (c a) (b c) (c b))])
    (match-define `(,from ,to) letter-pair)
    (add-tag g 'bind from to)))
      ;TODO Is this still the right way to call add-tag?

#;(define model
  (let* ([g (make-graph 'a 'b 'c '(tag next a b) '(tag next b c))]
         [g (bind-all-letters g)])
    g))

(define model
  (let* ([g (make-graph '(:group workspace
                                 a (placeholder letter x) c
                                 (bind a x) (bind x c))
                        '(:group slipnet-structure
                                 a b c (succ a b) (succ b c)))])
    g))

#;(pr-graph model)

;TODO UT
(define (try-to-bind-all g from-group to-group)
  (do-graph-edits g
    (for/list ([from-node (members-of g from-group)])
      (define class (class-of g from-node))
      (define to-nodes (nodes-of-class-in g class to-group))
      (if (null? to-nodes) ;if nothing to bind to, then build
        (let ([new (gensym class)])
          `(:make
             (:define ,new (:node ,class)) ;BUG? :node redefines succ
             (:edge (,to-group members) (,new member-of))
             (bind ,from-node ,new)))
        `(:make  ;bind to all of same class
           ,@(for/list ([to-node to-nodes])
               `(bind ,from-node ,to-node)))))))
;
;
;
;
;
;  (for/fold ([g g])
;            ([from-node (members-of g from-group)])
;    (define class (class-of g from-node))
;    (define to-nodes (nodes-of-class-in g class to-group))
;    (if (empty? to-nodes)
;        (let*-values ([(g newid) (make-node g `((class . ,class)))]
;                      [(g) (add-edge g `((,to-group members)
;                                         (,newid member-of)))])
;          (bind g from-node newid))
;        (for/fold ([g g])
;                  ([to-node to-nodes])
;          (bind g from-node to-node)))))

(newline)

;(pr-graph (try-to-bind-all model 'slipnet-structure 'workspace))

(module+ test
  (test-case "try-to-bind-all"
    (let* ([g (make-graph '(:group workspace
                                   a (placeholder letter x) c
                                   (bind a x) (bind x c))
                          '(:group slipnet-structure
                                   a b c (succ a b) (succ b c)))]
           [g (try-to-bind-all g 'slipnet-structure 'workspace)])
      (check-equal? (list->set (member-of g 'succ3)) (set 'workspace))
      (check-equal? (list->set (member-of g 'succ4)) (set 'workspace))
      (check-true (bound-to? g 'a2 'a))
      (check-true (bound-to? g 'a2 'x))
      (check-true (bound-to? g 'a2 'c))
      (check-true (bound-to? g 'b 'a))
      (check-true (bound-to? g 'b 'x))
      (check-true (bound-to? g 'b 'c))
      (check-true (bound-to? g 'c2 'a))
      (check-true (bound-to? g 'c2 'x))
      (check-true (bound-to? g 'c2 'c))
      #;(check-true (bound-to? g #R(tag-of 'succ g 'a2 'b)
                               #R(tag-of 'succ g 'a 'x)))
      (check-true (bound-to? g 'succ2 'succ3))
      (check-true (bound-to? g 'succ 'succ4))
    )))


(define g (make-graph '(:group workspace a b)
                      '(:group slipnet-structure a b (succ a b))))

;; Defining tag properties

(define succ '(tag (succ a b)
                (edges ((a succ-to) (succ succ-from))
                       ((succ succ-to) (b succ-from)))))

(define (mktag tagdef g node1 node2)
  (match tagdef
    [`(tag (,t ,a ,b)
        (edges ((,n1 ,pl1) (,n2 ,pl2)) ...))
      `((:define ,t (:node ,t))
        (:define ,a (:find-node ,node1))
        (:define ,b (:find-node ,node2))
        ,@(for/list ([nn1 n1] [ppl1 pl1] [nn2 n2] [ppl2 pl2])
            `(:edge (,nn1 ,ppl1) (,nn2 ,ppl2))))
      ]))

(define h (make-graph 'x 'y))
(define h* (do-graph-edits h (mktag succ h 'x 'y)))
#;(pr-graph h*)

;NEXT
; d-name->id -> d-alias->id
; :define -> :let
; do-graph-edits -> do-graph-deltas
; match required edges
; simple binding scout


; scout->builder *is* a generator. How can we copy scout->builders from
; another group and "run" them? Copying from another group seems to require
; a generator. There should be a way to lay down a few steps or stages and
; search for a way to connect them. Say, isn't the fully connected result
; a plan?

; It's OK (best, actually) if the ws abc is continually falling apart and
; getting regenerated.

; Should the slipnet store primarily generators? "Given part of this
; memory, here's how to regenerate the rest of it." From an 'a, we can
; generate a 'b (linked by 'succ). This might be the temporal asymmetry
; we've been looking for.

; In a way, support edges are builders, hence generators. Support has
; temporal asymmetry, hence the oscillations.

; Maybe a "trace reader" walks through a temporal trace and copies scouts
; and builders into the ws. The trace reader can't see very far ahead.


; INTEREST in searching for something: a scout. When we lose interest,
; the scout should fizzle.
;
; What's the difference between searching and building? Building explores
; a HYPOTHESIS and REMEMBERS observations of interest so that later
; processes can exploit them or build on them. So, do we need builders
; in addition to the nodes they build? Only if a builder has to build
; multiple nodes and possibly spawn scouts to help. You only need a
; builder if the building process could fail or be interrupted. Otherwise
; a support edge is builder enough. A 'failed tag on either a builder or
; a built node should cause the node to send antipathy to the source of
; builder or node. Maybe we need builders simply to track who wanted which
; node built. Or maybe a 'failed tag simply directs antipathy backward
; along the support edges to the failed node.
;
; Do builders have desiderata? Desiderata trigger follow-ups: scouts that
; search for nodes that could fulfill them. Scouts trigger follow-ups
; when they fail and when they need to build something too large or
; unknown to build in one timestep. Otherwise they just build it.
;
; So, we need active nodes only to manage processes that take *more* than one
; timestep. Active nodes create temporal continuity in the behavior of the
; model. Support for an active node means interest in continuing that process.
; An active node is an "agent": a node that pursues some interest until it
; succeeds, fails, or loses funding. Agents act on behalf of something else.
; So, when an agent builds something, it links support from its client to
; whatever it built. An agent carries a desideratum from its client. An agent
; has state: it knows where it's looked and what it's tried.
;
; (desideratum (every-node-in archetype) (bound-to? (some-node-in ws-group)))
;
; (desideratum 'a2 (bound-to? 'a))
;
; (desideratum 'a2 (has-same-tags-as 'a))

; Should we get rid of classes and just have slipnet links?  'a -is-a- 'letter

; There needs to be an agent that "copies", like a ribosome. It reads from an
; archetype and copies to a ws-group, mutatis mutandis, and then it slides its
; cursors analogously in both groups. Tags on a ribosome. node affect how it
; copies--and can be involved in competition. There can be several ribosomes
; walking the same pair of structures, supporting and/or undoing each other's
; work, or doing follow-up work.
;
; A ribosome "tags" its current operands: the from-node and to-node. It
; remembers where it's been (more tags?) and tries to move in parallel in both
; the source group and the target group. A ribosome might build a 'bind node
; between two existing nodes, or build a missing node and 'bind it, or skip a
; source node if it can't find a target to mate it with, or stop and wait.
; A ribosome might spawn a scout or another ribosome.
;
; An active node with an unfulfilled desideratum like "everything there must
; bind to everything here" spawns a ribosome. So, the arrow in Copycat
; spawns ribosomes. A 'bind node can spawn a ribosome to build matching
; neighborhoods, or to simply check for consistency.
;
; A ribosome can do some mapping based on where it started: it maintains
; a tag-link to the initial tags in both source and target groups, so it
; can convert 'succ to 'pred. Thus a ribosome enforces analogical consistency.
; Some other agent might build the initial tags and then set off a ribosome
; to "do it like that" throughout the rest of the group. You might give a
; ribosome a direction in which to move--an orientation.
;
; So, ribosomes are "rigged up" generators--exactly what we need.
;
; Temporal traces: where the ribosome started, where it ended, any adventures
; along the way. Adventures (noteworthy moments) violate expectations. When
; you start a ribosome (copying from an archetype??), you have expectations,
; like a time-frame for completion.

; In Numbo, a ribosome might adjust an arithmetic fact that doesn't exactly
; match the ws situation. When it crosses the +, it knows to make "the same"
; adjustment. Ribosomes might automatically preserve topology, so we don't
; need nodes to inhibit mates with bad topology. Bad topology
; (non-isomorphism) should be rare with ribosomes; when it does happen, an
; inspector can flag it and spawn an agent to correct it. An inspector is a
; sort of ribosome: it walks along structures looking for "syntax errors".


; MUCH LATER
; spreading-search
;   Like spreading activation, but it's a scout that keeps its own map
;   of all the wavefronts.
; coarse-match
;   Match a group against a neighborhood.
