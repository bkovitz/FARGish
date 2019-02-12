; graph.rkt -- Data structure for port graphs

#lang debug at-exp errortrace typed/racket

(require "types.rkt")

(struct graph* ([ht-node->attrs : (Hashof Node Any)]
                [ht-port->neighboring-ports : (Hashof Port (Setof Port))]
                [edges : (Hashof Edge/Set EdgeWeight)]
                [id-set : IdSet]
                [stacks : ??]
                [vars : (Hashof Symbol Any)]))
