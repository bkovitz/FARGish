SRCFILES = id-set.rkt graph.rkt

ut:
	raco test $(SRCFILES)

.PHONY: ut
