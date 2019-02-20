OLD_SRCFILES = graph.rkt make-graph.rkt numbo1.rkt 
SRCFILES = wheel.rkt id-set.rkt graph1.rkt fargish1.rkt model1.rkt \
	shorthand.rkt xsusp3.rkt make-slipnet.rkt equation.rkt \
	graph.rkt

ut:
	raco test $(SRCFILES)

tags:
	ctags --language-force=Scheme *.rkt

.PHONY: ut tags
