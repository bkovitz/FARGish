OLD_SRCFILES = graph.rkt make-graph.rkt 
SRCFILES = id-set.rkt graph1.rkt fargish1.rkt model1.rkt shorthand.rkt \
	xsusp3.rkt numbo1.rkt

ut:
	raco test $(SRCFILES)

tags:
	ctags --language-force=Scheme *.rkt

.PHONY: ut tags
