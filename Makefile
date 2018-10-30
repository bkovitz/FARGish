OLD_SRCFILES = graph.rkt make-graph.rkt 
SRCFILES = id-set.rkt numbo0.rkt xsusp3.rkt \
					 fargish.rkt graph1.rkt make-graph1.rkt numbo1.rkt #model1.rkt 

ut:
	raco test $(SRCFILES)

tags:
	ctags --language-force=Scheme *.rkt

.PHONY: ut tags
