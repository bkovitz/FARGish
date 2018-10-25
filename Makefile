SRCFILES = id-set.rkt graph.rkt make-graph.rkt numbo0.rkt xsusp3.rkt \
					 fargish.rkt #model1.rkt 

ut:
	raco test $(SRCFILES)

tags:
	ctags --language-force=Scheme *.rkt

.PHONY: ut tags
