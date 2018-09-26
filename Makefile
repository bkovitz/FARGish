SRCFILES = id-set.rkt graph.rkt numbo0.rkt #model1.rkt 

ut:
	raco test $(SRCFILES)

tags:
	ctags --language-force=Scheme *.rkt

.PHONY: ut tags
