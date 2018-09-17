SRCFILES = id-set.rkt graph.rkt

ut:
	raco test $(SRCFILES)

tags:
	ctags --language-force=Scheme *.rkt

.PHONY: ut tags
