OLD_SRCFILES = graph.rkt make-graph.rkt numbo1.rkt 
UNTYPED_SRCFILES = wheel.rkt id-set.rkt graph1.rkt fargish1.rkt model1.rkt \
	shorthand.rkt xsusp3.rkt make-slipnet.rkt equation.rkt \
	graph.rkt
UT_FILES = test-fargish.rkt test-model.rkt
SRCFILES = typed-wheel.rkt types.rkt id-set.rkt graph.rkt fargish.rkt \
	model.rkt fizzle.rkt $(UT_FILES)

ut:
	raco make -j 2 --vv $(UT_FILES)
	raco test -j 2 $(UT_FILES)

tags:
	ctags --language-force=Scheme *.rkt

.PHONY: ut tags
