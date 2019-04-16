OLD_SRCFILES = graph.rkt make-graph.rkt numbo1.rkt 
UNTYPED_SRCFILES = wheel.rkt id-set.rkt graph1.rkt fargish1.rkt model1.rkt \
	shorthand.rkt xsusp3.rkt make-slipnet.rkt equation.rkt \
	graph.rkt
UT_FILES = test-fargish.rkt test-model.rkt
SRCFILES = typed-wheel.rkt types.rkt id-set.rkt graph.rkt fargish.rkt \
	model.rkt fizzle.rkt support-core.rkt trace.rkt x32.rkt mkhtml.rkt \
	web.rkt $(UT_FILES)

all:
	raco make -j 2 -v $(SRCFILES)

ut:
	raco make -j 2 --vv $(UT_FILES)
	raco test -j 2 $(UT_FILES)

fargish.html: mkhtml.rkt
	racket mkhtml.rkt

wc:
	wc -l $(SRCFILES)

tags:
	ctags --language-force=Scheme *.rkt

clean:
	rm -f fargish.html

.PHONY: ut tags all clean
