SCALE= #-s72

see: test.pdf test100.pdf test11.pdf
	open test.pdf test100.pdf test11.pdf

%.pdf: %.dot Makefile
	dot $(SCALE) -Tpdf < $< > $@

%.eps: %.dot
	dot $(SCALE) -Teps < $< > $@

tags:
	ctags -R src/ checkouts/

.PHONY: tags

