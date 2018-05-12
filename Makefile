see: test.pdf
	open test.pdf

%.pdf: %.dot
	dot -Tpdf < $< > $@

%.eps: %.dot
	dot -Teps < $< > $@

tags:
	ctags -R src/ checkouts/

.PHONY: tags

