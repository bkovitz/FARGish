PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testIndent1.TestIndent1

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
