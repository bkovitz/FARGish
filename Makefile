PY = python3.7

ut:
	$(PY) -m unittest -v

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
