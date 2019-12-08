PY = python3.7

ut:
	$(PY) -m unittest

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
