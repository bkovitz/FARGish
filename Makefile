PY = python3
# On Ben's laptop, do  PY=python3.4 make -e

ut:
	$(PY) -m unittest

tags:
	ctags *.py force-directed*.js

.PHONY: ut tags
