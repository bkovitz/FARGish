PY = python3
ut:
	$(PY) -m unittest

tags:
	ctags *.py

.PHONY: ut tags
