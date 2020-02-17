PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testNodeSpec.TestLinkSpec
	#$(PY) -m unittest -v testPortGraph

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
