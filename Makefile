PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testBrute
	#$(PY) -m unittest -v testNodeSpec.TestNodeSpec.test_nodespec

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
