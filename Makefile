PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	#$(PY) -m unittest -v testNodeParams.TestNodeParams.testAlreadyBuilt
	$(PY) -m unittest -v testCodegen.TestCodegen.testNodeWithArg

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
