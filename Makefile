PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testCodegen.TestCodegen.testAutoLink
	#$(PY) -m unittest -v testNodeSpec.TestBuildSpec.test_agent_func_action

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
