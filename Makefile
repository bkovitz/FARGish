PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testIndenting
	#$(PY) -m unittest -v testCodegen.TestCodegen.test_see_do

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
