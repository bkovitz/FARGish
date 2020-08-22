PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testCodegen.TestCodegen.test_multiply_inherit_param
	#$(PY) -m unittest -v testHierarchy.TestHierarchy
	#$(PY) -m unittest -v testNumble

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm *.gen.py

.PHONY: ut tags clean
