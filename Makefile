PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	#$(PY) -m unittest -v testGloms.TestGloms.test_from_one_glom_to_another
	$(PY) -m unittest -v testHierarchy.TestHierarchy

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm *.gen.py

.PHONY: ut tags clean
