PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testNodeSpec.TestBasics.test_cartesian_product_not_linked_to_same_no_dups

tags:
	ctags *.py fargcyto.js  # force-directed*.js

.PHONY: ut tags
