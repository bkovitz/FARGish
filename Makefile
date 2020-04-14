PY = python3.7

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	#$(PY) -m unittest -v testBrute
	$(PY) -m unittest -v testPortGraph.TestPortGraph.test_partition_nodes

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm *.gen.py

.PHONY: ut tags clean
