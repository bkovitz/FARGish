PY = python3.7

# Convenience target for "do whatever I'm currently working on".
current: ut
	$(PY) -i numbo5.py

ut:
	$(PY) -m unittest -v

lut:
	$(PY) -m unittest -v `cat LIVETEST`

# Convenience target for single test current being worked on
u:
	#$(PY) -m unittest -v testStdGraph.TestStdGraph.test_slipnet_search testStdGraph.TestStdGraph.test_already_built__subclass testStdGraph.TestStdGraph.test_already_built__missing_arg
	#$(PY) -m unittest -v testCodegen.TestCodegen.test_build_agent
	#$(PY) -m unittest -v testNodeSpec.TestNodeSpec.test_cartesian_product_not_linked_to_same
	$(PY) -m unittest -v testBrute.TestBrute.test_brute

# Acceptance tests
at:
	$(PY) -m unittest -v atest*.py

a:
	$(PY) -m unittest -v atest_numbo.NumboTest.test_11111_5

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm *.gen.py

.PHONY: ut tags clean current
