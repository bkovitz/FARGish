PY = python3.7

# Convenience target for "do whatever I'm currently working on".
current: ut
	$(PY) -i numbo5.py

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	#$(PY) -m unittest -v testActionNode.TestActionNode.test_simplest_action_node
	#$(PY) -m unittest -v testBrute.TestBrute.test_brute
	$(PY) -m unittest -v testNetworkxPortGraph

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm *.gen.py

.PHONY: ut tags clean current
