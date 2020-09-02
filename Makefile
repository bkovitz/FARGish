PY = python3.7

# Convenience target for "do whatever I'm currently working on".
current: ut
	$(PY) -i numbo5.py

ut:
	$(PY) -m unittest -v

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testTimeStepper.TestTimeStepper.test_prev_new_nodes
	#$(PY) -m unittest -v testTimeStepper
	#$(PY) -m unittest -v testHierarchy.TestHierarchy
	#$(PY) -m unittest -v testNumble

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm *.gen.py

.PHONY: ut tags clean current
