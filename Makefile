ifeq ($(shell hostname),samosa)  # Ben's MacBook Air M1
PY = python3
else
PY = python3.7
endif

# Type 'make' to run unit tests and then run current program in interactive
# mode.
ut_and_go: ut current

# Convenience target for "do whatever I'm currently working on".
current:
	$(PY) -i numbo6.py

test: ut at

ut:
	$(PY) -m unittest -v

lut:
	$(PY) -m unittest -v `cat LIVETEST`

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testNumboGraph.TestNumboGraph.test_consume_result
	#$(PY) -m unittest -v testHierarchy.TestHierarchy
	#$(PY) -m unittest -v testStdGraph.TestStdGraph.test_port_inheritance
	#$(PY) -m unittest -v test_override.TestOverride.test_blocked_tag
	#$(PY) -m unittest -v testAc.TestAc.test_ac_notice_same_value_fail_eq
	#$(PY) -m unittest -v testAc.TestAc.test_ac_already_built_acnode_without_args
	#$(PY) -m unittest -v testStdGraph.TestStdGraph.test_already_built__missing_arg
	#$(PY) -m unittest -v testBrute.TestBrute.test_brute

# Acceptance tests
at:
	$(PY) -m unittest -v atest*.py

a:
	$(PY) -m unittest -v atest_numbo.NumboTest.test_22222_5

# line counts
lc: clean
	@echo -n 'LIVE: '
	@wc -l `cat LIVE` | tail -1 | awk '{print $$1}'
	@echo -n 'LIVETEST: '
	@wc -l `cat LIVETEST` | tail -1 | awk '{print $$1}'
	@echo -n '*.py: '
	@wc -l *.py | tail -1 | awk '{print $$1}'

# test coverage
ct coverage:
	coverage run -m unittest

bc browse-coverage:
	coverage html && open htmlcov/index.html

tags:
	ctags *.py fargcyto.js  # force-directed*.js

clean:
	rm -f *.gen.py

.PHONY: ut tags clean test current ut_and_go lc
