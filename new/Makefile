ifeq ($(shell hostname),samosa)  # Ben's MacBook Air M1
PY = python3
BROWSER = open
MYPY = mypy
else
#PY = python3.7
PY = python3.9
BROWSER = google-chrome
MYPY = mypy --python-version 3.9
endif
MYPY_FLAGS = --show-error-codes --allow-redefinition

FARGISH_SRC = Graph.py Propagator.py util.py Slipnet.py Canvas.py FARGModel.py \
	Equation.py Agents.py Codelets.py QArgs.py

# Type 'make' to run unit tests and then run current program in interactive
# mode.
ut_and_go: ut current

# Convenience target for "do whatever I'm currently working on".
current:
	$(PY) -i spike4.py

test: ut at

# Static typecheck. Requires that 'mypy' be installed.
mypy:
	@echo -e \\n\\n\\n\\n\\n
	$(MYPY) $(MYPY_FLAGS) $(FARGISH_SRC) test*.py #atest*.py

# Run all unit tests.
ut:
	$(PY) -m unittest -v

# Convenience target for single acceptance test current being worked on
u:
	#$(PY) -m unittest -v testFARGModel
	$(PY) -m unittest -v testAgents.TestAgents.test_want
	#$(PY) -m unittest -v testCodelets.TestCodelets.test_query_slipnet_for_delegate
	#$(PY) -m unittest -v test_util.TestIsTypeInstance
	#$(PY) -m unittest -v testGraph2.TestGraph.test_doubled_graph

# Acceptance tests
at:
	$(PY) -m unittest -v atest*.py

# Convenience target for single acceptance test current being worked on
a:
	$(PY) -m unittest -v atestEquation

# line counts
lc: clean
	@echo 'LIVE: '
	@wc -l `ls -1 *.py | grep -v '^test'`
	@echo -e '\nTEST: '
	@wc -l test*.py

# test coverage
# 'make ct bc' to generate coverage data and see it in a browser
ct coverage:
	coverage run -m unittest

bc browse-coverage:
	coverage html && $(BROWSER) htmlcov/index.html

tags:
	ctags *.py  # fargcyto.js  # force-directed*.js

clean:
	rm -f *.gen.py

.PHONY: ut tags clean test current ut_and_go lc
