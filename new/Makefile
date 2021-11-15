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

FARGISH_SRC = Agents.py Canvas.py Codelets.py Consume.py Detectors.py \
	Equation.py FARGModel.py FMTypes.py Graph.py Indenting.py \
	Propagator.py QArgs.py Slipnet.py util.py Workspace.py see.py
# Plot.py 
# Log.py 
# Numbo1.py 

# Type 'make' to run unit tests and then run current program in interactive
# mode.
ut_and_go: ut current

# Convenience target for "do whatever I'm currently working on".
current: mypy
	$(PY) -i atestPons.py
i: current  # "interactive" version of what I'm currently working on
see: mypy
	$(PY) -i see.py

test: ut at
mtest: mypy test

# Static typecheck. Requires that 'mypy' be installed.
mypy:
	@echo -e \\n\\n\\n\\n\\n
	$(MYPY) $(MYPY_FLAGS) $(FARGISH_SRC) test*.py #atest*.py

# For type-checking just one file (specified here).
m:
	$(MYPY) $(MYPY_FLAGS) Log.py


# Run all unit tests.
ut:
	$(PY) -m unittest -v

# Convenience target for single acceptance test current being worked on
u:
	#$(PY) -m unittest -v testCodelets.TestCodelets.test_make_variant_from_avails
	#$(PY) -m unittest -v testFARGModel.TestFARGModel.test_logging
	#$(PY) -m unittest -v testAgents.TestAgents.test_source_nodes_seq
	$(PY) -m unittest -v testGraph.TestGraph.test_concentric_walk

# Acceptance tests
at:
	$(PY) -m unittest -v atest*.py

# Convenience target for single acceptance test current being worked on
a:
	$(PY) -m unittest -v atestEquation

mut: mypy ut
mu: mypy u
mat: mypy at
ma: mypy a

# line counts
lc: clean
	@echo 'LIVE: '
	@wc -l $(FARGISH_SRC)
	@echo -e '\nTEST: '
	@wc -l test*.py atest*.py
#@wc -l `ls -1 *.py | grep -Ev '^a?test'`

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
