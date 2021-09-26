ifeq ($(shell hostname),samosa)  # Ben's MacBook Air M1
PY = python3
BROWSER = open
MYPY = mypy
else
#PY = python3.7
PY = python3.9
BROWSER = google-chrome
MYPY = mypy --python-version 3.9 --show-error-codes
endif

# Type 'make' to run unit tests and then run current program in interactive
# mode.
ut_and_go: ut current

# Convenience target for "do whatever I'm currently working on".
current:
	$(PY) -i numbo6.py

test: ut at

# Static typecheck. Requires that 'mypy' be installed.
mypy:
	@echo -e \\n\\n\\n\\n\\n
	$(MYPY) Numbo1a.py --exclude '/Propagator.py$$/'

# Unit tests for FARGish2. This will be obsolete once FARGModel.py and its
# mates are done.
ut2:
	$(PY) -m unittest -v testSlipnet.py testFARGish2.py testPropagator.py testNumbo.py

# Run all unit tests. 'ut' should be set to this. It's only here as a temporary
# measure before clearing out a bunch of obsolete unit tests.
full_ut:
	$(PY) -m unittest -v

ut:
	$(PY) -m unittest -v testFARGModel testSlipnet testNumberMatcher testGraph2 testEquation

lut:
	$(PY) -m unittest -v `cat LIVETEST`

# Convenience target for single test current being worked on
u:
	$(PY) -m unittest -v testEquation.TestEquation
	#$(PY) -m unittest -v testGraph2.TestGraph.test_doubled_graph

# Acceptance tests
at:
	#$(PY) -m unittest -v atest*.py
	# TODO Restore previous line after old atests are removed.
	$(PY) -m unittest -v atestEquation

a:
	#$(PY) -m unittest -v atest_numbo.NumboTest.test_456_1
	$(PY) -m unittest -v atestEquation

# line counts
lc: clean
	@echo -n 'LIVE: '
	@wc -l `cat LIVE` | tail -1 | awk '{print $$1}'
	@echo -n 'LIVETEST: '
	@wc -l `cat LIVETEST` | tail -1 | awk '{print $$1}'
	@echo -n '*.py: '
	@wc -l *.py | tail -1 | awk '{print $$1}'

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
