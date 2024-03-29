ifeq ($(shell hostname),samosa)  # Ben's MacBook Air M1
PY = python3
BROWSER = open
MYPY = mypy
else
#PY = python3.7
PY = PYTHONHASHSEED=0 python3.9
BROWSER = google-chrome
MYPY = mypy --python-version 3.9
endif
MYPY_FLAGS = --show-error-codes --allow-redefinition

FARGISH_SRC = ArgsMap.py CCModel.py CCTypes.py Complex.py FARGBase.py \
	FMTypes.py Indenting.py Log.py Program.py Tag.py run.py util.py
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
	$(MYPY) $(MYPY_FLAGS) Tag.py


# Run all unit tests.
ut:
	#$(PY) -m unittest -v
	$(PY) -m unittest -v testCCModel

# Convenience target for single acceptance test current being worked on
u:
	#$(PY) -m unittest -v testCodelets.TestCodelets.test_build_codelet
	#$(PY) -m unittest -v testDetectors.TestDetectors.test_dead_end_detector
	$(PY) -m unittest -v testCCModel.TestCCModel.test_subtaggers

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
